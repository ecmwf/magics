/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file MgQLayerItem.cc
    \brief Implementation of the MgQLayerItem class.
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#include <QDebug>
#include <QImage>
#include <QPainter>

#include "MgQLayerItem.h"

#include "MgQHistoItem.h"
#include "MgQLayoutItem.h"
#include "MgQRootItem.h"
#include "MgQScene.h"
#include "MgQStepItem.h"

using namespace magics;

QImage* MgQLayerItem::alphaDevice_    = 0;
QPainter* MgQLayerItem::alphaPainter_ = 0;


MgQLayoutItem* MgQSceneLayerItem::findProjectorItem(QPointF scenePos) {
    foreach (MgQLayoutItem* item, projectorItems_) {
        if (item->contains(item->mapFromScene(scenePos))) {
            return item;
        }
    }
    return 0;
}


MgQLayerItem::MgQLayerItem(Layer& layer, MgQLayoutItem* layout, int stepNum) : layer_(layer), layout_(layout) {
    rootItem_ = new MgQRootItem;
    rootItem_->setParentItem(this);
    rootItemOwner_ = MainScene;

    magnifierLayout_      = 0;
    magnifierDataEnabled_ = false;

    alphaScene_ = 0;

    previewScene_   = 0;
    previewDevice_  = 0;
    previewPainter_ = 0;

    histoScene_   = 0;
    histoPainter_ = 0;
    histoDevice_  = 0;

    // rootItem_->setTransform(layout->sceneTransform());

    for (int i = 0; i < stepNum; i++) {
        steps_ << 0;
    }

    for (int i = 0; i < stepNum; i++) {
        histoItems_ << 0;
    }
    if (stepNum == 0) {
        histoItems_ << 0;
    }

    // Show layer according to its visibility
    setData(MgQ::ItemIsVisibleKey, layer_.visibility());
}

MgQLayerItem::~MgQLayerItem() {
    if (alphaScene_)
        delete alphaScene_;
    if (previewScene_)
        delete previewScene_;
    if (histoScene_)
        delete histoScene_;

    if (alphaPainter_) {
        delete alphaPainter_;
        alphaPainter_ = 0;
    }
    if (previewPainter_)
        delete previewPainter_;
    if (histoPainter_)
        delete histoPainter_;

    if (alphaDevice_) {
        delete alphaDevice_;
        alphaDevice_ = 0;
    }
    if (previewDevice_)
        delete previewDevice_;
    if (histoDevice_)
        delete histoDevice_;
}

MgQMagnifierLayoutItem* MgQLayerItem::magnifierLayoutItem() {
    if (!magnifierLayout_)  // Init magnifier layout item
    {
        MgQLayoutItem* topLayout = topLayoutItem();
        if (!topLayout)
            return 0;

        // it is outside the root item
        magnifierLayout_ = new MgQMagnifierLayoutItem(topLayout);
        magnifierLayout_->setParentItem(this);
        magnifierLayout_->setParentItemInMainScene(this);
        magnifierLayout_->setData(MgQ::ItemIsVisibleKey, false);
    }

    return magnifierLayout_;
}


bool MgQLayerItem::layerVisibility() const {
    return layer_.visibility();
}

void MgQLayerItem::setLayerVisibility(bool visible) {
    layer_.visibility(visible);
    // setVisible(b);
    setData(MgQ::ItemIsVisibleKey, visible);
}

float MgQLayerItem::layerAlpha() const {
    int tr = layer_.transparency();
    return (100 - tr) / 100.;
}

void MgQLayerItem::setLayerAlpha(float alpha) {
    //	const float oriAlpha=layerAlpha();
    layer_.transparency((1. - alpha) * 100.);
    //	if(fabs(alpha-oriAlpha) > 0.05)
    //		update();
}

void MgQLayerItem::setStackLevel(int i) {
    float z = 1.1 + 0.01 * static_cast<float>(i);
    layer_.zindex(i);
    setZValue(z);
}

int MgQLayerItem::stackLevel() {
    return layer_.zindex();

    // int i=static_cast<int>((zValue()-1.1) *100.);
    // return i;
}

void MgQLayerItem::saveLayerState(MgQLayerState* st) {
    st->name_       = QString::fromStdString(layer_.name());
    st->id_         = QString::fromStdString(layer_.id());
    st->alpha_      = layerAlpha();
    st->visible_    = layerVisibility();
    st->stackLevel_ = stackLevel();
}

void MgQLayerItem::updateLayer(const MgQLayerItem* item) {
    if (item->name().toStdString() != layer_.name())
        return;

    setLayerVisibility(item->layerVisibility());
    setLayerAlpha(item->layerAlpha());
}

void MgQLayerItem::paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) {
    // Non-trasparent
    if (layerAlpha() >= 0.99) {
        addContentsToScene(MainScene);
    }
    // Transparent
    else {
        addContentsToScene(AlphaScene);

        bool clipping  = (painter->clipRegion().isEmpty()) ? false : true;
        QRect clipRect = painter->clipRegion().boundingRect();

        float sx = painter->transform().m11();
        float sy = painter->transform().m22();

        QRectF sourceRect, targetRect;
        QRect alphaRect, magRect;

        alphaDevice_->fill(qRgba(0, 0, 0, 0));

        // Magnifier
        if (clipping) {
            sourceRect = clipRect;
            magRect =
                QRect(sourceRect.x() * sx, sourceRect.y() * sy, sourceRect.width() * sx, sourceRect.height() * sy);
        }
        // Non magnifier!!
        else {
            // The rect in local coordinates
            sourceRect = layout_->boundingRect();
        }


        // Physical target rect on the alpha device
        alphaRect = QRect(0., 0., sourceRect.width() * sx, sourceRect.height() * sy);

        // qDebug() <<  "LAYER-MAGNIFY" << scale() << sx << sy << layout_->boundingRect()  << alphaRect <<  clipRect;

        // Check if the alpha device needs to be resized
        checkAlphaDeviceSize(alphaRect);

        // Set antialising for alpha painter
        alphaPainter_->setRenderHint(QPainter::Antialiasing, painter->renderHints() & QPainter::Antialiasing);

        // Render the layer into the alpha device: from sourceRect to alphaRect!
        alphaScene_->renderContents(alphaPainter_, option, alphaRect, sourceRect);

        // Save the painter transform
        QTransform trOri = painter->transform();

        // Set the scaling to 1 for the painter
        QTransform tr = trOri;
        tr.setMatrix(1., tr.m12(), tr.m13(), tr.m12(), 1., tr.m23(), tr.m31(), tr.m32(), tr.m33());
        painter->setTransform(tr);

        // Paint alphaRect form the alphaDevice
        painter->setOpacity(layerAlpha());

        // Magnifier
        if (clipping) {
            painter->drawImage(magRect, alphaDevice_->copy(alphaRect));
        }
        // Non magnifier!!
        else {
            painter->drawImage(alphaRect, alphaDevice_->copy(alphaRect));
        }

        // Reset the painter tranfor to the original value
        painter->setTransform(trOri);
    }
}

void MgQLayerItem::checkAlphaDeviceSize(QRect rect) {
    if (!alphaDevice_)
        return;

    int w = (alphaDevice_->width() < rect.width()) ? rect.width() : alphaDevice_->width();
    int h = (alphaDevice_->height() < rect.height()) ? rect.height() : alphaDevice_->height();

    if (alphaDevice_->width() < w || alphaDevice_->height() < h) {
        delete alphaPainter_;
        delete alphaDevice_;

        alphaDevice_  = new QImage(w + 2, h + 2, QImage::Format_ARGB32);
        alphaPainter_ = new QPainter(alphaDevice_);
    }
}

void MgQLayerItem::addContentsToScene(RootItemOwner newOwner) {
    if (rootItemOwner_ == newOwner)
        return;

    if (rootItemOwner_ == MainScene) {
        scene()->removeItem(rootItem_);
    }
    else if (rootItemOwner_ == AlphaScene) {
        if (!alphaScene_)
            return;

        alphaScene_->removePlotRootItemFromScene();
    }
    else if (rootItemOwner_ == PreviewScene) {
        if (!previewScene_)
            return;

        previewScene_->removePlotRootItemFromScene();
    }


    if (newOwner == MainScene) {
        rootItem_->setTransform(QTransform());
        rootItem_->setParentItem(this);
    }
    else if (newOwner == AlphaScene) {
        // rootItem_->setTransform(layout_->sceneTransform());
        if (!alphaScene_) {
            alphaScene_ = new MgQScene(rootItem_);
        }

        if (!alphaDevice_) {
            alphaDevice_  = new QImage(1100, 785, QImage::Format_ARGB32);
            alphaPainter_ = new QPainter(alphaDevice_);
        }

        alphaScene_->addPlotRootItemToScene();
    }
    else if (newOwner == PreviewScene) {
        rootItem_->setTransform(layout_->sceneTransform());

        if (!previewScene_) {
            previewScene_   = new MgQScene(rootItem_);
            previewDevice_  = new QImage(200, 200, QImage::Format_ARGB32);
            previewPainter_ = new QPainter(previewDevice_);
        }

        previewScene_->addPlotRootItemToScene();
    }

    rootItemOwner_ = newOwner;
}

void MgQLayerItem::setStepVisible(int step, bool visible) {
    if (steps_.count() == 0 || step < 0 || step >= steps_.count())
        return;

    if (steps_[step]) {
        steps_[step]->setData(MgQ::ItemIsVisibleKey, visible);
        // steps_[step]->setVisible(visible);
    }
}

void MgQLayerItem::clearStep(int step) {
    if (steps_.count() == 0 || step < 0 || step >= steps_.count())
        return;

    if (steps_[step]) {
        delete steps_[step];
        steps_[step] = 0;
    }
}

bool MgQLayerItem::stepCached(int step) {
    if (steps_.count() == 0 || step < 0 || step >= steps_.count())
        return false;

    if (steps_[step])
        return steps_[step]->cached();
    else
        return false;
}

MgQLayoutItem* MgQLayerItem::topLayoutItem() {
    foreach (MgQStepItem* step, steps_) {
        if (step != 0) {
            foreach (QGraphicsItem* item, step->childItems()) {
                if (item->type() == MgQ::LayoutItemType) {
                    MgQLayoutItem* layout = static_cast<MgQLayoutItem*>(item);
                    return layout;
                }
            }
        }
    }
    return 0;
}

void MgQLayerItem::renderPreview() {
    RootItemOwner prevOwner = rootItemOwner_;
    addContentsToScene(PreviewScene);

    // QRectF layoutRect=layout_->boundingRect();
    // QRectF targetRect((layout_->mapToScene( layoutRect).boundingRect()));
    // QRectF painterRect=painter->transform().mapRect(layout_->boundingRect());

    QRectF sourceRect = QRectF((layout_->mapToScene(layout_->boundingRect()).boundingRect()));

    const float w = sourceRect.width();
    const float h = sourceRect.height();
    //	float r=w/h;

    const float preW = previewDevice_->width();
    const float preH = previewDevice_->height();

    QRectF targetRect;
    if (h * preW / w > preH) {
        targetRect = QRectF(0., 0., w * preH / h, preH);
    }
    else {
        targetRect = QRectF(0., 0., preW, h * preW / w);
    }

    previewDevice_->fill(qRgba(255, 255, 255, 255));
    previewScene_->renderContents(previewPainter_, targetRect, sourceRect);

    QRect targetRectInt(targetRect.x(), targetRect.y(), targetRect.width(), targetRect.height());
    if (h * 100 / w > 50) {
        previewImg_ =
            previewDevice_->copy(targetRectInt).scaledToHeight(50, Qt::SmoothTransformation).mirrored(false, true);
    }
    else {
        previewImg_ =
            previewDevice_->copy(targetRectInt).scaledToWidth(100, Qt::SmoothTransformation).mirrored(false, true);
    }


    // previewImg_.save("/var/tmp/cgr/layer.png");


    // QRect targetRectInt(targetRect.x(),targetRect.y(),targetRect.width(),targetRect.height());

    /*painter->setOpacity(layerAlpha());
    painter->drawImage(layout_->boundingRect(),
               previewDevice_->copy(targetRectInt));*/

    addContentsToScene(prevOwner);
}


MgQHistoItem* MgQLayerItem::histoItem(int step) {
    if (!histoScene_) {
        histoScene_   = new QGraphicsScene;
        histoDevice_  = new QImage(300, 200, QImage::Format_ARGB32);
        histoPainter_ = new QPainter(histoDevice_);
    }

    if (stepNum() == 0) {
        if (!histoItems_[0]) {
            histoItems_[0] = new MgQHistoItem;
            histoItems_[0]->setVisible(false);
            histoScene_->addItem(histoItems_[0]);
        }
        return histoItems_[0];
    }
    else {
        if (step < 0 || step >= steps_.count() || !steps_[step])
            return 0;

        if (!histoItems_[step]) {
            histoItems_[step] = new MgQHistoItem;
            histoItems_[step]->setVisible(false);
            histoScene_->addItem(histoItems_[step]);
        }
        return histoItems_[step];
    }
}

MgQHistoItem* MgQLayerItem::resetHistoItem(int step) {
    MgQHistoItem* item = histoItem(step);

    if (item) {
        histoScene_->removeItem(item);
        delete item;

        if (stepNum() == 0) {
            histoItems_[0] = 0;
        }
        else if (step >= 0 && step < steps_.count() && steps_[step]) {
            histoItems_[step] = 0;
        }
        else {
            return 0;
        }
    }

    return histoItem(step);
}

void MgQLayerItem::addHistoItem(MgQHistoItem* item) {
    /*if(steps_.count() ==0 || step < 0 ||
           step >= steps_.count() || !steps_[step] )
        return;*/

    /*item->scene()->removeItem(item);

    if(!histoScene_)
    {
        histoScene_=new QGraphicsScene;
        histoDevice_=new QImage(200,200,QImage::Format_ARGB32);
        histoPainter_=new QPainter(histoDevice_);
    }

    histoScene_->addItem(item);*/
}

QPixmap MgQLayerItem::histoPixmap(int step, QSize size) {
    MgQHistoItem* item = histoItem(step);

    if (!item)
        return QPixmap();

    /*if(item->pixmap().isNull() && item->cached())
    {
        return QPixmap();
    }*/

    if (item->cached() && item->requestedPixmapSize() == size) {
        return item->pixmap();
    }
    else {
        /*foreach(QGraphicsItem *item,histoScene_->items())
        {
            qDebug() << item << item->childItems().count() <<  item->boundingRect() << item->sceneBoundingRect();
        }

        qDebug() << histoScene_->sceneRect() << item->boundingRect() << item->sceneBoundingRect() ;*/

        if (histoDevice_->size().width() < size.width() || histoDevice_->size().height() < size.height()) {
            delete histoDevice_;
            histoDevice_ = new QImage(size, QImage::Format_ARGB32);
        }

        item->setVisible(true);
        histoDevice_->fill(qRgba(255, 255, 255, 255));
        histoScene_->render(histoPainter_, QRectF(0, 0, size.width(), size.height()));
        item->setVisible(false);

        /*float ratio=1;
        QRectF scr=histoScene_->sceneRect();
        if(scr.height() > 0)
            ratio=scr.width()/scr.height();

        QImage img=histoDevice_->copy(0,0,size.width(),size.width()/ratio+1).mirrored(false,true);*/
        QImage img = histoDevice_->copy(0, 0, size.width(), size.height()).mirrored(false, true);

        item->setPixmap(QPixmap::fromImage(img), size);
        item->setCached(true);

        return item->pixmap();
    }

    return QPixmap();
}
