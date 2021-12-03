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
    \file MgQPlotScene.cc
    \brief Definition of MgQPlotScene
    \author Graphics Section, ECMWF

    Started: March 2010
*/


#include "MgQPlotScene.h"

#include <QDebug>
#include <QGraphicsItem>
#include <QGraphicsRectItem>
#include <QImage>
#include <QPainter>
#include <QPixmap>
#include <QProgressDialog>
#include <QStyleOptionGraphicsItem>

#include "MgQDriverObject.h"
#include "MgQHistoItem.h"
#include "MgQLayerItem.h"
#include "MgQLayoutItem.h"
#include "MgQRootItem.h"
#include "MgQSceneCacheItem.h"
#include "MgQSceneItem.h"
#include "MgQStepMetaData.h"


MgQPlotScene::MgQPlotScene(QObject* parent) : MgQScene(parent) {

    cacheDevice_ = new QPixmap(2000, 1500);
    cacheItem_ = new MgQSceneCacheItem(cacheDevice_);
    addItem(cacheItem_);
    cachePainter_ = new QPainter(cacheDevice_);
    cacheDevice_->fill(qRgba(255, 255, 255, 255));
    cachePainter_->setRenderHint(QPainter::Antialiasing, antialias_);

    connect(this, SIGNAL(sceneRectChanged(const QRectF&)), 
            this, SLOT(slotSceneRectChanged(const QRectF&)));
}

MgQPlotScene::~MgQPlotScene() {
    if (sceneLayerItem_)
        delete sceneLayerItem_;

    if (driverObject_)
        delete driverObject_;

    foreach (QList<MgQLayerState*> sc, previousSceneState_) {
        foreach (MgQLayerState* st, sc) { delete st; }
        sc.clear();
    }
}

void MgQPlotScene::clearBeforeNewRequest() {
    prevSceneItemCurrentStep_.clear();

    // Animation
    foreach (MgQSceneItem* item, sceneItems_) { prevSceneItemCurrentStep_ << item->currentStep(); }

    // we do not delete annotations
    plotRootItem_->clearContents();

    sceneItems_.clear();
    layerItems_.clear();

    if (sceneLayerItem_) {
        delete sceneLayerItem_;
    }

    sceneLayerItem_       = nullptr;
    previewLayoutItem_    = nullptr;
    stepNum_              = 0;
    highlightedSceneItem_ = nullptr;
}

void MgQPlotScene::saveStateBeforeNewRequest() {
}


void MgQPlotScene::restoreLayerState() {
}

void MgQPlotScene::addSceneItem(MgQSceneItem* item) {
    // item->setParentItem(plotRootItem_);
    sceneItems_ << item;

    if (prevSceneItemCurrentStep_.count() >= sceneItems_.count()) {
        int i = sceneItems_.count() - 1;
        sceneItems_[i]->setPrevCurrentStep(prevSceneItemCurrentStep_[i]);
    }

    // connect(item,SIGNAL(changed()),
    //	this,SLOT(slotSceneItemChanged()));
}

void MgQPlotScene::setCurrentSceneItem(MgQSceneItem* item) {
    currentSceneItemIndex_ = sceneItems_.indexOf(item);
}


MgQSceneItem* MgQPlotScene::currentSceneItem() {
    if (currentSceneItemIndex_ >= 0 && currentSceneItemIndex_ < sceneItems_.count()) {
        return sceneItems_[currentSceneItemIndex_];
    }
    return nullptr;
}

MgQLayoutItem* MgQPlotScene::firstProjectorItem() {
    foreach (MgQSceneItem* item, sceneItems_) {
        MgQLayoutItem* res = item->firstProjectorItem();
        if (res)
            return res;
    }

    return nullptr;
}

MgQLayoutItem* MgQPlotScene::findProjectorItem(QPointF scenePos) {
    foreach (MgQSceneItem* item, sceneItems_) {
        MgQLayoutItem* res = item->findProjectorItem(scenePos);
        if (res)
            return res;
    }

    return nullptr;
}

MgQSceneItem* MgQPlotScene::findSceneItem(QPointF scenePos) {
    foreach (MgQSceneItem* item, sceneItems_) {
        if (item->sceneBoundingRect().contains(scenePos))
            return item;
    }
    return nullptr;
}


bool MgQPlotScene::identifyPos(QPointF scenePos, MgQSceneItem** sceneItem, MgQLayoutItem** projectorItem) {
    MgQSceneItem* scn  = *sceneItem;
    MgQLayoutItem* prn = nullptr;

    // Find zoomable layout, i.e. subpage!!!
    if (scn) {
        prn = scn->findProjectorItem(scenePos);
        if (prn == 0) {
            scn = findSceneItem(scenePos);
            if (scn) {
                prn = scn->findProjectorItem(scenePos);
            }
        }
    }
    else {
        scn = findSceneItem(scenePos);
        if (scn) {
            prn = scn->findProjectorItem(scenePos);
        }
    }


    *sceneItem     = scn;
    *projectorItem = prn;


    return (scn && prn);
}

void MgQPlotScene::updateCache() {
    if (!plotRootItem_)
        return;

    if (cacheDevice_->width() < sceneRect().width() || cacheDevice_->height() < sceneRect().height()) {
        delete cachePainter_;
        delete cacheDevice_;

        cacheDevice_ = new QPixmap(sceneRect().width() + 100, sceneRect().height() + 100);

        cachePainter_ = new QPainter(cacheDevice_);
        cachePainter_->setRenderHint(QPainter::Antialiasing, antialias_);

        cacheItem_->setPixmap(cacheDevice_);

        MagLog::debug() << "MgQPlotScene::updateCache() ---> Pixmap size changed to: " << cacheDevice_->width() << "x"
                        << cacheDevice_->height() << endl;
    }

    cacheItem_->setClipRect(sceneRect());

    QPainter* painter = cachePainter_;
    QStyleOptionGraphicsItem options;

    QRectF sourceRect = sceneRect();
    QRectF targetRect(0, 0, painter->device()->width(), painter->device()->height());

    cacheDevice_->fill(qRgba(255, 255, 255, 255));

    painter->save();

    QTransform painterTransform;
    painterTransform *= QTransform()
                            .translate(targetRect.left(), targetRect.top())
                            //.scale(xratio, yratio)
                            .translate(-sourceRect.left(), -sourceRect.top());
    painter->setWorldTransform(painterTransform, true);

    plotRootItem_->setVisible(false);

    renderItemRecursively(plotRootItem_, painter, &options);

    plotRootItem_->setVisible(false);

    /*foreach(QGraphicsItem *item, items(Qt::AscendingOrder))
    {
        if(item->zValue() < 1.2 && item->isVisible())
        {
            painter->save();
                painter->setTransform(item->sceneTransform(), true);
                item->paint(painter, &options, 0);
                painter->restore();
        }
    }*/

    painter->restore();

    QPen pen(Qt::black);
    pen.setWidth(2);
    painter->setPen(pen);
    painter->drawRect(targetRect);

    // cacheDevice_->save("/var/tmp/cgr/test.png");

    // rootItem_->setFlag(QGraphicsItem::ItemHasNoContents,true);
    plotRootItem_->setVisible(false);

    // cacheItem_->setPixmap(*cacheDevice_);

    // setBackgroundBrush(QPixmap::fromImage(*cacheDevice_));
}

void MgQPlotScene::renderForMagnifier(QPainter* painter, const QRectF& targetRect, const QRectF& sourceRect) {
    QStyleOptionGraphicsItem option;
    painter->setRenderHint(QPainter::Antialiasing, antialias_);
    renderContents(painter, &option, targetRect, sourceRect, true);
}

void MgQPlotScene::drawBackground(QPainter* painter, const QRectF& rect) {
    /*qDebug() << "bg" << rect << sceneRect();

    QRectF targetRect(0, 0, sceneRect().width(), sceneRect().height());
    //painter->drawPixmap(sceneRect(),*cacheDevice_,targetRect);

    painter->drawPixmap(rect,*cacheDevice_,rect);*/
}

void MgQPlotScene::updateAfterNewRequest() {
    ignoreSceneItemChange_ = true;

    foreach (MgQSceneItem* item, sceneItems_) {
        item->selectCurrentStepForAnimation();
        item->renderLayerPreview();
    }

    ignoreSceneItemChange_ = false;
}


void MgQPlotScene::sceneItemChanged() {
    if (!ignoreSceneItemChange_) {
        updateCache();
        update();
    }
}

void MgQPlotScene::setEnableAntialias(bool status) {
    if (antialias_ != status) {
        antialias_ = status;
        cachePainter_->setRenderHint(QPainter::Antialiasing, antialias_);

        updateCache();
        update();
    }
}

void MgQPlotScene::setHighlightStyle(QBrush b, QPen p)
{
    highlightBrush_ = b;
    highlightPen_ = p;
}

void MgQPlotScene::setHighlightBriefStyle(QBrush b, QPen p)
{
    highlightBriefBrush_ = b;
    highlightBriefPen_ = p;
}

void MgQPlotScene::highlightSceneItem(MgQSceneItem* item, bool status) {
    if (!highlightItem_) {
        highlightItem_ = new QGraphicsRectItem;
        highlightItem_->setBrush(highlightBrush_);
        highlightItem_->setPen(highlightPen_);
        highlightItem_->setVisible(false);
        // highlightItem_->setScale(plotRootItem_->scale());
        addItem(highlightItem_);
    }

    if (status && item) {
        float w = (highlightPen_.width()==0)?1:highlightPen_.width()*0.5;
        highlightItem_->setRect(item->sceneBoundingRect().adjusted(w, w, -w, -w));
        highlightItem_->setVisible(true);
        highlightedSceneItem_ = item;
    }
    else {
        highlightItem_->setVisible(false);
    }
}

void MgQPlotScene::highlightSceneItemForBrief(MgQSceneItem* item, bool status) {
    if (!highlightItemForBrief_) {
        highlightItemForBrief_ = new QGraphicsRectItem;
        highlightItemForBrief_->setBrush(highlightBriefBrush_);
        highlightItemForBrief_->setPen(highlightBriefPen_);

        highlightItemForBrief_->setVisible(false);
        // highlightItem_->setScale(plotRootItem_->scale());
        addItem(highlightItemForBrief_);
    }

    if (status && item) {
        float w = (highlightBriefPen_.width()==0)?1:highlightBriefPen_.width()*0.5;
        highlightItemForBrief_->setRect(item->sceneBoundingRect().adjusted(w, w, -w, -w));
        highlightItemForBrief_->setVisible(true);
        // highlightedSceneItem_=item;
    }
    else {
        highlightItemForBrief_->setVisible(false);
    }
}

void MgQPlotScene::setPlotScale(float scaling, PlotScaleMode mode) {
    if (mode == RelativeToCurrentSize) {
        float currentScaling = plotRootItem_->scale();
        float newScaling     = scaling * currentScaling;

        plotRootItem_->setScale(newScaling);
        annotationRootItem_->setScale(newScaling);
        QRectF r = sceneRect();
        r.setWidth(r.width() * scaling);
        r.setHeight(r.height() * scaling);
        setSceneRect(r);
    }
    else if (mode == RelativeToOriSize) {
        plotRootItem_->setScale(scaling);
        annotationRootItem_->setScale(scaling);
        QRectF r = oriSceneRect_;
        r.setWidth(r.width() * scaling);
        r.setHeight(r.height() * scaling);
        setSceneRect(r);
    }
}

float MgQPlotScene::plotScale() {
    return plotRootItem_->scale();
}

void MgQPlotScene::slotSceneRectChanged(const QRectF& rect) {
    if (highlightItem_ && highlightedSceneItem_ && highlightItem_->isVisible()) {
        highlightItem_->setRect(highlightedSceneItem_->sceneBoundingRect());
        // highlightItem_->setScale(plotRootItem_->scale());
    }
}
