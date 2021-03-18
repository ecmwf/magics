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
    \file MgQLayerItem.h
    \brief Definition of MgQLayerItem.
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#ifndef _MgQLayerItem_H
#define _MgQLayerItem_H

#include <Layer.h>
#include "MgQ.h"

using magics::Layer;
using magics::SceneLayer;

class QImage;
class QPainter;

class MgQHistoItem;
class MgQLayoutItem;
class MgQMagnifierLayoutItem;
class MgQRootItem;
class MgQScene;
class MgQStepItem;

class MgQLayerState {
public:
    QString name_;
    QString id_;
    int stackLevel_;
    float alpha_;
    bool visible_;
};

class MgQSceneLayerItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::SceneLayerItemType
    };

    MgQSceneLayerItem(const SceneLayer& sc) : sceneLayer_(sc){};
    ~MgQSceneLayerItem(){};

    const SceneLayer& sceneLayer() { return sceneLayer_; };
    void addProjectorItem(MgQLayoutItem* n) { projectorItems_ << n; }
    QList<MgQLayoutItem*> projectorItems() { return projectorItems_; }
    MgQLayoutItem* findProjectorItem(QPointF);

    int type() const { return Type; }
    QRectF boundingRect() const { return QRectF(); }
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget*){};

protected:
    const SceneLayer& sceneLayer_;
    QList<MgQLayoutItem*> projectorItems_;
};


class MgQLayerItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::LayerItemType
    };
    enum InfoImageType
    {
        HistoImage
    };

    MgQLayerItem(Layer&, MgQLayoutItem*, int);
    ~MgQLayerItem();

    QString name() const { return QString::fromStdString(layer_.name()); }
    Layer& layer() { return layer_; };
    void setLayerAlpha(float);
    void setLayerVisibility(bool);
    float layerAlpha() const;
    bool layerVisibility() const;
    void updateLayer(const MgQLayerItem*);
    MgQRootItem* rootItem() { return rootItem_; }

    void setStep(int step, MgQStepItem* item) { steps_[step] = item; }
    void setStepVisible(int, bool);
    bool stepCached(int);
    void clearStep(int);
    int stepNum() { return steps_.count(); }
    bool magnifierDataEnabled() { return magnifierDataEnabled_; }
    void setMagnifierDataEnabled(bool b) { magnifierDataEnabled_ = b; }

    MgQLayoutItem* parentLayoutItem() { return layout_; }

    MgQMagnifierLayoutItem* magnifierLayoutItem();

    int type() const { return Type; }
    QRectF boundingRect() const { return QRectF(); }
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget* widget = 0);

    void setStackLevel(int);
    int stackLevel();

    void saveLayerState(MgQLayerState*);
    MgQLayoutItem* topLayoutItem();

    void renderPreview();

    const QImage& preview() const { return previewImg_; }


    MgQHistoItem* histoItem(int);
    MgQHistoItem* resetHistoItem(int);
    void addHistoItem(MgQHistoItem*);
    QPixmap histoPixmap(int, QSize);


protected:
    enum RootItemOwner
    {
        MainScene,
        AlphaScene,
        PreviewScene
    };

    void checkAlphaDeviceSize(QRect);
    void addContentsToScene(RootItemOwner);

    Layer& layer_;

    MgQRootItem* rootItem_;
    RootItemOwner rootItemOwner_;

    MgQLayoutItem* layout_;

    MgQScene* alphaScene_;
    static QImage* alphaDevice_;
    static QPainter* alphaPainter_;

    MgQScene* previewScene_;
    QImage* previewDevice_;
    QPainter* previewPainter_;
    QImage previewImg_;

    QGraphicsScene* histoScene_;
    QImage* histoDevice_;
    QPainter* histoPainter_;

    QList<MgQStepItem*> steps_;
    QList<MgQHistoItem*> histoItems_;


    MgQMagnifierLayoutItem* magnifierLayout_;

    bool magnifierDataEnabled_;
};


#endif
