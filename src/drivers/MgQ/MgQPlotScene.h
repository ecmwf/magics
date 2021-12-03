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
    \file MgQPlotScene.h
    \brief Definition of MgQPlotScene.
    \author Graphics Section, ECMWF

    Started: March 2010
*/

#ifndef MgQPlotScene_H
#define MgQPlotScene_H

#include <QColor>
#include <QGraphicsScene>
#include <QMap>

#include "MgQScene.h"
#include "QtDriver.h"

// for AIX compiler which fails when we have two definition of a macro
// (gcc also complains, but does not fail)
// in any case, we do not use the macro, so removing it should be safe
#undef ABS

class QGraphicsRectItem;
class QProgressDialog;

class MgQDriverObject;
class MgQLayerItem;
class MgQLayerState;
class MgQLayoutItem;
class MgQPreviewLayoutItem;
class MgQRootItem;
class MgQSceneCacheItem;
class MgQSceneItem;
class MgQSceneLayerItem;
class MgQStepMetaData;

using magics::Layer;
using magics::SceneLayer;

class MgQPlotScene : public MgQScene {
    Q_OBJECT

public:
    MgQPlotScene(QObject* parent = 0);
    ~MgQPlotScene() override;

    enum PlotScaleMode
    {
        RelativeToCurrentSize,
        RelativeToOriSize
    };

    void clearBeforeNewRequest();
    void saveStateBeforeNewRequest();
    void restoreLayerState();

    void addSceneItem(MgQSceneItem*);
    MgQSceneItem* currentSceneItem();
    void setCurrentSceneItem(MgQSceneItem*);
    QList<MgQSceneItem*> sceneItems() { return sceneItems_; }
    MgQSceneItem* findSceneItem(QPointF);

    QRectF oriSceneRect() { return oriSceneRect_; }
    void setOriSceneRect(QRectF r) { oriSceneRect_ = r; }

    MgQLayoutItem* firstProjectorItem();
    MgQLayoutItem* findProjectorItem(QPointF);
    MgQLayoutItem* projectorItem(QPointF p) { return findProjectorItem(p); }
    bool identifyPos(QPointF, MgQSceneItem**, MgQLayoutItem**);

    void setEnableAntialias(bool);
    void updateAfterNewRequest();
    void renderForMagnifier(QPainter*, const QRectF&, const QRectF&);

    void sceneItemChanged();
    void highlightSceneItem(MgQSceneItem*, bool);
    void highlightSceneItemForBrief(MgQSceneItem*, bool);
    void setPlotScale(float, PlotScaleMode);
    float plotScale();

    void setDpiResolution(int r) { dpiResolution_ = r; }
    int dpiResolution() { return dpiResolution_; }

    void setHighlightStyle(QBrush, QPen);
    void setHighlightBriefStyle(QBrush, QPen);

public slots:
    void slotSceneRectChanged(const QRectF&);

protected:
    void updateCache();
    void drawBackground(QPainter* painter, const QRectF& rect) override;

    bool ignoreSceneItemChange_{false};

    QList<int> prevSceneItemCurrentStep_;
    int stepNum_{0};

    QRectF magnifierSceneRect_;
    float magnifierFactor_;

    MgQPreviewLayoutItem* previewLayoutItem_{nullptr};
    QList<MgQSceneItem*> sceneItems_;
    int currentSceneItemIndex_{-1};

    QGraphicsRectItem* highlightItem_{nullptr};
    MgQSceneItem* highlightedSceneItem_{nullptr};
    QGraphicsRectItem* highlightItemForBrief_{nullptr};
    QBrush highlightBrush_{QColor(0, 0, 255, 10)};
    QPen highlightPen_{QColor(0, 0, 0, 100), 2};
    QBrush highlightBriefBrush_{QColor(255, 0, 0, 10)};
    QPen highlightBriefPen_{QColor(0, 0, 0, 100)};

    MgQSceneLayerItem* sceneLayerItem_{nullptr};
    QList<MgQLayerItem*> layerItems_;
    QList<QList<MgQLayerState*> > previousSceneState_;

    MgQDriverObject* driverObject_{nullptr};

    QPixmap* cacheDevice_{nullptr};
    QPainter* cachePainter_{nullptr};
    MgQSceneCacheItem* cacheItem_{nullptr};

    bool antialias_{false};
    QRectF oriSceneRect_;

    int dpiResolution_{75};
};


#endif
