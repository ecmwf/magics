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
    \file MgQLayoutItem.h
    \brief Definition of MgQLayoutItem.
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#ifndef _MgQLayoutItem_H
#define _MgQLayoutItem_H

#include "Layout.h"
#include "PaperPoint.h"
#include "magics.h"

#include "MgQ.h"

// namespace magics
//{

class MgQLayoutItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::LayoutItemType
    };

    // OpenGLLayoutItem();
    MgQLayoutItem(const Layout&);
    MgQLayoutItem(const MgQLayoutItem&);

    virtual int type() const override { return Type; }
    virtual QRectF boundingRect() const override;
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget*);

    const Layout& layout() { return layout_; };
    AnimationRules* animationRules() { return layout_.animationRules(); };
    // void animationRules(AnimationRules *anr) {animationRules_=anr;};

    bool clipped() const { return clipped_; }

    void coordRatioX(double f) { coordRatioX_ = f; };
    void coordRatioY(double f) { coordRatioY_ = f; };
    double coordRatioX() { return coordRatioX_; };
    double coordRatioY() { return coordRatioY_; };

    void dimensionX(double f) { dimensionX_ = f; };
    void dimensionY(double f) { dimensionY_ = f; };
    double dimensionX() { return dimensionX_; };
    double dimensionY() { return dimensionY_; };

    double projectedMinX() { return projectedMinX_; }
    void projectedMinX(double f) { projectedMinX_ = f; }
    double projectedMinY() { return projectedMinY_; }
    void projectedMinY(double f) { projectedMinY_ = f; }

    double projectedMaxX() { return projectedMaxX_; }
    void projectedMaxX(double f) { projectedMaxX_ = f; }
    double projectedMaxY() { return projectedMaxY_; }
    void projectedMaxY(double f) { projectedMaxY_ = f; }

    void mapFromSceneToProjectionCoords(QPointF&, QPointF&);
    string mapFromSceneToTransformationDefinition(QPointF&, QPointF&);
    void mapFromSceneToGeoCoords(const QPointF&, QPointF&);
    void mapFromSceneToGeoCoords(QRectF&, QList<QPointF>&);
    void mapFromGeoToSceneCoords(const QPointF&, QPointF&);

    bool containsSceneCoords(QPointF&);
    bool containsGeoCoords(QPointF&);
    bool containsPoint(QPointF&, qreal tolerance = 0.0001);

    QGraphicsItem* parentItemInMainScene() { return parentItemInMainScene_; }
    void setParentItemInMainScene(QGraphicsItem* p) { parentItemInMainScene_ = p; }

    void addToMainScene();


protected:
    const Layout& layout_;

    double coordRatioX_;
    double coordRatioY_;

    double dimensionX_;
    double dimensionY_;

    double projectedMinX_;
    double projectedMinY_;
    double projectedMaxX_;
    double projectedMaxY_;

    QGraphicsItem* parentItemInMainScene_;
    bool clipped_;
    // AnimationRules *animationRules_;
};


class MgQPreviewLayoutItem : public MgQLayoutItem {
public:
    enum
    {
        Type = MgQ::PreviewLayoutItemType
    };

    MgQPreviewLayoutItem(const Layout& l) : MgQLayoutItem(l){};
    int type() const { return Type; }
};

class MgQMagnifierLayoutItem : public MgQLayoutItem {
public:
    enum
    {
        Type = MgQ::MagnifierLayoutItemType
    };

    MgQMagnifierLayoutItem(MgQLayoutItem*);
    MgQMagnifierLayoutItem(const Layout& l) : MgQLayoutItem(l){};
    const MagnifierLayout& layout() { return (MagnifierLayout&)layout_; };
    void clearPlotContents();

    const QVector<QPointF>& area() const { return area_; }
    double resolutionX() { return resolutionY_; }
    double resolutionY() { return resolutionY_; }
    AnimationStep* step() { return step_; }
    double zoomFactor() { return zoomFactor_; }

    void setArea(const QVector<QPointF> area) { area_ = area; }
    void setResolutionX(double r) { resolutionX_ = r; }
    void setResolutionY(double r) { resolutionY_ = r; }
    void setStep(AnimationStep* step) { step_ = step; }
    void setZoomFactor(double zoomFactor) { zoomFactor_ = zoomFactor; }

    QRectF boundingRect() const;
    int type() const { return Type; }

protected:
    QVector<QPointF> area_;
    double resolutionX_;
    double resolutionY_;
    AnimationStep* step_;
    double zoomFactor_;
};


//}


#endif
