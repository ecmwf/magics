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
    \file MgQPolylineSetItem.h
    \brief Definition of MgQPolylineSetItem.
    \author Graphics Section, ECMWF

    Started: April 2010
*/

#ifndef _MgQPolylineSetItem_H
#define _MgQPolylineSetItem_H

#include <QGraphicsItem>

#include "MgQ.h"

class MgQPolyline {
public:
    MgQPolyline() : points_(0), path_(0), num_(0){};
    QPointF* points_;
    QPainterPath* path_;  // polygons with holes stored as QPainterPath
    int num_;
    int brushIndex_;
    int penIndex_;
    bool isPolygon_;
};

class MgQPolylineSetItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::PolylineSetItemType
    };

    MgQPolylineSetItem(QRectF&, QGraphicsItem* parent = 0);
    ~MgQPolylineSetItem();

    QRectF boundingRect() const;
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget* widget = 0);
    int type() const { return Type; }

    void addPolyline(QVector<QPointF>, QBrush, QPen, bool);
    void addPath(QPainterPath&, QBrush, QPen);

protected:
    QList<MgQPolyline*> polylines_;
    QList<QBrush> brushList_;
    QList<QPen> penList_;

    QRectF boundingRect_;
};

#endif
