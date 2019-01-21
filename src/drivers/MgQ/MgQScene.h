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
    \file MgQScene.h
    \brief Definition of MgQScene.
    \author Graphics Section, ECMWF

    Started: June 2010
*/

#ifndef MgQScene_H
#define MgQScene_H

#include <QGraphicsScene>

class MgQRootItem;

class MgQScene : public QGraphicsScene {
public:
    MgQScene(QObject* parent = 0);
    MgQScene(MgQRootItem*, QObject* parent = 0);
    ~MgQScene();

    void renderContents(QPainter*, const QRectF&, const QRectF&);
    void renderContents(QPainter*, const QStyleOptionGraphicsItem*, const QRectF&, const QRectF&,
                        bool renderAllItems = false);

    MgQRootItem* plotRootItem() { return plotRootItem_; }
    MgQRootItem* annotationRootItem() { return annotationRootItem_; }

    void addPlotRootItemToScene();
    void removePlotRootItemFromScene();

protected:
    void renderItemRecursively(QGraphicsItem*, QPainter*, const QStyleOptionGraphicsItem*);
    bool checkItemIsVisible(QGraphicsItem*);
    bool checkItemType(QGraphicsItem*, int);

    MgQRootItem* plotRootItem_;
    MgQRootItem* annotationRootItem_;
};


#endif
