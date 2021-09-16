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
    \file MgQRootItem.h
    \brief Definition of MgQRootItem.
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#ifndef _MgQRootItem_H
#define _MgQRootItem_H

#include "MgQ.h"
#include "magics.h"

#include <QGraphicsItem>
#include <QGraphicsScene>

class MgQRootItem : public QGraphicsItem {
public:
    MgQRootItem(QGraphicsItem* parent = 0) : QGraphicsItem(parent){};
    virtual ~MgQRootItem() override{};

    int type() const override { return Type; }
    QRectF boundingRect() const override { return QRectF(); }
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget*) override {};

    void clearContents() {
        foreach (QGraphicsItem* item, childItems()) {
            scene()->removeItem(item);
            delete item;
        }
    }
};


#endif
