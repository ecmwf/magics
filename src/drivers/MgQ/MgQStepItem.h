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
    \file MgQStepItem.h
    \brief Definition of MgQStepItem.
    \author Graphics Section, ECMWF

    Started: May 2008
*/

#ifndef _MgQStepItem_H
#define _MgQStepItem_H

#include <magics.h>

#include "MgQ.h"

class MgQLayoutItem;

class MgQStepItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::StepItemType
    };

    MgQStepItem(MgQLayoutItem*);
    ~MgQStepItem();

    int id() { return id_; }
    void id(int i) { id_ = i; }
    bool cached() { return cached_; }
    void setCached(bool b) { cached_ = b; }
    MgQLayoutItem* parentLayoutItem() { return parentLayoutItem_; }

    int type() const { return Type; }
    QRectF boundingRect() const { return QRectF(); }
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget*){};

protected:
    int id_;
    bool cached_;
    MgQLayoutItem* parentLayoutItem_;
};


//}


#endif
