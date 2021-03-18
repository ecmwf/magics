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
    \file MgQTextItem.h
    \brief Definition of MgQTextItem.
    \author Graphics Section, ECMWF

    Started: March 2010
*/

#ifndef _MgQTextItem_H
#define _MgQTextItem_H

#include <QGraphicsSimpleTextItem>

#include "MgQ.h"

class MgQTextItem : public QGraphicsSimpleTextItem {
public:
    enum
    {
        Type = MgQ::TextItemType
    };

    MgQTextItem(const QString&, QGraphicsItem* parent = 0);
    ~MgQTextItem();

    void setBoundingRectSize(float s) { boundingRectSize_ = s; }
    QRectF boundingRect() const;
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget* widget = 0);
    int type() const { return Type; }
    void setTextBlanking(bool b) { textBlanking_ = b; }

protected:
    float boundingRectSize_;
    bool textBlanking_;
};

#endif
