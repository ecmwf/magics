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
    \file MgQPixmapItem.h
    \brief Definition of MgQPixmapItem.
    \author Graphics Section, ECMWF

    Started: Feb 2010
*/

#ifndef _MgQPixmapItem_H
#define _MgQPixmapItem_H

#include <QGraphicsPixmapItem>

#include "MgQ.h"

class MgQPixmapItem : public QGraphicsPixmapItem {
public:
    enum
    {
        Type = MgQ::PixmapItemType
    };

    MgQPixmapItem(const QPixmap& pixmap, QGraphicsItem* parent = 0);
    ~MgQPixmapItem();

    void setTargetRect(QRectF r) { targetRect_ = r; }
    QRectF boundingRect() const;
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget* widget = 0);
    void setClipRect(QRectF);
    int type() const { return Type; }

protected:
    QRectF targetRect_;
    QRectF clipRect_;
};

#endif
