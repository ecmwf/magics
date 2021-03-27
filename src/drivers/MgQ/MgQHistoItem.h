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
    \file MgQHistoItem.h
    \brief Definition of MgQHistoItem.
    \author Graphics Section, ECMWF

    Started: February 2011
*/

#ifndef _MgQHistoItem_H
#define _MgQHistoItem_H

#include <magics.h>

#include "MgQ.h"

class MgQHistoItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::HistoItemType
    };

    MgQHistoItem();
    ~MgQHistoItem();
    QPixmap pixmap() { return pixmap_; }
    void setPixmap(QPixmap p, QSize s) {
        pixmap_              = p;
        requestedPixmapSize_ = s;
    }
    QSize requestedPixmapSize() { return requestedPixmapSize_; }
    bool cached() { return cached_; };
    void setCached(bool b) { cached_ = b; }
    QHash<QString, QString> pixmapId() { return pixmapId_; }
    void setPixmapId(QHash<QString, QString> id) { pixmapId_ = id; }

    int type() const { return Type; }
    QRectF boundingRect() const { return QRectF(); }
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget*){};

protected:
    QPixmap pixmap_;
    QSize requestedPixmapSize_;
    bool cached_;
    QHash<QString, QString> pixmapId_;
};

#endif
