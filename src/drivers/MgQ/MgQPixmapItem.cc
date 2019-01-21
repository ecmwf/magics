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
    \file MgQPixmapItem.cc
    \brief Definition of MgQPixmapItem
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#include "MgQPixmapItem.h"

#include <QDebug>
#include <QPainter>

MgQPixmapItem::MgQPixmapItem(const QPixmap& pixmap, QGraphicsItem* parent) : QGraphicsPixmapItem(pixmap, parent) {}

MgQPixmapItem::~MgQPixmapItem() {}

QRectF MgQPixmapItem::boundingRect() const {
    // qDebug() << "PIXMAP" << QGraphicsPixmapItem::boundingRect();
    // qDebug() << "PIXMAP" << targetRect_;

    // return QGraphicsPixmapItem::boundingRect();
    return QRectF(0, 0, targetRect_.width(), targetRect_.height());
}

void MgQPixmapItem::paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) {
    if (!clipRect_.isEmpty())
        painter->setClipRect(clipRect_);

    painter->setBackgroundMode(Qt::TransparentMode);

    // QGraphicsPixmapItem::paint(painter,option,widget);
    painter->drawPixmap(QRectF(0, 0, targetRect_.width(), targetRect_.height()), pixmap(),
                        QRectF(0, 0, pixmap().width(), pixmap().height()));
}

void MgQPixmapItem::setClipRect(QRectF rect) {
    clipRect_ = rect;
}
