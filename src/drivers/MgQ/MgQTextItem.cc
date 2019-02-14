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
    \file MgQTextItem.cc
    \brief Definition of MgQTextItem
    \author Graphics Section, ECMWF

    Started: March2010
*/

#include "MgQTextItem.h"

#include <QDebug>
#include <QPainter>


MgQTextItem::MgQTextItem(const QString& text, QGraphicsItem* parent) :
    QGraphicsSimpleTextItem(text, parent),
    textBlanking_(false) {}

MgQTextItem::~MgQTextItem() {}

QRectF MgQTextItem::boundingRect() const {
    return QGraphicsSimpleTextItem::boundingRect();
}

void MgQTextItem::paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) {
    if (textBlanking_) {
        painter->fillRect(boundingRect(), Qt::white);
    }

    QGraphicsSimpleTextItem::paint(painter, option, widget);
}
