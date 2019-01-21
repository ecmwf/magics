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
    \file MgQSceneCacheItem.cc
    \brief Definition of MgQSceneCacheItem
    \author Graphics Section, ECMWF

    Started: September 2010
*/

#include "MgQSceneCacheItem.h"

#include <QDebug>
#include <QPainter>
#include <QPixmap>
#include <QStyleOptionGraphicsItem>

MgQSceneCacheItem::MgQSceneCacheItem(QPixmap* pixmap, QGraphicsItem* parent) : QGraphicsItem(parent), pixmap_(pixmap) {
    setFlag(QGraphicsItem::ItemUsesExtendedStyleOption, true);
}

MgQSceneCacheItem::~MgQSceneCacheItem() {}

QRectF MgQSceneCacheItem::boundingRect() const {
    return clipRect_;
}

void MgQSceneCacheItem::paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) {
    QRectF rect = option->exposedRect;

    /*if(!painter->clipRegion().isEmpty());
    {
        clipRect=painter->clipRegion().boundingRect();
    }
    else
    {
        clipRect=painter->clipPath().boundingRect();
    }*/

    painter->drawPixmap(rect, *pixmap_, rect);
}
