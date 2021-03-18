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
    \file MgQ.h
    \brief Definition of MgQ.
    \author Graphics Section, ECMWF

    Started: September 2011
*/

#ifndef _MgQ_H
#define _MgQ_H

#include <magics.h>

#include <QGraphicsItem>

// namespace magics
//{

namespace MgQ {
enum ItemDataKey
{
    ItemIsZoomableKey,
    ItemIsVisibleKey,
    ItemStepIdKey
};

const int HistoItemType           = QGraphicsItem::UserType + 1;
const int LayerItemType           = QGraphicsItem::UserType + 2;
const int LayoutItemType          = QGraphicsItem::UserType + 3;
const int MagnifierLayoutItemType = QGraphicsItem::UserType + 4;
const int PathItemType            = QGraphicsItem::UserType + 5;
const int PixmapItemType          = QGraphicsItem::UserType + 6;
const int PolylineSetItemType     = QGraphicsItem::UserType + 7;
const int PreviewLayoutItemType   = QGraphicsItem::UserType + 8;
const int RootItemType            = QGraphicsItem::UserType + 9;
const int SceneCacheItemType      = QGraphicsItem::UserType + 10;
const int SceneLayerItemType      = QGraphicsItem::UserType + 11;
const int SceneItemType           = QGraphicsItem::UserType + 12;
const int StepItemType            = QGraphicsItem::UserType + 13;
const int SymbolSetItemType       = QGraphicsItem::UserType + 14;
const int TextItemType            = QGraphicsItem::UserType + 15;
}  // namespace MgQ

//}


#endif
