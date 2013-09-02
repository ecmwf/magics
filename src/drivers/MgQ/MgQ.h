/******************************** LICENSE ********************************


 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)
 
 Licensed under the Apache License, Version 2.0 (the "License"); 
 you may not use this file except in compliance with the License. 
 You may obtain a copy of the License at 
 
 	http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software 
 distributed under the License is distributed on an "AS IS" BASIS, 
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 See the License for the specific language governing permissions and 
 limitations under the License.


 ******************************** LICENSE ********************************/

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

//namespace magics 
//{

namespace MgQ
{
	enum ItemDataKey {ItemIsZoomableKey,ItemIsVisibleKey,ItemStepIdKey};

	const int HistoItemType=QGraphicsItem::UserType + 1;
	const int LayerItemType=QGraphicsItem::UserType + 2;
	const int LayoutItemType=QGraphicsItem::UserType + 3;
	const int MagnifierLayoutItemType=QGraphicsItem::UserType + 4;
	const int PathItemType=QGraphicsItem::UserType + 5;
	const int PixmapItemType=QGraphicsItem::UserType + 6;
	const int PolylineSetItemType=QGraphicsItem::UserType + 7;
	const int PreviewLayoutItemType=QGraphicsItem::UserType + 8;
	const int RootItemType=QGraphicsItem::UserType + 9;
	const int SceneCacheItemType=QGraphicsItem::UserType + 10;
	const int SceneLayerItemType=QGraphicsItem::UserType + 11;
	const int SceneItemType=QGraphicsItem::UserType + 12;
	const int StepItemType=QGraphicsItem::UserType + 13;
	const int SymbolSetItemType=QGraphicsItem::UserType + 14;
	const int TextItemType=QGraphicsItem::UserType + 15;
}

//}


#endif
