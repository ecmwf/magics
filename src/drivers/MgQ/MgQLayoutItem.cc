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
    \file MgQLayoutItem.cc
    \brief Definition of MgQLayoutItem
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#include <QPainter>
#include <QDebug>

#include "MgQLayoutItem.h"

#include "Transformation.h"

using namespace magics;


MgQLayoutItem::MgQLayoutItem(const Layout &l) : 
  layout_(l),
  clipped_(l.clipp())
{
}

MgQLayoutItem::MgQLayoutItem(const MgQLayoutItem& ori) :
	layout_(ori.layout_),
	coordRatioX_(ori.coordRatioX_),
	coordRatioY_(ori.coordRatioY_),
	dimensionX_(ori.dimensionX_),
	dimensionY_(ori.dimensionY_),
	projectedMinX_(ori.projectedMinX_),
	projectedMinY_(ori.projectedMinY_),
    clipped_(ori.clipped_)	
{}

	
void MgQLayoutItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *, QWidget *)
{
}
   	
QRectF MgQLayoutItem::boundingRect() const
{
	return QRectF(projectedMinX_,projectedMinY_,
			projectedMaxX_-projectedMinX_,
			projectedMaxY_-projectedMinY_);
}

void MgQLayoutItem::mapFromSceneToProjectionCoords(QPointF& scenePoint,QPointF& projPoint) 
{
	QPointF ppLocal=mapFromScene(scenePoint);
	projPoint=QPointF(ppLocal.x()/coordRatioX_,ppLocal.y()/coordRatioY_);
}


void MgQLayoutItem::mapFromSceneToGeoCoords(const QPointF& scenePoint,QPointF& geoPoint) 
{
	const Transformation& transformation = layout_.transformation();

	QPointF ppLocal=mapFromScene(scenePoint);
	PaperPoint ppLayout(ppLocal.x()/coordRatioX_,ppLocal.y()/coordRatioY_);

	UserPoint gp;
	
	transformation.revert(ppLayout, gp);

	geoPoint.setX(gp.x());
	geoPoint.setY(gp.y());
}
string   MgQLayoutItem::mapFromSceneToTransformationDefinition(QPointF& ll,QPointF& ur)
{
	const Transformation& transformation = layout_.transformation();

	QPointF lll = mapFromScene(ll);
	PaperPoint llp(lll.x()/coordRatioX_,lll.y()/coordRatioY_);
	UserPoint llu;
	transformation.revert(llp, llu);

	QPointF url = mapFromScene(ur);
	PaperPoint urp(url.x()/coordRatioX_,url.y()/coordRatioY_);
	UserPoint uru;
	transformation.revert(urp, uru);

	string def;
	transformation.getNewDefinition(llu, uru, def);
	return def;
}
void MgQLayoutItem::mapFromSceneToGeoCoords(QRectF& rect,QList<QPointF>& corners) 
{	
	const Transformation& transformation = layout_.transformation();

	QPointF pp=mapFromScene(rect.bottomLeft());
	PaperPoint pp_ll(pp.x()/coordRatioX_,pp.y()/coordRatioY_);

	pp=mapFromScene(rect.topRight());
	PaperPoint pp_ur(pp.x()/coordRatioX_,pp.y()/coordRatioY_);

	UserPoint ll, ur;
	
	transformation.revert(pp_ll, ll);
	transformation.revert(pp_ur, ur);

	corners <<  QPointF(ll.x(),ll.y());
	corners <<  QPointF(ur.x(),ur.y());
}

void MgQLayoutItem::mapFromGeoToSceneCoords(const QPointF& geoPoint,QPointF& scenePoint) 
{
	const Transformation& transformation = layout_.transformation();

	UserPoint gp(geoPoint.x(),geoPoint.y());
	PaperPoint ppLayout = transformation(gp);

	QPointF ppLocal(ppLayout.x()*coordRatioX_,ppLayout.y()*coordRatioY_);

	scenePoint = mapToScene(ppLocal);
}

bool MgQLayoutItem::containsSceneCoords(QPointF& sPoint)
{
	QPointF lPoint=mapFromScene(sPoint);
	
	return containsPoint(lPoint);
}


bool MgQLayoutItem::containsGeoCoords(QPointF& geoPoint)
{
	const Transformation& transformation = layout_.transformation();

	UserPoint gp(geoPoint.x(),geoPoint.y());
	PaperPoint ppLayout = transformation(gp);

	QPointF ppLocal(ppLayout.x()*coordRatioX_,ppLayout.y()*coordRatioY_);

	//contains() gave false for points on the border of the bounding rect!!
	return containsPoint(ppLocal);
}

bool MgQLayoutItem::containsPoint(QPointF &p,qreal tolerance)
{
	return boundingRect().adjusted(-tolerance,-tolerance,tolerance,tolerance).contains(p);
}

void MgQLayoutItem::addToMainScene()
{
	setParentItem(parentItemInMainScene_);
}


MgQMagnifierLayoutItem::MgQMagnifierLayoutItem(MgQLayoutItem* item) : MgQLayoutItem(*item)
{
}

QRectF MgQMagnifierLayoutItem::boundingRect() const
{
	return QRectF();
}

void MgQMagnifierLayoutItem::clearPlotContents()
{	
	foreach(QGraphicsItem *item,childItems())
	{
		delete item;
	}
}
