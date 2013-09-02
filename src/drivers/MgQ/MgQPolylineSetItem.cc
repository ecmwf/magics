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
    \file MgQPolylineSetItem.cc
    \brief Definition of MgQPolylineSetItem
    \author Graphics Section, ECMWF

    Started: April 2010
*/

#include "MgQPolylineSetItem.h"

#include <QDebug>
#include <QPainter>

//static int count=0;


MgQPolylineSetItem::MgQPolylineSetItem(QRectF &boundingRect,QGraphicsItem* parent) : 
	QGraphicsItem(parent)
{
	boundingRect_=boundingRect;
}

MgQPolylineSetItem::~MgQPolylineSetItem()
{	
	for(int i=0; i < polylines_.count(); i++)
	{
		if( polylines_[i]->points_)
			delete [] polylines_[i]->points_;
		if( polylines_[i]->path_)
			delete [] polylines_[i]->path_;
	}	
}

QRectF MgQPolylineSetItem::boundingRect() const
{
	//qDebug() << "PIXMAP" << QGraphicsPolylineSetItem::boundingRect();
	//qDebug() << "PIXMAP" << targetRect_;

	//return QGraphicsPolylineSetItem::boundingRect();
	//float w=boundingRectSize_;
	//return QRectF(-w/2.,-w/2.,w/2.,w/2.);
	return boundingRect_;
}

void MgQPolylineSetItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
{
	/*if(parentItem()->data(3).toRectF() != QRectF())
	{		
		QPolygonF p= mapFromScene(parentItem()->data(3).toRectF()); 
		if(p.boundingRect().intersects(boundingRect()) == false)
		{
			return;
		}
	}*/

	int brushIndex,penIndex;
	int prevBrushIndex=-1,prevPenIndex=-1;

	//qDebug() <<  "POLYLINESET" << polylines_.count() << painter->clipRegion() << painter->transform().mapRect(boundingRect_);

	bool antialias=painter->renderHints() & QPainter::Antialiasing;
	
	for(int i=0; i < polylines_.count(); i++)
	{
		penIndex=polylines_[i]->penIndex_;
		if(penIndex != prevPenIndex)
		{
			painter->setPen(penList_[penIndex]);
			prevPenIndex=penIndex;
		}

		brushIndex=polylines_[i]->brushIndex_;
		if(brushIndex != prevBrushIndex)
		{
			painter->setBrush(brushList_[brushIndex]);
			prevBrushIndex=brushIndex;
		}
	
		
		if(brushList_[brushIndex] != Qt::NoBrush &&
		   penList_[penIndex] == Qt::NoPen)
		{
			painter->setRenderHint(QPainter::Antialiasing,false);  
		}
	
		if(polylines_[i]->num_==0 && polylines_[i]->path_ != 0)
		{
		  	painter->drawPath(*(polylines_[i]->path_));
		}			
		else
		{
			if(polylines_[i]->num_==2)
			{
				painter->drawLine(polylines_[i]->points_[0],polylines_[i]->points_[1]);
			}		
			else if(polylines_[i]->isPolygon_)
			{
				painter->drawPolygon(polylines_[i]->points_,polylines_[i]->num_);
			}
			else
			{
				painter->drawPolyline(polylines_[i]->points_,polylines_[i]->num_);
			}
		
		}
		
		painter->setRenderHint(QPainter::Antialiasing,antialias);  
		
	}

	//count++;

	//qDebug() << "count" << count;	

	//QGraphicsPolylineSetItem::paint(painter,option,widget);
	//painter->drawPolylineSet(QRectF(0,0,targetRect_.width(),targetRect_.height()),
	//		    path(),
	//		    QRectF(0,0,pixmap().width(),pixmap().height()));
}


void MgQPolylineSetItem::addPolyline(QVector<QPointF> points,QBrush brush,QPen pen,bool isPolygon)
{
	if(points.count() <= 0)
		return;

	MgQPolyline *line=new MgQPolyline;
	polylines_.push_back(line);

	line->points_ = new QPointF[points.count()];
	for(int i=0; i < points.count(); i++)
	{		
		line->points_[i]=points[i];
	}
	
	line->num_=points.count();
	line->isPolygon_=isPolygon;

	int penIndex, brushIndex;

	if((brushIndex=brushList_.indexOf(brush)) == -1)
	{
		brushList_.push_back(brush);
		line->brushIndex_=brushList_.count()-1;
	}
	else
	{
		line->brushIndex_=brushIndex;
	}

	if((penIndex=penList_.indexOf(pen)) == -1)
	{
		penList_.push_back(pen);
		line->penIndex_=penList_.count()-1;
	}
	else
	{
		line->penIndex_=penIndex;
	}
}

void MgQPolylineSetItem::addPath(QPainterPath &path,QBrush brush,QPen pen)
{
	if(path.isEmpty())
		return;

	MgQPolyline *line=new MgQPolyline;
	polylines_.push_back(line);

	line->path_ = new QPainterPath(path);	
	line->num_=0;
	line->isPolygon_=true;

	int penIndex, brushIndex;

	if((brushIndex=brushList_.indexOf(brush)) == -1)
	{
		brushList_.push_back(brush);
		line->brushIndex_=brushList_.count()-1;
	}
	else
	{
		line->brushIndex_=brushIndex;
	}

	if((penIndex=penList_.indexOf(pen)) == -1)
	{
		penList_.push_back(pen);
		line->penIndex_=penList_.count()-1;
	}
	else
	{
		line->penIndex_=penIndex;
	}
}
