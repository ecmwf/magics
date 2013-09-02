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
    \file MgQSymbol.h
    \brief Definition of MgQSymbol
    \author Graphics Section, ECMWF

    Started: Feb 2010
*/

#include "MgQSymbol.h"

#include <QDebug>
#include <QPainter>

//====================================
//
//  Item
//
//====================================

bool MgQSymbolItem::equal(const QString& id,const float size)
{
	return id == id_ && size == size_;
}

bool MgQSymbolItem::hasFilledPart()
{
	for(int i=0; i < paths_.count(); i++)
	{		
		if(paths_[i].isFilled())
		{
		  	return true;
		}
	}
	
	return false;
}

//====================================
//
//  Item set
//
//====================================

void MgQSymbolSetItem::setColor(QColor color)
{
	pen_=QPen(color);

	for(int i=0; i < symbol_->paths().count(); i++)
	{					
		if(symbol_->paths().at(i).isFilled())
		{
			if(symbol_->paths().at(i).isDefaultFill())
			{
				brushes_ << QBrush(Qt::white);
			}
			else
			{			
				brushes_ << QBrush(color);
			}
		}
		else
		{
			brushes_ << QBrush();
		}
	}
}

void MgQSymbolSetItem::setOutlineColor(QColor color)
{
	pen_=QPen(color);
	outline_=true;
} 
 
 void MgQSymbolSetItem::setConnectLinePen(QPen pen)
{
	connectLinePen_=pen;
} 
 
 
QRectF MgQSymbolSetItem::boundingRect() const
{
	//qDebug() << "PIXMAP" << QGraphicsPolylineSetItem::boundingRect();
	//qDebug() << "PIXMAP" << targetRect_;

	//return QGraphicsPolylineSetItem::boundingRect();
	//float w=boundingRectSize_;
	//return QRectF(-w/2.,-w/2.,w/2.,w/2.);
	return boundingRect_;
}

void MgQSymbolSetItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *,QWidget*)
{
	//qDebug() << "Symbol" << painter->clipRegion() << painter->clipPath() <<  painter->transform(); 

	float sx=painter->transform().m11();
	float sy=painter->transform().m22();

	bool clipping=(painter->clipRegion().isEmpty())?false:true;
	QRect clipRect=painter->clipRegion().boundingRect();

	if(keepSizeWhenScaling_)	
	{
		QTransform tr=painter->transform();
		tr.scale(1/sx,1/sy);
		painter->setTransform(tr);
	}	

	qDebug() << "Total"  << xp_.count()*symbol_->paths().count();

	//Draw lines conncting symbols
	if(connectLine_ && xp_.count() > 1 )
	{
	  	painter->setPen(connectLinePen_);	
		
		for(int j=1; j < xp_.count(); j++)
		{			  			  							
			painter->drawLine(xp_[j-1],yp_[j-1],xp_[j],yp_[j]);
		}
	}	
	
	int n=0;
	double dx,dy;
	double prevX=0., prevY=0.;
	
	painter->setPen(pen_);
	for(int j=0; j < xp_.count(); j++)
	{			  	  
	  	if(!clipping || 
	      	    clipRect.contains(xp_[j],yp_[j]))
		{		  
	  		dx=xp_[j]-prevX;
			dy=yp_[j]-prevY;
				
			if(!keepSizeWhenScaling_)	
				painter->translate(dx,dy);
			else
				painter->translate(dx*sx,dy*sy);
				
			for(int i=0; i < symbol_->paths().count(); i++)
			{		
				if(!symbol_->paths().at(i).renderOnlyForOutline()  || outline_ )
				{
					painter->setBrush(brushes_[i]);			
					painter->drawPath(symbol_->paths().at(i));
				}	
			}
			
			prevX=xp_[j];
			prevY=yp_[j];

			n++;
		}
	}	

	qDebug() << "Current"  << n;
}

void MgQSymbolSetItem::addPoint(double x, double y)
{
	xp_.push_back(x);
	yp_.push_back(y);
}

//====================================
//
//  Manager
//
//====================================

MgQSymbolManager::~MgQSymbolManager()
{	
	foreach(MgQSymbolItem *item, symbols_)
	{		
		delete item;
	}
}

MgQSymbolItem* MgQSymbolManager::getSymbol(const QString& id,const float size)
{	
	foreach(MgQSymbolItem *item, symbols_) 
	{
		if( item->equal(id,size) == true)
		{
			return item;
		}
	}
	
	return 0;		
}


MgQSymbolItem* MgQSymbolManager::addSymbol(const QString& id,const float size)
{
	MgQSymbolItem *sym = getSymbol(id,size);
	
	if(sym) 
	{
		return sym;
	}
	else
	{	
		sym=new MgQSymbolItem(id,size);
		symbols_.push_back(sym);
	}
	
	return sym;
}

void MgQSymbolManager::deleteSymbol(const QString& id,const float size)	
{	
	foreach(MgQSymbolItem *item, symbols_)
	{
		if( item->equal(id,size) == true)
		{
			delete item;
			return;
		}	
	}
}	