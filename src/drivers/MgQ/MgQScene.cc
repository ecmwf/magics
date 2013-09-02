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
    \file MgQScene.cc
    \brief Definition of MgQScene
    \author Graphics Section, ECMWF

    Started: June 2010
*/


#include "MgQScene.h"

#include <QDebug>
#include <QGraphicsItem>
#include <QPainter>
#include <QStyleOptionGraphicsItem>

#include "MgQRootItem.h"

MgQScene::MgQScene(QObject *parent) : QGraphicsScene(parent)
{
	plotRootItem_= new MgQRootItem;
	addItem(plotRootItem_);
	plotRootItem_->setEnabled(false);
	plotRootItem_->setVisible(false); 
	
	annotationRootItem_=new MgQRootItem;	
	addItem(annotationRootItem_);
	annotationRootItem_->setZValue(1.); // it should be above the plotRootItem	
}


MgQScene::MgQScene(MgQRootItem *plotRootItem, QObject *parent) : QGraphicsScene(parent), plotRootItem_(plotRootItem)
{
	addItem(plotRootItem_);
	//plotRootItem->setEnabled(false);
	//plotRootItem->setVisible(false); 
	
	annotationRootItem_=new MgQRootItem;	
	addItem(annotationRootItem_);	
}


MgQScene::~MgQScene()
{	  
}


void MgQScene::addPlotRootItemToScene()
{
	if(plotRootItem_->scene() == 0)
	{
	  	addItem(plotRootItem_);
	}
}

void MgQScene::removePlotRootItemFromScene()
{
	if(plotRootItem_->scene() == this)
	{  	  
		removeItem(plotRootItem_);
	}
}

void MgQScene::renderContents(QPainter *painter, const QStyleOptionGraphicsItem* option,
			     const QRectF &targetRect, const QRectF &sourceRect,bool renderAllItems)
{
	if(plotRootItem_->scene() != this)
	{
	  	return;
	}	
  
  	//Save painter state
	painter->save();
  
	//Find the scaling ratio 
    	float xRatio = targetRect.width() / sourceRect.width();
    	float yRatio = targetRect.height() / sourceRect.height();
 
	//Keep aspect ratio	
	xRatio =(xRatio<yRatio)?xRatio:yRatio;
	yRatio=xRatio;

	//qDebug() << "CLIPPING " << sourceRect << targetRect;
    	
	//Set clipping
	painter->setClipRect(targetRect);

	//Tranform the painter
	QTransform painterTransform;
	painterTransform *= QTransform()
                        .translate(targetRect.left(), targetRect.top())
                        .scale(xRatio, yRatio)
                        .translate(-sourceRect.left(), -sourceRect.top());
    	painter->setWorldTransform(painterTransform, true);

	//qDebug() << "CLIP TRANSFORMED" << painter->transform().mapRect(sourceRect) << painter->transform().mapRect(targetRect);
	
	renderItemRecursively(plotRootItem_,painter,option);
	
	if(renderAllItems)
	{
		renderItemRecursively(annotationRootItem_,painter,option);
	}

	painter->restore();
}

void MgQScene::renderContents(QPainter *painter,
			      const QRectF &targetRect, const QRectF &sourceRect)
{
	QStyleOptionGraphicsItem option;
	renderContents(painter,&option,targetRect,sourceRect);
}

void MgQScene::renderItemRecursively(QGraphicsItem* item,QPainter *painter,const QStyleOptionGraphicsItem *option)
{
	//
	if(checkItemIsVisible(item) == true &&
	   item->type() != MgQ::PreviewLayoutItemType)
	{
		painter->save();
		painter->setTransform(item->sceneTransform(), true);
		item->paint(painter, option, 0);
          	painter->restore();

		foreach(QGraphicsItem *childItem,item->childItems())
		{
			renderItemRecursively(childItem,painter,option);
		}
	}
}


bool MgQScene::checkItemIsVisible(QGraphicsItem *item)
{
	if(item->data(MgQ::ItemIsVisibleKey).isNull()) 
	{
		return true;
	}
	else	 
	{
	    	return item->data(MgQ::ItemIsVisibleKey).toBool();
	}
}

