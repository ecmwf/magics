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
    \file MgQScene.cc
    \brief Definition of MgQScene
    \author Graphics Section, ECMWF

    Started: June 2010
*/


#include "MgQScene.h"
#include "MgQ.h"
#include "MgQLayoutItem.h"

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
	if(checkItemIsVisible(item) == true &&
	   item->type() != MgQ::PreviewLayoutItemType)
	{
		painter->save();
		painter->setTransform(item->sceneTransform(), true);
		item->paint(painter, option, 0);
        painter->restore();
       
        bool clipSet=false;
        QRectF oriClipRect;
        
        if(item->type() == MgQ::LayoutItemType)
        {        
            if(MgQLayoutItem* layout=static_cast<MgQLayoutItem*>(item))       
            {                 
                if(layout->clipped())
                {
                    painter->save();
                    clipSet=true;
                    oriClipRect=painter->clipRegion().boundingRect();
                    painter->setClipRect(item->mapToScene(item->boundingRect()).boundingRect(),
                                     Qt::IntersectClip);                
                }
            }    
        }        
        
		foreach(QGraphicsItem *childItem,item->childItems())
		{
			renderItemRecursively(childItem,painter,option);
		}
		
		if(clipSet)
        {    
            painter->setClipRect(oriClipRect);
            painter->restore();       
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

