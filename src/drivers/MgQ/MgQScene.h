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
    \file MgQScene.h
    \brief Definition of MgQScene.
    \author Graphics Section, ECMWF

    Started: June 2010
*/

#ifndef MgQScene_H
#define MgQScene_H

#include <QGraphicsScene>

class MgQRootItem;

class MgQScene : public QGraphicsScene
{

public:
	MgQScene(QObject *parent=0);
	MgQScene(MgQRootItem *, QObject *parent=0); 
	~MgQScene();

	void renderContents(QPainter *, const QRectF &, const QRectF &);
	void renderContents(QPainter *, const QStyleOptionGraphicsItem *,const QRectF &, const QRectF &,bool renderAllItems=false);
	
	MgQRootItem* plotRootItem() {return plotRootItem_;}	
	MgQRootItem* annotationRootItem() {return annotationRootItem_;}
	
	void addPlotRootItemToScene();
	void removePlotRootItemFromScene();

protected:
	void renderItemRecursively(QGraphicsItem*,QPainter *,const QStyleOptionGraphicsItem *);
	bool checkItemIsVisible(QGraphicsItem *);
	bool checkItemType(QGraphicsItem *,int);

	MgQRootItem* plotRootItem_;
	MgQRootItem* annotationRootItem_;
};


#endif
