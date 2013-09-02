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
    \brief Definition of MgQSymbol.
    \author Graphics Section, ECMWF

    Started: Feb 2010
*/

#ifndef _MgQSymbol_H
#define _MgQSymbol_H

#include <QBrush>
#include <QGraphicsItem>
#include <QList>
#include <QPainterPath>
#include <QPen>
#include <QString>

#include "MgQ.h"

class MgQPainterPath : public QPainterPath
{
public:
	MgQPainterPath(bool filled,bool defaultFill=false) :
		filled_(filled),defaultFill_(defaultFill), renderOnlyForOutline_(false) {};
	bool isFilled() const {return filled_;}
	bool isDefaultFill() const {return defaultFill_;}
	void setRenderOnlyForOutline(bool b) {renderOnlyForOutline_=b;}
	bool renderOnlyForOutline() const {return renderOnlyForOutline_;}

  protected:
	bool filled_;
	bool defaultFill_;
	bool renderOnlyForOutline_;
};

class MgQSymbolItem
{
public:
	MgQSymbolItem(QString id,float s) : id_(id), size_(s) {};

	const QString&  id() const   {return id_;}
	float size()  {return size_;};
	bool equal(const QString&,const float);
	const QList<MgQPainterPath>& paths() const {return paths_;}
	void addPath(MgQPainterPath &p) {paths_.push_back(p);}
	bool hasFilledPart();

protected:
	QString id_;
	float size_;
	QList<MgQPainterPath> paths_;	
};

class MgQSymbolManager : public QList<MgQSymbolItem*>
{
public:
	MgQSymbolManager() {};
	~MgQSymbolManager();
 
	MgQSymbolItem* addSymbol(const QString&,const float);
	MgQSymbolItem* getSymbol(const QString&,const float);	
	void deleteSymbol(const QString&,const float);		

	int    symbolNum() {return symbols_.count();};	

protected:
	QList<MgQSymbolItem*> symbols_;
};	
	
class MgQSymbolSetItem : public QGraphicsItem
{
public:
	enum {Type = MgQ::SymbolSetItemType}; 
	
	MgQSymbolSetItem(MgQSymbolItem* symbol,QRectF &boundingRect,
				QGraphicsItem* parent = 0) : 
				QGraphicsItem(parent), symbol_(symbol),
				boundingRect_(boundingRect), keepSizeWhenScaling_(false),
				outline_(false), connectLine_(false) {};

	QRectF boundingRect() const;
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *widget=0);
	int type() const {return Type;}

	void setColor(QColor);
	void setOutlineColor(QColor);
	void addPoint(double,double);
	void setKeepSizeWhenScaling(bool b) {keepSizeWhenScaling_=b;}
	bool connectLine() {return connectLine_;}
	void setConnectLine(bool b) {connectLine_=b;}
	void setConnectLinePen(QPen);

protected:
	QList<double> xp_;
	QList<double> yp_;
	MgQSymbolItem* symbol_;
	QPen pen_;
	QList<QBrush> brushes_;
	QRectF boundingRect_;	
	bool keepSizeWhenScaling_;
	bool outline_;
	bool connectLine_;
	QPen connectLinePen_;
};




#endif 
