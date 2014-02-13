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
    \file QtDriver.h
    \brief Definition of QtDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Jan  4 20:28:15 2010
*/

#ifndef _MPP_QtDriver_H
#define _MPP_QtDriver_H

#include <BaseDriver.h>
#include <QtDriverAttributes.h>
#include <XmlNode.h>

#include <QColor>
#include <QMap>

//#include <Qt/qqwidget.h>

class QGraphicsItem;
class QGraphicsScene;
class QPainterPath;

class MgQHistoItem;
class MgQLayerItem;
class MgQLayoutItem;
class MgQMagnifierLayoutItem;
class MgQPatternManager;
class MgQPlotScene;
class MgQPolylineSetItem;
class MgQSceneItem;
class MgQStepItem;
class MgQSymbolItem;
class MgQSymbolManager;

class QPainterPath;

namespace magics
{

/*! \class QtDriver
    \brief This driver produces output for Qt
    \ingroup drivers

    This driver ...
*/
class QtDriver: public BaseDriver, public QtDriverAttributes
{
friend class MgQAnimationStep;
friend class MgQPlotScene;

public:
	QtDriver();
	~QtDriver();
	void open();
	void close();

	/*!
	  \brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
		if ( magCompare(node.name(), "binary") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("qt");
			QtDriverAttributes::set(basic);
		}
	}

	/*!
	  \brief sets a new map
	*/
	void set(const map<std::string, std::string>& map)
	{
		BaseDriver::set(map);
		QtDriverAttributes::set(map);
	}
	
	MAGICS_NO_EXPORT void executeStep(int step) const;
	MAGICS_NO_EXPORT void executeStep(int step,MgQLayerItem *layerItem) const;	
	MAGICS_NO_EXPORT void executeStep(int step,MgQLayerItem *layerItem,const SceneLayer& sceneLayer) const;
	MAGICS_NO_EXPORT void executeMagnifier(Layer *,MgQMagnifierLayoutItem*) const;
	MAGICS_NO_EXPORT void executeHisto(Layer *,MgQHistoItem*,QString,QString) const;
	void setUpdateMode(bool mode) { updateMode_ = mode; }
	bool getUpdateMode() const    { return updateMode_; }

	void setScene(QGraphicsScene* sc) {scene_=sc;}

private:

	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout&) const;
	MAGICS_NO_EXPORT void project(const PreviewLayout&) const;
	MAGICS_NO_EXPORT void project(const MagnifierLayout&) const;
	MAGICS_NO_EXPORT void project(const HistoLayout&) const;
	MAGICS_NO_EXPORT void project(const SceneLayout&) const;
	MAGICS_NO_EXPORT void unproject() const;
	MAGICS_NO_EXPORT void newLayer(Layer&) const;
	MAGICS_NO_EXPORT void newLayer(StaticLayer&) const;
	MAGICS_NO_EXPORT void newLayer(StepLayer&) const;
	MAGICS_NO_EXPORT void closeLayer(Layer&) const;
	MAGICS_NO_EXPORT void closeLayer(StaticLayer&) const;
	MAGICS_NO_EXPORT void closeLayer(StepLayer&) const;

	MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const MFloat w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat *x, MFloat *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat *, MFloat *) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline&) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const;
	MAGICS_NO_EXPORT void renderImage(const ImportObject&) const;
	MAGICS_NO_EXPORT bool renderPixmap(MFloat,MFloat,MFloat,MFloat,int,int,unsigned char*,int,bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;
	MAGICS_NO_EXPORT void renderSymbols(const Symbol&) const;

	//! Methods to redisplay an object (virtual).
	MAGICS_NO_EXPORT void redisplay(const Layer&) const;
	MAGICS_NO_EXPORT void redisplay(const PreviewLayout&) const;
	MAGICS_NO_EXPORT void redisplay(const MagnifierLayout&) const;
	MAGICS_NO_EXPORT void redisplay(const HistoLayout&) const;
	MAGICS_NO_EXPORT void redisplay(const SceneLayout&) const;
	MAGICS_NO_EXPORT void redisplay(const SceneLayer&) const;
	MAGICS_NO_EXPORT void redisplay(const StaticLayer&) const;
	MAGICS_NO_EXPORT void redisplay(const StepLayer&) const;
	MAGICS_NO_EXPORT void redisplay(const PolylineSet&) const;
	MAGICS_NO_EXPORT void redisplay(const Arrow&) const;
	MAGICS_NO_EXPORT void redisplay(const Flag&) const;


	// QtDriver specific member functions BEGIN
	MAGICS_NO_EXPORT void project(MgQLayoutItem*) const;
	MAGICS_NO_EXPORT void newLayer(MgQLayerItem*) const;
	MAGICS_NO_EXPORT void closeLayer(MgQLayerItem*) const;	
	MAGICS_NO_EXPORT void generateSymbolPath(MgQSymbolItem *,svgSymbol) const;
	MAGICS_NO_EXPORT void textToUnicode(const string&,QString &) const;
	MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int,MgQSymbolItem*) const;
	MAGICS_NO_EXPORT void snowflake(const MFloat, const MFloat, const MFloat,MgQSymbolItem*) const;
	MAGICS_NO_EXPORT void drizzle(const MFloat, const MFloat, const MFloat,MgQSymbolItem*) const;
	MAGICS_NO_EXPORT void triangle(const MFloat, const MFloat, const MFloat, const int, const int,MgQSymbolItem*) const;
	MAGICS_NO_EXPORT void lightning(const MFloat x, const MFloat y, const MFloat size,MgQSymbolItem*) const;

//void executeStep(const MgQAnimationStep&,MgQStepItem*) const;
	//mutable QGraphicsScene* scene_;
	// QtDriver specific member functions END

	QColor getQtColour(const Colour &) const;

	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

	//! Copy constructor - No copy allowed
	QtDriver(const QtDriver&);
	//! Overloaded << operator to copy - No copy allowed
	QtDriver& operator=(const QtDriver&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const QtDriver& p)
		{ p.print(s); return s; }

	QGraphicsScene *scene_;
	mutable int stepToRender_;
	mutable std::stack<MgQLayoutItem*>  layoutItemStack_;
	mutable std::stack<MgQLayerItem*>   layerItemStack_;
	mutable MgQSceneItem* currentSceneItem_;
	mutable QGraphicsItem* currentItem_;
	mutable bool initialized_;
	mutable bool magnifierIsBeingRedisplayed_;
	mutable MFloat magnifierZoomFactor_;
	mutable MgQSymbolManager* symbolManager_;
	mutable MgQPatternManager* patternManager_;
	mutable MgQPolylineSetItem *currentPolylineSetItem_;
	
	mutable QMap<LineStyle,Qt::PenStyle> penStyle_;
	mutable Qt::PenStyle currentPenStyle_;

	bool updateMode_;

	MFloat lineWidthFactor_;
	MFloat fontSizeFactor_;
	MFloat dpiResolutionRatio_;
};

} // namespace magics
#endif
