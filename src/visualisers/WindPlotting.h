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

/*! \file WindPlotting.h
    \brief Definition of the Template class WindPlotting.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#ifndef WindPlotting_H
#define WindPlotting_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "WindPlottingAttributes.h"
#include "IntervalMap.h"
#include "Transformation.h"

namespace magics {



class WindPlotting  :public WindPlottingAttributes,
public LevelSelectionInterface,
public ColourTechniqueInterface
{

public:
	WindPlotting();
	virtual ~WindPlotting() {}
	virtual void set(const map<string, string>& map) { WindPlottingAttributes::set(map);  } ;
	virtual void set(const XmlNode& node) { WindPlottingAttributes::set(node); }
	virtual bool accept(const string& key) { return WindPlottingAttributes::accept(key); }
	virtual void toxml(ostream&) {}
	virtual WindPlotting* clone() { return 0; };
	virtual void operator()(bool, const PaperPoint&, double, double, double = 0) {}
	virtual bool operator()(Data& data, BasicGraphicsObjectContainer& parent) { return false; }
	virtual void prepare(BasicGraphicsObjectContainer&,double) {}
	virtual void prepare(BasicGraphicsObjectContainer&) {}
	virtual void finish(BasicGraphicsObjectContainer&) {}
	virtual void visit(LegendVisitor&);
	virtual void visit(Data&, PointsHandler&, HistoVisitor&) {};
	virtual void adjust( CustomisedPointsList&,const Transformation&);
	bool needLegend() { return legend_; }
	void legendOnly(bool legend) { legendOnly_ = legend; }
	Colour& colour(Colour& Colour, double, double, double);
	
	Colour& off(Colour& colour, double, double, double) { return colour; }
	Colour& advanced(Colour& colour, double, double, double);
	void offMinMax(double&, double&);
	void advancedMinMax(double&, double&);
	
	double value(double, double, double);	
	double speed(double, double, double);
	double parameter(double, double, double);	
	int getCount() const { return count_; }
	int getTolerance() const { return tolerance_; }
	double getReference() const { return reference_; }
	double getInterval() const { return interval_; }
	double getMin() const { return min_; }
	double getMax() const { return max_; }
	const Colour& getMinColour() const { return *minColour_; }
	const Colour& getMaxColour() const { return *maxColour_; }
	const string& getDirection() const { return direction_; }
	ListPolicy getPolicy() const { return M_LASTONE; }
	stringarray getColours()  const { return colours_; }
	floatarray getList()  const { return list_; }
protected:
	bool legendOnly_;
     	 //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const
	 {  out << "WindPlotting"; }
	 typedef Colour& (WindPlotting::*AdvancedMethod)(Colour&, double, double, double);
	 typedef double (WindPlotting::*ColouringMethod)(double, double, double);
	 typedef void (WindPlotting::*MinMaxMethod)(double&, double&);
	 typedef void (WindPlotting::*SettingMethod)(double&, double&);
	 static map<string,  WindPlotting::AdvancedMethod > methods_;
	 static map<string,  WindPlotting::ColouringMethod > colouringMethods_;
	 static map<string,  WindPlotting::MinMaxMethod > minMaxMethods_;
	 static map<string,  WindPlotting::SettingMethod > settingMethods_;

	 void setAdvanced(double&, double&);
	 void setNormal(double&, double&) {}

	 IntervalMap<Colour>  map_;
	 virtual double minSpeed() { assert(false); }
	 virtual double maxSpeed() { assert(false); }
	 

private:
    //! Copy constructor - No copy allowed
	WindPlotting(const WindPlotting&);
    //! Overloaded << operator to copy - No copy allowed
	WindPlotting& operator=(const WindPlotting&);
	
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const WindPlotting& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, WindPlotting> { 
public:
	WindPlotting* operator()(const string& val ) {
		 return SimpleObjectMaker<WindPlotting>::create(val);
	}     
	WindPlotting* magics(const string& param)
	{
		WindPlotting* object=0;
		ParameterManager::update(param, object);
		return object;
	}

};
} // namespace magics

#endif
