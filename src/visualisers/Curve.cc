/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Curve.cc
    \brief Implementation of the Template class Curve.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/



#include "Curve.h"
#include "PointsHandler.h"
#include "LegendVisitor.h"
#include "Symbol.h"

using namespace magics;


template <class T, class O>
void setHandler(const vector<string>& keys, vector<T>& values, ListPolicy policy, const T& def, map<string, O>& handler) 
{
	if (values.empty())
		values.push_back(def);

	typename vector<T>::const_iterator value = values.begin();
	MagTranslator<T, O> translator;
	for (vector<string>::const_iterator key = keys.begin(); key != keys.end(); ++key) {
		
		handler.insert(make_pair(*key, translator(*value)));
		value++;
		if ( value == values.end() )
			value = ( policy ==  M_CYCLE ) ? values.begin() : --value;
	}
}

Curve::Curve() 
{
	missingMethods_["ignore"] = &Curve::ignore;
	missingMethods_["join"] = &Curve::join;
	missingMethods_["drop"] = &Curve::drop;

	curveMethods_["straight"] = &Curve::straight;
	curveMethods_["stepped"] = &Curve::stepped;
	

	
}


Curve::~Curve() 
{}

/*!
 Class information are given to the output-stream.
*/		
void Curve::print(ostream& out)  const
{
	out << "Curve[";
	out << "]";
}

magics::Polyline* Curve::newCurve(BasicGraphicsObjectContainer& task) const
{

    magics::Polyline* curve  = new magics::Polyline();
	

	(*curve).setColour(currentColour_);
	(*curve).setLineStyle(currentStyle_);
	(*curve).setThickness(currentThickness_);


	return curve;
	
}
	
bool  Curve::missing(CustomisedPoint& point) const
{
	if ( point.missing() ) {
		cout << "FOUND MISSING" << endl;
		return true;
	}
	double x = point["x"];

	if ( x < this->x_below_ ) return true;
	if ( x > this->x_above_ ) return true;
	if ( same(x, this->x_below_) ) return true;
	if ( same(x, this->x_above_) ) return true;

	double y = point["y"];

	if ( y < this->y_below_ ) return true;
	if ( y > this->y_above_ ) return true;
	if ( same(y, this->y_below_) ) return true;
	if ( same(y, this->y_above_) ) return true;
	return false;
}

void  Curve::stepped(const UserPoint& point, vector<UserPoint>& out)
{


}

template <class T>
const T& get(const map<string, T>& handler, const string& key, const T& def) 
{
	
	typename std::map<string, T>::const_iterator value = handler.find(key);
	if ( value == handler.end() ) 
		return def;
	return value->second;

}

void Curve::operator()(Data& data, BasicGraphicsObjectContainer& task)
{

	vector<double> xpos;
	vector<double> ypos;
    const Transformation& transformation = task.transformation();


    std::set<string> needs;
    std::map<string, string> info;

    currentColour_ = *colour_;
	currentThickness_ = thickness_;
	currentStyle_ = style_;
    
    if ( magCompare(style_setting_, "advanced")  ) {
    	string style = "solid";
 		setHandler(colour_keys_, colour_list_, colour_policy_, colour_->name(), colourHandler_);
		setHandler(thickness_keys_, thickness_list_, thickness_policy_, thickness_, thicknessHandler_);
		setHandler(style_keys_, style_list_, style_policy_, style, styleHandler_);

    	info.insert(make_pair(style_key_, ""));
    	info.insert(make_pair(colour_key_, ""));
    	info.insert(make_pair(thickness_key_, ""));
		data.getInfo(info);
		
		string colour = get(colourHandler_, info[colour_key_], currentColour_.name());
		currentColour_ = Colour(colour);
		currentThickness_ = get(thicknessHandler_, info[thickness_key_], currentThickness_);
		currentStyle_ = get(styleHandler_, info[style_key_], currentStyle_);
	}


	

	
	


    CustomisedPointsList raw, points;

    data.customisedPoints(transformation, needs, points, true);

	if ( legend_text_ == "?" ) legend_text_ = data.legend(); 
	
    magics::Polyline* curve_ = newCurve(task);
	bool last_out = false;	
	
	vector<PaperPoint> missing, sv;
	PaperPoint last, current, toadd;
	
	CustomisedPointsList::iterator point = points.begin();

	// we clean the list;



    while ( point != points.end() ) {
    	if ( this->missing(**point) ) {
    		++point;
    	}
    	else
    		break;
    }
    
    if ( point == points.end() )
    	return;

    PaperPoint last_current = transformation(UserPoint((**point)["x"], (**point)["y"]));

    while ( point != points.end() ) {

    	vector<UserPoint> todo;
    	UserPoint up((**point)["x"], (**point)["y"]);


    		current = transformation(up);
    		if (!this->missing(**point) ) {
    			if ( last_out ) {
    				bool result;
    				std::map<string, MissingMethod>::iterator method = missingMethods_.find(lowerCase(missing_mode_));
    				result = (method == missingMethods_.end() ) ?
    						ignore(last, missing.front(), missing, task) :
    						(this->*method->second)(last, current, missing, task);
    				if ( result ) {
    					if ( line_ )
    						transformation(*curve_, task);
    					curve_ = newCurve(task);
    				}
    				missing.clear();
    			}

    			if ( magCompare(plot_method_, "stepped" ) ) {
    				if ( current.x_ != last_current.x_ && current.y_ != last_current.y_ ) {
    					toadd = PaperPoint((current.x_ + last_current.x_)/2., last_current.y_);
    					curve_->push_back(toadd);
    					sv.push_back(toadd);
    					toadd = PaperPoint((current.x_ + last_current.x_)/2., current.y_);
    				}
                    else 
                        toadd = current;

    			}
    			else
    				toadd = current;

    			last = toadd;
    			last_current = current;

    			last_out = false;
    			curve_->push_back(toadd);
    			sv.push_back(toadd);

    		}
    		else {
    			missing.push_back( current );
    			last_out = true;
    		}

		++point;

	}
    // add the last current if stepped method ..
    if ( magCompare(plot_method_, "stepped" ) ) {
    	curve_->push_back(current);
    	sv.push_back(current);
    }

    if ( missing.empty() == false ) {

    	std::map<string, MissingMethod>::iterator method = missingMethods_.find(lowerCase(missing_mode_));
    	if ( method == missingMethods_.end() )
    	{
    		ignore(last, missing.front(), missing, task);
    	}
    	(this->*method->second)(last, missing.front(), missing, task);

    }

	
	
	// apply the symbol

	if ( line_ ) //task.push_back(curve_);
		transformation(*curve_, task);
	 symbol(sv, task);

}

void CurveArea::operator()(Data& data, BasicGraphicsObjectContainer& task)
{

	vector<double> xpos;
	vector<double> ypos;
    const Transformation& transformation = task.transformation();

    currentColour_ = *colour_;
	currentThickness_ = thickness_;
	currentStyle_ = style_;
    
    std::set<string> needs;
    CustomisedPointsList points;
    if ( shade_->needCustomised() ) {
    	needs.insert("area");
    }
    data.customisedPoints(transformation, needs, points, true);

	if ( legend_text_ == "?" ) legend_text_ = data.legend();


    magics::Polyline* curve_ = newCurve(task);
	bool last_out = false;

	vector<PaperPoint> missing, sv;
	PaperPoint last, current;

	CustomisedPointsList::iterator point = points.begin();

    while ( point != points.end() ) {

    	if ( this->missing(**point) ) {
    		++point;
    	}
    	else
    		break;
    }



    while ( point != points.end() ) {

    	UserPoint up((**point)["x"], (**point)["y"]);

    	current = transformation(up);

		if (!this->missing(**point) ) {
			if ( last_out ) {
				bool result;
				std::map<string, MissingMethod>::iterator method = missingMethods_.find(lowerCase(missing_mode_));
				result = (method == missingMethods_.end() ) ?
							ignore(last, missing.front(), missing, task) :
							(this->*method->second)(last, missing.front(), missing, task);
				if ( result ) {
					if ( line_ )
						transformation(*curve_, task);
					curve_ = newCurve(task);
				}
				missing.clear();
			}

			last = current;
			last_out = false;
			curve_->push_back(current);
			sv.push_back(current);

		}
		else {
			missing.push_back( current );
			last_out = true;
		}
		++point;

	}
    if ( missing.empty() == false ) {
    	std::map<string, MissingMethod>::iterator method = missingMethods_.find(lowerCase(missing_mode_));
    	if ( method == missingMethods_.end() )
    	{
    		ignore(last, missing.front(), missing, task);
    	}
    	(this->*method->second)(last, missing.front(), missing, task);

    }


	// apply the symbol

	(*shade_)(*curve_);
	if ( line_ ) {
		task.push_back(curve_);
	} //transformation(*curve_, task);
	 symbol(sv, task);
	
}


class CurveEntry : public LegendEntry
{
public:
	CurveEntry(const string& legend, Curve& curve) : 
		LegendEntry(legend), curve_(curve) {}
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) 
		{ curve_.set(point, legend, *this); }
protected:
	Curve& curve_;
};

void Curve::visit(LegendVisitor& legend)
{
	if ( !legend_) return;
	CurveEntry* entry = new CurveEntry(legend_text_, *this);
	entry->userText(legend_text_, "user");
	legend.add(entry);
}

void  Curve::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend, LegendEntry& entry)
{
    magics::Polyline* curve  = new magics::Polyline();
	curve->setColour(currentColour_);
	curve->setLineStyle(currentStyle_);
	curve->setThickness(currentThickness_);
	
	double width = entry.computeWidth(0.8)/2;

	PaperPoint p = entry.centreSymbolBox(point);

	
	double x = p.x();
	double y = point.y();

	curve->push_back(PaperPoint(x-width, y));
	curve->push_back(PaperPoint(x+width,  y));
	

	this->legend(*curve);
	
	    
	legend.push_back(curve);
	legend_symbol(p, legend);
	
}
void CurveArea::legend(magics::Polyline& curve)
{
	shade_->legend(curve);
}
void Curve::symbol(vector<PaperPoint>& points, BasicGraphicsObjectContainer& out)
{
	if ( !symbol_ ) return;

	Symbol* symbol = new Symbol();

    symbol->setMarker(symbol_marker_);
    symbol->setHeight(symbol_height_);
    symbol->setColour(*symbol_colour_);
    symbol->outline(outline_, *outline_colour_, outline_thickness_, outline_style_);


	const Transformation& transformation = out.transformation();

    for ( vector<PaperPoint>::const_iterator point = points.begin(); point != points.end(); ++point) {
    	if ( !point->missing() && transformation.in(*point) )
    		symbol->push_back(transformation(*point));
    }

    out.push_back(symbol);
}

void Curve::legend_symbol(PaperPoint& point, BasicGraphicsObjectContainer& task)
{
	if ( !symbol_ ) return;

	Symbol* symbol = new Symbol();
    symbol->setMarker(symbol_marker_);
    symbol->setHeight(symbol_height_);
    symbol->setColour(*symbol_colour_);
    symbol->outline(outline_, *outline_colour_, outline_thickness_, outline_style_);
    symbol->push_back(point);

    task.push_back(symbol);
}


bool Curve::ignore(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&)
{
	return true; // need new line!
}

bool Curve::join(const PaperPoint& p1, const PaperPoint& p2, const vector<PaperPoint>& , BasicGraphicsObjectContainer& task)
{
	if ( !task.transformation().in(p2) )
		return true;
    magics::Polyline* curve  = new magics::Polyline();
	(*curve).setColour(*missing_colour_);
	(*curve).setLineStyle(missing_style_);
	(*curve).setThickness(missing_thickness_);

	curve->push_back(p1);
	curve->push_back(p2);
	task.transformation()(*curve, task);
	return true;
}
bool Curve::drop(const PaperPoint& p1, const PaperPoint& p2, const vector<PaperPoint>& points, BasicGraphicsObjectContainer& task)
{
    magics::Polyline* curve  = new magics::Polyline();
	(*curve).setColour(*missing_colour_);
	(*curve).setLineStyle(missing_style_);
	(*curve).setThickness(missing_thickness_);

	curve->push_back(p1);
	for ( vector<PaperPoint>::const_iterator point = points.begin(); point != points.end(); ++point)
		curve->push_back(*point);
	curve->push_back(p2);
	task.transformation()(*curve, task);
	return true;
}
