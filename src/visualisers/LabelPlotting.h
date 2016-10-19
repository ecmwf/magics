/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LabelPlotting.h
    \brief Definition of the Template class LabelPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 2-Feb-2004
    
    Changes:
    
*/

#ifndef LabelPlotting_H
#define LabelPlotting_H

#include "magics.h"

#include "SceneVisitor.h"
#include "LabelPlottingAttributes.h"
#include "UserPoint.h"
#include "Text.h"
#include "Symbol.h"
#include "VectorOfPointers.h"
#include "Transformation.h"


namespace magics {

class NoGridPlotting;



class NoLabelPlotting {

public:
	NoLabelPlotting() {}
	virtual ~NoLabelPlotting() {}
    
    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual void toxml(ostream&, int = 0) const {} 
    virtual bool accept(const string&) { return false; }
    virtual NoLabelPlotting* clone() const { return new NoLabelPlotting(); }
    virtual void operator()(DrawingVisitor&) {}
    virtual void prepare(NoGridPlotting&) {}
    
    virtual void operator()(LeftAxisVisitor&) {}
    virtual void operator()(RightAxisVisitor&) {}
    virtual void operator()(BottomAxisVisitor&) {}
    virtual void operator()(TopAxisVisitor&) {}
    
    virtual void layer(BasicGraphicsObjectContainer*) {}
     virtual void label(Transformation&);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "NoLabelPlotting\n"; } 

private:
    //! Copy constructor - No copy allowed
	NoLabelPlotting(const NoLabelPlotting&);
    //! Overloaded << operator to copy - No copy allowed
	NoLabelPlotting& operator=(const NoLabelPlotting&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoLabelPlotting& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, NoLabelPlotting> { 
public:
	NoLabelPlotting* operator()(const string& val )
	{
		return SimpleObjectMaker<NoLabelPlotting>::create(val);
	}     

	NoLabelPlotting* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

class LabelPlotting: public NoLabelPlotting, public LabelPlottingAttributes {

public:
	LabelPlotting();
	virtual ~LabelPlotting();
	void prepare(NoGridPlotting& grid);
	
	virtual LabelPlotting* clone() const
	{
		LabelPlotting* object = new LabelPlotting();
		object->copy(*this);
		return object;
	}
	virtual void set(const XmlNode& node) { LabelPlottingAttributes::set(node); }
    virtual void set(const map<string, string>& map) { LabelPlottingAttributes::set(map);}
	
	bool accept(const string& node) { return LabelPlottingAttributes::accept(node); }

	template <class V>
	void label(V& visitor) {
		if (!layer_) layer_ = &visitor.layout();
		visitor.transformation().labels(*this, visitor);
		layer_ = 0;
	}
	void operator()(DrawingVisitor& visitor)       { label(visitor); }
	void operator()(LeftAxisVisitor& visitor)       { if ( left_ ) label(visitor); }
	void operator()(RightAxisVisitor& visitor)     { if ( right_ ) label(visitor); }
	void operator()(BottomAxisVisitor& visitor)  { if ( bottom_ ) label(visitor); }
	void operator()(TopAxisVisitor& visitor)        { if (top_) label(visitor); }
	void layer(BasicGraphicsObjectContainer* layer) { layer_ = layer; }
	void label(Transformation&);
	void add(Text* text) const
	{
		MagFont font(font_, font_style_, height_);
		font.colour(*colour_);
		text->setFont(font);
		text->setBlanking(blanking_);
		layer_->push_back(text);
	}

	const vector<double>& longitudes() const { return longitudes_; }
	const vector<double>& latitudes()  const { return latitudes_; }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const;  
	vector<double> latitudes_;
	vector<double> longitudes_;
	BasicGraphicsObjectContainer* layer_;
private:
	//! Copy constructor - No copy allowed
	LabelPlotting(const LabelPlotting&);
	//! Overloaded << operator to copy - No copy allowed
	LabelPlotting& operator=(const LabelPlotting&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LabelPlotting& p)
		{ p.print(s); return s; }

};


} // namespace magics
#endif
