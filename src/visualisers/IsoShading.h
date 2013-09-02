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

/*! \file IsoShading.h
    \brief Definition of the Template class IsoShading.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
*/

#ifndef IsoShading_H
#define IsoShading_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"

#include "IsoShadingAttributes.h"
#include "Colour.h"
#include "LevelSelection.h"
#include "Polyline.h"
#include "MatrixHandler.h"
#include "LegendVisitor.h"
namespace magics {


class NoIsoShading  {

public:
	NoIsoShading() {}
	virtual ~NoIsoShading() {}
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual void toxml(ostream&, int = 0) const {}
	virtual bool accept(const string&) { return true;}
    
	virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&)
			{  }
		virtual void operator()(Data&, BasicGraphicsObjectContainer&)
				{  }
 
    
    virtual NoIsoShading* clone() {
		NoIsoShading* object = new NoIsoShading();
	    return object;
	}
    virtual CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range,
        	    		const Transformation& transformation, int width, int height,
        	    		float resolution, const string& technique);
	void close(const MatrixHandler&) const {}
	virtual bool shadingMode() { return false; }
	virtual bool hasLegend() { return false; }
	virtual int shadingIndex (double ) { return -1; }
	virtual int leftIndex (double ) { return -1; }
	virtual int rightIndex (double ) { return -1; }
	virtual double  leftRange(double) { return 0; }
	virtual double rightRange(double) { return 0; }
	virtual bool operator()(const LevelSelection&) { return false; }
	virtual void visit(LegendVisitor&) {} 
	virtual void operator()(Polyline*) const {}
	virtual void colour(double, Colour&) {};
	virtual bool needClipping() { return false;}
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "No Shading"; } 

private:
    //! Copy constructor - No copy allowed
	NoIsoShading(const NoIsoShading&);
    //! Overloaded << operator to copy - No copy allowed
	NoIsoShading& operator=(const NoIsoShading&);
	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoIsoShading& p)
		{ p.print(s); return s; }

};


class IsoShading: public NoIsoShading, public IsoShadingAttributes {

public:
	IsoShading();
	 ~IsoShading();
	void set(const map<string, string>& map ) { IsoShadingAttributes::set(map); }
	void set(const XmlNode& node ) { IsoShadingAttributes::set(node); }
	virtual void operator()(Polyline*) const;

		virtual bool accept(const string&) { return true;}
	virtual NoIsoShading* clone() const {
		IsoShading* object = new IsoShading();
		object->copy(*this);
	    return object;
	}
	 CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range,
	        	    		const Transformation& transformation, int width, int height,
	        	    		float resolution, const string& technique) {
		 return technique_->array(matrix, range, transformation, width, height, resolution, technique); }
	virtual void operator()(MatrixHandler& data, BasicGraphicsObjectContainer& parent)
		{ (*this->technique_)(data, parent); }
	virtual void operator()(Data& data, BasicGraphicsObjectContainer& parent)
			{ (*this->technique_)(data, parent); }
	virtual int     shadingIndex(double);
	virtual int  leftIndex(double);
	virtual int  rightIndex(double);

	virtual bool needClipping() { return (*this->technique_).needClipping(); }
	virtual bool operator()(const LevelSelection& list)
	{ 
		LevelSelection filter;
		for (LevelSelection::const_iterator level = list.begin(); level != list.end(); ++level) 
			if ( this->min_ <= *level && *level <= this->max_ ) 
			          filter.push_back(*level); 
		(*this->colourMethod_).prepare(filter); 
		if ( !filter.empty() && ( filter.back() == filter.front() ) )
			filter.push_back(filter.front());
		return (*this->technique_).prepare(filter, *this->colourMethod_);
	}
	// returns true, if the contouring lines have to be created... False, is the shading is finished...
	virtual void visit(LegendVisitor& legend)  { 
		legend.newLegend(); 
			(*this->technique_).visit(legend, *this->colourMethod_); }
	virtual bool shadingMode() { return (*this->technique_).shadingMode(); }
	virtual bool hasLegend() { return (*this->technique_).hasLegend(); }
	virtual void colour(double val, Colour& colour) {
		ColourTechnique::iterator icolour = colourMethod_->find(val);
		if ( icolour != colourMethod_->end() )
			colour = icolour->second.right_;
	}

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	vector<Colour> colours_;
	vector<Colour>::iterator colour_;

private:
	//! Copy constructor - No copy allowed
	IsoShading(const IsoShading&);
	//! Overloaded << operator to copy - No copy allowed
	IsoShading& operator=(const IsoShading&);


};

template <>
class MagTranslator<string, NoIsoShading > {
public:
	NoIsoShading* operator()(const string& val )
	{
		 return SimpleObjectMaker<NoIsoShading >::create(val);
	}     
	NoIsoShading* magics(const string& param)
	{
		NoIsoShading* object=0;
		ParameterManager::update(param, object);
		return object;
	}

};
} // namespace magics


#endif
