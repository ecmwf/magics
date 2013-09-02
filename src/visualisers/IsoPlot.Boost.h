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

/*! \file IsoPlot.h
    \brief Definition of the Template class IsoPlot.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 3-Mar-2004
    
    Changes:
    
*/

#ifndef IsoPlot_H
#define IsoPlot_H

#include "magics.h"

#include "IsoPlotAttributes.h"
#include "BasicSceneObject.h"
#include "Polyline.h"
#include "VectorOfPointers.h"
#include "XmlNode.h"
#include "IsoHelper.h"
#include "Timer.h"


namespace magics {


template <class P > class Cell;

template<class P> class CellArray : public VectorOfPointers<vector<Cell<P>* > > {
public:
	CellArray(MatrixHandler<P>& data, IntervalMap<int>& range, const Transformation& transformation, int, int);
	int rows_;
	int columns_;
	double missing_;
	
	Matrix points_;
	IntervalMap<int> rangeFinder_;

	Cell<P>* operator()(int row, int column) const {
		return (*this)[row*columns_ + column];
	}
	
	
	double value(const pair<int, int>& pos)  const {
		return points_(pos.first, pos.second);
	}
	
	double row(const pair<int, int>& pos)  const {
			return points_.row(pos.first, pos.second);
	}
	
	double column(const pair<int, int>& pos) const {
				return points_.column(pos.first, pos.second); 
	}
	double range(const pair<int, int>& pos)  const {
					return rangeFinder_.find(points_(pos.first, pos.second), -1);	
		}

	~CellArray() { }
};

enum RangeType { outOfRange, singleRange, multipleRange };

template <class P>
class Cell 
{
public:	
	Cell(const CellArray<P>& parent) : parent_(parent), missing_(parent.missing_) { } 
	Cell(const CellArray<P>& parent, int row, int column):  parent_(parent), row_(row), column_(column)  {
		indexes_[0] = make_pair(row_, column_);
		indexes_[1] = make_pair(row_, column_+1);
		indexes_[2] = make_pair(row_+1, column_+1);
		indexes_[3] = make_pair(row_+1, column_);
			
		min_  = 100000000;
		max_ = -min_;
		outOfRange_ = 0;
		
		for ( int i = 0; i < 4; i++ ) {
			
		    
			int index = range(i);
		    if (index != -1 ) {
		    	if (  index   < min_ ) min_ =   index; 
		    	if (  index   > max_ ) max_ = index; 	
		    }
		    else 
		    	outOfRange_++;
		  
		}
        
        missing_ = parent_.missing_;
        
       
	}
	
	virtual ~Cell() {  }
	

	const CellArray<P>& parent_;
	int row_;
	int column_;
	
	 
	
	RangeType range() {
		if ( outOfRange_ == 4 ) 
			return outOfRange;
		if ( min_ == max_ && !outOfRange_ )
			return singleRange;
		return multipleRange;	
	}

  

	
	double missing_;
	
	bool missing(int i) 
		{ return (same(missing_, this->parent_.value(this->indexes_[i])));	 }
	
	
	int min_;
	int max_;
	
	int outOfRange_;

	pair<int, int> indexes_[4];
	
	
	
	
	const pair<int, int>& index(int i) {
		assert(i < 4);
		return indexes_[i];		
	}
	
	virtual double value(int i) const {
		return this->parent_.value(this->indexes_[i]);		
	}
	
	virtual double height(int i, double val) const {
		    double value = this->parent_.value(this->indexes_[i]);
		    
			return  ( same(value, missing_) ) ? 0 : value - val;					
	}
	virtual int coef(int i, double val) const {
		double height = this->parent_.value(this->indexes_[i])-val;		
		return ( height ) ? (height/abs(height))+1 : 1;		
	}

	virtual bool isMissing(int i) const {
		return (same(missing_, this->parent_.value(this->indexes_[i])));		
	}
	
	virtual double column(int i) const {
		return parent_.column(this->indexes_[i]);
		}
	void xysect(int i, int j, double value, double& x, double& y) const {
	
		double v1 = this->parent_.value(this->indexes_[i])-value;
		double v2 = this->parent_.value(this->indexes_[j])-value;
		
		double x1 = this->parent_.column(this->indexes_[i]);
	    double x2 = this->parent_.column(this->indexes_[j]);
	    double y1 = this->parent_.row(this->indexes_[i]);
	    double y2 = this->parent_.row(this->indexes_[j]);
		x = (v2*x1-v1*x2)/(v2-v1);
		y = (v2*y1-v1*y2)/(v2-v1);
	}
	
	virtual double row(int i) {
		return this->parent_.row(this->indexes_[i]);
	}		
	virtual int range(int i) {
			return this->parent_.range(this->indexes_[i]);
		}		
	virtual int findRange(double value) {
		return this->parent_.rangeFinder_.find(value, -1);
	}
};


template <class P>
CellArray<P>::CellArray(MatrixHandler<P>& data, IntervalMap<int>& range, const Transformation& transformation, int width, int height) : rangeFinder_(range)
{
	//data_(data),, transformation_(transformation) 
	rows_ = height/4;
	columns_ = width/4;

	points_.set(rows_+1, columns_+1);		
	this->reserve(rows_* columns_);	

//	int i = 0;

	missing_ = data.missing();
	double x = transformation.getMinPCX();
	double y =  transformation.getMinPCY();
 
	double stepx =  ( transformation.getMaxPCX() -  transformation.getMinPCX() )/ (columns_);
	double stepy =  ( transformation.getMaxPCY() -  transformation.getMinPCY() )/ (rows_);

	{
		Timer timer("matrix", "prepare");
		
		vector<pair<double, double> > xypoints;
		vector<pair<double, double> > geopoints;
		xypoints.reserve(rows_+1 * columns_+1);
		for (int row = 0; row <= rows_; row++) {
			    x =  transformation.getMinPCX();
			    points_.rowsAxis().push_back(y);
				for (int column = 0; column <= columns_; column++) {   
					xypoints.push_back(make_pair(x, y));				
					if ( row == 0) {
						points_.columnsAxis().push_back(x);
						
					}
					x += stepx;			
				}
				
				y += stepy;				
		}
		transformation.revert(xypoints, geopoints);
		vector<pair<double, double> >::iterator geo= geopoints.begin();
		double min =  data.min();
		double max =  data.max();
		double missing =  data.missing();
		MagLog::dev() << "min = " << data.min() << "  max = " << data.max() << endl;
		for (vector<pair<double, double> >::iterator xy = xypoints.begin(); xy != xypoints.end(); ++xy) {
					double value = data.interpolate(geo->second, geo->first);
					
					if (value != missing) {
						if (value < min) 							
							value = min;
						if (value > max) 
							value=max;													
					}
																			
					points_.push_back(value);
					++geo;					
		}
		points_.setMapsAxis();
		
	}

	for (int row = 0; row < rows_; row++) 
		for (int column = 0; column < columns_; column++) {
			this->push_back(new Cell<P>(*this, row, column));
		}
}

template <class P>
class CellBox;

template <class P>
class IsoPlot: public IsoPlotAttributes<P> {

public:
	IsoPlot();
	virtual ~IsoPlot();
	
	// Implements the Visdef Interface...
	virtual void operator()(MatrixHandler<P>&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);

	void set(const map<string, string>& map ) {
		IsoPlotAttributes<P>::set(map); 
	}

	void set(const XmlNode& node )  {
		IsoPlotAttributes<P>::set(node); 
	}

	void toxml(ostream&, int)  const {
		//void toxml(ostream& out, int tabs)  const {
		//IsoPlotAttributes<P>::toxml(out, tabs); 
	}

	void setTag(const string&)  {
		//IsoPlotAttributes<P>::setTag(tag); 
	}

	void adjust(double min, double max) 
	{ min_ = min; max_ = max; }

	virtual IsoPlot<P>* clone() const { 
		IsoPlot<P>* object = new IsoPlot<P>();
		object->copy(*this);
		return object;
	}

	Colour colour( double value ) const {
		return this->shading_->colour(value);
	}

	bool reshape(Polyline&, Polyline&);
	void reshape(const Colour&, Polyline&);
	bool reduce( list<Polyline*>&, list<Polyline*>::iterator&);
	void isoline(Cell<P>&, CellBox<P>* = 0) const;
	virtual bool needIsolines() const { return true; }
	void operator()(Data<P>& data, BasicGraphicsObjectContainer& parent) {
			(*this->shading_)(data, parent); 
		}

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

	void isoline(MatrixHandler<P>&, BasicGraphicsObjectContainer&);
	
	bool prepare(MatrixHandler<P>&);
	
	double min_;
	double max_;

        vector<vector<Polyline* >* > lines_;

		double missing_;
		vector<double>  levels_;
		bool shadingMode_; 
		
		vector<IsoData*> segments_; 
		
		map<Colour, IsoData*> colourShapes_; 
		int threads_;
		
	
	

private:
	//! Copy constructor - No copy allowed
	IsoPlot(const IsoPlot&);
	//! Overloaded << operator to copy - No copy allowed
	IsoPlot& operator=(const IsoPlot&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const IsoPlot<P>& p)
		{ p.print(s); return s; }
    
};

template <class P>
class NoIsoPlot : public IsoPlot<P>
{
public:
	NoIsoPlot() { this->setTag("noisoline"); };
	~NoIsoPlot() {};
    
	// Implements the Visualiser Interface...
	void operator()(MatrixHandler<P>&, BasicGraphicsObjectContainer&);
	
	void set(const XmlNode& node)  {
		if ( magCompare(node.name(), "noisoline")  ) {
			XmlNode iso = node;
			iso.name("isoline");
		    IsoPlotAttributes<P>::set(iso);
		} 
		else 
			IsoPlotAttributes<P>::set(node);
	}

	IsoPlot<P>* clone() const
	{ 
		IsoPlot<P>* object = new NoIsoPlot<P>();
		return object;
	}
	bool needIsolines() { return false; }
	virtual void visit(LegendVisitor&);
protected:
 	void print(ostream& out) const 
 	{ out << "NoIsoPlot" << "\n"; }
};


template<class P>
class MagTranslator<string, IsoPlot<P> > { 
public:
	IsoPlot<P>* operator()(const string& val) {
		return SimpleObjectMaker<IsoPlot<P> >::create(val);
	}     

	IsoPlot<P>* magics(const string& param)
	{
		IsoPlot<P>* object=0;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics

#include "IsoPlot.cc"
#endif
