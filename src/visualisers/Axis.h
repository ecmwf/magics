/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Axis.h
    \brief Definition of the Template class Axis.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 7-May-2004
    
    Changes:
    
*/

#ifndef Axis_H
#define Axis_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "AxisAttributes.h"
#include "XmlNode.h"
#include "MagicsEvent.h"
namespace magics {
	
class XmlNode;

class SceneLayer;
class LayoutVisitor;
class AxisItem;

class Axis: public BasicSceneObject, public AxisAttributes  {

public:
	Axis();
	virtual ~Axis();
 
    void set(const XmlNode& node ) { AxisAttributes::set(node); }
    void set(const map<string, string>& map ) { AxisAttributes::set(map); }
    
    void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors);
    void visit(TextVisitor& );

    virtual void tick(VerticalAxisVisitor&) {}
    virtual void tick(HorizontalAxisVisitor&) {}

    virtual void minortick(VerticalAxisVisitor&) {}
    virtual void minortick(HorizontalAxisVisitor&) {}

    virtual void label(VerticalAxisVisitor&) {}
    virtual void label(HorizontalAxisVisitor&) {}

    virtual void title(VerticalAxisVisitor&) {}
    virtual void title(HorizontalAxisVisitor&) {}

    virtual void line(TopAxisVisitor& out) const 		{}
    virtual void line(BottomAxisVisitor& out) const 	{}
    virtual void line(LeftAxisVisitor& out) const 		{}
    virtual void line(RightAxisVisitor& out) const 	{}

    virtual void tip(TopAxisVisitor& out) const 		{}
       virtual void tip(BottomAxisVisitor& out) const 	{}
       virtual void tip(LeftAxisVisitor& out) const 		{}
       virtual void tip(RightAxisVisitor& out) const 	{}

    virtual void grid(DrawingVisitor&) const {}

    string createLabel(const AxisItem&);

    double min() const { return method_->min(); }
    double max() const { return method_->max(); }



protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     void ticks(double, double, vector<double>&);
     AxisItems items_;


     typedef string (Axis::*LabelHelper)(const AxisItem&);
     map<string,  LabelHelper> labelHelpers_;

     string number(const AxisItem&);
     string labellist(const AxisItem&);
     string latitude(const AxisItem&);
     string longitude(const AxisItem&);
     string basic(const AxisItem&);
     double title_position_;

     int currentLabel_;
private:
    //! Copy constructor - No copy allowed
	Axis(const Axis&);
    //! Overloaded << operator to copy - No copy allowed
	Axis& operator=(const Axis&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Axis& p)
		{ p.print(s); return s; }

};


class HorizontalAxis : public Axis 
{
public :
    HorizontalAxis();
    ~HorizontalAxis() {}
    
    template <class V> 
    void build(V& visitor) {
    	if ( items_ .empty() )  {   		
    		method_->updateX(visitor.transformation());
    	    method_->prepare(*this, items_);
    	}
    	line(visitor);
    	tick(visitor);
    	label(visitor);
    	title(visitor);
    	minortick(visitor);
    	tip(visitor);


    	
    }
    
    void visit(DrawingVisitor& visitor) {
    	if ( items_ .empty() ) {
    		method_->updateX(visitor.transformation());
    		method_->prepare(*this, items_);
    	}
    	grid(visitor);
    }
    
    void visit(TopAxisVisitor& visitor) {
    	if ( magCompare(position_, "top") )
    		build(visitor); 
    }
    void visit(BottomAxisVisitor& visitor) {  
    	if ( magCompare(position_, "bottom") ) 
    		build(visitor); 
    }
    
    void set(const XmlNode& node) { 
    	if ( magCompare(node.name(), "horizontal_axis") ) {
    		XmlNode axis = node;
    		axis.name("axis");    		
    		AxisAttributes::set(axis);
    	} 
    }

    void minortick(HorizontalAxisVisitor&);
    void tick(HorizontalAxisVisitor&);
    void label(HorizontalAxisVisitor&);
    void title(HorizontalAxisVisitor&);

    void line(TopAxisVisitor& out) const;
    void line(BottomAxisVisitor& out) const;

    void tip(TopAxisVisitor& out) const;
    void tip(BottomAxisVisitor& out) const;

    void grid(DrawingVisitor&) const;

};

class VerticalAxis : public Axis 
{
public :
    VerticalAxis();
    ~VerticalAxis() {}
    template <class V>
    void build(V& visitor) {
    	if ( items_ .empty() ) {
    		method_->updateY(visitor.transformation());
    		method_->prepare(*this, items_);
    	}
       line(visitor);
       tick(visitor);
       label(visitor);
       title(visitor);
       minortick(visitor);
       tip(visitor);

        
    }
    void visit(DrawingVisitor& visitor) {
      	if ( items_ .empty() ) {
      		method_->updateY(visitor.transformation());
      		method_->prepare(*this, items_);
      	}
      	grid(visitor);
      }
    void visit(LeftAxisVisitor& visitor) { 
    	if ( magCompare(position_, "left") )
    		build(visitor); 
    }
    void visit(RightAxisVisitor& visitor) { 	
    	if ( magCompare(position_, "right") )
    		build(visitor); 
    }
    void set(const XmlNode& node) { 
    	if ( magCompare(node.name(), "vertical_axis") ) {
    		XmlNode axis = node;
    		axis.name("axis");    		
    		AxisAttributes::set(axis);
    	} 
    }
    void tick(VerticalAxisVisitor&);
    void minortick(VerticalAxisVisitor&);
    void label(VerticalAxisVisitor&);
    void title(VerticalAxisVisitor&);

    void line(LeftAxisVisitor& out) const;
    void line(RightAxisVisitor& out) const;

    void tip(LeftAxisVisitor& out) const;
    void tip(RightAxisVisitor& out) const;

    void grid(DrawingVisitor&) const;
};

} // namespace magics
#endif
