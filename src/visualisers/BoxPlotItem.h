/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotItem.h
    \brief Definition of the Template class BoxPlotItem.
    
    Magics Team - ECMWF 2006
    
    Started: Fri 29-Sep-2006
    
    Changes:
    
*/

#ifndef BoxPlotItem_H
#define BoxPlotItem_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "BoxPlotBasicItem.h"

#include "BoxPlotBoxAttributes.h"
#include "BoxPlotWhiskerLineAttributes.h"
#include "BoxPlotWhiskerBoxAttributes.h"
#include "BoxPlotBoxBorderAttributes.h"
#include "BoxPlotWhiskerBorderAttributes.h"
#include "BoxPlotMedianAttributes.h"

namespace magics {


class BoxPlotBox : public NoBoxPlotBox, public BoxPlotBoxAttributes
{
public:
	BoxPlotBox() {}
	virtual ~BoxPlotBox() {}
	
	
    
   void set(const XmlNode& node) { BoxPlotBoxAttributes::set(node);  }
   void set(const map<string, string>& map) { BoxPlotBoxAttributes::set(map); }
   bool accept(const string& node) { return BoxPlotBoxAttributes::accept(node); }
   BoxPlotBox* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotBox();
    }

    void operator()(BasicGraphicsObjectContainer&, const CustomisedPoint&) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 

private:
    //! Copy constructor - No copy allowed
	BoxPlotBox(const BoxPlotBox&);
    //! Overloaded << operator to copy - No copy allowed
	BoxPlotBox& operator=(const BoxPlotBox&);

};


class BoxPlotBoxBorder : public NoBoxPlotBoxBorder, public BoxPlotBoxBorderAttributes {

public:
	BoxPlotBoxBorder() {}
	virtual ~BoxPlotBoxBorder() {}
    
    virtual void set(const XmlNode& node) {
       BoxPlotBoxBorderAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        BoxPlotBoxBorderAttributes::set(map);
    }
    bool accept(const string& node) { return BoxPlotBoxBorderAttributes::accept(node); }
    virtual NoBoxPlotBoxBorder* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotBoxBorder();
    }
    

    virtual void operator()(Polyline&) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 



};



class BoxPlotMedian : public NoBoxPlotMedian, public BoxPlotMedianAttributes {

public:
	BoxPlotMedian() {}
	virtual ~BoxPlotMedian() {}
    
    virtual void set(const XmlNode& node) {
       BoxPlotMedianAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        BoxPlotMedianAttributes::set(map);
    }
    bool accept(const string& node) { return BoxPlotMedianAttributes::accept(node); }

    virtual BoxPlotMedian* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotMedian();
    }

    
     virtual void operator()(BasicGraphicsObjectContainer&, Polyline*) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 



};


class BoxPlotWhiskerBorder : public NoBoxPlotWhiskerBorder, public BoxPlotWhiskerBorderAttributes {

public:
	BoxPlotWhiskerBorder() {}
	virtual ~BoxPlotWhiskerBorder() {}
    
    virtual void set(const XmlNode& node) {
       BoxPlotWhiskerBorderAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        BoxPlotWhiskerBorderAttributes::set(map);
    }
    bool accept(const string& node) { return BoxPlotWhiskerBorderAttributes::accept(node); }

    virtual NoBoxPlotWhiskerBorder* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotWhiskerBorder();
    }

    void operator()(Polyline&) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 



};


	
class BoxPlotWhiskerBox : public NoBoxPlotWhisker, public BoxPlotWhiskerBoxAttributes {

public:
	BoxPlotWhiskerBox() {}
	virtual ~BoxPlotWhiskerBox() {}
    
    virtual void set(const XmlNode& node) {
       BoxPlotWhiskerBoxAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        BoxPlotWhiskerBoxAttributes::set(map);
    }
    virtual BoxPlotWhiskerBox* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotWhiskerBox();
    }
    bool accept(const string& node) { return BoxPlotWhiskerBoxAttributes::accept(node); }


    void top(BasicGraphicsObjectContainer&, const CustomisedPoint&) const;
    void bottom(BasicGraphicsObjectContainer&, const CustomisedPoint&) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 



};

class BoxPlotWhiskerLine : public NoBoxPlotWhisker, public BoxPlotWhiskerLineAttributes {

public:
	BoxPlotWhiskerLine() {}
	virtual ~BoxPlotWhiskerLine() {}
    
    virtual void set(const XmlNode& node) {
       BoxPlotWhiskerLineAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        BoxPlotWhiskerLineAttributes::set(map);
    }
    bool accept(const string& node) { return BoxPlotWhiskerLineAttributes::accept(node); }
    virtual BoxPlotWhiskerLine* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new BoxPlotWhiskerLine();
    }

    void top(BasicGraphicsObjectContainer&, const CustomisedPoint&) const;
    void bottom(BasicGraphicsObjectContainer&, const CustomisedPoint&) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 



};





}

#endif
