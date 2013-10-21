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

/*! \file Coordinate.h
    \brief Definition of the Template class Coordinate.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 10-Aug-2006
    
    Changes:
    
*/

#ifndef Coordinate_H
#define Coordinate_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"

#include "XRegularCoordinateAttributes.h"
#include "YRegularCoordinateAttributes.h"

#include "XLogarithmicCoordinateAttributes.h"
#include "YLogarithmicCoordinateAttributes.h"

#include "XDateCoordinateAttributes.h"
#include "YDateCoordinateAttributes.h"

#include "XHyperCoordinateAttributes.h"
#include "YHyperCoordinateAttributes.h"
#include "DateTime.h"
#include "UserPoint.h"

#include "XmlNode.h"

#include <limits>
namespace magics {

class XmlNode;

class Coordinate {

public:
	Coordinate();
	virtual ~Coordinate();
	
	virtual double min() { return 0; }
    virtual double max() { return 100; }
    virtual double minpc() { return 0; }
    virtual double maxpc() { return 100; }
    virtual vector<double> maxs() { vector<double> vmax; vmax.push_back(max()); return vmax; }
    virtual vector<double> mins() { vector<double> vmin; vmin.push_back(min()); return vmin; }
    virtual string reference() { return ""; }
    

    
    virtual double operator()(double c) { return c; }
    virtual double revert(double c) { return c; }
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        
    }
    virtual void metadata(ostream& out) { out << "axis : {}"; }
    virtual void set() {}
    virtual bool accept(const string&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return false;
    }
    
    virtual void toxml(ostream&) const {}
    virtual AxisAutomaticSetting automatic() { return m_off; }
    virtual void automatic(bool) {  }
    virtual double operator()(const string& val) const { return tonumber(val); }
    virtual string type() const { return "regular"; }
    
    virtual void minmax(double max, double min) { assert(false); }
    
    virtual void dataMinMax(double min, double max) = 0;
    virtual void dataMinMax(double min, double max, const string&) = 0;

    virtual void setAutomatic(AxisAutomaticSetting) {}
    virtual void getNewDefinition(const UserPoint&, const UserPoint&, map<string, string>&) const
    {}
  
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	Coordinate(const Coordinate&);
    //! Overloaded << operator to copy - No copy allowed
	Coordinate& operator=(const Coordinate&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Coordinate& p)
		{ p.print(s); return s; }

};

class XCoordinate : public Coordinate
{
public:
	XCoordinate() {}
	virtual ~XCoordinate() {}
	virtual XCoordinate* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new XCoordinate();
    }
	virtual void toxml(ostream& out) const {}
	void set(const XmlNode& node) { }
	void set(const map<string, string>& map) {}
	void set() {}
	void print(ostream& out) const {  }
    virtual AxisAutomaticSetting automatic() { return m_off; }
    virtual void automatic(bool val) {  }
    bool accept(const string& tag) { }
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const XCoordinate& p)
		{ p.print(s); return s; }
	virtual void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
	{
		def["x_axis_type"]= type();
		def["x_min"]= tostring(ll.x_);
		def["x_max"]= tostring(ur.x_);
		def["x_automatic"]= "off";
	}
	virtual void dataMinMax(double min, double max) { assert(false); }
    virtual void dataMinMax(double min, double max, const string&) { assert(false); }
};

class YCoordinate : public Coordinate
{
public:
	YCoordinate() {}
	virtual ~YCoordinate() {}
	virtual void toxml(ostream& out) const {  }
	virtual YCoordinate* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new YCoordinate();
    }
	virtual void automatic(bool val) {}
	void set(const XmlNode& node) { }
	void set(const map<string, string>& map) {}
	void set() {}
	void print(ostream& out) const {  }
	bool accept(const string& tag) { return false; }
    virtual AxisAutomaticSetting automatic() { return m_off; }
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const YCoordinate& p)
		{ p.print(s); return s; }
	virtual void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
		{
			def["y_axis_type"]= type();
			def["y_min"]= tostring(ll.y_);
			def["y_max"]= tostring(ur.y_);
			def["y_automatic"]= "off";
		}
	virtual void dataMinMax(double min, double max) { assert(false); }
	virtual void dataMinMax(double min, double max, const string&) { assert(false); }
};

class RegularCoordinate 
{
public:
	RegularCoordinate() {}
	virtual ~RegularCoordinate() {}
};

class XRegularCoordinate : public RegularCoordinate, public XCoordinate, public XRegularCoordinateAttributes
{
public:
	XRegularCoordinate() { 
		set();	
	}
	virtual ~XRegularCoordinate() {}
	void toxml(ostream& out) const { XRegularCoordinateAttributes::toxml(out); }
	void set() {
		switch ( automatic_ ) {
			case m_both:
				min_ = std::numeric_limits<double>::max();
				max_ = -min_;
				break;
			case m_min_only:
				min_ = std::numeric_limits<double>::max();
				break;
			case m_max_only:
				max_ = -std::numeric_limits<double>::max();
				break;
			default:
				break;
		}
	}
	
	virtual void set(const XmlNode& node) {
		 XRegularCoordinateAttributes::set(node);
        	set();
	}
	void set(const map<string, string>& map) {
    	XRegularCoordinateAttributes::set(map);
    	set();
	}
    virtual bool accept(const string& node) { return XRegularCoordinateAttributes::accept(node); }
	double min() { return min_; }
    double max() { return max_; }
    double minpc() { return min_; }
    double maxpc() { return max_; }
    
    AxisAutomaticSetting automatic() { return automatic_; }
    virtual void setAutomatic(AxisAutomaticSetting automatic) { automatic_ = automatic; }
    virtual void  automatic(bool automatic) { automatic_ = automatic?m_both:m_off; set(); }

    void minmax(double min, double max) {

      	switch ( automatic_ ) {
      					case m_both:
      					case m_min_only:
      						if ( reverse_ )
      							max_ = std::max(max_, max);
      						else
      							min_ = std::min(min_, max);
      						break;
      					default:
      						break;
      				}
      	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;


      	switch ( automatic_ ) {
      		case m_both:
      		case m_max_only:
      			if ( reverse_ )
      				min_ = std::min(min_, min);
      			else
      				max_ = std::max(max_, min);
      			break;
      		default:
      			break;
      	}
      	automatic_ = (automatic_ == m_both) ? m_min_only : m_off;
      }
    
    virtual XCoordinate* clone() const {
    	XRegularCoordinate* x = new XRegularCoordinate();
    	x->copy(*this);
        return x;
    }
    
    

  
    
protected:
	virtual void print(ostream& out) const  {
		XRegularCoordinateAttributes::print(out);
	}
};

class YRegularCoordinate : public RegularCoordinate,  public YCoordinate, public YRegularCoordinateAttributes
{
public:
	YRegularCoordinate() {
		set();
	}
	virtual ~YRegularCoordinate() {}

	void toxml(ostream& out) const { YRegularCoordinateAttributes::toxml(out); }

	void set() {
		switch ( automatic_ ) {
				case m_both:
					min_ = std::numeric_limits<double>::max();
					max_ = -min_;
					break;
				case m_min_only:

					min_ = std::numeric_limits<double>::max();
					break;
				case m_max_only:

					max_ = -std::numeric_limits<double>::max();
					break;
				default:
					break;
			}
	}
	double min() { return min_; }
    double max() { return max_; }
    double minpc() { return min_; }
    double maxpc() { return max_; }
    virtual void setAutomatic(AxisAutomaticSetting automatic) { automatic_ = automatic; set(); }
    void minmax(double min, double max) {
    	AxisAutomaticSetting setting = automatic_;
    	switch ( automatic_ ) {
    					case m_both:
    						setting = m_max_only;
    					case m_min_only:
    						if ( reverse_ )
    							max_ = std::max(min, max_);
    						else
    							min_ = std::min(min, min_);


    						break;
    					default:
    						break;
    				}
    	automatic_ = setting;


    	switch ( automatic_ ) {
    		case m_both:
    			setting = m_min_only;
    		case m_max_only:
    			if ( reverse_ )
    				min_ = std::min(max, min_);
    			else
    				max_ = std::max(max, max_);
    			break;
    		default:
    			break;
    	}
    	automatic_ = setting;
    }

    virtual void automatic(bool automatic) { automatic_ = automatic?m_both:m_off; set();}
    virtual YCoordinate* clone() const {
    	YRegularCoordinate* y = new YRegularCoordinate();
    	y->copy(*this);
        return y;
    }
    void set(const XmlNode&  node ) { 
	YRegularCoordinateAttributes::set(node);
	set(); }
    void set(const map<string, string>& map) { 
    	YRegularCoordinateAttributes::set(map);
    	set();
	}
    bool accept(const string& node) { return YRegularCoordinateAttributes::accept(node); }

    void setMin(double min) { min_ = min; }
    void setMax(double max) { max_ = max; }
    AxisAutomaticSetting automatic() { return automatic_; }
    
   
protected:
	virtual void print(ostream& out) const  {
		YRegularCoordinateAttributes::print(out);
	}
};


class LogarithmicCoordinate : public Coordinate
{
public:
	LogarithmicCoordinate() {}
	virtual ~LogarithmicCoordinate() {}
	string type() const { return "logarithmic"; }

};

class XLogarithmicCoordinate : public LogarithmicCoordinate,  public XCoordinate, public XLogarithmicCoordinateAttributes
{
public:
	XLogarithmicCoordinate() {}
	virtual ~XLogarithmicCoordinate() {}
	virtual void toxml(ostream& out) const { XLogarithmicCoordinateAttributes::toxml(out); }
	double min() { return min_; }
    double max() { return max_; }
    double minpc() { return (*this)(min_); }
    double maxpc() { return (*this)(max_); }
    virtual void set(const XmlNode& node) {
	    if ( !magCompare(node.name(), "x_logarithmic") ) return; 
		XmlNode regular = node;
		regular.name("x_regular");
		XLogarithmicCoordinateAttributes::set(regular);
        set();
    }
    void set() {
    		switch ( automatic_ ) {
    				case m_both:
    					min_ = std::numeric_limits<double>::max();
    					max_ = -min_;
    					break;
    				case m_min_only:
    					automatic_ = m_off;
    					min_ = std::numeric_limits<double>::max();
    					break;
    				case m_max_only:
    					automatic_ = m_off;
    					max_ = -std::numeric_limits<double>::max();
    					break;
    				default:
    					break;
    			}
    	}
    void minmax(double min, double max) {
    	switch ( automatic_ ) {
    					case m_both:
    					case m_min_only:
      						if ( reverse_ )
      							max_ = std::max(max_, min);
      						else
      							min_ = std::min(min_, min);
    						break;
    					default:
    						break;
    				}
    	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;

    	switch ( automatic_ ) {
    		case m_both:
    		case m_max_only:
    			if ( reverse_ )
    				min_ = std::min(max, min_);
    			else
    				max_ = std::max(max, max_);
    			break;
    		default:
    			break;
    	}
    	automatic_ = (automatic_ == m_both) ? m_min_only : m_off;
    }

    double operator()(double c ) { return (c) ? log10(c) :0 ; }
    double revert(double c ) { return pow(c, 10); }
    
     virtual XCoordinate* clone() const {
    	XLogarithmicCoordinate* x = new XLogarithmicCoordinate();
    	x->copy(*this);
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return x;
    }
    
    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
    {
		def["x_axis_type"]= "logarithmic";
		def["x_min"]= tostring( ll.x_);
		def["x_max"]= tostring( ur.x_);
		def["x_automatic"]= "off";
    }

    virtual void dataMinMax(double min, double max) { assert(false); }
        virtual void dataMinMax(double min, double max, const string&) { assert(false); }
    AxisAutomaticSetting automatic() { return automatic_; }
    virtual void automatic(bool automatic) { automatic_ = (automatic?m_both:m_off);}
    virtual void setAutomatic(AxisAutomaticSetting automatic) { automatic_ = automatic; }
protected:
	virtual void print(ostream& out) const  {
		out << "XLogarithmicCoordinate[";
		XLogarithmicCoordinateAttributes::print(out);
		out << "]";
	}
};


class YLogarithmicCoordinate : public LogarithmicCoordinate,  public YCoordinate, public YLogarithmicCoordinateAttributes
{
public:
	YLogarithmicCoordinate() {}
	virtual ~YLogarithmicCoordinate() {}
	virtual void set(const XmlNode& node) {
		if ( !magCompare(node.name(), "y_logarithmic") ) return; 
		XmlNode regular = node;
		regular.name("y_regular");
		YLogarithmicCoordinateAttributes::set(regular);
        set();
    }
	virtual void set(const map<string, string>& map) {
			YLogarithmicCoordinateAttributes::set(map);
	        set();
	    }
	virtual void toxml(ostream& out) const { YLogarithmicCoordinateAttributes::toxml(out); }
	void set() {
		switch ( automatic_ ) {
				case m_both:
					min_ = std::numeric_limits<double>::max();
					max_ = -min_;
					break;
				case m_min_only:
					min_ = std::numeric_limits<double>::max();
					break;
				case m_max_only:
					max_ = -std::numeric_limits<double>::max();
					break;
				default:
					break;
			}
	}
	double min() { return min_; }
    double max() { return max_; }
    double minpc() { return (*this)(min_); }
    double maxpc() { return (*this)(max_); }
    virtual void dataMinMax(double min, double max) { assert(false); }
        virtual void dataMinMax(double min, double max, const string&) { assert(false); }
    void minmax(double min, double max) {
    	switch ( automatic_ ) {
    					case m_both:
    					case m_min_only:
      						if ( reverse_ )
      							max_ = std::max(max_, min);
      						else
      							min_ = std::min(min_, min);
    						break;
    					default:
    						break;
    				}
    	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;

    	switch ( automatic_ ) {
    		case m_both:
    		case m_max_only:
    			if ( reverse_ )
    				min_ = std::min(max, min_);
    			else
    				max_ = std::max(max, max_);
    			break;
    			break;
    		default:
    			break;
    	}
    	automatic_ = (automatic_ == m_both) ? m_min_only : m_off;
    }
    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
    {
    	def["y_axis_type"]= "logarithmic";
        def["y_min"]= tostring(::pow(10.,ll.y_));
        def["y_max"]= tostring(::pow(10.,ur.y_));
		def["y_automatic"]= "off";
    }
    AxisAutomaticSetting automatic() { return automatic_; }
    
    void setMinMax(double min, double max) { min_ = min; max_ = max; }

    double operator()(double c ) {  return (c) ? log10(c) :0 ; }
    double revert(double c ) { return ::pow(10.,c); }
    virtual YCoordinate* clone() const {
    	YLogarithmicCoordinate* y = new YLogarithmicCoordinate();
    	y->copy(*this);
        return y;
    }

    virtual void automatic(bool automatic) { automatic_ = automatic?m_both:m_off;}
    virtual void setAutomatic(AxisAutomaticSetting automatic) { automatic_ = automatic; }
    
protected:
	virtual void print(ostream& out) const  {
		out << "YLogarithmicCoordinate[";
		YLogarithmicCoordinateAttributes::print(out);
		out << "]";
	}
};


class DateCoordinate 
{
public:
	DateCoordinate() {}
	virtual ~DateCoordinate() {}   
	virtual string type() const { return "date"; }

};

class XDateCoordinate : public DateCoordinate, public XDateCoordinateAttributes, public XCoordinate
{
public:
	XDateCoordinate() { anna_ = false; }
	virtual ~XDateCoordinate() {}
	virtual void toxml(ostream& out) const {XDateCoordinateAttributes::toxml(out); }
	double min() { return 0; }
    double max() { return DateTime(date_max_) - DateTime(date_min_); }
    	
    double minpc() { return anna_ ? min_anna_ : 0; }
    double maxpc() { return anna_ ? max_anna_ : max(); }
    
    void min(double min) { min_anna_ = min; anna_ = true; }
    void max(double max) { max_anna_ = max; anna_ = true;}

    string reference() { return  DateTime(date_min_); }
    virtual XCoordinate* clone() const {
    	XDateCoordinate* x = new XDateCoordinate();
    	//x->copy(*this);
        return x;
    }
    virtual string type() const { return "date"; }
    AxisAutomaticSetting automatic() { return automatic_ ? m_both:  m_off;}
    void set(const XmlNode& node) { XDateCoordinateAttributes::set(node);  }
    void set(const map<string, string>& map) { XDateCoordinateAttributes::set(map); }
    bool accept(const string& xml) { return XDateCoordinateAttributes::accept(xml); }
    
    void setMin(double) { }
    void setMax(double) { }
    
    void automatic(bool automatic) { automatic_ = (automatic?m_both:m_off); }
    virtual void setAutomatic(AxisAutomaticSetting automatic) { automatic_ = (automatic); }
    
    void setMinMax(const string& min, const string& max ) { date_min_ = min;  date_max_ = max; }

    
    void dataMinMax(double min, double max, const string& date) {
    	switch ( automatic_ ) {
    		case m_both:
    		case m_min_only: {
    			if ( reverse_) {
    				DateTime newmax(date);
    				newmax = newmax + Second(min);
    				date_max_ = string(newmax);
    			}
    			else {
    				DateTime newmin(date);
    				newmin = newmin + Second(min);
    				date_min_ = string(newmin);
    			}
    			break;
    		}
    		default:
    			break;
    	}

    	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;


    	switch ( automatic_ ) {
			case m_both:
			case m_max_only: {
				if ( reverse_) {
					DateTime newmin(date);
					newmin = newmin + Second(max);
					date_min_ = string(newmin);
				}
				else {
					DateTime newmax(date);

					newmax = newmax + Second(max);
					date_max_ = string(newmax);
				}
				break;
			}
			default:
				break;
		}
    	automatic_ = (automatic_ == m_both) ? m_min_only : m_off;

    } 
     
    double operator()(double c ) { return c; }
    
    double operator()(const string& val) const  { 
    	DateTime date(val); 
    	return date - DateTime(date_min_);
    }

    void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
    {
    	DateTime min = DateTime(date_min_) + Second(ll.x_);
    	DateTime max = DateTime(date_min_) + Second(ur.x_);
    		def["x_axis_type"]="date";
    		def["x_date_min"]= tostring(min);
    		def["x_date_max"]= tostring(max);
    		def["x_automatic"]= "off";
    	}
	
	
protected:
	virtual void print(ostream& out) const  {
		XDateCoordinateAttributes::print(out);
	}
	// patch for anna! 
	
	double min_anna_;
	double max_anna_;
	bool anna_;

};


class YDateCoordinate : public DateCoordinate, public YDateCoordinateAttributes, public YCoordinate
{
public:
	YDateCoordinate() {}
	virtual ~YDateCoordinate() {}
	virtual void toxml(ostream& out) const { YDateCoordinateAttributes::toxml(out); }
	double min()   { return 0; }
    double max()   { return DateTime(date_max_) - DateTime(date_min_); }
    double minpc()     { return 0; }
    double maxpc()     { return DateTime(date_max_) - DateTime(date_min_);}
    string reference() { return DateTime(date_min_); }
    virtual string type() const { return "date"; }
    bool accept(const string& xml) { return YDateCoordinateAttributes::accept(xml); }
    void set(const XmlNode& node) { YDateCoordinateAttributes::set(node); }
       void set(const map<string, string>& map) { YDateCoordinateAttributes::set(map);  }
    virtual void setAutomatic(AxisAutomaticSetting automatic) { automatic_ = (automatic); }
    void automatic(bool automatic) { automatic_ = (automatic?m_both:m_off); }

    virtual YCoordinate* clone() const {
    	YDateCoordinate* y = new YDateCoordinate();
    	//y->copy(*this);
        return y;
    }
    AxisAutomaticSetting automatic() { return automatic_ ? m_both:  m_off; }
    
    void setMinMax(double, double) { }

    
    void setMinMax(const string& min, const string& max) { date_min_ = min; date_max_ = max;}

    
    double operator()(double c ) { return c; }
    double operator()(const string& val) const  { 
    	DateTime date(val); 
    	return date -  DateTime(date_min_);
    }
    void dataMinMax(double min, double max, const string& date) {
        	switch ( automatic_ ) {
        		case m_both:
        		case m_min_only: {
        			if ( reverse_) {
        				DateTime newmax(date);
        				newmax = newmax + Second(min);
        				date_max_ = string(newmax);
        			}
        			else {
        				DateTime newmin(date);
        				newmin = newmin + Second(min);
        				date_min_ = string(newmin);
        			}

        			break;
        		}
        		default:
        			break;
        	}
        	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;



        	switch ( automatic_ ) {
    			case m_both:
    			case m_max_only: {
    				if ( reverse_) {
    									DateTime newmin(date);
    									newmin = newmin + Second(max);
    									date_min_ = string(newmin);
    								}
    								else {
    									DateTime newmax(date);

    									newmax = newmax + Second(max);
    									date_max_ = string(newmax);
    								}
    				break;
    			}
    			default:
    				break;
    		}
        	automatic_ = (automatic_ == m_both) ? m_min_only : m_off;

        }
        void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
        {
        	DateTime min = DateTime(date_min_) + Second(ll.y_);
        	DateTime max = DateTime(date_min_) + Second(ur.y_);
        		def["y_axis_type"]="date";
        		def["y_date_min"]= tostring(min);
        		def["y_date_max"]= tostring(max);
        		def["y_automatic"]= "off";
        	}

protected:
	virtual void print(ostream& out) const  {
		YDateCoordinateAttributes::print(out);
	}
};


class YHyperCoordinate : public YHyperCoordinateAttributes, public YCoordinate
{
public:
	YHyperCoordinate() {}
	virtual ~YHyperCoordinate() {}
	double min()   { return  (min_lon_ == max_lon_ ) ?  min_lat_ : min_lon_; }
	double max()   { return  (min_lon_ == max_lon_ ) ?  max_lat_ : max_lon_; }
	double minpc()     { return (min_lon_ == max_lon_ ) ?  min_lat_ : min_lon_; }
	double maxpc()     { return  (min_lon_ == max_lon_ ) ?  max_lat_ : max_lon_;  }
    string reference() { return ""; }
    void toxml(ostream& out) const { YHyperCoordinateAttributes::toxml(out); }
     bool accept(const string& xml) { return YHyperCoordinateAttributes::accept(xml); }
    void set(const XmlNode& node) { YHyperCoordinateAttributes::set(node); }
    void set(const map<string, string>& map) { YHyperCoordinateAttributes::set(map); }
    virtual void setAutomatic(bool automatic) {automatic_ = automatic?m_both:m_off; }
    void automatic(bool automatic) { automatic_ = automatic?m_both:m_off; }
    
    virtual YCoordinate* clone() const {
    	YHyperCoordinate* y = new YHyperCoordinate();
    	//y->copy(*this);
        return y;
    }
    AxisAutomaticSetting automatic() { return automatic_ ; }
    
    void setMinMax(double, double) { }

    
    vector<double> mins() { vector<double> mins; mins.push_back(min_lon_); mins.push_back(min_lat_);  return mins; }
     vector<double> maxs() {  vector<double> maxs; maxs.push_back(max_lon_); maxs.push_back(max_lat_);  return maxs; }
     string type() const { return "geoline"; }
     void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
     {
    	double lon1 = ll.y();
		double lon2 = ur.y();

      	double lat1 = (max_lat_-min_lat_) ? min_lat_ + ((lon1-min_lon_)/(max_lon_-min_lon_))* (max_lat_-min_lat_) : 0;
        double lat2 = (max_lat_-min_lat_) ? min_lat_ + ((lon2-min_lon_)/(max_lon_-min_lon_)) * (max_lat_-min_lat_) : 0;
        def["y_automatic"]="off";
		def["y_axis_type"]="geoline";
		def["y_min_latitude"]= tostring(lat1);
		def["y_max_latitude"]= tostring(lat2);
		def["y_min_longitude"]= tostring(lon1);
		def["y_max_longitude"]= tostring(lon2);
     }
     void dataMinMax(double min, double max, const string& info) {

    	 //interpret the info : lonmin/latmin
    	 Tokenizer tokenizer("/");
    	 vector<string> tokens;
    	 tokenizer(info, tokens);
            	switch ( automatic_ ) {
            		case m_both:

            		case m_min_only: {

            			if (reverse_) {
            				max_lon_ = tonumber(tokens[0]);
            				max_lat_ = tonumber(tokens[1]);
            			}
            			else {
            				min_lon_ = tonumber(tokens[0]);
            				min_lat_ = tonumber(tokens[1]);
            			}
            			break;
            		}
            		default:
            			break;
            	}
            	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;



            	//interpret the info : lonmin/latmin

            	tokens.clear();
            	tokenizer(info, tokens);
            	switch ( automatic_ ) {
        			case m_both:
        			case m_max_only: {
        				if (reverse_) {
        				            				min_lon_ = tonumber(tokens[0]);
        				            				min_lat_ = tonumber(tokens[1]);
        				            			}
        				            			else {
        				            				max_lon_ = tonumber(tokens[0]);
        				            				max_lat_ = tonumber(tokens[1]);
        				            			}

        				break;
        			}
        			default:
        				break;
        		}
            	 automatic_ = (automatic_ == m_both) ? m_min_only : m_off;
            }

protected:
	virtual void print(ostream& out) const  {
		YHyperCoordinateAttributes::print(out);
	}
};

class XHyperCoordinate : public XHyperCoordinateAttributes, public XCoordinate
{
public:
	XHyperCoordinate() {}
	virtual ~XHyperCoordinate() {}
	double min()   { return  (min_lon_ == max_lon_ ) ?  min_lat_ : min_lon_; }
	double max()   { return  (min_lon_ == max_lon_ ) ?  max_lat_ : max_lon_; }
	double minpc()     { return (min_lon_ == max_lon_ ) ?  min_lat_ : min_lon_; }
	double maxpc()     { return  (min_lon_ == max_lon_ ) ?  max_lat_ : max_lon_;  }
    string reference() { return ""; }
    void toxml(ostream& out) const { XHyperCoordinateAttributes::toxml(out); }
     bool accept(const string& xml) { return XHyperCoordinateAttributes::accept(xml); }
    void set(const XmlNode& node) { XHyperCoordinateAttributes::set(node); }
    void set(const map<string, string>& map) { XHyperCoordinateAttributes::set(map); }
    virtual void setAutomatic(bool automatic) { automatic_ = (automatic?m_both:m_off); }
    void automatic(bool automatic) {  automatic_ = automatic?m_both:m_off; }
    vector<double> mins() { vector<double> mins; mins.push_back(min_lon_); mins.push_back(min_lat_);  return mins; }
    vector<double> maxs() {  vector<double> maxs; maxs.push_back(max_lon_); maxs.push_back(max_lat_);  return maxs; }
    
    virtual XCoordinate* clone() const {
    	XHyperCoordinate* x = new XHyperCoordinate();

        return x;
    }
    AxisAutomaticSetting automatic() { return automatic_ ? m_both:  m_off; }
    
    void setMinMax(double, double) { }

    
     string type() const { return "geoline"; }
     void dataMinMax(double min, double max,  const string& info) {

    	 //interpret the info : lonmin/latmin
    	 Tokenizer tokenizer("/");
    	 vector<string> tokens;
    	 tokenizer(info, tokens);
            	switch ( automatic_ ) {
            		case m_both:
            		case m_min_only: {
            			if (reverse_) {
            				max_lon_ = tonumber(tokens[0]);
            				max_lat_ = tonumber(tokens[1]);
            			}
            			else {
            				min_lon_ = tonumber(tokens[0]);
            				min_lat_ = tonumber(tokens[1]);
            			}
            			break;
            		}
            		default:
            			break;
            	}

            	automatic_ = (automatic_ == m_both) ? m_max_only : m_off;

            	//interpret the info : lonmin/latmin

            	tokens.clear();
            	tokenizer(info, tokens);
            	switch ( automatic_ ) {
            	case m_both:
            	case m_max_only: {
            		if (reverse_) {
            			min_lon_ = tonumber(tokens[0]);
            			min_lat_ = tonumber(tokens[1]);
            		}
            		else {
            			max_lon_ = tonumber(tokens[0]);
            			max_lat_ = tonumber(tokens[1]);
            		}


            	}
            	default:
            		break;
            	}
            	automatic_ = (automatic_ == m_both) ? m_min_only : m_off;
            }
            void getNewDefinition(const UserPoint& ll, const UserPoint& ur, map<string, string>& def) const
            {
            	double lon1 = ll.x();
            	double lon2 = ur.x();

            	// we have to compute the lat and lon of the point!

            	double lat1 = (max_lat_-min_lat_) ? min_lat_ + ((lon1-min_lon_)/(max_lon_-min_lon_))* (max_lat_-min_lat_) : 0;
            	double lat2 = (max_lat_-min_lat_) ? min_lat_ + ((lon2-min_lon_)/(max_lon_-min_lon_)) * (max_lat_-min_lat_) : 0;
            	def["x_automatic"]="off";
                def["x_axis_type"]="geoline";
                def["x_min_latitude"]= tostring(lat1);
                def["x_max_latitude"]= tostring(lat2);
                def["x_min_longitude"]= tostring(lon1);
                def["x_max_longitude"]= tostring(lon2);
               }

protected:
	virtual void print(ostream& out) const  {
		XHyperCoordinateAttributes::print(out);
	}
};

template <>
class MagTranslator<string, XCoordinate> { 
public:
	XCoordinate* operator()(const string& val )
	{
		return SimpleObjectMaker<XCoordinate>::create(val);
	}     

	XCoordinate* magics(const string& param)
	{
		XCoordinate* object;
		ParameterManager::update(param, object);
		return object;
	}

};
template <>
class MagTranslator<string, YCoordinate> { 
public:
	YCoordinate* operator()(const string& val )
	{
		return SimpleObjectMaker<YCoordinate>::create(val);
	}     

	YCoordinate* magics(const string& param)
	{
		YCoordinate* object;
		ParameterManager::update(param, object);
		return object;
	}

};
} // namespace magics
#endif
