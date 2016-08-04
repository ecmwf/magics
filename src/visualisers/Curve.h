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

/*! \file Curve.h
    \brief Definition of the Template class Curve.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef Curve_H
#define Curve_H

#include "magics.h"

#include "CurveAttributes.h"
#include "CurveAreaAttributes.h"
#include "Graph.h"
#include "Polyline.h"

namespace magics {

class XmlNode;
class LegendEntry;


class Curve: public CurveAttributes, public Graph {

public:
	Curve();
	virtual ~Curve();
    enum PointPosition { in , out , enter , exit };

    
    void set(const XmlNode& node) { Graph::set(node); CurveAttributes::set(node); }
    
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    void set(const PaperPoint&, BasicGraphicsObjectContainer&, LegendEntry&);

	Polyline* newCurve(BasicGraphicsObjectContainer&) const;
	PointPosition where(const UserPoint& point) const;
     
    // Implements the set method ... 
    void set(const map<string, string>& map ) { CurveAttributes::set(map); }
    void symbol(vector<PaperPoint>& points, BasicGraphicsObjectContainer& out);
    void legend_symbol(PaperPoint& point, BasicGraphicsObjectContainer& task);

    typedef bool (Curve::*MissingMethod)(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);
    typedef void (Curve::*CurveMethod)(const UserPoint&, vector<UserPoint>& );
    void straight(const UserPoint& point, vector<UserPoint>& out) { out.push_back(point); }
    void stepped(const UserPoint& point, vector<UserPoint>& out);

    virtual void legend(Polyline&) {}
    bool ignore(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);
    bool join(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);
    bool drop(const PaperPoint&, const PaperPoint&, const vector<PaperPoint>&, BasicGraphicsObjectContainer&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 bool  missing(CustomisedPoint&) const;
	 std::map<string, MissingMethod> missingMethods_;
	 std::map<string, CurveMethod> curveMethods_;

     std::map<string, int>       thicknessHandler_;
     std::map<string, LineStyle> styleHandler_;
     std::map<string, string>    colourHandler_;

     Colour currentColour_;
     LineStyle currentStyle_;
     int currentThickness_;

private:
    //! Copy constructor - No copy allowed
	Curve(const Curve&);
    //! Overloaded << operator to copy - No copy allowed
	Curve& operator=(const Curve&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Curve& p)
		{ p.print(s); return s; }

};

class CurveArea: public Curve, public CurveAreaAttributes {

public:
	CurveArea() {}
	virtual ~CurveArea() {}

	void operator()(Data&, BasicGraphicsObjectContainer&);
	void legend(Polyline&);
	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const
	 	{ out << "CurveArea";  Curve::print(out); }
 
    

private:
    //! Copy constructor - No copy allowed
	CurveArea(const CurveArea&);
    //! Overloaded << operator to copy - No copy allowed
	CurveArea& operator=(const CurveArea&);


};


} // namespace magics
#endif
