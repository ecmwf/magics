/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EpsGraph.h
    \brief Definition of the Template class EpsGraph.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef EpsGraph_H
#define EpsGraph_H

#include "magics.h"


#include "EpsGraphAttributes.h"
#include "EfiGraphAttributes.h"
#include "CdfGraphAttributes.h"
#include "EpsShadeAttributes.h"
#include "EpsPlumeAttributes.h"
#include "EpsWindAttributes.h"
#include "EpsCloudAttributes.h"
#include "EpsDirectionAttributes.h"

#include "magics.h" 
#include "Polyline.h"
#include "Visdef.h"
#include "BasicGraphicsObject.h"

namespace magics {

class XmlNode;


class EpsGraph: public EpsGraphAttributes, public Visdef {



public:
	EpsGraph();
	virtual ~EpsGraph();
    
   
    
   virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);
	bool needLegend() { return legend_; }
    
    // Implements the set method ... 
    void set(const map<string, string>& map ) { EpsGraphAttributes::set(map); }
    void set(const XmlNode& node) { EpsGraphAttributes::set(node); }
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 double resolution_;
	 bool   forecast_;
	 bool   control_;
	 bool   fullEps_;
 	 bool   eps_;
    

private:
    //! Copy constructor - No copy allowed
	EpsGraph(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
	EpsGraph& operator=(const EpsGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsGraph& p)
		{ p.print(s); return s; }

};


class EpsLight: public Visdef {



public:
	EpsLight() {}
	virtual ~EpsLight() {}
     // Implements the set method ... 
    void set(const map<string, string>& map ) {  }
    void set(const XmlNode&) { }
   
    
   void operator()(Data&, BasicGraphicsObjectContainer&);
   void visit(LegendVisitor&);
     
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	

private:
    //! Copy constructor - No copy allowed
	EpsLight(const EpsLight&);
    //! Overloaded << operator to copy - No copy allowed
	EpsLight& operator=(const EpsLight&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsLight& p)
		{ p.print(s); return s; }

};


class EpsWind: public Visdef, public EpsWindAttributes {



public:
	EpsWind() {}
	virtual ~EpsWind() {}
     // Implements the set method ... 
    void set(const map<string, string>& map ) { EpsWindAttributes::set(map); }
    void set(const XmlNode& node) { EpsWindAttributes::set(node); }
   
    
   virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);
    
    
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos, double max);

    

private:
    //! Copy constructor - No copy allowed
	EpsWind(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
	EpsWind& operator=(const EpsGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsWind& p)
		{ p.print(s); return s; }

};

class EpsCloud: public Visdef, public EpsCloudAttributes {



public:
	EpsCloud() {}
	virtual ~EpsCloud() {}
     // Implements the set method ... 
    void set(const map<string, string>& map ) { EpsCloudAttributes::set(map); }
    void set(const XmlNode& node) { EpsCloudAttributes::set(node); }
   
    
   virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);
    
    
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos);

    

private:
    //! Copy constructor - No copy allowed
	EpsCloud(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
	EpsCloud& operator=(const EpsGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsCloud& p)
		{ p.print(s); return s; }

};
class EpsBar: public Visdef, public EpsCloudAttributes {



public:
	EpsBar() {}
	virtual ~EpsBar() {}
     // Implements the set method ...
    void set(const map<string, string>& map ) { EpsCloudAttributes::set(map); }
    void set(const XmlNode& node) { EpsCloudAttributes::set(node); }


   virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);




protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;



private:
    //! Copy constructor - No copy allowed
	EpsBar(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
	EpsBar& operator=(const EpsGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsBar& p)
		{ p.print(s); return s; }

};

class EpsWave: public Visdef {

public:
	EpsWave() {}
	virtual ~EpsWave() {}
	// Implements the set method ... 
	void set(const map<string, string>&  ) { } // EpsWindAttributes::set(map); }
	void set(const XmlNode& ) { } // EpsWindAttributes::set(node); }

	virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	EpsWave(const EpsWave&);
    //! Overloaded << operator to copy - No copy allowed
	EpsWave& operator=(const EpsWave&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsWave& p)
		{ p.print(s); return s; }

};


class EfiGraph: public Visdef, public EfiGraphAttributes {



public:
	EfiGraph();
	virtual ~EfiGraph();
    
    void set(const XmlNode& node)            { EfiGraphAttributes::set(node); }
    void set(const map<string, string>& map) { EfiGraphAttributes::set(map); }
    
    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const { /*EfiGraphAttributes::print(out);*/ }	 
	
	
private:
    //! Copy constructor - No copy allowed
	EfiGraph(const EfiGraph&);
    //! Overloaded << operator to copy - No copy allowed
	EfiGraph& operator=(const EfiGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EfiGraph& p)
		{ p.print(s); return s; }

};

class CdfGraph: public Visdef, public CdfGraphAttributes {



public:
	CdfGraph();
	virtual ~CdfGraph();
    
    void set(const XmlNode& node)            { CdfGraphAttributes::set(node); }
    void set(const map<string, string>& map) { CdfGraphAttributes::set(map); }
    
    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const { /*EfiGraphAttributes::print(out);*/ }	 
	 vector<string> legend_;
	 vector<string> usedColours_;
	
private:
    //! Copy constructor - No copy allowed
    CdfGraph(const CdfGraph&);
    //! Overloaded << operator to copy - No copy allowed
	CdfGraph& operator=(const CdfGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CdfGraph& p)
		{ p.print(s); return s; }

};

class EpsShade: public Visdef, public EpsShadeAttributes {


public:
	EpsShade();
	virtual ~EpsShade();
    
    void set(const XmlNode& node)            { EpsShadeAttributes::set(node); }
    void set(const map<string, string>& map) { EpsShadeAttributes::set(map); }
    
    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	virtual void visit(LegendVisitor&);
    
      
    
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const { /*EfiGraphAttributes::print(out);*/ }
	 
	

private:
    //! Copy constructor - No copy allowed
	EpsShade(const EpsShade&);
    //! Overloaded << operator to copy - No copy allowed
	EpsShade& operator=(const EpsShade&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsShade& p)
		{ p.print(s); return s; }

};

class EpsDirection: public Visdef, public EpsDirectionAttributes {


public:
	EpsDirection() {}
	virtual ~EpsDirection() {}
    
    void set(const XmlNode& node)            { EpsDirectionAttributes::set(node); }
    void set(const map<string, string>& map) { EpsDirectionAttributes::set(map); }
    
    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	
    
      
    
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { EpsDirectionAttributes::print(out); }
	 
	

private:
    //! Copy constructor - No copy allowed
	EpsDirection(const EpsDirection&);
    //! Overloaded << operator to copy - No copy allowed
	EpsDirection& operator=(const EpsDirection&);


};

class EpsPlume: public Visdef, public EpsPlumeAttributes {


public:
	EpsPlume();
	virtual ~EpsPlume() {}
    
    void set(const XmlNode& node)            { EpsPlumeAttributes::set(node); }
    void set(const map<string, string>& map) { EpsPlumeAttributes::set(map); }
    
    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
	void visit(LegendVisitor&);
    

    
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { EpsPlumeAttributes::print(out); }
	 typedef void (EpsPlume::*Method)(Data&, BasicGraphicsObjectContainer&);

	 std::map<string, Method> methods_;
	 vector<Colour> shading_legend_;
	 void timeserie(Data&, BasicGraphicsObjectContainer&);
	 void verticalprofile(Data&, BasicGraphicsObjectContainer&);

private:
    //! Copy constructor - No copy allowed
	EpsPlume(const EpsPlume&);
    //! Overloaded << operator to copy - No copy allowed
	EpsShade& operator=(const EpsPlume&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsPlume& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
