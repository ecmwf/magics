/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file FortranMagics.h
    \brief Definition of the Template class FortranMagics.
    
    Magics Team - ECMWF 2007
    
    Started: Fri 9-Mar-2007
    
    Changes:
    
*/

#ifndef FortranMagics_H
#define FortranMagics_H

#include "magics.h"


#include "OutputHandler.h"
#include "DriverManager.h"
#include "DisplayManager.h"
namespace magics {

class FortranRootSceneNode;
class VisualAction;
class Data;

class UserPoint;
class UserPoint;
class Axis;
class FortranTextVisitor;
class LegendVisitor;

class FortranMagics : public std::stack<BasicSceneObject*> {

public:
	FortranMagics();
	~FortranMagics();
	typedef void (FortranMagics::*Action)();
	void popen();
	void pclose();
	void pnew(const string&);
	void pcoast();
	void ptephi();
	void prepare();
	
	void pgrib();
	void pmapgen();
	void pnetcdf();
	void pgeo();
	void pinput();
	void ptable();
	void pcont();
	void ptext();
	void ptest();
	void pline();
	void psymb();
	void pimage();
	void pimport();
	void poverlay();
	void pobs();
	void podb();
	void plegend();
	void simplelegend();
	void geojson();
	//Wrep-Eps family!
	void epsinput();
	void wrepjson();

	void metbufr();
	void metgraph();

	void epscloud();
	void epsplumes();
	void epsgraph();
	void epswave();
	void epswind();
	void epsbar();
	void epsshading();
	void epslight();
	
	void pgraph();
	void pboxplot();
	
	void paxis();
	void ptaylor();
	
	void pwind();
	
	
	static FortranMagics& magics() { 
		if ( !singleton_ ) 
			singleton_ = new FortranMagics();
		return *singleton_;
	}
	
	static void close() { 
		if ( singleton_ ) 
			delete singleton_;
		singleton_ = 0;
	}
	void subpage();
	void page();
	void newpage();
	
	void superpage();
	void legend();
	void drivers();
	
	void actions();
	
	bool geographical();
	
	void resetGrib() { gribindex_ = 0; } 
	
	void data(Data*);
	
	void flagInputSymbol() { symbolinput_todo_ = true; }
	void flagInputMatrix() { matrixinput_todo_ = true; }
	void flagInputPoly() { polyinput_todo_ = true; }

protected:
     //! Method to print string about this class on to a stream of type ostream.
	 void print(ostream&) const; 
	 void finish();
	 void dispatch();
	 static FortranMagics*    singleton_;
	 DriverManager*		  drivers_;
	 FortranRootSceneNode*    root_;
	 OutputHandler*           output_;

	 stack<Action>            actions_;
	 vector<FortranTextVisitor*>   texts_;
	 vector<LegendVisitor*>   legends_;
	 vector<BasicSceneObject*>   later_;
	 stack<Axis*>             axis_;
	 BasicSceneObject* axisContainer_;
	 VisualAction*    action_;
	 bool empty_;
	 int gribindex_;
	 bool legend_todo_;

	 bool symbolinput_todo_;
	 bool matrixinput_todo_;
	 bool polyinput_todo_;

private:
    //! Copy constructor - No copy allowed
	FortranMagics(const FortranMagics&);
    //! Overloaded << operator to copy - No copy allowed
	FortranMagics& operator=(const FortranMagics&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const FortranMagics& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
