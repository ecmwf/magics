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

	//Wrep-Eps family!
	void wrepjson();
	void epscloud();
	void epsplumes();
	void epsgraph();
	void epswave();
	void epswind();
	void epsbar();
	void epsshading();
	
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
