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

/*! \file ObsStatGraph.h
    \brief Definition of the Template class ObsStatGraph.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef ObsStatGraph_H
#define ObsStatGraph_H

#include "magics.h"

#ifdef LATER
#include "ObsStatGraphAttributes.h"
#endif

#include "magics.h"
#include "Polyline.h"
#include "Graph.h"

namespace magics {

class XmlNode;


#ifdef LATER
class ObsStatGraph: public ObsStatGraphAttribute, public Visualiser<PaperPoint> {
#else 
class ObsStatGraph: public Visualiser<PaperPoint>{
#endif


public:
	ObsStatGraph();
	virtual ~ObsStatGraph();
    
   
    
    void preparePlot(Data<PaperPoint>&, Task&);
    void visit(LegendBase&);
    
    
    // Implements the set method ... 
    void set(const map<string, string>& map ) {  }
    void set(const XmlNode& node) {  }
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
 
    

private:
    //! Copy constructor - No copy allowed
	ObsStatGraph(const ObsStatGraph&);
    //! Overloaded << operator to copy - No copy allowed
	ObsStatGraph& operator=(const ObsStatGraph&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ObsStatGraph& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
