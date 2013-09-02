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

/*! \file DisplayManager.h
    \brief Definition of the Template class DisplayManager.
    
    Magics Team - ECMWF 2007
    
    Started: Fri 9-Mar-2007
    
    Changes:
    
*/

#ifndef DisplayManager_H
#define DisplayManager_H

#include "magics.h"
#include "BasicGraphicsObject.h"
#include "Factory.h"
#include "MagTranslator.h"

namespace magics {
	
class BasicSceneObject;



class DisplayManager {


public:
	DisplayManager();
	virtual ~DisplayManager();
	
	
	
	
	void operator()(BasicSceneObject&, BasicGraphicsObjectContainer&);
	virtual void addInline(BasicSceneObject&, BasicGraphicsObjectContainer&); // inline mode 
	virtual void addBlock(BasicSceneObject&, BasicGraphicsObjectContainer&); // block mode 

	void bottomVertical(BasicSceneObject&, BasicGraphicsObjectContainer&);
	void bottomHorizontal(BasicSceneObject&, BasicGraphicsObjectContainer&);
	void topVertical(BasicSceneObject&, BasicGraphicsObjectContainer&);
	void topHorizontal(BasicSceneObject&, BasicGraphicsObjectContainer&);
	void nothing(BasicSceneObject&, BasicGraphicsObjectContainer&);
	
	void style(const string&, const string&, const string&);
	

	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	
	 
	 
	 typedef void (DisplayManager::*DisplayFunction)(BasicSceneObject&, BasicGraphicsObjectContainer&);
	 DisplayFunction  style_;
	 
	 

private:
    //! Copy constructor - No copy allowed
	DisplayManager(const DisplayManager&);
    //! Overloaded << operator to copy - No copy allowed
	DisplayManager& operator=(const DisplayManager&);
	bool fortran_;

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const DisplayManager& p)
		{ p.print(s); return s; }
	double x_;
	double y_;

};





} // namespace magics
#endif
