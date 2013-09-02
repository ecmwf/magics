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

/*! \file Visualiser.h
    \brief Definition of the Template class Visualiser.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 19-Jan-2004
    
    Changes:
    
*/

#ifndef Visualiser_H
#define Visualiser_H

#include "magics.h"


#include "VisualTask.h"


namespace magics {

template <class P> class Data;
template <class P> class MatrixHandler;

template <class P>
class VisualComponent
{
public:
	VisualComponent()  {};
	virtual ~VisualComponent() {};
	
	
	
    
    
    
    virtual void preparePlot(Data<P>&, Task&) 
    	{ MagLog::dev() << "VisualComponent::preparePlot(Data<P>&, Task&)--->Not yet implemented\n"; }
    virtual void preparePlot(MatrixHandler<P>&, Task&) 
        { MagLog::dev() << "VisualComponent::preparePlot(MatrixHandler<P>&, Task&)--->Not yet implemented\n"; }
    virtual void preparePlot(Task&) 
    	{ MagLog::dev() << "VisualComponent::preparePlot(Task&)--->Not yet implemented\n"; }
	
};


template <class P>
class Visualiser : public BaseSceneObject, public VisualComponent<P> {

public:
	Visualiser()  {};
	virtual ~Visualiser() {};
	
	 virtual void specialise(VisualTask<P>& task) { task.set(this); };
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "Base class Visualiser"; } 
  
     
private:
    //! Copy constructor - No copy allowed
	Visualiser(const Visualiser&);
    //! Overloaded << operator to copy - No copy allowed
	Visualiser& operator=(const Visualiser&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Visualiser<P>& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
