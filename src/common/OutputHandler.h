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

/*! \file OutputHandler.h
    \brief Definition of the Template class OutputHandler.
    
    Magics Team - ECMWF 2006
    
    Started: Wed 19-Jul-2006
    
    Changes:
    
*/

#ifndef OutputHandler_H
#define OutputHandler_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "OutputFactory.h"
#include "VectorOfPointers.h"

#include "OutputHandlerAttributes.h"

namespace magics {

class XmlNode;
class DriverManager;


class OutputHandler:  public OutputHandlerAttributes {

public:
	OutputHandler() {}
	virtual ~OutputHandler();
    
	virtual void set(const XmlNode& node)
	{
		OutputHandlerAttributes::set(node);
	}
	virtual void set(const map<string, string>& map)
	{
		OutputHandlerAttributes::set(map);
	}
	virtual OutputHandler* clone() const
	{
		return new OutputHandler();
	}
	virtual void toxml(ostream&, int = 0) const
	{
		MagLog::dev() << "OutputHandler::toxml(ostream&, int = 0)---> to be checked!...\n";
	}
	void set(DriverManager&);
	
	void set(const XmlNode& node, DriverManager&);
	
	void clear() { factories_.clear(); }
	
	static double patchLineSpacing()  { return lineSpacing_; }

	
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "OutputHandler[]\n"; } 
	 VectorOfPointers<vector<OutputFactory* > > factories_;
	 static double lineSpacing_;



private:
	//! Copy constructor - No copy allowed	VectorOfPointers<OutputFactory* > factories_;

	OutputHandler(const OutputHandler&);
	//! Overloaded << operator to copy - No copy allowed
	//OutputHandler& operator=(const OutputHandler&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const OutputHandler& p)
		{ p.print(s); return s; }
};

template <>
class MagTranslator<string, OutputHandler> { 
public:
	OutputHandler* operator()(const string& val )
	{
		return SimpleObjectMaker<OutputHandler>::create(val);
	}     

	OutputHandler* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
