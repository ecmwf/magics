/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
#include "AutoVector.h"

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
	 AutoVector<OutputFactory> factories_;
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
