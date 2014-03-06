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

/*! \file XmlReader.h
    \brief Definition of the Template class XmlReader.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jun-2005

*/

#ifndef XmlReader_H
#define XmlReader_H

#include <stack>

#include "magics.h"
#include "XmlTree.h"

namespace magics {
	
class XmlReader : private std::stack<XmlNode*> {

public:
	XmlReader(bool tag=false);
	virtual ~XmlReader();
	void interpret(const string&, XmlTree*);
	int decode(const string&, XmlTree*);
	void newElement(const string&, const map<string, string>&);
	void endElement(const string&);
	void addData(const string&);
	
	bool dataAsTag() { return dataAsTag_; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 XmlTree* tree_;
	 bool     dataAsTag_;

private:
    //! Copy constructor - No copy allowed
	XmlReader(const XmlReader&);
    //! Overloaded << operator to copy - No copy allowed
	XmlReader& operator=(const XmlReader&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const XmlReader& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
