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

/*! \file MagJSon.h
    \brief Definition of the Template class MagJSon.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 3-Apr-2007
    
    Changes:
    
*/

#ifndef MagJSon_H
#define MagJSon_H


#include "WebFormat.h"
#include "json_spirit.h"
#include "XmlTree.h"

namespace magics {



class MagJSon : public WebFormat { 

public:
	MagJSon();
	~MagJSon() {}
    
    void execute(const string&, const map<string, string>&);
    

    void magics(const json_spirit::Value&);
    void build(XmlNode& parent, const string&, json_spirit::Object& object);
    typedef void (MagJSon::*Patch)(XmlNode&, const json_spirit::Value&);
     
    map<string,  Patch > patchs_;
    XmlTree tree_;
    	
    void drivers(XmlNode& parent, const json_spirit::Value&);
    void definitions(XmlNode& parent, const json_spirit::Value& value);
    void interpret(const string&);
    
protected:	
	void print(ostream&) const {}
	void parse(const string&);
	


};

class ParamJSon : public map<string, string>
{
public:
	ParamJSon(const string&);
	~ParamJSon() {}

protected:
    void magics(const json_spirit::Value&);



};



} // namespace magics
#endif
