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

// File Path
// Sylvie Lamy-Thepaut - ECMWF Mar 02

#ifndef Path_H
#define Path_H


#include "magics.h"
#include "MagTranslator.h"


namespace magics
{
	
	
class XmlNode;
    
class Path {

public:
	Path() {}
	Path(const string& path) 
    {
        // remove the "white" space at the end of the string"
        int index = path.find_last_not_of(" ");
        path_ = path.substr(0, index+1);
    }
     void set(const map<string, string>&) {}
     void set(const XmlNode&) {}
     void toxml(ostream&, int)  const {}
     Path* clone() const { return new Path(path_); }
// -- Destructor
	virtual ~Path() {}

// -- Convertors
	operator const string&() const { return path_;} 
	operator const char*() const { return path_.c_str();}


protected:
	 virtual void print(ostream& out) const { out << "Path = " << path_; } 

private:
    string path_;

// -- Friends
	friend ostream& operator<<(ostream& s,const Path& p)
		{ p.print(s); return s; }
        

};


template<>
class MagTranslator<string, Path> {
public:
	Path* operator()(string value)
	{
		return new Path(value);
	}
    
    Path* magics(const string& param)
    {
        string val;
		ParameterManager::get(param, val);
		return (*this)(val);
    }

};

} // namespace magics

#endif
