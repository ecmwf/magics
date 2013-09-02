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

/*! \file MagML.h
    \brief Definition of the Template class MagML.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 3-Apr-2007
    
    Changes:
    
*/

#ifndef MagML_H
#define MagML_H


#include "XmlMagics.h"

namespace magics {

class TempFile
{
  public:
    TempFile() : filename(tmpnam(0)), ofs(filename)
    {
        if (!ofs) return;
    }

    ~TempFile()
    {
        ofs.close();
        remove(filename);
    }

    ofstream& operator()() { return ofs; }
    string name() { return filename; }

  private:
    const char* filename;
    ofstream ofs;
};


class WebFormat  {

public:
	WebFormat() {}
	~WebFormat() {}
	

	
	void prepare(const string&, const map<string, string>&, TempFile& file);		
	
    virtual void execute(const string&, const map<string, string>&) {}
	
	
	
	

protected:
	
	void print(ostream&) const {}
     
private:
    //! Copy constructor - No copy allowed
	WebFormat(const WebFormat&);
    //! Overloaded << operator to copy - No copy allowed
	WebFormat& operator=(const WebFormat&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const WebFormat& p)
		{ p.print(s); return s; }
};

class MagML : public WebFormat {

public:
	MagML() {}
	~MagML() {}
    
    void execute(const string&, const map<string, string>&);
    
protected:	
	void print(ostream&) const {}
};



class WebInterpretor: public map<string, string> 
{
public:
	WebInterpretor();
	~WebInterpretor();
	
	static void magml(const string&);
	static void json(const string&);
	static void set(const string& param,  const string& value) {
		web_.insert(make_pair(param, value)); 
	}
	static map<string, string>& parameters() { return web_; }
	
	
protected:
	static WebInterpretor web_;
	
};

} // namespace magics
#endif
