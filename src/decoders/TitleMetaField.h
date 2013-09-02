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

/*! \file TitleMetaField.h
    \brief Definition of the Template class TitleMetaField.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/

#ifndef TitleMetaField_H
#define TitleMetaField_H

#include "magics.h"

#include "TitleField.h"

namespace magics {

class TitleMetaField: public TitleField {

public:
	TitleMetaField(const string&);
	virtual ~TitleMetaField();
    
    virtual string name() { return token_; }
    
   
    virtual void operator()(vector<string>&) const;
    
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     string token_;
    

private:
    //! Copy constructor - No copy allowed
	TitleMetaField(const TitleMetaField&);
    //! Overloaded << operator to copy - No copy allowed
	TitleMetaField& operator=(const TitleMetaField&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TitleMetaField& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
