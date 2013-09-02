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

/*! \file Decoder.h
    \brief Definition of the Abstract template class Decoder.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 16-Jan-2004
    
    Changes:
    
*/

#ifndef Decoder_H
#define Decoder_H

#include "magics.h"


namespace magics {

class Decoder {

public:
	Decoder() {};
	virtual ~Decoder() {};
    //! Method to decode : abstract
    virtual void decode() = 0;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "Base class Decoder"; }

private:
    //! Copy constructor - No copy allowed
	Decoder(const Decoder&);
    //! Overloaded << operator to copy - No copy allowed
	Decoder& operator=(const Decoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Decoder& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
