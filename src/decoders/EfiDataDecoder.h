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

/*! \file EfiDataDecoder.h
    \brief Definition of the Template class EfiDataDecoder.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 18-May-2006
    
    Changes:
    
*/

#ifndef EfiDataDecoder_H
#define EfiDataDecoder_H

#include "magics.h"

#include "EfiDataDecoderAttributes.h"
#include "BaseSceneObject.h"
#include "Decoder.h"
#include "Data.h"
#include "PaperPoint.h"

namespace magics {

class XmgrBasic;

class EfiDataDecoder: public EfiDataDecoderAttributes, 	
            public Data<PaperPoint>, 
            public PointsList<PaperPoint> {

public:
	EfiDataDecoder();
	virtual ~EfiDataDecoder();
	void set(const map<string, string>& map) 
		{ EfiDataDecoderAttributes::set(map); }
	void set(const XmlNode& node) 
		{ EfiDataDecoderAttributes::set(node); }
	
	virtual void decode();
	virtual PointsHandler<PaperPoint>& points();
	void    visit(TitleBase& title);
	void    visit(LegendBase& legend);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	EfiDataDecoder(const EfiDataDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	EfiDataDecoder& operator=(const EfiDataDecoder&);
	XmgrBasic*       data_;

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EfiDataDecoder& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
