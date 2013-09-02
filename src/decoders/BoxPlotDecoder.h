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

/*! \file BoxPlotDecoder.h
    \brief Definition of the Template class BoxPlotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 29-Sep-2005
    
    Changes:
    
*/

#ifndef BoxPlotDecoder_H
#define BoxPlotDecoder_H

#include "magics.h"

#include "BoxPlotDecoderAttributes.h"
#include "Data.h"
#include "Decoder.h"
#include "UserPoint.h"

namespace magics {

class BoxPlotDecoder: 
		public BoxPlotDecoderAttributes, 
		public Data,
		public PointsList {

public:
	BoxPlotDecoder();
	virtual ~BoxPlotDecoder();
	
	virtual void set(const map<string, string>& map) 
		{  BoxPlotDecoderAttributes::set(map); }
	
	virtual void set(const XmlNode& node) 
		{  BoxPlotDecoderAttributes::set(node); }

    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
    {
    	customisedPoints(n, out);
    }
    PointsHandler& points(const Transformation&, bool) { assert(false); }
    void getReady(const Transformation&);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	BoxPlotDecoder(const BoxPlotDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	BoxPlotDecoder& operator=(const BoxPlotDecoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BoxPlotDecoder& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
