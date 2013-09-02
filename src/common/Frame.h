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

/*! \file Frame.h
    \brief Definition of the Template class Frame.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 29-Mar-2004
    
    Changes:
    
*/

#ifndef Frame_H
#define Frame_H

#include "magics.h"
#include "FrameBase.h"
#include "FrameAttributes.h"

namespace magics {

class PaperPoint;
class Polyline;


class Frame: public FrameBase, public FrameAttributes {

public:
	Frame();
	virtual ~Frame();	
	virtual FrameBase* clone() const;
	
	void set(const map<string, string>& map) { FrameAttributes::set(map); }
	void set(const XmlNode& xml) { FrameAttributes::set(xml); }
	
    virtual bool operator()() const { return true; }
    void set(Polyline&);
    void blank(Polyline&);
 // Simulate the FrameAttributes interface!
    virtual void setColour(Colour* colour)   { FrameAttributes::setColour(colour);  } 
	virtual void setStyle(LineStyle style)   { FrameAttributes::setStyle(style); } 
	virtual void setThickness(int thickness) { FrameAttributes::setThickness(thickness); } 
	virtual void setBlanking(bool blanking)  { FrameAttributes::setBlanking(blanking); }
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    
	

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Frame& p)
		{ p.print(s); return s; }

};
class NoFrame: public FrameBase
{
public:
    NoFrame() {}
    ~NoFrame() {}
 
    bool operator()() const {  return false; }
    FrameBase* clone() const { return new NoFrame(); }
protected:
    virtual void print(ostream&) const; 
};





} // namespace magics
#endif
