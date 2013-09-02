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

/*! \file HiLo.h
    \brief Definition of the Template class HiLo.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 22-Jun-2004
    
    Changes:
    
*/

/*! \defgroup hilo Computation of High and Lows

   \section hiloOverview Overview

   Magics++ supports the calculation of Highs and Lows.
   These can be plotted (as Letters, Values or Symbols)
   or written into a file.

   In the past (MAGICS 6 and Magics++ <2.8.1) simple tests
   were used to find local mins and maxs. This resulted in
   comments that Magics did too many (not meaningful) extremas.

   \sa MinMaxFilter::Process()
*/

#ifndef HiLo_H
#define HiLo_H

#include "magics.h"
#include "BasicSceneObject.h"
#include "HiLoBase.h"
#include "HiLoAttributes.h"

namespace magics {

class MatrixHandler;

class HiLo: public HiLoBase,  public HiLoAttributes, public vector<BasicGraphicsObject*> {

public:
   HiLo();
   virtual ~HiLo();
   virtual void set(const map<string, string>&);
   virtual void set(const XmlNode& node) { HiLoAttributes::set(node); }
   virtual bool accept(const string& node) { return HiLoAttributes::accept(node);; }

   virtual HiLoBase* clone() const {
      HiLo* hilo = new HiLo();
      hilo->copy(*this);
      return hilo;
    }
  
    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&);
    virtual void operator()(const PaperPoint&); 
    virtual void clear() {
      this->type_->clear();
      this->marker_->clear();
      vector<BasicGraphicsObject*>::clear();
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    HiLo(const HiLo&);
    //! Overloaded << operator to copy - No copy allowed
    HiLo& operator=(const HiLo&);
    
// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const HiLo& p)
       { p.print(s); return s; }
};
class NoHiLo: public HiLoBase {
public:
	NoHiLo() {}
	    virtual ~NoHiLo() {}
	    HiLoBase* clone() const { return new NoHiLo(); }
	    virtual void set(const map<string, string>&) {}
	    virtual BasicGraphicsObject* next() { return 0; }
	    virtual bool more() { return false; }
	    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) {}
	    void print(ostream& out)  const { out << "No HiLo";}
	};


class HighHiLo: public HiLo {

public:
   HighHiLo() {}
   virtual ~HighHiLo() {}
   HiLoBase* clone() const {
      HiLo* hilo =  new HighHiLo();
      //hilo->copy(*this);
      return hilo;
   }

   void operator()(const PaperPoint& point)  {
      if ( point.high() ) HiLo::operator()(point);
   }
};


class LowHiLo: public HiLo {

public:
   LowHiLo() {}
   virtual ~LowHiLo() {}
   HiLoBase* clone() const {
      HiLoBase* hilo =  new LowHiLo();
      //hilo->copy(*this);
      return hilo;
   }

   void operator()(const PaperPoint& point) {
      if ( point.low() ) HiLo::operator()(point);
   }
};


} // namespace magics

#endif
