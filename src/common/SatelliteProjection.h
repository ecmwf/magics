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

/*! \file SatelliteProjection.h
    \brief Definition of the Template class SatelliteProjection.
    
    Magics Team - ECMWF 2006
    
    Started: Fri 21-Apr-2006
    
    Changes:
    
*/

#ifndef SatelliteProjection_H
#define SatelliteProjection_H

#include "magics.h"

#include "Transformation.h"
#include "SatelliteProjectionAttributes.h"
#include "TeProjection.h"

namespace magics {


/*! \class SatelliteProjection
    \brief Implements a new projection for satellite data
    \ingroup projection

    This projection ...
*/
class SatelliteProjection: public Transformation, 
	public TeSatelliteProjection,
	public SatelliteProjectionAttributes
{

public:
	SatelliteProjection();
	virtual ~SatelliteProjection();
	
	void set(const map<string, string>& map) {
		SatelliteProjectionAttributes::set(map); 
		Transformation::set(map);
	}
    
	void set(const XmlNode& node) {
		SatelliteProjectionAttributes::set(node); 
	}
    
	bool accept(const string& node) { return SatelliteProjectionAttributes::accept(node); }

	virtual Transformation* clone() const {
		SatelliteProjection* object = new SatelliteProjection();
		object->copy(*this);
		return object;
	}


	void operator()(const Polyline&, BasicGraphicsObject::Container&) const;
	void operator()(const Text<UserPoint>&, Text&) const;
	void operator()(const vector<UserPoint>&, vector<PaperPoint>&) const;
	void operator()(const vector<PaperPoint>&, vector<PaperPoint>&) const;

	virtual TeProjection&  getProjection() { return *this; }

	virtual Polyline* reproject(const Polyline& from) const;

	virtual bool in(double x, double y) const; 


	// implements the Transformation 
	virtual void operator()(Layout&) const;
	virtual PaperPoint operator()(const UserPoint&) const;   
	virtual PaperPoint operator()(const PaperPoint&) const;
	virtual UserPoint revert(const PaperPoint&) const;

	virtual void gridLongitudes(const GridPlotting&, Task&) const;
	virtual void gridLatitudes(const GridPlotting&, Task&) const;

	virtual void topLabels(const LabelPlotting&, Task&) const;
	virtual void bottomLabels(const LabelPlotting&, Task&) const;
	virtual void leftLabels(const LabelPlotting&, Task&) const;
	virtual void rightLabels(const LabelPlotting&, Task&) const;

	void horizontalLabels(const LabelPlotting&, Task&, const string& box, double x, double miny, double maxy) const;
	void verticalLabels(const LabelPlotting&, Task&, const string& box, double y, double minx, double maxy) const;


	virtual double getMinX() const { return minlon_; }
	virtual double getMaxX() const { return maxlon_; }
	virtual double getMinY() const { return minlat_; }
	virtual double getMaxY() const { return maxlat_; }

	virtual double getMinPCX() const { return llx_; }
	virtual double getMaxPCX() const { return urx_; }
	virtual double getMinPCY() const { return lly_; }
	virtual double getMaxPCY() const { return ury_; }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
 
	mutable double llx_;
	mutable double lly_;
	mutable double urx_;
	mutable double ury_;

	mutable double minlon_;
	mutable double maxlon_;
	mutable double minlat_;
	mutable double maxlat_;

private:
    //! Copy constructor - No copy allowed
	SatelliteProjection(const SatelliteProjection&);
    //! Overloaded << operator to copy - No copy allowed
	SatelliteProjection& operator=(const SatelliteProjection&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SatelliteProjection& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
