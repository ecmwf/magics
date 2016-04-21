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

/*! \file ShapeDecoder.h
    \brief Definition of the Template class ShapeDecoder.

    Magics Team - ECMWF 2005

    Started: Mon 12-Dec-2005

    Changes:

*/

#ifndef ShapeDecoder_H
#define ShapeDecoder_H

#include "magics.h"

#include "ShapeDecoderAttributes.h"
#include "Data.h"
#include "Decoder.h"
#include "UserPoint.h"
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <boost/geometry/geometries/box.hpp>

typedef boost::geometry::model::d2::point_xy<double>                   point_2d;
typedef boost::geometry::model::polygon< boost::geometry::model::d2::point_xy<double> > polygon_2d;
typedef boost::geometry::model::box<     boost::geometry::model::d2::point_xy<double> >     box_2d;

namespace magics {

class XmlNode;

class ShapeDecoder: public ShapeDecoderAttributes,
		public Data, public Decoder, public vector<PointsList* >
{
public:
	ShapeDecoder();
	virtual ~ShapeDecoder();
	//! Method to access the data as a list of points : Used by psymb.

	virtual void decode(const Transformation&);

	virtual void decode(vector<Polyline*>&, const Transformation&);
	void clip(const Transformation&,const vector<Polyline>&, vector<Polyline*>&) const;
	void clipAndClose(const Transformation&, const vector<Polyline>&, vector<Polyline*>&) const;
	virtual void decode(const Transformation&, const string&, const vector<string>&);
	void set(const map<string, string>& map ) { ShapeDecoderAttributes::set(map); }
	void set(const XmlNode& node ) { ShapeDecoderAttributes::set(node); }
	void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	void decode() {ASSERT(false);}
	void needHoles(bool holes) { holes_ = holes; }
	void setPath(const string& path) { path_ = path; }
	PointsHandler& points()
	{
		NOTIMP;
	}

	PointsHandler& points(const Transformation&, bool )
	{
		NOTIMP;
	}

	void customisedPoints(const Transformation&, const std::set<string>& n, CustomisedPointsList& out, bool)
	{
		customisedPoints(n, out);
	}

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const;
	bool holes_; // Do we need to deal with the holes during decoding!.

    //! Method to ensure all inner rings lie within outer ring of polygon
    void sanityCheckPolygon(polygon_2d& io_rPolygon, bool& io_rbModified);

private:
	//! Copy constructor - No copy allowed
	ShapeDecoder(const ShapeDecoder&);
	//! Overloaded << operator to copy - No copy allowed
	ShapeDecoder& operator=(const ShapeDecoder&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ShapeDecoder& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
