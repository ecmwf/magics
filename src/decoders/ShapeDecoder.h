/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
    //void sanityCheckPolygon(polygon_2d& io_rPolygon, bool& io_rbModified);

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
