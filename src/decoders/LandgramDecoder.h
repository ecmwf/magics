/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LandgramDecoder.h
    \brief Definition of the Template class LandgramDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 25-Mar-2004
    
    Changes:
    
*/

#ifndef LandgramDecoder_H
#define LandgramDecoder_H

#include "magics.h"
#include "MagException.h"

#include "Data.h"
#include "Decoder.h"
#include "LandgramDecoderAttributes.h"
#include "XmlReader.h"

namespace magics {

class LandgramDecoder: public LandgramDecoderAttributes, 
	public Decoder, 
	public Data,
	public XmlNodeVisitor {

public:
	LandgramDecoder();
	virtual ~LandgramDecoder();
	// implements Decoder interface
	void decode();

	virtual void set(const map<string, string>& map) { LandgramDecoderAttributes::set(map); }
	virtual void set(const XmlNode& node) { LandgramDecoderAttributes::set(node); }
	void visit(const XmlNode& node);
	void visit(TextVisitor& node);
	
    
	PointsHandler& points() { throw MethodNotYetImplemented("LandgramDecoder::points()"); }
	virtual MatrixHandler& matrix()
	{
		decode();
		matrixHandlers_.push_back(new  MatrixHandler(matrix_));
		return *(matrixHandlers_.back());
	}
	void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	 void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
	    {
	    	customisedPoints(n, out);
	    }
	    PointsHandler& points(const Transformation& t, bool) { return points(); }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	Matrix matrix_;
	vector<double> values_;
	vector<double> steps_;
	vector<double> heights_;
	int first_;
private:
	//! Copy constructor - No copy allowed
	LandgramDecoder(const LandgramDecoder&);
	//! Overloaded << operator to copy - No copy allowed
	LandgramDecoder& operator=(const LandgramDecoder&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LandgramDecoder& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
