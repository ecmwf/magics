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

/*! \file ClassicMtgDecoder.h
    \brief Definition of the Template class ClassicMtgDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/

#ifndef ClassicMtgDecoder_H
#define ClassicMtgDecoder_H

#include "magics.h"

#include "ClassicMtgDecoderAttributes.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"

#include "DateTime.h"
#include <limits>

class  spot_query;
class spot_config;
class spot_query_result;


namespace magics {

class XmlNode;
class ClassicMtgDecoder;

class MetgramParameter
{
public:
	MetgramParameter() : scaling_(1), offset_(0) { 
		minx_ = std::numeric_limits<double>::max();
		maxx_ = -minx_;
		miny_ = std::numeric_limits<double>::max();
		maxy_ = -miny_;
	}
	MetgramParameter(const string& name, const string& title, const string& code) :
		name_(name), code_(code),  title_(title), scaling_(1), offset_(0) { 
		minx_ = std::numeric_limits<double>::max();
		maxx_ = -minx_;
		miny_ = std::numeric_limits<double>::max();
		maxy_ = -miny_;
	}
	virtual ~MetgramParameter() {}
	virtual double operator()(double value, const string&) const 
		{ return (value * scaling_) + offset_ ; } 
	virtual double operator()(double value) const 
		{ return (value * scaling_) + offset_ ; } 
	const string& code() const  { return code_; }
	virtual const string& title() const { return title_; }
	virtual spot_query_result* prepare(const ClassicMtgDecoder&, vector<CustomisedPoint*>&);
	virtual void interpretResult(spot_query_result*, vector<CustomisedPoint*>&, const string&);
    virtual void setTransformation(Transformation&);
    void scaling(double scaling) { scaling_ = scaling; }
    void offset(double offset)   { offset_ = offset; }
    
protected:
	string name_;
	string code_;
	mutable string title_;
	mutable string xml_;
	double epsz_;
	double detz_;
	mutable double step_;
	double correction_;
	string detResolution_;
	string epsResolution_;
	double height_;
	double scaling_;
	double offset_;
	double minx_; 
	double miny_;
	double maxx_; 
	double maxy_;
    DateTime base_;

};

class ClassicMtgDecoder: 
			public ClassicMtgDecoderAttributes, 
			public Decoder,
			public Data,
			public PointsList  {

public:
	ClassicMtgDecoder();
	virtual ~ClassicMtgDecoder();
	
	virtual void set(const map<string, string>& map) 
		{  ClassicMtgDecoderAttributes::set(map); }
	virtual void set(const XmlNode& node) 
		{  ClassicMtgDecoderAttributes::set(node); }
		
	void decode();
	
	spot_query* newQuery() const;
	
	
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	void customisedPoints(const Transformation&, const std::set<string>& n, CustomisedPointsList& out, bool)
	{ customisedPoints(n, out); }
	PointsHandler& points(const Transformation&, bool) { ASSERT(false); }
	void visit(TextVisitor&);
	void moreTitle(TextVisitor&);
	void visit(Transformation&);

	//void visit(MetaData&);
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 MetgramParameter* parameter_;
	 void moreTitle(TextVisitor&) const;
	 UserPoint grid_;
	 double   mask_;
	 double   detz_;
	 double   epsz_;
	 string   resolution_;
	 mutable spot_config* spot_;
	 vector<CustomisedPoint*> points_;

private:
    //! Copy constructor - No copy allowed
	ClassicMtgDecoder(const ClassicMtgDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	ClassicMtgDecoder& operator=(const ClassicMtgDecoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ClassicMtgDecoder& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
