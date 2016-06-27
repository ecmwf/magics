/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EpsgramDecoder.h
    \brief Definition of the Template class EpsgramDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/

#ifndef EpsgramDecoder_H
#define EpsgramDecoder_H

#include "magics.h"

#include "EpsgramDecoderAttributes.h"
#include "EfigramDecoderAttributes.h"
#include "EpsXmlInputAttributes.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"
#include "spot_database.h"
#include "BasicSceneObject.h"
#include "XmlReader.h"
#include "DateTime.h"

#include <limits>

namespace magics {

class XmlNode;
class SpotDecoder;

class EpsParameter
{
public:
	EpsParameter();
		
	EpsParameter(const string& name, const string& title, const string& code) :
		name_(name), code_(code),  title_(title), offset_(0), scaling_(1) {
		minx_ = std::numeric_limits<double>::max();
		maxx_ = -minx_;
		miny_ = std::numeric_limits<double>::max();
		maxy_ = -miny_;
		if (title_.empty() ) title_ = code_;
	}
	virtual ~EpsParameter() {}
	string height(); // return the height of the station... 
	void   steps(const vector<double>& steps);
	virtual double operator()(double value, const string&) const { return (value * scaling_) + offset_; } 
	virtual double operator()(double) const { return -6; } 
	const string& code() const  { return code_; }
	virtual const string& title() const { return title_; }
	virtual const string& xml() const { return xml_; }
	void epsHeight(double epsz) { epsz_ = epsz; }
	void deterministicHeight(double detz) { detz_ = detz; }
	void stationHeight(double height) { height_ = height; }
	void correction(bool correction) { correction_ = correction; }
	void deterministicResolution(const string& resolution) { detResolution_ = resolution; }
	void epsResolution(const string& resolution) { epsResolution_ = resolution; }
    virtual spot_query_result* prepare(const SpotDecoder& decoder, vector<CustomisedPoint*>& out);
    virtual void interpretResult(spot_query_result*, vector<CustomisedPoint*>&);
    virtual void specific10(CustomisedPoint&) const;
	virtual void specific15(CustomisedPoint&) const;
	virtual int x(int val) const { return val; }
	virtual string x(const string& prefix, const string& val) const;
	virtual void setTransformation(Transformation&);
	void scaling(double scaling) { scaling_ = scaling; }
	void offset(double offset)   { offset_ = offset; }
	void shift(double shift)   { shift_ = shift; }
	virtual void stepvalues(double step, vector<double>& xpos) { xpos.push_back( step); }  
	virtual void xvalues(double, vector<double>&) {}
	virtual double  plumesInterval() { NOTIMP; } 
	 
void type(const string& type) { type_ = type; }
protected:
	string name_;
	string code_;
	mutable string title_;
	mutable string xml_;
	mutable double epsz_;
	mutable double detz_;
	mutable double step_;
	double shift_;
	double correction_;
	mutable string detResolution_;
	mutable string epsResolution_;
	double height_;
	string type_;
	map<double, double> steps_;
	typedef void (EpsParameter::*SpecificFunction)(CustomisedPoint&) const;
	 map<string, EpsParameter::SpecificFunction> specifics_;
	string base_;
	 double minx_;
	 double miny_;
	 double maxx_;
	 double maxy_;
	 double offset_;
	 double scaling_;
	 double percentile_;
	 double threshold_;
	 string      prefix_;
	 
	 friend class SpotDecoder; 
	 friend class EfigramDecoder; 
};


class SpotDecoder:
			public Decoder,
			public Data,
			public PointsList  {
public:
	SpotDecoder();
	virtual ~SpotDecoder();
	
	virtual void set(const map<string, string>&) 	{}
	virtual void set(const XmlNode&) {}
	
	virtual void visit(Transformation&);
	virtual void decode() { decode(false); }	
	virtual void decode(bool);
    spot_query* newQuery() const;
	spot_query* newQuery(const string&) const;
	
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	virtual PointsHandler& points();

	void customisedPoints(const Transformation&, const std::set<string>& n, CustomisedPointsList& out, bool)
	{ customisedPoints(n, out); }
	PointsHandler& points(const Transformation&, bool) { return points(); }
	virtual void visit(TextVisitor&);
	//virtual void visit(MetaData&);
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 EpsParameter* parameter_;

	 UserPoint grid_;
	 double   mask_;
	 double   detz_;
	 double   epsz_;
	 double   bathymetry_;
	 string      prefix_;

	 string   resolution_;
	 mutable spot_config* spot_;
	 vector<CustomisedPoint*> points_;
	 virtual void moreTitle(TextVisitor&) const {}	
	 
	 
	 string database_;
	 string station_;
	 string param_;
	 string param_title_;
	 double latitude_;
	 double longitude_;
	 string type_;
	 string date_;
	 string time_;
	 double height_;
	 bool correction_;
	 vector<double> steps_;
	 double scaling_;
	 double offset_;
	 double shift_;
	 double percentile_;
	 double threshold_;


	 virtual void set() {}
	 
	 
private:
    //! Copy constructor - No copy allowed
	SpotDecoder(const SpotDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	SpotDecoder& operator=(const SpotDecoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SpotDecoder& p)
		{ p.print(s); return s; }

};

class EpsgramDecoder: public SpotDecoder, public EpsgramDecoderAttributes
{
public:
	EpsgramDecoder();
	virtual ~EpsgramDecoder();
	
	virtual void set(const XmlNode& node) 
		{  EpsgramDecoderAttributes::set(node); set(); }
	virtual void set(const map<string, string>& map) 
		{  EpsgramDecoderAttributes::set(map); set(); }
	virtual MatrixHandler& matrix();
	

protected:
	Matrix matrix_;

	
    void moreTitle(TextVisitor&) const;	
	
	void visit(MetaDataVisitor&);
	void set();

	
};

class EfigramDecoder: public SpotDecoder, public EfigramDecoderAttributes
{
public:
	EfigramDecoder();
	virtual ~EfigramDecoder();

	virtual void set(const map<string, string>& map) 
		{  EfigramDecoderAttributes::set(map); set(); }
	virtual void set(const XmlNode& node) 
		{  EfigramDecoderAttributes::set(node); set(); }	
	void set();
	void visit(TextVisitor&);
	void visit(MetaDataVisitor&);
    virtual void visit(LegendVisitor&);
    void visit(Transformation&);
	virtual void decode();
protected: 
	vector<int> efi_;
	vector<string> legends_;
    bool first_;
    int step_;
    double minx_;
   	 double maxx_;

	
};


} // namespace magics
#endif
