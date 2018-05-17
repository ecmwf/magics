/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#ifndef EpsJSon_H
#define EpsJSon_H

#include "magics.h"


#include "WrepJSonAttributes.h"

#include "json_spirit.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"
#include "DateTime.h"

#include "Matrix.h"
#include <limits>

namespace magics {



struct InputWrep
{
	 bool empty() { return steps_.empty(); }
	 map<string, vector<double> > values_;
	 map<string, double> info_;
	 map<string, vector<vector<double>>> ensembleValues_;
	
	 vector<double> steps_;
	 vector<double> levels_;
	 int index(double val) {
		 vector<double>::iterator step = steps_.begin();
		 int index = 0;
		 while ( *step != val ) {
			 ++step;
			 if ( step == steps_.end() ) {
				 return -1; 
			 }
			 index++;
		 }
		 return index;
	}
	void print() {
		 cout << "InputWrep" << endl;
		 string sep = "steps[";
		 for (vector<double>::iterator step = steps_.begin(); step != steps_.end(); ++ step) {
			 cout << sep << *step;
			 sep = ", ";
		 }
		 cout << "]" << endl;
		 for (map< string, double>::iterator value = info_.begin(); value != info_.end(); ++ value) {
			 cout << value->first <<  " = " << value->second << endl;
	
		}
		for (map< string, vector<double> >::iterator value = values_.begin(); value != values_.end(); ++ value) {
			 string sep = value->first + "[";
			 for (vector<double>::iterator val =  value->second.begin(); val !=  value->second.end(); ++ val) {
			 	std::cout << sep << *val;
			 	sep = ", ";
			 }
			 cout << "]" << endl;
		}
		 for (map< string, vector<vector<double>>>::iterator value = ensembleValues_.begin(); value != ensembleValues_.end(); ++ value) {
			 string sep = value->first + "[";
			 for (vector<vector<double>>::iterator val =  value->second.begin(); val !=  value->second.end(); ++ val) {
			 	std::cout << sep << val->size();
			 	sep = ", ";
			 }
			 cout << "]" << endl;
		}
	}
};


class WrepJSon:
			public Data,
			public PointsList,
			public WrepJSonAttributes 
{
public:
	WrepJSon();
	virtual ~WrepJSon();
	
	typedef void (WrepJSon::*Method)(const json_spirit::Value&);
	typedef void (WrepJSon::*Decoder)();
	typedef void (WrepJSon::*TransformationHandler)(Transformation&);
    typedef double (WrepJSon::*HeightCorrection)(double);
    typedef json_spirit::Value (WrepJSon::*MetaMethod)();
	map<string,  Method> methods_;
	map<string,  Decoder> decoders_;
	map<string,  TransformationHandler> transformationHandlers_;
	map<string,  HeightCorrection> heightCorrections_;
	map<string,  MetaMethod> metaMethods_;

	void efi();
	void cdf();
	void eps();
	void basic();
	void data();
	void profile();
	void tephigram();
	void hodograph();
	void cape();


	void profile(Transformation&);
	void eps(Transformation&);
	void efi(Transformation&);
	void cdf(Transformation&);
	
	// Simple  input
	void x_values(const json_spirit::Value&);
	void y_values(const json_spirit::Value&);
	void values(const json_spirit::Value&);
	void x_date_values(const json_spirit::Value&);
	void y_date_values(const json_spirit::Value&);
	
	//tephigram
	void param(const json_spirit::Value&);
	void levels(const json_spirit::Value&);
	void data(const json_spirit::Value&, vector<double>&);

	//Hodograph
	void hodo_u(const json_spirit::Value&);
	void hodo_v(const json_spirit::Value&);

	//Cape
	void cape0(const json_spirit::Value&);
	void cape1(const json_spirit::Value&);
	void cape2(const json_spirit::Value&);
	void cape3(const json_spirit::Value&);
	void cape_dig(const json_spirit::Value&);
	
	
	// common 
	void location(const json_spirit::Value&);
	void station_name(const json_spirit::Value&);
	void valid_time(const json_spirit::Value&);
	void epsz(const json_spirit::Value&);
	void detz(const json_spirit::Value&);
	void date(const json_spirit::Value&);
    void expver(const json_spirit::Value&);
	void height(const json_spirit::Value&);
	void time(const json_spirit::Value&);
	void api(const json_spirit::Value&);

	virtual void parameter(const json_spirit::Value&);
	virtual void eps(const json_spirit::Value&);
	virtual void clim(const json_spirit::Value&);
	virtual void efi(const json_spirit::Value&);
	void dig(const json_spirit::Value&);
	void ignore(const json_spirit::Value&) {}
	void missing(const json_spirit::Value&);
	void mask(const json_spirit::Value&);
	void station(const json_spirit::Value&);
	void metadata(const json_spirit::Value&);
	void temperature_correction(const json_spirit::Value&);
	void points_along_meridian(const json_spirit::Value&);
	

	json_spirit::Value temperature_correction();
	json_spirit::Value temperature_adjustment();
	json_spirit::Value eps_resolution();
	json_spirit::Value deterministic_resolution();
	json_spirit::Value height();
	json_spirit::Value station_name();

	MatrixHandler& matrix();
	void customisedPoints(const std::set<string>&, CustomisedPointsList&);
	void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);

	virtual void set(const map<string, string>& map) 	{ WrepJSonAttributes::set(map);  }
	virtual void set(const XmlNode& node) { WrepJSonAttributes::set(node); }
	
	virtual void visit(Transformation&);
	virtual void visit(TextVisitor&);
	void visit(const XmlNode& node);
		
	virtual void decode();
    
    double correctDetz(double);
    double correctEpsz(double);

    void visit(MetaDataVisitor&);
    void points(const Transformation&, vector<UserPoint>&);
    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
    {
       	customisedPoints(t, n, out);
     }
     PointsHandler& points(const Transformation& t, bool);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 DateTime base_;
	 vector<CustomisedPoint*> points_; 
	 PointsList list_;
     double minx_;
	 double maxx_;
	 double minClim_;
	 double maxClim_;
	 double miny_;
	 double maxy_;
	 double shift_;
	 double missing_;
	 double latitude_;
	 double longitude_;
	 double height_;
     double epsz_;
     double detz_;
	 double scaling_factor_;
	 double offset_factor_;
	 double mask_;
	 string station_name_;
	 double station_latitude_;
	 double station_longitude_;
	 string date_;
	 string time_;
	 string file_;
	 string valid_time_;
     string expver_; 
     string api_;
     string capekey_;

	 json_spirit::Value metadata_;
	 InputWrep   values_;
	 InputWrep* current_;

	 InputWrep clim_;
	 
	 map<string, InputWrep> eps_;
	 map<string, InputWrep> efi_;
	 map<string, InputWrep> capes_;


	 int points_along_meridian_;
	 Matrix matrix_;

	 bool xdate_;
	 DateTime xBase_;
	 bool ydate_;
	 DateTime yBase_;
	 bool regular_;

private:
    //! Copy constructor - No copy allowed
	WrepJSon(const WrepJSon&);
    //! Overloaded << operator to copy - No copy allowed
	WrepJSon& operator=(const WrepJSon&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const WrepJSon& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
