/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagTranslator.h
    \brief Definition of MagTranslator class.
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/

#ifndef MPP_MagTranslator_H
#define MPP_MagTranslator_H

// Forward declarations
#include "magics.h"
#include "ParameterManager.h"
#include "Tokenizer.h"


namespace magics {
	
class Matrix;

template<class From,class To> 
class MagTranslator {
public:
	virtual ~MagTranslator() {}
// Default template calls cast
	To* operator()(From from)
	{
		return new To(from); 
	}
  
	To* magics(const string& param)
	{
		From from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string, double>
{
public:
	double operator()(string value)
	{
		return atof(value.c_str());
	}

	double magics(const string& param)
	{
		double from;
		ParameterManager::get(param, from);
		return from;
	}
};


template<>
class MagTranslator<double, double> {
public:
	double operator()(double value)
	{
		return value;
	}
    
	double magics(const string& param)
	{
		double from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

#ifdef HAVE_CAIRO
template<>
class MagTranslator<cairo_t*, cairo_t*> {
public:
	cairo_t* operator()(cairo_t* value)
	{
		return value;
	}
    
	cairo_t* magics(const string& param)
	{
		cairo_t* from;
		ParameterManager::get(param, from);
		return (*this)(from); 
	}
};

template<>
class MagTranslator<string,  cairo_t*> {
public:
	cairo_t* operator()(const string&)
	{
		return 0;
	}
	string magics(const string&)
	{
		return "unknown";
	}
};
#endif

template<>
class MagTranslator<doublearray, doublearray> {
public:
	virtual ~MagTranslator() {}

	doublearray operator()(doublearray value)
	{
		return value;
	}
    
	doublearray magics(const string& param)
	{
		doublearray from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string, doublearray> {
public:
	doublearray operator()(const string& value)
	{
		doublearray array;
		vector<string> list;
		Tokenizer tokenizer("/");
		tokenizer(value, list);
		for(vector<string>::const_iterator item = list.begin(); item != list.end(); ++item) {
			array.push_back(atof((*item).c_str()));
		}
		return array;
	}
	doublearray magics(const string& param)
	{
		doublearray from;
		ParameterManager::get(param, from);
		return from;
	}
};
template<>
class MagTranslator<string, Matrix> {
public:

	Matrix operator()(const string& )
	{
		Matrix array;
		return array;
	}
	Matrix magics(const string& param)
	{
		Matrix from;
		ParameterManager::get(param, from);
		return from;
	}
};

template<>
class MagTranslator<string, int> {
public:
	int operator()(const string& value)
	{
		return atoi(value.c_str());
	}
	int magics(const string& param)
	{
		int from;
		ParameterManager::get(param, from);
		return from;
	}
};

template<>
class MagTranslator<int, int> {
public:
	int operator()(int value)
	{
		return value;
	}
	int magics(const string& param)
	{
		int from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string, intarray> {
public:
	intarray operator()(const string& value)
	{
		intarray array;
		vector<string> list;
		Tokenizer tokenizer("/");
		tokenizer(value, list);
    
		for(vector<string>::const_iterator item = list.begin(); item != list.end(); ++item) {
			array.push_back(atoi((*item).c_str()));
		}
		return array;
	}
	intarray magics(const string& param)
	{
		intarray from;
		ParameterManager::get(param, from);
		return from;
	}
};

template<>
class MagTranslator<string, longintarray> {
public:
	longintarray operator()(const string& value)
	{
		longintarray array;
		vector<string> list;
		Tokenizer tokenizer("/");
		tokenizer(value, list);

		for(vector<string>::const_iterator item = list.begin(); item != list.end(); ++item) {
			array.push_back(atoi((*item).c_str()));
		}
		return array;
	}
	longintarray magics(const string& param)
	{
		longintarray from;
		ParameterManager::get(param, from);
		return from;
	}
};
template<>
class MagTranslator<intarray, intarray> {
public:
	intarray operator()(intarray value)
	{
		return value;
	}
	intarray magics(const string& param)
	{
		intarray from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};
template<>
class MagTranslator<longintarray, longintarray> {
public:
	longintarray operator()(longintarray value)
	{
		return value;
	}
	longintarray magics(const string& param)
	{
		longintarray from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<const string&, const string&> {
public:
	const string& operator()(const string& value)
	{
		return value;
	}
	const string& magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};
template<>
class MagTranslator<string, string> {
public:
	string operator()(const string& value)
	{
		return value;
	}
	string magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<stringarray, stringarray> {
public:
	stringarray operator()(stringarray value)
	{
		return value;
	}
	stringarray magics(const string& param)
	{
		stringarray from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string, stringarray> {
public:
	stringarray operator()(string value)
	{
		stringarray result;
		
		Tokenizer tokenizer("/");//enum	LineStyle { SOLID , DASH , DOT , CHAIN_DASH , CHAIN_DOT };
		tokenizer(value, result);
		return result;
	}
    
	stringarray magics(const string& param)
	{
		stringarray from;
		ParameterManager::get(param, from);
		return from;
	}
};

template<>
class MagTranslator<bool,const string&> {
public:
	string operator()(bool value)
	{
		ostringstream out;
		out << value << ends;
		return out.str();
	}
};

template<>
class MagTranslator<const string&,bool> { 
public:
	bool operator()(const string& s)
	{
		if(s == "no" || s == "off" || s == "false") return false;
		if(s == "yes"|| s == "on"  || s == "true")  return true;

		// Catter for ints
		return atoi(s.c_str());
	}
	bool magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,bool> { 
public:
	bool operator()(const string& s)
	{
		 string val = lowerCase(s);
		if(val == "no" || val == "off" || val == "false") return false;
		if(val == "yes"|| val == "on"  || val == "true")  return true;

		// Catter for ints
		return atoi(val.c_str());
	}
	bool magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::Hemisphere> { 
public:
	magics::Hemisphere operator()(const string& s)
	{
 		string val = lowerCase(s);

		if (val  == "north") return NORTH;
		if (val  == "south") return SOUTH;
		
		return NORTH;
	}
	magics::Hemisphere magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::ArrowPosition> { 
public:
	magics::ArrowPosition operator()(const string& s)
	{
		string val = lowerCase(s);

		if (val  == "tail") return M_TAIL;
		if (val  == "centre") return M_CENTRE;
		
		return M_TAIL;
	}
	magics::ArrowPosition magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::LineStyle> { 
public:
	magics::LineStyle operator()(const string& s)
	{
		string val = lowerCase(s);
		if (val  == "solid") return M_SOLID;
		if (val  == "dash") return M_DASH;
		if (val  == "dot") return M_DOT;
		if (val  == "chain_dash") return M_CHAIN_DASH;
		if (val  == "chain_dot") return M_CHAIN_DOT;
		return M_SOLID;
	}
	magics::LineStyle magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::Position> {
public:
	magics::Position operator()(const string& s)
	{
		string val = lowerCase(s);
		if (val  == "automatic") return M_AUTOMATIC;
		if (val  == "left") return M_LEFT;
		if (val  == "right") return M_RIGHT;
		if (val  == "bottom") return M_BOTTOM;
		if (val  == "top") return M_TOP;
		return M_AUTOMATIC;
	}
	magics::Position magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::Justification> { 
public:
	magics::Justification operator()(const string& s)
	{
		string val = lowerCase(s);
		// enum	Justification { MLEFT , MCENTRE, MRIGHT }; };
		if (val  == "left") return MLEFT;
		if (val  == "centre") return MCENTRE;
		if (val  == "right") return MRIGHT;
		return MCENTRE;
	}
	magics::Justification magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::ListPolicy> { 
public:
	magics::ListPolicy operator()(const string& s)
	{
 		string val = lowerCase(s);
 		
		// enum	ListPolicy { M_LASTONE , M_CYCLE }; };
		if (val  == "lastone") return M_LASTONE;
		if (val  == "cycle") return M_CYCLE;
		MagLog::warning() << "Invalid value : " << val << "for a List Policy-->" << " changed to lastone" << endl;
		return M_LASTONE;
	}
	magics::ListPolicy magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};
template<>
class MagTranslator<string,magics::AxisAutomaticSetting> { 
public:
	magics::AxisAutomaticSetting operator()(const string& s)
	{
 		string val = lowerCase(s);
		// enum	ListPolicy {  m_off, m_both, m_min_only, m_max_only }; };
		if (val  == "off") return m_off;
		if (val  == "no") return m_off;
		if (val  == "false") return m_off;
		if (val  == "on") return m_both;
		if (val  == "yes") return m_both;
		if (val  == "true") return m_both;
		if (val  == "both") return m_both;
		if (val  == "min_only") return m_min_only;
		if (val  == "max_only") return m_max_only;
		
		MagLog::warning() << "Invalid value : " << val << "for an  Axis Automatic Setting-->" << " changed to off" << endl;
		return m_off;
	}
	magics::AxisAutomaticSetting magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<string,magics::DisplayType> { 
public:
	magics::DisplayType operator()(const string& s)
	{
		string val = lowerCase(s);
		// enum	Justification { MLEFT , MCENTRE, MRIGHT }; };
		if (val  == "inline") return INLINE;
		if (val  == "block") return BLOCK;
		if (val  == "none") return NONE;
		if (val  == "hidden") return HIDDEN;
		if (val  == "absolute") return ABSOLUTE;
		return ABSOLUTE;
	}
	magics::DisplayType magics(const string& param)
	{
		string from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

template<>
class MagTranslator<Matrix, Matrix> {
public:
	Matrix operator()(Matrix value)
	{
		return value;
	}
	Matrix magics(const string& param)
	{
		Matrix from;
		ParameterManager::get(param, from);
		return (*this)(from);
	}
};

} // namespace magics
#endif
