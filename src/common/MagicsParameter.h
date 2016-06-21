/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagicsParameter.h
    \brief Definition of the MagicsParameter template class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

    Changes:

*/
#ifndef MagicsParameter_H
#define MagicsParameter_H

#include <magics.h>
#include <BaseParameter.h>
#include <MagLog.h>

namespace magics {

template <class T>
class MagicsParameter: public BaseParameter
{
public:
	MagicsParameter(const string& name, const T& def, const string& migration = "") :
        BaseParameter(name), default_(def), global_(def), local_(def), migration_(migration) { }

	~MagicsParameter() {}

	void get(T& value) const { value = local_; }
	void reset() { global_ = local_ =  default_; }
	
	BaseParameter* clone() { return new MagicsParameter<T>(this->name_, this->default_);  }
	
	string type() const  { return getType(default_); }

	void set(const T& value)
	{
		global_ = local_ = value;		
		MagLog::info() << " Parameter " << name_  << " set to " << global_<< "\n";
		           
	}
	void setLocal(const BaseParameter* from)
	{
		    
			from->get(local_); 
			MagLog::debug() << " Parameter (local) " << name_  << " set to " << local_<< "\n";
	}
	void resetLocal()
	{
			local_ = global_; 
			MagLog::debug() << " Parameter (local) " << name_  << "reset\n";
	}

protected:
	void print(ostream& out) const 
		{ out << name_ << "[" << global_ << ", " << local_ << ", " << default_ << "]"; }
	T default_;
	T global_;
	T local_;
	string migration_;

private:
// No copy allowed
	MagicsParameter(const MagicsParameter<T>&);
	MagicsParameter& operator=(const MagicsParameter&);

// -- Friends
	friend ostream& operator<<(ostream& s,const MagicsParameter<T>& p)
		{ p.print(s); return s; }
};

} // namespace magics

#endif
