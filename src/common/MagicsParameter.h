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
