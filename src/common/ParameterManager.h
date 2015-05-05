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

/*! \file ParameterManager.h
    \brief Handles the Magics Parameters
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

    Changes:

*/

#ifndef ParameterManager_H
#define ParameterManager_H

#include <magics.h>
#include <BaseParameter.h>
#include <MagLog.h>
#include <Factory.h>
#include <MagException.h>


namespace magics {


class UnknownParameter : public MagicsException
{
public :
    UnknownParameter(const string& name) :
        MagicsException(name + ": unknown parameter, the call is ignored.")
   {
	MagLog::info() << "The parameter " << name << " is unknown in Magics++.\n"
	            << "Please check the documentation or contact\n"
	            << "the Meteorological Visualisation Section at ECMWF.\n";
   }
};


class ParameterManager : public map<string, BaseParameter*>
{
public:
	ParameterManager();
	virtual ~ParameterManager();

	static void add(const string&, BaseParameter*); 

	template  <class T>
	static void set(const string& name, const T& value)
	{
		ASSERT(table_);
		BaseParameter* param = (*table_).parameter(name);
		if (param)
		{
			try {
			    param->set(value);
			}
			catch (MagicsException& e)
			{
			    MagLog::warning() << "MagException > " << e << "\n";
			}
		}
		else
			MagLog::warning() << "The parameter " << name << " was not found.\n";
	}

	static void set(const string& name, const char* value)
	{
		ASSERT(table_);
		BaseParameter* param = (*table_).parameter(name);
		if (param)
			try {
			    param->set(string(value));
			}
			catch (MagicsException& e)
			{
			    MagLog::warning() << "MagException > " << e << "\n";
			}
		else
			MagLog::warning() << "The parameter " << name << " was not found.\n";
	}
	
	static void setLocal(const BaseParameter* from)
	{
		ASSERT(table_);
		BaseParameter* param = (*table_).parameter(from->name());
		if (param)
			try {
			    param->setLocal(from);
			}
			catch (MagicsException& e)
			{
			    MagLog::warning() << "MagException > " << e << "\n";
			}
		else
			MagLog::warning() << "The parameter " << from->name() << " was not found.\n";
	}

	static void resetLocal(const string& name)
	{
		ASSERT(table_);
		BaseParameter* param = (*table_).parameter(name);
		if (param)
		{
			try {
			    param->resetLocal();
			}
			catch (MagicsException& e)
			{
			    MagLog::warning() << "MagException > " << e << "\n";
			}
		}
		else
			MagLog::warning() << "The parameter " << name << " was not found.\n";
	}


	static void reset(const string& name)
	{
		ASSERT(table_);
		BaseParameter* param = (*table_).parameter(name);
		if (param) param->reset();
	}

	static void release()
	{
		if (table_) delete table_;
	}

	static BaseParameter* getCopy(const string& name)
	{
	     ASSERT(table_);
	     BaseParameter* param = (*table_).parameter(name);
	     return (param) ? param->clone() : 0;
	}

	template <class T>
	static void get(const string& name, T& value)
	{
		ASSERT(table_);
		BaseParameter* param = (*table_).parameter(name);
		if (param) param->get(value);
	}

	static double getDouble(const string& name) 
	{
		double value;
		get(name, value);
		return value;
	}

	static int getInt(const string& name) 
	{
		int value;
		get(name, value);
		return value;
	}

	static string getString(const string& name) 
	{
		string value;
		get(name, value);
		return value;
	}
	static stringarray getStringArray(const string& name)
		{
			stringarray value;
			get(name, value);
			return value;
		}
	static doublearray getDoubleArray(const string& name)
		{
			doublearray value;
			get(name, value);
			return value;
		}
	static intarray getIntArray(const string& name)
		{
			intarray value;
			get(name, value);
			return value;
		}

	static bool getBool(const string& name)
	{

		string s;
		get(name, s);
		s = lowerCase(s);

		if(s == "no" || s == "off" || s == "false") return false;
		if(s == "yes"|| s == "on"  || s == "true")  return true;

		// Catter for ints
		return atoi(s.c_str());

	}

	template <class T>
	static void update(const string& name, T*& object)
	{
		string val, def;

		if (!table_) {
     			MagLog::error() << "Problem in setting the parameter [" << name <<  "] ---> contact Magics team" << endl;
		}
		ASSERT(table_);

		BaseParameter* param = (*table_).parameter(name);
		if (!param)
		{
			MagLog::warning() << "parameter \"" << name << "\" not found " << endl;
			return;
		}

	#ifdef MAGICS_EXCEPTION
		try {
			param->get(val);
			object = SimpleObjectMaker<T>::create(val);
		}
		catch (NoFactoryException& e)
		{
			param->reset();
			param->get(def);
			MagLog::warning() << "parameter \"" << name << "\" : value [" << val << "] is not valid ---> default [" << def <<  "] used" << endl;

			try {
				object = SimpleObjectMaker<T>::create(def);
			}
			catch (NoFactoryException& e2) {
				MagLog::error() << "default [" << def <<  "] not found ---> contact Magics team" << endl;
				throw e2;
			}
		}
	#else
		param->get(val);
		object = SimpleObjectMaker<T>::create(val);

		if (!object)
		{
			param->reset();
			param->get(def);
			MagLog::warning() << "parameter \"" << name << "\" : value [" << val << "] is not valid ---> default [" << def <<  "] used" << endl;
			object = SimpleObjectMaker<T>::create(def);	
			if (!object)
			{
				MagLog::error() << "default [" << def <<  "] not found ---> contact Magics team\n"
				             << "Serious Error --> abort" << endl;
				throw NoFactoryException("name");
			}
		}
	#endif
	}

	void resetAll();

	static void reset() {
		if (table_)
			table_->resetAll();
	}

protected:
	virtual void print(ostream&) const;
	static ParameterManager* table_;

	BaseParameter* parameter(const string& name) const
	{
		string lower = lowerCase(name);
		size_type pos = lower.find_first_of(" ");

		string tofind = ( pos != string::npos) ? lower.substr(0,  pos) : lower;
		const_iterator param = find(tofind); 
		if ( param != end() )
		{
			return (*param).second;	
		}
		MagLog::info() << "The parameter " << name << " is unknown in Magics++.\n"
		            << "Please check the documentation or contact\n"
		            << "the Meteorological Visualisation Section at ECMWF.\n";
		return 0;
	}

private:
	// No copy allowed
	ParameterManager(const ParameterManager&);
	ParameterManager& operator=(const ParameterManager&);

	// -- Friends
	friend ostream& operator<<(ostream& s,const ParameterManager& p)
		{ ASSERT(table_); (*p.table_).print(s); return s; }
};

} // namespace magics
#endif
