/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ParameterManager.h
    \brief Handles the Magics Parameters
    \author Development Section, ECMWF

    Started: Jan 2004

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
	MagLog::info() << "The parameter " << name << " is unknown in Magics.\n"
	            << "Please check the documentation or contact Software Support at ECMWF.\n";
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
			
			 param->set(value);
			
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

	static longintarray getLongIntArray(const string& name)
	{
		longintarray value;
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
			(void)e; // prevent 'unreferenced local variable' compiler warning
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
