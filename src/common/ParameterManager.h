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

#include "BaseParameter.h"
#include "Factory.h"
#include "MagException.h"
#include "MagLog.h"
#include "MagicsGlobal.h"
#include "magics.h"


namespace magics {


class UnknownParameter : public MagicsException {
public:
    UnknownParameter(const string& name) : MagicsException("Unknown parameter '" + name + "'") {}
};


class ParameterManager : public map<string, BaseParameter*> {
public:
    ParameterManager();
    virtual ~ParameterManager();

    static void add(const string&, BaseParameter*);

    template <class T>
    static void set(const string& name, const T& value) {
        
        ASSERT(table_);
        BaseParameter* param = (*table_).parameter(name);
        if (param) {
            param->set(value);
        }
        else {
            if (MagicsGlobal::strict()) {
                throw UnknownParameter(name);
            }
            MagLog::warning() << "The parameter '" << name << "' was not found.\n";
        }
    }

    static void set(const string& name, const char* value);

    static void reset(const string& name);

    static void release();

    static BaseParameter* getCopy(const string& name);

    template <class T>
    static bool get(const string& name, T& value) {
        ASSERT(table_);
        BaseParameter* param = (*table_).parameter(name);
        if (param) {
            param->get(value);
            return true;
        }
        return false;
    }

    static double getDouble(const string& name);

    static int getInt(const string& name);
    static int getUnsignedInt(const string& name);
    static unsigned long long  getULong(const string& name);

    static string getString(const string& name);
    static stringarray getStringArray(const string& name);
    static doublearray getDoubleArray(const string& name);
    static intarray getIntArray(const string& name);

    static longintarray getLongIntArray(const string& name);
    static bool getBool(const string& name);

    template <class T>
    static void update(const string& name, T*& object) {
        string val, def;

        if (!table_) {
            MagLog::error() << "Problem in setting the parameter [" << name << "] ---> contact Magics team" << endl;
        }
        ASSERT(table_);

        BaseParameter* param = (*table_).parameter(name);
        if (!param) {
            if (MagicsGlobal::strict()) {
                throw UnknownParameter(name);
            }
            MagLog::warning() << "parameter \"" << name << "\" not found " << endl;
            return;
        }


        try {
            param->get(val);
            object = SimpleObjectMaker<T>::create(val);
        }
        catch (NoFactoryException&) {
            if (MagicsGlobal::strict()) {
                throw;
            }


            param->reset();
            param->get(def);
            MagLog::warning() << "parameter \"" << name << "\" : value [" << val << "] is not valid ---> default ["
                              << def << "] used" << endl;

            try {
                object = SimpleObjectMaker<T>::create(def);
            }
            catch (NoFactoryException& e) {
                MagLog::error() << "default [" << def << "] not found ---> contact Magics team " << e.what() << endl;
                throw;
            }
        }
    }

    void resetAll();

    static void reset();

protected:
    virtual void print(ostream&) const;
    static ParameterManager* table_;

    BaseParameter* parameter(const string& name) const;

private:
    // No copy allowed
    ParameterManager(const ParameterManager&);
    ParameterManager& operator=(const ParameterManager&);

    // -- Friends
    friend ostream& operator<<(ostream& s, const ParameterManager& p) {
        ASSERT(table_);
        (*p.table_).print(s);
        return s;
    }
};

}  // namespace magics
#endif
