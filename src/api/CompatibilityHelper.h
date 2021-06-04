/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


/*! \brief Ensures backwards compability with Magics 6.x

  This base class checks if parameter are set which are not supported anymore
  in Magics++ but need to be supported for backwards compability.

*/

#include "magics.h"

namespace magics {

class CompatibilityHelper {
public:
    CompatibilityHelper(const string& name) { compatibility_[name] = this; }
    CompatibilityHelper() {}

    void set(const string& name) { compatibility_[name] = this; }

    virtual ~CompatibilityHelper() {}
    template <class P>
    static bool check(const string& param, P value) {
        map<string, CompatibilityHelper*>::const_iterator tool = compatibility_.find(lowerCase(param));
        if (tool == compatibility_.end())
            return false;
        else
            return (*(tool->second))(value);
    }
    static void reset(const string& param) {
        map<string, CompatibilityHelper*>::const_iterator tool = compatibility_.find(lowerCase(param));
        if (tool != compatibility_.end())
            (*(tool->second)).reset();
    }
    static bool check(const string& param, const string& value) {
        map<string, CompatibilityHelper*>::const_iterator tool = compatibility_.find(lowerCase(param));
        if (tool == compatibility_.end())
            return false;
        else
            return (*(tool->second))(string(value));
    }
    virtual void reset() {}
    virtual bool operator()(int) { return false; }
    virtual bool operator()(const string&) { return false; }
    virtual bool operator()(double) { return false; }
    virtual bool operator()(const doublearray&) { return false; }
    virtual bool operator()(const stringarray&) { return false; }
    virtual bool operator()(const intarray&) { return false; }
    virtual bool operator()(bool) { return false; }

    static void resetAll();

protected:
    static map<string, CompatibilityHelper*> compatibility_;
};

}  // namespace magics
