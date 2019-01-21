/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribAddressMode.h
    \brief Definition of the Template class GribAddressMode.

    Magics Team - ECMWF 2006

    Started: Mon 13-Feb-2006

    Changes:

*/

#ifndef GribAddressMode_H
#define GribAddressMode_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "grib_api.h"


namespace magics {

class XmlNode;

class GribAddressMode {
public:
    GribAddressMode() {}
    virtual ~GribAddressMode() {}

    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}

    virtual bool accept(const string&) { return false; }

    virtual GribAddressMode* clone() const {
        MagLog::dev() << "GribAddressMode::set(const map<string, string&)---> to be checked!...\n";
        return new GribAddressMode();
    }
    virtual void toxml(ostream&, int = 0) const {
        MagLog::dev() << "GribAddressMode::toxml(ostream&, int = 0)---> to be checked!...\n";
    }
    virtual grib_handle* operator()(grib_context*, FILE*, int) const {
        MagLog::dev() << "GribAddressMode::toxml(ostream&, int = 0)---> to be checked!...\n";
        return 0;
    }
    virtual grib_handle* operator()(grib_context*, FILE*, long int) const {
        MagLog::dev() << "GribAddressMode::toxml(ostream&, int = 0)---> to be checked!...\n";
        return 0;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "GribAddressMode\n"; }

private:
    //! Copy constructor - No copy allowed
    GribAddressMode(const GribAddressMode&);
    //! Overloaded << operator to copy - No copy allowed
    GribAddressMode& operator=(const GribAddressMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GribAddressMode& p) {
        p.print(s);
        return s;
    }
};

class GribAddressRecordMode : public GribAddressMode {
public:
    GribAddressRecordMode() {}
    ~GribAddressRecordMode() {}

    virtual GribAddressMode* clone() const {
        GribAddressMode* mode = new GribAddressRecordMode();
        return mode;
    }

    virtual grib_handle* operator()(grib_context*, FILE* file, int position) const {
        grib_handle* handle = 0;


        grib_context* context = grib_context_get_default();
        int error;
        for (int i = 0; i < position - 1; i++) {
            // grib_read_any_from_file_alloc (context, file,  &msg , &size);
            // grib_context_free(context,msg);
            // MagLog::debug() << "call to grib_handle_new_from_file for position " << i << "\n";
            handle = grib_handle_new_from_file(context, file, &error);
            grib_handle_delete(handle);
        }

        handle = grib_handle_new_from_file(0, file, &error);

        return handle;
    }

protected:
    void print(ostream& out) const { out << "GribAddressRecordMode\n"; }
};

class GribAddressByteMode : public GribAddressMode {
public:
    GribAddressByteMode() {}
    ~GribAddressByteMode() {}
    virtual GribAddressMode* clone() const {
        GribAddressMode* mode = new GribAddressByteMode();
        return mode;
    }

    virtual grib_handle* operator()(grib_context* context, FILE* file, int position) const {
        long int offset = (long int)position;
        cout << "OFFSET-->" << offset << endl;
        fseek(file, (long int)position, SEEK_SET);
        grib_handle* handle = 0;

        int error;
        handle = grib_handle_new_from_file(0, file, &error);

        return handle;
    }
    virtual grib_handle* operator()(grib_context* context, FILE* file, long int position) const {
        fseek(file, position, SEEK_SET);
        grib_handle* handle = 0;

        int error;
        handle = grib_handle_new_from_file(0, file, &error);

        return handle;
    }

protected:
    void print(ostream& out) const { out << "GribAddressRecordMode\n"; }
};


template <>
class MagTranslator<string, GribAddressMode> {
public:
    GribAddressMode* operator()(const string& val) { return SimpleObjectMaker<GribAddressMode>::create(val); }

    GribAddressMode* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
