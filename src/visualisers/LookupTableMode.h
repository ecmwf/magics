/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LookupTableMode.h
    \brief Definition of the Template class LookupTableMode.

    Magics Team - ECMWF 2005

    Started: Tue 17-May-2005

    Changes:

*/

#ifndef LookupTableMode_H
#define LookupTableMode_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "FixedTableModeAttributes.h"
#include "Image.h"
#include "LookupTableModeAttributes.h"
#include "PaperPoint.h"
#include "RasterData.h"

namespace magics {


class LookupTableMode : public LookupTableModeAttributes {
public:
    LookupTableMode();
    virtual ~LookupTableMode();
    virtual void set(const map<string, string>& map)  // for MagML
    {
        LookupTableModeAttributes::set(map);
    }
    virtual void set(const XmlNode& node)  // for MagML
    {
        LookupTableModeAttributes::set(node);
    }
    virtual LookupTableMode* clone() const {
        LookupTableMode* object = new LookupTableMode();
        object->copy(*this);
        return object;
    }

    virtual void operator()(Image&, Raster&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    LookupTableMode(const LookupTableMode&);
    //! Overloaded << operator to copy - No copy allowed
    LookupTableMode& operator=(const LookupTableMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const LookupTableMode& p) {
        p.print(s);
        return s;
    }
};


class FixedTableMode : public LookupTableMode, public FixedTableModeAttributes {
public:
    FixedTableMode();
    virtual ~FixedTableMode();
    void set(const map<string, string>& map)  // for MagML
    {
        LookupTableMode::set(map);
        FixedTableModeAttributes::set(map);
    }
    void set(const XmlNode& node)  // for MagML
    {
        LookupTableMode::set(node);
        FixedTableModeAttributes::set(node);
    }

    virtual void operator()(Image&, Raster&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const FixedTableMode& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, LookupTableMode> {
public:
    LookupTableMode* operator()(const string& val) { return SimpleObjectMaker<LookupTableMode>::create(val); }

    LookupTableMode* magics(const string& param) {
        LookupTableMode* object;
        ParameterManager::update(param, object);
        return object;
    }
};


}  // namespace magics
#endif
