/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HeightSelection.h
    \brief Definition of the Template class HeightSelection.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef OutLayerTechnique_H
#define OutLayerTechnique_H

#include "magics.h"


#include "Factory.h"
#include "MagTranslator.h"

namespace magics {

class XmlNode;

class OutLayerTechniqueInterface {
public:
    OutLayerTechniqueInterface() {}
    virtual ~OutLayerTechniqueInterface() {}
    virtual float getMinOutlayer() const = 0;
    virtual float getMaxOutlayer() const = 0;
};

class NoOutLayerTechnique {
public:
    NoOutLayerTechnique();
    virtual ~NoOutLayerTechnique();
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }

    virtual NoOutLayerTechnique* clone() const { return new NoOutLayerTechnique(); }

    void toxml(ostream&) const {}


protected:
    //! Method to print  about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    NoOutLayerTechnique(const NoOutLayerTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    NoOutLayerTechnique& operator=(const NoOutLayerTechnique&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NoOutLayerTechnique& p) {
        p.print(s);
        return s;
    }
};

class SimpleOutLayerTechnique : public NoOutLayerTechnique {
public:
    SimpleOutLayerTechnique();
    virtual ~SimpleOutLayerTechnique();
};


template <>
class MagTranslator<string, NoOutLayerTechnique> {
public:
    NoOutLayerTechnique* operator()(const string& val) { return SimpleObjectMaker<NoOutLayerTechnique>::create(val); }

    NoOutLayerTechnique* magics(const string& param) {
        NoOutLayerTechnique* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
