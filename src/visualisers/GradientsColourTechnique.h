/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GradientsColourTechnique.h
    \brief Definition of the Template class GradientsColourTechnique.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/

#ifndef GradientsColourTechnique_H
#define GradientsColourTechnique_H

#include "magics.h"

#include "ColourTechnique.h"
#include "GradientsColourTechniqueAttributes.h"

namespace magics {

class GradientsColourTechnique : public ColourTechnique, public GradientsColourTechniqueAttributes {
public:
    GradientsColourTechnique();
    virtual ~GradientsColourTechnique();
    void set(const map<string, string>& map) { GradientsColourTechniqueAttributes::set(map); }
    void set(const XmlNode& node) { GradientsColourTechniqueAttributes::set(node); }
    bool accept(const string& node) { return GradientsColourTechniqueAttributes::accept(node); }

    void set(const ColourTechniqueInterface&);


    virtual ColourTechnique* clone() const {
        GradientsColourTechnique* object = new GradientsColourTechnique();
        object->copy(*this);
        return object;
    }

protected:
    void set(LevelSelection&, LevelSelection&, ColourTable&, int) const;
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    GradientsColourTechnique(const GradientsColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    GradientsColourTechnique& operator=(const GradientsColourTechnique&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const GradientsColourTechnique& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
