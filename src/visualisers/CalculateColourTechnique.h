/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CalculateColourTechnique.h
    \brief Definition of the Template class CalculateColourTechnique.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/

#ifndef CalculateColourTechnique_H
#define CalculateColourTechnique_H

#include "magics.h"

#include "CalculateColourTechniqueAttributes.h"
#include "ColourTechnique.h"

namespace magics {

class CalculateColourTechnique : public ColourTechnique, public CalculateColourTechniqueAttributes {
public:
    CalculateColourTechnique();
    virtual ~CalculateColourTechnique();
    void set(const map<string, string>& map) { CalculateColourTechniqueAttributes::set(map); }
    void set(const XmlNode& node) { CalculateColourTechniqueAttributes::set(node); }
    bool accept(const string& node) { return CalculateColourTechniqueAttributes::accept(node); }

    void set(const ColourTechniqueInterface&);


    virtual ColourTechnique* clone() const {
        CalculateColourTechnique* object = new CalculateColourTechnique();
        object->copy(*this);
        return object;
    }
    const Colour& getMax() const { return *max_; }
    const Colour& getMin() const { return *min_; }
    const string& getDirection() const { return direction_; }

protected:
    void set(LevelSelection&, LevelSelection&, ColourTable&, int) const;
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    CalculateColourTechnique(const CalculateColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    CalculateColourTechnique& operator=(const CalculateColourTechnique&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const CalculateColourTechnique& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
