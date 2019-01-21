/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImageCalculateColourTechnique.h
    \brief Definition of the Template class ImageCalculateColourTechnique.

    Magics Team - ECMWF 2005

    Started: Mon 11-Jul-2005

    Changes:

*/

#ifndef ImageCalculateColourTechnique_H
#define ImageCalculateColourTechnique_H

#include "magics.h"

#include "ColourTableDefinitionCompute.h"
#include "ImageCalculateColourTechniqueAttributes.h"

namespace magics {

class ImageCalculateColourTechnique : public ImageCalculateColourTechniqueAttributes,
                                      public ColourTableDefinitionCompute {
public:
    ImageCalculateColourTechnique() {}
    virtual ~ImageCalculateColourTechnique() {}

    void prepare() { ColourTableDefinitionCompute::set(*this); }

    void set(const map<string, string>& map) { ImageCalculateColourTechniqueAttributes::set(map); }
    void set(const XmlNode& node) { ImageCalculateColourTechniqueAttributes::set(node); }
    bool accept(const string& node) { return ImageCalculateColourTechniqueAttributes::accept(node); }

    const Colour& getMax() const { return *max_; }
    const Colour& getMin() const { return *min_; }
    const string& getDirection() const { return ImageCalculateColourTechniqueAttributes::direction_; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "ImageCalculateColourTechnique[]"; }

private:
    //! Copy constructor - No copy allowed
    ImageCalculateColourTechnique(const ImageCalculateColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
    ImageCalculateColourTechnique& operator=(const ImageCalculateColourTechnique&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ImageCalculateColourTechnique& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
