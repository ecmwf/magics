/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImageListColourTechnique.h
    \brief Definition of the Template class ImageListColourTechnique.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 11-Jul-2005
    
    Changes:
    
*/

#ifndef ImageListColourTechnique_H
#define ImageListColourTechnique_H

#include "magics.h"

#include "ImageListColourTechniqueAttributes.h"
#include "ColourTableDefinitionList.h"

namespace magics {

class ImageListColourTechnique: public ImageListColourTechniqueAttributes, public ColourTableDefinitionList {

public:
	ImageListColourTechnique() {}
	virtual ~ImageListColourTechnique() {} 
	
	void prepare() { ColourTableDefinitionList::set(*this); }
	void set(const  map<string, string>& map) { ImageListColourTechniqueAttributes::set(map); }
	void set(const  XmlNode& node) { ImageListColourTechniqueAttributes::set(node); }
	bool accept(const string& node ) { return ImageListColourTechniqueAttributes::accept(node); }

	virtual stringarray getValues() const { return values_; }
	virtual ListPolicy getPolicy() const { return M_LASTONE; }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out ) const { out <<  "ImageListColourTechnique[]"; }

private:
    //! Copy constructor - No copy allowed
	ImageListColourTechnique(const ImageListColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	ImageListColourTechnique& operator=(const ImageListColourTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ImageListColourTechnique& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
