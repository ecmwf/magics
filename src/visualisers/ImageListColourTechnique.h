/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
