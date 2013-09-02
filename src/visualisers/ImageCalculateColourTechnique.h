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

/*! \file ImageCalculateColourTechnique.h
    \brief Definition of the Template class ImageCalculateColourTechnique.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 11-Jul-2005
    
    Changes:
    
*/

#ifndef ImageCalculateColourTechnique_H
#define ImageCalculateColourTechnique_H

#include "magics.h"

#include "ImageCalculateColourTechniqueAttributes.h"
#include "ColourTableDefinitionCompute.h"

namespace magics {

class ImageCalculateColourTechnique: public ImageCalculateColourTechniqueAttributes, public ColourTableDefinitionCompute {

public:
	ImageCalculateColourTechnique() {}
	virtual ~ImageCalculateColourTechnique() {}
	
	void prepare() { ColourTableDefinitionCompute::set(*this); }
	
	void set(const  map<string, string>& map) { ImageCalculateColourTechniqueAttributes::set(map); }
	void set(const  XmlNode& node) { ImageCalculateColourTechniqueAttributes::set(node); }
	bool accept(const string& node ) { return ImageCalculateColourTechniqueAttributes::accept(node); }

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
	friend ostream& operator<<(ostream& s,const ImageCalculateColourTechnique& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
