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

/*! \file PolyShadingMethod.h
    \brief Definition of the Template class PolyShadingMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/


#include "PolyShadingTechnique.h"
#include "IsoPlot.h"

using namespace magics;
void ShadingTechnique::operator()(IsoPlot* iso, MatrixHandler& data, BasicGraphicsObjectContainer& parent)
{
	iso->isoline(data, parent);
}

CellArray* PolyShadingTechnique::array(MatrixHandler& matrix, IntervalMap<int>& range,
  	    		const Transformation& transformation, int width, int height,
  	    		float resolution, const string& technique)
{

	return new CellArray(matrix, range, transformation, width, height, resolution, technique);

}
void GridShading::visit(LegendVisitor& legend, const ColourTechnique& colour)
{
	MagLog::dev() << "Create legend information"  << "\n";
	LegendEntryBuilder helper(legend, colour);

	std::adjacent_find(colour.begin(), colour.end(), helper);

	if ( colour.size() == 1 ) {
		helper(*colour.begin(), *colour.begin());
	}
	legend.last(); // Flag the last entry as being the last! To get a nice labelling in cotinuous mode!!!


}
CellArray* GridShading::array(MatrixHandler& matrix, IntervalMap<int>& range,
		const Transformation& transformation, int width, int height,
		float resolution, const string& technique)
{

	return new GridArray(matrix, range, transformation, width, height, resolution, position_);

}
void GridShading::operator()(Polyline* poly) const
{
	int index = poly->index();

	        poly->setStroke(false);
	        poly->setFilled(true);
	        poly->setFillColour(method_->colours_[index]);
	        FillShadingProperties* shading = new FillShadingProperties();
	        poly->setShading(shading);
}
