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

/*! \file WindPlotting.cc
    \brief Implementation of the Template class Wind.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#include "Streamlines.h"
#include "LegendVisitor.h"
#include "CalcStreamlines.h"
#include "Timer.h"
#include "Polyline.h"

using namespace magics;

bool Streamlines::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{
	Timer timer("Streamlines", "Streamlines");


	const Transformation& transformation = parent.transformation();

	MatrixHandler* handler = transformation.prepareData(data.direction());



	float* direction = new float[handler->rows()*handler->columns()];
	int i = 0;
	for (int row = 0; row < handler->rows(); row++ )
		for (int column = 0; column < handler->columns(); column++ ) {
			direction[i] = (*handler)(row, column);
			i++;
		}


	GSStruct *gs = new GSStruct();
	gs->nx = handler->columns();
	gs->ny = handler->rows();


	gs->startx = handler->column(0,0);
	gs->starty = handler->row(0,0);

	// Distance between the gridpoints
	gs->dx = handler->XResolution();
	gs->dy = -handler->YResolution();
	gs->period_x = 0;

	OneLineClass ** result = 0;
	int size;


	CalcStreamlines(min_density_, direction, gs, result, size);

	for(int l = 0; l < size; l++)
    {

        Polyline poly;
        poly.setColour(*colour_);
        poly.setThickness(thickness_);
        poly.setLineStyle(style_);

        ArrowProperties* arrow = new ArrowProperties();
        poly.setArrow(arrow);
        for(int i = 0; i < result[l]->Len; i++)
        {
        	//if (result[l]->X[i] > 180) result[l]->X[i] -= 360;
        	poly.push_back(transformation(UserPoint(result[l]->X[i], result[l]->Y[i])));

        	transformation(poly, parent);
        }

    }
	delete handler;
	return true;
}

void Streamlines::visit(LegendVisitor& legend)
{

}

void Streamlines::print(ostream& out) const
{
	out << "Streamlines[";
	StreamlinesAttributes::print(out);
	out << "]";
}

