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

/*! \file ObsStatGraph.cc
    \brief Implementation of the Template class ObsStatGraph.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/



#include "ObsStatGraph.h"
#include "PointsHandler.h"
#include "Layer.h"
#include "DateTime.h"
#include "Layer.h"

using namespace magics;

ObsStatGraph::ObsStatGraph() 
{}


ObsStatGraph::~ObsStatGraph() 
{}

/*!
 Class information are given to the output-stream.
*/		
void ObsStatGraph::print(ostream& out)  const
{
	out << "ObsStatGraph[";
	out << "]";
}



void ObsStatGraph::preparePlot(Data<PaperPoint>& data, Task& task)
{
	CustomisedPointsList points; 
	std::set<string> request;
	data.customisedPoints(request, points);
	Box& area = getLayout().box("drawing_area");
	Layer * layer = new Layer();
	Polyline* line2  = new Polyline(layer, "drawing_area");
	line2->setColour(Colour("red"));
	line2->setLineStyle(M_DASH);
	line2->setThickness(2);
	task.push_back(line2);
	Polyline* line1  = new Polyline(layer, "drawing_area");
	line1->setColour(Colour("blue"));
	line1->setThickness(2);
	task.push_back(line1);
	
	
	int x = 1;	
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		if ( (**point).find("rms_1") !=   (**point).end() )
			(*line1).push_back(PaperPoint((**point)["rms_1"], (**point)["y"]));
		if ( (**point).find("rms_2") !=   (**point).end() ) 
			(*line2).push_back(PaperPoint((**point)["rms_2"], (**point)["y"]));
		x++;
	}
	MagLog::dev()<< area << endl;
}

void ObsStatGraph::visit(LegendBase&)
{}

