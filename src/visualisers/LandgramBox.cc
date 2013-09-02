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

/*! \file LandgramBox.cc
    \brief Implementation of the Template class LandgramBox.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/



#include "LandgramBox.h"
#include "PointsHandler.h"
#include "DateTime.h"
#include "Text.h"
#include "Polyline.h"
#include "Graph.h"


using namespace magics;



LandgramBox::LandgramBox() 
{}


LandgramBox::~LandgramBox() 
{}

/*!
 Class information are given to the output-stream.
*/		
void LandgramBox::print(ostream& out)  const
{
	out << "LandgramBox[";
	out << "]";
}



void LandgramBox::operator()(Data& /*data*/, BasicGraphicsObjectContainer& /*out*/)
{
/*
	CustomisedPointsList points; 
	std::set<magstring> request;
	data.customisedPoints(request, points);

	Box& box = owner.layout().box("drawing_area");	
	box.setClip(false);
	
	if (points.empty()) return;
	
	

	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point)
	{
		    MagLog::dev() << **point << endl;
		   
		    double step = (**point)["step"];
		    double top2 = (**point)["box2"];
		    double top1 = (**point)["box1"];
		    double bottom1 = (**point)["box-1"]; 
		    double bottom2 = (**point)["box-2"];
			Polyline<UserPoint>* box1  = new Polyline<UserPoint>(LayerManager::layer("landgram"), "drawing_area");				
			box1->setFilled(true);	
			box1->setFillColour(*top1_colour_);
			box1->setColour(Colour("black"));
			ShadingProperties* shading1 = (magCompare(top1_shading_, "solid") ) ? (ShadingProperties*)new FillShadingProperties() : (ShadingProperties*)new DotShadingProperties();
			box1->setShading(shading1);			
			
			box1->push_back(UserPoint(step-width_, 0));
			box1->push_back(UserPoint(step-width_, top1));
			box1->push_back(UserPoint(step, top1));
			box1->push_back(UserPoint(step, 0));
				
			Polyline<UserPoint>* box2  = new Polyline<UserPoint>(LayerManager::layer("landgram"), "drawing_area");				
			box2->setFilled(true);	
			box2->setFillColour(*top2_colour_);
			box2->setColour(Colour("black"));
			ShadingProperties* shading2 = (magCompare(top2_shading_, "solid") ) ? (ShadingProperties*)new FillShadingProperties() : (ShadingProperties*)new DotShadingProperties();
			box2->setShading(shading2);			
			
			box2->push_back(UserPoint(step-width_, top1));
			box2->push_back(UserPoint(step-width_, top2+top1));
			box2->push_back(UserPoint(step, top2+top1));
			box2->push_back(UserPoint(step, top1));
			
			Polyline<UserPoint>* box3  = new Polyline<UserPoint>(LayerManager::layer("landgram"), "drawing_area");				
			box3->setFilled(true);	
			box3->setFillColour(*bottom1_colour_);
			box3->setColour(Colour("black"));
			ShadingProperties* shading3 = (magCompare(bottom1_shading_, "solid") ) ? (ShadingProperties*)new FillShadingProperties() : (ShadingProperties* )new DotShadingProperties();
			box3->setShading(shading3);			
			
			box3->push_back(UserPoint(step-width_, 0));
			box3->push_back(UserPoint(step-width_, bottom1));
			box3->push_back(UserPoint(step, bottom1));
			box3->push_back(UserPoint(step, 0));
				
			Polyline<UserPoint>* box4  = new Polyline<UserPoint>(LayerManager::layer("landgram"), "drawing_area");				
			box4->setFilled(true);	
			box4->setFillColour(*bottom2_colour_);
			box4->setColour(Colour("black"));
			ShadingProperties* shading4 = (magCompare(bottom2_shading_, "solid") ) ? (ShadingProperties*) new FillShadingProperties() : (ShadingProperties*)new DotShadingProperties();
			box4->setShading(shading4);			
			
			box4->push_back(UserPoint(step-width_, bottom1));
			box4->push_back(UserPoint(step-width_, bottom2+bottom1));
			box4->push_back(UserPoint(step, bottom2+bottom1));
			box4->push_back(UserPoint(step, bottom1));
			
			out.push_back(box1);
			out.push_back(box2);
			out.push_back(box3);
			out.push_back(box4);
	}
	*/	
}

void LandgramBox::visit(LegendVisitor&)
{
	//legend.push_back(new EpsEntry());
}
