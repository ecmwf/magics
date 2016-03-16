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

/*! \file Boundaries.cc
    \brief Implementation of the Template class BoundariesBase.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 29-Aug-2006
    
    Changes:
	     2010 JUL change from mapgen to shp
    
*/


#include "Boundaries.h"
#include "Polyline.h"
#include "UserPoint.h"
#include "ShapeDecoder.h"

using namespace magics;

Boundaries::Boundaries() 
{
}

Boundaries::~Boundaries() 
{
}


/*!
 Class information are given to the output-stream.
*/		
void Boundaries::print(ostream& out)  const
{
	out << "Boundaries[";
	BoundariesAttributes::print(out);
	out << "]";
}


void Boundaries::operator()(const map<string, string>& setting, BasicGraphicsObjectContainer& task)
{
	ShapeDecoder boundaries, disputed;    
    boundaries.needHoles(true);
    disputed.needHoles(true);

	const string political_boundaries = setting.find("boundaries")->second;
	const string administrative_boundaries = setting.find("administrative_boundaries")->second;
	const string share_folder = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_;

	string file = share_folder + political_boundaries;

	boundaries.setPath(file);
	vector<string> treaty;
	treaty.push_back("International boundary");
	treaty.push_back("Country_Boundary");

	boundaries.decode(task.transformation(), "featurecla", treaty);
	const Transformation& transformation = task.transformation();
	
	if ( admistrative_)
	{
		ShapeDecoder admistrative;
        admistrative.needHoles(true);
		file = share_folder + administrative_boundaries;

		admistrative.setPath(file);
		admistrative.decode(task.transformation(),"adm0_a3", administrative_list_);
		const Transformation& transformation = task.transformation();

		for ( ShapeDecoder::const_iterator boundary = admistrative.begin(); boundary != admistrative.end(); ++boundary)
		{
			Polyline poly;
			poly.setColour( administrative_colour_->automatic() ? *colour_ : *administrative_colour_);
			poly.setThickness(administrative_thickness_);
			poly.setLineStyle(administrative_style_);

			(**boundary).setToFirst();
			while ((**boundary).more())
			{
				  poly.push_back(transformation((**boundary).current()));
				  (**boundary).advance();
			}
			transformation(poly, task);
		}
	}

	for ( ShapeDecoder::const_iterator boundary = boundaries.begin(); boundary != boundaries.end(); ++boundary)
	{
		Polyline poly;
		poly.setColour(*colour_);
		poly.setThickness(thickness_);
		poly.setLineStyle(style_);
		
		(**boundary).setToFirst();
		while ((**boundary).more())
		{
			  poly.push_back(transformation((**boundary).current()));
			  (**boundary).advance();
		}
		transformation(poly, task);
	}
	
	if ( disputed_)
	{
		ShapeDecoder disputed;
        disputed.needHoles(true);
	
		vector<string> dis;
		dis.push_back("Disputed");
		file = share_folder + political_boundaries;
	
		disputed.setPath(file);
		disputed.decode(task.transformation(),"featurecla", dis);
		const Transformation& transformation = task.transformation();

		for ( ShapeDecoder::const_iterator boundary = disputed.begin(); boundary != disputed.end(); ++boundary)
		{
			Polyline poly;
			poly.setColour( disputed_colour_->automatic() ? *colour_ : *disputed_colour_);
			poly.setThickness(disputed_thickness_);
			poly.setLineStyle(disputed_style_);


			(**boundary).setToFirst();
			while ((**boundary).more())
			{
			  poly.push_back(transformation((**boundary).current()));
			  (**boundary).advance();
			}
			transformation(poly, task);
		}
	}
}


NoBoundaries::NoBoundaries() 
{
}

NoBoundaries::~NoBoundaries() 
{
}

void NoBoundaries::print(ostream& out)  const
{
	out << "NoBoundaries[";
	out << "]";
}
