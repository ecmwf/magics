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

/*! \file LandgramDecoder.h
    \brief Implementation of the Template class LandgramDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 25-Mar-2004
    
    Changes:
    
*/



#include "LandgramDecoder.h"
#include "Factory.h"
#include <limits>
#include <TextVisitor.h>
using namespace magics;

LandgramDecoder::LandgramDecoder(): first_(0) 
{
	/*
	ifstream f("/home/graphics/cgs/public/map.txt");
	assert(f);
	int rows, columns;
	double lon, lat, inclon, inclat;
	f >> rows >> columns;
	f >> lat >> lon >> inclat >> inclon;
	matrix_.set(rows, columns);
	double missing = -std::numeric_limits<magfloat>::max();
     
	matrix_.missing(missing);
	
	char c;
    while (! f.eof() )
    {
      f.get(c);      
   
      if (isdigit(c) )   
      	matrix_.push_back(atoi(&c));
      if (c == '.') 
      	matrix_.push_back(missing);
         
    }
    f.close();
    
    
  
	
	
    for (int i = 0; i < columns; i++) {
           matrix_.columnsAxis().push_back(lon);
           lon+=inclon;
    }
    
    for (int i = 0; i < rows; i++) {
    	matrix_.rowsAxis().push_back(lat);
    	lat+=inclat;
    }

   
    matrix_.setMapsAxis();

    
   */
}


void LandgramDecoder::customisedPoints(const std::set<string>&, CustomisedPointsList& points)
{
	decode();
	for (unsigned int step = 0; step < steps_.size(); step++)
	{
		CustomisedPoint* point = new CustomisedPoint(0,0,"box");
		point->longitude(steps_[step]);		
		point->insert(make_pair("step", steps_[step]));	
		for (unsigned int height = 0; height < heights_.size(); height++)
		{
			ostringstream key;
			key << "box" <<  heights_[height];
			point->insert(make_pair(key.str(), values_[(step*heights_.size()) + height]));	
		}
		points.push_back(point);
	} 
}

void LandgramDecoder::decode()
{
	
	if ( !steps_.empty() ) return;
	
	XmlReader parser(true);
	XmlTree tree;	

		try {
				parser.interpret(path_, &tree);		
				tree.visit(*this);
		}
		catch (MagicsException& e) {
					MagLog::debug() << e.what() << endl;
		}
		
		
		matrix_.set(steps_.size(), heights_.size());
		
		for ( vector<double>::const_iterator height =heights_.begin(); height != heights_.end(); ++height) {
			matrix_.rowsAxis().push_back(*height);
		}	
		MagLog::dev()<< " step-->" <<endl;
		for ( vector<double>::const_iterator step = steps_.begin(); step != steps_.end(); ++step) {
			matrix_.columnsAxis().push_back(*step);
			MagLog::dev()<< *step << " ";
		}
		MagLog::dev()<< endl << "<--- step" <<endl;
		matrix_.setMapsAxis();
//		int i = 0;
		for (unsigned int height = 0; height < heights_.size(); height++)
		{
			MagLog::dev()<< endl <<  heights_[height] << " " << steps_.size() << endl;
			for (unsigned int step = 0; step < steps_.size(); step++)
			{
				matrix_.push_back(values_[(step*heights_.size()) + height]);
				MagLog::dev()<< matrix_.back() << " ";
			}
		}
	MagLog::debug() << matrix_ << endl;
}

LandgramDecoder::~LandgramDecoder() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void LandgramDecoder::print(ostream& out)  const
{
	out << "LandgramDecoder[";
	out << "]";
}

void LandgramDecoder::visit(const XmlNode& node)
{
	
	MagLog::dev() << node.name() << endl;
	if ( node.name() == "forecast" ) {
			node.visit(*this);
	}
	else if ( node.name() == "step" ) {
		    first_ ++;
			MagLog::dev()<< "step --> " << tonumber(node.getAttribute("value")) << endl;
			steps_.push_back(tonumber(node.getAttribute("value")));
			node.visit(*this);
			MagLog::dev()<< "step --> " <<  steps_.size();
		}
	else {		
			double height =  tonumber(node.getAttribute("height")) ;
			double value = tonumber(node.getAttribute("value")) ;
			if (first_ == 1) 
				heights_.push_back(height);
			
			values_.push_back((value*scaling_)+offset_);
			MagLog::dev()<< "height --> " << tonumber(node.getAttribute("height")) <<  " = value " << tonumber(node.getAttribute("value")) << endl;
			node.visit(*this);
	}
}

void LandgramDecoder::visit(TextVisitor& node)
{
	TextEntry* text = new TextEntry(title_);
	node.add(text);
}
