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

/*! \file LegendMethod.cc
    \brief Implementation of the Template class LegendMethod.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 9-May-2006
    
    Changes:
    
*/

#include "LegendMethod.h"
#include "LegendVisitor.h"



using namespace magics;

LegendMethod::LegendMethod() 
{
}


LegendMethod::~LegendMethod() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void LegendMethod::print(ostream& out)  const
{
	out << "LegendMethod[";
	out << "]";
}

void LegendMethod::row(LegendEntry& entry, double x, double y, Text& legend, BasicGraphicsObjectContainer& task) 
{
	PaperPoint middle(x, y);
	entry.set(middle, task);
	
	legend.setJustification(MLEFT);		
	legend.push_back(entry.leftTextBox(middle)); // WE attach the text on the rigth of the sumbol!
} 	

void LegendMethod::column(LegendEntry& entry, double x, double y, Text& legend, BasicGraphicsObjectContainer& task) 
{
	
	PaperPoint middle(x, y);
	entry.set(middle, task);
    
	legend.setJustification(MLEFT);
	legend.push_back(entry.leftTextBox(middle)); // WE attach the text on the rigth of the sumbol!
	

} 

void ContinuousLegendMethod::row(LegendEntry& entry, double x, double y, Text& legend, BasicGraphicsObjectContainer& task) 
{
	if ( labelCount_ % label_frequency_ != 0 )
		entry.notext();
	PaperPoint middle(x, y);
	entry.rowBox(middle, task);
	if ( labelCount_ % label_frequency_ == 0 )
		if ( entry.needContinuousText(legend) ) {
		
			middle.y_ -= 0.5;
			legend.push_back(middle); // We attach the text on the top middle of the symbol!
		}

	labelCount_++;
} 		

void ContinuousLegendMethod::column(LegendEntry& entry, double x, double y, Text& legend, BasicGraphicsObjectContainer& task) 
{
	if ( labelCount_ % label_frequency_ != 0 )
		entry.notext();
	entry.columnBox(PaperPoint(x, y), task);
	if ( labelCount_ % label_frequency_ == 0 )
	
		if ( entry.needContinuousText(legend) ) {
			
			legend.push_back(PaperPoint(x+0.25, y)); // WE attach the text on the right of the sumbol!
		}
	labelCount_++;
} 	
void HistogramLegendMethod::row(LegendEntry& entry, double x, double y, Text&, BasicGraphicsObjectContainer& out)
{
	if ( labelCount_ % label_frequency_ != 0 )
		entry.notext();
	PaperPoint middle(x, y);
	Colour colour = histo_border_ ? *histo_border_colour_ : Colour("automatic");
	ostringstream m;
	m << "magics_" << histo_mean_marker_;
	entry.histogramInformation(this);
	entry.rowHisto(middle, out, colour);
	labelCount_++;
}

void HistogramLegendMethod::column(LegendEntry& entry, double x, double y, Text&, BasicGraphicsObjectContainer& out)
{
	if ( labelCount_ % label_frequency_ != 0 )
		entry.notext();
	PaperPoint middle(x, y);
	Colour colour = histo_border_ ? *histo_border_colour_ : Colour("automatic");
	ostringstream m;
	m << "magics_" << histo_mean_marker_;
	entry.histogramInformation(this);
	entry.columnHisto(middle, out, colour);
	labelCount_++;
}
