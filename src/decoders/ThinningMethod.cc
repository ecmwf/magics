/*! \file ThinningMethod.h
    \brief Implementation of the Template class ThinningMethod.
    
    Magics Team - ECMWF 2010
    
    Started: Thu 28-Oct-2010
    
    Changes:
    
*/



#include "ThinningMethod.h"
#include "Data.h"

using namespace magics;

ThinningMethod::ThinningMethod() 
{
}


ThinningMethod::~ThinningMethod() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ThinningMethod::print(ostream& out)  const
{
	out << "ThinningMethod[";
	out << "]";
}

AutomaticThinningMethod::AutomaticThinningMethod() 
{
}


void ThinningMethod::operator()(Data&, const Transformation&, const std::set<string>&, CustomisedPointsList&)
{
}


void BasicThinningMethod::operator()(Data&data,
		const Transformation& transformation,
		const std::set<string>& request, CustomisedPointsList& points)
{
	try {
		data.customisedPoints(*this, transformation, request, points);
	}

	catch ( ...) {
		data.customisedPoints(transformation, request, points,false);
	}
}


void AutomaticThinningMethod::operator()(Data& data,
		const Transformation& transformation, const std::set<string>& request, CustomisedPointsList& points)
{

	try {
				data.customisedPoints(*this, transformation, request, points);
			}

			catch ( ...) {
				data.customisedPoints(transformation, request, points, false);
			}
}

AutomaticThinningMethod::~AutomaticThinningMethod() 
{
}

void AutomaticThinningMethod::set(const ThinningMethodUI& ui) 
{
	nbPoints_ = ui.nbPoints_;
	x_ = ui.width_*nbPoints_;
	y_ = ui.height_*nbPoints_;
	units_ = nbPoints_;
	rawOnly_ = ui.rawOnly_; // If true, do not thin .. only show raw data if resolution allows it! 
}


BasicThinningMethod::BasicThinningMethod() 
{
}


BasicThinningMethod::~BasicThinningMethod() 
{
}

void BasicThinningMethod::set(const ThinningMethodUI& ui) 
{
	factor_ = ui.factor_;
}

static SimpleObjectMaker<AutomaticThinningMethod, ThinningMethod> automatic("automatic");
static SimpleObjectMaker<BasicThinningMethod, BasicThinningMethod> basic("basic");
static SimpleObjectMaker<BasicThinningMethod, BasicThinningMethod> data("data");



