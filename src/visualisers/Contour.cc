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

/*! \file Contour.cc
    \brief Implementation of the Template class Contour.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 3-Mar-2004

    Changes:
    
*/

#include "Contour.h"
#include "Layout.h"
#include "Layer.h"
#include "Text.h"
#include "ContourLibrary.h"
#include "HistoVisitor.h"

using namespace magics;


Contour::Contour() : matrix_(0)
{
}


Contour::~Contour()
{
	if (matrix_) delete(matrix_);
}

/*!
 Class information are given to the output-stream.
*/	

void Contour::print(ostream& out)  const
{
	out << "Contour[";
	ContourAttributes::print(out);
	out << "]";
}


class MatrixTreshold : public MatrixHandler
{
public :
	MatrixTreshold(const AbstractMatrix& matrix, double min, double max) : 
		MatrixHandler(matrix), min_(min), max_(max)
	{
	}
	double operator()(int row, int column) const { 
		double val = this->matrix_(row, column); 
		if ( same(val, this->matrix_.missing()) ) 
				return val;
		if ( val  < min_ ) return min_;
		if ( val  > max_ ) return max_;
		return val; 
	}
	double min_;
	double max_;
};


void Contour::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{
    try {
    	ContourLibrary* library = MagTranslator<string, ContourLibrary>()(setting_);

    		// Here we try call the Contour libry to set up visual properties...
    		MetaDataCollector needId,needAttributes;
    		map<string, string> attributes;
    		

		library->askId(needId);
		data.visit(needId);

		if(library->checkId(needId,needAttributes))
		{			
    			data.visit(needAttributes);
    			library->getAttributes(needAttributes,attributes);
    			this->set(attributes);
    	}
		else {
			library->getAttributes(needId,attributes);
		}
		delete library;


    data.getReady(parent.transformation());

	MatrixHandler* box =  data.matrix().getReady(parent.transformation());

	if ( !box->rows() ||  !box->columns() ) {
		(*this->contour_)(data, parent);
		(*this->grid_)(data, parent);
		matrix_ = 0;
		delete box;
		return;
	}

	matrix_ = (*this->method_).handler(*box, parent);


	if (this->floor_ != -INT_MAX || this->ceiling_ != INT_MAX)
		matrix_ = new MatrixTreshold(*matrix_, this->floor_, this->ceiling_);

	(*this->contour_).adjust(matrix_->min(), matrix_->max());
	(*this->contour_)(*matrix_, parent);
	(*this->contour_)(data, parent);
	if ( magCompare( this->grid_->getType(), "akima" ) )
		(*this->grid_)(*matrix_, parent);
	else 
		(*this->grid_)(data, parent);
	(*this->hilo_)(*matrix_, parent);



// We do not need the box anymore!
	delete box;



    }
    catch (MagicsException& e) 
    {
    	// Do nothing! 
    }
}


void  Contour::visit(Data& data, HistoVisitor& visitor)
{
	if ( !matrix_ )
		return;


	contour_->visit(data, data.points(*visitor.dataLayoutTransformation(), false), visitor);
	
}

static SimpleObjectMaker<ContourLibrary, ContourLibrary> obstat("on");
static SimpleObjectMaker<EcChartLibrary, ContourLibrary> ecchart("ecchart");
static SimpleObjectMaker<NoContourLibrary, ContourLibrary> off("off");


void Contour::visit(Data& data, LegendVisitor& legend)
{
	if ( !this->legend_ ) return;
	contour_->visit(data, legend);

}

