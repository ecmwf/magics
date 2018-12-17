/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IsoShading.cc
    \brief Implementation of the Template class IsoShading.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
    
*/



#include "IsoShading.h"
#include "Polyline.h"
#include "UserPoint.h"
#include "PaperPoint.h"
#include "MatrixHandler.h"
#include "PolyShadingMethod.h"
#include "IsoPlot.h"

using namespace magics;


IsoShading::IsoShading() 
{
 
  
}


IsoShading::~IsoShading()
{
}

void IsoShading::operator()(magics::Polyline* poly) const
{
    (*this->technique_)(poly);
   
}


/*!
 Class information are given to the output-stream.
*/	

void IsoShading::print(ostream& out)  const
{
	out << "IsoShading[";
	out << "]";
}


class FullArray : public CellArray {
public:
	FullArray(MatrixHandler& data, IntervalMap<int>& range, const Transformation& transformation, int, int, float, const string&);
	double rows() const { return rows_; }
	double columns() const { return columns_; }
	double value(const pair<int, int>& pos)  const {
		return data_(pos.first, pos.second);
	}
	double value(int row, int column)  const {
			return data_(row, column);
	}
	double row(int row, int column)  const {
			return data_.row(row, column);
	}
	double row(const pair<int, int>& pos)  const {
				return data_.row(pos.first, pos.second);
		}

	double column(const pair<int, int>& pos) const {
				return data_.column(pos.first, pos.second);
	}
	double column(int row, int column) const {
		return data_.column(row, column);
	}
        double range(const pair<int, int>& pos)  const {
					return rangeFinder_.find(data_(pos.first, pos.second), -1);	
		}

};


CellArray* NoIsoShading::array(MatrixHandler& matrix, IntervalMap<int>& range,
	        	    		const Transformation& transformation, int width, int height,
	        	    		float resolution, const string& technique)
{
		 return new CellArray(matrix, range, transformation, width, height, resolution, technique);

//		 return new FullArray(matrix, range, transformation, width, height, resolution, technique);

}
int IsoShading::shadingIndex(double value)
{
	return technique_->index(value);
}

int IsoShading::leftIndex(double value)
{
	return technique_->leftIndex(value);
}

int IsoShading::rightIndex(double value)
{
	return technique_->rightIndex(value);
}
