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

void IsoShading::operator()(Polyline* poly) const
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
	technique_->leftIndex(value);
}

int IsoShading::rightIndex(double value)
{
	return technique_->rightIndex(value);
}
