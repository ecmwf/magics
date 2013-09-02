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

/*! \file InputMatrixRegularInterpretor.h
    \brief Implementation of the Template class InputMatrixRegularInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 16-Sep-2005
    
    Changes:
    
*/



#include "GeoMatrixInterpretor.h"
#include "InputMatrix.h"

using namespace magics;

InputMatrixRegularInterpretor::InputMatrixRegularInterpretor() 
{
	mappers_["upper_left"] = &InputMatrixRegularInterpretor::upperLeft;
    mappers_["lower_left"] = &InputMatrixRegularInterpretor::lowerLeft;
    mappers_["upper_right"] = &InputMatrixRegularInterpretor::upperRight;
    mappers_["lower_right"] = &InputMatrixRegularInterpretor::lowerRight;     
    mappers_["upper_left_transposed"] = &InputMatrixRegularInterpretor::upperLeftTransposed;
    mappers_["lower_left_transposed"] = &InputMatrixRegularInterpretor::lowerLeftTransposed;
    mappers_["upper_right_transposed"] = &InputMatrixRegularInterpretor::upperRightTransposed;
    mappers_["lower_right_transposed"] = &InputMatrixRegularInterpretor::lowerRightTransposed;
     
}


InputMatrixRegularInterpretor::~InputMatrixRegularInterpretor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void InputMatrixRegularInterpretor::print(ostream& out)  const
{
	out << "GeoMatrixInterpretor[";
	out << "]";
}

void InputMatrixRegularInterpretor::interpret(Matrix& in, const InputMatrix<UserPoint>& info)
{
	
	std::map<string, Mapper>::iterator mapper = mappers_.find(lowerCase(info.getMapping()));

	if ( mapper == mappers_.end() )
		MagLog::warning() << "unknow input matrix mapping " << info.getMapping() << endl;
	else 
		(this->*mapper->second)();
	
	
	int nblon =  in.columns();
	double lon = longitude_;
	
	for (int i = 0; i < nblon; i++) {		
		in.columnsAxis().push_back(lon);
		lon += longitude_step_;		
	}
    
    int nblat =  in.rows();
	double lat = latitude_;
	
	for (int i = 0; i < nblat; i++) {		
		in.rowsAxis().push_back(lat);
		lat += latitude_step_;
	}
    
    
	
}


void InputMatrixRegularInterpretor::interpret(Matrix& in, const InputMatrix<UserPoint>& info)
{

	  
	    double y = y_first_;
	    double step = (y_last_ - y_first_)/(in.rows()-1);
		for (int i = 0; i < in.rows(); i++) {
			MagLog::dev() << y << endl;
			in.rowsAxis().push_back(y);
			y += step;
		}
		
		double x = x_first_;
		 step = (x_last_ - x_first_)/(in.columns()-1);
		 for (int i = 0; i < in.columns(); i++) {
			MagLog::dev() << x << endl;
			in.columnsAxis().push_back(x);
			x += step;
		}
		
		in.setMapsAxis();
		
}


void InputMatrixRegularInterpretor::upperLeft()
{
	
}

void InputMatrixRegularInterpretor::lowerLeft()
{
}

void InputMatrixRegularInterpretor::upperRight()
{
}

void InputMatrixRegularInterpretor::lowerRight()   
{
}

  
void InputMatrixRegularInterpretor::upperLeftTransposed()
{
}

void InputMatrixRegularInterpretor::lowerLeftTransposed()
{
}

void InputMatrixRegularInterpretor::upperRightTransposed()
{
}

void InputMatrixRegularInterpretor::lowerRightTransposed()
{
}

