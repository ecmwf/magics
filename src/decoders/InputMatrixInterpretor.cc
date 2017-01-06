/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file InputMatrixInterpretor.h
    \brief Implementation of the Template class InputMatrixInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 16-Sep-2005
    
    Changes:
    
*/

#include "InputMatrixInterpretor.h"
#include "InputMatrix.h"
#include "GribDecoder.h"
#include <limits>

using namespace magics;

InputMatrixInterpretor::InputMatrixInterpretor() 
{
	mappers_["upper_left"] = &InputMatrixInterpretor::upperLeft;
	mappers_["lower_left"] = &InputMatrixInterpretor::lowerLeft;
	mappers_["upper_right"] = &InputMatrixInterpretor::upperRight;
	mappers_["lower_right"] = &InputMatrixInterpretor::lowerRight;     
	mappers_["upper_left_transposed"] = &InputMatrixInterpretor::upperLeftTransposed;
	mappers_["lower_left_transposed"] = &InputMatrixInterpretor::lowerLeftTransposed;
	mappers_["upper_right_transposed"] = &InputMatrixInterpretor::upperRightTransposed;
	mappers_["lower_right_transposed"] = &InputMatrixInterpretor::lowerRightTransposed;
}

InputMatrixInterpretor::~InputMatrixInterpretor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void InputMatrixInterpretor::print(ostream& out)  const
{
	out << "InputMatrixInterpretor[";
	out << "]";
}


InputMatrixRegularInterpretor::InputMatrixRegularInterpretor() 
{    
}

InputMatrixRegularInterpretor::~InputMatrixRegularInterpretor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void InputMatrixRegularInterpretor::print(ostream& out)  const
{
	out << "InputMatrixRegularInterpretor[";
	out << "]";
}

Matrix* InputMatrixRegularInterpretor::geoInterpret(Matrix* in, const InputMatrix& info)
{
	std::map<string, Mapper>::iterator mapper = mappers_.find(lowerCase(info.mapping_));



	
	if ( mapper == mappers_.end() )
		MagLog::warning() << "unknow input matrix mapping " << info.mapping_ << endl;
	else 
		(this->*mapper->second)();




	if ( in->columnsAxis().empty() == false )
		// The initialisation has already been done return;
		return in;

	//Apply scaling !
	double scaling;
	double offset;
	GribDecoder::scale(info.metadata_, scaling, offset);
	for ( int i  = 0; i < in->size(); ++i)
		(*in)[i] = ((*in)[i] * scaling) + offset;


	int nblon =  in->columns();
	double lon = longitude_;
	
	for (int i = 0; i < nblon; i++) {		
		lon = longitude_ +( i * longitude_step_);
		in->columnsAxis().push_back(lon);
		
		
	}

	int nblat =  in->rows();
	double lat = latitude_;
	cout << nblat << endl;
	for (int i = 0; i < nblat; i++) {	
		lat = latitude_ + (i*latitude_step_);	
		in->rowsAxis().push_back(lat);	
		
	}
	
	in->setMapsAxis();
	in->missing(std::numeric_limits<double>::max());
    return in;
}

Matrix* InputMatrixRegularInterpretor::xyInterpret(Matrix* in, const InputMatrix& info)
{
	in->missing(std::numeric_limits<double>::max());
	if ( !in->rowsAxis().empty() )
		// WE have already initialised the matrix ..
		return in;
	if ( x_coords_.size() == in->columns() && y_coords_.size() == in->rows()) {
		in->setRowsAxis(y_coords_);
		in->setColumnsAxis(x_coords_);

		return in;
		// the user has defined the vector of coordinates
		// We use them
	}
	// if y a date or an normal axis..

	if ( !y_first_date_.empty() ) {
		DateTime ref(dateY_);
		DateTime from(y_first_date_);
		DateTime to(y_last_date_);
		y_first_ = from-ref;
		y_last_ = to-ref;
	}
	if ( !x_first_date_.empty() ) {
			DateTime ref(dateX_);
			DateTime from(x_first_date_);
			DateTime to(x_last_date_);
			x_first_ = from-ref;
			x_last_ = to-ref;
	}
	double y = y_first_;
	double step = (y_last_ - y_first_)/(in->rows()-1);
	for (int i = 0; i < in->rows(); i++) {
			
			in->rowsAxis().push_back(y);
			y += step;
	}

	double x = x_first_;
	step = (x_last_ - x_first_)/(in->columns()-1);
	for (int i = 0; i < in->columns(); i++) {
			
			in->columnsAxis().push_back(x);
			x += step;
	}
	in->missing(std::numeric_limits<double>::max());
	in->setMapsAxis();
    return in;
}


InputMatrixIrregularInterpretor::InputMatrixIrregularInterpretor() 
{    
}

InputMatrixIrregularInterpretor::~InputMatrixIrregularInterpretor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void InputMatrixIrregularInterpretor::print(ostream& out)  const
{
	out << "InputMatrixIrregularInterpretor[";
	out << "]";
}

Matrix* InputMatrixIrregularInterpretor::geoInterpret(Matrix* in, const InputMatrix& info)
{

    ProjectedMatrix* matrix = new ProjectedMatrix(in->rows(), in->columns());
    vector<double>& values = matrix->values();
    vector<double>& rows = matrix->rowsArray();
    vector<double>& columns = matrix->columnsArray();

    double scaling;
    double offset;
    GribDecoder::scale(info.metadata_, scaling, offset);
    for ( int i  = 0; i < in->size(); ++i)
    		(*in)[i] = ((*in)[i] * scaling) + offset;
	for (vector<double>::iterator val = in->begin(); val != in->end(); ++val) {
            values.push_back( ((*val) * scaling ) + offset);
	}
           
            
	for (vector<double>::iterator y = latitudes_.begin(); y != latitudes_.end(); ++y) {
			//MagLog::dev() << *y << endl;
			rows.push_back(*y);
	}

	for (vector<double>::iterator x = longitudes_.begin(); x != longitudes_.end(); ++x) {
			//MagLog::dev() << *x << endl;
			columns.push_back(*x);					
	}
    
	matrix->missing(std::numeric_limits<double>::max());
	matrix->getReady();
    return matrix;
}

Matrix* InputMatrixIrregularInterpretor::xyInterpret(Matrix* in, const InputMatrix& info)
{
	
    // here we have to create a projected matrix! 
    
    ProjectedMatrix* matrix = new ProjectedMatrix(in->rows(), in->columns());
   

	vector<double>& values = matrix->values();
	vector<double>& rows = matrix->rowsArray();
	vector<double>& columns = matrix->columnsArray();
	
    for (int row = 0; row < in->rows(); row++)
          for (int column = 0; column < in->columns(); column++)
            values.push_back((*in)(row, column));

	for (vector<double>::iterator y = y_.begin(); y != y_.end(); ++y) {
		//MagLog::dev() << *y << endl;
		rows.push_back(*y);
	}

	for (vector<double>::iterator x = x_.begin(); x != x_.end(); ++x) {
			//MagLog::dev() << *x << endl;
			columns.push_back(*x);					
	}
	matrix->missing(std::numeric_limits<double>::max());
	matrix->getReady();
    
    return matrix;
}


void InputMatrixInterpretor::upperLeft()
{
}

void InputMatrixInterpretor::lowerLeft()
{
}

void InputMatrixInterpretor::upperRight()
{
}

void InputMatrixInterpretor::lowerRight()   
{
}

  
void InputMatrixInterpretor::upperLeftTransposed()
{
}

void InputMatrixInterpretor::lowerLeftTransposed()
{
}

void InputMatrixInterpretor::upperRightTransposed()
{
}

void InputMatrixInterpretor::lowerRightTransposed()
{
}

void InputMatrixRegularInterpretor::getReady(const Transformation& transformation)
{
    if ( transformation.xAxisType() != "date" )
        x_first_date_.clear();
    else
    	dateX_ = transformation.getReferenceX();

    if ( transformation.yAxisType() != "date" )
        y_first_date_.clear();
    else
        dateY_ = transformation.getReferenceY();
}

