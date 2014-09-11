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

/*! \file NetcdfMatrixInterpretor.h
    \brief Implementation of the Template class NetcdfMatrixInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#include <limits>

#include "NetcdfMatrixInterpretor.h"
#include "Factory.h"
#include "Netcdf.h"
#include "Coordinate.h"
#include "Layer.h"
#include "TextVisitor.h"
#include "Tokenizer.h"
using namespace magics;

NetcdfMatrixInterpretor::NetcdfMatrixInterpretor()
{
}


NetcdfMatrixInterpretor::~NetcdfMatrixInterpretor() 
{
}


bool NetcdfMatrixInterpretor::interpretAsMatrix(Matrix** matrix)
{
	
	MagLog::debug() << "NetcdfMatrixInterpretor::interpret()--->" << *this << "\n";
	if ( *matrix ) return false;

	
	matrix_ = new Matrix();
	
	*matrix = matrix_;
	if ( !matrix_->empty() ) return false;
    
	matrix_->missing(std::numeric_limits<double>::max());

	Netcdf netcdf(path_, dimension_method_);
	double missing =  netcdf.getMissing(field_, missing_attribute_);
	matrix_->missing(missing);

	string title = netcdf.getAttribute("title", "NO TITLE");


    x();
    y();
	// get the data ...
	try {


		map<string, string> first, last;
		setDimensions(dimension_, first, last);
		vector<double> rows = dateRows_.empty() ? rows_ : dateRows_;
		vector<double> columns = dateColumns_.empty() ? columns_ : dateColumns_;

		for ( vector<double>::iterator r = rows.begin(); r != rows.end(); r++) {
			vector<string> dims;
			ostringstream x,y;
			x.precision(20);
			y.precision(20);
			y << y_ << "/" << *r;
			x << x_ << "/" << columns.front() << "/" << columns.back();
			dims.push_back(y.str());
			dims.push_back(x.str());
			setDimensions(dims, first, last);
			vector<double> data;
			netcdf.get(field_, data, first, last);
			for ( vector<double>::iterator d = data.begin(); d != data.end(); d++) {
					matrix_->push_back(*d);
			}

		}
		

		MagLog::debug() << "matrix_[" << matrix_->size() << ", " << scaling_ << ", " << offset_ << "]" << "\n";

		matrix_->multiply(scaling_);
		matrix_->plus(offset_);
        
       
        
		MagLog::debug() << "matrix_[" << matrix_->size() << ", " << scaling_ << ", " << offset_ << "\n";
        
		
		vector<double> col;
	  
		
		
		matrix_->setColumnsAxis(columns_);
		matrix_->setRowsAxis(rows_);
		
    
		MagLog::dev() << *matrix_ << "\n";
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
		 return false;
	}    
	return true;
}



/*!
 Class information are given to the output-stream.
*/		
void NetcdfMatrixInterpretor::print(ostream& out)  const
{
	out << "NetcdfMatrixInterpretor[";
	NetcdfInterpretor::print(out);
	NetcdfMatrixInterpretorAttributes::print(out);
	out << "]";
}




	
bool NetcdfMatrixInterpretor::x()
{
	if ( !columns_.empty() )
			return false;

    Netcdf netcdf(path_, dimension_method_);
    map<string, string> first, last;
    setDimensions(dimension_, first, last);

    try {
        netcdf.get(x_, columns_, first, last);
        baseDateX_ = "";
        if ( !reference_date(netcdf, x_, refDateX_, baseDateX_, columns_, dateColumns_) )
        	cf_date(netcdf, x_, refDateX_, baseDateX_, columns_, dateColumns_);
	} 
    catch (...) {
    	try {
    		int x = netcdf.getDimension(x_);
    		if ( !x ) {
    	    			MagLog::warning() << "No valid X dimension.." << endl;
    	    			return false;
    	    		}
    		for (int i = 0; i < x; i++)
    			columns_.push_back(i);
    	}
        catch (...) {
        	MagLog::warning() << "No valid X dimension.." << endl;
        		return false;
        	}
    }

    if ( aux_x_.empty() )
    	return true;
    try {
    	vector<double> aux;

    	netcdf.get(aux_x_,aux, first, last);
    	if ( !aux.empty() ) {
    		// small check to make sure that we do not have problem :
    		ostringstream geominx, geomaxx;
    		if ( magCompare(geo_x_convention_, "latlon") ) {
    			geominx << aux.front() << "/" << columns_.front();
    			geomaxx << aux.back() << "/" << columns_.back();
    		}
    		else {
    			geominx << columns_.front() << "/" << aux.front();
    			geomaxx << columns_.back() << "/" << aux.back();
    		}
    		geoMinX_ = geominx.str();
    		geoMaxX_ = geomaxx.str();




    	}
    }
    catch (...) {
    	return false;
    }

    return true;

}

bool NetcdfMatrixInterpretor::y()
{

	if ( !rows_.empty() )
		return false;

    Netcdf netcdf(path_, dimension_method_);
    map<string, string> first, last;
    setDimensions(dimension_, first, last);
    try {
    	 netcdf.get(y_, rows_, first, last);

    	 baseDateY_ = "";
    	 if ( !reference_date(netcdf, y_, refDateY_, baseDateY_, rows_, dateRows_) )
    	      cf_date(netcdf, y_, refDateY_, baseDateY_, rows_, dateRows_);
	} 
    catch (...) {
    	try {
    		int y = netcdf.getDimension(y_);
    		if ( !y ) {
    			MagLog::warning() << "No valid Y dimension.." << endl;
    			return false;
    		}

    		for (int i = 0; i < y; i++)
    			rows_.push_back(i);
    	}
    	catch (...) {
    		MagLog::warning() << "No valid Y dimension.." << endl;
    		return false;
    	}
    } 

    if ( aux_y_.empty() )
    	return true;
    try {
    	vector<double> aux;
    	netcdf.get(aux_y_,aux, first, last);
    	if ( !aux.empty() ) {
    		ostringstream geominy, geomaxy;
    		if ( magCompare(geo_y_convention_, "latlon") ) {
    			geominy << rows_.front() << "/" << aux.front();
    			geomaxy << rows_.back() << "/" << aux.back();
    		}
    		else {
    			geominy << aux.front() << "/" << rows_.front();
    			geomaxy << aux.back() << "/" << rows_.back();
    		}

    		geoMinY_ = geominy.str();
    		geoMaxY_ = geomaxy.str();
    	}
    }
    catch (...) {
    	return false;
    }
    return true;
}
void NetcdfMatrixInterpretor::getReady(const Transformation& transformation)
{
	// adjust the data to the transformation..


		refDateX_ = transformation.getReferenceX();
		columns_.clear();
		x();


		refDateY_ = transformation.getReferenceY();
		rows_.clear();
		y();


}

void NetcdfMatrixInterpretor::visit(Transformation& transformation)
{
	// get the data ...
		// by default, respect the internal organisation of the data..
		try {

			refDateX_ = ( transformation.getAutomaticX() ) ? "" : transformation.getReferenceX();
			x();
			refDateY_ = ( transformation.getAutomaticY() ) ? "" : transformation.getReferenceY();
			y();

			if ( transformation.getAutomaticX() ) {
				if ( !baseDateX_.empty() ) {
					transformation.setDataMinMaxX(columns_.front(), columns_.back(), baseDateX_);

				}
				else if ( !geoMinX_.empty() ) {
					string coords = geoMinX_ + "/" + geoMaxX_;
					transformation.setDataMinMaxX(columns_.front(), columns_.back(), coords);
				}
				else
				{
					transformation.setMinMaxX(columns_.front(), columns_.back());

				}
			}
			if ( transformation.getAutomaticY() ) {
				if ( !baseDateY_.empty() ) {
					transformation.setDataMinMaxY(rows_.front(), rows_.back(), baseDateY_);

				}
				else if ( !geoMinY_.empty() ) {
					string coords = geoMinY_ + "/" + geoMaxY_;
					transformation.setDataMinMaxY(rows_.front(), rows_.back(),coords);

				}
				else {
					transformation.setMinMaxY(rows_.front(), rows_.back());

				}
			}

		}
		catch ( ... ) {}
		

}

void NetcdfMatrixInterpretor::statsData(map<string,vector<double> >& stats)
{
	if(matrix_)
	{
	  	for(unsigned int i=0; i < matrix_->size(); i++)
		{  
			if(matrix_->at(i) != matrix_->missing())
			{
			  	stats["value"].push_back(matrix_->at(i));
			}
		}
	}	
}

void NetcdfMatrixInterpretor::visit(MetaDataCollector& mdc)
{
	mdc["_datatype"]="NetCDF_matrix";
	mdc["path"]=path_;
	mdc["x"]=x_;
	mdc["y"]=y_;
	mdc["value"]=field_;	
	mdc["statsType"]="scalar";
	
	Netcdf nc(path_, dimension_method_);
	
	string attrKey;
	string attrVal;		

	//Value attributes
	getAttributes(nc,field_,attrKey,attrVal);		
	if(!attrKey.empty())
	{
		mdc["valueAttrKey"]=attrKey;
		mdc["valueAttrValue"]=attrVal;
	}
}

void NetcdfMatrixInterpretor::visit(ValuesCollector& vcp,PointsList&)
{
	vcp.setCollected(true);
	
	assert(matrix_); 
		
	const Transformation& transformation = vcp.transformation();
  	MatrixHandler* box =  transformation.prepareData(*matrix_);
	for (ValuesCollector::iterator point =  vcp.begin(); point != vcp.end(); ++point) {
		point->push_back(new ValuesCollectorData(point->x(),point->y(),box->nearest(point->y(), point->x()),-1.));
	}

	//for (ValuesCollector::iterator point =  points.begin(); point != points.end(); ++point) {
	//	point->push_back(new ValuesCollectorData(point->x(), point->y(),matrix_->nearest(point->y(), point->x()),-1.));
	//}
}




bool NetcdfMatrixInterpretor::interpretAsPoints(PointsList& points, const Transformation& transformation)
{
	refDateX_ = transformation.getReferenceX();
	refDateY_ = transformation.getReferenceY();

	Matrix* data = 0;

	if ( !interpretAsMatrix(&data) )
		return false;

	vector<double>::iterator d = data->begin();
	for (vector<double>::iterator row = rows_.begin(); row != rows_.end(); ++row)
		for (vector<double>::iterator column = columns_.begin(); column != columns_.end(); ++column) {
			UserPoint* point = new UserPoint(*column, *row, *d);
			++d;
			points.push_back(point);
		}
	return true;

}
