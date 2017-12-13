/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Matrix.h
    \brief Implementation of the Template class Matrix.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Feb-2004
    
    Changes:
    
*/

#include <algorithm>

#include "Matrix.h"
#include "Timer.h"
//#include "SegmentJoiner.h"
#include "MatrixHandler.h"


using namespace magics;


void Matrix::multiply(double factor) 
{
    if (factor == 1 ) return;
    std::transform(begin(), end(), begin(), Multiply(factor, missing_));
}
    
void Matrix::plus(double offset) 
{
	
    if (offset == 0 ) return;
    std::transform(begin(), end(), begin(), Plus(offset, missing_));
    
}

pair<int, bool> InfoIndex::index(double pos) const
{
	if ( pos < min_ ) return std::make_pair(-1, false);
	
	if ( pos > max_ ) {
		if ( pos > min_ + 360 )
			return std::make_pair(-1, false);
		else 
			return std::make_pair(nbPoints_-1, false);
	}
	return std::make_pair( floor( (pos - first_ )/step_), fmod( pos - first_, step_) == 0);
}

void ProjectedMatrix::build()
{
	
}

void ProjectedMatrix::getReady()
{
	ASSERT(!values_.empty());
	ASSERT(!rowsArray_.empty());
	ASSERT(!columnsArray_.empty());
	
	// find the bounding box! 
	//columns_ = 100;
	//rows_ = 100;
	
	minx_ = *std::min_element(columnsArray_.begin(), columnsArray_.end());
	maxx_ = *std::max_element(columnsArray_.begin(), columnsArray_.end());
	miny_ = *std::min_element(rowsArray_.begin(), rowsArray_.end());
	maxy_ = *std::max_element(rowsArray_.begin(), rowsArray_.end());
	
	stepx_ = (maxx_ - minx_)/(columns_-1);
    stepy_ = (maxy_ - miny_)/(rows_-1);
	double x = minx_;
	double y = miny_;
	// Create the Axis for Regular Matrix..
	for ( int i = 0; i < columns_; i++) {
		columnsAxis_.push_back(x); 
		MagLog::debug() << "x-> " << x << endl;
		x += stepx_;

	}
	for ( int j = 0; j < rows_; j++) {
			rowsAxis_.push_back(y); 
			MagLog::debug() << "y-> " << y << endl;
			y +=  stepy_;
	}
	setMapsAxis();
	
	// Now preapre the matrix! 
	build();
}


ProjectedMatrix::ProjectedMatrix(int rows, int columns): Matrix(rows, columns)
{
	       origColumns_ = columns;
	       origRows_ = rows;

	       rowsArray_.reserve(rows*columns);  
	       columnsArray_.reserve(rows*columns);
}




double Matrix::min() const {
  	 if ( min_ < DBL_MAX) 
  		 return min_;
  	
  	 for ( const_iterator val = begin(); val != end(); ++val) {
  		 if ( *val == missing_ ) continue; 
  		 if ( *val < min_ ) min_ = *val;
  		 if ( *val > max_ ) max_ = *val;
  	 }
  	 return min_;
 }


double Matrix::max() const
{
  	 if ( max_ > -DBL_MAX) 
  		 return max_; 
  	 
  	  for ( const_iterator val = begin(); val != end(); ++val) {
  	  		 if ( *val == missing_ ) continue; 
  	  		 if ( *val < min_ ) min_ = *val;
  	  		 if ( *val > max_ ) max_ = *val;
  	  }
  	 return max_;
}
   
double Matrix::interpolate(double i, double j) const    
{  
	   double xleft = std::min( left(), right());
	   double xright = std::max( left(), right());
	   double ybottom = std::min( bottom(), top());
	   double ytop = std::max( bottom(), top());
	   
	   if ( columns() == 0 || j < xleft || j > xright ) 
		   return missing_;
	   if ( i < ybottom || i > ytop ) 
   			return missing_;
		int ii = rowIndex(i);
	    	if (ii == -1) {
	    		// interpolate between 2 rows.
	    		double v1, v2;
	    		int i1, i2;
	    		boundRow(i, v1, i1, v2, i2);
	    		
	    		if (i1 == -1) return missing(); 
	    		
	    	    double a = (*this).interpolate(v1, j);
	            double b = (*this).interpolate(v2, j);
	          
	            if ( same(a, missing()) || same(b, missing()) ) return missing();
	            
	            double da = (v2-i)/(v2-v1);
	            double db = (i-v1)/(v2-v1);
	            double val = (a*da) + (b*db);
	            return val;
	        }
	        int jj = columnIndex(j);
	        if (jj == -1) {
	        	double v1, v2;
	    		int i1, i2;
	    		boundColumn(j, v1, i1, v2, i2);
	    		if (i1 == -1) return missing(); 
	    		
	        	
	    		double a = (*this)(ii, i1);
	            double b = (*this)(ii, i2);
	            
	            if ( same(a, missing()) || same(b, missing()) ) return missing();
	            
	            double da = (v2-j)/(v2-v1);
	            double db = (j-v1)/(v2-v1);
	            double val = (a*da) + (b*db);
	            return val;         
	   }
	   return (*this)(ii, jj);
}


/// @brief The usual PI/180 constant
//static const double DEG_TO_RAD = 0.017453292519943295769236907684886;
/// @brief Earth's quatratic mean radius for WGS-84
static const double EARTH_RADIUS_IN_METERS = 6372797.560856;

/** @brief Computes the arc, in radian, between two WGS-84 positions.
 *
 * The result is equal to <code>Distance(from,to)/EARTH_RADIUS_IN_METERS</code>
 *    <code>= 2*asin(sqrt(h(d/EARTH_RADIUS_IN_METERS )))</code>
 *
 * where:<ul>
 *    <li>d is the distance in meters between 'from' and 'to' positions.</li>
 *    <li>h is the haversine function: <code>h(x)=sin²(x/2)</code></li>
 * </ul>
 *
 * The haversine formula gives:
 *    <code>h(d/R) = h(from.lat-to.lat)+h(from.lon-to.lon)+cos(from.lat)*cos(to.lat)</code>
 *
 * @sa http://en.wikipedia.org/wiki/Law_of_haversines
 */
double geodistance(double lat1, double lon1, double lat2, double lon2) {
	double latitudeArc  = (lat1 - lat2) * DEG_TO_RAD;
	double longitudeArc = (lon1 - lon2) * DEG_TO_RAD;
	double latitudeH = sin(latitudeArc * 0.5);
	latitudeH *= latitudeH;
	double lontitudeH = sin(longitudeArc * 0.5);
	lontitudeH *= lontitudeH;
	double tmp = cos(lat1*DEG_TO_RAD) * cos(lat2*DEG_TO_RAD);
	return EARTH_RADIUS_IN_METERS* (2.0 * asin(sqrt(latitudeH + tmp*lontitudeH)));
}

#include <limits>
pair<double, double> Matrix::nearest_value(double row, double column,double &rowOut, double &colOut) const
{
	double col, offset;

	col = fmod(column - minX(), 360.) + minX();

	offset = column - col;

	map<double, map<double, pair<double, double> > >::const_iterator  row_index;
	map<double, pair<double, double> >::const_iterator column_index;
	rowOut = missing();
	colOut = missing();


	vector<pair<double, map<double, pair<double, double> >::const_iterator> > points;
	row_index = index_.find(row);

	if ( row_index != index_.end() ) {
		rowOut = row;
		// We have to find the columns
		column_index = row_index->second.find(col);
		if ( column_index != row_index->second.end() ) {
			// Perfect match !
			colOut = col;
			return column_index->second;
		}
		column_index = row_index->second.lower_bound(col);
		if ( column_index == row_index->second.end() || column_index == row_index->second.begin()) {
			rowOut = missing();
			return make_pair(missing(), missing());
		}
		// here we have 2 points : find the nearest
		points.push_back(make_pair(row, column_index));
		column_index--;
		points.push_back(make_pair(row, column_index));
	}
	else {
		row_index = index_.lower_bound(row);
		if ( row_index == index_.end() || row_index == index_.begin()) {
			rowOut = missing();
			return make_pair(missing(), missing());
		}
		// Here we may have 4 points!
		// Deal with the first row
		column_index = row_index->second.lower_bound(col);
		if ( column_index != row_index->second.end() || column_index != row_index->second.begin()) {
			points.push_back(make_pair(row, column_index));
			column_index--;
			points.push_back(make_pair(row, column_index));
		}
		row_index--;
		column_index = row_index->second.lower_bound(col);
		if ( column_index != row_index->second.end() || column_index != row_index->second.begin()) {
			points.push_back(make_pair(row, column_index));
			column_index--;
			points.push_back(make_pair(row, column_index));
		}

	}
	// Now we find the nearest!
	double min = numeric_limits<double>::infinity();
	pair<double, double> value = make_pair(missing(), missing());
	for (vector<pair<double, map<double, pair<double, double> >::const_iterator> >::iterator point = points.begin(); point != points.end(); ++point) {
		double  dist = geodistance(point->first, point->second->first, row, col);
		if (dist < min ) {
			min = dist;
			rowOut = point->first;
			colOut = point->second->first + offset;
			value =  point->second->second;
		}
	}

	return value;

}





int Matrix::nearest_index(double row, double column,double &rowOut, double &colOut) const
{
	double col, offset;


	
	int factor = (column-minX())/360.;

	if ( column-minX() < 0)
		factor--;

	offset = (factor*360);

	col = column - offset;
	rowOut = missing();
	colOut = missing();

	if ( col < left() || col > right() ) {
		return -1;
	} 
	if ( row < bottom() || row > top() ) {
		return -1;
	} 
	map<double, int >::const_iterator  row_index;
	pair<int, bool> column_index;
	vector<pair<double, pair<double, int> > > points;

	


	row_index = yIndex_.find(row);


	if ( row_index != yIndex_.end() ) {
		rowOut = row;
		column_index = xIndex_[row_index->second].index(col);
		if ( column_index.first == -1  ) {
			return -1;
		}
		if ( column_index.second ) {
					// Perfect match !
			colOut = column;
			int value = xIndex_[row_index->second].position(column_index.first);
			return ( data_[value] == missing() ) ? -1 : value;
		}
		else {
					// here we have 2 points : find the nearest
			points.push_back(make_pair(row, make_pair(xIndex_[row_index->second].value(column_index.first), xIndex_[row_index->second].position(column_index.first))));
			points.push_back(make_pair(row,  make_pair(xIndex_[row_index->second].value(column_index.first+1), xIndex_[row_index->second].position(column_index.first+1))));
		}
	}

	row_index = yIndex_.lower_bound(row);
	if ( row_index == yIndex_.end() || row_index == yIndex_.begin()) {
		rowOut = missing();
		return -1;
	}
			// Here we may have 4 points!
			// Deal with the first row

	InfoIndex xindex = xIndex_[row_index->second];
	column_index = xindex.index(col);
	if ( column_index.second )
		points.push_back(make_pair(row_index->first, make_pair(xindex.value(column_index.first), xindex.position(column_index.first))));
	else {
		points.push_back(make_pair(row_index->first, make_pair(xindex.value(column_index.first), xindex.position(column_index.first))));
		points.push_back(make_pair(row_index->first, make_pair(xindex.value(column_index.first+1), xindex.position(column_index.first+1))));
	}

	row_index--;
	xindex = xIndex_[row_index->second];
	column_index = xindex.index(col);

		if ( column_index.second )
			points.push_back(make_pair(row_index->first, make_pair(xindex.value(column_index.first), xindex.position(column_index.first))));
		else {
			points.push_back(make_pair(row_index->first, make_pair(xindex.value(column_index.first), xindex.position(column_index.first))));
			points.push_back(make_pair(row_index->first, make_pair(xindex.value(column_index.first+1), xindex.position(column_index.first+1))));
		}





	// Now we find the nearest!
	double min = numeric_limits<double>::infinity();
	int value = -1;
	for (vector<pair<double, pair<double, int> > >::const_iterator point = points.begin(); point != points.end(); ++point) {
		double  dist = geodistance(point->first, point->second.first, row, col);
		if (dist < min ) {
			min = dist;
			rowOut = point->first;
			colOut = point->second.first + offset;
			value =  point->second.second;

		}
	}
	if ( value == -1 ) return value;
	return ( data_[value] == missing() ) ? -1 : value;

}

double Matrix::nearest(double row, double col,double &rowOut, double &colOut) const
{  
	double xleft = std::min( left(), right());
	double xright = std::max( left(), right());
	double ybottom = std::min( bottom(), top());
	double ytop = std::max( bottom(), top());
	 
	
	if ( columns() == 0 || col < xleft || col > xright ) 
		return missing_;
	if ( columns() == 0 ||  row < ybottom || row > ytop ) 
  		return missing_;
	
	double row1, row2, col1, col2;
	int rowIdx1, rowIdx2, colIdx1, colIdx2;
	
	boundRow(row, row1, rowIdx1, row2, rowIdx2);   
	if(rowIdx1 == -1 || rowIdx2 == -1)
		return missing_;
	
	boundColumn(col, col1, colIdx1, col2, colIdx2);   
	if(colIdx1 == -1 || colIdx2 == -1)
		return missing_;
	  
	int rowIdx, colIdx;
	if(fabs(row1-row) < fabs(row2-row))
	{
		rowOut= row1;
		rowIdx=rowIdx1;
	}
	else
	{
	  	rowOut= row2;
		rowIdx=rowIdx2;
	}
	
	if(fabs(col1-col) < fabs(col2-col))
	{
		colOut= col1;
		colIdx=colIdx1;
	}
	else
	{
	  	colOut= col2;
		colIdx=colIdx2;
	}
		
	return (*this)(rowIdx, colIdx);
	
}	


double Matrix::operator()(int row, int column) const
  {
         // Here for perfrormance we are trusting the user we do not catch MagException

				double val = (*this)[ row * columns_ + column];
				return val;

  }
#include "MatrixHandler.h"
GeoBoxMatrixHandler::GeoBoxMatrixHandler(const AbstractMatrix& matrix, const Transformation& transformation):
	  TransformMatrixHandler(matrix), transformation_(transformation), original_(0)
{
	map<double, int> lats;
	map<double, int> lons;
	
	double lon, lat;
	double minlon, minlat, maxlon, maxlat;
	
	transformation.boundingBox(minlon, minlat, maxlon, maxlat);

	

	int rows = matrix_.rows();
	int columns = matrix_.columns();
	double step = matrix_.XResolution();

	bool global =  ( matrix_.regular_column(columns-1) - matrix_.regular_column(0) ) > ( 360. - 2 *matrix_.XResolution() );
	global = true;

	if (!global) {

		lon = matrix_.column(0, 0) - step;
		if ( minlon <= lon && lon <= maxlon)
				lons[lon] = -1;

		double ml = lon - 360;
		while ( ml >= minlon && ml <= maxlon ) {  lons[ml] = -1; ml -= 360; }
		ml = lon + 360;
		while (  ml >= minlon && ml <= maxlon ) {  lons[ml] = -1; ml += 360; }

		lon = matrix_.column(0, columns-1) + step;
		if ( minlon <= lon && lon <= maxlon)
				lons[lon] = -1;

		ml = lon - 360;
		while ( ml >= minlon && ml <= maxlon ) {  lons[ml] = -1; ml -= 360; }
		ml = lon + 360;
		while (  ml >= minlon && ml <= maxlon ) {  lons[ml] = -1; ml += 360; }
	}

	for (int i = 0; i < columns; i++)
	{
		lon = matrix_.regular_column(i);
		if ( minlon <= lon && lon <= maxlon) 
			lons[lon] = i;

		double ml = lon - 360;
		while ( ml >= minlon && ml <= maxlon ) {  lons[ml] = i; ml -= 360; }
		
		ml = lon + 360;

		while (  ml >= minlon && ml <= maxlon ) {  lons[ml] = i; ml += 360; }
	}


	int i = 0;
	for (map<double, int>::const_iterator entry = lons.begin(); entry != lons.end(); ++entry)
	{
		
		columns_[i] = entry->second;
		regular_longitudes_.push_back(entry->first);
		columnsMap_[entry->first] = i;
		i++;
	}




	for (int i = 0; i < rows; i++) {
		lat = matrix_.regular_row(i);
		if ( minlat < lat && lat < maxlat) {
			lats[lat] = i;
			//cout << "add lat -> " << lat << endl;
		}
		if ( same(minlat, lat) || same(lat,maxlat) ) { 
			lats[lat] = i;
			//cout << "PASS nEW TEST " << lat << " - " <<  minlat << " - " << maxlat << endl;
			//cout << "add lat -> " << lat << endl;
		}


	}

	i = 0;
	for (map<double, int>::const_iterator entry = lats.begin(); entry != lats.end(); ++entry) {
		rows_[i] = entry->second;
		regular_latitudes_.push_back(entry->first);
		rowsMap_[entry->first] = i;
		i++;
	}

	
}


MatrixHandler* Matrix::getReady(const Transformation& transformation)  const{
	return transformation.prepareData(*this);
}

MatrixHandler* Proj4Matrix::getReady(const Transformation&) const
{
	return new Proj4MatrixHandler(*this, proj4_);
}

MatrixHandler* RotatedMatrix::getReady(const Transformation&) const
{
	return new RotatedMatrixHandler(*this, southPoleLat_, southPoleLon_);
}



Proj4MatrixHandler::Proj4MatrixHandler(const AbstractMatrix& matrix, const string& proj4) : MatrixHandler(matrix) 
{
	proj4_ = pj_init_plus(proj4.c_str());
	latlon_ = pj_init_plus("+proj=longlat +ellps=WGS84 +datum=WGS84");
	internal_ = false;
}

RotatedMatrixHandler::RotatedMatrixHandler(const AbstractMatrix& matrix,  double lat, double lon) : 
	MatrixHandler(matrix), southPoleLat_(lat), southPoleLon_(lon) 
{
	internal_ = false;
}



       
double Proj4MatrixHandler::interpolate(double  row, double  column) const 
{
	if (internal_)
		return MatrixHandler::interpolate(row, column);
	double x = column * DEG_TO_RAD;
	double y = row * DEG_TO_RAD;

    int error = pj_transform(latlon_, proj4_, 1, 1, &x, &y, NULL);
    if ( error )
    	return missing();

    return MatrixHandler::interpolate(y, x);
}

double Proj4MatrixHandler::nearest(double  row, double  column) const
{
	if (internal_)
		return MatrixHandler::nearest(row, column);
	double x = column * DEG_TO_RAD;
	double y = row * DEG_TO_RAD;

    int error = pj_transform(latlon_, proj4_, 1, 1, &x, &y, NULL);
    if ( error )
    	return missing();

    return MatrixHandler::nearest(y, x);
}

double RotatedMatrixHandler::interpolate(double  row, double  column) const 
{
	if (internal_)
		return MatrixHandler::interpolate(row, column);
	
	pair<double, double> point = rotate(row, column);
    return MatrixHandler::interpolate(point.first, point.second);
}

double RotatedMatrixHandler::nearest(double  row, double  column) const
{
	if (internal_)
		return MatrixHandler::nearest(row, column);
	pair<double, double> point = rotate(row, column);
    return MatrixHandler::nearest(point.first, point.second);
}

RotatedMatrix::RotatedMatrix(int rows, int columns, double lat, double lon) : 
Matrix(rows, columns), southPoleLat_(lat), southPoleLon_(lon) 
{
    helper_ = new RotatedMatrixHandler(*this, southPoleLat_, southPoleLon_);
}
int RotatedMatrix::nearest_index(double lat, double lon,double &nlat, double &nlon) const
{
	
	pair<double, double> point = helper_->rotate(lat, lon);
	int index = Matrix::nearest_index(point.first, point.second, nlat, nlon);
	if ( index != -1 ) {
		point = helper_->unrotate(nlat, nlon);
		nlat = point.first;
		nlon = point.second;
	}
	return index;
}

double RotatedMatrixHandler::column(int i , int j) const
{
	double column = MatrixHandler::column(i, j);
	double row = MatrixHandler::row(i, j);

	pair<double, double> point = unrotate(row, column);
    return point.second;
}

double RotatedMatrixHandler::row(int i, int j) const
{
	double column = MatrixHandler::column(i, j);
	double row = MatrixHandler::row(i, j);

	pair<double, double> point = unrotate(row, column);
    return point.first;
   
}

double Proj4MatrixHandler::column(int i , int j) const
{
	double column = MatrixHandler::column(i, j);
	double row = MatrixHandler::row(i, j);

	int error = pj_transform(proj4_, latlon_, 1, 1, &column, &row, NULL);
    if ( error )
    	return missing();
    return column * RAD_TO_DEG;
	

}

double Proj4MatrixHandler::row(int i, int j) const
{
	double column = MatrixHandler::column(i, j);
	double row = MatrixHandler::row(i, j);

	int error = pj_transform(proj4_, latlon_, 1, 1, &column, &row, NULL);
    if ( error )
    	return missing();
    return row * RAD_TO_DEG;
}
    
pair<double, double> RotatedMatrixHandler::rotate(double lat_y,  double lon_x) const 
{
    const double cToRadians = M_PI / 180.0;
    double ZRADI = 1. / cToRadians;
    double ZSYCEN = sin(cToRadians * (southPoleLat_ + 90.));
    double ZCYCEN = cos(cToRadians * (southPoleLat_ + 90.));

    double ZXMXC = cToRadians * (lon_x - southPoleLon_);
    double ZSXMXC = sin(ZXMXC);
    double ZCXMXC = cos(ZXMXC);
    double ZSYREG = sin(cToRadians * lat_y);
    double ZCYREG = cos(cToRadians * lat_y);
    double ZSYROT = ZCYCEN * ZSYREG - ZSYCEN * ZCYREG * ZCXMXC;
    ZSYROT = MAX(MIN(ZSYROT, +1.0), -1.0);

    double PYROT = asin(ZSYROT) * ZRADI;

    double ZCYROT = cos(PYROT * cToRadians);
    double ZCXROT = (ZCYCEN * ZCYREG * ZCXMXC + ZSYCEN * ZSYREG) / ZCYROT;
    ZCXROT = MAX(MIN(ZCXROT, +1.0), -1.0);
    double ZSXROT = ZCYREG * ZSXMXC / ZCYROT;

    double PXROT = acos(ZCXROT) * ZRADI;

    if (ZSXROT < 0.0)
        PXROT = -PXROT;

    return std::make_pair(PYROT, PXROT);
}

pair<double, double> RotatedMatrixHandler::unrotate(double lat_y, double lon_x) const 
{
    const double cToRadians = M_PI / 180.0;
    double ZRADI = 1. / cToRadians;
    double ZSYCEN = sin(cToRadians * (southPoleLat_ + 90.));
    double ZCYCEN = cos(cToRadians * (southPoleLat_ + 90.));
    double ZSXROT = sin(cToRadians * lon_x);
    double ZCXROT = cos(cToRadians * lon_x);
    double ZSYROT = sin(cToRadians * lat_y);
    double ZCYROT = cos(cToRadians * lat_y);
    double ZSYREG = ZCYCEN * ZSYROT + ZSYCEN * ZCYROT * ZCXROT;
    ZSYREG = MAX(MIN(ZSYREG, +1.0), -1.0);
    double PYREG = asin(ZSYREG) * ZRADI;
    double ZCYREG = cos(PYREG * cToRadians);
    double ZCXMXC = (ZCYCEN * ZCYROT * ZCXROT - ZSYCEN * ZSYROT) / ZCYREG;
    ZCXMXC = MAX(MIN(ZCXMXC, +1.0), -1.0);
    double ZSXMXC = ZCYROT * ZSXROT / ZCYREG;
    double ZXMXC = acos(ZCXMXC) * ZRADI;
    if (ZSXMXC < 0.0)
        ZXMXC = -ZXMXC;
    double PXREG = ZXMXC + southPoleLon_;
    return std::make_pair(PYREG, PXREG);
}

   
