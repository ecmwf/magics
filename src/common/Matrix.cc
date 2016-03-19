

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

void ProjectedMatrix::build()
{
	/*
	vector< std::pair<int, int>  > coords(4);
	for (int r = 0; r < rows_; r++)
		for (int c = 0; c < columns_; c++)
			push_back(missing_);
	
	coords[0] = std::make_pair(0,0);
	coords[1] = std::make_pair(0,1);
	coords[2] = std::make_pair(1,1);
	coords[3] = std::make_pair(1,0);
	
	Timer  t("GridHelper::build", "GridHelper");
	for ( int row = 0 ; row < origRows_-1; row++)
		for ( int column = 0 ; column < origColumns_-1; column++)
		{
			double xmin = origColumns_;
			double xmax = 0;
			double ymin = rows_;
			double ymax = 0;
			
			vector<double> x, y, v;
			vector<double> xs, ys, vs;
			vector<Point> points;
			bool stop = true;
			
			for (vector< std::pair<int, int>  >::iterator p = coords.begin(); p != coords.end(); ++p) {
				int r = row+ p->first;
				int c = column+ p->second;
				double rr = rowsArray_[index(r, c)];
				double cc = columnsArray_[index(r, c)];
				MagLog::debug() << rr << " ? " << miny_ -  stepy_ << " " <<  maxy_ + stepy_ << endl;
				MagLog::debug() << cc << " ? " << minx_ -  stepx_ << " " <<  maxx_ + stepx_ << endl;
				if ( rr >= miny_ -  stepy_ && rr <= maxy_ + stepy_)
					stop = false;

				if ( cc >= minx_ -  stepx_ && cc <= maxx_ + stepx_ )
					stop = false;
				
			}
			if ( stop )
				// Go to next cell...

				continue;
			for (vector< std::pair<int, int>  >::iterator p = coords.begin(); p != coords.end(); ++p) {
				
				int r = row+ p->first;
				int c = column+ p->second;
				double  rr = rowsArray_[index(r, c)];
				double cc = columnsArray_[index(r, c)];
				
				

				 x.push_back( (cc - minx_ ) / stepx_ );
				 y.push_back( (rr - miny_ ) / stepy_);

				 points.push_back(Point(x.back(), y.back()));
				 v.push_back( values_[index(r, c)]);  						
			}
			points.push_back(points.front());
			xmin = *std::min_element(x.begin(), x.end());
			xmax = *std::max_element(x.begin(), x.end());
			ymin = *std::min_element(y.begin(), y.end());
			ymax = *std::max_element(y.begin(), y.end());

			MagLog::debug() << "x------->" << xmin << " " << xmax << endl;
			MagLog::debug() << "y------->" << ymin << " " << ymax << endl;
			int ii = (xmin < 0) ? 0 : xmin;

			while (ii <= xmax && ii < columns_ )
			{
				int jj = (ymin < 0) ? 0 : ymin;
				while ( jj <= ymax && jj < rows_ ) {
			
					MagLog::debug() << "ii=" << 	ii << "  jj=" << jj <<  endl;
					// is the point inside or oiutside .. if inside use it! 

				    int i = 0;
				    bool done = false;
					for (vector<Point>::iterator pt = points.begin(); pt != points.end(); ++pt) {
				    	MagLog::debug() << *pt << endl;
						if ( *pt == Point(ii, jj) ) {
							(*this)[(jj*columns_) + ii] = v[i];
							done = true;
				    		break;
				    	}
						i++;
				    }
					if (  !done && SegmentJoiner::pointInPoly(Point(ii, jj),  points) )
					 {
						//feed the matrix...
						vector<double> w;
						double total = 0.;
						for (unsigned int i = 0; i < x.size(); i++) {
							double val = sqrt( ((ii - x[i])*(ii-x[i]))  + ( (jj-y[i])*(jj-y[i]) ) );

														w.push_back(1/ (val) ? (val*val) : 1);
														if (v[i] != missing_)
														    total += w[i];
						}
						
						double val = 0.;
						for (unsigned int i = 0; i < w.size(); i++) {
							if (v[i] != missing_) 
								val += (v[i]*w[i]/total);
							}

				     (*this)[(jj*columns_) + ii] = val;
				     MagLog::debug() << "[" << ii << ", " << jj << "]=" << val << endl;
				     MagLog::debug() << "[" << ii <<  "]=" << columnsAxis_[ii] << endl;
				     MagLog::debug() << "[" << jj <<  "]=" << rowsAxis_[jj] << endl;
					}else {
						MagLog::debug() <<  "out[" << ii <<  "]=" << columnsAxis_[ii] << endl;
				     MagLog::debug() << "out[" << jj <<  "]=" << rowsAxis_[jj] << endl;
					}

					jj++;
				}
				ii++;
			}
		}
		*/
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
RotatedMatrix::RotatedMatrix(int rows, int columns): Matrix(rows, columns)
{
	rowsArray_.reserve(rows*columns);
	columnsArray_.reserve(rows*columns);
	values_.reserve(rows*columns);
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
static const double DEG_TO_RAD = 0.017453292519943295769236907684886;
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


void xx(double v)
{
	double offset;
	offset = fmod(v, 360.);
	int a = v/360;
	cout << "-------" << endl;
	cout << v << "--->" << offset  << "--->" << a <<  endl;

	if ( offset < 0 )
		a--;

	double nv = v - (a*360);

	cout << nv << "--->" << (a*360) << " " <<  nv + (a*360) << endl;



}


int Matrix::nearest_index(double row, double column,double &rowOut, double &colOut) const
{
	double col, offset;

	int factor = (column-minX())/360.;

	if ( column-minX() < 0)
		factor--;

	offset = (factor*360);

	col = column - offset;
	map<double, int >::const_iterator  row_index;
	pair<int, bool> column_index;
	vector<pair<double, pair<double, int> > > points;

	rowOut = missing();
	colOut = missing();


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
			int value = xIndex_[row_index->first].position(column_index.first);
			return ( data_[value] == missing() ) ? -1 : value;
		}
		else {
					// here we have 2 points : find the nearest
			points.push_back(make_pair(row, make_pair(xIndex_[row_index->first].value(column_index.first), xIndex_[row_index->first].position(column_index.first))));
			points.push_back(make_pair(row,  make_pair(xIndex_[row_index->first].value(column_index.first+1), xIndex_[row_index->first].position(column_index.first+1))));
		}
	}

	row_index = yIndex_.lower_bound(row);
	if ( row_index == yIndex_.end() || row_index == yIndex_.begin()) {
		rowOut = missing();
		return -1;
	}
			// Here we may have 4 points!
			// Deal with the first row
	column_index = xIndex_[row_index->second].index(col);
	if ( column_index.second )
		points.push_back(make_pair(row_index->first, make_pair(xIndex_[row_index->first].value(column_index.first), xIndex_[row_index->first].position(column_index.first))));
	else {
		points.push_back(make_pair(row_index->first, make_pair(xIndex_[row_index->second].value(column_index.first), xIndex_[row_index->first].position(column_index.first))));
		points.push_back(make_pair(row_index->first, make_pair(xIndex_[row_index->second].value(column_index.first+1), xIndex_[row_index->first+1].position(column_index.first))));
	}

	row_index--;
	column_index = xIndex_[row_index->second].index(col);
		if ( column_index.second )
			points.push_back(make_pair(row_index->first, make_pair(xIndex_[row_index->first].value(column_index.first), xIndex_[row_index->first].position(column_index.first))));
		else {
			points.push_back(make_pair(row_index->first, make_pair(xIndex_[row_index->second].value(column_index.first), xIndex_[row_index->first].position(column_index.first))));
			points.push_back(make_pair(row_index->first, make_pair(xIndex_[row_index->second].value(column_index.first+1), xIndex_[row_index->first+1].position(column_index.first))));
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
		if ( minlat <= lat && lat <= maxlat) lats[lat] = i;

	}
	i = 0;
	for (map<double, int>::const_iterator entry = lats.begin(); entry != lats.end(); ++entry) {
		rows_[i] = entry->second;
		regular_latitudes_.push_back(entry->first);
		rowsMap_[entry->first] = i;
		i++;
	}


}
MatrixHandler* RotatedMatrix::getReady(const Transformation&) const
{
	return new MatrixHandler(*this);
}
MatrixHandler* Matrix::getReady(const Transformation& transformation)  const{
	return transformation.prepareData(*this);
}
