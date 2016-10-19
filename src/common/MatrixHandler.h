/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MatrixHandler.h
    \brief Definition of the Template class MatrixHandler.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Feb-2004 
    
    Changes:
    
*/ 

#ifndef MatrixHandler_H
#define MatrixHandler_H
 
#include "magics.h"
#include "Matrix.h"
#include "BasePointsHandler.h"
#include "Transformation.h"
#include "VectorOfPointers.h"

#include "Timer.h"

#include "Transformation.h"

namespace magics {

class MatrixHandler : public AbstractMatrix, public AbstractPoints
{
public : 
    MatrixHandler(const AbstractMatrix& matrix) : AbstractMatrix(),
        AbstractPoints(),
        matrix_(matrix), min_(INT_MAX), max_(-INT_MAX)  {}
    MatrixHandler(const MatrixHandler& matrix) :
        AbstractMatrix(),
        AbstractPoints(),
        matrix_(matrix) , min_(INT_MAX), max_(-INT_MAX) {}
    virtual ~MatrixHandler() {}
   
    virtual double operator()(int  i, int  j) const { return matrix_(i, j); } 
    
    virtual int rowIndex(double r) const     { return matrix_.rowIndex(r); } 
    virtual int columnIndex(double c) const  { return matrix_.columnIndex(c); }  
    virtual bool  akimaEnable() const  { return matrix_.akimaEnable(); }
    
    
    virtual void boundRow(double r, 
    	double& row1, int& index1, double& row2, int& index2) const 
    		{ return matrix_.boundRow(r, row1, index1, row2, index2); }
    virtual void boundColumn(double r, 
    	double& column1, int& index1, double& column2, int& index2) const 
    		{ return matrix_.boundColumn(r, column1, index1, column2, index2); }		
    
    double  left() const { return matrix_.left(); }
         double bottom() const { return matrix_.bottom(); } 
         double  right() const { return matrix_.right(); }
         double  top() const { return matrix_.top(); }
         
         double x(double x, double y) const  { return matrix_.x(x, y); }
         double y(double x, double y) const { return matrix_.y(x, y); }
         
     virtual double nearest(double  row, double  column, double &rowOut, double &columnOut) const
     {
    		rowOut=-1;
		columnOut=-1;
		return nearest(row,column);
     }		
     virtual double nearest(double  row, double  column) const
     {
            	if ( columns() == 0  || rows() == 0)
            		return matrix_.missing();

        		if ( column < left() && !same(column, left()) )
                    return matrix_.missing();
                if ( column > right() && !same(column, right()) )
                    return matrix_.missing();
                if ( row < bottom() && !same(row, bottom()) )
                    return matrix_.missing();
                if ( row > top() && !same(row, top()) )
                    return matrix_.missing();
   
                int ri = rowIndex(row);
                int ci = columnIndex(column);
                if ( ri != -1 && ci != -1)
                	return  (*this)(ri, ci);
                double x1, x2;
                double y1, y2;
                int r1, r2, c1, c2;
                vector<double> distances;
                map<double, pair< std::pair<double, double>,  pair<int, int> > > helper;
                vector< std::pair< std::pair<double, double>,  pair<int, int> > > coordinates;
                if (ri != -1 ) {
                	boundColumn(column, x1, c1, x2, c2);
                	coordinates.push_back(make_pair(make_pair(row, x1), std::make_pair(ri, c1)));
                	coordinates.push_back(make_pair(make_pair(row, x2), std::make_pair(ri, c2)));
                }
                else if (ci != -1 ) {
                	boundRow(row, y1, r1, y2, r2);
                	coordinates.push_back(make_pair(make_pair(y1, column), std::make_pair(r1, ci)));
                	coordinates.push_back(make_pair(make_pair(y2, column), std::make_pair(r2, ci)));

                }
                else {
                	boundColumn(column, x1, c1, x2, c2);
                	boundRow(row, y1, r1, y2, r2);

                // 4 points ...
                // x1, y1 - x2, y1 -  x1, y2 - x2, y2
                // find the nearest...
                	coordinates.push_back(make_pair(make_pair(y1, x1), std::make_pair(r1, c1)));
                	coordinates.push_back(make_pair(make_pair(y1, x2), std::make_pair(r1, c2)));
                	coordinates.push_back(make_pair(make_pair(y2, x1), std::make_pair(r2, c1)));
                	coordinates.push_back(make_pair(make_pair(y2, x2), std::make_pair(r2, c2)));
                }

                for (vector< pair< std::pair<double, double>, pair<int, int> > >::iterator coord = coordinates.begin(); coord != coordinates.end(); ++coord) {
                	double distance = (row- coord->first.first)*(row-coord->first.first) + (column - coord->first.second)*(column - coord->first.second);
                	//cout << distance << " [ " << coord->first.first << ", " << coord->first.second << "]" << endl;
                	distances.push_back(distance);
                	helper.insert(make_pair(distance, *coord));
                }

				if ( distances.empty() ) 
                    return matrix_.missing();

				double min = *std::min_element(distances.begin(), distances.end());

                map<double, pair< std::pair<double, double>,  pair<int, int> > >::iterator near = helper.find(min);

				if ( near == helper.end() ) 
					return  matrix_.missing();

				return (*this)(near->second.second.first, near->second.second.second);

    }
	
    virtual double interpolate(double  i, double  j) const 
    {
    	if ( columns() == 0  || rows() == 0)
    		return matrix_.missing();

		if ( j < left() && !same(j, left()) )
            return matrix_.missing();
        if ( j > right() && !same(j, right()) )
            return matrix_.missing();
        if ( i < bottom() && !same(i, bottom()) )
            return matrix_.missing();
        if ( i > top() && !same(i, top()) )
            return matrix_.missing();


    	
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
    
    virtual int    rows() const { return matrix_.rows(); }
    virtual int    columns() const { return matrix_.columns(); }
    virtual int    lowerRow(double v) const { return matrix_.lowerRow(v); }
    virtual int    lowerColumn(double v) const { return matrix_.lowerColumn(v); } 
    virtual double  XResolution() const { return matrix_.XResolution(); } 
    virtual double  YResolution() const { return matrix_.YResolution(); } 
    virtual double  width() const { return matrix_.width(); } 
    virtual double  height() const { return matrix_.height(); } 
    
    virtual const AbstractMatrix& original() const { return matrix_.original(); }
    virtual int firstRow() const 	{ return matrix_.firstRow(); }
    virtual int nextRow(int i, int f) const   { return matrix_.nextRow(i, f); }
    virtual int firstColumn() const { return matrix_.firstColumn(); }
    virtual int nextColumn(int j, int f) const  { return matrix_.nextColumn(j, f); } 
    
    virtual void setMinMax() const {
        
        int nb_rows = rows();
    	int nb_columns = columns();
    	double missing =  matrix_.missing();    
    	        	
    	            for (int r = 0; r < nb_rows; r++) {
    	                for (int c = 0; c < nb_columns; c++) {
    	                     double val =  (*this)(r, c);
    	                     if ( val == missing ) continue; 
    	                     if ( val < min_ ) min_ = val;
    	                     if ( val > max_ ) max_ = val;   	                       
    	                }
    	            }            
    }
    
    double min() const {
       
      	if ( min_ != INT_MAX) 
      		 return min_;
      	
      	setMinMax();
      	return min_;
        
     }
     
    double max() const {
       
    	 
        if ( max_ != -INT_MAX) 
      		 return max_; 
      	
      	setMinMax();
      	return max_;
    }
    
    virtual double  minX() const { return matrix_.minX(); }
    virtual double  maxX() const { return matrix_.maxX(); }
    virtual double  minY() const { return matrix_.minY(); }
    virtual double  maxY() const { return matrix_.maxY(); }
    
    // Implements the AbstractPoints interface
    virtual void setToFirst()   {
        if (points_.empty()) {
        	int nb_rows = rows();
        	int nb_columns = columns();

            points_.reserve(nb_rows * nb_columns);
        	
            for (int r = 0; r < nb_rows; r++) {
                for (int c = 0; c < nb_columns; c++) {
                    if ( matrix_.accept(column(r, c), row(r, c)) ) 
                        if ( !same((*this)(r, c), matrix_.missing() ) ) 
                        	points_.push_back(new UserPoint(column(r,c), row(r,c), (*this)(r, c)));
                }
            }            
        }
        current_ = points_.begin();
    }
    
    //! Method to test the end of collection.
    virtual bool more()   {
        return current_ != points_.end();
    }
   
    virtual bool accept(double x, double y) const { return matrix_.accept(x, y); }
  
    virtual UserPoint& current()
    {
        return **current_;
       
    }
    
    virtual void advance()  {
        current_++;
    }
    
    
    virtual vector<double>&  rowsAxis()  const { return const_cast<MatrixHandler*>(this)->matrix_.rowsAxis(); }
    virtual vector<double>&  columnsAxis() const  { return const_cast<MatrixHandler*>(this)->matrix_.columnsAxis(); }
    
    virtual double  row(int i, int j) const { 
        return matrix_.row(i, j); 
    }
    virtual double  column(int i, int j) const { 
        return matrix_.column(i, j); }
    

    virtual double  regular_row(int i) const { 
        return matrix_.regular_row(i); 
    }
    virtual double  regular_column(int i) const { 
        return matrix_.regular_column(i); 
    }
    
    
    virtual double  missing() const  { return matrix_.missing(); }
    virtual bool  hasMissingValues() const  { 
       
        for (int r = 0; r < rows(); r++) {
                for (int c = 0; c < columns(); c++) {                   
                        if ( operator()(r, c) == matrix_.missing() )
                        	return true;
                }
            }
        return false;
    }    
    
    
    
    MatrixHandler* getReady(const Transformation& transformation) {
    	return matrix_.getReady(transformation);
    }
    
protected:    
    const AbstractMatrix& matrix_;
    mutable VectorOfPointers<vector<UserPoint*> > points_;
    mutable VectorOfPointers<vector<UserPoint*> >::const_iterator current_;
    mutable double min_;
    mutable double max_;
  
};



   










class TransformMatrixHandler : public MatrixHandler
{
public :
 	TransformMatrixHandler(const AbstractMatrix& matrix) : MatrixHandler(matrix)
      {}


    double  operator()(int  i, int  j) const
    {
    	return matrix_( i + minrow_ , j + mincolumn_ );
    }  
    
    double  left() const {
    	return minx_;
    }
    double  right() const {
    	return maxx_;
    }
    double  bottom() const {
        	return miny_;
    }
    double  top() const {
          return  maxy_;
     } 
    
    void set() {
    	for ( int i = 0; i < rows(); i++ ) {
    		double row = matrix_.regular_row(minrow_ +i);
    		rowsMap_.insert(make_pair(row, i));			
    		fastRows_.push_back(row);
    	}
    	for ( int i = 0; i < columns(); i++ ) {
        		double column = matrix_.regular_column(mincolumn_ + i);
        		columnsMap_.insert(make_pair(column, i));			
        		fastColumns_.push_back(column);
        	} 	
    	minx_ =  std::min(fastColumns_.front(), fastColumns_.back());
    	maxx_ =  std::max(fastColumns_.front(), fastColumns_.back());
    	miny_ =  std::min(fastRows_.front(), fastRows_.back());
        maxy_ =  std::max(fastRows_.front(), fastRows_.back());
    }
    
    int  rows() const { return maxrow_ - minrow_ +1; } 
    int  columns() const { return maxcolumn_ - mincolumn_+1; } 
    double regular_row(int index) const {       
    	return fastRows_[index]; 
    }
    double regular_column(int index) const { 
    	return fastColumns_[index]; 
    }
    double real_row(int index) const  { 
    	return fastRows_[index]; 
    }
    double real_column(int index) const { 
    	return fastColumns_[index]; 
    }
    inline double column(int, int j) const {  
    	return fastColumns_[j]; 
    }
    virtual double  real_row(double row, double) const { 
         return row; 
     }
     virtual double  real_column(double, double column) const { 
         return column; 
    }
    inline double row(int i, int) const {  
    	
        return fastRows_[i]; 
    }
    virtual bool  hasMissingValues() const {  return matrix_.hasMissingValues(); }
    double interpolate(double  i, double  j) const { return matrix_.interpolate(i, j);}
    double  missing() const { return matrix_.missing(); }
	int    lowerRow(double r) const {
	
		int last = -1;
		for ( map<double, int>::const_iterator i = rowsMap_.begin(); i != rowsMap_.end(); ++i) { 	
			if ( i->first >  r  ) {				
				return last;
			}				
			last = i->second;
		}		
		return -1;	
	}
	int    lowerColumn(double c) const { 
		
		int last = -1;
		for ( map<double, int>::const_iterator i = columnsMap_.begin(); i != columnsMap_.end(); ++i) {
				if ( i->first > c  ) 
					return last;
				last = i->second;
		}
		return -1;
    } 
	int    upperRow(double r) const {
		
		
		for ( map<double, int>::const_iterator i = rowsMap_.begin(); i != rowsMap_.end(); ++i) { 	
			if ( i->first >  r  ) {				
				return i->second;
			}				
		
		}		
		return -1;	
	}
	int    upperColumn(double c) const { 
		
		
		for ( map<double, int>::const_iterator i = columnsMap_.begin(); i != columnsMap_.end(); ++i) {
				if ( i->first > c  ) 
					return i->second;
		}
		return -1;
    } 

protected :
   int minrow_;
   int maxrow_;
   int mincolumn_;
   int maxcolumn_;
   map<double, int> rowsMap_;
   map<double, int> columnsMap_;
   vector<double> fastRows_;
   vector<double> fastColumns_;
   double minx_;
   double maxx_;
   double miny_;
   double maxy_;
   
   bool rowrevert_;
   bool columnrevert_;
};




class BoxMatrixHandler : public TransformMatrixHandler
{
public:
    BoxMatrixHandler(const AbstractMatrix& matrix, const Transformation& transformation) : 
        TransformMatrixHandler(matrix),
	transformation_(transformation),
	original_(0) 
   { 

        double minx = std::min(transformation.getMinX(), transformation.getMaxX());
        double maxx = std::max(transformation.getMinX(), transformation.getMaxX());
        double miny = std::min(transformation.getMinY(), transformation.getMaxY());
        double maxy = std::max(transformation.getMinY(), transformation.getMaxY());

        int rows = matrix_.rows();
        int columns = matrix_.columns();

        mincolumn_ = columns-1;
        maxcolumn_ = 0;
        minrow_ = rows-1;
        maxrow_ = 0;

        for ( int row = 0; row < rows;  row++) {
        	for ( int column = 0; column < columns;  column++) {
        		double x = matrix_.column(row, column);
        		double y = matrix_.row(row, column);
        		if ( minx <= x && x < maxx && miny <= y && y <= maxy) {
        			mincolumn_ = std::min(mincolumn_, column);
        			maxcolumn_ = std::max(maxcolumn_, column);
        			minrow_ = std::min(minrow_, row);
        			maxrow_ = std::max(maxrow_, row);
        		}
        	}
   	}


        if ( mincolumn_ > maxcolumn_ ) {
        	mincolumn_ = maxcolumn_;
        	MagLog::warning() << "No data to plot in the requested area" << endl;
        }
        if ( minrow_ > maxrow_ ) {
        	minrow_ = maxrow_;
        	MagLog::warning() << "No data to plot in the requested area" << endl;
        }
        //MagLog::broadcast();

        mincolumn_ = std::max(mincolumn_-1, 0);
        maxcolumn_ = std::min(maxcolumn_+1, columns-1);

        columnrevert_ =   matrix_.column(0,  maxcolumn_ ) < matrix_.column(0,  mincolumn_) ;
        
        minrow_ = std::max(minrow_-1, 0);
        maxrow_ = std::min(maxrow_+1, rows-1);

        rowrevert_ =   matrix_.row(maxrow_, 0 ) < matrix_.row(minrow_, 0 ) ;
        set();
               
    }
    
    virtual const AbstractMatrix& original() const { 
    	if ( !original_) 
		original_ = new BoxMatrixHandler(matrix_.original(), transformation_);
	return *original_;
    }
    
    
    virtual void boundRow(double r, 
       	double& row1, int& index1, double& row2, int& index2) const {        	
       		index1 = lowerRow(r);
       		row1 = regular_row(index1);
       		index2 = upperRow(r);
       		row2 = regular_row(index2);
       	
       } 
       
       virtual void boundColumn(double r, 
       	double& column1, int& index1, double& column2, int& index2) const { 
    	   index1 = lowerColumn(r);
           column1 = regular_column(index1);
           index2 = upperColumn(r);
           column2 = regular_column(index2);
       } 
        int rowIndex(double r) const {
            map<double, int>::const_iterator i = rowsMap_.lower_bound(r);
            if (i != rowsMap_.end())
            {
                if ( same(i->first, r) )
                    return i->second;
            }
         	return -1;
         }

        int columnIndex(double c) const {
            map<double, int>::const_iterator i = columnsMap_.lower_bound(c);
            if (i != columnsMap_.end())
            {
                if ( same(i->first, c) )
                    return i->second;
            }
         	return -1;
         }
    
    virtual ~BoxMatrixHandler() { delete original_; }
     
    // Implements the AbstractPoints interface
    virtual bool accept(double x, double y) const {
        return transformation_.in(x, y);
    }
   
    double  minX() const {return std::min(transformation_.getMinX(), transformation_.getMaxX());  }
    double  maxX() const { return std::max(transformation_.getMinX(), transformation_.getMaxX()); }
    double  minY() const { return std::min(transformation_.getMinY(), transformation_.getMaxY()); }
    double  maxY() const { return std::max(transformation_.getMinY(), transformation_.getMaxY());}

protected :
    const Transformation& transformation_;
    mutable BoxMatrixHandler*   original_;
};


class GeoBoxMatrixHandler: public TransformMatrixHandler
{
public:
	GeoBoxMatrixHandler(const AbstractMatrix& matrix, const Transformation& transformation);
    
    virtual const AbstractMatrix& original() const { 
    	if ( !original_) 
		original_ = new GeoBoxMatrixHandler(matrix_.original(), transformation_);
	return *original_;
    }
    
    int columns() const { return  columnsMap_.size(); }
    int rows() const { return rowsMap_.size(); }
    
    int rowIndex(double r) const {
        map<double, int>::const_iterator i = rowsMap_.lower_bound(r);
        if (i != rowsMap_.end())
        {
            if ( same(i->first, r) )
                return i->second;
        }
    	return -1;
    }
    
    int columnIndex(double c) const {
        map<double, int>::const_iterator i = columnsMap_.lower_bound(c);
        if (i != columnsMap_.end())
        {
            if ( same(i->first, c) )
                return i->second;
        }
        return -1;
    }

    
   
    inline double column(int, int column) const {   	
       	return regular_longitudes_[column];
    }
    inline double row(int row, int) const {
    	return regular_latitudes_[row];
    }
     double  operator()(int  row, int  column) const {
    	if ( columns_[column] == -1 )
    			return matrix_.missing();
        return matrix_(rows_[row], columns_[column]);
     }

	int lowerRow(double r) const
	{   
		map<double, int>::const_iterator i = rowsMap_.lower_bound(r);
		if (i != rowsMap_.end())
		{
			if ( same(i->first, r) )
			   return i->second;
			if  ( i != rowsMap_.begin() ) {
				i--;
				return i->second;
			}
		}
		return -1;
	}  

	int lowerColumn(double c) const
	{    	
		map<double, int>::const_iterator i = columnsMap_.lower_bound(c);
		if (i != columnsMap_.end())
		{
			if ( same(i->first, c) )
				return i->second;
			if  ( i != columnsMap_.begin() ) {
				i--;
				return i->second;
			}
		}
		return -1;
	}

	double regular_row(int i) const    { return regular_latitudes_[i]; }
	double regular_column(int i) const { return regular_longitudes_[i]; }
    
	virtual ~GeoBoxMatrixHandler() { delete original_; }
     
	// Implements the AbstractPoints interface
	virtual bool accept(double x, double y) const
	{
		return transformation_.in(x, y);
	}
   
	double  minX() const {return std::min(transformation_.getMinX(), transformation_.getMaxX());  }
	double  maxX() const { return std::max(transformation_.getMinX(), transformation_.getMaxX()); }
	double  minY() const { return std::min(transformation_.getMinY(), transformation_.getMaxY()); }
	double  maxY() const { return std::max(transformation_.getMinY(), transformation_.getMaxY());}

	double  left()   const { return regular_longitudes_.front(); }
	double  bottom() const { return regular_latitudes_.front(); } 
	double  right()  const { return regular_longitudes_.back(); }
	double  top()    const { return regular_latitudes_.back(); } 

	virtual void boundRow(double r, double& row1, int& index1, double& row2, int& index2) const
	{
		index1 = lowerRow(r);
		row1 = regular_latitudes_[index1];
		index2 = index1+1;
		row2 = regular_latitudes_[index2];
	} 

	virtual void boundColumn(double r, double& column1, int& index1, double& column2, int& index2) const
	{
		index1 = lowerColumn(r);
		column1 = regular_longitudes_[index1];
		index2 = index1+1;
		column2 = regular_longitudes_[index2];
	} 

           
protected :
	const Transformation& transformation_;
	mutable GeoBoxMatrixHandler*   original_;
	mutable map<int, int>     rows_;
	mutable map<int, int>     columns_;
	vector<double> regular_latitudes_;
	vector<double> regular_longitudes_;
};




class MonotonicIncreasingMatrixHandler : public MatrixHandler
{
public:
    MonotonicIncreasingMatrixHandler(const AbstractMatrix& matrix) : 
        MatrixHandler(matrix) {
	// Check RowAxis...
	int row = matrix_.rows();
	if (matrix_.regular_row(1) - matrix_.regular_row(0) >= 0) // Increasing Axis...
	     for (int i = 0; i < row; i++) {
             rows_[i] = i;
             newRowsMap_[matrix.regular_row(i)] = i;
	     }
	else // Decreasing axis...
	     for (int i = 0; i < row; i++) {
             rows_[i] = ( row - 1) - i;
             newRowsMap_[matrix.regular_row(( row - 1) - i)] = i;
	     }
	// Check ColumnAxis
	int column = matrix_.columns();
	if (matrix_.regular_column(1) - matrix_.regular_column(0) >= 0) // Increasing Axis...
	     for (int j = 0; j < column; j++) {
             columns_[j] = j;
             newColumnsMap_[matrix.regular_column(j)] = j;
	     }
	else // Decreasing axis...
	     for (int j = 0; j < column; j++) {
             columns_[j] = (column - 1) - j;
             newColumnsMap_[matrix.regular_column((column - 1) - j)] = j;
	     }
	
	}
    virtual ~MonotonicIncreasingMatrixHandler() {}
    
    double  operator()(int  i, int  j) const
    {
        int x = const_cast<MonotonicIncreasingMatrixHandler*>(this)->rows_[i];
        int y = const_cast<MonotonicIncreasingMatrixHandler*>(this)->columns_[j];
        
        return matrix_(x, y);
    }  

    int rows() const { return matrix_.rows(); }
    virtual int columns() const { return matrix_.columns(); }
    virtual double regular_column(int i) const { return matrix_.regular_column(const_cast<MonotonicIncreasingMatrixHandler*>(this)->columns_[i]); }
    virtual double regular_row(int j) const { return matrix_.regular_row( const_cast<MonotonicIncreasingMatrixHandler*>(this)->rows_[j]); }
    virtual double interpolate(double  i, double  j) const {return matrix_.interpolate(i, j);}
    virtual double missing() const { return matrix_.missing(); }
    void print() 
    {
        MagLog::debug() << "MonotonicIncreasingMatrixHandler->\n";
        for (int j = 0; j < rows() ; j++)
	{
            for (int i = 0; i < columns(); i++)
	    {
                MagLog::dev()<< (*this)(j,i) << " ";
            }
            MagLog::dev()<<"\n";
        }
        MagLog::debug() << "<--" << endl;
    }
    
    
    int  lowerRow(double r) const
    {    
        map<double, int>::const_iterator bound = newRowsMap_.find(r);
        if ( bound != newRowsMap_.end() ) return (*bound).second;
    	
        bound = newRowsMap_.lower_bound(r);
        if ( bound == newRowsMap_.end() ) return -1; 
        return (*bound).second - 1;
   
    }  
    
    int  lowerColumn(double c) const
    {    	
        map<double, int>::const_iterator bound = newColumnsMap_.find(c);
        if ( bound != newColumnsMap_.end() ) return (*bound).second;
            
        bound = newColumnsMap_.lower_bound(c);
        if ( bound == newColumnsMap_.end() ) return -1;
        return (*bound).second - 1;
    }
    
protected :
    map<int, int>     rows_;
    map<int, int>     columns_;
    map<double, int>   newRowsMap_;
    map<double, int>   newColumnsMap_;
};
 


class OriginalMatrixHandler : public MatrixHandler
{
public:
	OriginalMatrixHandler(AbstractMatrix& matrix) : 
		MatrixHandler(matrix.original())
	{
	}
};


class ThinningMatrixHandler : public MatrixHandler
{
public:
	ThinningMatrixHandler(const AbstractMatrix& matrix, int  fr, int  fc) : 
		MatrixHandler(matrix), frequencyRow_(fr), frequencyColumn_(fc)
	{
		int rows = matrix_.rows();
		int columns = matrix_.columns();

		int row = 0;
		for (int i = 0; i < rows; i+=frequencyRow_)
		{
		    rowIndex_.insert(make_pair(row, i));
		    row++;           
		}
		int column=0;
		for (int i = 0; i < columns; i+=frequencyColumn_)
		{
			//MagLog::dev()<< "Sample --> " << column << "=" << i << endl;
			columnIndex_.insert(make_pair(column, i));
			//MagLog::dev()<< "Sample --> " << column << "=" << i << "[" << regular_column(column) << "]" << endl;

			column++;           
		}
		columnIndex_.insert(make_pair(column, columns-1));
		//MagLog::dev()<< "Sample --> " << column << "=" << columns-1 << "[" << regular_column(column) << "]"<< endl;
	}

	int rows() const { return rowIndex_.size(); }
	int columns() const { return columnIndex_.size(); }
	
	double operator()(int row, int column) const {return matrix_(rowIndex(row), columnIndex(column));}
	double column(int row, int column) const {return matrix_.column(rowIndex(row), columnIndex(column));}
	double row(int row, int column) const {return matrix_.row(rowIndex(row), columnIndex(column));}
	double regular_row(int row) const {return matrix_.regular_row(rowIndex(row));}
	double regular_column(int column) const {return matrix_.regular_column(columnIndex(column));}

protected :
	int columnIndex(int column) const
	{
		map<int, int>::const_iterator index = columnIndex_.find(column);
		ASSERT( index != columnIndex_.end() );
		return index->second;
	}
	int rowIndex(int row) const
	{
		map<int, int>::const_iterator index = rowIndex_.find(row);
		ASSERT( index != rowIndex_.end() );
		return index->second;
	}
	int  frequencyRow_;
	int  frequencyColumn_;
	map<int, int> rowIndex_;
	map<int, int> columnIndex_;
};

} // namespace magics
#endif
