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
    \brief Definition of the Template class Matrix.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Feb-2004
    
    Changes:
    
*/

#ifndef Matrix_H
#define Matrix_H

#include <cfloat>
#include "magics.h"
#include "MagException.h"

namespace magics {

class XmlNode;
class MatrixHandler;
class Transformation;

class AbstractMatrix 
{
public :
    virtual ~AbstractMatrix(){}
    virtual double  operator()(int  i, int  j) const = 0;   
    virtual int    rows() const = 0;
    virtual int    columns() const = 0;
    virtual double  regular_row(int) const = 0;
    virtual double  regular_column(int) const = 0;
    virtual double  row(int, int) const = 0;
    virtual double  column(int, int) const = 0;


    virtual int    lowerRow(double) const = 0;
    virtual int    lowerColumn(double) const = 0;    
    virtual double  interpolate(double  i, double  j) const = 0; 
    virtual double  nearest(double i, double j, double &iOut, double &jOut) const = 0;  
    virtual double  nearest(double i, double j) const =0 ;
    virtual double  missing() const = 0;
    virtual double  XResolution() const = 0;
    virtual double  YResolution() const = 0;
    virtual double  width() const = 0;
    virtual double   height() const = 0;
    virtual bool akimaEnable() const { return false; }

    virtual MatrixHandler* getReady(const Transformation&) const { NOTIMP; return 0;}

   
   

    
     virtual const AbstractMatrix&  original() const { return *this; }
     
     virtual int firstRow() const = 0;
     virtual int nextRow(int, int) const = 0;
     
     virtual int firstColumn() const = 0;
     virtual int nextColumn(int, int) const = 0;
     
     template <class O> 
     void for_each(int xf, int yf, const O& object)
     {
        
     	 for ( int i = firstRow(); i > 0; i = nextRow(i, xf) )
     		 for ( int j = firstColumn(); j > 0; j = nextColumn(j, yf) )
     			 	object(row(i,j), column(i,j), (*this)(i, j));
     }

    virtual double  minX() const = 0;
    virtual double  minY() const = 0;
    virtual double  maxX() const = 0;
    virtual double  maxY() const = 0;
    virtual double  min() const = 0;
    virtual double  max() const = 0;
     
    virtual double  left() const = 0;
       virtual double  top() const = 0;
       virtual double  right() const = 0;
       virtual double  bottom() const = 0;
       
       virtual double  x(double, double) const = 0;
       virtual double  y(double, double) const = 0;      
       
    virtual int rowIndex(double r) const  = 0;
    virtual int columnIndex(double c) const = 0;
    
    virtual void boundRow(double r, 
    	double& row1, int& index1, double& row2, int& index2) const = 0;
    
    virtual void boundColumn(double r, 
    	double& column1, int& index1, double& column2, int& index2) const = 0;
    
    virtual bool accept(double, double) const { return true; }
   
    virtual vector<double>&  rowsAxis() const = 0;
    
    virtual bool  hasMissingValues() const  { return false; }
  
    
    virtual vector<double>&  columnsAxis() const = 0;
    virtual void print(ostream& out) const 
        { out << "No Print implemented for this MatrixHandler" << "\n"; }
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AbstractMatrix& p)
		{ p.print(s); return s; }
};

struct Plus 
{
    Plus(double offset, double missing) : offset_(offset), missing_(missing) {}
    double operator()(double x) const { return ( same(x, missing_)  ) ? missing_ : x + offset_; }
    double offset_;
    double missing_;
};

struct Multiply 
{
    Multiply(double factor, double missing) : factor_(factor), missing_(missing) {}
    double operator()(double x) const { return ( same(x, missing_) ) ? missing_ : x * factor_; }
    double factor_;
    double missing_;
};


class OutOfRange : public MagicsException
{
public:
    
    OutOfRange(double r, double c) 
    { 
        ostringstream s;
        s << "Out of Range: Cannot access [" << r << ", " << c << "]" << ends;
        what_ = s.str();
    }
    OutOfRange(double x) 
    { 
        ostringstream s;
        s << "Out of Range: Cannot access [" << x << "]" << ends;
        what_ = s.str();
    }
};



class Matrix: public AbstractMatrix, public magvector<double> {

public:
	Matrix(int rows, int columns): 
		rows_(rows), 
		columns_(columns),
		missing_(DBL_MIN),
		akima_(false),
		min_(DBL_MAX), max_(DBL_MIN)
	{  
         set(rows, columns);
	}
	
	Matrix* clone() { return new Matrix(); }
	void set(const XmlNode&) { }
	
	MatrixHandler* getReady(const Transformation&) const;

    Matrix(int rows, int columns, double val):     
        rows_(rows), 
        columns_(columns), missing_(DBL_MIN), akima_(false), min_(DBL_MAX), max_(DBL_MIN)
     {       
         
         resize(rows_ * columns_, val); 
         rowsAxis_.resize(rows_, val);
         columnsAxis_.resize(columns_, val); 
	}
    
    Matrix(): missing_(DBL_MIN), akima_(false), min_(DBL_MAX), max_(DBL_MIN) {}
    
    void set(int rows, int columns) 
    {
         rows_ = rows;
         columns_ = columns;
         reserve(rows_*columns_); 
         rowsAxis_.reserve(rows); 
         columnsAxis_.reserve(columns);  
    }
    
     double min() const;
     double max() const;
     void min(double m) {min_=m;}
     void max(double m) {max_=m;}
    
    virtual ~Matrix() {}
    
    double width() const { return regular_column(columns_ - 1) - regular_column(0); }
    double height() const { return regular_row(rows_ - 1) - regular_row(0); }
    
    int rows() const { return rows_; }
    int columns() const { return columns_; }
     
    double regular_row(int i) const { return rowsAxis_[i]; } 
    double row(int i, int) const { return regular_row(i); }
    
    void release()
    {
    	rows_ = 0;
    	columns_ = 0;
    	rowsAxis_.clear();
    	rowsAxis_.resize(0);
    	columnsAxis_.clear();
    	this->clear();
    	columnsAxis_.resize(0);
		this->resize(0);
    }
 
    
    double regular_column(int j) const { return columnsAxis_[j];  }
    double column(int, int j) const { return columnsAxis_[j];  }
     
    void missing(double missing) { missing_ = missing; }
    double missing() const { return missing_; }
    
    void setRowsAxis(const vector<double>& axis) 
    {
        int ind = 0;
        rowsAxis_.reserve(axis.size());
        for (vector<double>::const_iterator val = axis.begin(); val != axis.end(); ++val) {
            rowsAxis_.push_back(*val);
            rowsMap_[*val] = ind++;
        }
        rows_ = axis.size();          
    }
    void setColumnsAxis(const vector<double>& axis) 
    {
        int ind = 0;
        columnsAxis_.reserve(axis.size());
        for ( vector<double>::const_iterator val = axis.begin(); val != axis.end(); ++val) {
            columnsAxis_.push_back(*val);
            columnsMap_[*val] = ind++;
        }
        columns_ = axis.size();
            
    }
    
    virtual void setMapsAxis() 
    {
        int ind = 0;
        for (vector<double>::const_iterator val = rowsAxis_.begin(); val != rowsAxis_.end(); ++val) {
            rowsMap_[*val] = ind++;
        }
        rows_ = ind;
        
        
        ind = 0;
        for (vector<double>::const_iterator val = columnsAxis_.begin(); val != columnsAxis_.end(); ++val) {
            columnsMap_[*val] = ind++;
        }
        columns_ = ind;
    }
    
  
    double interpolate(double r, double c) const;
    double nearest(double i, double j) const {double d1, d2; return nearest(i,j,d1,d2);}
    double nearest(double i, double j,double &iOut, double &jOut) const;
    double nearest_index(double i, double j,double &iOut, double &jOut) const;

    void multiply(double factor);   
    void plus(double offset);
    
     virtual int firstRow() const { return 0; }
     virtual int nextRow(int i, int f) const   
     { 
     	i += f; 
	return ( i < rows_ ) ? i : -1;
     }
     
     virtual int firstColumn() const { return 0; }
     virtual int nextColumn(int j, int f) const   
     { 
     	j += f; 
	return ( j < rows_ ) ? j : -1;
     }
     
    
    double operator()(int row, int column) const;

    double YResolution() const {
    	
           magvector<double> diff;
           diff.reserve(rowsAxis_.size());
           std::adjacent_difference(rowsAxis_.begin(), rowsAxis_.end(), back_inserter(diff));
           double resol = std::accumulate(diff.begin()+1, diff.end(), 0.)/(diff.size()-1);
           //MagLog::dev() << "Matrix::YResolution()--->" << resol << "\n";
           return resol;
    }
     double XResolution() const {
           magvector<double> diff;
           diff.reserve(columnsAxis_.size());
           std::adjacent_difference(columnsAxis_.begin(), columnsAxis_.end(), back_inserter(diff));
           double resol = std::accumulate(diff.begin()+1, diff.end(), 0.)/(diff.size()-1);
           //MagLog::dev() << "Matrix::XResolution()--->" << resol << "\n";
           return resol;
    }
   
    vector<double>& rowsAxis() const { return rowsAxis_; }
    vector<double>& columnsAxis() const  { return columnsAxis_; }
    
    
    double  minX() const { return columnsAxis_.front(); }
    double  minY() const { return rowsAxis_.front(); } 
    double  maxX() const { return columnsAxis_.back(); }
    double  maxY() const { return rowsAxis_.back(); }
    
    double  left() const { return columnsAxis_.front(); }
      double bottom() const { return rowsAxis_.front(); } 
      double  right() const { return columnsAxis_.back(); }
      double  top() const { return rowsAxis_.back(); }
      
      double x(double x, double) const  { return x; }
      double y(double, double y) const { return y; }
    
    virtual int rowIndex(double r) const     { return row_ind(r); } 
    virtual int columnIndex(double c) const  { return column_ind(c); } 
    virtual bool akimaEnable() const  { return akima_; }  
    void akimaEnabled()  { akima_ = true; }  
    void akimaDisabled()  { akima_ = false; } 
    virtual void boundRow(double r, 
    	double& row1, int& index1, double& row2, int& index2) const { 
    	
    	index1 = this->lowerRow(r);
           		row1 = this->regular_row(index1);
           		index2 = this->upperRow(r);
           		row2 = this->regular_row(index2);
    } 
    
    virtual void boundColumn(double r, 
    	double& column1, int& index1, double& column2, int& index2) const { 
    	
    	index1 = this->lowerColumn(r);
    	column1 = this->regular_column(index1);
        index2 = this->upperColumn(r);
        column2 = this->regular_column(index2);
    } 
    
	int    lowerRow(double r) const {

		int last = -1;
		for ( map<double, int>::const_iterator i = rowsMap_.begin(); i != rowsMap_.end(); ++i) { 	
			if ( i->first >  r  ) {				
				return last;
			}				
			last = i->second;
		}		
		return last;	
	}
	int    lowerColumn(double c) const { 
		
		int last = -1;
		for ( map<double, int>::const_iterator i = columnsMap_.begin(); i != columnsMap_.end(); ++i) {
				if ( i->first > c  ) 
					return last;
				last = i->second;
		}
		return last;
    } 
	int    upperRow(double r) const {
		
		
		for ( map<double, int>::const_iterator i = rowsMap_.begin(); i != rowsMap_.end(); ++i) { 	
			if ( i->first >=  r  ) {				
				return i->second;
			}				
		
		}		
		return -1;	
	}
	int    upperColumn(double c) const { 
		
		
		for ( map<double, int>::const_iterator i = columnsMap_.begin(); i != columnsMap_.end(); ++i) {
				if ( i->first >= c  ) 
					return i->second;
		}
		return -1;
    } 

	map<double, map<double, double> > index_;
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const {
	 	out << "Matrix<P>[";
    	out << "rowsAxis=" << rowsAxis_;
    	out << ", columnsAxis=" << columnsAxis_;
   	 	out << ", values=";
    	magvector<double>::print(out);
    	out << "]"; 
	 }
	 
     map<double, int>   rowsMap_;
     mutable magvector<double>     rowsAxis_;
     
     map<double, int>    columnsMap_;
     mutable magvector<double>      columnsAxis_;
     
     int rows_;
     int columns_;
     double missing_;
     bool akima_;

     int row_ind(double row) const {
        map<double, int>::const_iterator i = rowsMap_.lower_bound(row);
    	if ( same(i->first, row) )
    			return i->second;
        if ( i == rowsMap_.end() ) {
            map<double, int>::const_reverse_iterator i = rowsMap_.rbegin();
            if ( same(i->first, row) )
                return i->second;
        }
    	return -1;
    }
    int column_ind(double column) const {
	  
       map<double, int>::const_iterator i = columnsMap_.lower_bound(column);
	   
    	if ( same(i->first, column) )
    	    	return i->second;
        if ( i == columnsMap_.end() ) {
            map<double, int>::const_reverse_iterator i = columnsMap_.rbegin();
            if ( same(i->first, column) )
                return i->second;
        }
    	return -1;
    }

private:
   mutable double min_;
   mutable double max_;
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Matrix& p)
		{ p.print(s); return s; }

};


class ProjectedMatrix: public Matrix
{
public:
	    
		ProjectedMatrix(int rows, int columns);
	
		
		void getReady(); // Prepare the matrix ... 
		  
		vector<double>&  values() const { return values_; }
		vector<double>&  rowsArray() const { return rowsArray_; }
		vector<double>&  columnsArray() const { return columnsArray_; }
		
		int index(int r, int c) { return (r* origColumns_) + c; }


protected:
	      void build();
	      
	      int origColumns_;
	      int origRows_;
		  mutable vector<double> rowsArray_;
		  mutable vector<double> columnsArray_; 
		  mutable vector<double> values_; 
		  
		  double minx_;
		  double miny_;
		  double maxx_;
		  double maxy_;
		  
		  double stepx_;
		  double stepy_;
		  
			
};
class RotatedMatrix: public Matrix
{

public:
		RotatedMatrix(int, int);

		vector<double>&  values() const { return values_; }
		vector<double>&  rowsArray() const { return rowsArray_; }
		vector<double>&  columnsArray() const { return columnsArray_; }

		double operator()(int r, int c) const { return values_[r* columns_ + c]; }
		double row(int r, int c) const { return rowsArray_[r* columns_ + c]; }
		double column(int r, int c) const { return columnsArray_[r* columns_ + c]; }
		double  XResolution() const  { return column(0,1) - column(0,0); }
		double  YResolution() const  { return row(1,0) - row(0,0); }
		MatrixHandler* getReady(const Transformation&) const;
protected:

		mutable vector<double> rowsArray_;
		mutable vector<double> columnsArray_;
		mutable vector<double> values_;



};

} // namespace magics

#endif
