/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeMatrix.h
    \brief This file models the object matrix of type double
*/
#ifndef  __TERRALIB_INTERNAL_MATRIX_H
#define  __TERRALIB_INTERNAL_MATRIX_H

#include "TeDefines.h"
#include <TeException.h>

//! This class represents a matrix of elements of type double
class TL_DLL TeMatrix
{
private:
	int nrow;	// Number of rows
	int ncol;	// Number of columns
	double **mat;	// TeMatrix of double elements 
	double lixo;	// Ancillary variable

	//! Allocates memory to holds a matrix of l lines by c columns
	short Alloc( int l, int c);

public:
	//! Normal constructor
	TeMatrix();

	//! Copy constructor
	TeMatrix(const TeMatrix& );

	//! Destructor
	~TeMatrix();

	//! Clear memory for matrix.
	void Clear();

	//! Checks if the matrix is initialized.
	int Initialized(){ return ( nrow > 0 && ncol > 0 ) ? (true) : (false); }

	//!	Initializes the matrix with a vector of values
	int Init( int nrow = 1, int ncol = 1 , double* f=0);

	//!	Initializes the matrix with the same value.
	int Init( int nrow , int ncol , double f  );
	
	//! Diagonal matrix with different values.
	int Init(int k, double* f);

	//! Diagonal matrix with the same value
	int Init(int k, double f); 

  //! Acess element in position (lin,col)
  const double& operator()( int lin, int col ) const {
    if( ( lin >= 0 ) && ( lin < nrow ) && ( col >= 0 ) && 
      ( col < ncol ) ) {
      
      return mat[lin][col];
    } else {
      throw TeException( UNKNOWN_ERROR_TYPE, 
        "Trying to access an invalid matrix position" );
            
      return lixo;
    }
  }  
      
	//! Acess element in position (lin,col)
	double& operator()( int lin, int col ){ 
    if( ( lin >= 0 ) && ( lin < nrow ) && ( col >= 0 ) && 
      ( col < ncol ) ) {
      
      return mat[lin][col];
    } else {
      throw TeException( UNKNOWN_ERROR_TYPE, 
        "Trying to access an invalid matrix position" );
            
      return lixo;
    }
	}

	//! Assign matrix values to another one using operator =
	TeMatrix& operator=( const TeMatrix& m ); 

	//! Compares two matrix using the operator ==
	int 	operator==(const TeMatrix& m) const;

	//! Operator *=
	void 	operator*=(double);

	//! Operator unary minus 
	TeMatrix 	operator-();

	//!	Returns the number of rows.
	int	Nrow() const { return nrow; }
 	
	//!	Returns the number of columns.
	int	Ncol()	const { return ncol; } 

	//!	Print the matrix elements .
	void Print();

	//!	Switches two rows "a" and "b".
    void switchRows(int a ,int b);
		
	//! Combines two rows in according to row(i) += b*row(j)
   void combineRows(int i,double b,int j);

	//! 		Finds the transpose of a matrix.
	int	Transpose( TeMatrix& mt ) const;
	
	//! 		Finds the inverse of a matrix, if any.
	int	Inverse ( TeMatrix& mt ) const;

	//!		Finds the inverse of a triangle matrix	acquired from Cholesky decomposition
	int	CholeskyInv (TeMatrix&	mt) const;	
		
	//!	Calculates the inverse of a lowertriangle matrix aquired from the Cholesky decomposition of a simetric positive definide matrix.
	int MatTransf( TeMatrix& mt );	

//!	Checks if the matrix is an uppertriangle matrix.
   	int isUpperTriangle() const;
	
//!	Checks if the matrix is an lowertriangle matrix.
    int isLowerTriangle() const;	

//!	Checks if the matrix is a simetric matrix
	int	isSimetric()	const;		

//!	Checks if the matrix is positive definide
	int	isPositiveDefinide() const;

//!	Calculates the cofactor value of a matrix.
	int	CoFactor(int irow, int jcol, TeMatrix& m) const;

//!	Calculates the determinant value.
	double	Determinant() const;

//	Extracts a triangle matrix from a simetric positive	definide one
	int	CholeskyDecomp( TeMatrix& mt );	

//!	Calculates the EigenVectors.
	int	EigenVectors( TeMatrix& mt ) const;

//!	Calculates the EigenValues.
	int	EigenValues( TeMatrix& mt ) const;

//!	Calculates the EigenVectors.
	int EigenVec( double e_vec[] );
	
	/**
	 * @brief	Matrix trace calcule (the sum of diagonal elements).
	 * @return The matrix trace.
	 */	
	double getTrace() const;
	
	/**
	 * @brief	Generate a identity matrix .
	 * @param identity The generated identity matrix.
	 * @param width The identity matrix width.
	 * @return true if OK, false on errors.
	 */
	static bool getIdentity( TeMatrix& identity, unsigned int width );

//!	Sums two matrices.
	friend TL_DLL TeMatrix operator+(const TeMatrix& m1,const TeMatrix& m2);

//!	Subtracts two matrices.
	friend TL_DLL TeMatrix operator-(const TeMatrix&,const TeMatrix&);

//!	Calculates the product of two matrices.
	friend TL_DLL TeMatrix operator*(const TeMatrix&,const TeMatrix&);

//!	Product of the matrix by a constant.
	friend TL_DLL TeMatrix operator*(double c,const TeMatrix&);
};


#endif

