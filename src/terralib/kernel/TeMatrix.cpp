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

 // TeMatrix.c -- matrix of type double

#include <TeMatrix.h>

#include <TeAgnostic.h>

#include <math.h>
#include <stdio.h>


TeMatrix::TeMatrix(){
	nrow = 0;
	ncol = 0;
	mat  = NULL;
	lixo = 0.;
}

void
TeMatrix::Clear()
{
	if( mat != NULL ){
		int i;
		for( i = 0; i < nrow; i++ )
			if( mat[i] != NULL )
			{
				delete []mat[i];
				mat[i]=NULL;
			}
		delete []mat;
		mat = NULL;
	}
	nrow = ncol = 0;
	lixo = (double)0;
}

TeMatrix::~TeMatrix()
{
	Clear();
}

short
TeMatrix::Alloc(int nl, int nc)
{
	Clear();
	if( nl <= 0 || nc <= 0 ){
		return true;
	}
	nrow = nl;
	ncol = nc;
	if (  (mat = new double*[nrow]) == NULL ){
		Clear();
		return false;
	}
	int i,j;
	for( i = 0; i < nrow; i++ ) mat[i] = NULL;
	for( i = 0; i < nrow; i++ ){
		if( (mat[i] = new double[ncol]) == NULL ){
			Clear();
	       		return false;
		}
		for( j = 0; j < ncol; j++ ) mat[i][j] = (double)0.;
	}
	return true;
}

TeMatrix::TeMatrix(const TeMatrix& m)
{
     nrow = ncol = 0;
     mat = NULL;
     lixo = 0.;
    *this = m;
}

int
TeMatrix::Init( int nl, int nc , double* f )
{
 	if( f == NULL )
		return Init( nl, nc,  (double) 0 );
	if( Alloc( nl, nc ) ){
		int c=0;
		int i,j;
		for ( i = 0; i < nrow; i++)
			for (j = 0; j < ncol; j++)
				mat[i][j] = f[c++];
		return true;
	}
	return false;
}

int
TeMatrix::Init(int nl, int nc, double f )
{
 	// initialize matrix
	if( Alloc( nl, nc ) ){
 		int i,j;
     		for (i = 0; i < nrow; i++)
       			 for (j = 0; j < ncol; j++)
        		 	mat[i][j] = f;
		return true;
	}
	return false;
}

int
TeMatrix::Init(int k, double* f)
{
 	if( f == NULL )
		return Init( k, k,  (double) 0 );
	if( Alloc(k,k) == false ) 
		return false;
	int i,j;
	for ( i = 0; i < nrow; i++  )
    		for (j = 0; j < ncol; j++)
        		mat[i][j] = (i==j)? f[i]: (double)0.;
   	return true;
}

int
TeMatrix::Init(int k, double f)
{
	if( Alloc(k,k) == false )
		return false;
	int i,j;
	for (i=0; i<nrow; i++)
		for (j=0; j<ncol; j++)
			mat[i][j] = (i==j)? (double)f:0;
	return true;
}

TeMatrix& 
TeMatrix::operator=(const TeMatrix& m)
{

	if( Alloc(m.nrow,m.ncol) == false ){
		printf("\nMatrix::operator=:no memory available \n");
		return *this;
    	}
	int	i,j;
    	for( i = 0; i < nrow; i++)
     		for( j = 0; j < ncol; j++)
       			mat[i][j] = m.mat[i][j];
    	return *this;
}

int
TeMatrix::operator==(const TeMatrix& m) const
{
	if( nrow != m.nrow || ncol != m.ncol )
		return false;
	int	i,j;
	for ( i = 0; i < nrow; i++)
		for ( j = 0; j < ncol; j++)
			if ( mat[i][j]!= m.mat[i][j] )
				return false;
	return true;
}

void
TeMatrix::operator*=(double f)
{
	int	i,j;
	for( i = 0; i < nrow; i++)
		for( j = 0; j < ncol; j++)
			mat[i][j] *= f;
	return;
}


//-------------------------------------------------------------friend methods

TeMatrix
operator+(const TeMatrix& m,const TeMatrix& n)
{
	if( m.nrow != n.nrow || m.ncol != n.ncol ){
		printf("\nMatrix::operator+ >>> Operandos com tamanhos diferentes\n");
	        return  m;
	} 
	TeMatrix rm;
	if( rm.Init(m.Nrow(),m.Ncol()) == false ){
		printf("\nMatrix::operator+ >>> Memoria nao disponivel\n");
		return	m;
	}
	int	i,j;
	for ( i = 0; i < m.Nrow(); i++)
		for ( j = 0; j < m.Ncol(); j++)
			rm.mat[i][j] = m.mat[i][j] + n.mat[i][j];
	return rm;
}

TeMatrix
operator-(const TeMatrix& m,const TeMatrix& n)
{
	if( m.nrow != n.nrow || m.ncol != n.ncol ){
		printf("\nMatrix::operator+ >>> Operandos com tamanhos diferentes\n");
	        return  m;
	} 
	TeMatrix rm;
	if( rm.Init(m.Nrow(),m.Ncol()) == false ){
		printf("\nMatrix::operator- >>> Memoria nao disponivel\n");
		return	m;
	}

	int	i,j;
	for ( i = 0; i <  m.Nrow(); i++)
		for ( j = 0; j < m.Ncol(); j++)
			rm.mat[i][j] = m.mat[i][j] - n.mat[i][j];
	return rm;
}

TeMatrix
TeMatrix::operator-()
{
	TeMatrix rm;  
	if( rm.Init(Nrow(),Ncol()) == false ){
		printf("\n operator-:no memory \n");
		return	*this;
	}
	int i,j;
	for (i=0; i<Nrow(); i++)
		for (j=0; j<Ncol(); j++)
			rm.mat[i][j] = -mat[i][j];
	return rm;
}

TeMatrix 
operator*(const TeMatrix& m,const TeMatrix& n)
{
	if ( m.Ncol() != n.Nrow() ) {
		printf( "\nMatrix::operator* >>> Operandos com tamanhos diferentes\n");
		return m;
	}
	int nr = m.Nrow(), nc =n.Ncol();
	TeMatrix result;
	if( result.Init(nr,nc) == false){
		printf("\nMatrix::operator* >>> Memoria nao disponivel\n");
		return	m;
	}

	double	sum = (double)0.;
	int	l,c,i;

	for ( l = 0; l < m.Nrow(); l++){
		for ( c = 0; c < n.Ncol(); c++){
        		sum = (double)0.;
			for ( i = 0; i < m.Ncol(); i++)
				sum += m.mat[l][i] * n.mat[i][c];
    			result.mat[l][c] = sum;

		}
	}
	return result; 
}

TeMatrix
operator*(double f,const TeMatrix& m)
{
	int nr = m.Nrow(), nc =m.Ncol();
	TeMatrix rm;
	if( rm.Init(nr,nc) == false){
		printf("\noperator*:no memory");
		return m;
	}
	int i,j;
	for (i = 0; i < rm.Nrow(); i++)
		for (j = 0; j < rm.Ncol(); j++)
			rm.mat[i][j] = f * m.mat[i][j];
	return rm;
}

//VRMC 12/98
int
TeMatrix::isUpperTriangle() const
{
	// elements above diagonal != 0
	int i,j;
	for(j = 0; j < ncol; j++)
		for(i = j+1; i < nrow; i++)
			if ( mat[i][j]  != (double)0. )
				return false;
	return true;
}

//VRMC 12/98
int
TeMatrix::isLowerTriangle() const	
{
	// elements under diagonal different 0
	int i,j;
	// VRMC 11/98
	for (i=0; i< nrow; i++)
		for (j= i+1; j <ncol; j++)
			if ( mat[i][j] != (double)0. )
				return false;
	return true;
}

double
TeMatrix:: Determinant() const
{
	if (Nrow() != Ncol()) {
		return lixo;
	}
	if (Nrow()==1) return mat[0][0];
	if (Nrow()==2)
		return mat[0][0] * mat[1][1] - mat[0][1] * mat[1][0];
	if (Nrow()==3) 
		return (  mat[0][0] * mat[1][1] * mat[2][2]
			 +mat[0][1] * mat[1][2] * mat[2][0]
			 +mat[0][2] * mat[1][0] * mat[2][1]
			 -mat[0][1] * mat[1][0] * mat[2][2]
			 -mat[0][0] * mat[1][2] * mat[2][1]
 			 -mat[0][2] * mat[1][1] * mat[2][0] );
	if ( isUpperTriangle() ) {
		double val = 1;
		int i;
		for (i = 0; i < Nrow(); i++) val *= mat[i][i];
		return val;
    	}
	TeMatrix mt;
	double val = 0;
	double det;
	int sign = 1;
	int j;
	for(j = 0; j < Ncol(); j++) {
		if( CoFactor(0,j, mt) == false )
			return (double)0;
		det = mt.Determinant();		
		val += sign * mat[0][j] * det;
		sign *= -1;
     	}
	return val;
}

int
TeMatrix::CoFactor(int irow, int jcol, TeMatrix& mt ) const
{
 	if ( nrow == 1||ncol == 1 ) {
		printf("\nMatrix::CoFactor >>>  can't CoFactor row or column matrix\n");
		return false;
   	}
	if( mt.Init(nrow-1,ncol-1) == false){
		printf("\nMatrix::CoFactor: Memoria nao disponivel\n");
		return false;
	}
	int getcol, getrow =0;
	int i,j;
	for(i=0; i < mt.Nrow(); i++) {
		if ( getrow == irow ) ++getrow;
		if ( getrow == nrow ) break;
		getcol = 0;
		for(j=0; j < mt.Ncol(); j++) {
			if ( getcol==jcol ) ++getcol;
			if ( getcol==ncol ) continue;
			mt.mat[i][j] = mat[getrow][getcol];
			++getcol;
		}
		++getrow;
	}
	return true;
}

int 
TeMatrix::Transpose( TeMatrix &rm ) const
{
	if( &rm == this ){
		printf("\nMatrix::Transpose >>> Operacao usa duas matrizes\n");
		return false;
	}

	if( rm.Init( ncol, nrow ) == false){
		printf("\nMatrix::Transpose >>> Memoria nao disponivel\n");
		return false;
	}
	int i,j;
	for(i = 0; i < ncol; i++)
		for( j = 0; j < nrow; j++)
			rm.mat[i][j] = mat[j][i];
	return true;
}

// VRMC 12/98
int
TeMatrix::isSimetric()	const
{
	int i,j;

	if (nrow != ncol){
		printf("\nMatrix::isSimetric >>> Memoria nao disponivel\n");
		return false;
	}

	for (i = 0; i < nrow; i++)
		for (j = 0; j < ncol; j++)
			if ( mat[i][j] != mat[j][i]) return false;

	return	true;
}

// VRMC 12/98
int		
TeMatrix::isPositiveDefinide() const
{
	int i, j, dim, subdim;
	TeMatrix	Submat;

	if (nrow != ncol){
		printf("\nMatrix::isPositiveDefinide >>> Matriz tem que ser quadrada\n");
		return false;
	}
	dim = nrow;

	for (subdim=1 ; subdim <= dim; subdim++)
	{
		if( Submat.Init( subdim,subdim ) == false ){
			printf("\nMatrix::isPositiveDefinide>>>Memoria nao disponivel!\n");
			return false;
		}

		for ( i=0; i < subdim; i++)
			for (j = 0; j < subdim; j++)
				Submat(i,j) = mat[i][j];
		
		if (Submat.Determinant() <= 0.) {
			printf("\nMatrix::isPositiveDefinide>>>Matriz nao e positiva definida!\n");
			return false;
		}

		Submat.Clear();
	}
	return true;
}

// VRMC 12/98
int
TeMatrix :: MatTransf( TeMatrix& mt )
{
	TeMatrix	TI;	// inverse of a inferior triangular matrix
	int	dim;

	if( &mt == this ){
		printf("\nMatrix::MatTransf >>> Operacao usa duas matrizes\n");
		return false;
	}

	if( nrow <= 0 || ncol <= 0 ){
		printf("\nMatrix::MatTransf >>> Dimensoes da matriz invalidas!\n");
		return false;
	}

	if( nrow != ncol ){
		printf("\nMatrix::MatTransf >>> Dimensoes da matriz invalidas!\n");
		return false;
	}

	dim = nrow;
	if( mat[0][0] <= 0. ){
		printf("\nMatrix::MatTranf >>> ERROR\n");
		return false;
	}

	if( TI.Init( dim, (double)0. ) == false ){
		printf("\nMatrix::MatTransf >>> Memoria nao disponivel!\n");
		return false;
	}


	//--	Calculate inferior triangular matrix by Cholesky decomposition method
	if ((*this).CholeskyDecomp(TI) == false) {
		printf("\nMatrix::MatTransf>>> Nao foi possivel decompor a matriz!\n");
		return false;
	}

	//--	Calculate the inverse of Cholesky matrix
//	if (TI.CholeskyInv(mt) != true)
	if (!(TI.CholeskyInv(mt)))
	{
		printf("\nMatrix::MatTransf>>> Matriz nao inversivel!\n");
		return false;
	}

	return true;
}

// VRMC 12/98			
int
TeMatrix::CholeskyInv (TeMatrix&	mt) const
{
	if( mt.Init(nrow,(double)0.) == false ){
		printf("\nMatrix::CholeskyInv >>> Memoria nao disponivel!\n");
		return false;
	}

	if( &mt == this ){
		printf("\nMatrix::CholeskyInv >>> Operacao usa duas matrizes\n");
		return false;
	}
	if ( nrow != ncol ){
       		printf("\nMatrix::CholeskyInv>>> Can't invert a non-square matrix");
       		return false;
   	}
//	if (this->isLowerTriangle() != true) {
	if (!(this->isLowerTriangle())) {
		printf("\nMatrix::CholeskyInv >>> Matriz nao e triangular inferior\n");
		return false;
	}

	int i; double rf;
	for( i = 0; i < nrow; i++ ){
		if( this->mat[i][i] == (double)0. ){
			printf("\nMatrix::CholeskyInv >>> ERROR IV\n");
			return false;
		}
		mt.mat[i][i] = (double)1. / this->mat[i][i];
		int j,k;
		for( j = i-1; j >= 0; j-- ){
			rf = (double)0.;
			for( k = j; k <= i-1; k++ ){
				rf += ( this->mat[i][k] * mt.mat[k][j] );
			}
			mt.mat[i][j] = -rf * mt.mat[i][i];
		}
	}
	return true;
}
	

int
TeMatrix::EigenValues( TeMatrix& mt ) const
{
	//---	calcula os eigenvalues  de uma
	//	matriz representada na forma vetorial 
	//	Os auto valores sao devolvidos em ordem decrescente    
	//	e ocupam as posicoes:  0, 2, 5, 9, ... .       

	if( &mt == this ){
		printf("\nMatrix::EigenValues >>> Operacao usa duas matrizes\n");
		return false;
	}

double	*cov	= NULL,		/* matriz de covariancia	*/
	*e_val	= NULL;		/*   "    de auto_valores	*/

int 	dim = (*this).Nrow();		/* ordem da matriz*/

int	i,j,k,		/*   cov[], e_val[]    */
	ia,             /*                     */
	ind,            /*     |0      |       */
	l,m,            /*     |1 2    |       */
	mq,lm,          /*     |3 4 5  |       */
	ll,mm,          /*     |6 7 8 9|       */
	ilq,imq,        /*                     */
	im,iq,il,lq,    /*    (para DIM=4)     */
	jq;

double	range,          /*       e_vec[]       */
	dim1,fdim,      /*                     */  
	anorm,          /*    |0  4  8 12|     */
	anrmx,          /*    |1  5  9 13|     */
	thr,            /*    |2  6 10 14|     */
	x,y,z,          /*    |3  7 11 15|     */
	sinx,cosx,      /*                     */
	sinx2,cosx2,
	sincs;

int 	fat = (dim*dim+dim)/2;

	if( dim <= 0 ){
		printf("\nMatrix::EigenValues >>> dimensao da matriz nula!\n");
		return false;
	}
	if( mt.Init( dim, (double)1 ) == false ){
		printf("\nMatrix::EigenValues >>> Memoria nao disponivel\n");
		return false;
	}
	range = 1.0e-6 ;
	dim1 = dim - 1 ;
	fdim = dim;

	cov = new double[fat];

	if( cov == NULL ){
		printf("\nMatrix::EigenValues >>> Memoria nao disponivel (COV)\n");
		return false;
	}

	e_val = new double[fat];
	if( e_val == NULL ){
		delete []cov; //SSL0296
		cov = NULL;
		printf("\nMatrix::EigenValues >>> Memoria nao disponivel (EVAL)\n");
		return false;
	}
	k = 0;
	for ( i = 0; i< dim; i++)
		for ( j = 0; j <= i; j++)
			cov[k++] = mat[i][j];

	for (i=0; i < ((dim*dim+dim)/2); i++) e_val[i] = cov[i];

	anorm = 0.0;

	for ( j = 0; j < dim; j++){
		for (i = 0; i <= j; i++){
			if (i != j){
				ia = i + (j*j+j)/2;
				anorm = anorm + e_val[ia] * e_val[ia];
			}
		}
	}

	if (anorm <= 0) goto l5;

	anorm = 1.414 * sqrt((double)anorm);
	anrmx = anorm * range/fdim;
	ind   = 0;
	thr   = anorm;

l1:
	thr = thr/fdim;
l2:
	l = 0;
l3:
	m = l+1;
l4:
	mq = (m*m + m)/2;
	lq = (l*l + l)/2;
	lm = l + mq;

	if ( fabs((double)(e_val[lm])) >= thr )    
	{
		ind = 1;
		ll = l + lq;
		mm = m + mq;

		x = 0.5 * (e_val[ll] - e_val[mm]);
		z = e_val[lm] * e_val[lm] + x*x;
		y = - e_val[lm] / sqrt((double)(z));

		if (x < 0)  
		{
			y = -y;
		}

		z = sqrt( (double)(1.0-y*y) );
		sinx = y / sqrt( (double)( 2.0*(1.0 + z) ) );
		sinx2 = sinx * sinx;
		
		cosx = sqrt( (double)(1.0 - sinx2) );
		cosx2 = cosx * cosx;

		sincs = sinx * cosx;
	
		ilq = dim * l;
		imq = dim * m;

		for (i = 0; i < dim; i++)
		{
			iq = (i*i + i)/2;
			if ( i != l )
			{
				if (i != m)
				{
					if (i > m) 
						im = m + iq;
					else
						im = i + mq;
					if (i < l)
						il = i + lq;
					else
						il = l + iq;

					x = e_val[il] * cosx - e_val[im] * sinx;
					e_val[im] = e_val[il] * sinx + e_val[im] * cosx;
					e_val[il] = x;
				}
			}
		}
		x = 2.0 * e_val[lm] * sincs;
		y = e_val[ll] * cosx2 + e_val[mm] * sinx2 - x;
		x = e_val[ll] * sinx2 + e_val[mm] * cosx2 + x;

		e_val[lm] = (e_val[ll]-e_val[mm])*sincs+e_val[lm]*(cosx2-sinx2);
		e_val[ll] = y;
		e_val[mm] = x;
	}

	if (m != (dim-1))
	{
		m = m + 1;	goto l4;
	}
	if (l != (dim-2))
	{
		l = l + 1;	goto l3;
	}
	if (ind == 1)
	{
		ind = 0;	goto l2;
	}
	if (thr > anrmx) goto l1;

l5:
	iq = -dim;

	for (i = 0; i < dim; i++)
	{
		iq = iq + dim;
		ll = i + (i*i + i)/2;
		jq = dim * (i-1);

		for (j = i; j < dim; j++)
		{
			jq = jq + dim;
			mm = j + (j*j + j)/2;

			if (e_val[ll] < e_val[mm])
			{
				x = e_val[ll];
				e_val[ll] = e_val[mm];
				e_val[mm] = x;
			}
		}
	}

	for ( i = 0; i< dim; i++){
		mt(i,0) = e_val[(i*(i+1))/2+i];
	}

	delete []cov;	//SSL0296
	delete []e_val; //SSL0296

	return true;
}

//--------------------------------------------------EigenVectors
//
//	Metodo adaptado a partir de uma implementacao em C
//	utilizada no software SITIM.

int
TeMatrix::EigenVectors( TeMatrix& mt ) const
{
	if( &mt == this ){
		printf("\nMatrix::EigenVectors >>> Operacao usa duas matrizes\n");
		return false;
	}

double	*cov	= NULL,		/* matriz de covariancia	*/
	*e_val	= NULL,		/*   "    de auto_valores	*/
	*e_vec	= NULL; 	/*   "    de auto_vetores	*/

int 	dim = (*this).Nrow();		/* ordem da matriz*/

int	i,j,ij,k,       /*   cov[], e_val[]    */
	ia,             /*                     */
	ind,            /*     |0      |       */
	l,m,            /*     |1 2    |       */
	mq,lm,          /*     |3 4 5  |       */
	ll,mm,          /*     |6 7 8 9|       */
	ilq,imq,        /*                     */
	im,iq,il,lq,    /*    (para DIM=4)     */
	ilr,imr,
	jq;

double	range,          /*       e_vec[]       */
	dim1,fdim,      /*                     */  
	anorm,          /*    |0  4  8 12|     */
	anrmx,          /*    |1  5  9 13|     */
	thr,            /*    |2  6 10 14|     */
	x,y,z,          /*    |3  7 11 15|     */
	sinx,cosx,      /*                     */
	sinx2,cosx2,
	sincs;

	if( dim <= 0 ){
		printf("\nMatrix::EigenValues >>> dimensao da matriz nula!\n");
		return false;
	}
	if( mt.Init( dim, (double)1 ) == false ){
		printf("\nMatriz::EigenVectors >>> Memoria nao disponivel!\n");
		return false;
	}

	int fat =(dim*dim+dim)/2;
	range = 1.0e-6 ;
	dim1 = dim - 1 ;
	fdim = dim;

	cov   = new double[fat];
	e_vec = new double[dim*dim];
	e_val = new double[fat];

	if( cov == NULL || e_vec == NULL || e_val == NULL ){
		printf("\nMatrix::EigenVectors >>> Memoria nao disponivel\n");
		return false;
	}

	k = 0;
	for ( i = 0; i< dim; i++)
		for ( j = 0; j <= i; j++)
			cov[k++] = (*this)(i,j);

	for (i=0; i < ((dim*dim+dim)/2); i++) e_val[i] = cov[i];

	iq = -dim;
	for (i = 0; i < dim; i++)
	{
		iq = iq + dim;
		for ( j = 0; j < dim; j++)
		{
			ij = iq + j;
			if (i == j)
				e_vec[ij] = 1.0;
			else
				e_vec[ij] = 0.0;
		}
	}
 
	anorm = 0.0;

	for ( j = 0; j < dim; j++)
	{	
		for (i = 0; i <= j; i++)
		{
			if (i != j)
			{
				ia = i + (j*j+j)/2;
				anorm = anorm + e_val[ia] * e_val[ia];
			}
		}
	}

	if (anorm <= 0) goto l5;

	anorm = 1.414 * sqrt((double)anorm);
	anrmx = anorm * range/fdim;
	ind   = 0;
	thr   = anorm;

l1:
	thr = thr/fdim;
l2:
	l = 0;
l3:
	m = l+1;
l4:
	mq = (m*m + m)/2;
	lq = (l*l + l)/2;
	lm = l + mq;

	if ( fabs((double)(e_val[lm])) >= thr )    
	{
		ind = 1;
		ll = l + lq;
		mm = m + mq;

		x = 0.5 * (e_val[ll] - e_val[mm]);
		z = e_val[lm] * e_val[lm] + x*x;
		y = - e_val[lm] / sqrt((double)(z));

		if (x < 0)  
		{
			y = -y;
		}

		z = sqrt( (double)(1.0-y*y) );
		sinx = y / sqrt( (double)( 2.0*(1.0 + z) ) );
		sinx2 = sinx * sinx;
		
		cosx = sqrt( (double)(1.0 - sinx2) );
		cosx2 = cosx * cosx;

		sincs = sinx * cosx;
	
		ilq = dim * l;
		imq = dim * m;

		for (i = 0; i < dim; i++)
		{
			iq = (i*i + i)/2;
			if ( i != l )
			{
				if (i != m)
				{
					if (i > m) 
						im = m + iq;
					else
						im = i + mq;
					if (i < l)
						il = i + lq;
					else
						il = l + iq;

					x = e_val[il] * cosx - e_val[im] * sinx;
					e_val[im] = e_val[il] * sinx + e_val[im] * cosx;
					e_val[il] = x;
				}
			}

			//---	calculate eigenvectors

			ilr = ilq + i;
			imr = imq + i;
			x   = e_vec[ilr] * cosx - e_vec[imr] * sinx;
			e_vec[imr] = e_vec[ilr] * sinx + e_vec[imr] * cosx;
			e_vec[ilr] = x;
		}

		x = 2.0 * e_val[lm] * sincs;
		y = e_val[ll] * cosx2 + e_val[mm] * sinx2 - x;
		x = e_val[ll] * sinx2 + e_val[mm] * cosx2 + x;

		e_val[lm] = (e_val[ll]-e_val[mm])*sincs+e_val[lm]*(cosx2-sinx2);
		e_val[ll] = y;
		e_val[mm] = x;
	}

	if (m != (dim-1))
	{
		m = m + 1;	goto l4;
	}
	if (l != (dim-2))
	{
		l = l + 1;	goto l3;
	}
	if (ind == 1)
	{
		ind = 0;	goto l2;
	}
	if (thr > anrmx) goto l1;

l5:
	iq = -dim;

	for (i = 0; i < dim; i++)
	{
		iq = iq + dim;
		ll = i + (i*i + i)/2;
		jq = dim * (i-1);

		for (j = i; j < dim; j++)
		{
			jq = jq + dim;
			mm = j + (j*j + j)/2;

			if (e_val[ll] < e_val[mm])
			{
				x = e_val[ll];
				e_val[ll] = e_val[mm];
				e_val[mm] = x;

				for (k = 0; k < dim; k++)
				{
					ilr = iq + k;
					imr = jq + k;
					x   = e_vec[ilr];
					e_vec[ilr] = e_vec[imr];
					e_vec[imr] = x;
				}
			}
		}
	}
	
	k=0;
	for ( i = 0; i< dim; i++){
		for ( j = 0; j< dim; j++)
			mt(j,i) = e_vec[k++];
	}

	delete []cov;	//SSL0296
	delete []e_vec;	//SSL0296
	delete []e_val; //SSL0296

	return true;
}

//--------------------------------------------------EigenVec
//
//	Metodo desenvolvido para suporte de decisao (suporte_stubs.cpp)
//	Missae Yamamoto  (junho/1999)

int
TeMatrix::EigenVec( double e_vec[] )
{

double	*e_vec_aux	= NULL, soma = 0.0; 
	
int 	dim = (*this).Nrow();		/* ordem da matriz*/

TeMatrix  aux1, aux2, mt;

int	i,j,k;       
	
	if( dim <= 0 )
	{
		printf("\nMatrix::EigenVecVal >>> dimensao da matriz nula!\n");
		return false;
	}

	if( aux1.Init( dim, (double)1 ) == false )
	{
		printf("\nMatrix::EigenVecVal >>> Memoria nao disponivel!\n");
		return false;
	}

	if( aux2.Init( dim, (double)1 ) == false )
	{
		printf("\nMatrix::EigenVecVal >>> Memoria nao disponivel!\n");
		return false;
	}

	e_vec_aux = new double[dim];

	if( e_vec_aux == NULL )
	{
		printf("\nMatrix::EigenVecVal >>> Memoria nao disponivel\n");
		return false;
	}

	for ( k = 0; k< dim; k++)
	{
		e_vec_aux[k] = 0.0;
		e_vec[k] = 0.0;
	}

	aux1 = *this;
	aux2 = *this;
	
	for (;;)
	{
		mt = aux1 * aux2;

		for (i=0; i<dim; i++)
		{
			for (j=0; j<dim; j++)
				 e_vec[i] = e_vec[i] + mt(i,j);
			soma = soma + e_vec[i];
		}

		for (j=0; j<dim; j++)
			e_vec[j] = e_vec[j] / soma;

		for (j=0; j<dim; j++)
		{
			if ( fabs(e_vec_aux[j] - e_vec[j]) < 0.001 ) 
			{
				delete []e_vec_aux;	
				return true;
			}
			e_vec_aux[j] = e_vec[j];
		}

		aux1 = mt;
		aux2 = mt;
	}

}

double TeMatrix::getTrace() const
{
  TEAGN_TRUE_OR_THROW( ( nrow == ncol ), 
    "Cannot get trace from a non-square matrix" );
    
  double trace = 0.0;
  
  for( int curr_row = 0 ; curr_row < nrow ; ++curr_row ) {
    trace += mat[ curr_row ][ curr_row ];
  }
  
  return trace;
}


bool TeMatrix::getIdentity( TeMatrix& identity, unsigned int width )
{
  if( identity.Init( width, width, 0.0 ) == 0 ) return false;
  
  for( unsigned int curr_row = 0 ; curr_row < width ; ++curr_row ) {
    identity( curr_row, curr_row ) = 1.0;
  }
  
  return true;
}

//------------------------------------------ Print
void
TeMatrix::Print()
{
	//printf("\nMATRIZ ( %d, %d ) LIXO %f **mat %p\n", nrow,ncol, lixo, mat);
	printf("\nMATRIZ ( %d, %d )\n\n", nrow,ncol);
	if( mat == NULL ){
		printf("\n>>> mat e NULL \n");
		return;
	}
	int i,j;	
	for(i = 0; i < nrow; i++){
		for( j = 0; j < ncol;j++){
			printf("%10.4f ",(float)mat[i][j]);
		}
		printf("\n\n");
	}
}

// VRMC 12/98
int
TeMatrix::CholeskyDecomp( TeMatrix& mt )
{

	//--	Verify if the matrix is simetric
	if ((*this).isSimetric() == false){
		printf("\nMatrix::CholeskyDecomp >>> Matriz nao e simetrica!\n");
		return false;
	}
	//--	Verify if the matrix is positive definide
	if ((*this).isPositiveDefinide() == false){
		printf("\nMatrix::CholeskyDecomp >>> Matriz nao e positiva definida!\n");
		return false;
	}
	if( &mt == this ){
		printf("\nMatrix::CholeskyDecomp >>> Operacao usa duas matrizes\n");
		return false;
	}
	if( mat[0][0] <= 0. ){
		printf("\nMatrix::CholeskyDecomp >>> Posicao [0,0] e' nula\n");
		return false;
	}
    if( mt.Init(nrow,ncol) == false ){
		printf("\nMatrix::CholeskyDecomp>>>no memory \n");
		return false;
   	}
	mt(0,0) = (double)sqrt((double)(*this)(0,0));

	//---	Calculate the first column of the TeMatrix mt

	int i,j,k;
	for( i= 0; i < nrow; i++){
		mt(i,0) = (*this)(i,0)/mt(0,0);
	}
	 
	for( i= 1; i < nrow; i++){
		for( j= 1; j <= i; j++){
			//int m = (j*j - j)/2 + 1;
			double rf = 0.;
			//int k2 = j - 1;
			//for( k = 0;k<=k2;k++)
			for( k = 0;k < j;k++)
				rf += mt(i,k) * mt(j,k);
			if( i == j ){
				rf = (*this)(i,j) - rf;
				if( rf < 0. ){
					printf("\nMatrix::CholeskyDecomp:ERRO \n");
					return false;
				}
				mt(i,j) = (double)sqrt((double)rf);
			}
			else{
				if( mt(j,j) == 0. ){
					printf("\nMatrix::CholeskyDecomp:ERRO \n");
					return false;
				}
				mt(i,j) = ((*this)(i,j)-rf)/mt (j,j );
			}
		}
	}
	return true;
}


//-------------------------------------------------Inverse(SITIM)
//
//---	Calcula a matriz inversa pelo metodo de Gauss-Jordan.
//

int
TeMatrix::Inverse( TeMatrix& matinv ) const
{

	if( &matinv == this ){
		printf("\nMatrix::Inverse >>> Operacao usa duas matrizes\n");
		return false;
	}
	if ( nrow != ncol ){
       		printf("\nMatrix::Inverse: can't invert a non-square matrix");
       		return false;
   	 }

	if( matinv.Init( this->nrow, (double)1 ) == false ){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}

//---	Calculo da inversa se a matriz tem dimensao unitaria

	if(nrow == 1){
		if(mat[0][0] == 0.0) {
		        printf("\noperator~: can't invert singular matrix");
			return false;
		}
    		matinv(0,0) = 1. / mat[0][0];
    		return true;
	}
  
//---	Formacao da matriz aumentada mataum

	int	m2   = 2 * nrow ;
	TeMatrix	mataum;

	if( mataum.Init( nrow , m2 ) == false ){
	   printf("\nInverse:no memory");
	   return false;
       	}
	
	int i1,i2,i3;
	for(i1 = 0 ; i1 < nrow ; i1++){
		for( i2 = 0 ; i2 < ncol ; i2++)
        		mataum(i1,i2) = mat[i1][i2];

		for( i3 = nrow ; i3 < m2 ; i3++)
        		mataum(i1,i3) = 0.0;

		mataum( i1, i1 + nrow) = 1.0;
	}

//--	Inicializa ponteiro de linhas

	double	*maxlinha = NULL;

	if( ( maxlinha = new double[nrow] ) == NULL ){
		printf("\nMatrix::Inverse>>no memory");
		return false;
	}

	double	sup,lcon1,amul,pivo;
	int	* lp = NULL;

	if( (lp = new int[nrow]) == NULL ){
		printf("\nMatrix::Inverse>>no memory");
		if( maxlinha != NULL){delete []maxlinha; maxlinha = NULL;} //SSL0296
		return false;
	}

	for(i1 = 0 ; i1 < nrow ; i1++)
		lp[i1] = i1;

	int	lcon = 0;
	int	m1 = nrow - 1;

	while(lcon < nrow){
//---		Selecao do maior valor em cada linha . seu inverso e armazenado em maxlinha.
		for(i1 = 0 ; i1 < nrow ; i1++){
			sup = 0.0;
			for(i2 = 0 ; i2 < nrow ; i2++){
				lcon1 = (double)fabs(mataum(lp[i1],i2));
				if((lcon1 - sup ) > 0.000001 )
				sup = lcon1;
			}
			if(sup == 0.0){
				if( maxlinha != NULL){ delete []maxlinha; maxlinha = NULL; } //SSL0296
				if( lp != NULL){ delete []lp; lp = NULL; } //SSL0296
				return false;
			}
			maxlinha[lp[i1]] = 1.0 / sup;
		 }

//---		Selecao do pivo em cada linha

		int lpi;
		double	supc = fabs((double)( mataum(lp[lcon],lcon) * maxlinha[lp[lcon]] ));
		pivo = mataum(lp[lcon],lcon);
		lpi  = lcon;
		for(i1 = lcon ; i1 < nrow ; i1++){
        		amul = mataum(lp[i1],lcon) * maxlinha[lp[i1]];
			if (fabs(amul) > supc ){
			supc = (fabs((double)amul));
			pivo = mataum(lp[i1],lcon);
			lpi  = i1;
			}
		}

		if(pivo == 0.0){
			if( maxlinha != NULL){ delete []maxlinha; maxlinha = NULL; } //SSL0296
			if( lp != NULL){ delete []lp; lp = NULL; } //SSL0296
			return false;
		}
     
		/* troca de linhas */
		i1 	 = lp[lcon];
		lp[lcon] = lp[lpi];
		lp[lpi]  = i1;
	
//---		Divide a linha pelo pivo
		for(i2 = 0 ; i2 < m2 ; i2++){
			mataum(lp[lcon],i2) = mataum(lp[lcon],i2) / pivo ;
		}

//---		Realiza subtracoes nas linhas de forma a zerar os elementos da coluna
//---		dada pelo indice lcon

		int	il = lcon;

		for(i2 = 0; i2 < m1 ; i2++){
	        	il = il + 1;
			if(il >= nrow ) il = 0;
			double	aux = - mataum(lp[il],lcon);
			for(i3 = lcon; i3 < m2 ; i3++)
				mataum(lp[il],i3) = mataum(lp[il],i3) + aux * mataum(lp[lcon],i3);

			mataum(lp[il],lcon) = 0.0;
		}

		lcon = lcon + 1;
	}

//---	Copia a parte extendida de mataum em matinv que passa a ser a matriz inversa
	for(i1 = 0 ; i1 < nrow ; i1++){
		for(i2 = nrow ; i2 < m2 ; i2++)
		{
        		i3 = i2 - nrow ;
			matinv(i1,i3) = mataum(lp[i1],i2);
		}
	}

	if( maxlinha != NULL){
		delete []maxlinha; //SSL0296
		maxlinha = NULL;
	}
	if( lp != NULL){
		delete []lp; //SSL0296
		lp = NULL;
	}

	return true;
}

//	Esse mtodo, no esta correto 
//------------------------------------------Inverse
//	NIH library
// 1. triangulate: *this = ~P*T
// 2. when T isUpperTriangle ~T isUpperTriangle
// 3. split: T = T.row(0) + subT
// 4. I = ~T*T
// 5. ~T.at(0,0) = 1/T.at(0,0)
// 6. sub~T = ~(subT)
// 7. ~T.row(0) = [1/T.at(0,0)]&B
//    where T.at(0,0)*B = [t21 ... t2n]*~subT
// 8. ~*this = ~T*P

/*int 
TeMatrix::InverseNIH( TeMatrix& mt ) const
{
	TeMatrix  r;
	TeMatrix	_subT;
	TeMatrix	B;
	TeMatrix	val;

	if ( nrow!=ncol || nrow <= 0 || ncol <= 0 ){
		printf("\nMatrix::Inverse >>> Matriz nao inversivel!\n");
		return false;
	}

	if( mt.Init( this->nrow, (double)1 ) == false ){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}

	if ( nrow==1 ){
		if( mat[0][0] == (double)0 ){
			printf("\nMatrix::Inverse >>> Matriz nao inversivel!\n");
			return false;
		}
		mt.mat[0][0] = 1/mat[0][0];
		return true;
   	}
	TeMatrix T(*this);
	if( T.Initialized() == false ){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}
		
	// 1. triangulate: *this = ~P*T
	TeMatrix P;
	if( P.Init(nrow,ncol) == false){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}
	if( T.UpperTriangle( P ) == false ){
		printf("\nMatriz::Inverse >>> ERRO!\n");
		return false;
	}
	if ( T.Determinant()==0 ) {
		printf("\nMatrix::Inverse >>> can't invert singular matrix\n");
		return false;
	}      

	// 2. when T isUpperTriangle ~T isUpperTriangle
	// 3. split: T = T.row(0) + subT

	if( r.Init(1,ncol-1,&(T.mat[0][1])) == false){
		printf("\nMatrix::Inverse* >>> Memoria nao disponivel\n");
		return false;
	}

	T.CoFactor(0,0,mt);
	mt.Inverse(_subT);
	if( _subT.Initialized()  == false){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}


	B = (-(1/T.mat[0][0])*r*_subT);
	if( B.Initialized() == false ){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}

	if( val.Init(nrow,ncol) == false){
		printf("\nMatrix::Inverse >>> Memoria nao disponivel\n");
		return false;
	}

	val.mat[0][0] = 1/T.mat[0][0];
	int i;
	for(i=1; i<ncol; i++) val.mat[0][i]=B.mat[0][i-1];
	val.CoFactor(0,0,_subT);

 	mt = val*P; // P is now the row-reduction transformation

	return true; 
}
*/

// os mtodos switchRows e combineRows ficarao comentados 
// pois s sao utilizados no calculo da matriz inversa NIH,
// inversaNIH, que tambem foi comentado 
/*void 
TeMatrix::switchRows(int i,int j)
{
	double *tmp;

	if( i < 0 || j < 0 || i >= nrow || j >= ncol ){
		printf("\nMatrix::switchRows >>> Switching linhas invalidas\n");
		return;
	}
	tmp    = mat[i];
	mat[i] = mat[j];
	mat[j] = tmp; 
}

void
TeMatrix::combineRows(int i, double a, int j)
{
	if( i < 0 || j < 0 || i >= nrow || j >= ncol ){
		printf("\nMatrix::combineRows >>> Combining linhas invalidas\n");
		return;
	}
	int h;
	for(h = 0; h < ncol; h++)
	mat[i][h] += a * mat[j][h];
}
*/

//--------Esse metodo nao esta correto e por isso ficar comentado VRMC
//----------------------------------------------------
/*int 
TeMatrix::UpperTriangle( TeMatrix& mt )
{
	if( &mt == this ){
		printf("\nMatrix::UpperTriangle >>> Operacao usa duas matrizes\n");
		return false;
	}
	if( mt.Init(nrow,(double)1) == false){
		printf("\nMatrix::UpperTriangle >>> Memoria nao disponivel\n");
		return	false;
	}	

	if ( this->isUpperTriangle() )
		return true;
	
	int j;
	for(j = 0; j < ncol; j++) {
		int b_row = nrow-1;  // 1st non-zero entry from bottom
		int t_row = j;       // 1st zero entry from the top

		//--- switch rows until all zeros are at
		//--- the bottom of jTH column 

		while ( b_row>=t_row ) { 
			while ( b_row > j && mat[b_row][j] == 0. )
				--b_row;
			if ( b_row == j )
				break; // bottom at diagonal
			while ( b_row >= t_row && mat[t_row][j] != 0. )
				++t_row;
			if ( t_row == Nrow() )
				break; // top at last row
			if ( t_row > b_row )
				break;
			switchRows(b_row,t_row); 
			mt.switchRows(b_row,t_row);
		}

		//--- now b_row is last non-zero entry from the top
		//--- now t_row is first zero entry from the bottom
		//--- combine until all entries below diagonal in jTH column = 0

		if ( b_row <= j )
			continue;
			int i;
		for( i = j+1; i<= b_row; i++) {
			double f = -mat[i][j] / mat[j][j];
			combineRows(i,f,j);
			mt.combineRows(i,f,j);
		}
	}
	return true;
}
*/
