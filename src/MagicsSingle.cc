/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*

   API for single precision versions of Magics++
   

*/
#include <iostream>
#include <math.h>
#include <stdio.h>
extern "C"
{
#include <magics_api.h>


void psetr_(char* name, float* value, int length)
{
	std::string n(name, length);
	double dval = *value;
	// Here we try to improve the conversion to double for small numbers.

	if ( *value < 1.0 && *value > -1.0 ) {
		int val = *value * 10000000;
		dval = val/10000000.;
	}



	mag_setr(n.c_str(), dval);
}

void pset1r_(char* name, float* data, int* dim, int length)
{
	std::string n(name, length);
	double* da = new double [*dim];
	for(int i=0;i<*dim;i++) da[i] = (double)data[i];

	mag_set1r(n.c_str(), da, *dim);
	delete [] da;
}

void pset2r_(char* name, float* data, int *dim, int *dim2, int length)
{    
	std::string n(name, length);
	const long no = (*dim)*(*dim2); 
	double* da = new double [no];
	for(int i=0;i<no;i++) da[i] = (double)data[i];

	mag_set2r(n.c_str(), da, *dim, *dim2);
	delete [] da;
}

void pset3r_(char* name, float* data, int* dim, int *dim2, int* dim3, int length)
{    
	std::string n(name, length);
	const long no = (*dim)*(*dim2)*(*dim3);
	double* da = new double [no];
	for(int i=0;i<no;i++) da[i] = (double)data[i];

	mag_set3r(n.c_str(), da, *dim, *dim2, *dim3);
	delete [] da;
}

void penqr_(const char* name, float* value, int length)
{	
	std::string n(name, length);
	double tmp;
	
	mag_enqr( n.c_str(), &tmp);
	*value = float(tmp);
}


}
