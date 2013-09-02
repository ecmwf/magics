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
