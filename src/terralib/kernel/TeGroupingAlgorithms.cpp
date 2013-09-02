/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeGroupingAlgorithms.h"

void
TeGroupByUniqueValue(vector<string>& vec, TeAttrDataType tipo, vector<TeSlice>& result, int precision)
{
	unsigned int i, j;
		
	if (tipo == TeINT)
	{
		vector<int> v;
		for (i = 0; i < vec.size(); ++i)
			v.push_back(atoi(vec[i].c_str()));
		sort(v.begin(), v.end());

		for (i = 0; i < v.size(); ++i)
			vec[i] = Te2String(v[i]);
	}
	else if (tipo == TeREAL)
	{
		vector<double> v;
		for (i = 0; i < vec.size(); ++i)
		{
			double a = atof(vec[i].c_str());
			v.push_back(a);
		}
		stable_sort(v.begin(), v.end());

		for (i = 0; i < v.size(); ++i)
			vec[i] = Te2String(v[i], precision);
	}
	else
	{
		sort(vec.begin(), vec.end());
	}

	// Check the elements that are equal, incrementing
	// the variable count associated to each one
	int count = 1;
	TeSlice slice;
	unsigned int sz = vec.size();
	for (i = 0, j = 1; i < sz - 1 && j < sz; ++i, ++j)
	{
		if (vec[i] == vec[j])
			++count;
		else
		{
			slice.from_ = vec[i];
			slice.count_ = count;
			result.push_back(slice);
			count = 1;
		}
	}

   if ( (i>1) && (vec[i] == vec[i-1]) ) 
   {
       slice.from_ = vec[i];
       slice.count_ = count;
       result.push_back(slice);
   }
   else
   {
       slice.from_ = vec[i];
       slice.count_ = 1;
       result.push_back(slice);
   } 
}

void
TeGroupByEqualStep(double min, double max, int nstep, vector<TeSlice>& result, int precision)
{
	double slice = (max - min)/double(nstep);
	int ns;
	for (ns=0;ns<nstep;ns++)
	{
		TeSlice ps;
		ps.count_ = 0;
		ps.from_ = Te2String(min+double(ns)*slice, precision);
		ps.to_ = Te2String(min+double(ns+1)*slice, precision);
		result.push_back(ps);
	}
	min = TeAdjustToPrecision(min, precision, true);
	result[0].from_ = Te2String(min, precision);
	max = TeAdjustToPrecision(max, precision);
	result[result.size()-1].to_ = Te2String(max, precision);
}
