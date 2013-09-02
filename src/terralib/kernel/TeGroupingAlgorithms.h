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
/*! \file TeGroupingAlgorithms.h
    \brief This file contains some generic grouping algorithms (based on iterators)
*/

#ifndef  __TERRALIB_INTERNAL_GROUPINGALGORITHMS_H
#define  __TERRALIB_INTERNAL_GROUPINGALGORITHMS_H

#include "TeDefines.h"
#include "TeSlice.h"
#include "TeUtils.h"
#include "TeDataTypes.h"

#include <math.h>
#include <time.h>
#include <algorithm>


//! Finds the element with minimum value among the elements contained in a range of iterators
template<typename It>
void TeMinimumValue(It begin, It end, vector<double>& minValue, double dummy=-9999.99, bool usesDummy=false)
{
	for (int i=0; i<minValue.size(); i++)
		minValue[i] = TeMAXFLOAT;

	It it = begin;
	double val;
	while ( it != end) 
	{
		if (!(usesDummy && dummy == val))
			for (int i=0; i<minValue.size(); i++)
			{
				val= (*it)[i];
				if (val < minValue[i])
					minValue[i] = val;
			}
		it++;
	}
}

//! Finds the element with maximum value among the elements contained in a range of iterators
template<typename It>
void TeMaximumValue(It begin, It end, vector<double>& maxValue, double dummy=-9999.99, bool usesDummy=false)
{
	for (int i=0; i<maxValue.size(); i++)
		maxValue[i] = -TeMAXFLOAT;

	It it = begin;
	double val;
	while ( it != end) 
	{
		if (!(usesDummy && dummy == val))
			for (int i=0; i<maxValue.size(); i++)
			{
				val= (*it)[i];
				if (val > maxValue[i])
					maxValue[i] = val;
			}
		it++;
	}
}

//! Defines the classes (slices) of a equal step grouping
TL_DLL void TeGroupByEqualStep(double min, double max, int nstep, vector<TeSlice>& result, int precision=0);

/** @defgroup GenGroupAlg Generic Algorithms to do grouping
 *  A set of of generic functions to do grouping
 *  @{
 */

//! Groups a set of elements defined by a range of iterators in nstep groups, using Equal Step algorithm
template<class iterator>
void TeGroupByEqualStep(iterator begin, iterator end, int nstep, vector<TeSlice>& result,
				   int precision=0, bool countElements = true)
{
	double	min = TeMAXFLOAT;
	double	max = -TeMAXFLOAT;

	iterator it=begin;
	while(it < end)
	{
		min = MIN(min, *it);
		max = MAX(max, *it);
		it++;
	}
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

	// Set the number of elements for each slice
	if (countElements == true)
		TeElemCountingBySlice(begin, end, result);
}

//! Groups a set of elements defined by a range of iterators in nstep groups, using Quantil algorithm
template<class iterator>
void TeGroupByQuantil(iterator begin, iterator end, int nstep, vector<TeSlice>& result,
				 int precision = 0, bool countElements = true)
{
	sort(begin, end);

	int size = end - begin;
	double	step = (double)size / (double)nstep;

	int	n = 0;
	iterator it = begin;
	while(it < end)
	{
		TeSlice	ps;
		ps.from_ = Te2String((*it), precision);
		int p = (int)(step * (double)++n + .5);
		it = begin + p;
		if(it < end)
			ps.to_ = Te2String((*it), precision);
		else
			ps.to_ = Te2String(*(it-1), precision);
		result.push_back(ps);
	}
	if(end-begin > 1)
	{
		double min = (*begin);
		double max = (*(end-1));
		min = TeAdjustToPrecision(min, precision, true);
		result[0].from_ = Te2String(min, precision);
		max = TeAdjustToPrecision(max, precision);
		result[result.size()-1].to_ = Te2String(max, precision);
	}

	// Set the number of elements for each slice
	if (countElements == true)
		TeElemCountingBySlice(begin, end, result);
}

//! Groups a set of elements defined by a range of iterators in ndev groups, using Standanrd deviation algorithm
template<class iterator>
void TeGroupByStdDev(iterator begin, iterator end, double ndev, vector<TeSlice>& result, string& rmean,
				int precision = 0, bool countElements = true)
{
	// Compute mim, max and mean
	double	min = TeMAXFLOAT;
	double	max = -TeMAXFLOAT;
	long double	sum=0.;
	long double	sm2=0.;
	iterator it=begin;
	while(it < end)
	{
		min = MIN(min, *it);
		max = MAX(max, *it);
		sum += (*it);
		sm2 += ((*it) * (*it));
		it++;
	}
	double cont = (double)(end - begin);
	double	mean = (double)(sum/cont);
	long double var = (sm2 / cont) - (mean * mean);
	double	sdev = sqrt(var);

	double	slice = sdev * ndev;

	vector<TeSlice>	aux;
	rmean = Te2String(mean, precision);
	double	val = mean;
	while(val-slice > min-slice)
	{
		TeSlice ps;
		double v = val - slice;
		ps.from_ = Te2String(v, precision);
		ps.to_ = Te2String(val, precision);
		aux.push_back(ps);
		val = v;
	}

	if(aux.size())
	{
		typename vector<TeSlice>::iterator sit = aux.end() - 1;
		while(sit >= aux.begin())
			result.push_back(*sit--);
	}

	string media = "mean = " + rmean;
	TeSlice ps;
	ps.from_ = media;
	ps.to_.clear();
	result.push_back(ps);
	val = mean;
	while(val+slice < max+slice)
	{
		TeSlice ps;
		double	v = val + slice;
		ps.from_ = Te2String(val, precision);
		ps.to_ = Te2String(v, precision);
		result.push_back(ps);
		val = v;
	}
	if(result.size() > 2)
	{
		if (result[0].from_.find("mean")  == string::npos)
		{
			min = TeAdjustToPrecision(min, precision, true);
			result[0].from_ = Te2String(min, precision);
		}
		if (result[result.size()-1].from_.find("mean")  == string::npos)
		{
			max = TeAdjustToPrecision(max, precision);
			result[result.size()-1].to_ = Te2String(max, precision);
		}
	}

	// Set the number of elements for each slice
	if (countElements == true)
		TeElemCountingBySlice(begin, end, result);
}


//! Counts the number of elements, from a general container, per slice of a vector of slices
template<class iterator>
void TeElemCountingBySlice(iterator begin, iterator end, vector<TeSlice>& result)
{
	iterator it;
	double from, to;

	for (unsigned int i = 0; i < result.size(); ++i)
	{
		TeSlice& sl = result[i];
		sl.count_ = 0;
		from = atof(sl.from_.c_str());
		to = atof(sl.to_.c_str());
		for (it = begin; it != end; ++it)
		{
			if (*it >= from && *it < to)
				++sl.count_;
		}
	}
}


//! Groups a set of elements in a vector of string  using Unique Value algorithm
TL_DLL void TeGroupByUniqueValue(vector<string>& vec, TeAttrDataType tipo, vector<TeSlice>& result, int precision);

/** @} */ 
#endif





 


