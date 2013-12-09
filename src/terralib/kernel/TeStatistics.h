/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeStatistics.h
    \brief This file contains functions for calculate statistics  
*/

#ifndef TeStatistics_H
#define TeStatistics_H

#include "TeDataTypes.h"

#include <string>
#include <map>

using namespace std;

/** @defgroup Stat Statistical functions
  @{
 */
//! vector of statistics
typedef vector<double>	stats;

//! Associate a statistics set for each dimension 
struct TL_DLL TeStatisticsDimension
{
	int					dimension_;
	TeStatisticValMap	stat_;

	TeStatisticsDimension(int d, TeStatisticValMap& st): 
	dimension_(d), 
	stat_(st)
	{}
};

//! vector of the statistics associated with the dimensions 
typedef vector<TeStatisticsDimension>	TeStatisticsDimensionVect;

//! map each value to its count: histogram
typedef map<double, int> TeHistogram;


/** Calculates the histrogram of a set of values kept in a data structure 
	\param itBegin	the iterator to the begin of the data structure 
	\param itEnd	the iterator to the end of the data structure
	\param histOut	to return the histogram  
*/
template<typename T>
bool TeCalculateHistogram(const T& itBegin, const T& itEnd, TeHistogram& histOut)  
{
	T it = itBegin;

	//initialization 
	while(it!=itEnd)
	{
		double val = (*it); 
		histOut[val] = 0;
		it++;
	}
	
	it= itBegin;
	
	//calcule histogram
	while(it!=itEnd)
	{
		double val = (*it); 
		histOut[val]++;
		it++;
	}
	
	return true;
}

/** Calculates the statistics of a specific dimension of a data structure 
	\param itBegin	the iterator to the begin of the data structure
	\param itEnd	the iterator to the end of the data structure
	\param stat		to return the statistics  
	\param dim		the dimension of the data structure  
*/
template<typename It>  
bool TeCalculateStatistics(const It& itBegin, const It& itEnd, TeStatisticValMap& stat, int dim)
{
	double	sum, mean, minValue, maxValue, variance, assim, 
			curtose, stDev, coefVar, amplitude, moda, median;  
	sum=mean=variance=assim=curtose=stDev=coefVar=amplitude=0.0;
	
	minValue = TeMAXFLOAT;
	maxValue = TeMINFLOAT;

	vector<double> values;
	int count=0;
	int countTotal = 0;
	It itt = itBegin;	
	while(itt != itEnd)
    {
		double val;
		val = (*itt)[dim];
		
		if(val!=TeMAXFLOAT)  // MAXFLOAT is considered invalid value
		{
			values.push_back (val);
			sum += val;
		
			if(minValue>val)
				minValue = val;
			if(maxValue<val)
				maxValue = val;
			
			++count;
			mean = sum/count;
		}
		
		++itt;
		++countTotal;
	}

	for(int i=0; i<count; i++)
	{
		double v= values[i];
		variance += pow((v-mean),2); 
		assim += pow((v-mean),3);   
		curtose += pow((v-mean),4); 
	}
	
	if(!count)
		return false;

	variance /= count; 
	stDev = pow(variance,0.5); 
	assim /= count;
	assim /= pow(stDev,3); 
	curtose /= count;
	curtose /= pow(stDev,4); 

	coefVar = (100*stDev)/mean;
	amplitude = maxValue-minValue;  

	std::sort(values.begin(), values.end());
		
	//calculate median 
	if((count%2)==0)
		median = (values[(count/2)]+values[(count/2-1)])/2;
	else
		median = values[(count-1)/2];

	//calculate the mode
	TeHistogram histog;
	TeCalculateHistogram(values.begin(),values.end(), histog);
		
	TeHistogram::iterator itHist = histog.begin();
	int nCount=0;
	while(itHist!=histog.end())
	{
		int hCount = itHist->second; 
		if (hCount > nCount)
		{
			nCount = hCount;
			moda = itHist->first;
		}
		itHist++;	
	}
	
	//fill results
	stat[TeCOUNT] = countTotal; 
	stat[TeVALIDCOUNT] = count;
	stat[TeMINVALUE] = minValue;
	stat[TeMAXVALUE] = maxValue;
	stat[TeSUM] = sum;
	stat[TeMEAN] = mean;
	stat[TeSTANDARDDEVIATION] = stDev;
	stat[TeVARIANCE] = variance;
	stat[TeSKEWNESS] = assim;
	stat[TeKURTOSIS] = curtose;
	stat[TeAMPLITUDE] = amplitude;
	stat[TeMEDIAN] = median;		
	stat[TeVARCOEFF] = coefVar;		
	stat[TeMODE] = moda;
 
	return true;
}


/**  Calculates the statistics of a string data structure 
	\param itBegin	the iterator to the begin of the data structure
	\param itEnd	the iterator to the end of the data structure
	\param stat		to return the statistics  
*/
template<typename It>  
bool TeCalculateStatistics(const It& itBegin, const It& itEnd, TeStatisticStringValMap& stat)
{
	string min, max, val;
	int	count = 0;

	It it = itBegin;
	int totalCount = 0;

	if(it != itEnd)
		min = max = (*it);
	while(it != itEnd)
	{
		val = (*it);
		if(val.empty() == false)
		{
			count++;
			min = MIN(min, val);
			max = MAX(max, val);
		}
		++it;
		++totalCount;
	}

	stat[TeMINVALUE] = min;
	stat[TeMAXVALUE] = max;
	stat[TeCOUNT] = Te2String(totalCount); 
	stat[TeVALIDCOUNT] = Te2String(count); 
	return true;
}


/** Calculates the statistics of a data structure 
	\param itBegin	the iterator to the begin of the data structure
	\param itEnd	the iterator to the end of the data structure
	\param stat		to return the statistics  
*/
template<typename It>  
bool TeCalculateStatistics(const It& itBegin, const It& itEnd, TeStatisticValMap& stat)
{
	double	sum, mean, minValue, maxValue, variance, assim, 
			curtose, stDev, coefVar, amplitude, moda, median;  
	sum=mean=variance=assim=curtose=stDev=coefVar=amplitude=moda=0.0;
	
	minValue = TeMAXFLOAT;
	maxValue = TeMINFLOAT;

	vector<double>	values;

	int	totalCount = 0;
	int count=0;
	It itt = itBegin;	
	while(itt != itEnd)
    {
		double val = (*itt);
		if(val != TeMAXFLOAT) // MAXFLOAT is considered invalid value
		{
			values.push_back (val);
			sum += val;
			minValue = MIN(minValue, val);
			maxValue = MAX(maxValue, val);
		}
		
		++itt;
		++totalCount;
	}

	count = values.size();
	mean = sum/count;

	for(int i=0; i<count; i++)
	{
		double v= values[i];
		variance += pow((v-mean),2); 
		assim += pow((v-mean),3);   
		curtose += pow((v-mean),4); 
	}
	
	if(!count)
		return false;

	variance /= count; 
	stDev = pow(variance,0.5); 
	assim /= count;
	assim /= pow(stDev,3); 
	curtose /= count;
	curtose /= pow(stDev,4); 

	coefVar = (100*stDev)/mean;
	amplitude = maxValue-minValue;  

	std::sort(values.begin(), values.end());
		
	//calculate median 
	if((count%2)==0)
		median = (values[(count/2)]+values[(count/2-1)])/2;
	else
		median = values[(count-1)/2];

	//calculate the mode
	TeHistogram histog;
	TeCalculateHistogram(values.begin(),values.end(), histog);
		
	TeHistogram::iterator itHist = histog.begin();
	int nCount=0;
	while(itHist!=histog.end())
	{
		int hCount = itHist->second; 
		if (hCount > nCount)
		{
			nCount = hCount;
			moda = itHist->first;
		}
		itHist++;	
	}
	
	//fill results
	stat[TeVALIDCOUNT] = count; 
	stat[TeCOUNT] = totalCount; 
	stat[TeMINVALUE] = minValue;
	stat[TeMAXVALUE] = maxValue;
	stat[TeSUM] = sum;
	stat[TeMEAN] = mean;
	stat[TeSTANDARDDEVIATION] = stDev;
	stat[TeVARIANCE] = variance;
	stat[TeSKEWNESS] = assim;
	stat[TeKURTOSIS] = curtose;
	stat[TeAMPLITUDE] = amplitude;
	stat[TeMEDIAN] = median;		
	stat[TeVARCOEFF] = coefVar;		
	stat[TeMODE] = moda;
 
	return true;
}


/** Calculates the statistics of all dimensions of a data structure 
	\param itBegin	the iterator to the begin of the data structure
	\param itEnd	the iterator to the end of the data structure
	\param stat		to return the statistics  
*/
template<typename It>  
bool TeCalculateStatistics(It& itBegin, It& itEnd, TeStatisticsDimensionVect& stat)
{
	
	vector<double>	sum, mean, minValue, maxValue, variance, assim, curtose, 
					stDev, coefVar, amplitude, moda, median;  
	
	int nb = itBegin.nBands();
	
	//initialization of the vetors
	for (int i=0; i<nb; i++)
	{
		sum.push_back(0.0); 
		mean.push_back(0.0); 
		variance.push_back(0.0);
		assim.push_back(0.0); 
		curtose.push_back(0.0); 
		stDev.push_back(0.0);
		coefVar.push_back(0.0);	
		amplitude.push_back(0.0);
		median.push_back (0.0);
		moda.push_back (0.0);
		minValue.push_back(TeMAXFLOAT);
		maxValue.push_back(TeMINFLOAT);
	}
			
	map<int,stats> bandValues;
	
	int count=0;
	It itt = itBegin;	
	
	while(itt != itEnd)
    {
		for (int j=0; j<nb; j++)
		{
			double val = itt.operator* (j);
			bandValues[j].push_back(val);
			sum[j] += val;
			
			if(minValue[j]>val)
				minValue[j] = val;
			if(maxValue[j]<val)
				maxValue[j] = val;

			int size = bandValues[j].size();
			mean[j] = sum[j]/size;
		}
		
		++itt;
		++count;
	}

	if(!count)
		return false;

	for (int jj=0; jj<nb; jj++)
	{
		for(int i=0; i<count; i++)
		{	
			double v = bandValues[jj][i];
			variance[jj] += pow((v-mean[jj]),2);
			assim[jj] += pow((v-mean[jj]),3);
			curtose[jj] += pow((v-mean[jj]),4);
		}

		variance[jj] /= count;
		stDev[jj] = pow(variance[jj],0.5);
		assim[jj] /= count;
		assim[jj] /= pow(stDev[jj],3);
		curtose[jj] /= count;
		curtose[jj] /= pow(stDev[jj],4);

		coefVar[jj] = (100*stDev[jj])/mean[jj];
		amplitude[jj] = maxValue[jj]-minValue[jj];

		std::sort(bandValues[jj].begin(), bandValues[jj].end());
		
		//calculate median
		if((count%2)==0)
			median[jj] = ((bandValues[jj][(count/2)])+(bandValues[jj][(count/2-1)]))/2;
		else
			median[jj] = bandValues[jj][(count-1)/2];

		//calculate the mode
		TeHistogram histog;
		TeCalculateHistogram(bandValues[jj].begin(),bandValues[jj].end(), histog);
			
		TeHistogram::iterator itHist = histog.begin();
		int nCount=0;
		while(itHist!=histog.end())
		{
			int hCount = itHist->second; 
			if (hCount > nCount)
			{
				nCount = hCount;
				moda[jj] = itHist->first;
			}
			itHist++;	
		}
			
		//fill results
		TeStatisticValMap statVal;

		statVal[TeCOUNT] = count; 
		statVal[TeMINVALUE] = minValue[jj];
		statVal[TeMAXVALUE] = maxValue[jj];
		statVal[TeSUM] = sum[jj];
		statVal[TeMEAN] = mean[jj];
		statVal[TeSTANDARDDEVIATION] = stDev[jj];
		statVal[TeVARIANCE] = variance[jj];
		statVal[TeSKEWNESS] = assim[jj];
		statVal[TeKURTOSIS] = curtose[jj];
		statVal[TeAMPLITUDE] = amplitude[jj];
		statVal[TeMEDIAN] = median[jj];
		statVal[TeVARCOEFF] = coefVar[jj];
		statVal[TeMODE] = moda[jj];

		TeStatisticsDimension statBand(jj, statVal);
		stat.push_back(statBand);
	}
	
	return true;
}


//!	Returns the sum of the elements of a specific dimension 
template <typename It> 
double TeSum (It begin, It end, int dim)
{
	double sum = 0.;
	while ( begin != end)
	{
		if(((*begin)[dim])!= TeMAXFLOAT) // MAXFLOAT is considered invalid value
			sum += (*begin)[dim];
		
		++begin;
	}
	return sum;
}


//!	Return the sum of the elements
template <typename It> 
double TeSum (It begin, It end)
{
	double sum = 0.;
	while ( begin != end)
	{
		if((*begin)!= TeMAXFLOAT) // MAXFLOAT is considered invalid value
			sum += (*begin);
		
		++begin;
	}
	return sum;
}


//! Calculates the mean (first moment) of the elements of a specific dimension
template <typename It> 
double TeFirstMoment (It begin, It end, int dim)
{
	int number = 0;
	double mean = 0.;
	while (begin != end)
	{
		if(((*begin)[dim])!= TeMAXFLOAT) // MAXFLOAT is considered invalid value
		{
			mean += (*begin)[dim];
			number++; 
		}
		++begin;
	}
	return mean /= number;
}


//! Calculates the mean (first moment) of the elements
template <typename It> 
double TeFirstMoment (It begin, It end)
{
	int number = 0;
	double mean = 0.;
	while (begin != end)
	{
		if((*begin)!= TeMAXFLOAT) // MAXFLOAT is considered invalid value
		{
			mean += (*begin);
			number++; 
		}
		begin++;
	}
	return mean /= number;
}


//! Calculates the second moment
template <typename It> 
double TeSecondMoment (It begin, It end, double mean)
{
	int number = 0;
	double SSD = 0;  //sum of squares of desviation

	while (begin != end)
	{
		if((*begin)!= TeMAXFLOAT) // MAXFLOAT is considered invalid value
		{
			SSD += pow(( (*begin) - mean), 2);
			number++; 
		}
		begin++;
	}
	return SSD /= number;
}


//! Calculates the second moment
template <typename It> 
double TeSecondMoment (It begin, It end, double mean, int dim)
{
	int number = 0;
	double SSD = 0;  //sum of squares of desviation

	while (begin != end)
	{
		if(((*begin)[dim])!= TeMAXFLOAT) // MAXFLOAT is considered invalid value
		{
			SSD += pow(((*begin)[dim] - mean), 2);
			number++; 
		}
		++begin;
	}
	return SSD /= number;
}



//!	calculates the vector with the means of the attributs
template <typename It, typename T> 
bool TeMeanVector ( It begin, It end, T& result)
{
	int i = 0;
	int dim = result.size();
	T soma;

	for (int m = 0; m < dim ; m++)
		soma.push_back(0.);

	while (begin != end)
	{
		for( int j = 0; j < dim; j++)
		{
			if(((*begin)[j]) != TeMAXFLOAT) // MAXFLOAT is considered invalid value 
			{
				soma[j]+= (*begin)[j];
				i++;
			}
			else
				soma[j]= TeMAXFLOAT;
		}
		begin++;
	}
	
	for (int k = 0; k < dim; k++)
	{
		if(soma[k] != TeMAXFLOAT) 
			result[k] = soma[k]/i;
		else
			result[k] = TeMAXFLOAT;
	}

	return true;
}

//! Calculates the deviations in relation to mean
template <typename It> 
bool TeDeviation ( It begin, It end, It result, double mean)
{
	while (begin != end)
	{
		if((*begin) != TeMAXFLOAT) // MAXFLOAT is considered invalid value 
			*result = double(*begin) - mean;
		else
			*result = TeMAXFLOAT;

		begin++; result++;
	}
	return true;
}


//! calculates the deviations in relation to mean
template <typename iteratorSet> 
bool TeDeviation ( iteratorSet begin, iteratorSet end, double mean, int index=0)
{
	//Adds a new attribute in the attribute list of the set
	TeAttribute attrRep;
	attrRep.rep_.name_ = "Z";
	attrRep.rep_.type_ = TeREAL;
	begin->addProperty(attrRep);
	
	while (begin != end)
	{
		double result;
		
		if(((*begin)[index]) != TeMAXFLOAT) // MAXFLOAT is considered invalid value 
			result = ((*begin)[index]) - mean;
		else
			result = TeMAXFLOAT;
		
		(*begin).addPropertyValue(Te2String(result, 9)); 
		++begin; 
	}
	return true;
}
/** @} */
#endif

