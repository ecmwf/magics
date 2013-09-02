//***********************************************************************
//      TerraLib is a GIS Classes and Functions Library that 
//      strongly explores Spatial Database Technologies 
//
//      Copyright © 2002 INPE and Tecgraf/PUC-Rio. 
//
//      This library is free software; you can redistribute it 
//      and/or modify it under the terms of the GNU Lesser General 
//      Public License as published by the Free Software Foundation
//      version 2.1.(http://www.opensource.org/licenses/lgpl-license.php)
//
//      
//
//      Send questions or suggestions about the TerraLib Project 
//      to terralib@dpi.inpe.br .
//**************************************************************************//
/*! \file TeSpatialStatistics.h
    This file provides support for spatial statistics functions
	\note for a general overview of spatial statistics, please see
  "Spatial Data Analysis by Example", Bailey and  Gattrell
*/
#ifndef spatialStatsDEFINED
#define spatialStatsDEFINED
#include <string>
#include <vector>
#include <map>
#include <set>
#include "TeAttribute.h"
#include "TeSelectedObject.h"

class TeDatabase;
class TeDatabasePortal;
class TeLayer;

using namespace std;

// structure for storing the proximity matrix
typedef map<string, TeSelectedObjectVector>  TeProxMatrix;

// structure for local indexes
typedef map<string, TeProperty> TeLocalIndex; // review TeProperty ??

//! return the mean of the attributes of the objects
double TeMean ( TeSelectedObjectMap& bObjects );

//! return the median value of the attributes of the objects
double TeMedian ( TeSelectedObjectMap& bObjects );

//! calculates the variance of the attributes of the objects
double TeVariance ( TeSelectedObjectMap& bObjects, double mean );

//! calculates the deviation standart
double TeStandardDeviation ( double variance );

//! generates the spatial proximity matrix
void TeCreateProxMatrix ( TeDatabase* portal, TeSelectedObjectMap& objectMap, 
					      TeLayer& curLayer, TeProxMatrix& proxMatrix );

void TeLocalMean ( TeSelectedObjectMap& bObjects, TeProxMatrix& proxMatrix,
					TeSelectedObjectMap& result );

//! calcuates de Moran Index
double TeMoranIndex ( TeSelectedObjectMap& bObjects, TeProxMatrix& proxMatrix );


//! calculates G and G * statistics 
void TeGStatistics ( TeSelectedObjectMap& objects, TeProxMatrix& proxMatrix,
			  TeSelectedObjectMap& Gresult, TeSelectedObjectMap& GStarresult );

//! evaluates the statistical significance of Moran Index
double
TeGlobalMoranSignificance ( TeSelectedObjectMap& objects, vector<double>& desviations,
						   TeProxMatrix& proxMatrix, unsigned int permutationsNumber,
						  double moranIndex);
//  ---

//! return the average value
double TeMean(TeDatabasePortal *atrib);

//! return the median value 
double TeMedian(TeDatabasePortal *atrib);

//! calculates the variance
double TeVariance(TeDatabasePortal *atrib, double mean );

//! return the minimum value 
double TeMinValue(TeDatabasePortal *atrib);

//! return the maximum value 
double TeMaxValue(TeDatabasePortal *atrib);

//!	return the sum of the elements
template <typename It> double
TeVectorSum (It begin, It end)
{
	double sum = 0.;
	while ( begin != end)
	{
		sum += (*begin);
		begin++;
	}
	return sum;
}

//!	calculates the vector with the means of the attributs
template <typename It, typename T> void
TeMeanVector ( It begin, It end, T& result)
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
			soma[j]+= (*begin)[j];
		}
		i++;
		begin++;
	}
	
	for (int k = 0; k < dim; k++)
		result[k] = soma[k]/i;
}

//! calculates the deviations in relation to mean
template <typename It> void
TeDeviation ( It begin, It end, It result, double mean)
{
	while (begin != end)
	{
		*result = double(*begin) - mean;
		begin++; result++;
	}
}

//! calculates the mean (first moment)
template <typename It> double		
TeFirstMoment ( It begin, It end)
{
	int number = 0;
	double mean = 0.;
	while (begin != end)
	{
		mean += (*begin);
		number++; begin++;
	}
	return mean /= number;
}

//! calculates the second moment
template <typename It> double
TeSecondMoment ( It begin, It end, double mean)
{
	int number = 0;
	double SSD = 0;  //sum of squares of desviation

	while (begin != end)
	{
		SSD += pow(( (*begin) - mean), 2);
		number++; begin++;
	}
	return SSD /= number;
}

//!  calculates the indexes of Moran (global and local)
template <typename It> double
TeAlternativeMoranIndex (It it_beginZ, It it_endZ, It it_beginWZ, It it_lisa)
{
	double variance = 0.;
	double sum = 0;
	int number = 0;

	variance = TeSecondMoment (it_beginZ, it_endZ, 0);

	while ( it_beginZ != it_endZ)
	{
		double ZxWz = ( (*it_beginZ)*(*it_beginWZ) )/variance;
		*it_lisa = ZxWz;
		sum += ZxWz;
		number++; it_beginZ++; it_beginWZ++; it_lisa++;
	}
	
	return sum /= number;
}

//! classifies the objects in quadrants it conforms the scatterplot of Moran
template <typename ItIn, typename ItOut> void
TeBoxMap (ItIn it_beginZ, ItIn it_endZ, ItIn it_beginWZ, double mean, ItOut it_beginBMap)
{
	while ( it_beginZ != it_endZ)
	{
		if ( ((*it_beginZ) >= mean) && ((*it_beginWZ) >= mean))
			*it_beginBMap = "1";
		if ( ((*it_beginZ) < mean) && ((*it_beginWZ) >= mean))	
			*it_beginBMap = "4";	
		if ( ((*it_beginZ) < mean) && ((*it_beginWZ) < mean))
			*it_beginBMap = "2";		
		if ( ((*it_beginZ) >= mean) && ((*it_beginWZ) < mean))
			*it_beginBMap = "3";
		it_beginZ++, it_beginWZ++, it_beginBMap++;
	}	
}


//! evaluates the statistical significance of the local indexes (LISA)

template <typename T1, typename T2>
void TeLisaStatisticalSignificance ( int permutationsNumber, T1& attributes,
                                     T1& lisa, T2& neighNumber, T1& probabilities)
{

	double sum;
	double WZperm;
	double significance;

	typename T1::iterator it = attributes.begin();

	double variance = TeSecondMoment (it, attributes.end(), 0.);

	int index = 0;
	while ( it != attributes.end())
	{
		int objectsNumber = attributes.size();
		int neighborsNumber = neighNumber[index];
		double* permut = new double[permutationsNumber];
		for (int j = 0; j < permutationsNumber; j++)
		{
			int randon = 0;
			srand(j);
			sum = 0;
			set<int> setNeigh;
			for (int k = 0; k < neighborsNumber; k++)
			{
				randon = (int)((rand()*objectsNumber)/32768);
				if (index == randon || (setNeigh.find(randon) != setNeigh.end()) )
					k--;	// raffle annulled	
				else
				{		
					setNeigh.insert(randon);
					sum += attributes[randon];
				}
			}

			WZperm = sum/neighborsNumber;
			permut[j] = attributes[index] * WZperm / variance;
		}	

		int position = 0;
		for (int k = 0; k < permutationsNumber; k++)
		{
			if (lisa[index] > permut[k])
				position++;
		}
 		if ( lisa[index] >= 0)
			significance = (double) (permutationsNumber-position)/(permutationsNumber+1);
		else
			significance = (double) position/( permutationsNumber + 1 );
			
		probabilities[index] = significance;
		it++;
		index++;
	}
}


//! classifies the objects it conforms the statistical significance
template<typename It1, typename It2> void
TeLisaMap ( It1 begin, It1 end, It2 itLisaMap, int permutationNumber)
{
	while (begin != end)
	{
		string significanceClass = "0";
		if ( ( (*begin) < 0.001 ) && (permutationNumber >= 999) )
			significanceClass = "3";
		else if ( (*begin) < 0.01 && (permutationNumber >= 99) )
			significanceClass = "2";
		else if ( (*begin) < 0.05)
			significanceClass = "1";
		(*itLisaMap) = significanceClass; 
		begin++; itLisaMap++;
	}
}

//! classifies the objects it conforms the scatterplot of Moran and statistical significance
template <typename It1, typename It2, typename ItOut> void
TeMoranMap (It1 lisaBegin, It1 lisaEnd, It2 boxMap, ItOut moranMap)
{
	while ( lisaBegin != lisaEnd)
	{
		if ( (*lisaBegin) != "0")
		{
			(*moranMap) = (*boxMap);
		}
		else
			(*moranMap) = "0";
		lisaBegin++; boxMap++; moranMap++;
	}
}

//! calculates the empirical Bayes estimation
template <typename It> void
TeEmpiricalBayes (It rBegin, It rEnd, It n, double mean, double variance, It bayes)
{
	double w= 0.;
	while (rBegin != rEnd)
	{
		w = varince / ( variance + ( mean/(*n)) );
		*bayes = w*(*rBegin) + (1-w)*mean;
		rBegin++; n++; bayes++;
	}
}

//! calculates the local Bayes estimation
template <typename It> void
TeSpatialEmpiricalBayes (It rBegin, It rEnd, It n, It mean, It variance, It bayes)
{
	double w = 0.;
	while (rBegin != rEnd)
	{
		w = (*varince)/( (*variance) + ( (*mean)/n ));	
		*bayes = w*(*rBegin) + (1-w)*(*mean);
		rBegin++; n++; mean++; variance++; bayes++;
	}
}
  
//! calculates the value average in function of the neighbor objects
void TeLocalMean ( TeSelectedObjectMap& bObjects, TeProxMatrix& proxMatrix,
					TeSelectedObjectMap& result );


double TeMoranIndex ( TeSelectedObjectMap& bObjects, TeProxMatrix& proxMatrix, TeSelectedObjectMap& result );

#endif

