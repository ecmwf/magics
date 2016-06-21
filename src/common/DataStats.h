/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DataStats.h
    \brief Definition of the  class  dataStats.
    
    Magics Team - ECMWF 2004
    
    Started: April 2011
    
    Changes:
    
*/

#ifndef DataStats_H
#define DataStats_H

#include <vector>

using namespace std;

class DataStats
{
public:
	DataStats(vector<double>&);

	unsigned int num() {return num_;}
	double min() {return min_;}
	double max() {return max_;}
	double mean() {return mean_;}
	double stDev() {return stDev_;}
	double skewness() {return skewness_;}
	double kurtosis() {return kurtosis_;}
	bool hasStDev() {return hasStDev_;}
	bool hasThirdMoment() {return hasThirdMoment_;}
	static double correlation(vector<double>&,vector<double>&,DataStats&,DataStats&);

private:
	void compute(vector<double>&);

	unsigned int num_;
	double min_;
	double max_;
	double mean_;
	double stDev_;
	double skewness_;
	double kurtosis_;
	bool hasStDev_;
	bool hasThirdMoment_;
};

#endif
