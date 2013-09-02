
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
