/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DataStats.cc
    \brief Implementation of the class DataStats.

    Magics Team - ECMWF 2004

    Started: April 2011

    Changes:

*/

#include <math.h>
#include <algorithm>
#include <numeric>

#include "DataStats.h"

DataStats::DataStats(vector<double>& data) {
    num_            = 0;
    hasStDev_       = false;
    hasThirdMoment_ = false;

    compute(data);
}

void DataStats::compute(vector<double>& data) {
    num_ = data.size();

    if (num_ == 0) {
        return;
    }

    min_ = *(std::min_element(data.begin(), data.end()));
    max_ = *(std::max_element(data.begin(), data.end()));

    mean_ = std::accumulate(data.begin(), data.end(), 0.0) / data.size();

    if (data.size() >= 1) {
        double v, m2 = 0., m3 = 0., m4 = 0.;

        for (vector<double>::const_iterator it = data.begin(); it != data.end(); it++) {
            v = (*it) - mean_;
            m2 += v * v;
            m3 += v * v * v;
            m4 += v * v * v * v;
        }

        m2 /= data.size();
        m3 /= data.size();
        m4 /= data.size();

        stDev_    = sqrt(m2);
        hasStDev_ = true;

        if (m2 != 0) {
            skewness_       = m3 / (stDev_ * stDev_ * stDev_);
            kurtosis_       = m4 / (m2 * m2) - 3.;
            hasThirdMoment_ = true;
        }
    }
}

double DataStats::correlation(vector<double>& dataX, vector<double>& dataY, DataStats& stX, DataStats& stY) {
    double corr = 0.;

    if (stX.hasStDev() == false || stY.hasStDev() == false || stX.stDev() <= 0. || stY.stDev() <= 0. ||
        stX.num() != stY.num() || dataX.size() != stX.num() || dataY.size() != stY.num() || stX.num() == 0) {
        return corr;
    }

    for (unsigned int i = 0; i < dataX.size(); i++) {
        corr += (dataX[i] - stX.mean()) * (dataY[i] - stY.mean());
    }

    corr /= stX.num() * stX.stDev() * stY.stDev();

    return corr;
}
