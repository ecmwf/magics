/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LinearTableMode.cc
    \brief Implementation of the Template class LinearTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 17-May-2005
    
    Changes:
    
*/



#include "LinearTableMode.h"
#include "IntervalMap.h"

using namespace magics;

LinearTableMode::LinearTableMode()
{
	return;
}


LinearTableMode::~LinearTableMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void LinearTableMode::print(ostream& out)  const
{
	out << "LinearTableMode[";
	LookupTableModeAttributes::print(out);
	out << "]";
}

void LinearTableMode::operator()(Image& im, Raster& rd)
{
     int 	i;		//auxiliary variables
     double   rint;		//interval of levels

     // Check image range of values
//     double imin = (double)rd.min();
//     double imax = (double)rd.max();
     double imin = (minLevel_ < -1000000) ? (double)rd.min() : minLevel_;
     double imax = (maxLevel_ >  1000000) ? (double)rd.max() : maxLevel_;

     // Initialize lookuptable
     ColourTable& table = im.getColourTable();
     int nlevels = table.size();

    // Create a map of image levels interval
    IntervalMap<int> map;
    rint = (imax-imin)/(double)nlevels;
    double level = imin;
    for( i = 0; i < nlevels; i++)
    {
         map[  Interval(level, level+rint) ] = i;
//MagLog::dev()<< i << " " << level << " " << level+rint << endl;
	 level += rint;
    }

     // Create output image
//vector<long> hist1(nlevels,0L); //test
//long iii=0L;
     for (vector<double>::const_iterator val = rd.begin(); val != rd.end(); ++val)
     {
//	     im.push_back(lut[(int)*val]);
	     short ii = map.find(*val,nlevels-1);
	     im.push_back(ii);
// iii++;
// hist1[ii]++;
     }

//for( i = 0; i < nlevels; i++)
//       MagLog::dev()<< i << " " << hist1[i] << endl;
//MagLog::dev()<< "total=" << iii<< endl;

     return;
}

#if 0
void LinearTableMode::operator()(Image& im, Raster& rd)
{
     int 	i;		 //auxiliary variables
     double   ratio;           // imagel level ratio

     // Check image range of values
     int imin = (int)rd.min();
     int imax = (int)rd.max();
#if 0 //remove later
     if (imax > ETM_MLEN || imin < 0 )
     {
	  MagLog::dev()<< "LinearTableMode: Invalid range of image values (" << imin << "," << imax << ")" << endl;
	  MagLog::dev()<< "Current implementation accepts values between 0 to " << ETM_MLEN << endl;
	  return;
     }
#endif

     // Initialize lookuptable
     ColourTable& table = im.getColourTable();
     int nlevels = table.size();

     // Reserve memory for the lut
     imax++; //index imax is needed
     vector<short> lut(imax);
   
    // Create lut
    ratio = (double)(imax-imin)/(double)nlevels;
    for( i = 0; i < imax; i++)
    {
   	 if (i < imin)
	     lut[i] = 0;
	 else if (i > imax)
	     lut[i] = nlevels-1;
	 else
       	     lut[i] =  (short)( (double)(i-imin)/ratio );
     }

     // Create output image
vector<long> hist1(nlevels+1,0L); //test
     for (vector<double>::const_iterator val = rd.begin(); val != rd.end(); ++val)
     {
//	     im.push_back(lut[(int)*val]);
	     short ii = lut[(int)*val];
	     im.push_back(ii);
hist1[ii]++;
     }

     MagLog::dev()<< "LinearTableMode::operator()" << endl;
     for( i = 0; i < nlevels+1; i++)
	     MagLog::dev()<< i << " " << hist1[i] << endl;

     return;
}
#endif
