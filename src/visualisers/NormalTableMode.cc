/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NormalTableMode.h
    \brief Implementation of the Template class NormalTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 14-July-2005
    
    Changes:
    
*/

#include "NormalTableMode.h"

#define ETM_MLEN 1024 //maximum number of elements of a histogram

using namespace magics;

NormalTableMode::NormalTableMode()
{
}


NormalTableMode::~NormalTableMode()
{
}

/*!
 Class information are given to the output-stream.
*/		
void NormalTableMode::print(ostream& out)  const
{
	out << "NormalTableMode[";
	LookupTableModeAttributes::print(out);
	out << "]";
}

void NormalTableMode::operator()(Image& im, Raster& rd)
{
//int normalslicing_ (Real *outlayer, int *value, int *nb, int *lut, 
//					int* MLEN, int* min, int* max)

     int 	i;		// auxiliary variables
     long	acum;		// accumulated population
     long	pop;		// total population
     long       cutv;		// outlayer population
     double	ratio;		// ratio value


     // Check outlayer value
     if ( outlayer_ < 0. || outlayer_ > 0.5 )
     {
	   MagLog::warning() << "Invalid outlayer value. Range between 0. and 0.5" << endl;
	   MagLog::warning() << "Default value used (0.0)" << endl;
	   outlayer_ = 0.;
     }

     // Check image range of values
     int imin = (int)rd.min();
     int imax = (int)rd.max();
     if (imax > ETM_MLEN || imin < 0 )
     {
	  MagLog::error() << "NormalTableMode: Invalid range of image values (" << imin << "," << imax << ")\n"
	               << "  Current implementation accepts values between 0 to " << ETM_MLEN << endl;
	  return;
     }

     // Initialize lookuptable
     ColourTable& table = im.getColourTable();
     int nlevels = table.size();

     // Reserve memory for the histogram and lut
     imax++; //index imax is needed
     vector<long> hist(imax,0L);
     vector<short> lut(imax);


     // Compute histogram (Maybe we do not need to use all values -> one every 10???)
     for (vector<double>::const_iterator val = rd.begin(); val != rd.end(); ++val)
     {
	     // Remove this test later
	     if(*val < imin || *val > imax-1)
		     MagLog::warning() << "ERROR" << endl;

	     hist[(int)*val]++;
     }

//remove later
//long aa=0L;
//for(i = 0; i < imax; i++) 
//{
//aa += hist[i];
//MagLog::dev()<< i << " " << hist[i] << endl;
//}

     // Remove outlayers and compute minmax indexes
     int minOut = 0;
     int maxOut = imax-1;
     pop  = (long)rd.getRows() * (long)rd.getColumns();
     cutv = (long) ((double)pop * outlayer_ + 1.);
     acum = 0L;
     for (i = 0; i < imax; i++)
     {
	  acum += hist[i];
	  if ( acum >= cutv )
	  {
	       minOut = i;
	       break;
	  }
     }
     acum = 0L;
     for (i = imax-1; i >= 0; i--)
     {
	  acum += hist[i];
	  if ( acum >= cutv )
	  {
	       maxOut = i;
	       break;
	  }
     }

     // Create lut
     ratio = (double)(maxOut-minOut) / (double)nlevels;
     for( i = 0; i < imax; i++)
     {
	  if ( i < minOut ) 
	       lut[i] = 0;
	  else if (i >= maxOut)
	       lut[i] = nlevels-1;
	  else
		  lut[i] = (short)( (double)(i-minOut) / ratio);
     }

     // Create output image
//vector<long> hist1(nlevels+1,0L); //test
     for (vector<double>::const_iterator val = rd.begin(); val != rd.end(); ++val)
     {
//	     im.push_back(lut[(int)*val]);
	     short ii = lut[(int)*val];
	     im.push_back(ii);
//hist1[ii]++;
     }
/*
     MagLog::dev()<< "NormalTableMode::operator()" << endl;
     for( i = 0; i < nlevels+1; i++)
	     MagLog::dev()<< i << " " << hist1[i] << endl;
*/
     return;
}
