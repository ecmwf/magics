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

#include "TeGTParams.h"

#include <float.h>

TeGTParams::TeGTParams()
{
  reset();  
}


TeGTParams::TeGTParams( const TeGTParams& external )
{
  operator=( external );
}


TeGTParams::~TeGTParams()
{
  reset();
}


const TeGTParams& TeGTParams::operator=( 
  const TeGTParams& external )
{
  transformation_name_ = external.transformation_name_;
  out_rem_strat_ = external.out_rem_strat_;
  max_dmap_error_ = external.max_dmap_error_;
  max_imap_error_ = external.max_imap_error_;
  tiepoints_ = external.tiepoints_;
  direct_parameters_ = external.direct_parameters_;
  inverse_parameters_ = external.inverse_parameters_;
  maxIters_          = external.maxIters_;
  tolerance_         = external.tolerance_;
  useAdaptiveParams_ = external.useAdaptiveParams_;  
  WMatrix_ = external.WMatrix_;
  
  return external;
}


std::string TeGTParams::decName() const
{
  return transformation_name_;
}


void TeGTParams::reset()
{
  transformation_name_ = "affine";
  out_rem_strat_ = NoOutRemotion;
  max_dmap_error_ = DBL_MAX;
  max_imap_error_ = DBL_MAX;
  maxIters_  = 20;
  tolerance_ = 0.0001;
  useAdaptiveParams_ = false; 
  
  direct_parameters_.Clear();
  inverse_parameters_.Clear();
  tiepoints_.clear();
  WMatrix_.Clear();
}

