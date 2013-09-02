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

/*! \file RasterData.cc
    \brief Implementation of the Template class RasterData.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 12-Apr-2005
    
    Changes:
    
*/

#include "RasterData.h"

using namespace magics;

template <class P>
RasterData<P>::RasterData() : projection_(0)
{
}

template <class P>
RasterData<P>::~RasterData() 
{
	if (projection_) delete projection_;	
}

/*!
 Class information are given to the output-stream.
*/	
template <class P>	
void RasterData<P>::print(ostream& out)  const
{
	out << "RasterData<P>[";
	out << "xRes=" << x_;
	out << ", yRes=" << y_;
	out << ", columns=" << columns_;
	out << ", rows=" << rows_;
	out << ", lower_left=(" << lowerLeft_.y() << ", " << lowerLeft_.x() << ")";
	out << ", upper_right=(" << upperRight_.y() << ", " << upperRight_.x() << ")";
	out << ", " << size() << " points]";
}
