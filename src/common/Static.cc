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

#include "Static.h"
#include "MagException.h"
#include "DriverStatic.h"



using namespace magics;

Static::Static() 
{
    DriverStatic drivers;
}

Static::~Static() 
{
   
}


#include "PaperDimension.h"
static SimpleObjectMaker<A4, PaperDimension> a4("a4");
static SimpleObjectMaker<A3, PaperDimension> a3("a3");
static SimpleObjectMaker<A5, PaperDimension> a5("a5");
static SimpleObjectMaker<A6, PaperDimension> a6("a6");



