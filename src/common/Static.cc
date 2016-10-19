/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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



