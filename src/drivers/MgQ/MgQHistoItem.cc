/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file MgQHistoItem.cc
    \brief Definition of MgQHistoItem
    \author Graphics Section, ECMWF

    Started: February 2011
*/

#include "MgQHistoItem.h"

//using namespace magics;

MgQHistoItem::MgQHistoItem() : cached_(false)
{
	setData(MgQ::ItemIsVisibleKey,false);	
}

MgQHistoItem::~MgQHistoItem()
{	

}
