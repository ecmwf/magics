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
    \file MgQDriverObject.h
    \brief Definition of MgQDriverObject 
    \author Graphics Section, ECMWF

    Started: April 2010
*/

#ifndef MgQDriverObject_H
#define MgQDriverObject_H

#include "QtDriver.h"

class MgQDriverObject
{
public:
	MgQDriverObject(const QtDriver& driver) : driver_(driver) {};

	const QtDriver&  driver() {return driver_;} 

protected:
	const QtDriver& driver_;
};

#endif
