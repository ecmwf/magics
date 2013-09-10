/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// fobject.cpp
// rev vk 940824

#include <stddef.h>
#include "inc_iostream.h"

#include  "fobject.h"

//_________________________________________________________ TFObject

TFObject::TFObject(void)
{
}
//_________________________________________________________ ~TFObject

TFObject::~TFObject(void)
{
}
//_________________________________________________________ operator==

bool TFObject::operator == (const TFObject & aObjTest) const
{
  return (IsEqual ( aObjTest ));
}
//_________________________________________________________ operator!=

bool TFObject::operator != (const TFObject & aObjTest) const
{
  return (bool) ( !(IsEqual ( aObjTest )));
}
