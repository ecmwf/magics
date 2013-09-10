/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// fsortabl.cc
// rev vk 940824

#include  "fsortabl.h"

bool TSortable::operator <  (const TSortable & aDateObject) const
{
  return (IsLessThan(aDateObject) );
}


bool TSortable::operator >  (const TSortable & aDateObject) const
{
  return (bool) (!( *this < aDateObject ) &&  (*this != aDateObject));
}


bool TSortable::operator >=  (const TSortable & aDateObject) const
{
  return (bool) (!(*this < aDateObject));
}

bool TSortable::operator <=  (const TSortable & aDateObject) const
{
  return (bool) ( *this < aDateObject || *this == aDateObject );
}





