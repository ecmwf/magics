/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// fsortabl.h
// rev vk 940824

#ifndef __TSORTABLE_H__
#define __TSORTABLE_H__

#include "fobject.h"


class TSortable : public TFObject {
public:
    virtual bool IsEqual(const TFObject& aDateObject) const    = 0;
    virtual bool IsLessThan(const TFObject& aDateObject) const = 0;

    // methods as 'const', vk 940824
    bool operator<(const TSortable&) const;
    bool operator>(const TSortable&) const;
    bool operator>=(const TSortable&) const;
    bool operator<=(const TSortable&) const;
};

#endif
//__TSORTABLE_H__
