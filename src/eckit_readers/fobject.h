/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// fobject.h
// rev vk 940630

#ifndef __TOBJECT_H__
#define __TOBJECT_H__


class TFObject
{
 public:
    TFObject(void);
    virtual ~TFObject(void);

    virtual bool    IsEqual(const TFObject &) const = 0;

    bool operator == (const TFObject &aObjTest) const;
    bool operator != (const TFObject &aObjTest) const;
};

#endif //__TOBJECT_H__
