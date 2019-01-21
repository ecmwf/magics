/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// rev vk 941229 ---------------------------- TDynamicTime

#ifndef __TDYNAMICTIME_H__
#define __TDYNAMICTIME_H__

#include "fstatime.h"

class TDynamicTime : public TStaticTime {
public:
    TDynamicTime() : TStaticTime() {}
    TDynamicTime(const TStaticTime& aTime) : TStaticTime(aTime) {}
    TDynamicTime(const long datePart, const long timePart);
    TDynamicTime(const short year, const short month, const short day) : TStaticTime(year, month, day) {}
    TDynamicTime(const short year, const short month, const short day, const short hour, const short minute = 0,
                 const short sec = 0) :
        TStaticTime(year, month, day, hour, minute, sec) {}

    void ChangeByMinutes(const short minutes);
    void ChangeByHours(const short hours);
    void ChangeByDays(const short days);

    long DifferenceInMinutes(const TDynamicTime& anotherTime) const;
    long DifferenceInHours(const TDynamicTime& anotherTime) const;
    long DifferenceInDays(const TDynamicTime& anotherTime) const;

    long ClockInSeconds(void) const;  // add/vk 941229

    short GetWeekday(void) const;  // mon=1, tue=2,..., sat=6,  sun=7
    void PrintWeekday(void) const;

protected:
    long GetCompareValue(void) const;
    void DecodeCompareValue(const long& aCompareValue);
    short DaysInYear(const short aYear) const;
    short DaysInMonth(const short aMonth, const short aYear) const;

private:
};

#endif
//__TDYNAMICTIME_H__
