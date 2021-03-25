/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// rev vk 000802 -------------------------

#include <time.h>
#include "inc_iostream.h"

#include "fdyntime.h"


const int firstYear = 1830; /* oldest comparable year ! */

static int monthLength[]      = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
           dayCount[]         = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
static const char* weekdays[] = {"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"};

//_________________________________________________________________________
// Q&D hack to use Metview Date and Time fields in constructor:
// - datePart should be <= 10 (relative) or in format YYMMDD (absolute)
// - timePart is interpreted as hours HH when < 24, otherwise in format HHMM

TDynamicTime ::TDynamicTime(const long datePart, const long timePart) {
    _setCurrent();

    if (datePart <= 10)
        ChangeByDays((short)datePart);
    else {
        if (datePart > 101)
            SetDate((short)(datePart / 10000), (short)((datePart / 100) % 100), (short)(datePart % 100));
    }

    if (timePart < 24)
        SetTime((short)timePart, 0, 0);
    else
        SetTime((short)(timePart / 100), (short)(timePart % 100), 0);
}

/**-----------------------------------------------------------------------*/

void TDynamicTime ::ChangeByMinutes(const short minutes) {
    DecodeCompareValue(GetCompareValue() + (long)minutes);
}

void TDynamicTime ::ChangeByHours(const short hours) {
    DecodeCompareValue(GetCompareValue() + 60L * (long)hours);
}

void TDynamicTime ::ChangeByDays(const short days) {
    DecodeCompareValue(GetCompareValue() + 60L * 24L * (long)days);
}


long TDynamicTime ::DifferenceInMinutes(const TDynamicTime& anotherTime) const {
    return GetCompareValue() - anotherTime.GetCompareValue();
}

long TDynamicTime ::DifferenceInHours(const TDynamicTime& anotherTime) const {
    return DifferenceInMinutes(anotherTime) / 60L;
}

long TDynamicTime ::DifferenceInDays(const TDynamicTime& anotherTime) const {
    return DifferenceInMinutes(anotherTime) / 60L / 24L;
}
//_______________________________________________________ ClockInSeconds
// added 941229/vk

long TDynamicTime ::ClockInSeconds(void) const {
    return 3600L * (long)GetHour() + 60L * (long)GetMin() + (long)GetSec();
}
//_______________________________________________________ GetWeekday

short TDynamicTime ::GetWeekday(void) const  // mon=1, tue=2,..., sat=6,  sun=7
{
    TDynamicTime Sunday((short)1830, (short)1, (short)4);

    return 1 + DifferenceInDays(Sunday) % 7;
}
void TDynamicTime ::PrintWeekday(void) const {
    cout << weekdays[GetWeekday() - 1];
}


//  p r i v a t e  functions
long TDynamicTime ::GetCompareValue(void) const {
    long days = 0;

    for (short v = firstYear; v < GetYear(); ++v)
        days += (long)DaysInYear(v);

    days += (long)dayCount[GetMonth() - 1] + (long)(GetDay() - 1);
    if (GetMonth() > 2 && DaysInYear(GetYear()) == 366)
        ++days;

    return 24L * 60L * (long)days + 60L * (long)GetHour() + (long)GetMin();
}

void TDynamicTime ::DecodeCompareValue(const long& aCompareValue) {
    short xYear, xMonth;
    long aValue = aCompareValue;

    for (short y = firstYear; aValue >= 0; xYear = y++)
        aValue -= 24L * 60L * (long)DaysInYear(y);
    if (aValue < 0)
        aValue += 24L * 60L * (long)DaysInYear(xYear);

    for (short m = 1; aValue >= 0; xMonth = m++)
        aValue -= 24L * 60L * (long)DaysInMonth(m, xYear);
    if (aValue < 0)
        aValue += 24L * 60L * (long)DaysInMonth(xMonth, xYear);

    SetYear(xYear);
    SetMonth(xMonth);
    SetDay((short)(aValue / (60L * 24L) + 1));

    aValue %= (60L * 24L);
    SetHour((short)(aValue / 60L));

    aValue %= 60L;
    SetMin((short)aValue);
}

short TDynamicTime ::DaysInYear(const short aYear) const {
    if ((aYear % 4) != 0)
        return 365;
    else {
        if (aYear == 1900)
            return 365;
        else
            return 366;
    }
}

short TDynamicTime ::DaysInMonth(const short aMonth, const short aYear) const {
    if (aMonth == 2)
        return monthLength[1] + DaysInYear(aYear) - 365;
    else
        return monthLength[aMonth - 1];
}
