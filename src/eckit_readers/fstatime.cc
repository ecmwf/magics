/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// rev vk 981001 --------------------- fstatime.cc

#include <time.h>
#include "inc_iostream.h"

#include "fstatime.h"


static char charValueString[100];


/**-----------------------------------------------------------------------*/

TStaticTime ::TStaticTime(void) {
    _setCurrent();
}
//__________________________________________________________________

TStaticTime ::TStaticTime(const TStaticTime& from) {
    fYear  = from.fYear;
    fMonth = from.fMonth;
    fDay   = from.fDay;
    fHour  = from.fHour;
    fMin   = from.fMin;
    fSec   = from.fSec;
}
//__________________________________________________________________

TStaticTime ::TStaticTime(const short aY, const short aM, const short aD) {
    SetDate(aY, aM, aD);
    SetTime(0, 0, 0);
}
//__________________________________________________________________

TStaticTime ::TStaticTime(const short aY, const short aM, const short aD, const short aH, const short aMin,
                          const short aSec) {
    SetDate(aY, aM, aD);
    SetTime(aH, aMin, aSec);
}
//__________________________________________________________________
void TStaticTime ::_setCurrent(void) {
    time_t tTime;
    struct tm* xTime;

    (void)time(&tTime);
    xTime = localtime(&tTime);

    SetYear(xTime->tm_year + 1900);
    SetMonth(xTime->tm_mon + 1);
    SetDay(xTime->tm_mday);
    SetHour(xTime->tm_hour);
    SetMin(xTime->tm_min);
    SetSec(xTime->tm_sec);
}
//__________________________________________________________________

TStaticTime& TStaticTime::operator=(const TStaticTime& from)  //   =
{
    fYear  = from.fYear;
    fMonth = from.fMonth;
    fDay   = from.fDay;
    fHour  = from.fHour;
    fMin   = from.fMin;
    fSec   = from.fSec;
    return *this;
}
//__________________________________________________________________
//            "yyyy-mm-dd"
const char* TStaticTime::CharDate() const {
    ostrstream ss(charValueString, sizeof(charValueString));
    ss << setfill('0') << setw(4) << GetYear() << "-" << setw(2) << GetMonth() << "-" << setw(2) << GetDay() << ends;

    return charValueString;
}
//__________________________________________________________________
//            "hh:mm"
const char* TStaticTime::CharHhMm() const {
    ostrstream ss(charValueString, sizeof(charValueString));
    ss << setfill('0') << setw(2) << GetHour() << ":" << setw(2) << GetMin() << ends;

    return charValueString;
}
//__________________________________________________________________
//            "yyyy-mm-dd hh:mm:ss"
const char* TStaticTime::CharValue(void) const {
    ostrstream ss(charValueString, sizeof(charValueString));
    ss << CharDate() << " " << setfill('0') << setw(2) << GetHour() << ":" << setw(2) << GetMin() << ":" << setw(2)
       << GetSec() << ends;

    return charValueString;
}
//__________________________________________________________________
//            "yymmdd hhmm"
const char* TStaticTime::ShorterCharValue(void) const {
    ostrstream ss(charValueString, sizeof(charValueString));
    ss << setfill('0') << setw(2) << (GetYear() % 100) << setw(2) << GetMonth() << setw(2) << GetDay() << " " << setw(2)
       << GetHour() << setw(2) << GetMin() << ends;

    return charValueString;
}
//__________________________________________________________________

bool TStaticTime ::IsEqual(const TFObject& anotherTime) const {
    if (GetYear() != ((const TStaticTime*)&anotherTime)->GetYear())
        return false;

    if (GetMonth() != ((const TStaticTime*)&anotherTime)->GetMonth())
        return false;

    if (GetDay() != ((const TStaticTime*)&anotherTime)->GetDay())
        return false;

    if (GetHour() != ((const TStaticTime*)&anotherTime)->GetHour())
        return false;

    if (GetMin() != ((const TStaticTime*)&anotherTime)->GetMin())
        return false;

    if (GetSec() != ((const TStaticTime*)&anotherTime)->GetSec())
        return false;

    return true;
}
//__________________________________________________________________

bool TStaticTime ::IsLessThan(const TFObject& anotherTime) const {
    if (GetYear() > ((const TStaticTime*)&anotherTime)->GetYear())
        return false;
    else {
        if (GetYear() < ((const TStaticTime*)&anotherTime)->GetYear())
            return true;
    }

    if (GetMonth() > ((const TStaticTime*)&anotherTime)->GetMonth())
        return false;
    else {
        if (GetMonth() < ((const TStaticTime*)&anotherTime)->GetMonth())
            return true;
    }

    if (GetDay() > ((const TStaticTime*)&anotherTime)->GetDay())
        return false;
    else {
        if (GetDay() < ((const TStaticTime*)&anotherTime)->GetDay())
            return true;
    }

    if (GetHour() > ((const TStaticTime*)&anotherTime)->GetHour())
        return false;
    else {
        if (GetHour() < ((const TStaticTime*)&anotherTime)->GetHour())
            return true;
    }

    if (GetMin() > ((const TStaticTime*)&anotherTime)->GetMin())
        return false;
    else {
        if (GetMin() < ((const TStaticTime*)&anotherTime)->GetMin())
            return true;
    }

    if (GetSec() < ((const TStaticTime*)&anotherTime)->GetSec())
        return true;

    return false;
}

//__________________________________________________________________

void TStaticTime ::SetYear(const short aYear) {
    fYear = aYear;

    if (fYear < 25)
        fYear += 2000;  //-- assume 2000+

    if (fYear < 200)
        fYear += 1900;  //-- assume 1900+
}
void TStaticTime ::SetMonth(const short aMonth) {
    if (aMonth >= 1 && aMonth <= 12)
        fMonth = aMonth;
    else {
        fMonth = 0;
        cerr << "'TStaticTime::SetMonth': erroneous month value: " << aMonth << "\n";
    }
}
void TStaticTime ::SetDay(const short aDay) {
    if (aDay >= 1 && aDay <= 31)
        fDay = aDay;
    else {
        fDay = 0;
        cerr << "'TStaticTime::SetDay': erroneous day value: " << aDay << "\n";
    }
}
//__________________________________________________________________

void TStaticTime ::SetHour(const short aHour) {
    if (aHour >= 0 && aHour <= 23)
        fHour = aHour;
    else {
        fHour = 0;
        cerr << "'TStaticTime::SetHour': erroneous hour value: " << aHour << "\n";
    }
}

void TStaticTime ::SetMin(const short aMin) {
    if (aMin >= 0 && aMin <= 59)
        fMin = aMin;
    else {
        fMin = 0;
        cerr << "'TStaticTime::SetMin': erroneous minutes value: " << aMin << "\n";
    }
}

void TStaticTime ::SetSec(const short aSec) {
    if (aSec >= 0 && aSec <= 59)
        fSec = aSec;
    else {
        fSec = 0;
        cerr << "'TStaticTime::SetSec': erroneous seconds value: " << aSec << "\n";
    }
}
//__________________________________________________________________

void TStaticTime ::SetDate(const short year, const short month, const short day) {
    SetYear(year);
    SetMonth(month);
    SetDay(day);
}
void TStaticTime ::GetDate(short& year, short& month, short& day) const {
    year  = GetYear();
    month = GetMonth();
    day   = GetDay();
}
void TStaticTime ::SetTime(const short hour, const short minute, const short sec) {
    SetHour(hour);
    SetMin(minute);
    SetSec(sec);
}
void TStaticTime ::GetTime(short& hour, short& minute, short& sec) const {
    hour   = GetHour();
    minute = GetMin();
    sec    = GetSec();
}
//__________________________________________________________________

void TStaticTime ::ReadDateTime(void)  // test function!!!!
{
    short y, m, d, h;
    cout << " Input date 'yy mm dd hh': ";
    cin >> y >> m >> d >> h;
    SetDate(y, m, d);
    SetTime(h, 0, 0);
}
//__________________________________________________________________

void TStaticTime ::XPrint(void) const  // test function!!!!
{
    Print();
}
void TStaticTime ::XPrint(const char* str) const  // test function!!!!
{
    cout << str << ": ";
    XPrint();
}
//__________________________________________________________________

void TStaticTime ::Print(void) const  // test function!!!!
{
    cout << fDay << "." << fMonth << "." << fYear << " " << fHour << ":" << fMin << ":" << fSec;  //<< "\n";
}
//__________________________________________________________________

ostream& operator<<(ostream& oStream, const TStaticTime& myTime) {
    oStream << myTime.CharValue();  //-- ISO date format
#if 0
   oStream.width( 2 );
   oStream.fill( '0' );
   oStream << myTime.GetDay() << ".";
   oStream.width( 2 );
   oStream.fill( '0' );
   oStream << myTime.GetMonth() << "." << myTime.GetYear() << " ";
   oStream.width( 2 );
   oStream.fill( '0' );
   oStream << myTime.GetHour() << ":";
   oStream.width( 2 );
   oStream.fill( '0' );
   oStream << myTime.GetMin() << ":";
   oStream.width( 2 );
   oStream.fill( '0' );
   oStream << myTime.GetSec();
#endif
    return oStream;
}
