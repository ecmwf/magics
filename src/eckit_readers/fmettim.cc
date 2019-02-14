/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// rev vk 940913 -------------------------- fmettim.cc

#include <time.h>
#include "inc_iostream.h"

#ifndef __FMETTIM_H__
#include "fmettim.h"
#endif  //__FMETTIM_H__

//__________________________________________________________________

TMetTime ::TMetTime(void) : TDynamicTime() {
    TDynamicTime aTimeNow;

    ConstructMetTime(60);
    if (*this > aTimeNow) {
        PreviousMetTime();
    }
}
//__________________________________________________________________

void TMetTime ::ConstructMetTime(const short timeStepInMinutes) {
    fTimeStepInMinutes = timeStepInMinutes;
    NearestMetTime();
}
//__________________________________________________________________

TMetTime& TMetTime ::operator=(const TMetTime& aTime) {
    TStaticTime::operator=(aTime);  // retain present time step
    NearestMetTime(GetTimeStep());
    return *this;
}
//__________________________________________________________________

ostream& operator<<(ostream& oStream, const TMetTime& myTime) {
    oStream.width(2);
    oStream.fill('0');
    oStream << myTime.GetDay() << ".";
    oStream.width(2);
    oStream.fill('0');
    oStream << myTime.GetMonth() << "." << myTime.GetYear() << " ";
    oStream.width(2);
    oStream.fill('0');
    oStream << myTime.GetHour() << ":";
    oStream.width(2);
    oStream.fill('0');
    oStream << myTime.GetMin();
    // no seconds for MetTime!

    return oStream;
}

//__________________________________________________________________

TMetTime TMetTime ::operator++(void)  // prefix++
{
    NextMetTime(GetTimeStep());
    return *this;
}
//__________________________________________________________________

TMetTime TMetTime ::operator++(int)  // postfix++
{
    NextMetTime(GetTimeStep());
    return *this;
}
//__________________________________________________________________

TMetTime TMetTime ::operator--(void)  // prefix++
{
    PreviousMetTime(GetTimeStep());
    return *this;
}
//__________________________________________________________________

TMetTime TMetTime ::operator--(int)  // postfix++
{
    PreviousMetTime(GetTimeStep());
    return *this;
}
//__________________________________________________________________

void TMetTime ::NextMetTime(void) {
    NextMetTime(GetTimeStep());
}
//__________________________________________________________________

void TMetTime ::NextMetTime(const short deltaInMinutes) {
    long extraMinutes = (60 * GetHour() + GetMin()) % deltaInMinutes;

    // add observation interval and delete extra minutes (if any)
    DecodeCompareValue(GetCompareValue() + deltaInMinutes - extraMinutes);
    SetSec((short)0);
}
//__________________________________________________________________

void TMetTime ::PreviousMetTime(void) {
    PreviousMetTime(GetTimeStep());
}
//__________________________________________________________________

void TMetTime ::PreviousMetTime(const short deltaInMinutes) {
    long extraMinutes = (60 * GetHour() + GetMin()) % deltaInMinutes;

    if (extraMinutes > 0)
        // delete only extra minutes
        DecodeCompareValue(GetCompareValue() - extraMinutes);
    else
        // subtract given interval
        DecodeCompareValue(GetCompareValue() - deltaInMinutes);

    SetSec((short)0);
}
//__________________________________________________________________

void TMetTime ::NearestMetTime(void) {
    NearestMetTime(GetTimeStep());
}
//__________________________________________________________________

void TMetTime ::NearestMetTime(const short deltaInMinutes) {
    long extraMinutes = (60 * GetHour() + GetMin()) % deltaInMinutes;

    if (extraMinutes == 0)  // already a meteorological time!
        return;             // add vk 940824

    if (extraMinutes < (deltaInMinutes - extraMinutes))
        PreviousMetTime(deltaInMinutes);
    else
        NextMetTime(deltaInMinutes);
}
/*
//__________________________________________________________________ ???

TMetTime TMetTime :: NextObservation (const short deltaInMinutes) const
{
 int minutesSoFar = 60*GetHour() + GetMin();
 TMetTime aNewTime;

   aNewTime.DecodeCompareValue (GetCompareValue() +
            (long)(deltaInMinutes - (minutesSoFar % deltaInMinutes))
                   );
   aNewTime.SetSec((short) 0);
   return aNewTime;
}
//__________________________________________________________________ ???

TMetTime TMetTime :: PreviousObservation (const short deltaInMinutes) const
{
 int minutesSoFar = 60*GetHour() + GetMin();
 TMetTime aNewTime;

   aNewTime.DecodeCompareValue (GetCompareValue() -
             (long)(minutesSoFar % deltaInMinutes)
                   );
   aNewTime.SetSec((short) 0);
   return aNewTime;
}
//__________________________________________________________________ ???

TMetTime TMetTime :: NearestObservation (const short deltaInMinutes) const
{
 int minutesSoFar = 60*GetHour() + GetMin();

   if ( (deltaInMinutes - (minutesSoFar % deltaInMinutes)) <
    (minutesSoFar % deltaInMinutes))
       return NextObservation (deltaInMinutes);
   else
       return PreviousObservation (deltaInMinutes);
}
*/
//__________________________________________________________________

void TMetTime ::SetTimeStep(const short timeStepInMinutes) {
    fTimeStepInMinutes = timeStepInMinutes;
}
//__________________________________________________________________

short TMetTime ::GetTimeStep(void) const {
    return fTimeStepInMinutes;
}
//__________________________________________________________________

short TMetTime ::GetLocalHour() const {
    return (GetHour() + 2) % 24;  // testiarvona vakioero
}
