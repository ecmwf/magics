/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// rev vk 941021 ------------------------- TMetTime

#ifndef __FMETTIM_H__
#define __FMETTIM_H__

#include  "fdyntime.h"

// TMetTime: Discrete time object for meteorological observations and
//           forecasts.  Watch out for the "="-operator and constructors
//           with 'out of phase' times as the TMetTime object will always
//           try to fit itself into 'the correct phase'!

class TMetTime : public TDynamicTime
{
 friend ostream& operator<< ( ostream& oStream, const TMetTime& myTime);

 public:
    TMetTime( void );                               // latest present MetTime
             // : TDynamicTime();  // { ConstructMetTime( 60 ); }
    TMetTime( const int timeStepInMinutes )
     : TDynamicTime() { ConstructMetTime( timeStepInMinutes ); }
    TMetTime ( const long datePart, const long timePart )
    : TDynamicTime ( datePart,  timePart ) { SetTimeStep( 60 ); }
    TMetTime( const TMetTime& aMetTime )
     : TDynamicTime( aMetTime ) { SetTimeStep( aMetTime.GetTimeStep() ); }

    TMetTime( const TStaticTime& aTime )
     : TDynamicTime( aTime ) { ConstructMetTime( 60 ); }
    TMetTime( const short year, const short month, const short day )
     : TDynamicTime( year, month, day ) { ConstructMetTime( 60 ); }
    TMetTime( const short year, const short month, const short day
	    , const short hour, const short minute=0, const short sec=0 )
     : TDynamicTime( year, month, day
	       , hour, minute, sec ) { ConstructMetTime( 60 ); }

    TMetTime& operator= ( const TMetTime& );
    TMetTime  operator++ ( void );          // prefix
    TMetTime  operator++ ( int );           // postfix
    TMetTime  operator-- ( void );          // prefix
    TMetTime  operator-- ( int );           // postfix

    void NextMetTime( void );
    void NextMetTime( const short deltaInMinutes );
    void PreviousMetTime( void );
    void PreviousMetTime( const short deltaInMinutes );
    void NearestMetTime( void );
    void NearestMetTime( const short deltaInMinutes );
    // I think these should change the internal value of the object, but return the
    // new value, leaving the object unchanged values! For the sake of clarity, should be
    // GetNextObservation (), Get ...
//    TMetTime NextObservation( const short deltaInMinutes ) const;
//    TMetTime PreviousObservation( const short deltaInMinutes ) const;
//    TMetTime NearestObservation( const short deltaInMinutes ) const;

    void  SetTimeStep( const short timeStepInMinutes );
    short GetTimeStep( void ) const;

    short GetLocalHour () const;

 private:
    void  ConstructMetTime( const short timeStepInMinutes );

 private:
    short fTimeStepInMinutes;
};

#endif //__FMETTIM_H__
