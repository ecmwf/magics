/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// rev vk 970214 -------------------------- TStaticTime

#ifndef __TSTATICTIME_H__
#define __TSTATICTIME_H__

#include  "inc_iostream.h"
#include  "fsortabl.h"
#ifdef METVIEW
#include  "MvDate.h"
#endif


class TStaticTime : public TSortable
{
 friend ostream& operator<< ( ostream& oStream, const TStaticTime& myTime);

 public:
    TStaticTime ();
    TStaticTime (const TStaticTime&);
    TStaticTime (const short year, const short month, const short day);
    TStaticTime (const short year, const short month, const short day
		,const short hour, const short minute=0, const short sec=0);

    virtual bool IsEqual (const TFObject& anotherTime) const;
    virtual bool IsLessThan (const TFObject& anotherTime) const;

    TStaticTime& operator= (const TStaticTime&);
#ifdef METVIEW
    operator MvDate( void ) const { return MvDate( CharValue() ); }
#endif
    void  SetDate (const short year, const short month, const short day);
    void  GetDate (short &year, short &month, short &day) const;
    void  SetTime( const short hour, const short min=0, const short sec=0 );
    void  GetTime (short &hour, short &min, short &sec) const;

   short  GetYear( void ) const  { return fYear; }
   short  GetMonth( void ) const { return fMonth; }
   short  GetDay( void ) const   { return fDay; }
   short  GetHour( void ) const  { return fHour; }
   short  GetMin( void ) const   { return fMin; }
   short  GetSec( void ) const   { return fSec; }
   // short GetLocalHour () const;
   const char* CharDate() const;
   const char* CharHhMm() const;
   const char* CharValue( void ) const;
   const char* ShorterCharValue( void ) const;

    void  ReadDateTime ();                     // for testing
    void  XPrint (void) const;                 // for testing
    void  XPrint (const char *str) const;      // for testing
    virtual void Print (void) const;

 protected:
    void  SetYear (const short year);
    void  SetMonth (const short month);
    void  SetDay (const short day);
    void  SetHour (const short hour);
    void  SetMin (const short minute);
    void  SetSec (const short sec);
    void  _setCurrent( void );

 private:
    short   fYear;
    short   fMonth;
    short   fDay;
    short   fHour;
    short   fMin;
    short   fSec;
};

#endif //__TSTATICTIME_H__
