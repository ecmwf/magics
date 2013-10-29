/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvObsSet.cc,   vk July94
//            rev vk 980306

//--------------------------------------------------------------------
//  MvObsSet hides the data structures and parameter driven subroutines
//  needed to access BUFR files by wrapping them to a nicer C++ cover.
//--------------------------------------------------------------------
//  Routines and required data structures are described in
//  ECMWF Meteorological Bulletin:
//
//  "Accessing GRIB and BUFR data."
//       by J.D.Chambers
//
//  May 1994 Original version.
//--------------------------------------------------------------------

#include "inc_iostream.h"
#include "MvObsSet.h"

#ifdef METVIEW_PREPBUFR
# include "MvPrepBufrPrep.h"
#endif

#ifndef METVIEW
#include "grib_api.h"
long _readbufr(FILE *f, char *b, long *l)  // from mars/tools.c
{
	size_t len = *l;
	long e =  wmo_read_any_from_file(f,(unsigned char*)b,&len);
	*l = len;
	return e;
}
#endif

int MAX_MESSAGE_LENGTH = 32000;

const int    cMSG_TYPE_BUFR_TABLES = 11;

static string WRITE("w");  // i/o mode

//____________________________________________________________________
//==================================================================== MvObsSet
//____________________________________________________________________

MvObsSet :: MvObsSet( const char *aName )
 : _minTime( 2247, 6, 20 )
 , _maxTime( 1799, 12, 31 )
{
   _IO_mode = "r";
   _init( aName );
}
//____________________________________________________________________

MvObsSet :: MvObsSet( const char *aName, const char *aMode )
 : _minTime( 2247, 6, 20 )
 , _maxTime( 1799, 12, 31 )
{
   _IO_mode = aMode;
   _init( aName );
}
//____________________________________________________________________
#ifdef METVIEW
MvObsSet :: MvObsSet( MvRequest &aRequest, const char *aMode )
 : _minTime( 2247, 6, 20 )
 , _maxTime( 1799, 12, 31 )
{
   _IO_mode = aMode;
   const char *aName = 0;
   aRequest.getValue( aName, "PATH" );
   _init( aName );
}
#endif
//____________________________________________________________________

MvObsSet :: ~MvObsSet()
{
   close();
   delete [] _message;    // added 950201/vk
   if( _bufrOut )
     delete _bufrOut;
#ifdef METVIEW_PREPBUFR
   if( prepBufr_  )
     delete prepBufr_;
#endif

}
//____________________________________________________________________
void
MvObsSet :: _init( const  char *aName )
{
   _bufrFile = 0;
   _msgCount = -1;
   _obsCount = -1;
   _msgNumber = -1;
   _msgLen = 0;
   _minMaxDone = false;
  // _message = new char[ MAX_MESSAGE_LENGTH ];
   _IO_buffer_OK = false;
#ifdef METVIEW_PREPBUFR
   isPrepBufrFile_ = false;
   prepBufr_ = 0;
#endif

   Open( aName );

   if( _IO_mode == WRITE )  //should be two classes!!!!!!! (but this a working one...)
   {
     _message = 0;
     _bufrOut = new MvBufrOut( MAX_MESSAGE_LENGTH, this );  //( _message, MAX_MESSAGE_LENGTH, this );
   }
   else
   {
     _message = new char[ MAX_MESSAGE_LENGTH ];
     _bufrOut = 0;

#ifdef METVIEW_PREPBUFR
#ifdef METVIEW
     cout << "MvObsSet::_init - PrepBUFR support available, testing if PrepBUFR file..." << endl;
#endif
     firstObs_ = next();                    //-- check if PrepBUFR file (contains BUFR tables)
     isPrepBufrFile_ = ( firstObs_.messageType() == cMSG_TYPE_BUFR_TABLES );
     rewind();

//aki:
//  1) test if PrepBUFR tables have already been extracted: MV_PREPBUFR_TABLES_EXTRACTED
//  2) extract only if not yet extracted
     if( isPrepBufrFile_ && ! getenv("MV_PREPBUFR_TABLES_EXTRACTED") )
     {
        prepBufrFile();
        putenv("MV_PREPBUFR_TABLES_EXTRACTED=YES");
     }
#else
     cout << "MvObsSet::_init - PrepBUFR support NOT available!!!!" << endl;
#endif
   }
   //cout << "in MvObsSet::_init(" << aName << ")" << endl;
}
//____________________________________________________________________ setSubsetMax
void
MvObsSet :: setSubsetMax( int subsetMax )
{
  //_bufrOut->_maxNrSubsets = subsetMax; //replaced vk 941219
  _bufrOut->setSubsetCount( subsetMax );
}
//____________________________________________________________________ Open
boolean
MvObsSet :: Open( const char* aFileName )
{
   _msgCount = -1;
   _msgNumber = 0;

   _bufrFile = fopen( aFileName, _IO_mode.c_str() );

   if( _bufrFile )
      return true;
   else
   {
      cerr << " >>> MvObsSet::Open: file \'" << aFileName << "\', not opened!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "Unable to open file %s", aFileName );
#endif
      return false;
   }
}
//____________________________________________________________________ close
boolean
MvObsSet :: close()
{
   long myReturnValue = -1;
   if( _bufrFile )
   {
     if( _IO_mode == WRITE && _bufrOut->_outState == kBufrOut_dataInBuffers )
     {
       _bufrOut->encode();
     }

     myReturnValue = fclose( _bufrFile);
     _bufrFile = 0;
   }
   _IO_buffer_OK = false;
   return myReturnValue ? false : true;
}
//____________________________________________________________________ rewind
void
MvObsSet :: rewind()
{
   _msgNumber = 0;
   if( _bufrFile )
     ::rewind( _bufrFile );  // execute C command 'rewind'!
   else
     cerr << " >>> MvObsSet::rewind: File Unit not valid!" << endl;
}

//____________________________________________________________________ next
// Reads next BUFR message and returns an MvObs constructed with it.
// An MvObs without a message is returned at EOF.
//------------------------------------------------
MvObs
MvObsSet::next()
{
   const int EOF_STATUS = -1;

   if( ! _bufrFile )
     return MvObs( NULL );  // nothing if file not ok

   if( _IO_mode == WRITE )
     return MvObs( NULL );  // no next when writing !

   _msgLen = MAX_MESSAGE_LENGTH;

   _msgNumber++;

   long lastPos = ftell(_bufrFile);
   long myError = _readbufr( _bufrFile, _message, &_msgLen );

   if ( myError == -3 )  // Bufr too small
     {

       // Go back to previous and allocate memory
       fseek(_bufrFile,lastPos,SEEK_SET);
       delete [] _message;
       MAX_MESSAGE_LENGTH = _msgLen + 8;
       _message = new char[ MAX_MESSAGE_LENGTH ];
       _msgLen = MAX_MESSAGE_LENGTH;

       if ( ! _message )
	 {
	   cerr << "MvObsSet::next: Cannot allocate memory for next BUFR message" << endl;
#ifdef METVIEW
	   marslog( LOG_EROR, "Cannot allocate memory for next BUFR message" );
#endif
	   _msgLen = MAX_MESSAGE_LENGTH  = 0;
	   _IO_buffer_OK = false;
	   return MvObs( NULL );
	 }
       else
	 {
	   cout << "MvObsSet::next: Allocated more memory for BUFR msg" << endl;
	   myError = _readbufr( _bufrFile, _message, &_msgLen );
	 }
     }

   if( myError )
     {
       if( myError != EOF_STATUS )
	 {
	   cerr << "MvObsSet::next: Failed reading next BUFR msg, returned status=" << myError << endl;
#ifdef METVIEW
	   marslog( LOG_EROR, "Failed reading next BUFR msg, status = %ld", myError );
#endif
	 }
       _IO_buffer_OK = false;
       return MvObs( NULL );
     }

#ifdef METVIEW_PREPBUFR
   //--
   //-- NCEP PrepBUFR files may contain msgs with ZERO subsets!!!
   //-- 'bufrex' cannot handle such illegal msgs, if so we must skip it
   //--
   MvBufr tmpBufr( _message, _msgLen, _msgNumber  ); //-- make BUFR octets into an object
   if( tmpBufr.subsetCount() == 0 )
     {
       ostringstream os;
       os << "Original BUFR msg " << _msgNumber
          << " has ZERO subsets - ignoring (not counting) this illegal msg!"
          << ends;

       cout << os.str() << endl;

#ifdef METVIEW
       //-- why this marslog crashes ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
//       marslog( LOG_WARN, "%s", os.str().c_str() );
       //-- why this marslog crashes ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
#endif

       //-- get next msg and cross your fingers it fits into current _message array
       _msgLen = MAX_MESSAGE_LENGTH;
       myError = _readbufr( _bufrFile, _message, &_msgLen );

       //_msgNumber++;
     }
#endif

     _IO_buffer_OK = true;
     return MvObs( new MvBufr( _message, _msgLen, _msgNumber ) );
}

//____________________________________________________________________ add
void
MvObsSet::add( MvObs& anObs )
{
   if( _IO_mode != WRITE )
     return;  // no add when reading !

   _bufrOut->add( anObs );
   _msgCount++; //??
}
//____________________________________________________________________ write
void
MvObsSet::write( const char* aMsg, int aMsgLen )
{
  if( _IO_mode != WRITE )
    return;  // no write when reading !

  fwrite( aMsg, sizeof( char ), aMsgLen, _bufrFile );
  _msgNumber++;
}

//____________________________________________________________________ messageCount
// Returns the number of BUFR messages found in the file.
// WARNING: a packed BUFR message may contain several observations
//          i.e. the nr of BUFR msgs found <= nr of observation msgs
//          i.e. messageCount() <= obsCount()
//------------------------------------------------------------------
int
MvObsSet :: messageCount()
{
   if( _msgCount < 1 )
   {
      if( _bufrFile )
      {
         long myOriginalFilePos = ftell( _bufrFile );
	 rewind();

         _msgCount = 0;
         while( next() )
	    _msgCount++;

	 fseek( _bufrFile, myOriginalFilePos, SEEK_SET );
      }
   }
   return _msgCount;
}
//____________________________________________________________________ obsCount
int
MvObsSet :: obsCount()
{
   if( _obsCount < 1 )
   {
      if( _bufrFile )
      {
         long myOriginalFilePos = ftell( _bufrFile );
	 rewind();

         _obsCount = 0;
	 MvObs oneBufrMsg;
         while( ( oneBufrMsg = next() ) )
	    _obsCount += oneBufrMsg._bufrIn->subsetCount();

	 fseek( _bufrFile, myOriginalFilePos, SEEK_SET );
      }
   }
   return _obsCount;
}
//_________________________________________________________ searchMinMaxTime
void
MvObsSet :: searchMinMaxTime()
{
   if( ! _minMaxDone )
   {
      if( _bufrFile )
      {
         long myOriginalFilePos = ftell( _bufrFile );   rewind();

         TDynamicTime msgTime;
         MvObs myObs;
         while( ( myObs = next() ) )
         {
            msgTime = myObs.msgTime();
            if( msgTime > _maxTime )
                _maxTime = msgTime;
            if( msgTime < _minTime )
                _minTime = msgTime;
         }

         fseek( _bufrFile, myOriginalFilePos, SEEK_SET );

      }
      _minMaxDone = true;
   }
}
#ifdef METVIEW_PREPBUFR
//____________________________________________________________________ prepBufrFile
bool
MvObsSet::prepBufrFile()
{
   if( isPrepBufrFile_ )
   {
#ifdef METVIEW
      cout << "in MvObsSet::prepBufrFile()" << endl;
      marslog( LOG_INFO, "BUFR file contains BUFR tables, processing..." );
#endif
                                            //-- create new PrepBUFR table files etc...
      prepBufr_ = new MvPrepBufrPrep( *this );
      bool ok = prepBufr_->prepareAll();
      if( !ok )
      {
#ifdef METVIEW
         marslog( LOG_EROR, "Unable to process PrepBUFR file" );
#endif
         cerr << "Errors: unable to process the PrepBUFR file" << endl;
         return false;
      }
   }
   return isPrepBufrFile_;
}
#endif

//____________________________________________________________________ minDate
#ifdef METVIEW
MvDate
MvObsSet :: minDate()
{
   searchMinMaxTime();
   double timfloat = 10000.0L*(double)_minTime.GetYear()
                     + 100.0L*(double)_minTime.GetMonth()
                     +        (double)_minTime.GetDay()
                     +        (double)_minTime.GetHour()/24.0L
                     +        ((double)_minTime.GetMin()+0.5L)/60.0L/24.0L; //rounded
   return MvDate( timfloat );
}
//____________________________________________________________________ maxDate
MvDate
MvObsSet :: maxDate()
{
   searchMinMaxTime();
   double timfloat = 10000.0L*(double)_maxTime.GetYear()
                     + 100.0L*(double)_maxTime.GetMonth()
                     +        (double)_maxTime.GetDay()
                     +        (double)_maxTime.GetHour()/24.0L
                     +        ((double)_maxTime.GetMin()+0.5L)/60.0L/24.0L; //rounded
   return MvDate( timfloat );
}
#endif

/*-------
//____________________________________________________________________ operator +=
// use 'add' member function instead!
void
MvObsSet :: operator += ( MvObs anObs )
{
   if( _IO_mode == "r" )
     return;  // no writing for read files !

   cout << "MvObsSet::operator+= not yet implemented..." << endl;
}
-------*/
//___________________________________________________________________
//=================================================================== MvObsSetIterator
//___________________________________________________________________

MvObsSetIterator :: MvObsSetIterator( MvObsSet& s)
{
   ObsSet = &s;

   _NoFiltersSet = true;
   _TimeFilterState = kTFS_notSet;
   _WmoBlockCount = 0;
   _WmoStationCount = 0;
   _MsgTypeCount = 0;
   _MsgSubtypeCount = 0;
   _SelectState = SF_notSet;
   _SelectValueCount = 0;
   useObsTime_ = false;
}
//___________________________________________________________________

MvObsSetIterator :: ~MvObsSetIterator()
{
}
//___________________________________________________________________ operator()


MvObs MvObsSetIterator :: operator() ( ENextReturn returnType )
{
  if( ! current || ( returnType == NR_returnMsg ) || ! current.Advance() )
    next();

  while( current && ! AcceptedObs( current ) )
  {
    if( ! current || ( returnType == NR_returnMsg ) || ! current.Advance() )
      next();
  }
  return current;
}
//____________________________________________________________________ next

void
MvObsSetIterator :: next()
{
  static int change_me_with_debugger = 0;

  current = ObsSet->next();

#ifdef METVIEW
  if( mars.debug && ( ( ObsSet->messageNumber() == 1 ) || change_me_with_debugger ) )
    current.printAllValues();
#endif
}
//____________________________________________________________________ setTime

void
MvObsSetIterator :: setTime( const TDynamicTime& anObsTime )
{
   setTimeRange( anObsTime, anObsTime );
}
//____________________________________________________________________

void
MvObsSetIterator :: setTimeRange( const TDynamicTime& anObsTime, short deltaInMinutes )
{
   fBeginTime = fEndTime = anObsTime;
   fBeginTime.ChangeByMinutes( -deltaInMinutes );
   fEndTime.ChangeByMinutes( deltaInMinutes );
   _TimeFilterState = kTFS_bothSet;
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setTimeRange( const TDynamicTime& aBeginTime, const TDynamicTime& anEndTime )
{
   fBeginTime = aBeginTime;
   fEndTime = anEndTime;
   _TimeFilterState = kTFS_bothSet;
   _NoFiltersSet = false;
}
//_____________________________________________________________ setTimeRangeWithoutDate
// add 941229/vk
//
// parameters: in format HHMM, i.e. 1200 == 12.00, 15 == 0.15, i.e. 100*hour+min!!!
//             values are normalized into range [0000..2400)
//_________________________________________
void
MvObsSetIterator :: setTimeRangeWithoutDate( int aBegin, int anEnd )
{
   TDynamicTime aTime;  // date part is not used, so let's use today..
   int myBegin = aBegin;
   int myEnd   = anEnd;

   while( myBegin < 0 ) myBegin += 2400;
   aTime.SetTime( myBegin / 100, myBegin % 100, 0 );
   fBeginTime = aTime;

   while( myEnd >= 2400 ) myEnd -= 2400;
   aTime.SetTime( myEnd / 100, myEnd % 100, 0 );
   fEndTime = aTime;

   _TimeFilterState = kTFS_clockSet;
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setWmoBlock( int aWmoBlockNumber )
{
   if( _WmoBlockCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      cerr << ">>> MvObsIterator::setWmoBlock: array overflow!!!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "MvObsIterator::setWmoBlock: array overflow!" );
#endif
      return;
   }
   _WmoBlockNumber[ _WmoBlockCount++ ] = aWmoBlockNumber;
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setWmoStation( long aWmoStation )
{
   if( _WmoStationCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      cerr << ">>> MvObsIterator::setWmoStation: array overflow!!!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "MvObsIterator::setWmoStation: array overflow!" );
#endif
      return;
   }
   _WmoStation[ _WmoStationCount++ ] = aWmoStation;
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: select( long aDescriptor
			  , float aValue )
{
   if( _SelectValueCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      cerr << ">>> MvObsIterator::select: array overflow!!!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "MvObsIterator::select: array overflow!" );
#endif
      return;
   }
   if( _SelectValueCount > 0 && aDescriptor != _SelectDescriptor )
   {
      cerr << ">>> MvObsIterator::select: changing the descriptor while building the list!!!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "MvObsIterator::select: changing the descriptor while building the list!" );
#endif
   }
  _SelectDescriptor = aDescriptor;
  _SelectValue[ _SelectValueCount++ ] = aValue;
  _SelectState = SF_listSet;
  _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: selectRange( long aDescriptor
			       , float firstValue
			       , float secondValue )
{
  _SelectDescriptor = aDescriptor;
  _SelectValue[ 0 ] = firstValue < secondValue ? firstValue : secondValue;
  _SelectValue[ 1 ] = secondValue > firstValue ? secondValue : firstValue;
  _SelectValueCount = 2;
  _SelectState = SF_rangeSet;
  _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: excludeRange( long aDescriptor
			        , float firstValue
			        , float secondValue )
{
  _SelectDescriptor = aDescriptor;
  _SelectValue[ 0 ] = firstValue < secondValue ? firstValue : secondValue;
  _SelectValue[ 1 ] = secondValue > firstValue ? secondValue : firstValue;
  _SelectValueCount = 2;
  _SelectState = SF_excludeRangeSet;
  _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setXSectionLine( const MvLocation& aStartPoint
			      , const MvLocation& anEndPoint
			      , float aDeltaInMeters )
{
   fXSectionLine.setLine( aStartPoint, anEndPoint );
   fXSectionLine.setMaxDelta( aDeltaInMeters );
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setArea( const MvLocation& aCorner1, const MvLocation& aCorner2 )
{
   fArea.set( aCorner1, aCorner2 );
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setMessageType( int aMsgType )
{
   if( _MsgTypeCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      cerr << ">>> MvObsIterator::setMessageType: array overflow!!!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "MvObsIterator::setMessageType: array overflow!" );
#endif
      return;
   }
   _MsgType[ _MsgTypeCount++ ] = aMsgType;
   _NoFiltersSet = false;
}
//____________________________________________________________________
void
MvObsSetIterator :: setMessageSubtype( int aMsgSubtype )
{
   if( _MsgSubtypeCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      cerr << ">>> MvObsIterator::setMessageSubtype: array overflow!!!" << endl;
#ifdef METVIEW
      marslog( LOG_EROR, "MvObsIterator::setMessageSubtype: array overflow!" );
#endif
      return;
   }
   _MsgSubtype[ _MsgSubtypeCount++ ] = aMsgSubtype;
   _NoFiltersSet = false;
}
//____________________________________________________________________
float
MvObsSetIterator :: distanceFromXSectionLine( const MvLocation& aPoint )
{
   if( fXSectionLine.startPoint().latitude() == MISSING_LOC_VALUE )
   {
      return MISSING_LOC_VALUE;
   }
   return fXSectionLine.deltaInMeters( aPoint );
}
//____________________________________________________________________
boolean
MvObsSetIterator :: TimeOk( MvObs *anObs ) const
{
   switch( _TimeFilterState )
   {
   case kTFS_notSet:
     break;   //return true;

   case kTFS_clockSet:
     {
       long obsTime;
       if( useObsTime_ )
	 obsTime = anObs->obsTime().ClockInSeconds();  //-- need to decode => slow
       else
	 obsTime = anObs->msgTime().ClockInSeconds();  //-- use metadata => fast

       long time1 = fBeginTime.ClockInSeconds();
       long time2 = fEndTime.ClockInSeconds();

       if( time1 <= time2 )        // e.g.  12-18
       {
         if( obsTime < time1 || obsTime > time2 )
	   return false;
       }
       else    // time1 > time2       e.g.  21-03
       {
          if( obsTime < time1 && obsTime > time2 )
	    return false;
       }
     }
     break;

   case kTFS_bothSet:
     {
       TDynamicTime myTime;
       if( useObsTime_ )
	 myTime = anObs->obsTime();  //-- time from obs => decode first => slow
       else
	 myTime = anObs->msgTime();  //-- time from sec1 => no decode => fast

       if( myTime < fBeginTime || myTime > fEndTime )
          return false;
     }
     break;
   }

   return true;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: WmoBlockOk( MvObs *anObs ) const
{
  if( _WmoBlockCount < 1 )
    return true;

  for( int i = 0; i < _WmoBlockCount; i++ )
    if( anObs->WmoBlockNumber() == _WmoBlockNumber[ i ] )
      return true;

  return false;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: WmoStationOk( MvObs *anObs ) const
{
   if( _WmoStationCount < 1 )
      return true;

  for( int i = 0; i < _WmoStationCount; i++ )
    if( anObs->WmoIdentNumber() == _WmoStation[ i ] )
      return true;

  return false;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: WithinXSectionLine( MvObs *anObs ) const
{
   if( fXSectionLine.maxDelta() < 0 )    // not set ?
      return true;

   if( fXSectionLine.withinDelta( anObs->location() ) )
     return true;
   else
     return false;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: InsideArea( MvObs *anObs ) const
{
   if( fArea.lowerLeft().latitude() == MISSING_LOC_VALUE )
     return true;
   else
     return fArea.inside( anObs->location() );
}
//____________________________________________________________________
boolean
MvObsSetIterator :: msgTypeOk( MvObs *anObs ) const
{
   if( _MsgTypeCount < 1 )
      return true;

  for( int i = 0; i < _MsgTypeCount; i++ )
    if( anObs->messageType() == _MsgType[ i ] )
      return true;

  return false;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: msgSubtypeOk( MvObs *anObs ) const
{
   if( _MsgSubtypeCount < 1 )
      return true;

  for( int i = 0; i < _MsgSubtypeCount; i++ )
    if( anObs->messageSubtype() == _MsgSubtype[ i ] )
      return true;

  return false;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: selectOk( MvObs *anObs ) const
{
  if( _SelectState == SF_notSet )
    return true;

  float myValue = anObs->value( _SelectDescriptor );
  if( myValue == kBufrMissingValue )
    return false;

  switch( _SelectState )
  {
    case SF_notSet:

        break;

    case SF_listSet:

        {
          for( int i = 0; i < _SelectValueCount; i++ )
            if( myValue == _SelectValue[ i ] )
              return true;
        }
        return false;

    case SF_rangeSet:

        if( myValue < _SelectValue[ 0 ] || myValue > _SelectValue[ 1 ] )
          return false;
        break;

    case SF_excludeRangeSet:

        if( myValue >= _SelectValue[ 0 ] && myValue <= _SelectValue[ 1 ] )
          return false;
        break;

  }
  return true;
}
//____________________________________________________________________
boolean
MvObsSetIterator :: AcceptedObs( MvObs& anObs ) const
{
   if( _NoFiltersSet )
      return true;

   if( ! msgTypeOk( &anObs ) )         // from Section 1: no decoding required
      return false;
   if( ! msgSubtypeOk( &anObs ) )      // from Section 1: no decoding required
      return false;

   if( ! TimeOk( &anObs ) )            // from Section 1: no decoding required
      return false;

   if( ! WmoBlockOk( &anObs ) )        // others require decoding of the msg
      return false;
   if( ! WmoStationOk( &anObs ) )
      return false;

   if( ! selectOk( &anObs ) )
      return false;

   if( ! WithinXSectionLine( &anObs ) )
      return false;

   if( ! InsideArea( &anObs ) )
      return false;

   return true;
}
//_____________________________________________________________ operator<<

ostream& operator<< ( ostream& aStream, const MvObsSetIterator& aFilter )
{
 int i;

  aStream << "Observation Filter values set:\n";
  if( aFilter._NoFiltersSet )
  {
    aStream << "   No filter values set!";
    aStream << endl;
  }
  else
  {
    if( aFilter._TimeFilterState != kTFS_notSet )
    {
      aStream << "   Timerange: ";
      switch( aFilter._TimeFilterState )
      {
      case kTFS_notSet:
        aStream << "[not set!]";
        break;

      case kTFS_clockSet:
        aStream << aFilter.fBeginTime.GetHour() << ".";
        aStream.width(2); aStream.fill('0');
        aStream << aFilter.fBeginTime.GetMin();
        if( aFilter.fBeginTime != aFilter.fEndTime )
        {
          aStream << " - " << aFilter.fEndTime.GetHour() << ".";
          aStream.width(2); aStream.fill('0');
          aStream << aFilter.fEndTime.GetMin();
        }
        break;

      case kTFS_bothSet:
        aStream << aFilter.fBeginTime;
        if( aFilter.fBeginTime != aFilter.fEndTime )
          aStream << " - " << aFilter.fEndTime;
        break;
      }
      aStream << endl;
    }

    if( aFilter._MsgTypeCount > 0 )
    {
      aStream << "   Message types: ";
      for( i=0; i < aFilter._MsgTypeCount; i++ )
        aStream << " " << aFilter._MsgType[ i ];
      aStream << endl;
    }

    if( aFilter._MsgSubtypeCount > 0 )
    {
      aStream << "   Message subtypes: ";
      for( i=0; i < aFilter._MsgSubtypeCount; i++ )
        aStream << " " << aFilter._MsgSubtype[ i ];
      aStream << endl;
    }

    if( aFilter._WmoBlockCount > 0 )
    {
      aStream << "   WMO Blocks:";
      for( i=0; i < aFilter._WmoBlockCount; i++ )
        aStream << " " << aFilter._WmoBlockNumber[ i ];
      aStream << endl;
    }

    if( aFilter._WmoStationCount > 0 )
    {
      aStream << "   WMO Stations:";
      for( i=0; i < aFilter._WmoStationCount; i++ )
        aStream << " " << aFilter._WmoStation[ i ];
      aStream << endl;
    }

    if( aFilter._SelectState != SF_notSet )
    {
      aStream << "   Select ";
      switch( aFilter._SelectState )
      {
       case SF_notSet:
         aStream << "by values/range: [not set!]";
         break;

       case SF_listSet:
         aStream << "by values: ";
         break;

       case SF_rangeSet:
         aStream << "by range: ";
         break;

       case SF_excludeRangeSet:
         aStream << "by excluding range: ";
         break;
      }

      for( i=0; i < aFilter._SelectValueCount; i++ )
        aStream << aFilter._SelectValue[ i ] << " ";
      aStream << "(descr. " << aFilter._SelectDescriptor << ")";
      aStream << endl;
    }

    if( aFilter.fXSectionLine.startPoint().latitude() != MISSING_LOC_VALUE )
    {
      aStream << "   Cross Section Line: " << aFilter.fXSectionLine;
      aStream << endl;
    }

    if( aFilter.fArea.lowerLeft().latitude() != MISSING_LOC_VALUE )
    {
      aStream << "   Area: " << aFilter.fArea;
      aStream << endl;
    }
   // aStream << endl;  // calling method should add the final 'endl'!
  }
  return aStream;
}
