/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvObsSet.h


#ifndef MvObsSet_DEFINED
#define MvObsSet_DEFINED

//-- <aki> temporarily for testing only; should be done via 'configure'
#define METVIEW_PREPBUFR

#include "MvBufrObs.h"

class MvPrepBufrPrep;

//--------------------------------------------------------------- MvObsSet
//! A class to access a file containing observation reports
/*! This class is used to handle a file containing several
 *  observation reports stored as BUFR messages.
 */
class MvObsSet
{
 friend class MvObsSetIterator;
 friend class MvBufrOut;

	// prevent copy

	MvObsSet( const MvObsSet& );
	void operator= ( const MvObsSet& );
	void _init( const char* aFile );

 public:

//! Constructor with the input BUFR file name as an argument
   MvObsSet( const char* fileName );

//! Constructor with a BUFR file name and access mode as arguments
/*! This constructor can be used for writing to a file, use
 *  access mode "w" or "a".
 */
   MvObsSet( const char* fileName, const char* accessMode );

#ifdef METVIEW
//! Constructor with a BUFR request and access mode as arguments
    MvObsSet( MvRequest&, const char* aMode = "r" );

//! Returns the minimum (earliest) date found in MvObsSet
    MvDate  minDate();

//! Returns the maximum (latest) date found in MvObsSet
    MvDate  maxDate();
#endif

//! Destructor
   ~MvObsSet();

//! Set max subset count for a new file
      void  setSubsetMax( int subsetMax );

//! Add one observation report to output file
      void  add( MvObs& anObs );

//! Rewind the input file
      void  rewind();

//! Returns the number of BUFR messages in the input file
/*! This number is smaller than returned by method obsCount()
 *  if the input file contains multisubset BUFR messages.
 */
       int  messageCount(); // nr of BUFR-messages in a file

//! Returns the number of observation reports in the input file
/*! This number is bigger than returned by method messageCount()
 *  if the input file contains multisubset BUFR messages.
 */
       int  obsCount();     // nr of observation messages in a BUFR-file

//! Returns the (running) number of the current BUFR message
       int  messageNumber(){ return (int)_msgNumber; }

//! Closes the input/output file
   boolean  close();

//! Prepares 'PrepBUFR' tables if the input file contains BUFR tables
      bool  prepBufrFile();

            //-- hack for ObsFilter (vk 970729) --
      void  write( MvObs& anObs ){ if(_bufrOut) _bufrOut->write( anObs ); }

     MvObs& firstObs(){ return firstObs_; }

 protected:
     MvObs  next();
   boolean  Open( const char* fileName );  //, char* aMode = "r" );
      void  write( const char* aMsg, int aMsgLen );
      void  searchMinMaxTime();
 //Logical  currentInputBufferOK() {return _IO_buffer_OK;}

 protected:
     string  _IO_mode;
       FILE* _bufrFile;
       long  _msgNumber;
       long  _msgCount;
       long  _obsCount;
   TDynamicTime  _minTime;
   TDynamicTime  _maxTime;
    boolean  _minMaxDone;
       long  _msgLen;
       char* _message;
       bool  _IO_buffer_OK;

  MvPrepBufrPrep* prepBufr_;
       bool  isPrepBufrFile_;
      MvObs  firstObs_;

  MvBufrOut* _bufrOut;
};

//------------------------------------------------------------------------------

const int MAX_FILTER_LIST_ARRAY_SIZE = 100;

//! \enum ENextReturn Mnemonic names to define subset access
/*! Used in MvObsSetIterator::operator() to tell which
 *  observation report to return. \n \n
 *  Use NR_returnMsg to skip the remaining observation reports
 *  in a multisubset BUFR message that does not contain<aki>
 *  These mnemonics have an effect only on multisubset
 *  BUFR messages. For single subset BUFR messages both
 *  mnemonics function the same.
 */
enum ENextReturn
{
   NR_returnObs  /**< - return the next available subset (from this or the next BUFR msg */
  ,NR_returnMsg  /**< - return the first subset from the next BUFR message */
};

//! \enum ESelectFilterState Used internally by MvObsSetIterator
enum ESelectFilterState
{
   SF_notSet
  ,SF_listSet
  ,SF_rangeSet
  ,SF_excludeRangeSet
};

//! \enum ETimeFilterState Used internally by MvObsSetIterator
enum ETimeFilterState
{
   kTFS_notSet
  ,kTFS_clockSet
  ,kTFS_bothSet
};

//------------------------------------------------------------ MvObsSetIterator
//! A class to filter a set of observation reports
/*! This class is used to filter a file containing several
 *  observation reports stored in BUFR messages. Several
 *  different filtering criteria are available. When several
 *  filtering criteria are set the resulting filter will use
 *  logical AND to combine different criteria.
 */
class MvObsSetIterator
{
//! Writes the current filtering options of 'anIter' into stream 'aStream'
 friend ostream& operator<< ( ostream& aStream, const MvObsSetIterator& anIter );

	MvObs     current;
	MvObsSet* ObsSet;

 public:
//! Constructor with a MvObset as an argument
	MvObsSetIterator( MvObsSet& );
//! Destructor
	~MvObsSetIterator();

//! Operator that returns the next available valid observation report
/*! Subset structure is hidden from the calling application.
 */
	MvObs operator() () { return operator()( NR_returnObs ); }

//! Operator that returns the next valid observation report
/*! Using mnemonic argument 'NR_returnMsg' accesses only first
 *  subsets in multisubset BUFR messages. See enum ENextReturn.
 */
	MvObs operator() ( ENextReturn returnType );

//! Returns the sequence number of the current BUFR message
     int   msgNumber() { return (ObsSet ? (int)ObsSet->_msgNumber : -1); }

//! Returns the subset number of the current observation report in the BUFR message
     int   subsetNumber() { return current.subsetNumber(); }

//! Defines where the date/time for filtering should be taken from
/*! Date and time information is always available as metadata in
 *  BUFR section 1, and normally also in the data (observation report)
 *  itself. In single subset messages these times should be the same,
 *  but for some multisubset messages observation reports may come from
 *  different times. \n \n
 *  The default is to use date/time from the metadata in section 1 (faster),
 *  but by calling this method it is possible to change to use date/time
 *  from the data.
 */
     void  useObsTime( bool b = true ){ useObsTime_ = b; }

//! Set the filtering criteria for a single date/time
     void  setTime( const TDynamicTime& anObsTime );

//! Set the filtering criteria for a date/time range
/*! Observation reports that have their data/time within the range
 *  'anObsTime'-'deltaInMinutes' and 'anObsTime'+'deltaInMinutes'
 *  will be selected.
 */
     void  setTimeRange( const TDynamicTime& anObsTime, short deltaInMinutes );

//! Set the filtering criteria for a date/time range
     void  setTimeRange( const TDynamicTime& aBeginTime, const TDynamicTime& anEndTime );

//! Set the filtering criteria for time range only (date ignored)
/*! Observation reports that have their time stamp between 'aBeginTime'
 *  and 'anEndTime' will be selected. \n \n
 *  Time format is HHMM, i.e. 100*hours+minutes. Thus the value 1500
 *  signifies 3PM, and the value 15 signifies 15 mins past midnight.
 */
     void  setTimeRangeWithoutDate( int aBegin, int anEnd );

//! Set the filtering criteria for accepted WMO block numbers
/*! This method can be called several times in order to set
 *  more than one accepted WMO block, i.e. WMO blocks are
 *  combined with logical OR operator.
 */
     void  setWmoBlock( int aWmoBlock );

//! Set the filtering criteria for accepted WMO station numbers
/*! This method can be called several times in order to set
 *  more than one accepted WMO station, i.e. WMO stations are
 *  combined with logical OR operator.
 */
     void  setWmoStation( long aWmoStation );

//! Set the filtering criteria for an accepted area
/*! Observation report locations must be within the given area.
 */
     void  setArea( const MvLocation& aCorner1, const MvLocation& aCorner2 );

//! Set filtering criteria for a proximity of a cross section line
     void  setXSectionLine( const MvLocation& aStartPoint
			  , const MvLocation& anEndPoint
			  , float aDeltaInMeters );

//! Set the filtering criteria for accepted BUFR message types
/*! This method can be called several times in order to set
 *  more than one accepted BUFR message type, i.e. message types are
 *  combined with logical OR operator.
 */
     void  setMessageType( int aMsgType );

//! Set the filtering criteria for accepted BUFR message local subtypes
/*! This method can be called several times in order to set
 *  more than one accepted BUFR message local subtype, i.e. message subtypes
 *  are combined with logical OR operator.
 */
     void  setMessageSubtype( int aMsgSubtype );

//! Set the filtering criteria for data 'aDescriptor' having value 'aValue'
/*! Observation reports where the data corresponding to descriptor 'aDescriptor'
 *  has value 'aValue' will be selected.\n \n
 *  This method can be called several times in order to set more than one
 *  accepted value for the same 'aDescriptor', i.e. values are
 *  combined with logical OR operator.

 */
     void  select( long aDescriptor
		 , float aValue );


//! Set the filtering criteria for the given data having its value within a range
/*! Observation reports where the data corresponding to descriptor 'aDescriptor'
 *  has its value in the range 'firstValue'...'secondValue' will be selected.
 */
     void  selectRange( long aDescriptor
		      , float firstValue
		      , float secondValue );

//! Set the filtering criteria for the given data having its value outside a range
/*! Observation reports where the data corresponding to descriptor 'aDescriptor'
 *  has its value outside the range 'firstValue'...'secondValue' will be selected.
 */
     void  excludeRange( long aDescriptor
		       , float firstValue
		       , float secondValue );

 protected:

     void  next();
  boolean  AcceptedObs( MvObs& anObs ) const;
  boolean  TimeOk( MvObs* anObs ) const;
  boolean  WmoBlockOk( MvObs* anObs ) const;
  boolean  WmoStationOk( MvObs* anObs ) const;
  boolean  WithinXSectionLine( MvObs* anObs ) const;
  boolean  msgTypeOk( MvObs* anObs ) const;
  boolean  msgSubtypeOk( MvObs* anObs ) const;
  boolean  InsideArea( MvObs* anObs ) const;
  boolean  selectOk( MvObs* anObs ) const;
    float  distanceFromXSectionLine( const MvLocation& aPoint ); //-- used anywhere?

 protected:
         boolean  _NoFiltersSet;
	    bool  useObsTime_;
        TDynamicTime  fBeginTime, fEndTime;
      // boolean  fTimeFilterSet;
ETimeFilterState  _TimeFilterState;
	     int  _WmoBlockCount;
             int  _WmoBlockNumber[ MAX_FILTER_LIST_ARRAY_SIZE ];
	     int  _WmoStationCount;
            long  _WmoStation[ MAX_FILTER_LIST_ARRAY_SIZE ];
  MvXSectionLine  fXSectionLine;
          MvArea  fArea;
	     int  _MsgTypeCount;
	     int  _MsgType[ MAX_FILTER_LIST_ARRAY_SIZE ];
	     int  _MsgSubtypeCount;
	     int  _MsgSubtype[ MAX_FILTER_LIST_ARRAY_SIZE ];
 ESelectFilterState _SelectState;
	    long  _SelectDescriptor;
	   float  _SelectValue[ MAX_FILTER_LIST_ARRAY_SIZE ];
	     int  _SelectValueCount;
};

#endif
// MvObsSet_DEFINED
