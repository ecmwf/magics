/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#ifndef MvObsSet_DEFINED
#define MvObsSet_DEFINED

//-- <aki> temporarily for testing only; should be done via 'configure'
//e #define METVIEW_PREPBUFR

#include <eccodes.h>
#include "MvBufrObs.h"

#include <vector>

//namespace mvObs {
   std::string simplified(const std::string&);
//}

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

   // Prevent copy
   MvObsSet( const MvObsSet& );
   void operator= ( const MvObsSet& );
   void _init( const char* );

 public:

//! Constructor with the input BUFR file name as an argument
   MvObsSet( const char*);

//! Constructor with a BUFR file name and access mode as arguments
/*! This constructor can be used for writing to a file, use
 *  access mode "w" or "a".
 */
   MvObsSet( const char*, const char*);

#ifdef METVIEW
//! Constructor with a BUFR request and access mode as arguments
   MvObsSet( MvRequest&, const char* aMode = "r" );

//! Returns the minimum (earliest) date found in MvObsSet
   MvDate minDate();

//! Returns the maximum (latest) date found in MvObsSet
   MvDate maxDate();
#endif

//! Destructor
   ~MvObsSet();

//FAMI20171016 - I think ecCodes does not need this function (_maxNrSubsets).
//               If this is the case delete it later.
//! Set max subset count for a new file
   void setSubsetMax( int );


//! Add one observation report to output file
   void add( MvObs& );

//! Rewind the input file
   void rewind();

//! Returns the number of BUFR messages in the input file
/*! This number is smaller than returned by method obsCount()
 *  if the input file contains multisubset BUFR messages.
 */
   int messageCount(); // nr of BUFR-messages in a file

//! Returns the number of observation reports in the input file
/*! This number is bigger than returned by method messageCount()
 *  if the input file contains multisubset BUFR messages.
 */
   int obsCount();     // nr of observation messages in a BUFR-file

//! Returns the (running) number of the current BUFR message
   int messageNumber() { return (int)_msgNumber; }

//! Closes the input/output file
   bool close();

//! Prepares 'PrepBUFR' tables if the input file contains BUFR tables
   bool prepBufrFile();

   // Writes message
   bool write( MvObs& );
   bool writeCompressed(MvObs *obs);
   bool writeCompressed(MvObs *obs,const std::vector<int>& subsetVec);
   bool write( const void*, const size_t );
#ifdef MV_BUFRDC_TEST
   bool write( const char*, int );
#endif

   MvObs& firstObs() { return _firstObs; }

   // Expand message
   void expand();

   void setUseSkipExtraAttributes(bool b) {useSkipExtraAttributes_=b;}
   void setCacheCompressedData(bool b) {cacheCompressedData_=b;}

   MvObs gotoMessage(long offset, int msgCnt);

protected:
   // Advance to the next message and by default unpack the message
   MvObs next( bool unpack=true );

   bool Open( const char* );  //, char* aMode = "r" );
   void searchMinMaxTime();
 //Logical  currentInputBufferOK() {return _IO_buffer_OK;}

protected:

#ifdef MV_BUFRDC_TEST
FILE* _bufrFile;
char*  _message;
long _msgLen;
#endif

   FILE*       _ecFile;
   bool        _minMaxDone;
   bool        _IO_buffer_OK;
   bool        _unpacked;
   int         _msgCount;
   int         _obsCount;
   long        _msgNumber;
   std::string _IO_mode;

   codes_handle* _ecH;      // handle general access to eccodes
   MvObs         _firstObs;
   TDynamicTime  _minTime;
   TDynamicTime  _maxTime;

#ifdef METVIEW_PREPBUFR
   MvPrepBufrPrep* _prepBufr;
   bool _isPrepBufrFile;
#endif

   MvBufrOut* _bufrOut;

   bool useSkipExtraAttributes_; //enables optimisation option for eccodes
   bool cacheCompressedData_; //flag for MvObs
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


class MvObsSetIteratorObserver
{
public:
      MvObsSetIteratorObserver() {}
      virtual void notifyObsIteratorProgress(int)=0;
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
   friend std::ostream& operator<< ( std::ostream& aStream, const MvObsSetIterator& anIter );

public:
//! Constructor with a MvObset as an argument
   MvObsSetIterator( MvObsSet&);

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
   int currentMessageNumber() const;

//! Returns the subset number of the current observation report in the BUFR message
   int subsetNumber() { return current.subsetNumber(); }

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
   void useObsTime( bool b = true ) { useObsTime_ = b; }

//! Set the filtering criteria for a single date/time
   void setTime( const TDynamicTime& );

//! Set the filtering criteria for a date/time range
/*! Observation reports that have their data/time within the range
 *  'anObsTime'-'deltaInMinutes' and 'anObsTime'+'deltaInMinutes'
 *  will be selected.
 */
   void setTimeRange( const TDynamicTime&, short );

//! Set the filtering criteria for a date/time range
   void setTimeRange( const TDynamicTime&, const TDynamicTime& );

//! Set the filtering criteria for time range only (date ignored)
/*! Observation reports that have their time stamp between 'aBeginTime'
 *  and 'anEndTime' will be selected. \n \n
 *  Time format is HHMM, i.e. 100*hours+minutes. Thus the value 1500
 *  signifies 3PM, and the value 15 signifies 15 mins past midnight.
 */
   void setTimeRangeWithoutDate( int, int );
   void setTimeRangeInSecWithoutDate(int, int);

//! Set the filtering criteria for accepted WMO block numbers
/*! This method can be called several times in order to set
 *  more than one accepted WMO block, i.e. WMO blocks are
 *  combined with logical OR operator.
 */
   void setWmoBlock(int);

//! Set the filtering criteria for accepted WMO station numbers
/*! This method can be called several times in order to set
 *  more than one accepted WMO station, i.e. WMO stations are
 *  combined with logical OR operator.
 */
   void setWmoStation( long );

//! Set the filtering criteria for an accepted area
/*! Observation report locations must be within the given area.
 */
   void setArea( const MvLocation&, const MvLocation& );

//! Set filtering criteria for a proximity of a cross section line
   void setXSectionLine( const MvLocation&, const MvLocation&, float );

//! Set the filtering criteria for accepted BUFR message types
/*! This method can be called several times in order to set
 *  more than one accepted BUFR message type, i.e. message types are
 *  combined with logical OR operator.
 */
   void setMessageType( int );

//! Set the filtering criteria for accepted BUFR message local subtypes
/*! This method can be called several times in order to set
 *  more than one accepted BUFR message local subtype, i.e. message subtypes
 *  are combined with logical OR operator.
 */
  void setMessageSubtype( int );
  void setMessageRdbtype( int );

  void setMessageNumber(int);
  void setEditionNumber(int);
  void setOriginatingCentre(int);
  void setOriginatingCentreAsStr(const std::string&);
  void setOriginatingSubCentre(int);
  void setMasterTableVersion(int);
  void setLocalTableVersion(int);

  void setHeaderIdent(const std::string&);
  void setIdentKey(const std::string&);
  void setIdentValue(const std::string&);

//! Set the filtering criteria for data 'aDescriptor' having value 'aValue'
/*! Observation reports where the data corresponding to descriptor 'aDescriptor'
 *  has value 'aValue' will be selected.\n \n
 *  This method can be called several times in order to set more than one
 *  accepted value for the same 'aDescriptor', i.e. values are
 *  combined with logical OR operator.
 */
   void select( const std::string&, double );

//! Set the filtering criteria for the given data having its value within a range
/*! Observation reports where the data corresponding to descriptor 'aDescriptor'
 *  has its value in the range 'firstValue'...'secondValue' will be selected.
 */
   void selectRange( const std::string&, double, double );

//! Set the filtering criteria for the given data having its value outside a range
/*! Observation reports where the data corresponding to descriptor 'aDescriptor'
 *  has its value outside the range 'firstValue'...'secondValue' will be selected.
 */
   void excludeRange( const std::string&, double, double );

   MvObs nextMessage();
   bool AcceptedObs( MvObs&, bool skipPreFilterCond=false) const;
   bool useHeaderOnly() const {return useHeaderOnly_;}

   MvObs gotoMessage(long offset, int msgCnt);

   //Set the observer
   void setObserver(MvObsSetIteratorObserver* observer) {observer_=observer;}
   void removeObserver() {observer_=0;}
   void setFilterProgressStep(int filterProgressStep);

protected:
   bool   checkOptionSize(std::size_t num,const std::string& functionName);
   void   next();

   bool   TimeOk( MvObs* ) const;
   bool   WmoBlockOk( MvObs* ) const;
   bool   WmoStationOk( MvObs* ) const;
   bool   WithinXSectionLine( MvObs* ) const;
   bool   messageNumberOk( MvObs* ) const;
   bool   msgTypeOk( MvObs* ) const;
   bool   msgSubtypeOk( MvObs* ) const;
   bool   msgRdbtypeOk( MvObs* ) const;
   bool   editionNumberOk(MvObs*) const;
   bool   originatingCentreOk(MvObs*) const;
   bool   originatingCentreAsStrOk(MvObs*) const;
   bool   originatingSubCentreOk(MvObs*) const;
   bool   masterTableVersionOk(MvObs*) const;
   bool   localTableVersionOk(MvObs*) const;
   bool   headerIdentOk(MvObs*) const;
   bool   identValueOk(MvObs*) const;
   bool   InsideArea( MvObs* ) const;
   bool   selectOk( MvObs* ) const;
   float  distanceFromXSectionLine( const MvLocation& ); //-- used anywhere?

 protected:
   bool _NoFiltersSet;
   bool  useObsTime_;
   int  _SelectValueCount;
   int  _MsgTypeCount;
   int  _MsgType[ MAX_FILTER_LIST_ARRAY_SIZE ];
   int  _MsgSubtypeCount;
   int  _MsgSubtype[ MAX_FILTER_LIST_ARRAY_SIZE ];
   double _SelectValue[ MAX_FILTER_LIST_ARRAY_SIZE ];
   std::string _SelectDescriptor;
   std::vector<int> messageNumber_; //starts at 1
   std::vector<int> editionNumber_;
   std::vector<int> originatingCentre_;
   std::vector<std::string> originatingCentreStr_;
   std::vector<int> originatingSubCentre_;
   std::vector<int> masterTableVersion_;
   std::vector<int> localTableVersion_;
   std::vector<int> wmoBlock_;
   std::vector<int> wmoStation_;
   std::vector<std::string> headerIdent_;
   std::string identKey_;
   std::vector<std::string> identValue_;   
   std::vector<int> rdbType_;

   TDynamicTime fBeginTime, fEndTime;
   ETimeFilterState _TimeFilterState;
   ESelectFilterState _SelectState;

   MvXSectionLine fXSectionLine;
   MvArea fArea;

private:
   MvObs     current;
   MvObsSet* ObsSet;
   bool useHeaderOnly_;
   MvObsSetIteratorObserver* observer_;
   int filterProgressStep_;
};

#endif
// MvObsSet_DEFINED
