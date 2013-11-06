/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvObs.h,   vk july94
//        rev vk 010724

#ifndef MvBufrObs_DEFINED
#define MvBufrObs_DEFINED

#include <string>
#include "inc_iostream.h"
//#include <fstream.h>
//#include <strstream.h>
#include "MvBufr.h"
#include "MvLocation.h"
#include "fmettim.h"

#ifdef METVIEW
//#   include "Metview.h"
#    include "MvDate.h"
#    include "MvRequest.h"
#endif

//---------------------------------------------------
// Function definitions for the FORTRAN BUFR routines
//---------------------------------------------------

#ifdef FORTRAN_NO_UNDERSCORE
#define  BUS012   bus012
#define  BUPRS0   buprs0
#define  BUPRS1   buprs1
#define  BUPRS2   buprs2
#define  BUFREX   bufrex
#define  BUFREN   bufren
#define  BUSEL    busel
#define  BUSEL2   busel2
#define  BUUKEY   buukey
#define  BUPRS3   buprs3
#define  BUBOX    bubox
#define  BUPRTBOX buprtbox
#define  FT_OP6   ft_op6
#define  FT_CLO   ft_clo
#else
#define  BUS012   bus012_
#define  BUPRS0   buprs0_
#define  BUPRS1   buprs1_
#define  BUPRS2   buprs2_
#define  BUFREX   bufrex_
#define  BUFREN   bufren_
#define  BUSEL    busel_
#define  BUSEL2   busel2_
#define  BUUKEY   buukey_
#define  BUPRS3   buprs3_
#define  BUBOX    bubox_
#define  BUPRTBOX buprtbox_
#define  FT_OP6   ft_op6_
#define  FT_CLO   ft_clo_
#endif

//--------------------------------------------------------------- MvObs
//! A class to handle one observation report stored in a BUFR message
/*! This class is used to request data and metadata from a single
 *  BUFR message (Observation Report). Access to BUFR messages in
 *  files is done using classes MvObsSet and MvObsSetIterator. \n
 *
 * <B>Descriptors</B> \n
 * Elements within the message body are referenced by BUFR descriptors.
 * These descriptors are of the form 'ZXXYYY', where Z=0, XX defines
 * an element class and YYY defines an element within a class.
 * Descriptor values can be found in "BUFR, User Guide and Reference Manual"
 * by Milan Dragosavac (ECMWF). \n
 * A Word of Warning: leave out leading zeroes when writing constants
 * (such as descriptors) into your code, unless you intend to use
 * octal constants! \n
 *
 * <B>Current Descriptor</B> \n
 * Some member functions need no descriptor, because they will use
 * the internal Current Descriptor set by a previously called member
 * function which had a descriptor parameter. \n
 *
 * <B>Missing Values</B> \n
 * Functions returning values from the message will return const kBufrMissingValue
 * if the requested value is not available. Another const, called
 * kBufrMissingIntValue is used for missing integer values.
 */
class MvObs
{
 friend class MvBufrOut;
 friend class MvObsSet;
 friend class MvObsSetIterator;

	void _copy( MvBufr* b );
	void _copy( const MvObs& b );
	void _clean();

public:
//! Constructor
/*! Arguments are mainly used by MvObsSet and MvObsSetIterator.
 *  Applications normally call this constructor without arguments.
 */
	MvObs( MvBufr* b = NULL, int i = 1 );

//! Copy constructor
	MvObs( const MvObs&  b );

	~MvObs();

//! Assignment operator
	MvObs& operator= ( const MvObs& b );

//! Operator to test the validity of a new MvObs
/*! In this example operator void*() is used implicitly
 *  in testing the validity of MvObs object returned by
 *  MvObsSetIterator object:
 * <PRE>
 *      MvObsSet mySet("/path/to/my/file");
 *      MvObsSetIterator myIter(mySet);
 *      ...
 *      while(myObs=myIter()) //-- void*() is called here
 *      {
 *         ... //-- do the stuff
 *      }
 * </PRE>
 */
    operator void*();

//! Returns the number of subsets available in this BUFR message
/*! Use method Advance() to get to the next subset (in a multisubset BUFR message).
 */
        int  msgSubsetCount(){ return _bufrIn->subsetCount(); }

//! Returns the current subset number (in a multisubset BUFR message)
        int  subsetNumber() const { return _subsetNr; }

//! Advances to the next subset in a multisubset BUFR message
/*! Returns 'false' if current subset is the last (or the only)
 *  one. Returns 'true' on success.
 */
	bool   Advance();

//! Checks whether this MvObs contains a BUFR message or is empty
    bool  operator! ();

//! Checks that this MvObs contains a valid decodable BUFR message
    bool  msg_ok() const; // { return _bufr_id == _bufrIn->currentBufrId(); }

    // ----  A P I   f u n c t i o n s :  ---- //

    //-- APIs for requesting parameter values --//

//! Returns the value of an element defined by 'aDescriptor'
/*! Also sets Current Descriptor.
 *  Missing value indicator is returned if the observation report
 *  does not contain elements of type 'aDescriptor', or if the element
 *  does not have value.
 */
      float  value( long aDescriptor );

//! Returns the value of an element defined by 'aDescriptor' as 'long'
/*! Also sets Current Descriptor.
 */
       long  intValue( long aDescriptor );

//! Returns the value of an element defined by 'aDescriptor' as 'string'
/*! Also sets Current Descriptor.
 */
      string stringValue( long aDescriptor );

//! Returns the value of Current Descriptor as a string
      string stringValue();

//! Returns the value of the next element with the same descriptor
/*! Uses Current Descriptor to start looking for the next occurrence
 *  of the same descriptor later in the message.
 *  Updates Current Descriptor.
 *  Returns 'kFortranBufrMissingValue' if no such element is found
 *  or if such an element has no value.
 */
      float  nextValue();

//! Returns the value corresponding to the 'n'th occurrence of descriptor 'descr'
/*! Method searches the current subset from the beginning looking
 *  for descriptors 'descr'. If BUFR message contains 'n' (or more)
 *  occurrences of descriptor 'descr', the corresponding data value
 *  is returned, otherwise 'kFortranBufrMissingValue' is returned.
 */
      float  valueByOccurrence( int n, long descr );

//! Index access operator returns the 'n'th data value
/*! This operator treats BUFR message as an array (as 'bufrdc' does)
 *  and returns the value of the 'n'th element as 'double'.
 *  Index 'n' starts from '1', i.e. n=1,2,3,...
 */
     double  operator[] ( int n );  //-- n starts from 1: 1,2,...,n

//! Returns the value from the 'col'th feedback column for Current Descriptor
/*! Uses 'bufrdc' subroutine 'BUBOX()' to arrange feedback values into
 *  a two dimensional array. Argument 'col' refers to a column in this array.
 */
     double  feedbackValue( int col ){ return _bufrIn->feedbackValue( col, _subsetNr ); }

//! Returns the value of the 'row'th element in the 'col'th feedback column
/*! Uses 'bufrdc' subroutine 'BUBOX()' to arrange feedback values into
 *  a two dimensional array. Arguments 'row' and 'col' are indices into
 *  this array.
 */
     double  feedbackValue( int row, int col ){ return _bufrIn->feedbackValue( row, col, _subsetNr ); }

     string  feedbackItemName( int row){ return _bufrIn->feedbackItemName( row, _subsetNr ); }
     string  feedbackItemUnit( int row){ return _bufrIn->feedbackItemUnit( row, _subsetNr ); }

    //-- APIs for requesting parameter metadata --//

//! Returns the name of the data referenced by 'aDescriptor' (from BUFR Table B)
     string  name( long aDescriptor );

//! Returns the name of the data referenced by Current Descriptor
     string  name();

//! Returns the unit of the data referenced by 'aDescriptor' (from BUFR Table B)
     string  unit( long aDescriptor );

//! Returns the unit of the data referenced by Current Descriptor
     string  unit();

    //-- APIs for iterating through all parameters --//

//! Sets the first expanded descriptor in the message as Current Descriptor
/*! In case of decoding problems, returns 'false'.
 */
       bool  setFirstDescriptor(){ return _bufrIn->SetFirstDescriptor(); }

//! Advances Current Descriptor to the next expanded descriptor in the message
/*! Returns 'false' if message contains no more data (no more descriptors).
 */
       bool  setNextDescriptor(){ return _bufrIn->SetNextDescriptor(); }

//! Returns the data value related to Current Descriptor
      float  currentValue(){ return _bufrIn->CurrentValue( _subsetNr ); }

//! Returns Current Descriptor
       long  currentDescriptor(){ return _bufrIn->CurrentDescriptor(); }

    //-- APIs for requesting parameter types --//

//! Returns the type of data related to descriptor 'aDescriptor'
 EElementValueType  elementValueType( long aDescriptor );

//! Returns the type of data related to Current Descriptor
 EElementValueType  elementValueType();

    //-- APIs for requesting time and location data --//

//! Returns the time from the observation report (from the data: BUFR section 4)
        TDynamicTime  obsTime(){ return _bufrIn->obsTime( _subsetNr ); }

//! Returns the time from the message metadata (from BUFR section 1)
        TDynamicTime  msgTime(){ return _bufrIn->msgTime(); }

//! Returns the location from the observation report (from the data: BUFR section 4)
      MvLocation  location();

    //-- APIs for message type, subtype, originating centre, etc --//

//! Returns the Message Type code from BUFR section 1
        int  messageType();

//! Returns the locally defined Message Subtype code from BUFR section 1
        int  messageSubtypeLocal();

//! Returns the international Message Subtype code (WMO defined) from BUFR section 1
/*! Note that this code is available only in BUFR Edition 4 messages. For Edition 3
 *  messages a value of 255 is returned (255 corresponds to an octet with all bits '1').
 */
        int  messageSubtypeInternational();

//! Returns either the local or the international message subtype
/*! In BUFR Edition 3 only the local subtype was available.
 *  In BUFR Edition 4 the WMO defined international subtype was
 *  added. Local subtype was left for backwards compatibility.
 *
 *  For BUFR Edition 3 messages this function always returns the local subtype.
 *
 *  For later Editions this function returns the international subtype if
 *  it has been set. If not set (has a value 255) then the local subtype
 *  is returned
 */
        int  messageSubtype();

//! Returns the Originating Centre code from BUFR section 1
        int  originatingCentre();

//! Returns the Originating Subcentre code from BUFR section 1
        int  originatingSubCentre();

//! Returns the Edition number from BUFR section 0
        int editionNumber();

//! Returns the Master Table code from BUFR section 1
/*! Master Table 0 is for Meteorology, 10 is for Oceanography
 */
	int masterTable();

//! Returns the Master Table version from BUFR section 1
	int masterTableVersion();

//! Returns the Local Table version from BUFR section 1
	int localTableVersion();

    //-- APIs for  --//
//! Returns the total length of the message from BUFR section 0
	int messageTotalLen();

    //-- APIs for requesting weather station values --//
//! Returns the 5 digit WMO station identifier
/*! Looks for WMO block number 'BB' and station identifier 'SSS'
 *  and returns the value of 'BBSSS' if found. Returns zero if
 * 'BB' or 'SSS' is not found.
 */
       long  WmoIdentNumber();

//! Returns the 2 digit WMO block identifier 'BB'
/*! Returns zero if 'BB' not found.
 */
        int  WmoBlockNumber();

//! Returns the 3 digit WMO station identifier 'SSS'
/*! Returns zero if 'SSS' not found.
 */
        int  WmoStationNumber();

//! Looks for an ident from the BUFR message
/*! Looks for the following message identifiers: \n
 *  - WMO Station Identifier 'BBSSS' \n
 *  - Ship or mobile land station identifier (001011) \n
 *  - Buoy/platform identifier (001005) \n
 *  - Aircraft flight number (001006) \n
 *  - Satellite identifier (001007) \n
 *  - Aircraft registration number (001008) \n
 *  - Stationary buoy platform identifier (001010) \n
 *  - Storm identifier (001025)
 *  - WMO storm name (001026)
 *  - WMO long storm name (001027)
 *
 *  and returns the first one found, as a string. If none
 *  of the above is found, returns "id???".
 */
     string  findSomeIdent();

    //-- APIs for accessing replicated parameters --//

//! Returns the number of times level coordinator descriptor 'levelDescriptor' is found
	int  numberOfLevels( long levelDescriptor );

//! Returns the first value related to 'levelDescriptor', i.e. value of the first level
      float  firstLevel( long levelDescriptor );

//! Returns the value of the next level related to Current Level Descriptor
      float  nextLevel();

//! Returns the value of the specified data for the specified level
/*! First looks for a data block related to level 'aLevel' as the
 *  value of the data element related to level descriptor 'aLevelDescriptor',
 *  and then looks for a data element related to descriptor 'aDescriptor'
 *  within this level.
 */
      float  valueByLevel( long  aLevelDescriptor
                         , float aLevel
			 , long  aDescriptor );
			 
      float  valueByLevelRange( long  aLevelDescriptor
                         , float firstLevel
			 , float seconLevel	
			 , long  aDescriptor );
			 		 
//! Returns the number of pressure levels found in the observation report
/*! Pressure level descriptor is 007004, so this is an alias for member function
 *  numberOfLevels(7004).
 */
	int  numberOfPressureLevels();

//! Returns the value of the first pressure level in hPa
/*! Original pressure values are stored in Pa, so this is
 *  the same as 100.0*firstLevel(7004).
 */
      float  firstPressureLevel();

//! Returns the value of the next pressure level in hPa
/*! Original pressure values are stored in Pa, so this is
 *  the same as 100.0*nextLevel(7004).
 */
      float  nextPressureLevel();

//! Returns the value of the data corresponding to 'aDescriptor' on level 'aLevel'
/*! Here 'aLevel' is given in hPa. Original pressure values are stored in Pa,
 *  so this is the same as \n
 * <PRE>
 *      valueByLevel( 7004, 100.0*aLevel, aDescriptor )
 * </PRE>
 */
      float  valueByPressureLevel( float aLevel, long aDescriptor );

//! Returns the value of the data corresponding to 'aDescriptor' in a layer
/*! Level values are for the top and the bottom pressure values of a layer
 *  and they are given in hPa. \n \n
 *  This method looks for two consecutive pressure coordinate descriptors
 *  007004 with the given values (hPa is first converted to Pa). If such
 *  a layer is found and the layer contains 'aDescriptor' then the corresponding
 *  data value is returned, otherwise 'kBufrMissingValue' is returned.
 */
      float  valueByLayer( float firstLevel
		         , float secondLevel
		         , long aDescriptor );

    //-- APIs for printing obs --//
    // Section 0,1,2 and 3 just delegated to bufr class.

//! Prints BUFR section 0 to output stream 'aStream'
    bool  printSection0(ostream &aStream = cout)
      { return _bufrIn->printSection_012(aStream,0); }

//! Prints BUFR section 1 to output stream 'aStream'
    bool  printSection1(ostream &aStream = cout)
      { return _bufrIn->printSection_012(aStream,1); }

//! Prints BUFR section 2 to output stream 'aStream'
    bool  printSection2(ostream &aStream = cout)
      { return _bufrIn->printSection_012(aStream,2); }

//! Prints BUFR section 3 to output stream 'aStream'
    bool  printSection3(ostream &aStream = cout)
      { return _bufrIn->printSection(aStream,3); }

//! Decode BUFR section 2 and place the result into an std::map
    bool getDataFromSection2(map<string,string> &data)
	{ return _bufrIn->getDataFromSection2(data); }

//! Prints all data values into standard output
/*! For output format see method 'writeAllValues' below
 */
    bool  printAllValues();

//! Writes all data values into output stream 'aStream'
/*! Writes the data, one data value per line. Each line consists of:
 *  - index of the data
 *  - data value
 *  - name of the data
 *  - unit of the data, in square brackets
 *  - the corresponding BUFR descriptor, in parenthesis
 *
 *  Example output (an extract from a SYNOP report): \n
 * <PRE>
 *       1.      10 Wmo Block Number [NUMERIC] (01001)
 *       2.     500 Wmo Station Number [NUMERIC] (01002)
 *       3.       1 Type Of Station [CODE TABLE 002001] (02001)
 *       4.    2007 Year [YEAR] (04001)
 *       5.      11 Month [MONTH] (04002)
 *       6.       6 Day [DAY] (04003)
 *       7.      22 Hour [HOUR] (04004)
 *       8.       0 Minute [MINUTE] (04005)
 *       9.   50.97 Latitude (High Accuracy) [DEGREE] (05001)
 *      10.    6.05 Longitude (High Accuracy) [DEGREE] (06001)
 *      11.      98 Height Of Station [M] (07001)
 *     ...
 * </PRE>
 */
    bool  writeAllValues( ostream& aStream );

//! Writes all data values into file 'aPathName'
/*! For output format see the version of method 'writeAllValues' above
 */
    bool  writeAllValues( const char* aPathName );

//! Calls 'bufren' routine BUPRTBOX to write feedback data into stream 'aStream'
    bool  writeBufrBox( ostream& aStream = cout );

    //-- APIs for accessing original section 1 and 2 headers --//

//	const unsigned char*
//	         section1Ptr(){ return (unsigned char*)(_bufrIn->Sec1->start()); }

//! Returns 'true' if BUFR message contains local section 2, 'false' if not
	bool     hasSection2(){ return _bufrIn->fSec2 != NULL; }

//! Returns a pointer to the beginning of local section 2 in BUFR message
/*! Returns 0 (NULL) if message has no local section 2
 */
	const unsigned char*
	         section2Ptr(){ return _bufrIn->fTotalSec2; }

    //-- APIs for accessing confidence values --//

//! Checks whether the BUFR message contains confidence values or not
/*! Returns 'true' if operator descriptor 222000 is found in the
 *  message, otherwise 'false'.
 */
       bool  hasConfidences();

//! Returns the confidence value for the current data, if exists
/*! Otherwise returns -1.
 */
        int  confidence();

//! Writes confidence values into stream 'aStream'
       bool  writeConfidenceValues( ostream& aStream );

 bool  getBufrBoxSize( int& rows, int& cols )
                 { return _bufrIn->getBufrBoxSize( rows, cols, _subsetNr ); }

     //-- Q&D: valueBySpecifier & specifierIndex made public
     //--      so that ObsPicker can use them freely (vk/Jul)
     double  valueBySpecifier( long   aSpecifierDescriptor
			     , double aSpecifierValue
		             , long   aDescriptor
			     , int   firstIndexValue = 0 );
     int     specifierIndex( long   aSpecifierDescriptor
		           , double aSpecifierValue
		           , int    firstIndexValue = 0 );

#ifdef METVIEW
//! Decodes OPERA BUFR radar data into unsigned char array
   unsigned char* OperaRadarImage( /* <aki> add arguments? */ );
//! Retrieves metadata for OPERA radar image
   bool  OperaRadarMetadata( /* <aki> add arguments? */ );
#endif

 private:
 //protected:
    float  pressureLevel( int firstIndexValue );
    float  level( long levelDescriptor, int firstIndexValue );
    bool writeValues(ostream &aStream,int first, int last);

 private:
        int  subsetOffset() const;

	const unsigned char*
	         section1Ptr(){ return (unsigned char*)(_bufrIn->Sec1->start()); }

 protected:
 	int     _subsetNr;
	MvBufr* _bufrIn;
	long    _bufr_id;
 	int     _lastSpecifierIndex; //required???? used by who?????
	long    _currentLevelCoordinate;
	int     _currentLevelIndex;
	MvBufrConfidence* _confidence;
};

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------- MvBufrConfidence
// Q&D hack for QC Feed Back using descr operator 222000; vk Apr-95

//! \enum ESelectFilterState Used internally by MvBufrConfidence
enum EBufrConfState
{
   kBCS_unknown
  ,kBCS_missing
  ,kBCS_exists
};

//------------------------------------------------------------------------------
//! Auxiliary class to handle quality control feedback
/*! Warning: This is Q&C ("quick&dirty") hack to access
 *  QC FB data behind operator descriptor 222000.
 */
class MvBufrConfidence
{
 public:
          MvBufrConfidence( MvBufr* aBufr, int aSubsetNr );
          ~MvBufrConfidence();

      bool  hasConfidences();
       int  confidence( long aDescriptor );
       int  confidenceByIndex( int anIndex );
       int  lastDataIndex();

 protected:
       int  startOfDataPresent();
       int  startOfConfidences();
       int  delta( int anIndex );

 private:
   MvBufr*  _bufr;
   int      _subsetNr;
   int      _startOfDataPresent;
   int      _startOfConfidences;
   EBufrConfState _state;
};
#endif


#endif
// MvBufrObs_DEFINED
