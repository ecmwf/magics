/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvBufr.h,   vk Sep94
//        rev vk 010720

#ifndef MvBufr_DEFINED_
#define MvBufr_DEFINED_

#include <eccodes.h> //F ec
#include "fdyntime.h"
#include <map>

using namespace std;

/*! \file */

//--------------------------------------------------------
// This constant should be removed into 'Site Dependent Values'...
//--------------------------------------------------------
const int BUFR_ORIGINATING_CENTER = 98;   // 98 == ECMWF


//------------------------------------------------------//
// missing value code to be used in C/C++ routines      //
//------------------------------------------------------//
const float     kBufrMissingValue = 1.7e38;
const int   kBufrMissingIntValue = 2147483647;


//------------------------------------------------------//
// missing value code returned by FORTRAN routines      //
//------------------------------------------------------//
const double kFortranBufrMissingValue = 1.7e38;
const int   kFortranBufrMissingIntValue = 2147483647;

//--------------------------------------------------------
// maximum sizes of arrays used in FORTRAN routines
// NOTE: sizes for arrrays for BUBOX set in MvObs.cc!!!
// NOTE2: IBM/AIX cannot handle bigger arrays in -q32 mode
//--------------------------------------------------------
#if defined(AIX) || defined(MV_USE_SMALL_ARRAYS)
 const int MAX_KELEM=  40000;  //12000;
 const int NUM_MAX_KVALS=3;
 const int aMAX_KVALS[NUM_MAX_KVALS]={90000, 180000, 360000}; // previous max values: 80000
 //                                      7MB    14MB   27MB
//                                    (vals*80)/(1024*1024)MB
 const int MAX_KVALS=aMAX_KVALS[NUM_MAX_KVALS-1]; // biggest entry in aMAX_KVALS
#else
 const int MAX_KELEM = 160000; // max allowed is 160,000 !!!  // 80000;  // 40000;   //12000;
 const int NUM_MAX_KVALS=3;
 const int aMAX_KVALS[NUM_MAX_KVALS]={600000, 1500000, 4096000};  //360000;   //80000;
//                                      45MB    115MB   312MB
//                                    (vals*80)/(1024*1024)MB
 const int MAX_KVALS=aMAX_KVALS[NUM_MAX_KVALS-1]; // biggest entry in aMAX_KVALS
#endif

//! \enum EBufrInState Status of a BUFR message being read
enum  EBufrInState
{
   kBufrIn_Error
 , kBufrIn_Coded
 , kBufrIn_Sections012Expanded
 , kBufrIn_DataDecoded
 , kBufrIn_DataAndDescriptorsDecoded
};

//! \enum EBufrOutState Status of a BUFR message being created
enum  EBufrOutState
{
   kBufrOut_error
  ,kBufrOut_noBuffers
  ,kBufrOut_emptyBuffers
  ,kBufrOut_formatedBuffers
  ,kBufrOut_dataInBuffers
};

//! \enum EElementValueType Values for data element types
enum EElementValueType
{
   kEVT_unknown
  ,kEVT_missing
  ,kEVT_numeric
  ,kEVT_string
};

#ifndef DOXYGEN_SHOULD_SKIP_THIS
                                             //  Sections of a BUFR message
typedef struct
{
   unsigned char startStr;       // "B"
   unsigned char startStr2;      // "U"
   unsigned char startStr3;      // "F"
   unsigned char startStr4;      // "R"
   unsigned char totalLen;    // 24 bits
   unsigned char totalLen2;
   unsigned char totalLen3;
   unsigned char editionNr;
} TSection0;
#endif

#if 0
typedef struct
{
   unsigned char len;        // 24;  // 1-3
   unsigned char len2;       // 24;  // 2
   unsigned char len3;       // 24;  // 3
   unsigned char masterTable;//  8;  // 4
   unsigned char origCentre; // 16;  // 5-6
   unsigned char origCentre2;// 16;  // 6
   unsigned char updateSeq;  //  8;  // 7

   unsigned char bitField;   //  8;  // 8...
   unsigned char msgType;    //  8;
   unsigned char msgSubtype; //  8;
   unsigned char masterTableVers; //8;

   unsigned char localTableVers; //8;  // 12...
   unsigned char yearYY;     //  8;
   unsigned char month;      //  8;
   unsigned char day;        //  8;

   unsigned char hour;       //  8;  // 16...
   unsigned char minute;     //  8;
} TSection1;
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// internal classes, should not be included in Doxygen docs

const unsigned char cOctetMissingIndicator = 255;

class Section1Base
{
 public:
   Section1Base( const unsigned char* octs );
   Section1Base( const Section1Base* aSec1 );
   virtual ~Section1Base(){ delete [] octets_; }

   int         len() const { return 65536*octets_[0] + 256*octets_[1] + octets_[2]; }
   bool        isDifferent( const Section1Base* aSec1 ) const;
   const unsigned char* start() const { return octets_; }

   virtual bool     hasSection2() = 0;
   virtual TDynamicTime date() = 0;
   virtual int      msgType() = 0;
   virtual int      msgSubtypeWMO() = 0;
   virtual int      msgSubtypeLocal() = 0;
   virtual int      msgSubtype() = 0;
   virtual int      origCentre() = 0;
   virtual int      origSubCentre() = 0;
   virtual int      masterTable() = 0;
   virtual int      masterTableVersion() = 0;
   virtual int 	    localTableVersion() = 0;

 protected:
   unsigned char* octets_;
};

class Section1_preEd4 : public Section1Base
{
 public:
   Section1_preEd4( const unsigned char* octs ) : Section1Base( octs ){};
   Section1_preEd4( const Section1Base* aSec1 ) : Section1Base( aSec1 ){};

   bool     hasSection2();
   TDynamicTime date();
   int      msgType();
   int      msgSubtypeWMO();
   int      msgSubtypeLocal();
   int      msgSubtype();
   int      origCentre();
   int      origSubCentre();
   int      masterTable();
   int      masterTableVersion();
   int 	    localTableVersion();
};

class Section1_Ed4 : public Section1Base
{
 public:
   Section1_Ed4( const unsigned char* octs ) : Section1Base( octs ){};
   Section1_Ed4( const Section1Base* aSec1 ) : Section1Base( aSec1 ){};

   bool     hasSection2();
   TDynamicTime date();
   int      msgType();
   int      msgSubtypeWMO();
   int      msgSubtypeLocal();
   int      msgSubtype();
   int      origCentre();
   int      origSubCentre();
   int      masterTable();
   int      masterTableVersion();
   int 	    localTableVersion();
};

typedef struct
{
   unsigned char len;      // : 24;
   unsigned char len2;
   unsigned char len3;
   unsigned char reserved; // :  8;
} TSection2;

typedef struct
{
   unsigned char len;      // : 24;
   unsigned char len2;
   unsigned char len3;
   unsigned char reserved;   //  8;
   unsigned char subsetCnt;  // 16;
   unsigned char subsetCnt2;
   unsigned char bitField;   //  8;
} TSection3;

typedef struct
{
   unsigned char len;      // : 24;
   unsigned char len2;
   unsigned char len3;
   unsigned char reserved; // :  8;
} TSection4;
// DOXYGEN_SHOULD_SKIP_THIS
#endif

//--------------------------------------------------------------- MvBufrBase

class MvObsSet;

//! Wrapper around Fortran 'bufren', base class for MvBufr and MvBufrOut
/*! This class and its methods are a lower level wrapper around Fortran
 *  subroutine 'bufren'. This class should be hidden from Metview applications
 *  and the methods of this class should be called only from MvObs.
 */
class MvBufrBase
{
 friend class MvBufrOut;  //???
 //friend class MvObsSet;

 protected:
	 int  _refCount;
	long  fMessageNumber;

        long *longptr;
	char *fMessage;
	long  fMessageLength;
   TSection0 *fSec0;
   Section1Base* Sec1;
   TSection2 *fSec2;
   unsigned char *fTotalSec2;
   TSection3 *fSec3;
   TSection4 *fSec4;

     int  fKERR;
     int *fKSUP;   // [ 9 ];
     int *fKSEC0;  // [ 3 ];
     int *fKSEC1;  // [ 40 ];
     int *fKSEC2;  // [ 64 ];
     int *fKSEC3;  // [ 4 ];
     int *fKSEC4;  // [ 2 ];

 protected:
	    MvBufrBase( const long len );  //( char *msg, long len );
	    virtual ~MvBufrBase( void );

      void  attach( void );   // { _refCount++; }
      void  detach( void );   // { if( --_refCount == 0 ) delete this; }
      void  createFortranArrays( void );
    void  createDataArrays( void );
    void  deleteDataArrays( void );
      unsigned int unsignedInt( const unsigned char* firstOctet
                              , int octetCount );
      int subsetCount(){ return unsignedInt( &(fSec3->subsetCnt), 2); }
      int totalLen() { return unsignedInt( &(fSec0->totalLen), 3); }
};

//--------------------------------------------------------------- MvBufr

//! C++ wrapper around Fortran 'bufren', used by MvObs
/*! This class and its methods are a lower level wrapper around Fortran
 *   subroutine 'bufren'. These methods should be called only via MvObs class.
 */
class MvBufr : public MvBufrBase
{
 friend class MvObs;
 friend class MvObsSet;
 friend class MvObsSetIterator;
 friend class MvBufrConfidence;
 friend class MvBufrOut;

   static long  _bufrIn_ref;

 protected:
	    MvBufr( char *msg, long len, long aMessageNumber=0 );
       ~MvBufr( void );
//F
void setEccodes( codes_handle** ecH );

      void  Decode( void );
      void  Decode_012( void );
      void  ExpandDescriptors( int subsetNumber );
       int  descriptorToFortranIndex( const long aDescr, const int firstIndex = 0 );
      long  currentBufrRef( void ) const { return _bufrIn_ref; };

 EElementValueType  elementValueType( const int aSubsetNr );
 EElementValueType  elementValueType( const long aDescriptor, const int aSubsetNr );
 EElementValueType  elementValueTypeByIndex( const int anIndex, const int aSubsetNr );

 bool  Value( const long aDescriptor
		 , const long aSubsetNumber
		 , double &aDataValue
		 , int   firstIndex = 0 );
     double  DataValue( const int aDescriptorArrayIndex
		     , const long aSubsetNumber);
     double  PeekDataValue( const int aDescriptorArrayIndex
		         , const long aSubsetNumber);
      long  intValue( const long aDescriptor, const int subsetNr );

    double  feedbackValue( int col, int subset );
    double  feedbackValue( int row, int col, int subset );
    string   feedbackItemName( int row,int subset );
    string   feedbackItemUnit( int row,int subset );

  TDynamicTime  obsTime( const int subsetNr ); //- from msg body (section 4)
  TDynamicTime  msgTime( void );               //- from msg header (section 1)

    string  stringValue( const long aDescriptor, const int aSubsetNr );
    string  stringValue( const int aSubsetNr );
    string  stringValueByIndex( const int anIndex, const int aSubsetNr );

    string  unit( const long aDescriptor );
    string  unit( void );
    string  unitByIndex( const int anIndex );

    string  name( const long aDescriptor );
    string  name( void );
    string  nameByIndex( const int anIndex );

   bool  SetFirstDescriptor( void );
   bool  SetNextDescriptor( void );
      long  CurrentDescriptor( void ){ return _currentDescr; }
 double  CurrentValue( const int aSubsetNr ){ return DataValue( _currentDescrInd, aSubsetNr); }

      bool  printSection(ostream &aStream,int which);
      bool printSection_012( ostream& aStream,int which);
      bool  writeBufrBox( int aSubsetNr );
       int  fillBufrBox( int aSubsetNr );
      bool  getBufrBoxSize( int& rows, int& cols, int aSubsetNr );

      bool  getDataFromSection2(map<string,string> &data);
      void  parseSection2(int *fKEY,map<string,string> &data);

      void  setSubset( int subsetNumber ){ _lastKnownSubsetValue = subsetNumber; }

 private:
      void  computeIn_KELEM( void );

codes_handle** _ecH;   //F ec


 protected:
  EBufrInState  _inState;
	  long  _currentDescr;
	  int   _currentDescrInd;
	  int   _bufrBoxFilledSubset;
	  int  _lastKnownSubsetValue; //-- Q&D trick, BUSEL2 requires
};

#if 0
//--------------------------------------------------------------- MvBufrOut
// A simple class capable of producing BUFR code only from 'MvObs'
// objects i.e. usable in filtering applications which read a
// BUFR file and write a new file with less messages...
//---------------------------------------------------------------

class MvObsSet;
class MvObs;
class MvBufrConfidence;

const int MAX_KDLEN = 2000;  //-- big value needed for some NCEP PrepBUFR files

//! Wrapper around Fortran 'bufren', for (re)encoding
/*! This class and its methods are a lower level wrapper around Fortran
 *  subroutine 'bufren'. These methods should be called only via MvObs class.
 */
class MvBufrOut : public MvBufrBase
{
 friend class MvObs;
 friend class MvObsSet;
 friend class MvObsSetIterator;

 protected:
	    MvBufrOut( const long len, MvObsSet* anOutSet );
	    ~MvBufrOut( void );

   static long  _bufrOut_ref;

      void  createBuffers();
      void  resetBuffers( void );
      void  formatBuffers( const MvObs& anObs );
      void  write( MvObs& anObs );
      void  add( MvObs& anObs );
      void  addIntoBuffers( MvObs& anObs );
      void  encode( void );
      void  checkDescriptors( const MvObs& anObs );
       int  differentDescriptors( void ) const;
       int  differentHeader( const MvObs& anObs ) const;
       int  shouldBeWritten( void );
      void  setSubsetCount( int MaxNrSubsets );
   bool  isDelayedDescriptor( const long aDescriptor ) const;
     // long  msgLength( void ) { return _msgIntLen*sizeof( int ); }

 protected:
       MvObsSet* _outSet;
            int  _maxNrSubsets;
	    int  _nextValue;
	    int  _nextCharParamPos;
  EBufrOutState  _outState;
        int  _KDLEN;
        int  _KDATA[ MAX_KDLEN ];
   Section1Base* _currentSec1;
};
#endif

#endif  // MvBufr_DEFINED
