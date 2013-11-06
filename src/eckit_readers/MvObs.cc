/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvObs.cc,     vk 940818...
//           rev vk 980501

//--------------------------------------------------------------------
//  A class to hide the complexities of BUFR routines written in
//  FORTRAN.  'MvObs' tries to wrap these routines into a nicer C++
//  interface and hide the required data structures.
//  Headers for the FORTRAN functions are defined in the file 'MvBufr.h'
//--------------------------------------------------------------------
//    FORTRAN routines and required data structures are described in:
//
//    "Decoding Data Represented in FM 94 BUFR"
//         by J.K.Gibson and M.Dragosavac
//
//             published as PAPER 4 in
//
//    "BINARY
//     UNIVERSAL FORM FOR
//     DATA REPRESENTATION
//     ----------------------------------
//     FM 94 BUFR
//     Collected papers and specification"
//
//            ECMWF February 1988.
//
//  and in:
//
//     "BUFR User Guide and Reference Manual"
//         by Milan Dragosavac
//     European Centre for Medium-Range Weather Forecasts
//         preprint  12 October 1994
//--------------------------------------------------------------------

#include "inc_iostream.h"
#include <sstream>  //-- requires new templated standard io headers!
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <assert.h>
//F #include "Metview.h"
#include "MvObs.h"
#include "MvObsSet.h"

#include <map>

#ifdef METVIEW
#  include "MvException.h"
#else
#  include <exception>
#endif

// Static int used to duplicate stdout and get it back.
static int new_fd = -1;

const int MAX_BUBOX_KELEM_LIMIT  = MAX_KELEM;

static string MESSED_UP("[messed messages!]");

static string BBOXNAME("prtbbox.txt"); //for BUPRTBOX output.
static string redirect_dir(""); // For tmp files from print.

//--------------------------------------------------------
// Fortran routines expect previous values to be remained
// (at least in some of the arrays) if new msg has same
// descriptors as previous msg !!!
// Thus static arrays, common to all bufr-objects...
// Added lazy evaluation 980501/vk...
//--------------------------------------------------------
static char* In_CNAMES = 0;  //static char In_CNAMES[ MAX_KELEM ][ 64 ]; //- is not updated if same descriptors
static char* In_CUNITS = 0;  //static char In_CUNITS[ MAX_KELEM ][ 24 ]; //- is not updated if same descriptors
static char* In_CVALS  = 0;  //static char In_CVALS [ MAX_KVALS ][ 80 ]; //+ rest of the arrays always updated
static char* Out_CVALS = 0;  //static char Out_CVALS [ MAX_KVALS ][ 80 ];

static fortfloat* In_VALUES = 0;  //static fortfloat  In_VALUES[ MAX_KVALS ];
static fortfloat* Out_VALUES = 0; //static fortfloat Out_VALUES[ MAX_KVALS ];

static fortint  In_KELEM;
static fortint  In_KVALS;
static fortint  Out_KELEM = -1;
	                                // for BUSEL
static fortint  In_KTDEXL;
static fortint* In_KTDEXP = 0;  //static fortint  In_KTDEXP[ MAX_KELEM ];
static fortint  In_KTDLEN;
static fortint* In_KTDLST = 0;  //static fortint  In_KTDLST[ MAX_KELEM ];
static fortint  Out_KTDEXL;
static fortint* Out_KTDEXP = 0; //static fortint Out_KTDEXP[ MAX_KELEM ];
static fortint  Out_KTDLEN;
static fortint* Out_KTDLST = 0; //static fortint Out_KTDLST[ MAX_KELEM ];

static int arraySizeIndex = 0; // incremented each time we try to allocate a new size of arrays
static int kVals;

long MvBufr::_bufrIn_ref = 0;
long MvBufrOut::_bufrOut_ref = 0; //not yet implemented...


//-- Linux/g++ does not like hardcoded big? arrays (works ok in debugger,
//-- but crashes inside Metview...) => create arrays dynamically, once!
//-- These are for BUFR-BOX routines (accessing Feedback info)

static fortint    myKSUB, myKBOX, myKAPP, myKLEN, myKERR;
static fortint*   myKBOXR = 0;
static fortfloat* myVALS  = 0;
static char*      myCBOXN = 0;
static char*      myCBOXU = 0;


const long cPressureCoordinate = 7004L;  // pressure vertical coord. descriptor value

//--------------------------------------------------------
//  Descriptor mnemonics for class 'MvBufrParam'
//--------------------------------------------------------

#ifndef DOXYGEN_SHOULD_SKIP_THIS

typedef struct
{
   const char *name;
   long        descriptor;
} descriptorStruct;

static descriptorStruct knownParams[] =
{
   {"z", 10003}, {"p", 10004},
   {"ddd", 11001}, {"ff", 11002}, {"u", 11003}, {"v", 11004}, {"w", 11006},
   {"T", 12001}, {"Td",12003}, {"T(2m)",12004}, {"Td(2m)",12006},
   {"END",0}
};
#endif

//--------------------------------------------------------
//  Function definitions for the FORTRAN & C BUFR routines
//--------------------------------------------------------

extern "C" {

 void
 BUS012( fortint *KBUFL, fortint *KBUFF, fortint *KSUP
       , fortint *KSEC0, fortint *KSEC1, fortint *KSEC2, fortint *KERR );

 void
 BUPRS0( fortint *KSEC0 );
 void
 BUPRS1( fortint *KSEC1 );
 void
 BUPRS2( fortint *KSUP, fortint *KEY);

 void
 BUFREX( fortint *KBUFL, fortint *KBUFF, fortint *KSUP
       , fortint *KSEC0, fortint *KSEC1, fortint *KSEC2, fortint *KSEC3, fortint *KSEC4
       , fortint *KELEM, char *CNAMES,     char *CUNITS
       , fortint *KVALS, fortfloat *VALUES,  char *CVALS, fortint *KERR );

 void
 BUFREN( fortint *KSEC0, fortint *KSEC1, fortint *KSEC2, fortint *KSEC3, fortint *KSEC4
       , fortint *KTDLEN,fortint *KTDLST,fortint *KDLEN, fortint *KDATA
       , fortint *KELEM, fortint *KVALS, fortfloat *VALUES, char *CVALS
       , fortint *KBUFL, fortint *KBUFF, fortint *KERR );

 void
 BUSEL( fortint *KTDLEN, fortint *KTDLST, fortint *KTDEXL, fortint *KTDEXP, fortint *KERR );

 void
 BUSEL2( fortint *KSUBSET,fortint *KELEM,fortint *KTDLEN,fortint *KTDLST
       , fortint *KTDEXL, fortint *KTDEXP, char *CUNITSCNAMES, char *CUNITSCUNITS
       , fortint *KERR);

 void
 BUUKEY(fortint *KSEC1, fortint *KSEC2, fortint *KEY, fortint *KSUP,fortint *KERR);
 void
 BUPRS3( fortint *KSEC3, fortint *KTDLEN, fortint *KTDLST
       , fortint *KTDEXL, fortint *KTDEXP, fortint *KELEM, char *CNAMES );

 void
 BUBOX( fortint *KSUB,   fortint *KSUP,      fortint *KELEM
      , fortint *KWTR,   char *CNAMES, char *CUNITS
      , fortint *KVALS,  fortfloat *VALUES,  fortint *KBOX
      , fortint *KAPP,   fortint *KLEN,      fortint *KBOXR
      , fortfloat *VALS, char* CBOXN,        char* CBOXU
      , fortint *KERR );

  void
  BUPRTBOX( fortint *KBOX,   fortint *KAPP,   fortint *KLEN
          , fortint *KBOXR,  fortfloat *VALS, char* CBOXN
          , char* CBOXU );

 // Helper functions
 bool redirect_6(const char *);  //-- Trick to redirect Fortran unit 6.
 bool reconnect_6();
 bool file_to_stream(const char *,ostream&,int skip);
 void delete_print_file(const char *);
 void eraseWhiteSpaceFromStringEnd(string &str);
 int CIND(int i) { return i-1;}
}

static string intToString(int);
static string floatToString(float);
static void keyToStringMap(map<string,string> &,string,fortint *,int);
static void keyToStringMap(map<string,string> &,string,float);

//______________________________________________________________________

Section1Base::Section1Base( const unsigned char* octs )
{
   int slen = 65536*octs[0] + 256*octs[1] + octs[2];
   octets_ = new unsigned char[slen];
   memcpy( octets_, octs, slen );
}

Section1Base::Section1Base( const Section1Base* aSec1 )
{
   octets_ = new unsigned char[ aSec1->len() ];
   memcpy( octets_, aSec1->start(), aSec1->len() );
}

bool
Section1Base::isDifferent( const Section1Base* aSec1 ) const
{
   if( len() != aSec1->len() )
      return true;

   for( int i=0; i<len(); ++i )
      if( octets_[i] != aSec1->octets_[i] )
         return true;

   return false;
}

bool
Section1_preEd4::hasSection2()
{
   return octets_[7] > 127;   //-- octet 8
}
bool
Section1_Ed4::hasSection2()
{
   return octets_[9] > 127;   //-- octet 10
}

TDynamicTime
Section1_preEd4::date()
{
   //-- octet 13=year, 14=month, etc.
   return TDynamicTime( octets_[12], octets_[13], octets_[14], octets_[15], octets_[16] );
}
TDynamicTime
Section1_Ed4::date()
{
   //-- octet 13=year, 14=month, etc.
   return TDynamicTime( 256*octets_[15]+octets_[16], octets_[17], octets_[18]
                  , octets_[19], octets_[20], octets_[21] );
}

int
Section1_preEd4::msgType()
{
   return octets_[8];         //-- octet 9 in ed.3
}
int
Section1_Ed4::msgType()
{
   return octets_[10];         //-- octet 11 in ed.4
}

int
Section1_preEd4::msgSubtypeWMO()
{
   return cOctetMissingIndicator; //-- not available in ed.3
}
int
Section1_Ed4::msgSubtypeWMO()
{
   return octets_[11];         //-- octet 12 in ed.4
}

int
Section1_preEd4::msgSubtypeLocal()
{
   return octets_[9];         //-- octet 10 in ed.3
}
int
Section1_Ed4::msgSubtypeLocal()
{
   return octets_[12];         //-- octet 13 in ed.4
}

int
Section1_preEd4::msgSubtype()
{
   return msgSubtypeLocal();  //-- only local available in ed.3
}
int
Section1_Ed4::msgSubtype()
{
   //-- WMO subtype is the preferred one
   return msgSubtypeWMO() != cOctetMissingIndicator ? msgSubtypeWMO() : msgSubtypeLocal();
}

int
Section1_preEd4::origCentre()
{
  return octets_[5]; //-- octet 6
}
int
Section1_Ed4::origCentre()
{
   return 256*octets_[4] + octets_[5]; //-- octets 5 and 6
}

int
Section1_preEd4::origSubCentre()
{
  return octets_[4]; //-- octet 5
}
int
Section1_Ed4::origSubCentre()
{
   return 256*octets_[6] + octets_[7]; //-- octets 7 and 8
}

int
Section1_preEd4::masterTable()
{
   return octets_[3];         //-- octet 4
}
int
Section1_Ed4::masterTable()
{
   return octets_[3]; //-- octet 4
}

int
Section1_preEd4::masterTableVersion()
{
   return octets_[10];         //-- octet 11
}
int
Section1_Ed4::masterTableVersion()
{
   return octets_[13]; //-- octet 14
}

int
Section1_preEd4::localTableVersion()
{
   return octets_[11];         //-- octet 12
}
int
Section1_Ed4::localTableVersion()
{
   return octets_[14]; //-- octet 15
}



//====================================================================== MvBufrBase
//______________________________________________________________________

MvBufrBase :: MvBufrBase( const long len ) : Sec1(NULL),fSec2(NULL),fTotalSec2(NULL),fSec3(NULL),fSec4(NULL)
{
  _refCount = 0;

  // Make sure the data is correctly aligned.
  longptr = new long[(len/sizeof(long)) + 1];
  fMessage = (char *)longptr;
  //fMessage = new char[ len  + 8 ];    // +8 for an extra "safety word"
  fMessageLength = len;

  fKSUP  = NULL;
  fKSEC0 = NULL;
  fKSEC1 = NULL;
  fKSEC2 = NULL;
  fKSEC3 = NULL;
  fKSEC4 = NULL;

      //-- lazy evaluation: create static arrays when first needed --
  createDataArrays ();
    }
//___________________________________________________________

MvBufrBase :: ~MvBufrBase( void )
{
//  delete [] fMessage;
  delete [] longptr;

  if( fSec2 )
  {
    delete fSec2;
    delete [] fTotalSec2;
  }
  delete fSec3;
  delete fSec4;

  fSec2 = 0;
  fSec3 = 0;
  fSec4 = 0;
  fTotalSec2 = 0;

  if( fKSUP ) delete [] fKSUP;
  if( fKSEC0 ) delete [] fKSEC0;
  if( fKSEC1 ) delete [] fKSEC1;
  if( fKSEC2 ) delete [] fKSEC2;
  if( fKSEC3 ) delete [] fKSEC3;
  if( fKSEC4 ) delete [] fKSEC4;

  fKSUP = fKSEC0 = fKSEC1 = fKSEC2 = fKSEC3 = fKSEC4 = 0;
}
//___________________________________________________________
void
MvBufrBase :: attach( void )
{
   _refCount++;
}
void
MvBufrBase :: detach( void )
{
   if( --_refCount == 0 )
     delete this;
}
//_________________________________________________________ createFortranArrays
void
MvBufrBase :: createFortranArrays( void )
{
  fKSUP  = new fortint[  9 ];
  fKSEC0 = new fortint[  3 ];
  fKSEC1 = new fortint[ 40 ];
  fKSEC2 = new fortint[ 4096 ]; //[ 128 ]; //[ 64 ];
  fKSEC3 = new fortint[  4 ];
  fKSEC4 = new fortint[  2 ];
}
//_________________________________________________________ createDataArrays
void
MvBufrBase :: createDataArrays( void )
{
    kVals    = aMAX_KVALS[arraySizeIndex];
    In_KVALS = kVals;

    try
    {
        if( In_VALUES == 0 )
            In_VALUES = new fortfloat[ kVals ];

        if( In_CVALS == 0 )
            In_CVALS = new char[ kVals * 80 ];

        if( In_CNAMES == 0 )
            In_CNAMES = new char[ MAX_KELEM * 64 ];

        if( In_CUNITS == 0 )
            In_CUNITS = new char [ MAX_KELEM * 24 ];

        if( In_KTDEXP == 0 )
            In_KTDEXP = new fortint[ MAX_KELEM ];

        if( In_KTDLST == 0 )
            In_KTDLST = new fortint[ MAX_KELEM ];
    }

    catch(...)
    {
        deleteDataArrays ();

#ifdef METVIEW
        marslog( LOG_EROR, "MvBufrBase::MvBufrBase: out-of-memory!" );
        throw MvException( "MvBufrBase::MvBufrBase: out-of-memory!" );
#else
        cerr << ">>>> MvBufrBase::MvBufrBase: out-of-memory => throw an exception <<<<" << endl;
        throw std::bad_alloc();
#endif
    }
}
//_________________________________________________________ createDataArrays
void
MvBufrBase :: deleteDataArrays( void )
{
    if( In_VALUES != 0 )
    {
        delete [] In_VALUES; In_VALUES = 0;
    }

    if( In_CVALS != 0 )
    {
        delete [] In_CVALS;  In_CVALS  = 0;
    }

    if( In_CNAMES != 0 )
    {
        delete [] In_CNAMES; In_CNAMES = 0;
    }

    if( In_CUNITS != 0 )
    {
        delete [] In_CUNITS; In_CUNITS = 0;
    }

    if( In_KTDEXP != 0 )
    {
        delete [] In_KTDEXP; In_KTDEXP = 0;
    }

    if( In_KTDLST != 0 )
    {
        delete [] In_KTDLST; In_KTDLST = 0;
    }
}
//___________________________________________________________
unsigned int
MvBufrBase :: unsignedInt( const unsigned char* firstOctet
                         , int octetCount )
{
  unsigned int value = *firstOctet;
  const unsigned char* anOctet = ++firstOctet;
  for( int i=octetCount-1; i; --i )
  {
    value = 256*value + *anOctet++;
  }
  return value;
}

//______________________________________________________________________
//====================================================================== MvBufr
//______________________________________________________________________

//______________________________________________________________________

MvBufr :: MvBufr( char *msg, long len, long aMessageNumber )
    : MvBufrBase( len )   //( msg, len )
{
  _lastKnownSubsetValue = 1; //-- Q&D

  memcpy( fMessage, msg, (int)len );

  const unsigned char* msgStart = (const unsigned char*)fMessage;

  fSec0 = (TSection0 *)fMessage;
  int offSet = fSec0->editionNr < 2 ? 4 : 8;
                        // Section 0 was shorter in BUFR editions 0 and 1 !!!!
                        //                                  rev vk 950802
  if( fSec0->editionNr > 3 )
    Sec1 = new Section1_Ed4( msgStart + offSet );
  else
    Sec1 = new Section1_preEd4( msgStart + offSet );
  offSet += Sec1->len();

  if( Sec1->hasSection2() )         // bit 0 <=> Optional Section 2 present?
  {
    fSec2 = new TSection2;
    memcpy( (char *)fSec2, msgStart + offSet, sizeof(TSection2)  );

    int sec2Len = unsignedInt( &(fSec2->len), 3 );
    fTotalSec2 = new unsigned char[ sec2Len ];
    memcpy( (char *)fTotalSec2, msgStart + offSet, sec2Len );

    offSet += sec2Len;
  }
  else
  {
    fSec2 = NULL;
    fTotalSec2 = NULL;
  }

  fSec3 = new TSection3;
  memcpy( (char *)fSec3, msgStart + offSet, sizeof(TSection3)  );
  offSet += unsignedInt( &(fSec3->len), 3 );

  fSec4 = new TSection4;
  memcpy( (char *)fSec4, msgStart + offSet, sizeof(TSection4)  );
  offSet += unsignedInt( &(fSec4->len), 3 );

  computeIn_KELEM ();

  fMessageNumber = aMessageNumber;
  _currentDescr = 0;
  _currentDescrInd = -1;
  _inState = kBufrIn_Coded;
  _bufrIn_ref++;
  _bufrBoxFilledSubset = 0;
}
//______________________________________________________________________

MvBufr :: ~MvBufr( void ) { }

//____________________________________________________________________ Decode
void
MvBufr :: Decode( void )
{
   if( _inState == kBufrIn_Error )
      return;

   if( _inState < kBufrIn_DataDecoded )
   {
      createFortranArrays();
      fortint myKBUFL = fMessageLength / sizeof(fortint) + 1;  // +1 = Q&D
        bool keepGoing = true;

        // we will try to decode the BUFR a number of times, starting with
        // relatively small data arrays, then building up to larger ones
        // (so that we don't waste too much memory)

        while (keepGoing)
        {
            keepGoing = false;

      BUFREX(&myKBUFL
	    , (fortint *)fMessage  // buffer for the BUFR message
	    , fKSUP          // array for suplementary info
	    , fKSEC0         // FORTRANized section 0
	    , fKSEC1         // FORTRANized section 1
	    , fKSEC2         // FORTRANized section 2 (site dependent)
	    , fKSEC3         // FORTRANized section 3 (data descriptors)
	    , fKSEC4         // FORTRANized section 4 (data values)
	    ,&In_KELEM
	    , In_CNAMES
	    , In_CUNITS
	    ,&In_KVALS
	    , In_VALUES
	    , In_CVALS
	    ,&fKERR );


            // if KELEM is not big enough, then we will
            // try again with a bigger value

            if( fKERR == 25 || fKERR == 14)
            {
                cout << "MvBufr :: Decode - kVals of " << kVals << " and In_KELEM of " << In_KELEM
                     << " is not large enough." << endl;

                if (++arraySizeIndex < NUM_MAX_KVALS)
                {
                    cout << "Trying kVals of " << aMAX_KVALS[arraySizeIndex] <<
                         " (" << (aMAX_KVALS[arraySizeIndex] * 80) / (1024*1024) << "MB)" << endl;

                    deleteDataArrays ();
                    createDataArrays ();
                    computeIn_KELEM ();

                    keepGoing = true;
                }
                else
                    arraySizeIndex = NUM_MAX_KVALS - 1; // should not be used, but just in case,
                                                        // make sure the index is not out of bounds...
            }
        }


      if( fKERR )
      {
	 cerr << "In_KELEM " << In_KELEM << " kvals " << In_KVALS << endl;
	 cerr << " >>> MvBufr::Decode, bufrmsg " << fMessageNumber
	      << ": fKERR = " << fKERR << endl;


#ifdef METVIEW
	 marslog(LOG_EROR, "BUFR decoding (BUFREX) failed, status = %d", fKERR );
	 stringstream sst;
	 sst << "Unable to decode BUFR message \nBUFR decoding (BUFREX) failed, status =" << fKERR;
	 throw MvException(sst.str().c_str());

#endif
	 _inState = kBufrIn_Error;
	 return;
      }

      _inState = kBufrIn_DataDecoded;
   }
}

//____________________________________________________________________ Decode
void
MvBufr :: Decode_012( void )
{
   if( _inState == kBufrIn_Error )
      return;

   if( _inState < kBufrIn_Sections012Expanded)
   {
      createFortranArrays();
      fortint myKBUFL = fMessageLength / sizeof(fortint) + 1;  // +1 = Q&D

      BUS012(&myKBUFL
	    , (fortint *)fMessage  // buffer for the BUFR message
	    , fKSUP          // array for suplementary info
	    , fKSEC0         // FORTRANized section 0
	    , fKSEC1         // FORTRANized section 1
	    , fKSEC2         // FORTRANized section 2 (site dependent)
	    ,&fKERR );



      if( fKERR )
      {

	 cerr << " >>> MvBufr::Decode_012, bufrmsg " << fMessageNumber
	      << ": fKERR = " << fKERR << endl;

#ifdef METVIEW
	 marslog( LOG_EROR, "BUFR decoding (BUFREX) failed, status = %d", fKERR );
	 stringstream sst;
	 sst << "Unable to decode BUFR message \nBUFR decoding (BUFREX) failed, status =" << fKERR;
	 throw MvException(sst.str().c_str());
#endif
	 _inState = kBufrIn_Error;
	 return;
      }

      _inState = kBufrIn_Sections012Expanded;
   }
}

//___________________________________________________________________ ExpandDescriptors
void
MvBufr :: ExpandDescriptors( int subsetNumber )
{
  static int mySubsetSavedValue = 0;

   _lastKnownSubsetValue = subsetNumber;

   if( _inState == kBufrIn_Error )
      return;

//   In_KTDLEN = -1;
//   In_KTDEXL = -1;
   fKERR     =  0;

   if( _inState < kBufrIn_DataDecoded )
      Decode();

   if( ( fKSEC3[3-1] > 1  &&                           //-- if several subsets
        _lastKnownSubsetValue != mySubsetSavedValue )  //-- AND subset number changed
                      ||                               //-- OR
        _inState == kBufrIn_DataDecoded  )             //-- descriptors not yet expanded
   {
//cout << "-----> MvBufr::ExpandDescriptors(" << _lastKnownSubsetValue << ") => BUSEL2..." << endl;
//      CALL BUSEL2(KSUBSET,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
         BUSEL2( &_lastKnownSubsetValue  //-- Q&D variable...
               , &In_KELEM
               , &In_KTDLEN      // nr of original data descriptors in Section 3
               ,  In_KTDLST      // original descriptors
               , &In_KTDEXL      // nr of expanded data descriptors
               ,  In_KTDEXP      // expanded descriptors
               ,  In_CNAMES
               ,  In_CUNITS
               , &fKERR
               );

         mySubsetSavedValue = _lastKnownSubsetValue;
   }
   else
   {
      if( _inState == kBufrIn_DataDecoded )
      {
//cout << "-----> MvBufr::ExpandDescriptors(" << _lastKnownSubsetValue << ") => BUSEL..." << endl;
         BUSEL( &In_KTDLEN      // nr of original data descriptors in Section 3
	      ,  In_KTDLST      // original descriptors
	      , &In_KTDEXL      // nr of expanded data descriptors
	      ,  In_KTDEXP      // expanded descriptors
	      , &fKERR
              );
      }
   }

   if( fKERR )
   {
      cerr << " >>> MvBufr::ExpandDescriptors: fKERR = " << fKERR << endl;

#ifdef METVIEW
      marslog( LOG_EROR, "BUFR expansion (BUSEL) failed, status = %d", fKERR );
#endif

      _inState = kBufrIn_Error;
   }
   else
      _inState = kBufrIn_DataAndDescriptorsDecoded;
}
//__________________________________________________________ descriptorToFortranIndex
int
MvBufr :: descriptorToFortranIndex( const long aDescr, const int firstIndex )
{
   if( _inState == kBufrIn_Error )
      return -1;

   if( _inState != kBufrIn_DataAndDescriptorsDecoded )
      ExpandDescriptors( _lastKnownSubsetValue ); //-- Q&D

   if( _inState == kBufrIn_DataAndDescriptorsDecoded )
   {
      for( int i=firstIndex; i < In_KTDEXL; i++ )
      {
	 if( In_KTDEXP[ i ] == aDescr )
         {
	   return i;
         }
      }
   }
   return -1;
}
//__________________________________________________________ computeIn_KELEM
void
MvBufr :: computeIn_KELEM( void )
{
    if ( subsetCount() > 1 )
        In_KELEM = kVals / subsetCount();
    else
        In_KELEM = MAX_KELEM;

    if( In_KELEM > MAX_BUBOX_KELEM_LIMIT )
        In_KELEM = MAX_BUBOX_KELEM_LIMIT;
}
//__________________________________________________________ DataValue
//
// Returns the requested parameter value (or missing value)
// and updates _currentDescrInd.
//

fortfloat
MvBufr :: DataValue( const int aDescrArrayInd, const long aSubsetNumber )
{
   _lastKnownSubsetValue = aSubsetNumber;

   if( _inState < kBufrIn_DataDecoded )
      Decode();

   if( ( aSubsetNumber > subsetCount() ) || ( aDescrArrayInd < 0 ) )
   {
      _currentDescr = 0;
      _currentDescrInd = -1;
      return kFortranBufrMissingValue;
   }

   _currentDescrInd = aDescrArrayInd;
   _currentDescr = In_KTDEXP[ _currentDescrInd ];

   return PeekDataValue( _currentDescrInd, aSubsetNumber );
}
//__________________________________________________________ PeekDataValue
//
// Returns the requested parameter value (or missing value)
// without updating _currentDescrInd.
//

fortfloat
MvBufr :: PeekDataValue( const int aDescrArrayInd, const long aSubsetNumber )
{
   if( ( aSubsetNumber > subsetCount() ) || ( aDescrArrayInd < 0 ) )
     return kFortranBufrMissingValue;
   else
     return In_VALUES[ aDescrArrayInd + ( aSubsetNumber - 1 ) * In_KELEM ];
}
//__________________________________________________________ Value
bool
MvBufr :: Value( const long aDescriptor
	       , const long aSubsetNumber
	       , fortfloat &aDataValue
	       , int   firstInd )
{
  aDataValue = DataValue( descriptorToFortranIndex( aDescriptor, firstInd )
			, aSubsetNumber );
  return aDataValue != kFortranBufrMissingValue ? true : false;
}
//__________________________________________________________ intValue
// returns 'kFortranBufrMissingIntValue' if not found!
//----------------------------------------------------
long
MvBufr :: intValue( const long aDescriptor, const int subsetNr )
{
   fortfloat myValue;
   Value( aDescriptor, subsetNr, myValue );

   if( myValue != kFortranBufrMissingValue )
     return (long)myValue;
   else
     return kFortranBufrMissingIntValue;
}
//____________________________________________________________________ feedbackValue
double
MvBufr::feedbackValue( int col, int subset )
{
   if( _currentDescrInd < 0 )
      return kBufrMissingValue;
   else
      //-- first 6 rows is reserved info, obs report starts on row 7
      return feedbackValue( _currentDescrInd + 6, col, subset );
}

double
MvBufr::feedbackValue( int row, int col, int subset )
{
   int err = fillBufrBox( subset );

   assert( row > 0 && row <= myKBOX );
   assert( col > 0 && col <= myKAPP );

   if( err == 0 )
      return myVALS[ myKLEN*(col-1) + row - 1 ];
   else
      return kBufrMissingValue;
}

string
MvBufr::feedbackItemName( int row, int subset )
{
   int err = fillBufrBox( subset );

   assert( row > 0 && row <= myKBOX );

   if( err == 0 )
   {
      char c[64];
      strncpy(c,myCBOXN+(row-1)*64,63);
      c[63]='\0';
      string s(c);
      eraseWhiteSpaceFromStringEnd(s);
      return s;
   }
   else
      return std::string();
}

string
MvBufr::feedbackItemUnit( int row, int subset )
{
   int err = fillBufrBox( subset );

   assert( row > 0 && row <= myKBOX );;

   if( err == 0 )
   {
      char c[24];
      strncpy(c,myCBOXU+(row-1)*24,23);
      c[23]='\0';
      string s(c);
      eraseWhiteSpaceFromStringEnd(s);
      return s;
   }
   else
      return std::string();
}

//____________________________________________________________________ obsTime
TDynamicTime
MvBufr :: obsTime( const int subsetNr )
{
   fortint myYear  = intValue( 4001L, subsetNr );
   fortint myMonth = intValue( 4002L, subsetNr );
   fortint myDay   = intValue( 4003L, subsetNr );
   fortint myHour  = intValue( 4004L, subsetNr );

   short myMin, mySec;
   fortfloat myValue;

   if( Value( 4005L, subsetNr, myValue ) )
      myMin = (short)myValue;
   else
      myMin = 0;

   if( Value( 4006L, subsetNr, myValue ) )
      mySec = (short)myValue;
   else
      mySec = 0;

   //-- quirky NCEP PrepBUFR obs may not contain date/time infromation
   if( myYear  == kBufrMissingIntValue &&   //-- date OK?
       myMonth == kBufrMissingIntValue &&
       myDay   == kBufrMissingIntValue  )
   {                                        //-- if date missing from obs
      return msgTime();                     //-- then take it from section 1
   }
   else                                     //-- OK, take from obs
   {
      return TDynamicTime( (short)myYear, (short)myMonth, (short)myDay
                         , (short)myHour, myMin, mySec );
   }
}
//____________________________________________________________________ msgTime
TDynamicTime
MvBufr :: msgTime( void )
{
#if 0
   short myYear  = fSec1->yearYY ;
   short myMonth = fSec1->month ;
   short myDay   = fSec1->day ;
   short myHour  = fSec1->hour ;
   short myMin   = fSec1->minute ;

   return TDynamicTime( myYear, myMonth, myDay, myHour, myMin, 0 );
#endif

   return Sec1->date();
}
//____________________________________________________________________ stringValue
string
MvBufr :: stringValue( const long aDescriptor, const int aSubsetNr )
{
   _currentDescrInd = descriptorToFortranIndex( aDescriptor );
   return stringValueByIndex( _currentDescrInd, aSubsetNr );
}
//____________________________________________________________________ stringValue
string
MvBufr :: stringValue( const int aSubsetNr )
{
   return stringValueByIndex( _currentDescrInd, aSubsetNr );
}
//__________________________________________________________ stringValueByIndex
string
MvBufr :: stringValueByIndex( const int anIndex, const int aSubsetNr )
{
   if( ( anIndex < 0 ) || ( anIndex >= In_KTDEXL ) )
     {
       return string( "[string index error!]" );
     }

   //-- get coded float value --
   fortfloat myValue = DataValue( anIndex, aSubsetNr );

   //-- here we should be passing subset nr, not index!!! (020307/vk)
   //-- thus always pass 1:

   if( elementValueType( 1 ) == kEVT_missing )
     {
	   return string( "[Missing]" );
     }
   else if( elementValueType( 1 ) == kEVT_string )
     {
       //-- get pointer and length to In_CVALS array, In_CVALS starts from 0.. --
       int myIndex = (int)myValue / 1000 - 1;
       int myLength = (int)myValue % 1000;

       //-- add C-terminator (sacrifice last character if necessary) --
       int terminatorPos = myLength;
       if( terminatorPos > 79 )
	 terminatorPos = 79;
       //In_CVALS[ myIndex ][ terminatorPos ] = '\0';
       In_CVALS[ myIndex*80 + terminatorPos ] = '\0';

       return string(In_CVALS + myIndex*80);
     }
   else if( elementValueType( 1 ) == kEVT_numeric )
     {
       ostringstream oss;
       oss << myValue;

       return oss.str();
     }

   return string( "[Internal error]" ); //-- we should never get here!
}
//____________________________________________________________________ unit
string
MvBufr :: unit( const long aDescriptor )
{
   return unitByIndex( descriptorToFortranIndex( aDescriptor ) );
}
//____________________________________________________________________ unit
string
MvBufr :: unit( void )
{
   return unitByIndex( _currentDescrInd );
}
//____________________________________________________________________ unitByIndex
string
MvBufr :: unitByIndex( const int anIndex )
{
   char strbuf[ 25 ];

   if( ( anIndex >= 0 ) && ( anIndex < In_KTDEXL ) )
   {
      strbuf[ 24 ] = '\0';               //-- make a copy
      int pos;
      for( pos=23; pos>=0; --pos )
         strbuf[ pos ] = In_CUNITS[ anIndex*24 + pos ];
         //strbuf[ pos ] = In_CUNITS[ anIndex ][ pos ];

      for( pos=23; pos > 0; pos-- )      //-- remove trailing blanks
         if( strbuf[ pos ] == ' ' )
            strbuf[ pos ] = '\0';
         else
            break;

      return string( strbuf );
   }
   return string( "[Unit not found!]" );
}
//____________________________________________________________________ name
string
MvBufr :: name( const long aDescriptor )
{
   return nameByIndex( descriptorToFortranIndex( aDescriptor ) );
}
//____________________________________________________________________ name
string
MvBufr :: name( void )
{
   return nameByIndex( _currentDescrInd );
}
//____________________________________________________________________ nameByIndex
string
MvBufr :: nameByIndex( const int anIndex )
{
   char strbuf[ 65 ];

   if( ( anIndex >= 0 ) && ( anIndex < In_KTDEXL ) )
   {
      strbuf[ 64 ] = '\0';               //-- make copy
      int pos;
      for( pos=63; pos>=0; --pos )
         strbuf[ pos ] = In_CNAMES[ anIndex*64 + pos ];
        // strbuf[ pos ] = In_CNAMES[ anIndex ][ pos ];

      for( pos=63; pos > 0; pos-- )      //-- remove trailing blanks
         if( strbuf[ pos ] == ' ' )
            strbuf[ pos ] = '\0';
         else
            break;

      bool retainCapital = true;      //-- change to lower case
      for( pos = 0; pos < (int)(strlen( strbuf )); pos++ )
      {
         if( isupper( strbuf[ pos ] ) )
         {
            if( retainCapital )
               retainCapital = false;
            else
               strbuf[ pos ] = tolower( strbuf[ pos ] );
          }
          else
             retainCapital = true;
      }
      return string( strbuf );
   }
   else
      return string( "[Name index error!]" );
}
//____________________________________________________________________ elementValueType
EElementValueType
MvBufr :: elementValueType( const int aSubsetNr )
{
  return elementValueTypeByIndex( _currentDescrInd, aSubsetNr );
}
//____________________________________________________________ elementValueType
EElementValueType
MvBufr :: elementValueType( const long aDescriptor, const int aSubsetNr )
{
  return elementValueTypeByIndex( descriptorToFortranIndex( aDescriptor ), aSubsetNr );
}
//_____________________________________________________ elementValueTypeByIndex
EElementValueType
MvBufr :: elementValueTypeByIndex( const int anIndex, const int aSubsetNr )
{
  fortfloat myValue = DataValue( anIndex, aSubsetNr );

  if( myValue == kFortranBufrMissingValue )
  {
    return kEVT_missing;
  }
  else
  {
    bool isString = unitByIndex( anIndex ) == "CCITTIA5"   //-- ECMWF notation
                 || unitByIndex( anIndex ) == "CCITT IA5"; //-- WMO & NCEP PrepBUFR notation
    return isString ? kEVT_string : kEVT_numeric;
  }
}
//______________________________________________________________________
//
// Reset descriptor iterator by pointing to the first descriptor.
// Make sure msg has been expanded!
//

bool
MvBufr :: SetFirstDescriptor( void )
{
   if( _inState == kBufrIn_Error )
      return false;

   if( _inState != kBufrIn_DataAndDescriptorsDecoded )
      ExpandDescriptors( _lastKnownSubsetValue ); //-- Q&D

   if( _inState == kBufrIn_DataAndDescriptorsDecoded )
   {
      _currentDescrInd = 0;
      _currentDescr = In_KTDEXP[ _currentDescrInd ];
      return true;
   }
   return false;
}
//______________________________________________________________________
//
// Advance descriptor iterator.
// Check that it still points ok.
//

bool
MvBufr :: SetNextDescriptor( void )
{
  if( _currentDescrInd < 0 )
     return false;           // SetFirstDescriptor had not been called !

  _currentDescrInd++;
  if( _currentDescrInd == In_KTDEXL ) // or: fKSUP[ 5 - 1 ]
  {
     _currentDescrInd = -1;               //-- end-of-msg reached
     _currentDescr = 0;                   //-- probably end-of-iteration => issue no msg
     return false;
  }
  else if( _currentDescrInd > In_KTDEXL )
  {
     _currentDescrInd = -1;               //-- past end-of-msg
     _currentDescr = 0;                   //-- must be an error => issue error msg

    cerr << "MvBufr::SetNextDescriptor: _currentDescrInd=" << _currentDescrInd
         << ", limiting In_KTDEXL=" << In_KTDEXL << endl;

     return false;
  }

  _currentDescr = In_KTDEXP[ _currentDescrInd ];
  return true;
}
//______________________________________________________________________
//
// calls Emoslib routines BUBOX and BUPRTBOX to produce "boxed" output
//
int
MvBufr::fillBufrBox( int aSubsetNr )
{
#if 0
  //-- Linux/g++ does not like hardcoded big? arrays (works ok in debugger,
  //-- but crashes inside Metview...) => create arrays dynamically, once!

  static fortint    myKSUB, myKBOX, myKAPP, myKLEN, myKERR;
  static fortint*   myKBOXR = 0;
  static fortfloat* myVALS  = 0;
  static char*      myCBOXN = 0;
  static char*      myCBOXU = 0;
#endif

  if( _bufrBoxFilledSubset == aSubsetNr )
     return 0;                               //-- OK, already filled

  if( ! myKBOXR )
    {
      try
	{
	  myKBOXR = new fortint[ kVals ];
	  cout << " fillBufrBox: array myKBOXR created" << endl;

	  myVALS  = new fortfloat[ kVals ];
	  cout << " fillBufrBox: array myVALS  created" << endl;

	  myCBOXN = new char[ kVals * 64 ];
	  cout << " fillBufrBox: array myCBOXN created" << endl;

	  myCBOXU = new char[ kVals * 24 ];
	  cout << " fillBufrBox: array myCBOXU created" << endl;
	}
      catch(...)
	{
	  cout << " >>> fillBufrBox: problems in creating fort arrays <<<" << endl;
	  delete myKBOXR;  myKBOXR = 0;
	  delete myVALS;   myVALS  = 0;
	  delete myCBOXN;  myCBOXN = 0;
	  delete myCBOXU;  myCBOXU = 0;
#ifdef METVIEW
	  marslog( LOG_EROR, "MvBufr::fillBufrBox: out-of-memory?" );
#endif
	  return -13;
	}
    }


  //-- Initialize array given to bubox. bubox will not initialize
  //-- all values, and this will cause runtime error from buprtbox.
  for (int i = 0; i < kVals; i++ )
    myVALS[i] = kFortranBufrMissingValue;

  if( _inState == kBufrIn_Error )
    {
      cout << " fillBufrBox: BUFR msg error state, return false" << endl;
      return -1313;
    }

  if( _inState < kBufrIn_DataDecoded )
    {
      Decode();
    }

  if( _inState != kBufrIn_DataAndDescriptorsDecoded )
    {
      ExpandDescriptors( aSubsetNr );
    }

  myKSUB = (fortint)aSubsetNr;
  myKBOX = 0;
  myKAPP = 0;
  myKLEN = 0;
  myKERR = 0;

  BUBOX( &myKSUB     //-- INPUT arguments
       ,  fKSUP
       , &In_KELEM
       ,  In_KTDEXP
       ,  In_CNAMES
       ,  In_CUNITS
       , &In_KVALS
       ,  In_VALUES
                      //-- OUTPUT arguments
       , &myKBOX      //-- number of (valid) elements in 1st column
       , &myKAPP      //-- number of columns (apps) in the box
       , &myKLEN      //-- number of rows in the box
       ,  myKBOXR     //-- Table B descriptors
       ,  myVALS      //-- boxed values
       ,  myCBOXN     //-- boxed element names
       ,  myCBOXU     //-- boxed units
       , &myKERR );

  if( myKERR == 0 )
      _bufrBoxFilledSubset = myKSUB;

  return myKERR;
}
//______________________________________________________________________
//
bool
MvBufr::writeBufrBox( int aSubsetNr )
{
  cout << " writeBufrBox: entering" << endl;

  myKERR = fillBufrBox( aSubsetNr );

  //-- forces Fortran unit 6 into a file
  if( ! redirect_6(BBOXNAME.c_str()) )
    {
      cout << ">>> UNABLE TO REDIRECT stdout <<<" << endl;
      return false;
    }

  if( myKERR == 0 )
  {
     BUPRTBOX(&myKBOX
             ,&myKAPP
             ,&myKLEN
             , myKBOXR
             , myVALS
             , myCBOXN
             , myCBOXU );
  }

  //-- closes "unit'ified" unit 6
  if( ! reconnect_6() )
    {
      cerr << ">>> UNABLE TO RECONNECT TO stdout <<<" << endl;
      cout << ">>> UNABLE TO RECONNECT TO stdout <<<" << endl;
    }

  return myKERR == 0;
}

bool
MvBufr::getBufrBoxSize( int& rows, int& cols, int aSubsetNr )
{
  bool status = false;

  myKERR = fillBufrBox( aSubsetNr );

  if( myKERR == 0 )
  {
     rows = myKBOX;
     cols = myKAPP;
     status = true;
  }
  return status;
}

//------------- Printing functions -----------------
bool
MvBufr :: printSection( ostream& aStream,int which)
{
  bool return_val = true;
  if( _inState == kBufrIn_Error )
    {
      aStream << "!!!!!!!!!!! Bad BUFR message " << endl;
      return false;
    }

  if ( _inState < kBufrIn_Sections012Expanded )
    Decode();

  if ( which == 3 && _inState < kBufrIn_DataAndDescriptorsDecoded )
    ExpandDescriptors( _lastKnownSubsetValue ); //-- Q&D


  char sec_name[30];
  sprintf(sec_name,"prtsec%d.txt",which);

  redirect_6(sec_name);
  if ( which == 0 )
    BUPRS0(fKSEC0);
  else if ( which == 1 )
    BUPRS1(fKSEC1);
  else if ( which == 2 )
    {
      fortint *fKEY = new fortint[60];
      BUUKEY(fKSEC1,fKSEC2,fKEY,fKSUP,&fKERR);
      if ( fKERR )
	cout << "\nProblems getting key. Maybe non-existent? " << endl;
      else
	BUPRS2(fKSUP,fKEY);

      delete [] fKEY;
    }
  else
    BUPRS3(fKSEC3,&In_KTDLEN,In_KTDLST,&In_KTDEXL,In_KTDEXP,&In_KELEM,In_CNAMES);

  reconnect_6();
  return_val = file_to_stream(sec_name,aStream,1);
  delete_print_file(sec_name);
  return return_val;
}

//------------- Printing functions -----------------
bool
MvBufr::getDataFromSection2(map<string,string> &data)
{
    bool retval=false;

    if( _inState == kBufrIn_Error )
    {
      cout << "!!!!!!!!!!! Bad BUFR message " << endl;
      return false;
    }

    if ( _inState < kBufrIn_Sections012Expanded )
         Decode_012();

      fortint *fKEY = new fortint[60];
      BUUKEY(fKSEC1,fKSEC2,fKEY,fKSUP,&fKERR);
      if ( fKERR )
      {
		cout << "\nProblems getting key. Maybe non-existent? " << endl;
		retval=false;
      }
      else
      {
		parseSection2(fKEY,data);
		retval=true;
      }

      delete [] fKEY;

      return retval;
}

void MvBufr::parseSection2(fortint *fKEY,map<string,string> &data)
{
	//fKSUP - global variable

	if(fKSUP[CIND(2)] < 1)
	{
		return;
	}

	int type=0;
	if(fKEY[CIND(2)] == 2)
		type = 2;
	else if(fKEY[CIND(2)] == 3)
		type = 2;
	else if(fKEY[CIND(2)] == 12)
		type = 2;
	else if(fKEY[CIND(2)] == 8)
		type = 2;

	if(type == 0 && fKSUP [CIND(6)] > 1)
		type = 2;

	if(type == 2)
	{
		keyToStringMap(data,"RDB DATA TYPE",fKEY,2);
		keyToStringMap(data,"RDB DATA SUBTYPE",fKEY,3);
		keyToStringMap(data,"YEAR",fKEY,4);
		keyToStringMap(data,"MONTH",fKEY,5);
		keyToStringMap(data,"DAY",fKEY,6);
		keyToStringMap(data,"HOUR",fKEY,7);
		keyToStringMap(data,"MINUTE",fKEY,8);
		keyToStringMap(data,"SECOND",fKEY,9);

		float RLAT1=(fKEY[CIND(11)]-9000000)/100000.;
         	float RLON1=(fKEY[CIND(10)]-18000000)/100000.;
		keyToStringMap(data,"LATITUDE 1",RLAT1);
		keyToStringMap(data,"LONGITUDE 1",RLON1);

		float RLAT2=(fKEY[CIND(13)]-9000000)/100000.;
         	float RLON2=(fKEY[CIND(12)]-18000000)/100000.;
		keyToStringMap(data,"LATITUDE 2",RLAT2);
		keyToStringMap(data,"LONGITUDE 2",RLON2);

		keyToStringMap(data,"NUMBER OF OBSERVATIONS",fKEY,14);

		//char ident[9];
		//memcpy(ident,&fKEY[CIND(15)],8);
		//data["IDENTIFIER"]=string(ident);

		keyToStringMap(data,"IDENTIFIER",fKEY,15);
		keyToStringMap(data,"TOTAL BUFR MESSAGE LENGTH",fKEY,25);
		keyToStringMap(data,"DAY (RDB INSERTION)",fKEY,26);
		keyToStringMap(data,"HOUR (RDB INSERTION)",fKEY,27);
		keyToStringMap(data,"MINUTE (RDB INSERTION)",fKEY,28);
		keyToStringMap(data,"SECOND (RDB INSERTION)",fKEY,29);
		keyToStringMap(data,"DAY (MDB ARRIVAL)",fKEY,30);
		keyToStringMap(data,"HOUR (MDB ARRIVAL)",fKEY,31);
		keyToStringMap(data,"MINUTE (MDB ARRIVAL",fKEY,32);
		keyToStringMap(data,"SECOND (MDB ARRIVAL)",fKEY,33);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,34);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,35);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,37);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,38);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,40);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,41);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,43);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,44);
		keyToStringMap(data,"QUALITY CONTROL % CONF",fKEY,46);
	}
	else
	{
		keyToStringMap(data,"RDB DATA TYPE",fKEY,2);
		keyToStringMap(data,"RDB DATA SUBTYPE",fKEY,3);
		keyToStringMap(data,"YEAR",fKEY,4);
		keyToStringMap(data,"MONTH",fKEY,5);
		keyToStringMap(data,"DAY",fKEY,6);
		keyToStringMap(data,"HOUR",fKEY,7);
		keyToStringMap(data,"MINUTE",fKEY,8);
		keyToStringMap(data,"SECOND",fKEY,9);

		float RLAT1=(fKEY[CIND(11)]-9000000)/100000.;
         	float RLON1=(fKEY[CIND(10)]-18000000)/100000.;
		keyToStringMap(data,"LATITUDE 1",RLAT1);
		keyToStringMap(data,"LONGITUDE 1",RLON1);

		//char ident[9];
		//memcpy(ident,&fKEY[CIND(15)],8);
		//data["IDENTIFIER"]=string(ident);

		char ident[10];
		for(int i=16; i <=24; i++ )
		{
			ident[i-16]=fKEY[CIND(i)];
		}
		ident[9]='\0';
		data["IDENTIFIER"]=string(ident);

		keyToStringMap(data,"TOTAL BUFR MESSAGE LENGTH",fKEY,25);
		keyToStringMap(data,"DAY (RDB INSERTION)",fKEY,26);
		keyToStringMap(data,"HOUR (RDB INSERTION)",fKEY,27);
		keyToStringMap(data,"MINUTE (RDB INSERTION)",fKEY,28);
		keyToStringMap(data,"SECOND (RDB INSERTION)",fKEY,29);
		keyToStringMap(data,"DAY (MDB ARRIVAL)",fKEY,30);
		keyToStringMap(data,"HOUR (MDB ARRIVAL)",fKEY,31);
		keyToStringMap(data,"MINUTE (MDB ARRIVAL",fKEY,32);
		keyToStringMap(data,"SECOND (MDB ARRIVAL)",fKEY,33);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,34);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,35);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,37);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,38);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,40);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,41);
		keyToStringMap(data,"CORRECTION NUMBER",fKEY,43);
		keyToStringMap(data,"PART OF MESSAGE",fKEY,44);
		keyToStringMap(data,"QUALITY CONTROL % CONF",fKEY,46);
	}
}






//------------- Printing functions -----------------
bool
MvBufr :: printSection_012( ostream& aStream,int which)
{
  if(which <0 || which > 2)
	return false;

  bool return_val = true;
  if( _inState == kBufrIn_Error )
    {
      aStream << "!!!!!!!!!!! Bad BUFR message " << endl;
      return false;
    }

  if ( _inState < kBufrIn_Sections012Expanded )
    Decode_012();

  char sec_name[30];
  sprintf(sec_name,"prtsec%d.txt",which);

  redirect_6(sec_name);
  if ( which == 0 )
    BUPRS0(fKSEC0);
  else if ( which == 1 )
    BUPRS1(fKSEC1);
  else if ( which == 2 )
    {
      fortint *fKEY = new fortint[60];
      BUUKEY(fKSEC1,fKSEC2,fKEY,fKSUP,&fKERR);
      if ( fKERR )
	cout << "\nProblems getting key. Maybe non-existent? " << endl;
      else
	BUPRS2(fKSUP,fKEY);

      delete [] fKEY;
    }

  reconnect_6();
  return_val = file_to_stream(sec_name,aStream,1);
  delete_print_file(sec_name);
  return return_val;
}


//______________________________________________________________________
//====================================================================== MvBufrOut
//______________________________________________________________________

MvBufrOut::MvBufrOut( const long len, MvObsSet* aSet )  //( char *msg, long len, MvObsSet* aSet )
    : MvBufrBase( len ), _currentSec1( 0 )
{
  _outSet = aSet;
  _maxNrSubsets = 1;
  Out_KELEM = -1;        // MAX_KELEM / _maxNrSubsets;

  createFortranArrays();

  _outState = kBufrOut_noBuffers;
  resetBuffers();
}
//____________________________________________________________

MvBufrOut :: ~MvBufrOut( void )
{
  if(   _outState == kBufrOut_dataInBuffers )
  {
    encode();
  }
  _outSet->close();
  delete _currentSec1;
}

//____________________________________________________________________ createBuffers
void
MvBufrOut::createBuffers() // XXX still need more dynamic memory allocation
{
  Out_KTDEXL = -1;
  Out_KELEM = -1;
  _KDLEN = 0;
  _nextValue = 0;
  _nextCharParamPos = 0;

  if( _outState == kBufrOut_noBuffers )
    {
      char cbuf[ 120 ];

      try
	{
	  cout << "MvBufrOut::createBuffers, checking for memory..." << endl;

	  sprintf( cbuf, "requesting %d new fortints", MAX_KELEM );
	  cout << cbuf << endl;
	  Out_KTDLST = new fortint[ MAX_KELEM ];

	  sprintf( cbuf, "requesting %d new fortints", MAX_KELEM );
	  cout << cbuf << endl;
	  Out_KTDEXP = new fortint[ MAX_KELEM ];

	  sprintf( cbuf, "requesting %d new fortfloats", MAX_KVALS );
	  cout << cbuf << endl;
	  Out_VALUES = new fortfloat[ MAX_KVALS ];

	  sprintf( cbuf, "requesting %d new chars", 80*MAX_KVALS );
	  cout << cbuf << endl;
	  Out_CVALS = new char[ MAX_KVALS * 80 ];
	}
      catch(...)
	{
	  delete [] Out_VALUES; //-- (I)
	  delete [] Out_CVALS;  //-- (II)
	  delete [] Out_KTDEXP; //-- (III)

	  Out_VALUES = 0;
	  Out_CVALS  = 0;
	  Out_KTDEXP = 0;
	  Out_KTDLST = 0;

	  _outState = kBufrOut_error;

	  cout << ">>>\n>>> MvBufrOut::createBuffers failed in " << cbuf << "\n>>>" << endl;
	  cout << "MvBufrOut::createBuffers: throw MvException..." << endl;
#ifdef METVIEW
	  marslog( LOG_EROR, "MvBufrOut::createBuffers failed in %s", cbuf );
	  throw MvException( "MvBufrOut::createBuffers: out-of-memory!" );
#else
          throw std::bad_alloc();
#endif
	}
    }

  cout << "MvBufrOut::createBuffers memory ok" << endl;

  _outState = kBufrOut_emptyBuffers;
}
//____________________________________________________________________ resetBuffers
void
MvBufrOut :: resetBuffers( void )
{
  Out_KTDEXL = -1;
  Out_KELEM = -1;
  _KDLEN = 0;
  _nextValue = 0;
  _nextCharParamPos = 0;

  if( _outState > kBufrOut_emptyBuffers )
    {
      _outState = kBufrOut_emptyBuffers;
    }
}
//____________________________________________________________________ write
void
MvBufrOut :: write( MvObs& anObs )
{
    //-- if no packing into subsets, copy message as is...
    _outSet->write( anObs._bufrIn->fMessage, (int)anObs._bufrIn->fMessageLength );
}

//____________________________________________________________________ add
void
MvBufrOut :: add( MvObs& anObs )
{
  if( _maxNrSubsets == 1 && anObs._bufrIn->subsetCount() == 1 )
    //-- if no packing into subsets, copy message as is...
    write( anObs );
  else
    addIntoBuffers( anObs );
}

//____________________________________________________________________ addIntoBuffers
void
MvBufrOut::addIntoBuffers( MvObs& anObs )
{
  if( _outState <= kBufrOut_noBuffers )
    {
      createBuffers();
    }

  checkDescriptors( anObs );

  if( MAX_KVALS < _nextValue + Out_KTDEXL )
  {
    encode();
    formatBuffers( anObs );
  }

  //-- in case (non-compressed) multisubset msg, do expanded descriptors here
  Out_KTDEXL = In_KTDEXL;             // expected nr of data values / expanded descriptors
  for(int  i=0; i<In_KTDEXL; i++ )    // expanded descriptors
    Out_KTDEXP[ i ] = In_KTDEXP[ i ]; // no offset

  Out_KELEM = In_KTDEXL;  // In_KELEM;// expected nr of expanded elements (????)

  int elemIndex = _nextValue;
  for( int i = 0; i < Out_KTDEXL; i++, elemIndex++ )
  {
    Out_VALUES[ elemIndex ] = In_VALUES[ anObs.subsetOffset() + i ];

    //-- character data..?
    if( fKSUP[ 7-1 ] > 0  &&                                 //-- msg contains char data
        ( strncmp( In_CUNITS + i*24, "CCITTIA5", 8 ) == 0 || //-- ECMWF notation
          strncmp( In_CUNITS + i*24, "CCITT IA5", 9 ) == 0   //-- WMO & NCEP PrepBUFR notation
        )
       )
    {
      //-- copy character string value to next available slot --
      int charInd = (int)Out_VALUES[ elemIndex ] / 1000 - 1;
      int charLen = (int)Out_VALUES[ elemIndex ] % 1000;
      strncpy( (Out_CVALS + _nextCharParamPos*80)
	     , (In_CVALS + charInd*80)
	     , charLen );

      //-- set "pointer" to this slot --
      Out_VALUES[ elemIndex ] = 1000*( _nextCharParamPos + 1 ) + charLen;
      _nextCharParamPos++;
    }
    else  //-- if delayed replication factor...
    {
      if( isDelayedDescriptor( Out_KTDEXP[ i ] ) )
      {
//cerr << "isDelayedDescriptor( " << Out_KTDEXP[ i ] << "), _KDLEN=" << _KDLEN << endl;
	if( _KDLEN < MAX_KDLEN )
	{
	  fortint delayedRepeat = (fortint)Out_VALUES[ elemIndex ];
	  if( delayedRepeat < 0 )
	  {
	    cerr << ">>> MvBufrOut::add: data error - negative delayed repetition " << delayedRepeat
	         << " (from element " << elemIndex << ")" << endl;
#ifdef METVIEW
	    marslog( LOG_EROR, "MvBufrOut::add: data error - negative delayed repetition!" );
#endif
	  }
	  _KDATA[ _KDLEN ] = delayedRepeat;
	  _KDLEN++;
	}
	else
	  {
	    cerr << ">>> MvBufrOut::add: array _KDATA overflow! _KDLEN=" << _KDLEN
	         << ", MAX_KDLEN=" << MAX_KDLEN << endl;
#ifdef METVIEW
	    marslog( LOG_EROR, "MvBufrOut::add: array _KDATA overflow!" );
#endif
	  }
      }
    }
  }
  _nextValue += (int)Out_KELEM;

  fKSEC3[ 2 ] += 1;  //-- Nr of Subsets
  _outState = kBufrOut_dataInBuffers;

  if( shouldBeWritten() )
    encode();
}
//____________________________________________________________________ formatBuffers
void
MvBufrOut :: formatBuffers( const MvObs& anObs )
{
  delete _currentSec1;
#if 0
  _currentSec1 = new TSection1;
  *_currentSec1 = *(anObs._bufrIn->Sec1);
#endif
  if( anObs._bufrIn->fSec0->editionNr > 3 )
    _currentSec1 = new Section1_Ed4( anObs._bufrIn->Sec1 );
  else
    _currentSec1 = new Section1_preEd4( anObs._bufrIn->Sec1 );

  int i;

  fKSEC0[ 0 ] = 0;
  for( i=1; i<3; i++)
    fKSEC0[ i ] = anObs._bufrIn->fKSEC0[ i ];

  // fKSEC1[ 0 ] = 0; // ??????????
  for( i=0; i<40; i++)
    fKSEC1[ i ] = anObs._bufrIn->fKSEC1[ i ];

#if 0
  int wmoi = BUFR_ORIGINATING_CENTER;
  if( getenv( "WMO_SITE_NR" ) )
    wmoi = atoi( getenv( "WMO_SITE_NR" ) );  //-- change WMO Centre Number if given

  if( wmoi < 0 || wmoi > 255 )
    wmoi = BUFR_ORIGINATING_CENTER;          //-- fall back to hard coded default

  fKSEC1[ 2 ] = wmoi;                        //-- WMO Originating Centre
#endif

  fKSEC1[ 3 ] += 1;                          // increment Update Sequence Number!!
  if( fKSEC1[ 4 ] > 127 )
    fKSEC1[ 4 ] -= 128;                      // remove section 2 (which one to copy?)

  fKSEC3[ 0 ] = 0;
  for( i=1; i<4; i++)
    fKSEC3[ i ] = anObs._bufrIn->fKSEC3[ i ];
  fKSEC3[ 2 ] = 0;                           // reset nr of Subsets

  fKSEC4[ 0 ] = 0;
  for( i=1; i<2; i++)
    fKSEC4[ i ] = anObs._bufrIn->fKSEC4[ i ];

  Out_KTDLEN = In_KTDLEN;             // nr of original data descriptors in Sec 3
  for( i=0; i<In_KTDLEN; i++ )        // packed descriptors
    Out_KTDLST[ i ] = In_KTDLST[ i ];

  //-- do this also later because with non-compressed multisubset msgs these vary
  Out_KTDEXL = In_KTDEXL;             // expected nr of data values / expanded descriptors
  for( i=0; i<In_KTDEXL; i++ )        // expanded descriptors
    Out_KTDEXP[ i ] = In_KTDEXP[ i ]; // no offset

  Out_KELEM = In_KTDEXL;  // In_KELEM;// expected nr of expanded elements (????)

  _outState = kBufrOut_formatedBuffers;
}
//____________________________________________________________________ encode
void
MvBufrOut :: encode( void )
{
  if( _outState == kBufrOut_dataInBuffers )
  {
    fortint myKERR = 0;
    fortint myKBUFL = 0;

    BUFREN( fKSEC0, fKSEC1, fKSEC2, fKSEC3, fKSEC4
	  , &Out_KTDLEN   // &fKTDEXL
	  , Out_KTDLST    // fKTDEXP   // &fKTDLEN, fKTDLST
	  , &_KDLEN
	  , _KDATA
	  , &Out_KELEM
	  , &Out_KTDEXL   // was: fKVALS
	  , Out_VALUES
	  , Out_CVALS
	  , &myKBUFL
	  , (fortint *)fMessage
	  , &myKERR );

    if( myKERR == 0 )
      _outSet->write( fMessage, (int)(myKBUFL*sizeof(fortint)) );
    else
      {
	cerr << ">>> MvBufrOut::encode, KERR=" << myKERR << endl;
#ifdef METVIEW
	marslog( LOG_EROR, "BUFR encoding (BUFREN) failed, status = %d", myKERR );
#endif
      }

    resetBuffers();
  }
}
//_______________________________________________________________ checkDescriptors
// checks descriptors and also header consistency i.e.
// to make sure that the new obs fits into the current
// multisubset message
//---------------------------------------------------------------
void
MvBufrOut :: checkDescriptors( const MvObs& anObs )
{
//--  if( anObs._bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
  //-- expand always, in case non-compressed multisubset msg where exaoanded descriptors vary
  anObs._bufrIn->ExpandDescriptors( anObs.subsetNumber() );

  if( _outState == kBufrOut_emptyBuffers )
    formatBuffers( anObs );
  else
    if( differentDescriptors() || differentHeader( anObs ) )
    {
       encode();
       formatBuffers( anObs );
    }
}

//_______________________________________________________________ differentDescriptors
// returns 1 if descriptors of the BUFR message in 'anObs' differs
// from current descriptors.
// returns 0 if they are equal
//---------------------------------------------------------------
int
MvBufrOut :: differentDescriptors( void ) const
{
  if( _outState < kBufrOut_formatedBuffers )
    return 1;

  if( In_KTDLEN != Out_KTDLEN )
    return 1;

  for( int i = 0; i < In_KTDLEN; i++ )
    if( In_KTDLST[ i ] != Out_KTDLST[ i ] )
      return 1;

  return 0;
}
//_______________________________________________________________ differentHeader
// returns 1 if section 1 header of the BUFR message in 'anObs'
// differs from current header 1.
// returns 0 if they are equal
//---------------------------------------------------------------
int
MvBufrOut :: differentHeader( const MvObs& anObs ) const
{
#if 0
  int seclen = 17;  // = unsignedInt( &(_currentSec1->len), 3 );  //syntax error???

  unsigned char* v1 = &(_currentSec1->len);	    //-- start of current section 1
  unsigned char* v2 = &(anObs._bufrIn->fSec1->len); //-- start of sec 1 in other header

  for( int i=seclen; i>0; --i )
  {
     if( *v1 != *v2 )
        return 1;

     ++v1;  ++v2;
  }

  return 0;
#endif

  return _currentSec1->isDifferent( anObs._bufrIn->Sec1 );
}
//______________________________________________________________ shouldBeWritten
int
MvBufrOut :: shouldBeWritten( void )
{
  if( _outState != kBufrOut_dataInBuffers )
    return 0;

  return fKSEC3[ 2 ] >= _maxNrSubsets;  // Nr of Subsets
}
//______________________________________________________________ setSubsetCount
void
MvBufrOut :: setSubsetCount( int maxNrSubsets )
{
  if( _outState == kBufrOut_dataInBuffers )
    encode();
  _maxNrSubsets = maxNrSubsets;
}
//______________________________________________________________ isDelayedDescriptor
bool
MvBufrOut :: isDelayedDescriptor( const long aDescr ) const
{
//  if( aDescr == 31001 || aDescr == 31002 || aDescr == 31011 || aDescr == 31012 )
  if( aDescr >= 31000 && aDescr <= 31012 )
    return true;
  else
    return false;
}
//______________________________________________________________________
//====================================================================== MvObs
//______________________________________________________________________

MvObs :: MvObs( MvBufr *b, int i )
{
   _subsetNr = i;
   _copy( b );
}

MvObs :: MvObs( const MvObs&  b )
{
   _copy( b );
}
//___________________________________________________________________
MvObs :: ~MvObs()
{
   _clean();
}
//___________________________________________________________________ _copy
void
MvObs :: _copy( MvBufr *b )
{
   _bufrIn = b;
   _bufr_id = 0;
   if( _bufrIn )
   {
     _bufrIn->attach();
     _bufr_id = _bufrIn->currentBufrRef();
   }
   _currentLevelCoordinate = cPressureCoordinate;
   _currentLevelIndex = -1;
   _confidence = new MvBufrConfidence( _bufrIn, _subsetNr );
}
//___________________________________________________________________ _copy
void
MvObs :: _copy( const MvObs& b )
{
  _subsetNr = b._subsetNr;
  _copy( b._bufrIn );

}
//___________________________________________________________________ _clean
void
MvObs :: _clean()
{
   if( _bufrIn )
     _bufrIn->detach();
   _bufrIn = NULL;
   _bufr_id = 0;
   delete _confidence;
   _confidence = 0;
}
//___________________________________________________________________ operator=

MvObs&
MvObs :: operator= ( const MvObs& b )
{
   _clean();
   _copy( b );
   return *this;
}
//___________________________________________________________________ operator void*

MvObs :: operator void* ()
{
   return _bufrIn;
}
//___________________________________________________________________ operator!
bool
MvObs :: operator! ()
{
   return !_bufrIn;
}
//___________________________________________________________________ msg_ok
//
bool
MvObs :: msg_ok() const
{
  if( _bufr_id != _bufrIn->currentBufrRef() )
  {
    //-- restriction due to static bufr arrays --
    cerr << ">>> [MvObs::msg_ok] Static bufr arrays rewritten, not valid any more!" << endl;
    return false;
  }
  return ( _bufrIn && ( _bufrIn->_inState != kBufrIn_Error ) ) ? true : false;
}
//___________________________________________________________________ Advance
bool
MvObs :: Advance()
{
   if( _bufrIn->subsetCount() > 1000 ||
       _bufrIn->subsetCount() < 1 ) // DEBUG TEST ONLY
   {
      cerr << " >>> MvObs::Advance, unbelievable nr of fsubsets ("
	   << _bufrIn->subsetCount() << ") in msg "
       << _bufrIn->fMessageNumber << endl;
   }
   _subsetNr++;
   _bufrIn->setSubset( _subsetNr );
   return  _subsetNr <= _bufrIn->subsetCount();
}

//_________________________________________________________________ messageType
int
MvObs :: messageType()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->msgType();
}
//_________________________________________________ messageSubtypeInternational
int
MvObs :: messageSubtypeInternational()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->msgSubtypeWMO();
}
//_________________________________________________________ messageSubtypeLocal
int
MvObs :: messageSubtypeLocal()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->msgSubtypeLocal();
}
//______________________________________________________________ messageSubtype
int
MvObs :: messageSubtype()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->msgSubtype();
}
//___________________________________________________________ originatingCentre
int
MvObs::originatingCentre()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->origCentre();
}
int
MvObs::originatingSubCentre()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->origSubCentre();
}
//___________________________________________________________ editionNumber
int
MvObs::editionNumber()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->fSec0->editionNr;
}
//___________________________________________________________ masterTable
int
MvObs::masterTable()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->masterTable();
}
//___________________________________________________________ masterTableVersion
int
MvObs::masterTableVersion()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->masterTableVersion();
}
//___________________________________________________________ localTableVersion
int
MvObs::localTableVersion()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->Sec1->localTableVersion();
}
//_________________________________________________  messageTotalLen()
int
MvObs :: messageTotalLen()
{
   if( ! msg_ok() )
      return -1;

   return _bufrIn->totalLen();
}
//____________________________________________________________________ operator[]
double
MvObs::operator[] ( int index )  //-- index starts from 1: 1,2,...,n
{
  return (double)(_bufrIn->DataValue( index-1, _subsetNr ));
}
//____________________________________________________________________ value
// returns 'kBufrMissingValue' if not found!
//-------------------------------------------------
float
MvObs :: value( long aDescriptor )
{
   if( ! msg_ok() )
      return kBufrMissingValue;

   fortfloat myValue;
   _bufrIn->Value( aDescriptor, _subsetNr, myValue );
   return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
}
//___________________________________________________________________ nextValue
// returns 'kBufrMissingValue' if not found!
//-------------------------------------------------
float
MvObs :: nextValue()
{
   if( ! msg_ok() )
      return kBufrMissingValue;

   fortfloat myValue;
   _bufrIn->Value( _bufrIn->_currentDescr, _subsetNr, myValue, _bufrIn->_currentDescrInd + 1 );
   return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
}
//___________________________________________________________ valueByOccurrence
// returns 'kBufrMissingValue' if not found!
//-------------------------------------------------
float
MvObs :: valueByOccurrence( int anOccurrenceIndex, long aDescriptor )
{
  fortfloat myValue = value( aDescriptor );
  for( int myInd = 1; myInd < anOccurrenceIndex; myInd++ )
    myValue = nextValue();
  return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
}
//______________________________________________________________ hasConfidences
bool
MvObs :: hasConfidences()
{
  return  _confidence->hasConfidences();
}
//__________________________________________________________________ confidence
int
MvObs :: confidence()
{
  return _bufrIn->_currentDescrInd > -1 ?
         _confidence->confidenceByIndex( _bufrIn->_currentDescrInd ) : -1;
}
//____________________________________________________________________ intValue
// returns 'kFortranBufrMissingIntValue' if not found!
//----------------------------------------------------
long
MvObs :: intValue( long aDescriptor )
{
  return msg_ok() ? _bufrIn->intValue( aDescriptor, _subsetNr ) : kFortranBufrMissingIntValue;
}
//____________________________________________________________________ elementValueType
EElementValueType
MvObs :: elementValueType()
{
  return _bufrIn->elementValueType( _subsetNr );
}
//____________________________________________________________________ elementValueType
EElementValueType
MvObs :: elementValueType( long aDescriptor )
{
  return _bufrIn->elementValueType( aDescriptor, _subsetNr );
}
//____________________________________________________________________ stringValue
string
MvObs :: stringValue()
{
  if( ! msg_ok() )
    return MESSED_UP;
  else
    return _bufrIn->stringValue( _subsetNr );
}
//_________________________________________________________________ stringValue
string
MvObs :: stringValue( long aDescriptor )
{
  if( ! msg_ok() )
    return MESSED_UP;
  else
    return _bufrIn->stringValue( aDescriptor, _subsetNr );
}
//______________________________________________________ numberOfPressureLevels
int
MvObs :: numberOfPressureLevels()
{
  return numberOfLevels( cPressureCoordinate );
}
//______________________________________________________ numberOfLevels
int
MvObs :: numberOfLevels( long levelDescriptor )
{
  int myCount = 0;

  if( firstLevel( levelDescriptor ) != kBufrMissingValue )
  {
    myCount++;
    while( nextLevel() != kBufrMissingValue )
      myCount++;
  }

  _currentLevelIndex = -1;
  return myCount;
}
//__________________________________________________________ firstPressureLevel
float
MvObs :: firstPressureLevel()
{
  _currentLevelCoordinate = cPressureCoordinate;
  return pressureLevel( 0 );
}
//__________________________________________________________ firstLevel
float
MvObs :: firstLevel( long levelDescriptor )
{
  _currentLevelCoordinate = levelDescriptor;
  return level( levelDescriptor, 0 );
}
//___________________________________________________________ nextPressureLevel
float
MvObs :: nextPressureLevel()
{
  return pressureLevel( _currentLevelIndex + 1 );
}
//___________________________________________________________ nextLevel
float
MvObs :: nextLevel()
{
  return level( _currentLevelCoordinate, _currentLevelIndex + 1 );
}
//______________________________________________________________ pressureLevel
float
MvObs :: pressureLevel( int firstIndexValue )
{
  float  myLevelValue = level( cPressureCoordinate, firstIndexValue );

  return myLevelValue == kBufrMissingValue ? kBufrMissingValue : myLevelValue / 100.;
}
//______________________________________________________________ level
float
MvObs :: level( long levelDescriptor, int firstIndexValue )
{
  if( ! msg_ok() )
     return kBufrMissingValue;

  fortfloat myLevelValue = kFortranBufrMissingValue;

  if( _bufrIn->Value( levelDescriptor, _subsetNr, myLevelValue, firstIndexValue ) )
    _currentLevelIndex = _bufrIn->_currentDescrInd;
  else
    _currentLevelIndex = -1;

  return myLevelValue == kFortranBufrMissingValue ? kBufrMissingValue : myLevelValue;
}

//______________________________________________________________ specifierIndex
int
MvObs :: specifierIndex( long   aSpecifierDescriptor
		       , double aSpecifierValue
		       , int    firstIndexValue )
{
   if( ! msg_ok() )
      return -1;

   if( _bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
      _bufrIn->ExpandDescriptors( _subsetNr );

   if( _bufrIn->_inState == kBufrIn_DataAndDescriptorsDecoded )
   {
      //-- search for a specifier data with specified value --
      for( int index=firstIndexValue; index < In_KTDEXL; index++ )
	 if( In_KTDEXP[ index ] == aSpecifierDescriptor
	     && _bufrIn->DataValue( index, _subsetNr ) == aSpecifierValue )
	 {
	    _lastSpecifierIndex = index;
	    return index;
	 }
   }
   return -1;
}
//____________________________________________________________ valueBySpecifier
// A generic function to retrieve repeating data
// specified by some other data
// (e.g.  temperature at a certain pressure level)
//--------------------------------------------------------------------
double
MvObs :: valueBySpecifier( long   aSpecifierDescriptor
			 , double aSpecifierValue
			 , long   aDescriptor
			 , int    firstIndexValue )
{
   int index = specifierIndex( aSpecifierDescriptor, aSpecifierValue, firstIndexValue );
   if( index > 0 )
   {
     //-- if the coordinate value itself is requested
     if( aSpecifierDescriptor == aDescriptor )
     {
         fortfloat myVal = _bufrIn->DataValue( index, _subsetNr );
         return myVal == kFortranBufrMissingValue ? kBufrMissingValue : myVal;
     }
     //-- search real data before next specifier data --
     for( int ind=index+1; ind < In_KTDEXL; ind++ )
     {
       if( In_KTDEXP[ ind ] == aDescriptor )
       {
         fortfloat myVal = _bufrIn->DataValue( ind, _subsetNr );
         return myVal == kFortranBufrMissingValue ? kBufrMissingValue : myVal;
       }

       //-- not found if specifier data again --
       if( In_KTDEXP[ ind ] == aSpecifierDescriptor )
         break;
     }
   }
   return kBufrMissingValue;   //-- Not Found or Troubled Msg --
}
//________________________________________________________ valueByPressureLevel
float
MvObs :: valueByPressureLevel( float aLevelValue        // in 'hPa'
			     , long  aDescriptor )
{
   return valueBySpecifier( cPressureCoordinate, aLevelValue*100., aDescriptor );
}
//________________________________________________________ valueByLevel
float
MvObs :: valueByLevel( long  aLevelDescriptor
                     , float aLevelValue
		     , long  aDescriptor )
{
   return valueBySpecifier( aLevelDescriptor, aLevelValue, aDescriptor );
}
//________________________________________________________ valueByLevel
float
MvObs :: valueByLevelRange( long  aLevelDescriptor
                     , float level1
	             , float level2    
		     , long  aDescriptor )
{
    
  float levelVal=firstLevel(aLevelDescriptor);
  
  while(levelVal != kBufrMissingValue)
  {  
	if(levelVal >= level1 && levelVal <= level2)
	{
		for( int ind = _currentLevelIndex + 1; ind < In_KTDEXL; ind++ )
  		{
    			if( In_KTDEXP[ ind ] == aDescriptor )
   			{
      				fortfloat myValue = _bufrIn->DataValue( ind, _subsetNr );
				if(myValue != kFortranBufrMissingValue)
					return myValue;
				
      				//return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
    			}
    			
    			else if(In_KTDEXP[ ind ] ==aLevelDescriptor)
				break; 	
		}
	}
	
	levelVal=nextLevel();
  }	
	
  return kBufrMissingValue;
}  
 
//________________________________________________________________ valueByLayer
float
MvObs :: valueByLayer( float firstLevel
		     , float secondLevel
		     , long aDescriptor )
{
  int firstLevelIndex = specifierIndex( cPressureCoordinate, firstLevel*100., 0 );
  int secondLevelIndex = -1;

  while( firstLevelIndex > 0 )          //-- loop candidates --
  {
    //-- try to find the second level next to the current first one --

    if( In_KTDEXP[ firstLevelIndex - 1 ] == cPressureCoordinate &&
        _bufrIn->DataValue( firstLevelIndex - 1, _subsetNr ) == secondLevel*100. )
    {
      secondLevelIndex = firstLevelIndex - 1;
      break;
    }
    else
      if( In_KTDEXP[ firstLevelIndex + 1 ] == cPressureCoordinate &&
          _bufrIn->DataValue( firstLevelIndex + 1, _subsetNr ) == secondLevel*100. )
      {
        secondLevelIndex = firstLevelIndex + 1;
        break;
      }

    //-- no match, let's find the next candidate --

    firstLevelIndex = specifierIndex( cPressureCoordinate
				    , firstLevel*100.
				    , firstLevelIndex + 1 );
  }

  if( firstLevelIndex < 0 )
    return kBufrMissingValue;  //-- levels were not found!!!

  //-- search real data before next specifier data --

  int maxim = firstLevelIndex > secondLevelIndex ? firstLevelIndex : secondLevelIndex;
  for( int ind = maxim + 1; ind < In_KTDEXL; ind++ )
  {
    if( In_KTDEXP[ ind ] == aDescriptor )
    {
      fortfloat myValue = _bufrIn->DataValue( ind, _subsetNr );
      return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
    }


    //-- not found if specifier data again --

    if( In_KTDEXP[ ind ] == cPressureCoordinate )
      break;
  }
  return kBufrMissingValue;   //-- Not Found or Troubled Msg --
}


//______________________________________________________________ printAllValues
bool
MvObs :: printAllValues()
{
   ostream* myStream = &cout;
   return writeAllValues( *myStream );
}
//_______________________________________________________________ writeAllValues
bool
MvObs :: writeAllValues( ostream& aStream )
{
  if( ! msg_ok() )
    return false;

  if( _bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
    _bufrIn->ExpandDescriptors( _subsetNr );

  if( _bufrIn->_inState == kBufrIn_DataAndDescriptorsDecoded )
    {
      long  myEndingIndex = _confidence->hasConfidences() ?
	_confidence->lastDataIndex()+1 : In_KTDEXL;
      writeValues(aStream,0, myEndingIndex);
      return true;
   }
   return false;
}

bool
MvObs::writeValues(ostream& aStream, int firstIndex, int lastIndex)
{
   if( ! msg_ok() )
      return false;

   if( _bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
      _bufrIn->ExpandDescriptors( _subsetNr );

   if( _bufrIn->_inState == kBufrIn_DataAndDescriptorsDecoded )
   {
      fortfloat myValue;
      if ( firstIndex < 0 )
	firstIndex = 0;
      if (lastIndex > In_KTDEXL )
	lastIndex = In_KTDEXL;

      for( int i = firstIndex; i < lastIndex; i++ )
      {
	 aStream.width( 3 ); aStream.fill( ' ' );      // index
	 aStream << i+1 << ".  ";

	 myValue = _bufrIn->DataValue( i, _subsetNr ); // get parameter value
	                                               // = set current parameter!
	 switch( elementValueType() )                    // print parameter value
	 {
	   case kEVT_unknown:
	      aStream << " ?????";
	      break;

	   case kEVT_missing:
	      aStream << "   ~~~";
	      break;

	   case kEVT_numeric:
	      aStream.width( 6 ); aStream.fill( ' ' );
	      aStream << myValue;
	      break;

	   case kEVT_string:
	      aStream.width( 6 ); aStream.fill( ' ' );
	      aStream << stringValue( In_KTDEXP[ i ] );
	      break;
	 }

	 aStream << " " << name( In_KTDEXP[ i ] );           // parameter name
	 aStream << " [" << unit( In_KTDEXP[ i ] ) << "] ("; // parameter unit
	 aStream.width( 5 ); aStream.fill( '0' );            // parameter descriptor
	 aStream << In_KTDEXP[ i ] << ")";
	 aStream << endl;
      }
      return true;
   }
   return false;
}

//_______________________________________________________________ writeAllValues
bool
MvObs :: writeAllValues( const char* aPathName )
{
  ofstream myStream( aPathName, ios::out );
  if( ! myStream )
  {
    cerr << " >>> MvObs::writeAllValues(char*): error in creating file " << aPathName << endl;
#ifdef METVIEW
    marslog( LOG_EROR, "MvObs::writeAllValues: error in creating file %s", aPathName );
#endif
    return false;
  }

  return writeAllValues( myStream );
}
//_______________________________________________________________ writeBufrBox
bool
MvObs :: writeBufrBox( ostream& aStream )
{
   bool return_value = true;

   bool b_ret = _bufrIn->writeBufrBox( _subsetNr );
   if( ! b_ret )
   {
      aStream << "\n   >>> Problems encountered!!! <<<\n" << endl;
      return_value = false;
   }
   else
     {
       return_value = file_to_stream(BBOXNAME.c_str(),aStream,0);
       delete_print_file(BBOXNAME.c_str());
     }
   return return_value;
}
//______________________________________________________________ WmoIdentNumber
long
MvObs :: WmoIdentNumber()
{
  return msg_ok() ? WmoBlockNumber()*1000 + WmoStationNumber() : kBufrMissingIntValue;
}
//______________________________________________________________ WmoBlockNumber
int
MvObs :: WmoBlockNumber()
{
   fortfloat myValue;

   if( msg_ok() && _bufrIn->Value( 1001L, _subsetNr, myValue ) )
      return (long)myValue;
   else
      return 0;
}
//____________________________________________________________ WmoStationNumber
int
MvObs :: WmoStationNumber()
{
   fortfloat myValue;

   if( msg_ok() && _bufrIn->Value( 1002L, _subsetNr, myValue ) )
      return (long)myValue;
   else
      return 0;
}
//____________________________________________________________ findSomeIdent
string
MvObs::findSomeIdent()
{
  if( ! msg_ok() )
    return string( "???" );

                            //-- 5-digit WMO identifier available?
  if( WmoIdentNumber() > 0 )
    {
      ostringstream oss;
      oss << setw(5) << setfill('0') << WmoIdentNumber();
      return oss.str();
    }
                            //-- No WMO id found, thus look for other candidates,
                            //-- this is a list of known identifier candidates.
  const long idList[] =
    {
      1011L,    //-- Ship or mobile land station identifier
      1005L,    //-- Buoy/platform identifier
      1006L,    //-- Aircraft flight number
      1007L,    //-- Satellite identifier
      1008L,    //-- Aircraft registration number
      1010L,    //-- Stationary buoy platform identifier
      1025L,    //-- Storm identifier
      1026L,    //-- WMO storm name
      1027L     //-- WMO long storm name
    };

  int idVals = sizeof(idList)/sizeof(idList[0]);

  for( int i=0; i<idVals; ++i )
    {
      long descr = idList[i];
      fortfloat myValue = value( descr );

      if( myValue != kBufrMissingValue )
	{
	  return stringValue( descr );
	}
    }

  return string( "id???" );
}
//____________________________________________________________________ location
MvLocation
MvObs :: location()
{
   MvLocation myLocation( value( 5001L ), value( 6001L ) ); //-- "high accuracy"

   if( myLocation.latitude() == kBufrMissingValue ||
       myLocation.longitude() == kBufrMissingValue )
   {
      myLocation.set( value( 5002L ), value( 6002L ) );     //-- "coarse accuracy"
   }                                                        //-- hopefully not missing

   //-- this one is for those quirky NCEP PrepBUFR msgs
   if( myLocation.latitude() != kBufrMissingValue &&
       myLocation.longitude() == kBufrMissingValue )
   {
      //-- lat OK, try NCEP PrepBUFR local descriptor 0'06'240 for lon
      myLocation.set( myLocation.latitude(), value( 6240L ) );
   }

   return myLocation;
}
//____________________________________________________________________ unit
string
MvObs :: unit( long aDescriptor )
{
  return msg_ok() ? _bufrIn->unit( aDescriptor ) : MESSED_UP;
}
//____________________________________________________________________ unit
string
MvObs :: unit()
{
  return msg_ok() ? _bufrIn->unit() : MESSED_UP;
}
//____________________________________________________________________ name
string
MvObs :: name( long aDescriptor )
{
  return msg_ok() ? _bufrIn->name( aDescriptor ) : MESSED_UP;
}
//____________________________________________________________________ name
string
MvObs :: name()
{
  return msg_ok() ? _bufrIn->name() : MESSED_UP;
}
//________________________________________________________________ subsetOffset
int
MvObs :: subsetOffset() const
{
  return ( _subsetNr - 1 ) * In_KELEM;
}
//________________________________________________________________ writeConfidenceValues
bool
MvObs::writeConfidenceValues(ostream& aStream)
{
  if( ! msg_ok() || ! _confidence->hasConfidences() )
    return false;

  if( _bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
    _bufrIn->ExpandDescriptors( _subsetNr );

  if( _bufrIn->_inState == kBufrIn_DataAndDescriptorsDecoded )
    {
      long  myStartingIndex = _confidence->lastDataIndex()+1;
      long  myEndingIndex =  In_KTDEXL;
      writeValues(aStream,myStartingIndex, myEndingIndex);
      return true;
   }
   return false;
}

#ifdef METVIEW
//---
//-- Extracts a 4 or 8 bit OPERA radar image
//-- from a BUFR message into 'unsigned char' array
//--
//-- ( NOTE: U N F I N I S H E D ! ! ! )
//____________________________________________________________________ OperaRadarImage
unsigned char*
MvObs::OperaRadarImage(  )
{
   const float cRadarMissingVal = 255;       //-- BUFR radar pixel with no data
   const float cMyMissingValue = 0;          //-- output radar pixel with no data

   //---
   //-- Some checks to ensure that we can handle this data
   //-

   int msgType = messageType();
   if( msgType != 6 )                        //-- 6 = Radar data
   {
      throw MvException( "MvObs::OperaRadarImage: not radar data!" );
   }

   float val = value( 30021 );               //-- 030021: Number of pixels per row
   if( val == kBufrMissingValue )
   {
      throw MvException( "MvObs::OperaRadarImage: unable to get number-of-rows!" );
   }
   int n_rows = val;

   val = value( 30022 );                     //-- 030022: Number of pixels per column
   if( val == kBufrMissingValue )
   {
      throw MvException( "MvObs::OperaRadarImage: unable to get number-of-columns!" );
   }
   int n_cols = val;

   //---
   //-- So far so good, now let's try to locate the first data row
   //-

   int firstRowStartIndex = _bufrIn->descriptorToFortranIndex( 5031 );
   if( firstRowStartIndex < 0 )
   {
      throw MvException( "MvObs::OperaRadarImage: unable to get Row Number element!" );
   }
                                                     //-- "FortranIndex": 0,1,2,...
   int rowRepeatCount = (*this)[firstRowStartIndex]; //-- operator[i], i=1,2,3,...
   long currentDescr = currentDescriptor();
   if( currentDescr != 31002 )
   {
      throw MvException( "MvObs::OperaRadarImage: internal error - not 031002" );
   }

   //---
   //-- Still OK, now create the pixel array and start decoding the BUFR msg
   //-

   unsigned char* radimg = new unsigned char[n_rows*n_cols];
   cout << "radimg size: " << n_rows << "*" << n_cols << " = " << n_rows*n_cols << endl;

   int startPos = firstRowStartIndex - 1;    //-- start of radimg pixels
   int pixelPos = 0;

   float imax = 0;
   for( int msgPos = startPos; msgPos < In_KTDEXL; ++msgPos )
   {
      float currVal = (*this)[msgPos];       //-- get the value
      long  currDescr = currentDescriptor(); //-- and it's descriptor

      if( currDescr == 31012 || currDescr == 31011 ) //-- delayed descr & data repetition factor?
      {
         int repeatCount = currVal;
         float repeatVal = (*this)[++msgPos]; //-- get next value (advances to next descriptor)

         //-- Double check that the descriptor that follows is a pixel data descriptor
         //-- NOTE: Descriptor 30004 (16 bit pixel value) is ignored by this code!
         //--       We only check for 4 bit (30001) and 8 bit (30002) pixel descriptors
         long nextCurrDescr = currentDescriptor();
         if( nextCurrDescr != 30001 && nextCurrDescr != 30002 )
         {
            delete [] radimg;                //-- free memory before throwing
            if( nextCurrDescr == 30004 )     //-- cannot process 16 bit pixels
            {
               throw MvException( "MvObs::OperaRadarImage: cannot process 16 bit radar images" );
            }

            cerr << "MvObs::OperaRadarImage: expected 30001 or 30002, found "
                 << nextCurrDescr << endl;
            throw MvException( "MvObs::OperaRadarImage: internal or data error => debug needed" );
         }

         //---
         //-- OK, we are processing a repeated pixel value (run-length coding)
         //-
         if( repeatVal == cRadarMissingVal )
            repeatVal = cMyMissingValue;
         else if( repeatVal > imax )
            imax = repeatVal;

         for( int i=0; i < repeatCount; ++i )
         {
            radimg[pixelPos++] = (unsigned char)repeatVal; //-- repeated data value
         }
      }
      else if( currDescr == 30002 || currDescr == 30001 )
      {
         //---
         //-- this is not a repeated value, just a single value
         //-
         if( currVal == cRadarMissingVal )
            currVal = cMyMissingValue;
         else if( currVal > imax )
            imax = currVal;

         radimg[pixelPos++] = (unsigned char)currVal;      //-- individual data value
      }
   }
   cout << "Last pixel pos: " << pixelPos << endl;
   cout << "Max pixel value: " << imax << endl;

   return radimg;
}
//---
//-- Extracts OPERA radar image metadata that is required
//-- for instance for geolocating the image
//--
//-- ( NOTE: U N F I N I S H E D ! ! ! )
//____________________________________________________________________ OperaRadarMetadata
bool
MvObs::OperaRadarMetadata( /* <aki> arguments? */ )
{
   return false;
}
#endif

//=============================================================================
//                                                                 vk April -95
// Quick&Dirty hack to access Feed Back Quality Control Confidence info:
//  * works only for QC operator 222000
//  * uses Data Present Descriptors (031031) and Confidence Descriptors (033007)
//  * presumes following structure:
//     - 222000 descriptor exists (and is followed by:)
//     - 031031 descriptors are all in one group (only first one is used!)
//     - 033007 descriptors are all in one group (only first one is used!)
//     - Extended Delayed Descr Replication Factors (031002) are not checked!
//  * Data Present group is used to define index into Confidence group
//____________________________________________________________ MvBufrConfidence

MvBufrConfidence :: MvBufrConfidence( MvBufr* aBufr, int aSubsetNr )
{
  _bufr = aBufr;
  if( _bufr )
    _bufr->attach();
  _subsetNr = aSubsetNr;
  _state = kBCS_unknown;
  _startOfDataPresent = -2;
  _startOfConfidences = -2;
}
//___________________________________________________________ ~MvBufrConfidence

MvBufrConfidence :: ~MvBufrConfidence()
{
  if( _bufr )
    _bufr->detach();
}
//______________________________________________________________ hasConfidences
bool
MvBufrConfidence :: hasConfidences()
{
  if( ! _bufr )
    return false;

  if( _state == kBCS_unknown )
    _state = _bufr->descriptorToFortranIndex( 222000L ) > 0 ? kBCS_exists : kBCS_missing;

  return _state == kBCS_exists ? true : false;
}
//_______________________________________________________________ confidence
int
MvBufrConfidence :: confidence( long aDescr )
{
  int myDataIndex = _bufr->descriptorToFortranIndex( aDescr );
  return myDataIndex > -1 ? confidenceByIndex( myDataIndex ) : -1;
}
//___________________________________________________________ confidenceByIndex
int
MvBufrConfidence :: confidenceByIndex( int aDataInd )
{
  int myConfidenceInd = -1;

  if( hasConfidences() )
  {
     //--    case a:        case b:
     //--        . . .          . . .
     //--       0xxyyy         0xxyyy    <- 0xxyyy: element descriptors
     //--       0xxyyy         222000
     //--       222000         03100x
     //--       031031         031031    <- startOfDataPresent()
     //--       031031         031031
     //--        . . .          . . .
     //--
     if( aDataInd < ( startOfDataPresent() - 1 ) &&  //-- skip 222000
         _bufr->CurrentDescriptor() != 222000L )     //-- if previous was 03100*
     {
        if( _bufr->DataValue( aDataInd, _subsetNr ) != kFortranBufrMissingValue )
           myConfidenceInd = startOfConfidences() + delta( aDataInd );
     }
  }

  return myConfidenceInd > -1 ?
            (int)(_bufr->PeekDataValue( myConfidenceInd, _subsetNr )) : -1;
}
//_______________________________________________________________ lastDataIndex
int
MvBufrConfidence :: lastDataIndex()
{
  return hasConfidences() ? _bufr->descriptorToFortranIndex( 222000L ) - 1 : -1;
}
//__________________________________________________________ startOfDataPresent
// Q&D hack !!!

int
MvBufrConfidence :: startOfDataPresent()
{
  if( _startOfDataPresent == -2 )
  {
    if( hasConfidences() )
    {
      _startOfDataPresent = _bufr->descriptorToFortranIndex( 31031L );
      if( _startOfDataPresent < 1 )
      {
	cerr << "[MvBufrConfidence::startOfDataPresent] Q&D hack does not work!!" << endl;
	_state = kBCS_missing;
      }
    }
  }

  return hasConfidences() ? _startOfDataPresent : -1;
}
//_______________________________________________________________ startOfConfidences
// Q&D hack !!!

int
MvBufrConfidence :: startOfConfidences()
{
  if( _startOfConfidences == -2 )
  {
    if( hasConfidences() )
    {
      _startOfConfidences = _bufr->descriptorToFortranIndex( 33007L );
      if( _startOfConfidences < 1 )
      {
	cerr << "[MvBufrConfidence::startOfConfidences] Q&D hack does not work!!" << endl;
	_state = kBCS_missing;
      }
    }
  }

  return hasConfidences() ? _startOfConfidences : -1;
}
//_______________________________________________________________ delta
int
MvBufrConfidence :: delta( int anInd )
{
  if( hasConfidences() )
  {
    int myDelta = 0;

    for( int i = 0; i < anInd; i++ )
      if( _bufr->PeekDataValue( startOfDataPresent()+i, _subsetNr ) == 0 )
	myDelta++;

    return myDelta;
  }
  else
    return -1;
}

//________________________________________________________________________
//======================================================================== MvBufrParam
//________________________________________________________________________

MvBufrParam :: MvBufrParam( const char *aParamName )
{
   descriptorStruct *par = knownParams;
   while( par->descriptor != 0 )
   {
      if( strcmp( aParamName, par->name ) == 0 )
      {
         fDescriptor = par->descriptor;
	 return;
      }
      ++par;
   }
   cerr << " >>> MvBufrParam::MvBufrParam: param not defined: " << aParamName << endl;
   fDescriptor = 0;
}
//_____________________________________________________________ PrintAllKnownParameters

void
MvBufrParam :: PrintAllKnownParameters() const
{
   descriptorStruct *par = knownParams;
   cout << " The Known Parameters of class MvBufrParam and the corresponding Descriptors:\n";
   while( par->descriptor != 0 )
   {
      cout << "\n";
      cout.width( 16 );
      cout.fill( ' ' );
      cout << par->name << " = ";
      cout.width( 6 );
      cout.fill( '0' );
      cout << par->descriptor;
      ++par;
   }
   cout << endl;
}

//-------------------- Helper functions to redirect stdout ----------------
// This function will duplicate the file descriptor for stdout, and keep
// it for later. Then stdout is connected to the file given as arg.
// The first time the function is run, it tries to find a temporary dir.
// to use ( class could be used outside metview ).

bool redirect_6(const char *fname)
{
  cout << " redirect_6: redirect stdout into file " << fname << endl;
  char *tmp_dir;
  // Set the output dir. if it's not set.
  if ( redirect_dir == string("") )
    {
      tmp_dir = getenv("METVIEW_TMPDIR");
      if ( !tmp_dir )
	{
	  tmp_dir = getenv("TMPDIR");
	  if ( !tmp_dir )
	    redirect_dir = "/usr/tmp";
	  else
	    redirect_dir = tmp_dir;
	}
      else
	redirect_dir = tmp_dir;
    }

  // Flush stdout before redirecting, to get rid of any pending output.
  fflush(stdout);

  new_fd = dup(STDOUT_FILENO);

  cout << "new_fd: " << new_fd << endl;

  close(STDOUT_FILENO);

  string tmp_name;
  tmp_name = redirect_dir + "/" + fname;
  // As 1 is just closed, it will be the fd used.
  // Will fail if stdin is explicitly closed and no fopens are done
  if ( ! (fopen(tmp_name.c_str(),"w")) )
    {
	    cerr << "Problems opening file " << (const char*)tmp_name.c_str() << " for writing" << endl;
      return false;
    }

  else
    return true;
}

//
// Will reconnect stdout by using a file descriptor saved from earlier.
//
bool reconnect_6()
{
  // Make sure we get everything before closing.
  fflush(NULL);
  close(STDOUT_FILENO);

  int ret = dup2( new_fd, STDOUT_FILENO );
  close(new_fd);
  if( ret < 0 )
    {
      cerr << " reconnect_6: reconnecting into stdout FAILED!" << endl;
      cout << " reconnect_6: reconnecting into stdout FAILED!" << endl;
      return false;
    }
  cout << " reconnect_6: reconnected into stdout!" << endl;
  return ret > 0 ;
}

//
// Read a file and write the contents to given stream
//
bool file_to_stream(const char *fname,ostream &aStream,int skip)
{
  const int MAX_LINE_LEN = 512;
  char      supposedToBeANewLine;
  string    myTmpFileName;

  myTmpFileName = redirect_dir + "/" + fname;

  ifstream myTmpFile( myTmpFileName.c_str() );

  if ( ! myTmpFile )
    {
	    aStream << "Can not read file " << (const char*)myTmpFileName.c_str() << endl;
      return false;
    }

  char     myLine[ MAX_LINE_LEN ];

  myTmpFile.get( myLine, MAX_LINE_LEN, '\n' );
  myTmpFile.get( supposedToBeANewLine );
  int i = 0;
  while( myTmpFile && !myTmpFile.eof() )
    {
      if ( i++ >= skip )
	aStream << myLine << endl;

      myTmpFile.get( myLine, MAX_LINE_LEN, '\n' );
      myTmpFile.get( supposedToBeANewLine );
    }

  myTmpFile.close();

  return true;
}

void delete_print_file(const char *name)
{
  string fname = redirect_dir + "/" + name;
  unlink(fname.c_str());
}

void eraseWhiteSpaceFromStringEnd(string &str)
{
   static string whitespaces (" \t\f\v\n\r");
   size_t found=str.find_last_not_of(whitespaces);
   if (found!=string::npos)
       str.erase(found+1);
   else
       str.clear();
}

string intToString(int i)
{
	stringstream out;
	out << i;
	return out.str();
}

string floatToString(float f)
{
	stringstream out;
	out << f;
	return out.str();
}

void keyToStringMap(map<string,string> &data,string keyName,fortint *keyArray,int fortIndex)
{
	data[keyName]=intToString(keyArray[fortIndex-1]);
}

void keyToStringMap(map<string,string> &data,string keyName,float keyValue)
{
	data[keyName]=floatToString(keyValue);
}
