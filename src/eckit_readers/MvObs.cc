/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvObs.cc,     vk 940818...
//           rev vk 980501
//           rev fi 20170801  replace BUFRDC by eccodes

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

#include <assert.h>

#include "MvBufrEdition.h"
#include "MvBufrElementTable.h"
#include "MvObs.h"
#include "MvObsSet.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
using std::string;
#include <cerrno>
#include <cstring>

#ifdef METVIEW
#include "MvException.h"
#else
#include <exception>
#endif

#ifdef MV_BUFRDC_TEST
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// Static int used to duplicate stdout and get it back.
static int new_fd = -1;

const int MAX_BUBOX_KELEM_LIMIT = MAX_KELEM;

static string MESSED_UP("[messed messages!]");

static string BBOXNAME("prtbbox.txt");  // for BUPRTBOX output.
static string redirect_dir("");         // For tmp files from print.

//--------------------------------------------------------
// Fortran routines expect previous values to be remained
// (at least in some of the arrays) if new msg has same
// descriptors as previous msg !!!
// Thus static arrays, common to all bufr-objects...
// Added lazy evaluation 980501/vk...
//--------------------------------------------------------
static char* In_CNAMES = 0;  // static char In_CNAMES[ MAX_KELEM ][ 64 ]; //- is not updated if same descriptors
static char* In_CUNITS = 0;  // static char In_CUNITS[ MAX_KELEM ][ 24 ]; //- is not updated if same descriptors
static char* In_CVALS  = 0;  // static char In_CVALS [ MAX_KVALS ][ 80 ]; //+ rest of the arrays always updated
static char* Out_CVALS = 0;  // static char Out_CVALS [ MAX_KVALS ][ 80 ];

static double* In_VALUES  = 0;  // static double  In_VALUES[ MAX_KVALS ];
static double* Out_VALUES = 0;  // static double Out_VALUES[ MAX_KVALS ];

static int In_KELEM;
static int In_KVALS;
static int Out_KELEM = -1;
// for BUSEL
static int In_KTDEXL;
static int* In_KTDEXP = 0;  // static int  In_KTDEXP[ MAX_KELEM ];
static int In_KTDLEN;
static int* In_KTDLST = 0;  // static int  In_KTDLST[ MAX_KELEM ];
static int Out_KTDEXL;
static int* Out_KTDEXP = 0;  // static int Out_KTDEXP[ MAX_KELEM ];
static int Out_KTDLEN;
static int* Out_KTDLST = 0;  // static int Out_KTDLST[ MAX_KELEM ];

static int arraySizeIndex = 0;  // incremented each time we try to allocate a new size of arrays
static int kVals;

long MvBufr::_bufrIn_ref     = 0;
long MvBufrOut::_bufrOut_ref = 0;  // not yet implemented...


//-- Linux/g++ does not like hardcoded big? arrays (works ok in debugger,
//-- but crashes inside Metview...) => create arrays dynamically, once!
//-- These are for BUFR-BOX routines (accessing Feedback info)

static int myKSUB, myKBOX, myKAPP, myKLEN, myKERR;
static int* myKBOXR   = 0;
static double* myVALS = 0;
static char* myCBOXN  = 0;
static char* myCBOXU  = 0;

//--------------------------------------------------------
//  Function definitions for the FORTRAN & C BUFR routines
//--------------------------------------------------------

extern "C" {

void BUS012(int* KBUFL, int* KBUFF, int* KSUP, int* KSEC0, int* KSEC1, int* KSEC2, int* KERR);

void BUPRS0(int* KSEC0);
void BUPRS1(int* KSEC1);
void BUPRS2(int* KSUP, int* KEY);

void BUFREX(int* KBUFL, int* KBUFF, int* KSUP, int* KSEC0, int* KSEC1, int* KSEC2, int* KSEC3, int* KSEC4, int* KELEM,
            char* CNAMES, char* CUNITS, int* KVALS, double* VALUES, char* CVALS, int* KERR);

void BUFREN(int* KSEC0, int* KSEC1, int* KSEC2, int* KSEC3, int* KSEC4, int* KTDLEN, int* KTDLST, int* KDLEN,
            int* KDATA, int* KELEM, int* KVALS, double* VALUES, char* CVALS, int* KBUFL, int* KBUFF, int* KERR);

void BUSEL(int* KTDLEN, int* KTDLST, int* KTDEXL, int* KTDEXP, int* KERR);

void BUSEL2(int* KSUBSET, int* KELEM, int* KTDLEN, int* KTDLST, int* KTDEXL, int* KTDEXP, char* CUNITSCNAMES,
            char* CUNITSCUNITS, int* KERR);

void BUUKEY(int* KSEC1, int* KSEC2, int* KEY, int* KSUP, int* KERR);
void BUPRS3(int* KSEC3, int* KTDLEN, int* KTDLST, int* KTDEXL, int* KTDEXP, int* KELEM, char* CNAMES);

void BUBOX(int* KSUB, int* KSUP, int* KELEM, int* KWTR, char* CNAMES, char* CUNITS, int* KVALS, double* VALUES,
           int* KBOX, int* KAPP, int* KLEN, int* KBOXR, double* VALS, char* CBOXN, char* CBOXU, int* KERR);

void BUPRTBOX(int* KBOX, int* KAPP, int* KLEN, int* KBOXR, double* VALS, char* CBOXN, char* CBOXU);

// Helper functions
bool redirect_6(const char*);  //-- Trick to redirect Fortran unit 6.
bool reconnect_6();
bool file_to_stream(const char*, ostream&, int skip);
void delete_print_file(const char*);
void eraseWhiteSpaceFromStringEnd(string& str);
int CIND(int i) {
    return i - 1;
}
}

// Helper functions
static string intToString(int);
static string floatToString(float);
static void keyToStringMap(map<string, string>&, string, int*, int);
static void keyToStringMap(map<string, string>&, string, float);

// F
static void TEMPCHECKVALUEDOUBLE(double, double, const string&, long);
static void TEMPCHECKVALUELONG(long, long, bool isLevel = false);
static void TEMPCHECKVALUESTRING(string&, string&);
//______________________________________________________________________

Section1Base::Section1Base(const unsigned char* octs) {
    int slen = 65536 * octs[0] + 256 * octs[1] + octs[2];
    octets_  = new unsigned char[slen];
    memcpy(octets_, octs, slen);
}

Section1Base::Section1Base(const Section1Base* aSec1) {
    octets_ = new unsigned char[aSec1->len()];
    memcpy(octets_, aSec1->start(), aSec1->len());
}

bool Section1Base::isDifferent(const Section1Base* aSec1) const {
    if (len() != aSec1->len())
        return true;

    for (int i = 0; i < len(); ++i)
        if (octets_[i] != aSec1->octets_[i])
            return true;

    return false;
}

bool Section1_preEd4::hasSection2() {
    return octets_[7] > 127;  //-- octet 8
}
bool Section1_Ed4::hasSection2() {
    return octets_[9] > 127;  //-- octet 10
}

TDynamicTime Section1_preEd4::date() {
    //-- octet 13=year, 14=month, etc.
    return TDynamicTime(octets_[12], octets_[13], octets_[14], octets_[15], octets_[16]);
}
TDynamicTime Section1_Ed4::date() {
    //-- octet 13=year, 14=month, etc.
    return TDynamicTime(256 * octets_[15] + octets_[16], octets_[17], octets_[18], octets_[19], octets_[20],
                        octets_[21]);
}

int Section1_preEd4::msgType() {
    return octets_[8];  //-- octet 9 in ed.3
}
int Section1_Ed4::msgType() {
    return octets_[10];  //-- octet 11 in ed.4
}

int Section1_preEd4::msgSubtypeWMO() {
    return cOctetMissingIndicator;  //-- not available in ed.3
}
int Section1_Ed4::msgSubtypeWMO() {
    return octets_[11];  //-- octet 12 in ed.4
}

int Section1_preEd4::msgSubtypeLocal() {
    return octets_[9];  //-- octet 10 in ed.3
}
int Section1_Ed4::msgSubtypeLocal() {
    return octets_[12];  //-- octet 13 in ed.4
}

int Section1_preEd4::msgSubtype() {
    return msgSubtypeLocal();  //-- only local available in ed.3
}
int Section1_Ed4::msgSubtype() {
    //-- WMO subtype is the preferred one
    return msgSubtypeWMO() != cOctetMissingIndicator ? msgSubtypeWMO() : msgSubtypeLocal();
}

int Section1_preEd4::origCentre() {
    return octets_[5];  //-- octet 6
}
int Section1_Ed4::origCentre() {
    return 256 * octets_[4] + octets_[5];  //-- octets 5 and 6
}

int Section1_preEd4::origSubCentre() {
    return octets_[4];  //-- octet 5
}
int Section1_Ed4::origSubCentre() {
    return 256 * octets_[6] + octets_[7];  //-- octets 7 and 8
}

int Section1_preEd4::masterTable() {
    return octets_[3];  //-- octet 4
}
int Section1_Ed4::masterTable() {
    return octets_[3];  //-- octet 4
}

int Section1_preEd4::masterTableVersion() {
    return octets_[10];  //-- octet 11
}
int Section1_Ed4::masterTableVersion() {
    return octets_[13];  //-- octet 14
}

int Section1_preEd4::localTableVersion() {
    return octets_[11];  //-- octet 12
}
int Section1_Ed4::localTableVersion() {
    return octets_[14];  //-- octet 15
}


//====================================================================== MvBufrBase
//______________________________________________________________________

MvBufrBase ::MvBufrBase(const long len) : Sec1(NULL), fSec2(NULL), fTotalSec2(NULL), fSec3(NULL), fSec4(NULL) {
    _refCount = 0;

    // Make sure the data is correctly aligned.
    longptr  = new long[(len / sizeof(long)) + 1];
    fMessage = (char*)longptr;
    // fMessage = new char[ len  + 8 ];    // +8 for an extra "safety word"
    fMessageLength = len;

    fKSUP  = NULL;
    fKSEC0 = NULL;
    fKSEC1 = NULL;
    fKSEC2 = NULL;
    fKSEC3 = NULL;
    fKSEC4 = NULL;

    //-- lazy evaluation: create static arrays when first needed --
    createDataArrays();
}
//___________________________________________________________

MvBufrBase ::~MvBufrBase(void) {
    //  delete [] fMessage;
    delete[] longptr;

    if (fSec2) {
        delete fSec2;
        delete[] fTotalSec2;
    }
    delete fSec3;
    delete fSec4;

    fSec2      = 0;
    fSec3      = 0;
    fSec4      = 0;
    fTotalSec2 = 0;

    if (fKSUP)
        delete[] fKSUP;
    if (fKSEC0)
        delete[] fKSEC0;
    if (fKSEC1)
        delete[] fKSEC1;
    if (fKSEC2)
        delete[] fKSEC2;
    if (fKSEC3)
        delete[] fKSEC3;
    if (fKSEC4)
        delete[] fKSEC4;

    fKSUP = fKSEC0 = fKSEC1 = fKSEC2 = fKSEC3 = fKSEC4 = 0;
}
//___________________________________________________________
void MvBufrBase ::attach(void) {
    _refCount++;
}
void MvBufrBase ::detach(void) {
    if (--_refCount == 0)
        delete this;
}
//_________________________________________________________ createFortranArrays
void MvBufrBase ::createFortranArrays(void) {
    fKSUP  = new int[9];
    fKSEC0 = new int[3];
    fKSEC1 = new int[40];
    fKSEC2 = new int[4096];  //[ 128 ]; //[ 64 ];
    fKSEC3 = new int[4];
    fKSEC4 = new int[2];
}
//_________________________________________________________ createDataArrays
void MvBufrBase ::createDataArrays(void) {
    kVals    = aMAX_KVALS[arraySizeIndex];
    In_KVALS = kVals;

    try {
        if (In_VALUES == 0)
            In_VALUES = new double[kVals];

        if (In_CVALS == 0)
            In_CVALS = new char[kVals * 80];

        if (In_CNAMES == 0)
            In_CNAMES = new char[MAX_KELEM * 64];

        if (In_CUNITS == 0)
            In_CUNITS = new char[MAX_KELEM * 24];

        if (In_KTDEXP == 0)
            In_KTDEXP = new int[MAX_KELEM];

        if (In_KTDLST == 0)
            In_KTDLST = new int[MAX_KELEM];
    }

    catch (...) {
        deleteDataArrays();

#ifdef METVIEW
        marslog(LOG_EROR, "MvBufrBase::MvBufrBase: out-of-memory!");
        throw MvException("MvBufrBase::MvBufrBase: out-of-memory!");
#else
        cerr << ">>>> MvBufrBase::MvBufrBase: out-of-memory => throw an exception <<<<" << std::endl;
        throw std::bad_alloc();
#endif
    }
}
//_________________________________________________________ createDataArrays
void MvBufrBase ::deleteDataArrays(void) {
    if (In_VALUES != 0) {
        delete[] In_VALUES;
        In_VALUES = 0;
    }

    if (In_CVALS != 0) {
        delete[] In_CVALS;
        In_CVALS = 0;
    }

    if (In_CNAMES != 0) {
        delete[] In_CNAMES;
        In_CNAMES = 0;
    }

    if (In_CUNITS != 0) {
        delete[] In_CUNITS;
        In_CUNITS = 0;
    }

    if (In_KTDEXP != 0) {
        delete[] In_KTDEXP;
        In_KTDEXP = 0;
    }

    if (In_KTDLST != 0) {
        delete[] In_KTDLST;
        In_KTDLST = 0;
    }
}
//___________________________________________________________
unsigned int MvBufrBase ::unsignedInt(const unsigned char* firstOctet, int octetCount) {
    unsigned int value           = *firstOctet;
    const unsigned char* anOctet = ++firstOctet;
    for (int i = octetCount - 1; i; --i) {
        value = 256 * value + *anOctet++;
    }
    return value;
}

//______________________________________________________________________
//====================================================================== MvBufr
//______________________________________________________________________

//______________________________________________________________________

// F temporary method. The objective is to pass the eccodes file handler
void MvBufr::setEccodes(codes_handle** ecH) {
    _ecH = ecH;
}
// F


MvBufr ::MvBufr(char* msg, long len, long aMessageNumber) :
    MvBufrBase(len)  //( msg, len )
{
    _lastKnownSubsetValue = 1;  //-- Q&D

    memcpy(fMessage, msg, (int)len);

    const unsigned char* msgStart = (const unsigned char*)fMessage;

    fSec0      = (TSection0*)fMessage;
    int offSet = fSec0->editionNr < 2 ? 4 : 8;
    // Section 0 was shorter in BUFR editions 0 and 1 !!!!
    //                                  rev vk 950802
    if (fSec0->editionNr > 3)
        Sec1 = new Section1_Ed4(msgStart + offSet);
    else
        Sec1 = new Section1_preEd4(msgStart + offSet);
    offSet += Sec1->len();

    if (Sec1->hasSection2())  // bit 0 <=> Optional Section 2 present?
    {
        fSec2 = new TSection2;
        memcpy((char*)fSec2, msgStart + offSet, sizeof(TSection2));

        int sec2Len = unsignedInt(&(fSec2->len), 3);
        fTotalSec2  = new unsigned char[sec2Len];
        memcpy((char*)fTotalSec2, msgStart + offSet, sec2Len);

        offSet += sec2Len;
    }
    else {
        fSec2      = NULL;
        fTotalSec2 = NULL;
    }

    fSec3 = new TSection3;
    memcpy((char*)fSec3, msgStart + offSet, sizeof(TSection3));
    offSet += unsignedInt(&(fSec3->len), 3);

    fSec4 = new TSection4;
    memcpy((char*)fSec4, msgStart + offSet, sizeof(TSection4));
    offSet += unsignedInt(&(fSec4->len), 3);

    computeIn_KELEM();

    fMessageNumber   = aMessageNumber;
    _currentDescr    = 0;
    _currentDescrInd = -1;
    _inState         = kBufrIn_Coded;
    _bufrIn_ref++;
    _bufrBoxFilledSubset = 0;
}
//______________________________________________________________________

MvBufr ::~MvBufr(void) {}

//____________________________________________________________________ Decode
void MvBufr ::Decode(void) {
    if (_inState == kBufrIn_Error)
        return;

    if (_inState < kBufrIn_DataDecoded) {
        createFortranArrays();
        int myKBUFL    = fMessageLength / sizeof(int) + 1;  // +1 = Q&D
        bool keepGoing = true;

        // we will try to decode the BUFR a number of times, starting with
        // relatively small data arrays, then building up to larger ones
        // (so that we don't waste too much memory)

        while (keepGoing) {
            keepGoing = false;

            BUFREX(&myKBUFL, (int*)fMessage  // buffer for the BUFR message
                   ,
                   fKSUP  // array for suplementary info
                   ,
                   fKSEC0  // FORTRANized section 0
                   ,
                   fKSEC1  // FORTRANized section 1
                   ,
                   fKSEC2  // FORTRANized section 2 (site dependent)
                   ,
                   fKSEC3  // FORTRANized section 3 (data descriptors)
                   ,
                   fKSEC4  // FORTRANized section 4 (data values)
                   ,
                   &In_KELEM, In_CNAMES, In_CUNITS, &In_KVALS, In_VALUES, In_CVALS, &fKERR);


            // if KELEM is not big enough, then we will
            // try again with a bigger value

            if (fKERR == 25 || fKERR == 14) {
                std::cout << "MvBufr :: Decode - kVals of " << kVals << " and In_KELEM of " << In_KELEM
                          << " is not large enough." << std::endl;

                if (++arraySizeIndex < NUM_MAX_KVALS) {
                    std::cout << "Trying kVals of " << aMAX_KVALS[arraySizeIndex] << " ("
                              << (aMAX_KVALS[arraySizeIndex] * 80) / (1024 * 1024) << "MB)" << std::endl;

                    deleteDataArrays();
                    createDataArrays();
                    computeIn_KELEM();

                    keepGoing = true;
                }
                else
                    arraySizeIndex = NUM_MAX_KVALS - 1;  // should not be used, but just in case,
                                                         // make sure the index is not out of bounds...
            }
        }


        if (fKERR) {
            cerr << "In_KELEM " << In_KELEM << " kvals " << In_KVALS << std::endl;
            cerr << " >>> MvBufr::Decode, bufrmsg " << fMessageNumber << ": fKERR = " << fKERR << std::endl;


#ifdef METVIEW
            marslog(LOG_EROR, "BUFR decoding (BUFREX) failed, status = %d", fKERR);
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
void MvBufr ::Decode_012(void) {
    if (_inState == kBufrIn_Error)
        return;

    if (_inState < kBufrIn_Sections012Expanded) {
        createFortranArrays();
        int myKBUFL = fMessageLength / sizeof(int) + 1;  // +1 = Q&D

        BUS012(&myKBUFL, (int*)fMessage  // buffer for the BUFR message
               ,
               fKSUP  // array for suplementary info
               ,
               fKSEC0  // FORTRANized section 0
               ,
               fKSEC1  // FORTRANized section 1
               ,
               fKSEC2  // FORTRANized section 2 (site dependent)
               ,
               &fKERR);


        if (fKERR) {
            cerr << " >>> MvBufr::Decode_012, bufrmsg " << fMessageNumber << ": fKERR = " << fKERR << std::endl;

#ifdef METVIEW
            marslog(LOG_EROR, "BUFR decoding (BUFREX) failed, status = %d", fKERR);
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
void MvBufr ::ExpandDescriptors(int subsetNumber) {
    static int mySubsetSavedValue = 0;

    _lastKnownSubsetValue = subsetNumber;

    if (_inState == kBufrIn_Error)
        return;

    //   In_KTDLEN = -1;
    //   In_KTDEXL = -1;
    fKERR = 0;

    if (_inState < kBufrIn_DataDecoded)
        Decode();

    if ((fKSEC3[3 - 1] > 1 &&                          //-- if several subsets
         _lastKnownSubsetValue != mySubsetSavedValue)  //-- AND subset number changed
        ||                                             //-- OR
        _inState == kBufrIn_DataDecoded)               //-- descriptors not yet expanded
    {
        // cout << "-----> MvBufr::ExpandDescriptors(" << _lastKnownSubsetValue << ") => BUSEL2..." << std::endl;
        //      CALL BUSEL2(KSUBSET,KELEM,KTDLEN,KTDLST,KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
        BUSEL2(&_lastKnownSubsetValue  //-- Q&D variable...
               ,
               &In_KELEM, &In_KTDLEN  // nr of original data descriptors in Section 3
               ,
               In_KTDLST  // original descriptors
               ,
               &In_KTDEXL  // nr of expanded data descriptors
               ,
               In_KTDEXP  // expanded descriptors
               ,
               In_CNAMES, In_CUNITS, &fKERR);

        mySubsetSavedValue = _lastKnownSubsetValue;
    }
    else {
        if (_inState == kBufrIn_DataDecoded) {
            // cout << "-----> MvBufr::ExpandDescriptors(" << _lastKnownSubsetValue << ") => BUSEL..." << std::endl;
            BUSEL(&In_KTDLEN  // nr of original data descriptors in Section 3
                  ,
                  In_KTDLST  // original descriptors
                  ,
                  &In_KTDEXL  // nr of expanded data descriptors
                  ,
                  In_KTDEXP  // expanded descriptors
                  ,
                  &fKERR);
        }
    }

    if (fKERR) {
        cerr << " >>> MvBufr::ExpandDescriptors: fKERR = " << fKERR << std::endl;

#ifdef METVIEW
        marslog(LOG_EROR, "BUFR expansion (BUSEL) failed, status = %d", fKERR);
#endif

        _inState = kBufrIn_Error;
    }
    else
        _inState = kBufrIn_DataAndDescriptorsDecoded;
}
//__________________________________________________________ descriptorToFortranIndex
int MvBufr ::descriptorToFortranIndex(const long aDescr, const int firstIndex) {
    if (_inState == kBufrIn_Error)
        return -1;

    if (_inState != kBufrIn_DataAndDescriptorsDecoded)
        ExpandDescriptors(_lastKnownSubsetValue);  //-- Q&D

    if (_inState == kBufrIn_DataAndDescriptorsDecoded) {
        for (int i = firstIndex; i < In_KTDEXL; i++) {
            if (In_KTDEXP[i] == aDescr) {
                return i;
            }
        }
    }
    return -1;
}
//__________________________________________________________ computeIn_KELEM
void MvBufr ::computeIn_KELEM(void) {
    if (subsetCount() > 1)
        In_KELEM = kVals / subsetCount();
    else
        In_KELEM = MAX_KELEM;

    if (In_KELEM > MAX_BUBOX_KELEM_LIMIT)
        In_KELEM = MAX_BUBOX_KELEM_LIMIT;
}
//__________________________________________________________ DataValue
//
// Returns the requested parameter value (or missing value)
// and updates _currentDescrInd.
//

double MvBufr ::DataValue(const int aDescrArrayInd, const long aSubsetNumber) {
    _lastKnownSubsetValue = aSubsetNumber;

    if (_inState < kBufrIn_DataDecoded)
        Decode();

    if ((aSubsetNumber > subsetCount()) || (aDescrArrayInd < 0)) {
        _currentDescr    = 0;
        _currentDescrInd = -1;
        return kFortranBufrMissingValue;
    }

    _currentDescrInd = aDescrArrayInd;
    _currentDescr    = In_KTDEXP[_currentDescrInd];

    return PeekDataValue(_currentDescrInd, aSubsetNumber);
}
//__________________________________________________________ PeekDataValue
//
// Returns the requested parameter value (or missing value)
// without updating _currentDescrInd.
//

double MvBufr ::PeekDataValue(const int aDescrArrayInd, const long aSubsetNumber) {
    if ((aSubsetNumber > subsetCount()) || (aDescrArrayInd < 0))
        return kFortranBufrMissingValue;
    else
        return In_VALUES[aDescrArrayInd + (aSubsetNumber - 1) * In_KELEM];
}
//__________________________________________________________ Value
bool MvBufr ::Value(const long aDescriptor, const long aSubsetNumber, double& aDataValue, int firstInd) {
    aDataValue = DataValue(descriptorToFortranIndex(aDescriptor, firstInd), aSubsetNumber);
    return aDataValue != kFortranBufrMissingValue ? true : false;
}
//__________________________________________________________ intValue
// returns 'kFortranBufrMissingIntValue' if not found!
//----------------------------------------------------
long MvBufr ::intValue(const long aDescriptor, const int subsetNr) {
    double myValue;
    Value(aDescriptor, subsetNr, myValue);

    if (myValue != kFortranBufrMissingValue)
        return (long)myValue;
    else
        return kFortranBufrMissingIntValue;
}
//____________________________________________________________________ feedbackValue
double MvBufr::feedbackValue(int col, int subset) {
    if (_currentDescrInd < 0)
        return kBufrMissingValue;
    else
        //-- first 6 rows is reserved info, obs report starts on row 7
        return feedbackValue(_currentDescrInd + 6, col, subset);
}

double MvBufr::feedbackValue(int row, int col, int subset) {
    int err = fillBufrBox(subset);

    assert(row > 0 && row <= myKBOX);
    assert(col > 0 && col <= myKAPP);

    if (err == 0)
        return myVALS[myKLEN * (col - 1) + row - 1];
    else
        return kBufrMissingValue;
}

string MvBufr::feedbackItemName(int row, int subset) {
    int err = fillBufrBox(subset);

    assert(row > 0 && row <= myKBOX);

    if (err == 0) {
        char c[64];
        strncpy(c, myCBOXN + (row - 1) * 64, 63);
        c[63] = '\0';
        string s(c);
        eraseWhiteSpaceFromStringEnd(s);
        return s;
    }
    else
        return std::string();
}

string MvBufr::feedbackItemUnit(int row, int subset) {
    int err = fillBufrBox(subset);

    assert(row > 0 && row <= myKBOX);
    ;

    if (err == 0) {
        char c[24];
        strncpy(c, myCBOXU + (row - 1) * 24, 23);
        c[23] = '\0';
        string s(c);
        eraseWhiteSpaceFromStringEnd(s);
        return s;
    }
    else
        return std::string();
}

//____________________________________________________________________ obsTime
TDynamicTime MvBufr::obsTime(const int subsetNr) {
    int myYear  = intValue(4001L, subsetNr);
    int myMonth = intValue(4002L, subsetNr);
    int myDay   = intValue(4003L, subsetNr);
    int myHour  = intValue(4004L, subsetNr);

    short myMin, mySec;
    double myValue;

    if (Value(4005L, subsetNr, myValue))
        myMin = (short)myValue;
    else
        myMin = 0;

    if (Value(4006L, subsetNr, myValue))
        mySec = (short)myValue;
    else
        mySec = 0;

    //-- quirky NCEP PrepBUFR obs may not contain date/time infromation
    if (myYear == kBufrMissingIntValue &&                                    //-- date OK?
        myMonth == kBufrMissingIntValue && myDay == kBufrMissingIntValue) {  //-- if date missing from obs
        return msgTime();                                                    //-- then take it from section 1
    }
    else  //-- OK, take from obs
    {
        return TDynamicTime((short)myYear, (short)myMonth, (short)myDay, (short)myHour, myMin, mySec);
    }
}
//____________________________________________________________________ msgTime
TDynamicTime MvBufr ::msgTime(void) {
    return Sec1->date();
}
//____________________________________________________________________ stringValue
string MvBufr ::stringValue(const long aDescriptor, const int aSubsetNr) {
    _currentDescrInd = descriptorToFortranIndex(aDescriptor);
    return stringValueByIndex(_currentDescrInd, aSubsetNr);
}
//____________________________________________________________________ stringValue
string MvBufr ::stringValue(const int aSubsetNr) {
    return stringValueByIndex(_currentDescrInd, aSubsetNr);
}
//__________________________________________________________ stringValueByIndex
string MvBufr ::stringValueByIndex(const int anIndex, const int aSubsetNr) {
    if ((anIndex < 0) || (anIndex >= In_KTDEXL)) {
        return string("[string index error!]");
    }

    //-- get coded float value --
    double myValue = DataValue(anIndex, aSubsetNr);

    //-- here we should be passing subset nr, not index!!! (020307/vk)
    //-- thus always pass 1:

    if (elementValueType(1) == kEVT_missing) {
        return string("[Missing]");
    }
    else if (elementValueType(1) == kEVT_string) {
        //-- get pointer and length to In_CVALS array, In_CVALS starts from 0.. --
        int myIndex  = (int)myValue / 1000 - 1;
        int myLength = (int)myValue % 1000;

        //-- add C-terminator (sacrifice last character if necessary) --
        int terminatorPos = myLength;
        if (terminatorPos > 79)
            terminatorPos = 79;
        // In_CVALS[ myIndex ][ terminatorPos ] = '\0';
        In_CVALS[myIndex * 80 + terminatorPos] = '\0';

        return string(In_CVALS + myIndex * 80);
    }
    else if (elementValueType(1) == kEVT_numeric) {
        ostringstream oss;
        oss << myValue;

        return oss.str();
    }

    return string("[Internal error]");  //-- we should never get here!
}
//____________________________________________________________________ unit
string MvBufr ::unit(const long aDescriptor) {
    return unitByIndex(descriptorToFortranIndex(aDescriptor));
}
//____________________________________________________________________ unit
string MvBufr ::unit(void) {
    return unitByIndex(_currentDescrInd);
}
//____________________________________________________________________ unitByIndex
string MvBufr ::unitByIndex(const int anIndex) {
    char strbuf[25];

    if ((anIndex >= 0) && (anIndex < In_KTDEXL)) {
        strbuf[24] = '\0';  //-- make a copy
        int pos;
        for (pos = 23; pos >= 0; --pos)
            strbuf[pos] = In_CUNITS[anIndex * 24 + pos];
        // strbuf[ pos ] = In_CUNITS[ anIndex ][ pos ];

        for (pos = 23; pos > 0; pos--)  //-- remove trailing blanks
            if (strbuf[pos] == ' ')
                strbuf[pos] = '\0';
            else
                break;

        return string(strbuf);
    }
    return string("[Unit not found!]");
}
//____________________________________________________________________ name
string MvBufr ::name(const long aDescriptor) {
    return nameByIndex(descriptorToFortranIndex(aDescriptor));
}
//____________________________________________________________________ name
string MvBufr ::name(void) {
    return nameByIndex(_currentDescrInd);
}
//____________________________________________________________________ nameByIndex
string MvBufr ::nameByIndex(const int anIndex) {
    char strbuf[65];

    if ((anIndex >= 0) && (anIndex < In_KTDEXL)) {
        strbuf[64] = '\0';  //-- make copy
        int pos;
        for (pos = 63; pos >= 0; --pos)
            strbuf[pos] = In_CNAMES[anIndex * 64 + pos];
        // strbuf[ pos ] = In_CNAMES[ anIndex ][ pos ];

        for (pos = 63; pos > 0; pos--)  //-- remove trailing blanks
            if (strbuf[pos] == ' ')
                strbuf[pos] = '\0';
            else
                break;

        bool retainCapital = true;  //-- change to lower case
        for (pos = 0; pos < (int)(strlen(strbuf)); pos++) {
            if (isupper(strbuf[pos])) {
                if (retainCapital)
                    retainCapital = false;
                else
                    strbuf[pos] = tolower(strbuf[pos]);
            }
            else
                retainCapital = true;
        }
        return string(strbuf);
    }
    else
        return string("[Name index error!]");
}
//____________________________________________________________________ elementValueType
EElementValueType MvBufr ::elementValueType(const int aSubsetNr) {
    return elementValueTypeByIndex(_currentDescrInd, aSubsetNr);
}
//____________________________________________________________ elementValueType
EElementValueType MvBufr ::elementValueType(const long aDescriptor, const int aSubsetNr) {
    return elementValueTypeByIndex(descriptorToFortranIndex(aDescriptor), aSubsetNr);
}
//_____________________________________________________ elementValueTypeByIndex
EElementValueType MvBufr ::elementValueTypeByIndex(const int anIndex, const int aSubsetNr) {
    double myValue = DataValue(anIndex, aSubsetNr);

    if (myValue == kFortranBufrMissingValue) {
        return kEVT_missing;
    }
    else {
        bool isString = unitByIndex(anIndex) == "CCITTIA5"       //-- ECMWF notation
                        || unitByIndex(anIndex) == "CCITT IA5";  //-- WMO & NCEP PrepBUFR notation
        return isString ? kEVT_string : kEVT_numeric;
    }
}
//______________________________________________________________________
//
// Reset descriptor iterator by pointing to the first descriptor.
// Make sure msg has been expanded!
//

bool MvBufr ::SetFirstDescriptor(void) {
    if (_inState == kBufrIn_Error)
        return false;

    if (_inState != kBufrIn_DataAndDescriptorsDecoded)
        ExpandDescriptors(_lastKnownSubsetValue);  //-- Q&D

    if (_inState == kBufrIn_DataAndDescriptorsDecoded) {
        _currentDescrInd = 0;
        _currentDescr    = In_KTDEXP[_currentDescrInd];
        return true;
    }
    return false;
}
//______________________________________________________________________
//
// Advance descriptor iterator.
// Check that it still points ok.
//

bool MvBufr ::SetNextDescriptor(void) {
    if (_currentDescrInd < 0)
        return false;  // SetFirstDescriptor had not been called !

    _currentDescrInd++;
    if (_currentDescrInd == In_KTDEXL)  // or: fKSUP[ 5 - 1 ]
    {
        _currentDescrInd = -1;  //-- end-of-msg reached
        _currentDescr    = 0;   //-- probably end-of-iteration => issue no msg
        return false;
    }
    else if (_currentDescrInd > In_KTDEXL) {
        _currentDescrInd = -1;  //-- past end-of-msg
        _currentDescr    = 0;   //-- must be an error => issue error msg

        cerr << "MvBufr::SetNextDescriptor: _currentDescrInd=" << _currentDescrInd
             << ", limiting In_KTDEXL=" << In_KTDEXL << std::endl;

        return false;
    }

    _currentDescr = In_KTDEXP[_currentDescrInd];
    return true;
}
//______________________________________________________________________
//
// calls Emoslib routines BUBOX and BUPRTBOX to produce "boxed" output
//
int MvBufr::fillBufrBox(int aSubsetNr) {
#if 0
  //-- Linux/g++ does not like hardcoded big? arrays (works ok in debugger,
  //-- but crashes inside Metview...) => create arrays dynamically, once!

  static int    myKSUB, myKBOX, myKAPP, myKLEN, myKERR;
  static int*   myKBOXR = 0;
  static double* myVALS  = 0;
  static char*      myCBOXN = 0;
  static char*      myCBOXU = 0;
#endif

    if (_bufrBoxFilledSubset == aSubsetNr)
        return 0;  //-- OK, already filled

    if (!myKBOXR) {
        try {
            myKBOXR = new int[kVals];
            std::cout << " fillBufrBox: array myKBOXR created" << std::endl;

            myVALS = new double[kVals];
            std::cout << " fillBufrBox: array myVALS  created" << std::endl;

            myCBOXN = new char[kVals * 64];
            std::cout << " fillBufrBox: array myCBOXN created" << std::endl;

            myCBOXU = new char[kVals * 24];
            std::cout << " fillBufrBox: array myCBOXU created" << std::endl;
        }
        catch (...) {
            std::cout << " >>> fillBufrBox: problems in creating fort arrays <<<" << std::endl;
            delete myKBOXR;
            myKBOXR = 0;
            delete myVALS;
            myVALS = 0;
            delete myCBOXN;
            myCBOXN = 0;
            delete myCBOXU;
            myCBOXU = 0;
#ifdef METVIEW
            marslog(LOG_EROR, "MvBufr::fillBufrBox: out-of-memory?");
#endif
            return -13;
        }
    }


    //-- Initialize array given to bubox. bubox will not initialize
    //-- all values, and this will cause runtime error from buprtbox.
    for (int i = 0; i < kVals; i++)
        myVALS[i] = kFortranBufrMissingValue;

    if (_inState == kBufrIn_Error) {
        std::cout << " fillBufrBox: BUFR msg error state, return false" << std::endl;
        return -1313;
    }

    if (_inState < kBufrIn_DataDecoded) {
        Decode();
    }

    if (_inState != kBufrIn_DataAndDescriptorsDecoded) {
        ExpandDescriptors(aSubsetNr);
    }

    myKSUB = (int)aSubsetNr;
    myKBOX = 0;
    myKAPP = 0;
    myKLEN = 0;
    myKERR = 0;

    BUBOX(&myKSUB  //-- INPUT arguments
          ,
          fKSUP, &In_KELEM, In_KTDEXP, In_CNAMES, In_CUNITS, &In_KVALS,
          In_VALUES
          //-- OUTPUT arguments
          ,
          &myKBOX  //-- number of (valid) elements in 1st column
          ,
          &myKAPP  //-- number of columns (apps) in the box
          ,
          &myKLEN  //-- number of rows in the box
          ,
          myKBOXR  //-- Table B descriptors
          ,
          myVALS  //-- boxed values
          ,
          myCBOXN  //-- boxed element names
          ,
          myCBOXU  //-- boxed units
          ,
          &myKERR);

    if (myKERR == 0)
        _bufrBoxFilledSubset = myKSUB;

    return myKERR;
}
//______________________________________________________________________
//
bool MvBufr::writeBufrBox(int aSubsetNr) {
    std::cout << " writeBufrBox: entering" << std::endl;

    myKERR = fillBufrBox(aSubsetNr);

    //-- forces Fortran unit 6 into a file
    if (!redirect_6(BBOXNAME.c_str())) {
        std::cout << ">>> UNABLE TO REDIRECT stdout <<<" << std::endl;
        return false;
    }

    if (myKERR == 0) {
        BUPRTBOX(&myKBOX, &myKAPP, &myKLEN, myKBOXR, myVALS, myCBOXN, myCBOXU);
    }

    //-- closes "unit'ified" unit 6
    if (!reconnect_6()) {
        cerr << ">>> UNABLE TO RECONNECT TO stdout <<<" << std::endl;
        std::cout << ">>> UNABLE TO RECONNECT TO stdout <<<" << std::endl;
    }

    return myKERR == 0;
}

bool MvBufr::getBufrBoxSize(int& rows, int& cols, int aSubsetNr) {
    bool status = false;

    myKERR = fillBufrBox(aSubsetNr);

    if (myKERR == 0) {
        rows   = myKBOX;
        cols   = myKAPP;
        status = true;
    }
    return status;
}

//------------- Printing functions -----------------
bool MvBufr ::printSection(ostream& aStream, int which) {
    bool return_val = true;
    if (_inState == kBufrIn_Error) {
        aStream << "!!!!!!!!!!! Bad BUFR message " << std::endl;
        return false;
    }

    if (_inState < kBufrIn_Sections012Expanded)
        Decode();

    if (which == 3 && _inState < kBufrIn_DataAndDescriptorsDecoded)
        ExpandDescriptors(_lastKnownSubsetValue);  //-- Q&D


    char sec_name[30];
    sprintf(sec_name, "prtsec%d.txt", which);

    redirect_6(sec_name);
    if (which == 0)
        BUPRS0(fKSEC0);
    else if (which == 1)
        BUPRS1(fKSEC1);
    else if (which == 2) {
        int* fKEY = new int[60];
        BUUKEY(fKSEC1, fKSEC2, fKEY, fKSUP, &fKERR);
        if (fKERR)
            cout << "\nProblems getting key. Maybe non-existent? " << std::endl;
        else
            BUPRS2(fKSUP, fKEY);

        delete[] fKEY;
    }
    else
        BUPRS3(fKSEC3, &In_KTDLEN, In_KTDLST, &In_KTDEXL, In_KTDEXP, &In_KELEM, In_CNAMES);

    reconnect_6();
    return_val = file_to_stream(sec_name, aStream, 1);
    delete_print_file(sec_name);
    return return_val;
}

//------------- Printing functions -----------------
bool MvBufr::getDataFromSection2(map<string, string>& data) {
    bool retval = false;

    if (_inState == kBufrIn_Error) {
        std::cout << "!!!!!!!!!!! Bad BUFR message " << std::endl;
        return false;
    }

    if (_inState < kBufrIn_Sections012Expanded)
        Decode_012();

    int* fKEY = new int[60];
    BUUKEY(fKSEC1, fKSEC2, fKEY, fKSUP, &fKERR);
    if (fKERR) {
        cout << "\nProblems getting key. Maybe non-existent? " << std::endl;
        retval = false;
    }
    else {
        parseSection2(fKEY, data);
        retval = true;
    }

    delete[] fKEY;

    return retval;
}

void MvBufr::parseSection2(int* fKEY, map<string, string>& data) {
    // fKSUP - global variable

    if (fKSUP[CIND(2)] < 1) {
        return;
    }

    int type = 0;
    if (fKEY[CIND(2)] == 2)
        type = 2;
    else if (fKEY[CIND(2)] == 3)
        type = 2;
    else if (fKEY[CIND(2)] == 12)
        type = 2;
    else if (fKEY[CIND(2)] == 8)
        type = 2;

    if (type == 0 && fKSUP[CIND(6)] > 1)
        type = 2;

    if (type == 2) {
        keyToStringMap(data, "RDB DATA TYPE", fKEY, 2);
        keyToStringMap(data, "RDB DATA SUBTYPE", fKEY, 3);
        keyToStringMap(data, "YEAR", fKEY, 4);
        keyToStringMap(data, "MONTH", fKEY, 5);
        keyToStringMap(data, "DAY", fKEY, 6);
        keyToStringMap(data, "HOUR", fKEY, 7);
        keyToStringMap(data, "MINUTE", fKEY, 8);
        keyToStringMap(data, "SECOND", fKEY, 9);

        float RLAT1 = (fKEY[CIND(11)] - 9000000) / 100000.;
        float RLON1 = (fKEY[CIND(10)] - 18000000) / 100000.;
        keyToStringMap(data, "LATITUDE 1", RLAT1);
        keyToStringMap(data, "LONGITUDE 1", RLON1);

        float RLAT2 = (fKEY[CIND(13)] - 9000000) / 100000.;
        float RLON2 = (fKEY[CIND(12)] - 18000000) / 100000.;
        keyToStringMap(data, "LATITUDE 2", RLAT2);
        keyToStringMap(data, "LONGITUDE 2", RLON2);

        keyToStringMap(data, "NUMBER OF OBSERVATIONS", fKEY, 14);

        // char ident[9];
        // memcpy(ident,&fKEY[CIND(15)],8);
        // data["IDENTIFIER"]=string(ident);

        keyToStringMap(data, "IDENTIFIER", fKEY, 15);
        keyToStringMap(data, "TOTAL BUFR MESSAGE LENGTH", fKEY, 25);
        keyToStringMap(data, "DAY (RDB INSERTION)", fKEY, 26);
        keyToStringMap(data, "HOUR (RDB INSERTION)", fKEY, 27);
        keyToStringMap(data, "MINUTE (RDB INSERTION)", fKEY, 28);
        keyToStringMap(data, "SECOND (RDB INSERTION)", fKEY, 29);
        keyToStringMap(data, "DAY (MDB ARRIVAL)", fKEY, 30);
        keyToStringMap(data, "HOUR (MDB ARRIVAL)", fKEY, 31);
        keyToStringMap(data, "MINUTE (MDB ARRIVAL", fKEY, 32);
        keyToStringMap(data, "SECOND (MDB ARRIVAL)", fKEY, 33);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 34);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 35);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 37);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 38);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 40);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 41);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 43);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 44);
        keyToStringMap(data, "QUALITY CONTROL % CONF", fKEY, 46);
    }
    else {
        keyToStringMap(data, "RDB DATA TYPE", fKEY, 2);
        keyToStringMap(data, "RDB DATA SUBTYPE", fKEY, 3);
        keyToStringMap(data, "YEAR", fKEY, 4);
        keyToStringMap(data, "MONTH", fKEY, 5);
        keyToStringMap(data, "DAY", fKEY, 6);
        keyToStringMap(data, "HOUR", fKEY, 7);
        keyToStringMap(data, "MINUTE", fKEY, 8);
        keyToStringMap(data, "SECOND", fKEY, 9);

        float RLAT1 = (fKEY[CIND(11)] - 9000000) / 100000.;
        float RLON1 = (fKEY[CIND(10)] - 18000000) / 100000.;
        keyToStringMap(data, "LATITUDE 1", RLAT1);
        keyToStringMap(data, "LONGITUDE 1", RLON1);

        // char ident[9];
        // memcpy(ident,&fKEY[CIND(15)],8);
        // data["IDENTIFIER"]=string(ident);

        char ident[10];
        for (int i = 16; i <= 24; i++) {
            ident[i - 16] = fKEY[CIND(i)];
        }
        ident[9]           = '\0';
        data["IDENTIFIER"] = string(ident);

        keyToStringMap(data, "TOTAL BUFR MESSAGE LENGTH", fKEY, 25);
        keyToStringMap(data, "DAY (RDB INSERTION)", fKEY, 26);
        keyToStringMap(data, "HOUR (RDB INSERTION)", fKEY, 27);
        keyToStringMap(data, "MINUTE (RDB INSERTION)", fKEY, 28);
        keyToStringMap(data, "SECOND (RDB INSERTION)", fKEY, 29);
        keyToStringMap(data, "DAY (MDB ARRIVAL)", fKEY, 30);
        keyToStringMap(data, "HOUR (MDB ARRIVAL)", fKEY, 31);
        keyToStringMap(data, "MINUTE (MDB ARRIVAL", fKEY, 32);
        keyToStringMap(data, "SECOND (MDB ARRIVAL)", fKEY, 33);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 34);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 35);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 37);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 38);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 40);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 41);
        keyToStringMap(data, "CORRECTION NUMBER", fKEY, 43);
        keyToStringMap(data, "PART OF MESSAGE", fKEY, 44);
        keyToStringMap(data, "QUALITY CONTROL % CONF", fKEY, 46);
    }
}

//------------- Printing functions -----------------
bool MvBufr ::printSection_012(ostream& aStream, int which) {
    if (which < 0 || which > 2)
        return false;

    bool return_val = true;
    if (_inState == kBufrIn_Error) {
        aStream << "!!!!!!!!!!! Bad BUFR message " << std::endl;
        return false;
    }

    if (_inState < kBufrIn_Sections012Expanded)
        Decode_012();

    char sec_name[30];
    sprintf(sec_name, "prtsec%d.txt", which);

    redirect_6(sec_name);
    if (which == 0)
        BUPRS0(fKSEC0);
    else if (which == 1)
        BUPRS1(fKSEC1);
    else if (which == 2) {
        int* fKEY = new int[60];
        BUUKEY(fKSEC1, fKSEC2, fKEY, fKSUP, &fKERR);
        if (fKERR)
            cout << "\nProblems getting key. Maybe non-existent? " << std::endl;
        else
            BUPRS2(fKSUP, fKEY);

        delete[] fKEY;
    }

    reconnect_6();
    return_val = file_to_stream(sec_name, aStream, 1);
    delete_print_file(sec_name);
    return return_val;
}
#endif  // MV_BUFRDC_TEST

//______________________________________________________________________
//====================================================================== MvBufrOut
// FAMI20171024 : This class is kept for backwards compatibility. At the
// moment, ecCodes is only using method "add". This method is called from
// class MvObsSet and contains only one command which calls back class
// MvObsSet. Maybe, this class can be removed in the future.
//______________________________________________________________________

//____________________________________________________________________ Constructor

#ifdef MV_BUFRDC_TEST
MvBufrOut::MvBufrOut(const long len, MvObsSet* aSet) :
    MvBufrBase(len),
    _currentSec1(0),
    _outSet(aSet)
#else
MvBufrOut::MvBufrOut(MvObsSet* aSet) :
    _outSet(aSet)
#endif
{
#ifdef MV_BUFRDC_TEST
    _maxNrSubsets = 1;
    Out_KELEM     = -1;  // MAX_KELEM / _maxNrSubsets;

    createFortranArrays();

    _outState = kBufrOut_noBuffers;
    resetBuffers();
#endif
}

//____________________________________________________________________ Destructor
MvBufrOut::~MvBufrOut() {
#ifdef MV_BUFRDC_TEST
    if (_outState == kBufrOut_dataInBuffers)
        encode();

    //  _outSet->close();
    delete _currentSec1;
#else
//   _outSet->close();  // the owner should be responsible to close this file
#endif
}

//____________________________________________________________________ add
void MvBufrOut::add(MvObs& anObs) {
#ifdef MV_BUFRDC_TEST
    if (_maxNrSubsets == 1 && anObs._bufrIn->subsetCount() == 1)
        //-- if no packing into subsets, copy message as is...
        write_bufrdc(anObs);
    else
        addIntoBuffers(anObs);
#endif

    _outSet->write(anObs);
}

#ifdef MV_BUFRDC_TEST
//____________________________________________________________________ createBuffers
void MvBufrOut::createBuffers()  // XXX still need more dynamic memory allocation
{
    Out_KTDEXL        = -1;
    Out_KELEM         = -1;
    _KDLEN            = 0;
    _nextValue        = 0;
    _nextCharParamPos = 0;

    if (_outState == kBufrOut_noBuffers) {
        char cbuf[120];

        try {
            std::cout << "MvBufrOut::createBuffers, checking for memory..." << std::endl;

            sprintf(cbuf, "requesting %d new ints", MAX_KELEM);
            std::cout << cbuf << std::endl;
            Out_KTDLST = new int[MAX_KELEM];

            sprintf(cbuf, "requesting %d new ints", MAX_KELEM);
            std::cout << cbuf << std::endl;
            Out_KTDEXP = new int[MAX_KELEM];

            sprintf(cbuf, "requesting %d new doubles", MAX_KVALS);
            std::cout << cbuf << std::endl;
            Out_VALUES = new double[MAX_KVALS];

            sprintf(cbuf, "requesting %d new chars", 80 * MAX_KVALS);
            std::cout << cbuf << std::endl;
            Out_CVALS = new char[MAX_KVALS * 80];
        }
        catch (...) {
            delete[] Out_VALUES;  //-- (I)
            delete[] Out_CVALS;   //-- (II)
            delete[] Out_KTDEXP;  //-- (III)

            Out_VALUES = 0;
            Out_CVALS  = 0;
            Out_KTDEXP = 0;
            Out_KTDLST = 0;

            _outState = kBufrOut_error;

            std::cout << ">>>\n>>> MvBufrOut::createBuffers failed in " << cbuf << "\n>>>" << std::endl;
            std::cout << "MvBufrOut::createBuffers: throw MvException..." << std::endl;
#ifdef METVIEW
            marslog(LOG_EROR, "MvBufrOut::createBuffers failed in %s", cbuf);
            throw MvException("MvBufrOut::createBuffers: out-of-memory!");
#else
            throw std::bad_alloc();
#endif
        }
    }

    std::cout << "MvBufrOut::createBuffers memory ok" << std::endl;

    _outState = kBufrOut_emptyBuffers;
}

//____________________________________________________________________ resetBuffers
void MvBufrOut ::resetBuffers(void) {
    Out_KTDEXL        = -1;
    Out_KELEM         = -1;
    _KDLEN            = 0;
    _nextValue        = 0;
    _nextCharParamPos = 0;

    if (_outState > kBufrOut_emptyBuffers) {
        _outState = kBufrOut_emptyBuffers;
    }
}

//____________________________________________________________________ write
void MvBufrOut::write_bufrdc(MvObs& anObs) {
    //-- if no packing into subsets, copy message as is...
    _outSet->write(anObs._bufrIn->fMessage, (int)anObs._bufrIn->fMessageLength);
}

//____________________________________________________________________ addIntoBuffers
void MvBufrOut::addIntoBuffers(MvObs& anObs) {
    if (_outState <= kBufrOut_noBuffers) {
        createBuffers();
    }

    checkDescriptors(anObs);

    if (MAX_KVALS < _nextValue + Out_KTDEXL) {
        encode();
        formatBuffers(anObs);
    }

    //-- in case (non-compressed) multisubset msg, do expanded descriptors here
    Out_KTDEXL = In_KTDEXL;              // expected nr of data values / expanded descriptors
    for (int i = 0; i < In_KTDEXL; i++)  // expanded descriptors
        Out_KTDEXP[i] = In_KTDEXP[i];    // no offset

    Out_KELEM = In_KTDEXL;  // In_KELEM;// expected nr of expanded elements (????)

    int elemIndex = _nextValue;
    for (int i = 0; i < Out_KTDEXL; i++, elemIndex++) {
        Out_VALUES[elemIndex] = In_VALUES[anObs.subsetOffset() + i];

        //-- character data..?
        if (fKSUP[7 - 1] > 0 &&                                  //-- msg contains char data
            (strncmp(In_CUNITS + i * 24, "CCITTIA5", 8) == 0 ||  //-- ECMWF notation
             strncmp(In_CUNITS + i * 24, "CCITT IA5", 9) == 0    //-- WMO & NCEP PrepBUFR notation
             )) {
            //-- copy character string value to next available slot --
            int charInd = (int)Out_VALUES[elemIndex] / 1000 - 1;
            int charLen = (int)Out_VALUES[elemIndex] % 1000;
            strncpy((Out_CVALS + _nextCharParamPos * 80), (In_CVALS + charInd * 80), charLen);

            //-- set "pointer" to this slot --
            Out_VALUES[elemIndex] = 1000 * (_nextCharParamPos + 1) + charLen;
            _nextCharParamPos++;
        }
        else  //-- if delayed replication factor...
        {
            if (isDelayedDescriptor(Out_KTDEXP[i])) {
                // cerr << "isDelayedDescriptor( " << Out_KTDEXP[ i ] << "), _KDLEN=" << _KDLEN << std::endl;
                if (_KDLEN < MAX_KDLEN) {
                    int delayedRepeat = (int)Out_VALUES[elemIndex];
                    if (delayedRepeat < 0) {
                        cerr << ">>> MvBufrOut::add: data error - negative delayed repetition " << delayedRepeat
                             << " (from element " << elemIndex << ")" << std::endl;
#ifdef METVIEW
                        marslog(LOG_EROR, "MvBufrOut::add: data error - negative delayed repetition!");
#endif
                    }
                    _KDATA[_KDLEN] = delayedRepeat;
                    _KDLEN++;
                }
                else {
                    cerr << ">>> MvBufrOut::add: array _KDATA overflow! _KDLEN=" << _KDLEN
                         << ", MAX_KDLEN=" << MAX_KDLEN << std::endl;
#ifdef METVIEW
                    marslog(LOG_EROR, "MvBufrOut::add: array _KDATA overflow!");
#endif
                }
            }
        }
    }
    _nextValue += (int)Out_KELEM;

    fKSEC3[2] += 1;  //-- Nr of Subsets
    _outState = kBufrOut_dataInBuffers;

    if (shouldBeWritten())
        encode();
}
//____________________________________________________________________ formatBuffers
void MvBufrOut ::formatBuffers(const MvObs& anObs) {
    delete _currentSec1;
#if 0
  _currentSec1 = new TSection1;
  *_currentSec1 = *(anObs._bufrIn->Sec1);
#endif
    if (anObs._bufrIn->fSec0->editionNr > 3)
        _currentSec1 = new Section1_Ed4(anObs._bufrIn->Sec1);
    else
        _currentSec1 = new Section1_preEd4(anObs._bufrIn->Sec1);

    int i;

    fKSEC0[0] = 0;
    for (i = 1; i < 3; i++)
        fKSEC0[i] = anObs._bufrIn->fKSEC0[i];

    // fKSEC1[ 0 ] = 0; // ??????????
    for (i = 0; i < 40; i++)
        fKSEC1[i] = anObs._bufrIn->fKSEC1[i];

#if 0
  int wmoi = BUFR_ORIGINATING_CENTER;
  if( getenv( "WMO_SITE_NR" ) )
    wmoi = atoi( getenv( "WMO_SITE_NR" ) );  //-- change WMO Centre Number if given

  if( wmoi < 0 || wmoi > 255 )
    wmoi = BUFR_ORIGINATING_CENTER;          //-- fall back to hard coded default

  fKSEC1[ 2 ] = wmoi;                        //-- WMO Originating Centre
#endif

    fKSEC1[3] += 1;  // increment Update Sequence Number!!
    if (fKSEC1[4] > 127)
        fKSEC1[4] -= 128;  // remove section 2 (which one to copy?)

    fKSEC3[0] = 0;
    for (i = 1; i < 4; i++)
        fKSEC3[i] = anObs._bufrIn->fKSEC3[i];
    fKSEC3[2] = 0;  // reset nr of Subsets

    fKSEC4[0] = 0;
    for (i = 1; i < 2; i++)
        fKSEC4[i] = anObs._bufrIn->fKSEC4[i];

    Out_KTDLEN = In_KTDLEN;          // nr of original data descriptors in Sec 3
    for (i = 0; i < In_KTDLEN; i++)  // packed descriptors
        Out_KTDLST[i] = In_KTDLST[i];

    //-- do this also later because with non-compressed multisubset msgs these vary
    Out_KTDEXL = In_KTDEXL;            // expected nr of data values / expanded descriptors
    for (i = 0; i < In_KTDEXL; i++)    // expanded descriptors
        Out_KTDEXP[i] = In_KTDEXP[i];  // no offset

    Out_KELEM = In_KTDEXL;  // In_KELEM;// expected nr of expanded elements (????)

    _outState = kBufrOut_formatedBuffers;
}
//____________________________________________________________________ encode
void MvBufrOut ::encode(void) {
    if (_outState == kBufrOut_dataInBuffers) {
        int myKERR  = 0;
        int myKBUFL = 0;

        BUFREN(fKSEC0, fKSEC1, fKSEC2, fKSEC3, fKSEC4, &Out_KTDLEN  // &fKTDEXL
               ,
               Out_KTDLST  // fKTDEXP   // &fKTDLEN, fKTDLST
               ,
               &_KDLEN, _KDATA, &Out_KELEM, &Out_KTDEXL  // was: fKVALS
               ,
               Out_VALUES, Out_CVALS, &myKBUFL, (int*)fMessage, &myKERR);

        if (myKERR == 0)
            _outSet->write(fMessage, (int)(myKBUFL * sizeof(int)));
        else {
            cerr << ">>> MvBufrOut::encode, KERR=" << myKERR << std::endl;
#ifdef METVIEW
            marslog(LOG_EROR, "BUFR encoding (BUFREN) failed, status = %d", myKERR);
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
void MvBufrOut ::checkDescriptors(const MvObs& anObs) {
    //--  if( anObs._bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
    //-- expand always, in case non-compressed multisubset msg where exaoanded descriptors vary
    anObs._bufrIn->ExpandDescriptors(anObs.subsetNumber());

    if (_outState == kBufrOut_emptyBuffers)
        formatBuffers(anObs);
    else if (differentDescriptors() || differentHeader(anObs)) {
        encode();
        formatBuffers(anObs);
    }
}

//_______________________________________________________________ differentDescriptors
// returns 1 if descriptors of the BUFR message in 'anObs' differs
// from current descriptors.
// returns 0 if they are equal
//---------------------------------------------------------------
int MvBufrOut ::differentDescriptors(void) const {
    if (_outState < kBufrOut_formatedBuffers)
        return 1;

    if (In_KTDLEN != Out_KTDLEN)
        return 1;

    for (int i = 0; i < In_KTDLEN; i++)
        if (In_KTDLST[i] != Out_KTDLST[i])
            return 1;

    return 0;
}
//_______________________________________________________________ differentHeader
// returns 1 if section 1 header of the BUFR message in 'anObs'
// differs from current header 1.
// returns 0 if they are equal
//---------------------------------------------------------------
int MvBufrOut ::differentHeader(const MvObs& anObs) const {
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

    return _currentSec1->isDifferent(anObs._bufrIn->Sec1);
}
//______________________________________________________________ shouldBeWritten
int MvBufrOut ::shouldBeWritten(void) {
    if (_outState != kBufrOut_dataInBuffers)
        return 0;

    return fKSEC3[2] >= _maxNrSubsets;  // Nr of Subsets
}

//______________________________________________________________ setSubsetCount
void MvBufrOut ::setSubsetCount(int maxNrSubsets) {
    if (_outState == kBufrOut_dataInBuffers)
        encode();
    _maxNrSubsets = maxNrSubsets;
}
//______________________________________________________________ isDelayedDescriptor
bool MvBufrOut ::isDelayedDescriptor(const long aDescr) const {
    //  if( aDescr == 31001 || aDescr == 31002 || aDescr == 31011 || aDescr == 31012 )
    if (aDescr >= 31000 && aDescr <= 31012)
        return true;
    else
        return false;
}

#endif  // MV_BUFRDC_TEST

//--------------------------------------------------------
//  Descriptor mnemonics for class 'MvBufrParam'
//--------------------------------------------------------

#ifndef DOXYGEN_SHOULD_SKIP_THIS
typedef struct {
    const char* name;
    long descriptor;
} descriptorStruct;

static descriptorStruct knownParams[] = {{"z", 10003},  {"p", 10004},     {"ddd", 11001},    {"ff", 11002},
                                         {"u", 11003},  {"v", 11004},     {"w", 11006},      {"T", 12001},
                                         {"Td", 12003}, {"T(2m)", 12004}, {"Td(2m)", 12006}, {"END", 0}};
#endif

// e Remove cPressureCoordinate
const long cPressureCoordinate   = 7004L;       // pressure vertical coord. descriptor value
const string sPressureCoordinate = "pressure";  // pressure vertical coord. descriptor value

//______________________________________________________________________
//====================================================================== MvObs
//______________________________________________________________________
#ifdef MV_BUFRDC_TEST
MvObs::MvObs(MvBufr* b, int subset_current, bool unpacked, codes_handle** ecH) :
    _currentKey(""),
    _currentLevelKey(""),
    _currentLevelOccurrence(0),
    _compressed_data(0),
    _unpacked(unpacked),
    _messageTotalLen(-1),
    _editionNumber(-1),
    _number_of_subsets(-1),
    _messageType(-1),
    _subTypeInternational(-1),
    _subTypeLocal(-1),
    _originatingCentre(-1),
    _originatingCentreStr(""),
    _originatingSubCentre(-1),
    _masterTable(-1),
    _masterTableVersion(-1),
    _localTableVersion(-1),
    _lyear(-1),
    _lmonth(-1),
    _lday(-1),
    _lhour(-1),
    _lminute(-1),
    _edition(0),
    _ecHSS(0),
    _ecIter(0),
    _bufferSS(0) {
    _subsetNr = subset_current;
    _copy(b);
    _ecH = ecH;

    // Initialize variables
    if (_ecH)
        init();

    // e  Remove later when MvBufr is update
    if (b)
        b->setEccodes(ecH);
}
#else
MvObs::MvObs(codes_handle** ecH, int subset_current, bool unpacked, bool useSkipExtraAttributes,
             bool cacheCompressedData) :
    _currentKey(""),
    _currentLevelKey(""),
    _currentLevelOccurrence(0),
    _compressed_data(0),
    _unpacked(unpacked),
    _messageTotalLen(-1),
    _editionNumber(-1),
    _number_of_subsets(-1),
    _messageType(-1),
    _subTypeInternational(-1),
    _subTypeLocal(-1),
    _rdbType(-1),
    _originatingCentre(-1),
    _originatingSubCentre(-1),
    _originatingCentreStr(""),
    _masterTable(-1),
    _masterTableVersion(-1),
    _localTableVersion(-1),
    _lyear(-1),
    _lmonth(-1),
    _lday(-1),
    _lhour(-1),
    _lminute(-1),
    headerIdent_("__UNDEF__"),
    _edition(0),
    useSkipExtraAttributes_(useSkipExtraAttributes),
    cacheCompressedData_(cacheCompressedData),
    _ecHSS(0),
    _ecIter(0),
    _bufferSS(0) {
    _subsetNr = subset_current;
    _ecH      = ecH;

    // Initialize variables
    if (_ecH)
        init();
}
#endif

MvObs::MvObs(const MvObs& obs) {
    _copy(obs);
}

//___________________________________________________________________Destructor
MvObs::~MvObs() {
    clear();
}

#ifdef MV_BUFRDC_TEST
//___________________________________________________________________ _copy
void MvObs::_copy(MvBufr* b) {
    _bufrIn  = b;
    _bufr_id = 0;
    if (_bufrIn) {
        _bufrIn->attach();
        _bufr_id = _bufrIn->currentBufrRef();
    }
    _currentLevelCoordinate1 = cPressureCoordinate;
    _currentLevelIndex1      = -1;
    _confidence              = new MvBufrConfidence(_bufrIn, _subsetNr);
}
#endif

//___________________________________________________________________ _copy
void MvObs::_copy(const MvObs& b) {
#ifdef MV_BUFRDC_TEST
    _currentLevelIndex1      = b._currentLevelIndex1;
    _currentLevelCoordinate1 = b._currentLevelCoordinate1;
    _lastSpecifierIndex1     = b._lastSpecifierIndex1;
    _copy(b._bufrIn);
    _bufr_id = b._bufr_id;
#endif

    _subsetNr               = b._subsetNr;
    _compressed_data        = b._compressed_data;
    _unpacked               = b._unpacked;
    _messageTotalLen        = b._messageTotalLen;
    _currentLevelOccurrence = b._currentLevelOccurrence;
    _currentLevelKey        = b._currentLevelKey;
    _currentKey             = b._currentKey;
    _editionNumber          = b._editionNumber;
    _number_of_subsets      = b._number_of_subsets;
    _messageType            = b._messageType;
    _subTypeInternational   = b._subTypeInternational;
    _subTypeLocal           = b._subTypeLocal;
    _rdbType                = b._rdbType;
    _originatingCentre      = b._originatingCentre;
    _originatingCentreStr   = b._originatingCentreStr;
    _originatingSubCentre   = b._originatingSubCentre;
    _masterTable            = b._masterTable;
    _masterTableVersion     = b._masterTableVersion;
    _localTableVersion      = b._localTableVersion;
    _lyear                  = b._lyear;
    _lmonth                 = b._lmonth;
    _lday                   = b._lday;
    _lhour                  = b._lhour;
    _lminute                = b._lminute;
    headerIdent_            = b.headerIdent_;
    _edition                = b._edition;
    useSkipExtraAttributes_ = b.useSkipExtraAttributes_;
    cacheCompressedData_    = b.cacheCompressedData_;
    _ecH                    = b._ecH;
    _ecIter                 = 0;
    _ecHSS                  = 0;
    _bufferSS               = 0;

    if (cacheCompressedData_)
        compressedData_ = b.compressedData_;
}

//___________________________________________________________________ clear
void MvObs::clear() {
#ifdef MV_BUFRDC_TEST
    if (_bufrIn)
        _bufrIn->detach();

    _bufrIn  = NULL;
    _bufr_id = 0;
    delete _confidence;  // e Is it needed???
    _confidence = 0;     // e Is it needed???
#endif

    // Delete iterator
    if (_ecH && *_ecH && _ecIter) {
        codes_bufr_keys_iterator_delete(_ecIter);
        _ecIter = 0;
    }

    // Delete handle/iterator/buffer related to a subset
    if (_bufferSS)
        _bufferSS = 0;

    if (_ecH && *_ecH && _ecHSS) {
        codes_handle_delete(_ecHSS);
        _ecHSS = 0;
    }

    // Bufr handler is just a pointer, it shoul be deleted somewhere else.
    // Just move the pointer to NULL
    if (_ecH)
        _ecH = 0;
}

//___________________________________________________________________ operator=
MvObs& MvObs::operator=(const MvObs& b) {
    clear();
    _copy(b);
    return *this;
}

//___________________________________________________________________ operator void*
MvObs::operator void*() {
    return _ecH ? *_ecH : NULL;

    // e   return _bufrIn;
}

//___________________________________________________________________ operator!
bool MvObs::operator!() {
    return !(_ecH && *_ecH);

    // e   return !_bufrIn;
}

//___________________________________________________________________ msg_ok
//
bool MvObs::msg_ok() const {
    return (_ecH && *_ecH) ? true : false;

#if 0  // e bufrdc code
if (MV_BUFRDC_TEST)
{
  if( _bufr_id != _bufrIn->currentBufrRef() )
  {
    //-- restriction due to static bufr arrays --
    cerr << ">>> [MvObs::msg_ok] Static bufr arrays rewritten, not valid any more!" << std::endl;
    return false;
  }
  return ( _bufrIn && ( _bufrIn->_inState != kBufrIn_Error ) ) ? true : false;
}
#endif
}

//___________________________________________________________________ Advance
bool MvObs::Advance() {
    _subsetNr++;

#ifdef MV_BUFRDC_TEST
    _bufrIn->setSubset(_subsetNr);
    // ec return  _subsetNr <= _bufrIn->subsetCount();
#endif

    return _subsetNr <= _number_of_subsets;
}

//____________________________________________________________________ operator[]
double MvObs::operator[](int index)  //-- index starts from 1: 1,2,...,n
{
    std::cout << "MvObs::operator[] -> not yet implemented" << std::endl;
//   exit(0);

// ec check _currentKey value
#ifdef MV_BUFRDC_TEST
    return (double)(_bufrIn->DataValue(index - 1, _subsetNr));
#endif
    return kBufrMissingValue;
}

//____________________________________________________________________ hasSection2
bool MvObs::hasSection2() {
    long val = intValue("section2Present");
    return val ? true : false;
}

//____________________________________________________________________ value
// returns 'kBufrMissingValue' if not found!
//-------------------------------------------------
double MvObs::valueC(const std::string& aDescriptor) {
    // Check only positive integer values; otherwise, use "-.0123456789"
    std::string skey;
    if (strspn(aDescriptor.c_str(), "0123456789") == aDescriptor.size())
        skey = key(atol(aDescriptor.c_str()));
    else
        skey = aDescriptor;

    return value(skey);
}

// Parameter occurrence must start from 1
double MvObs::value(long aDescriptor, int occurrence) {
    // Build key and get value
    string skey    = this->key(aDescriptor, occurrence);
    double myValue = value(skey);

#ifdef MV_BUFRDC_TEST
    double myValue1;
    _bufrIn->Value(aDescriptor, _subsetNr, myValue1);
    TEMPCHECKVALUEDOUBLE(myValue, myValue1, skey, aDescriptor);
#endif

    return myValue;
}

double MvObs::value(long aDescriptor) {
    // Build key and get value
    string skey    = this->key(aDescriptor);
    double myValue = value(skey);

#ifdef MV_BUFRDC_TEST
    double myValue1;
    _bufrIn->Value(aDescriptor, _subsetNr, myValue1);
    TEMPCHECKVALUEDOUBLE(myValue, myValue1, skey, aDescriptor);
#endif

    return myValue;
}

double MvObs::value(const string& key, const int occurrence) {
    // Build key and get value
    string skey = this->key(key, occurrence);
    return value(skey);
}

double MvObs::value(const string& skey) {
    // Check input key
    if (skey.empty())
        return kBufrMissingValue;

    // Get number of elements
    size_t nelems;
    codes_get_size(*_ecH, skey.c_str(), &nelems);

    // No elements found
    if (nelems == 0)
        return kBufrMissingValue;

    // There is only one element
    double dvalue = CODES_MISSING_DOUBLE;
    if (nelems == 1) {
        codes_get_double(*_ecH, skey.c_str(), &dvalue);
        return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
    }

    // It is an array of elements
    // FII 20170922: update this code when function codes_get_double_element can
    // handle uncompressed data.
    if (_compressed_data) {
        // codes_get_double_element(*_ecH, skey.c_str(), _subsetNr-1, &dvalue);

        // Always use a hashtag because the array size will be smaller. Two possibilities:
        // a) number_of_subsets instead of number_of_subsets*number_of_occurrences
        // b) 1 element which means all the subsets have the same value
        // By default we retrieve a value from the first occurrence.
        string sskey = (skey[0] != '#') ? "#1#" + skey : skey;

        // We try to use the cached compressed values
        if (cacheCompressedData_) {
            const std::vector<double>& chData = compressedData_.doubleData(sskey);
            if (!chData.empty()) {
                // vector
                if (static_cast<int>(chData.size()) == _number_of_subsets) {
                    dvalue = chData[_subsetNr - 1];
                }
                else if (chData.size() == 1) {
                    dvalue = chData[0];
                }
                return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
            }
        }

        codes_get_size(*_ecH, sskey.c_str(), &nelems);
        if (nelems == 1)  // get the unique element
        {
            codes_get_double(*_ecH, sskey.c_str(), &dvalue);
            if (cacheCompressedData_) {
                compressedData_.addDoubleData(sskey, dvalue);
            }
            return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
        }

        // retrieve the element related to the current subset number
        double* v1 = new double[nelems];
        codes_get_double_array(*_ecH, sskey.c_str(), v1, &nelems);
        if (cacheCompressedData_) {
            compressedData_.addDoubleData(sskey, v1, nelems);
        }
        dvalue = v1[_subsetNr - 1];
        delete[] v1;
        v1 = 0;
    }
    else  // uncompressed data
    {
        string sskey;
        if (skey[0] == '/')
            sskey = skey;
        else {
            // add key subsetNumber
            std::ostringstream sstream;
            sstream << _subsetNr << "/";
            sskey = "/subsetNumber=" + sstream.str() + skey;
            size_t nn;
            codes_get_size(*_ecH, sskey.c_str(), &nn);
            if (nn == 0)
                sskey = skey;  // retrieve using the original key
            else if (nn == 1) {
                codes_get_double(*_ecH, sskey.c_str(), &dvalue);
                return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
            }
            else
                nelems = nn;
        }

        double* v1 = new double[nelems];
        codes_get_double_array(*_ecH, sskey.c_str(), v1, &nelems);
        dvalue = v1[0];  // first occurrence
        delete[] v1;
        v1 = 0;
        ;
    }

    return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
}

//___________________________________________________________ valueByOccurrence
double MvObs::valueByOccurrenceC(int anOccurrenceIndex, const std::string& aDescriptor) {
    // Check only positive integer values; otherwise, use "-.0123456789"
    std::string skey;
    if (strspn(aDescriptor.c_str(), "0123456789") == aDescriptor.size())
        skey = key(atol(aDescriptor.c_str()), anOccurrenceIndex);
    else
        skey = key(aDescriptor, anOccurrenceIndex);

    return value(skey);
}

double MvObs::valueByOccurrence(int anOccurrenceIndex, const std::string& aDescriptor) {
    // Build key and get value
    string skey = key(aDescriptor, anOccurrenceIndex);
    return value(skey);
}

double MvObs::valueByOccurrence(int anOccurrenceIndex, long aDescriptor) {
    // Build key and get value
    string skey    = this->key(aDescriptor, anOccurrenceIndex);
    double myValue = value(skey);

#ifdef MV_BUFRDC_TEST
    double myValue1 = value(aDescriptor);
    for (int myInd = 1; myInd < anOccurrenceIndex; myInd++)
        // e myValue = nextValue();
        _bufrIn->Value(_bufrIn->_currentDescr, _subsetNr, myValue1, _bufrIn->_currentDescrInd + 1);
    TEMPCHECKVALUEDOUBLE(myValue, myValue1, skey, _bufrIn->_currentDescr);
    // e return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
#endif

    return myValue;
}

// Returns all values of a given key from a message/subset. The key is either a simple string
// e.g. "airTemperature" or a containing the rank e.g. "#1#airTemperature"
void MvObs::allValues(const string& keyName, std::vector<double>& vals) {
    // Check input key
    if (keyName.empty())
        return;

    // Get number of elements
    size_t valLen;
    codes_get_size(*_ecH, keyName.c_str(), &valLen);

    // No elements found
    if (valLen == 0)
        return;

    // There is only one element
    double val = CODES_MISSING_DOUBLE;
    if (valLen == 1) {
        codes_get_double(*_ecH, keyName.c_str(), &val);
        vals.push_back((val == CODES_MISSING_DOUBLE) ? kBufrMissingValue : val);
        return;
    }

    double* valArr   = 0;
    size_t valArrNum = 0;

    // It is an array of elements
    if (_compressed_data) {
        int maxRank = 1000000;  // we do not know how many ranks we have!
        int ir      = 1;
        int rank    = occurenceFromKey(keyName);

        // we read a sing rank only
        if (rank >= 1) {
            ir      = rank;
            maxRank = ir + 1;
        }

        // loop for the ranks
        while (ir < maxRank) {
            valLen               = 0;
            std::string rKeyName = keyName;
            if (rank < 1)
                rKeyName = "#" + toString(ir) + "#" + keyName;

            // We try to use the cached compressed values
            bool hasCache = false;
            if (cacheCompressedData_) {
                const std::vector<long>& chData = compressedData_.longData(rKeyName);
                if (!chData.empty()) {
                    // vector
                    if (static_cast<int>(chData.size()) == _number_of_subsets) {
                        val = chData[_subsetNr - 1];
                    }
                    else if (chData.size() == 1) {
                        val = chData[0];
                    }

                    vals.push_back((val == CODES_MISSING_DOUBLE) ? kBufrMissingValue : val);
                    hasCache = true;
                }
            }

            if (!hasCache) {
                codes_get_size(*_ecH, rKeyName.c_str(), &valLen);

                if (valLen == 0)
                    break;

                // Single value
                if (valLen == 1) {
                    codes_get_double(*_ecH, rKeyName.c_str(), &val);
                    if (cacheCompressedData_) {
                        compressedData_.addDoubleData(rKeyName, val);
                    }
                    vals.push_back((val == CODES_MISSING_DOUBLE) ? kBufrMissingValue : val);
                }
                // Array
                else if (_subsetNr <= static_cast<int>(valLen)) {
                    if (valArrNum < valLen) {
                        delete[] valArr;
                        valArr    = new double[valLen];
                        valArrNum = valLen;
                    }
                    assert(valArr);
                    codes_get_double_array(*_ecH, rKeyName.c_str(), valArr, &valLen);
                    assert(_subsetNr <= static_cast<int>(valLen));
                    val = valArr[_subsetNr - 1];
                    if (cacheCompressedData_) {
                        compressedData_.addDoubleData(rKeyName, valArr, valLen);
                    }
                    vals.push_back((val == CODES_MISSING_DOUBLE) ? kBufrMissingValue : val);
                }
            }

            ir++;
        }
    }

    else  // uncompressed data
    {
        valLen               = 0;
        std::string rKeyName = "/subsetNumber=" + toString(_subsetNr) + "/" + keyName;

        codes_get_size(*_ecH, rKeyName.c_str(), &valLen);
        assert(!valArr);
        if (valLen == 1) {
            codes_get_double(*_ecH, rKeyName.c_str(), &val);
            vals.push_back((val == CODES_MISSING_DOUBLE) ? kBufrMissingValue : val);
        }
        // Array
        else {
            assert(!valArr);
            valArr    = new double[valLen];
            valArrNum = valLen;
            codes_get_double_array(*_ecH, rKeyName.c_str(), valArr, &valLen);
            for (size_t i = 0; i < valLen; i++)
                vals.push_back((valArr[i] == CODES_MISSING_DOUBLE) ? kBufrMissingValue : valArr[i]);
        }
    }

    if (valArr) {
        assert(valArrNum > 0);
        delete[] valArr;
    }
}

//____________________________________________________________________ intValue

long MvObs::currentIntValue() {
    return intValue(_currentKey);
}

long MvObs::intValue(const long aDescriptor, const int occurrence) {
    // Build key and get value
    string skey  = this->key(aDescriptor, occurrence);
    long myValue = intValue(skey);

#ifdef MV_BUFRDC_TEST
    // e   return msg_ok() ? _bufrIn->intValue( aDescriptor, _subsetNr ) : kFortranBufrMissingIntValue;
    long myValue1 = _bufrIn->intValue(aDescriptor, _subsetNr);
    TEMPCHECKVALUELONG(myValue, myValue1);
#endif

    return myValue;
}

long MvObs::intValue(const long aDescriptor) {
    // Build key and get value
    string skey  = this->key(aDescriptor);
    long myValue = intValue(skey);

#ifdef MV_BUFRDC_TEST
    // e   return msg_ok() ? _bufrIn->intValue( aDescriptor, _subsetNr ) : kFortranBufrMissingIntValue;
    long myValue1 = _bufrIn->intValue(aDescriptor, _subsetNr);
    TEMPCHECKVALUELONG(myValue, myValue1);
#endif

    return myValue;
}

long MvObs::intValue(const string& key, const int occurrence) {
    // Build key and get value
    string skey = this->key(key, occurrence);
    return intValue(skey);
}

long MvObs::intValue(const string& skey) {
    // Check input key
    if (skey.empty())
        return kBufrMissingIntValue;

    // Get number of elements
    size_t nelems;
    codes_get_size(*_ecH, skey.c_str(), &nelems);

    // No elements found
    if (nelems == 0)
        return kBufrMissingIntValue;

    // There is only one element
    long value = CODES_MISSING_LONG;
    if (nelems == 1) {
        codes_get_long(*_ecH, skey.c_str(), &value);
        return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
    }

    // It is an array of elements
    // FII 20170922: update this code when function codes_get_double_element can
    // handle uncompressed data.
    if (_compressed_data) {
        // Always use a hashtag because the array size will be smaller. Two possibilities:
        // a) number_of_subsets instead of number_of_subsets*number_of_occurrences
        // b) 1 element which means all the subsets have the same value
        // By default we retrieve a value from the first occurrence.
        string sskey = (skey[0] != '#') ? "#1#" + skey : skey;

        // We try to use the cached compressed values
        if (cacheCompressedData_) {
            const std::vector<long>& chData = compressedData_.longData(sskey);
            if (!chData.empty()) {
                // vector
                if (static_cast<int>(chData.size()) == _number_of_subsets) {
                    value = chData[_subsetNr - 1];
                }
                else if (chData.size() == 1) {
                    value = chData[0];
                }
                return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
            }
        }

        // read the data values
        codes_get_size(*_ecH, sskey.c_str(), &nelems);
        if (nelems == 1)  // get the unique element
        {
            codes_get_long(*_ecH, sskey.c_str(), &value);
            if (cacheCompressedData_) {
                compressedData_.addLongData(sskey, value);  // add to cache
            }
            return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
        }

        // retrieve the element related to the current subset number
        long* v1 = new long[nelems];
        codes_get_long_array(*_ecH, sskey.c_str(), v1, &nelems);
        value = v1[_subsetNr - 1];
        if (cacheCompressedData_) {
            compressedData_.addLongData(sskey, v1, nelems);  // add to cache
        }
        delete[] v1;
        v1 = 0;
    }
    else  // uncompressed data
    {
        string sskey;
        if (skey[0] == '/')
            sskey = skey;
        else {
            // add key subsetNumber
            std::ostringstream sstream;
            sstream << _subsetNr << "/";
            sskey = "/subsetNumber=" + sstream.str() + skey;
            size_t nn;
            codes_get_size(*_ecH, sskey.c_str(), &nn);
            if (nn == 0)
                sskey = skey;  // retrieve using the original key
            else if (nn == 1) {
                codes_get_long(*_ecH, sskey.c_str(), &value);
                return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
            }
            else
                nelems = nn;
        }

        long* v1 = new long[nelems];
        codes_get_long_array(*_ecH, sskey.c_str(), v1, &nelems);
        value = v1[0];  // first occurrence
        delete[] v1;
        v1 = 0;
    }

    return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
}

// Returns all values of a given key from a message/subset. The key is either a simple string
// e.g. "airTemperature" or one containing the rank e.g. "#1#airTemperature"
void MvObs::allIntValues(const string& keyName, std::vector<long>& vals) {
    // Check input key
    if (keyName.empty())
        return;

    // Get number of elements
    size_t valLen;
    codes_get_size(*_ecH, keyName.c_str(), &valLen);

    // No elements found
    if (valLen == 0)
        return;

    // There is only one element
    long val = CODES_MISSING_LONG;
    if (valLen == 1) {
        codes_get_long(*_ecH, keyName.c_str(), &val);
        vals.push_back((val == CODES_MISSING_LONG) ? kBufrMissingIntValue : val);
        return;
    }

    long* valArr     = 0;
    size_t valArrNum = 0;

    // It is an array of elements
    if (_compressed_data) {
        int maxRank = 1000000;  // we do not know how many ranks we have!
        int ir      = 1;
        int rank    = occurenceFromKey(keyName);

        // we read a single rank only
        if (rank >= 1) {
            ir      = rank;
            maxRank = ir + 1;
        }

        // loop for the ranks
        while (ir < maxRank) {
            valLen               = 0;
            std::string rKeyName = keyName;
            if (rank < 1)
                rKeyName = "#" + toString(ir) + "#" + keyName;

            // We try to use the cached compressed values
            bool hasCache = false;
            if (cacheCompressedData_) {
                const std::vector<long>& chData = compressedData_.longData(rKeyName);
                if (!chData.empty()) {
                    // vector
                    if (static_cast<int>(chData.size()) == _number_of_subsets) {
                        val = chData[_subsetNr - 1];
                    }
                    else if (chData.size() == 1) {
                        val = chData[0];
                    }

                    vals.push_back((val == CODES_MISSING_LONG) ? kBufrMissingIntValue : val);
                    hasCache = true;
                }
            }

            if (!hasCache) {
                codes_get_size(*_ecH, rKeyName.c_str(), &valLen);

                if (valLen == 0)
                    break;

                // Single value
                if (valLen == 1) {
                    codes_get_long(*_ecH, rKeyName.c_str(), &val);
                    if (cacheCompressedData_) {
                        compressedData_.addLongData(rKeyName, val);
                    }
                    vals.push_back((val == CODES_MISSING_LONG) ? kBufrMissingIntValue : val);
                }
                // Array
                else if (_subsetNr <= static_cast<int>(valLen)) {
                    if (valArrNum < valLen) {
                        delete[] valArr;
                        valArr    = new long[valLen];
                        valArrNum = valLen;
                    }
                    assert(valArr);
                    codes_get_long_array(*_ecH, rKeyName.c_str(), valArr, &valLen);
                    assert(_subsetNr <= static_cast<int>(valLen));
                    val = valArr[_subsetNr - 1];
                    if (cacheCompressedData_) {
                        compressedData_.addLongData(rKeyName, valArr, valLen);
                    }
                    vals.push_back((val == CODES_MISSING_LONG) ? kBufrMissingIntValue : val);
                }
            }
            ir++;
        }
    }

    else  // uncompressed data
    {
        valLen               = 0;
        std::string rKeyName = "/subsetNumber=" + toString(_subsetNr) + "/" + keyName;

        codes_get_size(*_ecH, rKeyName.c_str(), &valLen);
        assert(!valArr);
        if (valLen == 1) {
            codes_get_long(*_ecH, rKeyName.c_str(), &val);
            vals.push_back((val == CODES_MISSING_LONG) ? kBufrMissingIntValue : val);
        }
        // Array
        else {
            assert(!valArr);
            valArr    = new long[valLen];
            valArrNum = valLen;
            codes_get_long_array(*_ecH, rKeyName.c_str(), valArr, &valLen);
            for (size_t i = 0; i < valLen; i++)
                vals.push_back((valArr[i] == CODES_MISSING_LONG) ? kBufrMissingIntValue : valArr[i]);
        }
    }

    if (valArr) {
        assert(valArrNum > 0);
        delete[] valArr;
    }
}

//_________________________________________________________________ stringValue
string MvObs::stringValue(const long aDescriptor, const int occurrence) {
    // Build key and get value
    string skey    = this->key(aDescriptor, occurrence);
    string myValue = stringValue(skey);

#ifdef MV_BUFRDC_TEST
    string myValue1 = _bufrIn->stringValue(aDescriptor, _subsetNr);
    TEMPCHECKVALUESTRING(myValue, myValue1);
#endif

    return myValue;
}

string MvObs::stringValue(const long aDescriptor) {
    // Build key and get value
    string skey    = this->key(aDescriptor);
    string myValue = stringValue(skey);

#ifdef MV_BUFRDC_TEST
    string myValue1 = _bufrIn->stringValue(aDescriptor, _subsetNr);
    TEMPCHECKVALUESTRING(myValue, myValue1);
#endif

    return myValue;
}

string MvObs::stringValue(const string& key, const int occurrence) {
    // Build key and get value
    string skey = this->key(key, occurrence);
    return stringValue(skey);
}

string MvObs::stringValue(const string& skey) {
    // Check input key
    if (skey.empty())
        return string("");

    // Get number of elements
    size_t nelems;
    codes_get_size(*_ecH, skey.c_str(), &nelems);

    // No elements found
    if (nelems == 0)
        return string("");

    // There is only one element
    char buf[1024];
    size_t len = 1024;
    if (nelems == 1) {
        codes_get_string(*_ecH, skey.c_str(), buf, &len);
        // buf[len] = 0;  //???
        return string(buf);
    }

    // It is an array of elements
    size_t isize   = 128;  //????
    char** cValues = NULL;
    if (_compressed_data) {
        // Always use a hashtag because the array size will be smaller. Two possibilities:
        // a) number_of_subsets instead of number_of_subsets*number_of_occurrences
        // b) 1 element which means all the subsets have the same value
        // By default we retrieve a value from the first occurrence.
        string sskey = (skey[0] != '#') ? "#1#" + skey : skey;
        codes_get_size(*_ecH, sskey.c_str(), &nelems);
        if (nelems == 1)  // get the unique element
        {
            codes_get_string(*_ecH, sskey.c_str(), buf, &len);
            // buf[len] = 0;  //???
            return string(buf);
        }

        // There are number_of_subsets values
        cValues = new char*[nelems];
        for (unsigned int i = 0; i < nelems; ++i)
            cValues[i] = new char[isize];

        // Get all values and select the required one
        size_t itotal = isize * nelems;
        codes_get_string_array(*_ecH, sskey.c_str(), cValues, &itotal);
        // for(int i=0; i<nelems; ++i)
        // printf("string[%d]=%s\n", i, cValues[i]);

        strcpy(buf, cValues[_subsetNr - 1]);
    }
    else  // uncompressed data
    {
        string sskey;
        if (skey[0] == '/')
            sskey = skey;
        else {
            // add key subsetNumber
            std::ostringstream sstream;
            sstream << _subsetNr << "/";
            sskey = "/subsetNumber=" + sstream.str() + skey;
            size_t nn;
            codes_get_size(*_ecH, sskey.c_str(), &nn);
            if (nn == 0)
                sskey = skey;  // retrieve using the original key
            else if (nn == 1) {
                codes_get_string(*_ecH, sskey.c_str(), buf, &len);
                // buf[len] = 0;  //???
                return string(buf);
            }
            else
                nelems = nn;
        }

        cValues = new char*[nelems];
        for (unsigned int i = 0; i < nelems; ++i)
            cValues[i] = new char[isize];

        size_t itotal = isize * nelems;
        codes_get_string_array(*_ecH, sskey.c_str(), cValues, &itotal);
        strcpy(buf, cValues[0]);  // first occurrence
    }

    // Delete auxiliary buffer
    for (unsigned int i = 0; i < nelems; ++i)
        delete cValues[i];
    delete[] cValues;
    cValues = 0;

    // buf[len] = 0;  //???
    return string(buf);
}

// It should only be used through the iterator
string MvObs::stringValue() {
    string myValue = stringValue(_currentKey);

#ifdef MV_BUFRDC_TEST
    // e return _bufrIn->stringValue( _subsetNr );
    string myValue1 = _bufrIn->stringValue(_subsetNr);
    TEMPCHECKVALUESTRING(myValue, myValue1);
#endif

    return myValue;
}


// Returns all values of a given key from a message/subset. The key is either a simple string
// e.g. "airTemperature" or a containing the rank e.g. "#1#airTemperature"
void MvObs::allStringValues(const std::string& keyName, std::vector<std::string>& vals) {
    // Check input key
    if (keyName.empty())
        return;

    // Get number of elements
    size_t valLen;
    codes_get_size(*_ecH, keyName.c_str(), &valLen);

    // No elements found
    if (valLen == 0)
        return;

    // There is only one element
    std::string val;
    char buf[1024];
    std::size_t sLen = 1024;
    if (valLen == 1) {
        codes_get_string(*_ecH, keyName.c_str(), buf, &sLen);
        vals.push_back(std::string(buf));
        return;
    }

    char** valArr    = 0;
    size_t valArrNum = 0;
    size_t sLenArr   = 128;  // the maximum string size we handle

    // It is an array of elements
    if (_compressed_data) {
        int maxRank = 1000000;  // we do not know how many ranks we have!
        int ir      = 1;
        int rank    = occurenceFromKey(keyName);

        // we read a sing rank only
        if (rank >= 1) {
            ir      = rank;
            maxRank = ir + 1;
        }

        // loop for the ranks
        while (ir < maxRank) {
            valLen               = 0;
            std::string rKeyName = keyName;
            if (rank < 1)
                rKeyName = "#" + toString(ir) + "#" + keyName;

            codes_get_size(*_ecH, rKeyName.c_str(), &valLen);

            if (valLen == 0)
                break;

            // Single value
            if (valLen == 1) {
                codes_get_string(*_ecH, rKeyName.c_str(), buf, &sLen);
                vals.push_back(std::string(buf));
            }
            // Array
            else if (_subsetNr <= static_cast<int>(valLen)) {
                if (valArrNum < valLen) {
                    for (std::size_t i = 0; i < valArrNum; ++i)
                        delete[] valArr[i];
                    delete[] valArr;

                    valArr = new char*[valLen];
                    for (std::size_t i = 0; i < valLen; ++i)
                        valArr[i] = new char[sLenArr];

                    valArrNum = valLen;
                }

                assert(valArr);
                std::size_t sTotal = valLen * sLenArr;
                codes_get_string_array(*_ecH, rKeyName.c_str(), valArr, &sTotal);
                assert(_subsetNr <= static_cast<int>(valLen));
                val = std::string(valArr[_subsetNr - 1]);
            }
            ir++;
        }
    }

    else  // uncompressed data
    {
        valLen               = 0;
        std::string rKeyName = "/subsetNumber=" + toString(_subsetNr) + "/" + keyName;

        codes_get_size(*_ecH, rKeyName.c_str(), &valLen);
        assert(!valArr);
        if (valLen == 1) {
            codes_get_string(*_ecH, rKeyName.c_str(), buf, &sLen);
            vals.push_back(std::string(buf));
        }
        // Array
        else {
            assert(!valArr);
            valArr = new char*[valLen];
            for (std::size_t i = 0; i < valLen; ++i)
                valArr[i] = new char[sLenArr];

            valArrNum          = valLen;
            std::size_t sTotal = valLen * sLenArr;
            codes_get_string_array(*_ecH, rKeyName.c_str(), valArr, &sTotal);
            for (size_t i = 0; i < valLen; i++)
                vals.push_back(std::string(valArr[i]));
        }
    }

    if (valArr) {
        assert(valArrNum > 0);
        for (std::size_t i = 0; i < valArrNum; ++i)
            delete[] valArr[i];
        delete[] valArr;
    }
}


bool MvObs::setFirstDescriptor(bool skipConfidence) {
    // Set Confidence values flag
    _skipConfidence = skipConfidence;

    // Delete previous iterator
    if (_ecIter) {
        codes_bufr_keys_iterator_delete(_ecIter);
        _ecIter = 0;
    }

    // Data needs to be unpacked
    if (!_unpacked) {
        if (useSkipExtraAttributes_) {
            codes_set_long(*_ecH, "skipExtraKeyAttributes", 1);
        }
        codes_set_long(*_ecH, "unpack", 1);
        _unpacked = true;
    }

    // Initialise iterator
    _ecIter = codes_bufr_data_section_keys_iterator_new(*_ecH);
    if (!_ecIter) {
        std::cout << "ERROR MvObs::setFirstDescriptor() -> Unable to create BUFR keys iterator" << std::endl;
        return false;
    }

    // Set first key/descriptor
    if (!setNextDescriptor())
        return false;

#ifdef MV_BUFRDC_TEST
    // e return _bufrIn->SetFirstDescriptor();
    _bufrIn->SetFirstDescriptor();
#endif

    return true;
}

bool MvObs::setNextDescriptor() {
    // Advance iterator
    if (!codes_bufr_keys_iterator_next(_ecIter)) {
        codes_bufr_keys_iterator_delete(_ecIter);
        _ecIter = 0;
        return false;
    }

    // Get the key's name
    bool flag = true;
    if (_skipConfidence) {
        while (flag) {
            _currentKey = codes_bufr_keys_iterator_get_name(_ecIter);
            if (_currentKey.find("->") == std::string::npos)
                break;

            flag = codes_bufr_keys_iterator_next(_ecIter);
        }
    }
    else
        _currentKey = codes_bufr_keys_iterator_get_name(_ecIter);

#ifdef MV_BUFRDC_TEST
    // e return _bufrIn->SetNextDescriptor();
    _bufrIn->SetNextDescriptor();
#endif

    if (!flag) {
        codes_bufr_keys_iterator_delete(_ecIter);
        _ecIter = 0;
    }

    return flag;
}

void MvObs::clearIterator() {
    if (_ecIter) {
        codes_bufr_keys_iterator_delete(_ecIter);
        _ecIter = 0;
    }
}

void MvObs::expand() {
    if (!_unpacked && _ecH && *_ecH) {
        if (useSkipExtraAttributes_) {
            codes_set_long(*_ecH, "skipExtraKeyAttributes", 1);
        }
        codes_set_long(*_ecH, "unpack", 1);
        _unpacked = true;
    }
}

long MvObs::currentDescriptor() {
    string skey     = _currentKey + "->code";
    long descriptor = intValue(skey);

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->CurrentDescriptor();
//   TEMPCHECKVALUELONG(descriptor,vold);
#endif

    return descriptor;
}

const std::string& MvObs::currentKey() {
#ifdef MV_BUFRDC_TEST
    long vold   = _bufrIn->CurrentDescriptor();
    string skey = key(vold);
    // remove occurrence tag, if exists
    std::size_t ipos = 0;
    if (_currentKey[0] == '#')
        ipos = _currentKey.find('#', 1);

    if (_currentKey.substr(ipos + 1) != skey)
        std::cout << "currentKey() : SHOULD HAVE THE SAME KEY NAME: " << _currentKey.substr(ipos + 1).c_str() << " "
                  << skey.c_str() << std::endl;
    TEMPCHECKVALUESTRING(_currentKey, skey);
#endif

    return _currentKey;
}

const std::string MvObs::currentKeyWithoutRank() {
    // No occurrence tag
    if (_currentKey[0] != '#')
        return _currentKey;

    // Remove occurrence tag
    std::size_t ipos = _currentKey.find('#', 1);
    return _currentKey.substr(ipos + 1);
}

// It should only be used through the iterator
double MvObs::currentValue() {
    double myValue = value(_currentKey);

#ifdef MV_BUFRDC_TEST
    double myValue1 = _bufrIn->CurrentValue(_subsetNr);
    TEMPCHECKVALUEDOUBLE(myValue, myValue1, _currentKey, _bufrIn->CurrentDescriptor());
#endif

    return myValue;
}

double MvObs ::nextValue() {
    std::cout << "MvObs :: nextValue() -> not yet implemented" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    double myValue;
    _bufrIn->Value(_bufrIn->_currentDescr, _subsetNr, myValue, _bufrIn->_currentDescrInd + 1);
    return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
#endif
}

MvObs MvObs::cloneSubset(long subset_number) {
    // Check if the input subset number is valid
    if (subset_number > msgSubsetCount()) {
        std::cout << "ERROR MvObs::cloneSubset() -> invalid input subset number" << std::endl;
        return MvObs(NULL);
    }

    if (_ecHSS) {
        codes_handle_delete(_ecHSS);
        _ecHSS    = 0;
        _bufferSS = 0;
    }

    // Clone, unpack and extract that particular subset
    // h2 is a temporary handle; it will be deleted at the end of this function
    codes_handle* h2 = codes_handle_clone(*_ecH);
    assert(h2);
    //   codes_set_long(h2,"skipExtraKeyAttributes",1);  //ECC-741
    codes_set_long(h2, "unpack", 1);
    codes_set_long(h2, "extractSubset", subset_number);
    codes_set_long(h2, "doExtractSubsets", 1);

    // Put result into buffer then form new handle from it
    size_t size = 0;
    codes_get_message(h2, &_bufferSS, &size);
    _ecHSS = codes_handle_new_from_message_copy(0, _bufferSS, size);
    assert(_ecHSS);
    codes_set_long(_ecHSS, "unpack", 1);

    // Delete the temporary codes handle
    codes_handle_delete(h2);
    h2 = 0;

    return MvObs(&_ecHSS, 1);
}

//__________________________________________________________________elementValueType
// IMPORTANT: this function is not backwards compatible with BUFRDC.
// BUFRDC returns kEVT_missing if the value is a missing value.
// ecCodes always returns the Type of the element: CODES_TYPE_LONG,
// CODES_TYPE_DOUBLE or CODES_TYPE_STRING. It does not check if the
// value is a missing value or not.
int MvObs::elementValueType(long aDescriptor) {
    // Build key and get type of the value
    string skey = this->key(aDescriptor);
    int itype   = elementValueType(skey);

#ifdef MV_BUFRDC_TEST
    int vold = _bufrIn->elementValueType(aDescriptor, _subsetNr);
    if (vold == kEVT_missing) {
        // printf("IMPORTANT: MvObs::elementValueType -> values are different but it seems ok to continue: %d %d
        // \n",itype,vold);
        return itype;
    }
    if ((vold == kEVT_numeric && (itype != CODES_TYPE_LONG && itype != CODES_TYPE_DOUBLE)) ||
        (vold == kEVT_string && itype != CODES_TYPE_STRING) ||
        (vold == kEVT_unknown && itype != CODES_TYPE_UNDEFINED)) {
        cout << CODES_TYPE_MISSING << " " << CODES_TYPE_LONG << " " << CODES_TYPE_DOUBLE << " " << CODES_TYPE_STRING
             << " " << CODES_TYPE_UNDEFINED << std::endl;
        printf("MvObs::elementValueType() : Different values: %d %d \n", itype, vold);
        exit(0);
    }
#endif

    return itype;
}

int MvObs::elementValueType(const string& skey) {
    int itype;
    codes_get_native_type(*_ecH, skey.c_str(), &itype);

    return itype;
}

int MvObs::elementValueType() {
    //   return elementValueType(_currentKey);
    int itype = elementValueType(_currentKey);

#ifdef MV_BUFRDC_TEST
    int vold = _bufrIn->elementValueType(_subsetNr);
    if (vold == kEVT_missing) {
        // printf("IMPORTANT: MvObs::elementValueType() -> values are different but it seems ok to continue: %d %d
        // \n",itype,vold);
        return itype;
    }
    if ((vold == kEVT_numeric && (itype != CODES_TYPE_LONG && itype != CODES_TYPE_DOUBLE)) ||
        (vold == kEVT_string && itype != CODES_TYPE_STRING) ||
        (vold == kEVT_unknown && itype != CODES_TYPE_UNDEFINED)) {
        printf("MvObs::elementValueType() : Different values: %d %d \n", itype, vold);
        exit(0);
    }
#endif

    return itype;
}

//______________________________________________________ numberOfPressureLevels
int MvObs::numberOfPressureLevels() {
    int npl = numberOfLevels(sPressureCoordinate);

#ifdef MV_BUFRDC_TEST
    int nn = numberOfLevels(cPressureCoordinate);
    TEMPCHECKVALUELONG((long)npl, (long)nn, true);
// e   return numberOfLevels( cPressureCoordinate );
#endif

    return npl;
}

//______________________________________________________ numberOfLevels
int MvObs::numberOfLevels(long levelDescriptor) {
    // Build key and get value
    string skey = this->key(levelDescriptor);
    int nelems  = numberOfLevels(skey);

#ifdef MV_BUFRDC_TEST
    int myCount = 0;
    if (firstLevel(levelDescriptor) != kBufrMissingValue) {
        myCount++;
        while (nextLevel() != kBufrMissingValue)
            myCount++;
    }
    _currentLevelIndex1 = -1;
    // e  return myCount;
    TEMPCHECKVALUELONG((long)nelems, (long)myCount, true);
#endif

    return nelems;
}

//______________________________________________________ numberOfLevels
int MvObs::numberOfLevels(const string& skey) {
    // Get number of elements
    size_t nelems;
    codes_get_size(*_ecH, skey.c_str(), &nelems);

    //   _currentLevelOccurrence = 0;

    return (int)nelems;
}

//__________________________________________________________ firstPressureLevel
double MvObs::firstPressureLevel() {
#ifdef MV_BUFRDC_TEST
    _currentLevelCoordinate1 = cPressureCoordinate;
    return pressureLevel(1, 0);
#else
    return pressureLevel(1);
#endif
}

//______________________________________________________________ pressureLevel
// Parameter indexValue must start from 1...N
#ifdef MV_BUFRDC_TEST
double MvObs::pressureLevel(int indexValue, int firstIndexValue) {
    _currentLevelKey        = sPressureCoordinate;
    _currentLevelOccurrence = indexValue;
    double myLevelValue     = level(_currentLevelKey, _currentLevelOccurrence);

#ifdef MV_BUFRDC_TEST
    double myLevelValue1 = level(_currentLevelKey, _currentLevelOccurrence, cPressureCoordinate, firstIndexValue);
    TEMPCHECKVALUELONG(myLevelValue, myLevelValue1);
#endif

    return myLevelValue == kBufrMissingValue ? kBufrMissingValue : myLevelValue / 100.;
}
#endif

double MvObs::pressureLevel(int indexValue) {
    _currentLevelKey        = sPressureCoordinate;
    _currentLevelOccurrence = indexValue;
    double myLevelValue     = level(_currentLevelKey, _currentLevelOccurrence);

    return myLevelValue == kBufrMissingValue ? kBufrMissingValue : myLevelValue / 100.;
}

//___________________________________________________________ nextPressureLevel
double MvObs::nextPressureLevel() {
#ifdef MV_BUFRDC_TEST
    // e   return pressureLevel( _currentLevelIndex + 1 );
    double myLevelValue = pressureLevel(_currentLevelOccurrence + 1, _currentLevelIndex1 + 1);
    return myLevelValue;
#else
    double myLevelValue = pressureLevel(_currentLevelOccurrence + 1);
    return myLevelValue;
#endif
}

//__________________________________________________________ firstLevel
double MvObs::firstLevel(long levelDescriptor) {
    string skey         = key(levelDescriptor);
    double myLevelValue = firstLevel(skey);

#ifdef MV_BUFRDC_TEST
    _currentLevelCoordinate1 = levelDescriptor;
    double myLevelValue1     = level(skey, 1, levelDescriptor, 0);
    TEMPCHECKVALUEDOUBLE(myLevelValue, myLevelValue1, skey, levelDescriptor);
#endif

    return myLevelValue;
}

double MvObs::firstLevel(const string& skey) {
    _currentLevelOccurrence = 1;
    _currentLevelKey        = skey;

    return level(_currentLevelKey, _currentLevelOccurrence);
}

//___________________________________________________________ nextLevel
double MvObs::nextLevel() {
    _currentLevelOccurrence++;
    double myLevelValue = level(_currentLevelKey, _currentLevelOccurrence);

#ifdef MV_BUFRDC_TEST
    // return level( _currentLevelCoordinate, _currentLevelIndex + 1 );
    double myLevelValue1 =
        level(_currentLevelKey, _currentLevelOccurrence, _currentLevelCoordinate1, _currentLevelIndex1 + 1);
    TEMPCHECKVALUEDOUBLE(myLevelValue, myLevelValue1, _currentLevelKey, 0);
#endif

    return myLevelValue;
}

#ifdef MV_BUFRDC_TEST
//______________________________________________________________ level
double MvObs::level(const string& skey, int indexValue, long levelDescriptor, int firstIndexValue) {
    // Build key and get value
    double myLevelValue = level(skey, indexValue);

    // if(MV_BUFRDC_TEST)
    {
        if (!msg_ok())
            return kBufrMissingValue;

        double myLevelValue1 = kFortranBufrMissingValue;
        if (_bufrIn->Value(levelDescriptor, _subsetNr, myLevelValue1, firstIndexValue))
            _currentLevelIndex1 = _bufrIn->_currentDescrInd;
        else
            _currentLevelIndex1 = -1;

        TEMPCHECKVALUEDOUBLE(myLevelValue, myLevelValue1, skey, levelDescriptor);
        // return myLevelValue == kFortranBufrMissingValue ? kBufrMissingValue : myLevelValue;
    }

    return myLevelValue;
}
#endif

//______________________________________________________________ level
// It is a private function; so, it is the responsability of the caller
// to update parameters _currentLevelKey and _currentLevelOccurrence .
// Parameter indexValue must start from 1...N
//
double MvObs::level(const string& key, int indexValue) {
    // Get value
    double myLevelValue = value(key, indexValue);

    return myLevelValue == CODES_MISSING_DOUBLE ? kBufrMissingValue : myLevelValue;
}

#ifdef MV_BUFRDC_TEST
//______________________________________________________________ specifierIndex
int MvObs::specifierIndex(long aSpecifierDescriptor, double aSpecifierValue, int firstIndexValue) {
    std::cout << "IMPORTANT: MvObs::specifierIndex should only be called by BUFRDC not from ecCodes" << std::endl;
    std::cout << "IMPORTANT: in ecCodes calls MvObs::valueBySpecifier" << std::endl;
    //   exit(0);

    if (!msg_ok())
        return -1;

    if (_bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded)
        _bufrIn->ExpandDescriptors(_subsetNr);

    if (_bufrIn->_inState == kBufrIn_DataAndDescriptorsDecoded) {
        //-- search for a specifier data with specified value --
        for (int index = firstIndexValue; index < In_KTDEXL; index++)
            if (In_KTDEXP[index] == aSpecifierDescriptor && _bufrIn->DataValue(index, _subsetNr) == aSpecifierValue) {
                _lastSpecifierIndex1 = index;
                return index;
            }
    }
    return -1;
}
#endif

//____________________________________________________________ valueBySpecifier
// A generic function to retrieve repeating data
// specified by some other data
// (e.g.  temperature at a certain pressure level)
//--------------------------------------------------------------------
double MvObs::valueBySpecifier(long aSpecifierDescriptor, double aSpecifierValue, long aDescriptor,
                               int firstIndexValue) {
    // Get the correspondent keys
    string s1key = key(aSpecifierDescriptor);
    string s2key = key(aDescriptor);

    double value = valueBySpecifier(s1key, aSpecifierValue, s2key);

#ifdef MV_BUFRDC_TEST
    int index = specifierIndex(aSpecifierDescriptor, aSpecifierValue, firstIndexValue);
    if (index > 0) {
        //-- if the coordinate value itself is requested
        if (aSpecifierDescriptor == aDescriptor) {
            double myVal = _bufrIn->DataValue(index, _subsetNr);
            // return myVal == kFortranBufrMissingValue ? kBufrMissingValue : myVal;
            myVal = (myVal == kFortranBufrMissingValue) ? kBufrMissingValue : myVal;
            TEMPCHECKVALUEDOUBLE(value, myVal, s2key, aDescriptor);
            return myVal;
        }
        //-- search real data before next specifier data --
        for (int ind = index + 1; ind < In_KTDEXL; ind++) {
            if (In_KTDEXP[ind] == aDescriptor) {
                double myVal = _bufrIn->DataValue(ind, _subsetNr);
                // return myVal == kFortranBufrMissingValue ? kBufrMissingValue : myVal;
                myVal = (myVal == kFortranBufrMissingValue) ? kBufrMissingValue : myVal;
                TEMPCHECKVALUEDOUBLE(value, myVal, s2key, aDescriptor);
                return myVal;
            }

            //-- not found if specifier data again --
            if (In_KTDEXP[ind] == aSpecifierDescriptor)
                break;
        }
    }

    // Key "DewPoint Temperature" can have descriptors 12103 and 12003.
    // The same for Temperature, 12101 and 12001
    // This cause a problem because BUFRDC can return a missing value
    // and ecCodes a valid value.
    if (aDescriptor == 12003 || aDescriptor == 12103 || aDescriptor == 12001 || aDescriptor == 12101)

        return value;

    TEMPCHECKVALUEDOUBLE(value, kBufrMissingValue, s2key, aDescriptor);
    // return kBufrMissingValue;   //-- Not Found or Troubled Msg --
#endif

    return value;
}

double MvObs::valueBySpecifier(const string& coordinateKey, double coordinateValue, const string& paramKey) {
#if 0
   // FAMI20171110 This is a simple solution, but eccodes is not working if the
   // specifier value is double/float (only works with integer values currently)
   std::ostringstream sstream;
   sstream << "=" << coordinateValue << "/";
   string skey = "/" + coordinateKey + sstream.str() + paramKey;
   return value(skey);
#endif

    // FAMI20171110 This is a temporary solution while the above code is not available
    // Find a coordinate key whose value matches the input value
    this->setFirstDescriptor();   // initialise key iterator
    double precision = 0.000001;  // maybe needs to be adjusted
    std::string skey;
    bool flag = true;
    while (flag) {
        skey = keyWithoutOccurrenceTag(_currentKey);
        if (skey == coordinateKey) {
            // Found coordinate descriptor key
            // Check if its value matches the input value
            // if ( this->currentValue() == coordinateValue )  // Dangerous to compare to "doubles"
            double val = this->currentValue();
            if (val > coordinateValue - precision && val < coordinateValue + precision)
                break;  // found it
        }

        flag = this->setNextDescriptor();  // advance to the next data
    }

    // Coordinate key with a specific value not found
    if (!flag)
        return kBufrMissingValue;

    // If the coordinate value itself is requested
    if (coordinateKey == paramKey)
        return coordinateValue;

    // Search parameter key before next coordinate data
    flag = this->setNextDescriptor();  // advance to the next data
    while (flag) {
        // Not found if coordinate data again
        skey = keyWithoutOccurrenceTag(_currentKey);
        if (skey == coordinateKey)
            return kBufrMissingValue;

        if (skey == paramKey)  // found parameter
            return currentValue();

        flag = this->setNextDescriptor();
    }

    // Not found
    return kBufrMissingValue;
}

//________________________________________________________ valueByPressureLevel
double MvObs::valueByPressureLevelC(float aLevelValue, const std::string& aDescriptor)  // in 'hPa'
{
    // Check only positive integer values; otherwise, use "-.0123456789"
    std::string skey;
    if (strspn(aDescriptor.c_str(), "0123456789") == aDescriptor.size())
        skey = key(atol(aDescriptor.c_str()));
    else
        skey = aDescriptor;

    return valueByPressureLevel(aLevelValue, skey);
}

double MvObs::valueByPressureLevel(float aLevelValue, long aDescriptor)  // in 'hPa'
{
    string s2key = key(aDescriptor);
    double value = valueByPressureLevel(aLevelValue, s2key);

#ifdef MV_BUFRDC_TEST
    // return valueBySpecifier( cPressureCoordinate, aLevelValue*100., aDescriptor );
    double value1 = valueBySpecifier(cPressureCoordinate, aLevelValue * 100., aDescriptor);
    TEMPCHECKVALUEDOUBLE(value, value1, s2key, aDescriptor);
#endif

    return value;
}

double MvObs::valueByPressureLevel(float aLevelValue, const string& s2key)  // in 'hPa'
{
    return valueBySpecifier(sPressureCoordinate, aLevelValue * 100., s2key);
}

//________________________________________________________ valueByLevel
double MvObs::valueByLevelC(const string& aLevelDescriptor, float aLevelValue, const std::string& aDescriptor) {
    // Check only positive integer values; otherwise, use "-.0123456789"
    std::string s1key;
    if (strspn(aLevelDescriptor.c_str(), "0123456789") == aLevelDescriptor.size())
        s1key = key(atol(aLevelDescriptor.c_str()));
    else
        s1key = aLevelDescriptor;

    std::string s2key;
    if (strspn(aDescriptor.c_str(), "0123456789") == aDescriptor.size())
        s2key = key(atol(aDescriptor.c_str()));
    else
        s2key = aDescriptor;

    return valueByLevel(s1key, aLevelValue, s2key);
}

double MvObs::valueByLevel(long aLevelDescriptor, float aLevelValue, long aDescriptor) {
    std::string s1key = key(aLevelDescriptor);
    std::string s2key = key(aDescriptor);
    double value      = valueByLevel(s1key, aLevelValue, s2key);

#ifdef MV_BUFRDC_TEST
    // return valueBySpecifier( aLevelDescriptor, aLevelValue, aDescriptor );
    double value1 = valueBySpecifier(aLevelDescriptor, aLevelValue, aDescriptor);
    TEMPCHECKVALUEDOUBLE(value, value1, s2key, aDescriptor);
#endif

    return value;
}

double MvObs::valueByLevel(const std::string& s1key, float aLevelValue, const std::string& s2key) {
    return valueBySpecifier(s1key, aLevelValue, s2key);
}

//________________________________________________________ valueByRange
// Get the first value from aDescriptor whose value from aLevelDescriptor
// is within level1 and level2 (both in Pa)
double MvObs::valueByLevelRangeC(const std::string& aLevelDescriptor, float level1, float level2,
                                 const std::string& aDescriptor) {
    // Check only positive integer values; otherwise, use "-.0123456789"
    std::string s1key;
    if (strspn(aLevelDescriptor.c_str(), "0123456789") == aLevelDescriptor.size())
        s1key = key(atol(aLevelDescriptor.c_str()));
    else
        s1key = aLevelDescriptor;

    std::string s2key;
    if (strspn(aDescriptor.c_str(), "0123456789") == aDescriptor.size())
        s2key = key(atol(aDescriptor.c_str()));
    else
        s2key = aDescriptor;

    return valueByLevelRange(s1key, level1, level2, s2key);
}

double MvObs::valueByLevelRange(long aLevelDescriptor, float level1, float level2, long aDescriptor) {
    std::string s1key = key(aLevelDescriptor);
    std::string s2key = key(aDescriptor);
    double value      = valueByLevelRange(s1key, level1, level2, s2key);

#ifdef MV_BUFRDC_TEST
    float minv = level1;
    float maxv = level2;
    if (level1 > level2) {
        minv = level2;
        maxv = level1;
    }
    float levelVal = firstLevel(aLevelDescriptor);
    while (levelVal != kBufrMissingValue) {
        if (levelVal >= minv && levelVal <= maxv) {
            for (int ind = _currentLevelIndex1 + 1; ind < In_KTDEXL; ind++) {
                if (In_KTDEXP[ind] == aDescriptor) {
                    double myValue = _bufrIn->DataValue(ind, _subsetNr);
                    if (myValue != kFortranBufrMissingValue) {
                        // e return myValue;
                        TEMPCHECKVALUEDOUBLE(value, myValue, s2key, aDescriptor);
                        return value;
                    }
                }
                else if (In_KTDEXP[ind] == aLevelDescriptor)
                    break;
            }
        }

        levelVal = nextLevel();
    }

    // e return kBufrMissingValue;
    TEMPCHECKVALUEDOUBLE(value, kBufrMissingValue, s2key, aDescriptor);
    return value;
#endif

    return value;
}

double MvObs::valueByLevelRange(const std::string& s1key, float level1, float level2, const std::string& s2key) {
    // Get number of elements
    size_t nelems, len;
    codes_get_size(*_ecH, s1key.c_str(), &nelems);

    // Allocate memory for the values to be read
    double* dlevels = new double[nelems];

    // Get values
    double myValue = kBufrMissingValue;
    len            = nelems;
    int ierr       = codes_get_double_array(*_ecH, s1key.c_str(), dlevels, &len);
    if (ierr || len != nelems) {
        delete[] dlevels;
        dlevels = 0;
        return kBufrMissingValue;
    }

    // Check input levels parameters (make sure level1 < level2)
    float minv = level1;
    float maxv = level2;
    if (level1 > level2) {
        minv = level2;
        maxv = level1;
    }

    // Search for a level between the intervals
    for (unsigned int i = 0; i < nelems; i++) {
        if (dlevels[i] >= minv && dlevels[i] <= maxv) {
            // Get value from the second key
            // double myValue = value(s2key,i+1); // can not assume that s1key and
            // s2key have the same occurrence
            // number
            myValue = valueBySpecifier(s1key, dlevels[i], s2key);
            if (myValue != kBufrMissingValue)
                break;
        }
    }

    // Release memory
    delete[] dlevels;
    dlevels = 0;

    return myValue;
}


// e This function is not working
//________________________________________________________________ valueByLayer
float MvObs::valueByLayerC(float firstLevel, float secondLevel, const std::string& aDescriptor) {
    std::cout << "MvObs::valueByLayerC -> not implemented yet" << std::endl;

    return kBufrMissingValue;  //-- Not Found or Troubled Msg --
}

float MvObs::valueByLayer(float firstLevel, float secondLevel, long aDescriptor) {
    std::cout << "MvObs :: valueByLayer -> not implemented yet" << std::endl;
    // exit(0);

#ifdef MV_BUFRDC_TEST
    int firstLevelIndex  = specifierIndex(cPressureCoordinate, firstLevel * 100., 0);
    int secondLevelIndex = -1;

    while (firstLevelIndex > 0)  //-- loop candidates --
    {
        //-- try to find the second level next to the current first one --

        cout << In_KTDEXP[firstLevelIndex - 1] << " " << In_KTDEXP[firstLevelIndex + 1] << std::endl;
        cout << _bufrIn->DataValue(firstLevelIndex - 1, _subsetNr) << " "
             << _bufrIn->DataValue(firstLevelIndex + 1, _subsetNr) << std::endl;
        if (In_KTDEXP[firstLevelIndex - 1] == cPressureCoordinate &&
            _bufrIn->DataValue(firstLevelIndex - 1, _subsetNr) == secondLevel * 100.) {
            secondLevelIndex = firstLevelIndex - 1;
            break;
        }
        else if (In_KTDEXP[firstLevelIndex + 1] == cPressureCoordinate &&
                 _bufrIn->DataValue(firstLevelIndex + 1, _subsetNr) == secondLevel * 100.) {
            secondLevelIndex = firstLevelIndex + 1;
            break;
        }

        //-- no match, let's find the next candidate --

        firstLevelIndex = specifierIndex(cPressureCoordinate, firstLevel * 100., firstLevelIndex + 1);
    }

    if (firstLevelIndex < 0)
        return kBufrMissingValue;  //-- levels were not found!!!

    //-- search real data before next specifier data --

    int maxim = firstLevelIndex > secondLevelIndex ? firstLevelIndex : secondLevelIndex;
    for (int ind = maxim + 1; ind < In_KTDEXL; ind++) {
        if (In_KTDEXP[ind] == aDescriptor) {
            double myValue = _bufrIn->DataValue(ind, _subsetNr);
            return myValue == kFortranBufrMissingValue ? kBufrMissingValue : myValue;
        }


        //-- not found if specifier data again --

        if (In_KTDEXP[ind] == cPressureCoordinate)
            break;
    }
#endif

    return kBufrMissingValue;  //-- Not Found or Troubled Msg --
}

//______________________________________________________________ printAllValues
bool MvObs::printAllValues() {
    std::ostream* myStream = &std::cout;
    return writeAllValues(*myStream);
}

//_______________________________________________________________ writeAllValues
bool MvObs::writeAllValues(std::ostream& aStream) {
#ifdef MV_BUFRDC_TEST
    if (!msg_ok())
        return false;

    if (_bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded)
        _bufrIn->ExpandDescriptors(_subsetNr);

    if (_bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded)
        return false;
#endif

    // e remove confidence code temporarily
    //   long myEndingIndex = _confidence->hasConfidences() ? _confidence->lastDataIndex()+1 : In_KTDEXL;
    long myEndingIndex = 100000;
    writeValues(aStream, 0, myEndingIndex);

    return true;
}

bool MvObs::writeValues(std::ostream& aStream, int firstIndex, int lastIndex) {
    double dval;

    // Main loop
    int index       = firstIndex;
    bool dataToRead = setFirstDescriptor();
    while (dataToRead && index <= lastIndex) {
        aStream.width(3);
        aStream.fill(' ');  // index
        aStream << index + 1 << ".  ";

        // Print value
        switch (elementValueType())  // print parameter value
        {
            case CODES_TYPE_LONG:
            case CODES_TYPE_DOUBLE:
                aStream.width(6);
                aStream.fill(' ');
                dval = currentValue();
#ifdef MV_BUFRDC_TEST
                {
                    double myValue = _bufrIn->DataValue(index, _subsetNr);
                    TEMPCHECKVALUEDOUBLE(dval, myValue, currentKey(), currentDescriptor());
                }
#endif
                if (dval == kBufrMissingValue)
                    aStream << "   ~~~";
                else
                    aStream << dval;

                break;
            case CODES_TYPE_STRING:
                aStream.width(6);
                aStream.fill(' ');
#ifdef MV_BUFRDC_TEST
                {
                    string sval     = stringValue();
                    string smyValue = stringValue(In_KTDEXP[index]);
                    std::cout << "MvObs::writeValues(1) STRINGS SHOULD BE THE SAME: " << sval.c_str() << " "
                              << smyValue.c_str(

                                     )
                              << std::endl;
                    TEMPCHECKVALUESTRING(sval, smyValue);
                }
#else
                aStream << stringValue();
#endif
                break;
        }

        // Print auxiliary information
        aStream << " " << name();            // parameter name
        aStream << " [" << unit() << "] (";  // parameter unit
        aStream.width(5);
        aStream.fill('0');
        aStream << currentDescriptor() << ")";  // parameter descriptor
        aStream << std::endl;

        // Evaluate next iteration
        setNextDescriptor();
        index++;
    }
    return true;

#if 0
#ifdef MV_BUFRDC_TEST
   if( _bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
      _bufrIn->ExpandDescriptors( _subsetNr );

   if( _bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded )
      return false;

   double myValue;
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
      switch( elementValueType() )   // print parameter value
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
      aStream << std::endl;
   }
   return true;
#endif
#endif
}

//_______________________________________________________________ writeAllValues
bool MvObs::writeAllValues(const char* aPathName) {
    std::ofstream myStream(aPathName, std::ios::out);
    if (!myStream) {
        std::cerr << " >>> MvObs::writeAllValues(char*): error in creating file " << aPathName << std::endl;
#ifdef METVIEW
        marslog(LOG_EROR, "MvObs::writeAllValues: error in creating file %s", aPathName);
#endif
        return false;
    }

    return writeAllValues(myStream);
}

#ifdef MV_BUFRDC_TEST
//_______________________________________________________________ writeBufrBox
bool MvObs::writeBufrBox(ostream& aStream) {
    std::cout << "MvObs::writeBufrBox() -> not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    bool return_value = true;

    bool b_ret = _bufrIn->writeBufrBox(_subsetNr);
    if (!b_ret) {
        aStream << "\n   >>> Problems encountered!!! <<<\n" << std::endl;
        return_value = false;
    }
    else {
        return_value = file_to_stream(BBOXNAME.c_str(), aStream, 0);
        delete_print_file(BBOXNAME.c_str());
    }
    return return_value;
#endif
}
#endif

//______________________________________________________________ WmoIdentNumber
long MvObs::WmoIdentNumber() {
    return WmoBlockNumber() * 1000 + WmoStationNumber();
}

//______________________________________________________________ WmoBlockNumber
int MvObs::WmoBlockNumber() {
    long myValue = intValue("blockNumber");

#ifdef MV_BUFRDC_TEST
    long myValue1;
    // e   if( msg_ok() && _bufrIn->Value( 1001L, _subsetNr, myValue ) )
    // e      return (long)myValue;
    // e   else
    // e      return 0;
    myValue1 = _bufrIn->intValue(1001L, _subsetNr);
    TEMPCHECKVALUELONG(myValue, (long)myValue1);
#endif

    return myValue == kBufrMissingIntValue ? 0 : (int)myValue;
}

//____________________________________________________________ WmoStationNumber
int MvObs::WmoStationNumber() {
    long myValue = intValue("stationNumber");

#ifdef MV_BUFRDC_TEST
    long myValue1;
    // e   if( msg_ok() && _bufrIn->Value( 1002L, _subsetNr, myValue ) )
    // e      return (long)myValue;
    // e   else
    // e      return 0;
    myValue1 = _bufrIn->intValue(1002L, _subsetNr);
    TEMPCHECKVALUELONG(myValue, (long)myValue1);
#endif

    return myValue == kBufrMissingIntValue ? 0 : (int)myValue;
}

//____________________________________________________________ findSomeIdent
string MvObs::findSomeIdent() {
    //-- 5-digit WMO identifier available?
    long lid = WmoIdentNumber();
    if (lid > 0) {
        std::ostringstream oss;
        oss << std::setw(5) << std::setfill('0') << lid;
        return oss.str();
    }

    //-- No WMO id found, thus look for other candidates,
    //-- this is a list of known identifier candidates.
    const string idList[] = {
        "shipOrMobileLandStationIdentifier",                // 1011
        "buoyOrPlatformIdentifier",                         // 1005
        "aircraftFlightNumber",                             // 1006
        "satelliteIdentifier",                              // 1007
        "aircraftRegistrationNumberOrOtherIdentification",  // 1008
        "stationaryBuoyPlatformIdentifierEGCManBuoys",      // 1010
        "stormIdentifier",                                  // 1025
        "stormName",                                        // 1026
        "longStormName"                                     // 1027
    };

#ifdef MV_BUFRDC_TEST
    const long idList1[] = {1011, 1005, 1006, 1007, 1008, 1010, 1025, 1026, 1027};
#endif

    int idVals = sizeof(idList) / sizeof(idList[0]);
    for (int i = 0; i < idVals; ++i) {
        string descr = idList[i];
        long myValue = intValue(descr);

#ifdef MV_BUFRDC_TEST
        long myValue1 = intValue(idList1[i]);
        TEMPCHECKVALUELONG((double)myValue, myValue1);
#endif

        if (myValue != kBufrMissingIntValue) {
            std::ostringstream oss;
            oss << std::setw(5) << std::setfill('0') << myValue;
            return oss.str();
        }
    }

    return string("id???");

#if 0
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
      double myValue = value( descr );

      if( myValue != kBufrMissingValue )
         return stringValue( descr );
    }
   return string( "id???" );
#endif
}

//____________________________________________________________________ location
MvLocation MvObs::location() {
    // F NEW CODE
    // ERROR   ERROR   ERROR    ERROR   20170628
    // THERE IS A PROBLEM HERE. IN THE ORIGINAL CODE (ABOVE) IF "HIGH
    // ACCURACY" VALUES ARE NOT PRESENTED, IT TRIES TO GET THE "COARSE
    // ACCURACY" ONES (5002,6002).
    // ECCODES ASSIGNS THE SAME KEY NAME FOR BOTH HIGH/LOW ACCURACY
    // VALUES. AT THE MOMENT, THE CODE IS DIFFERENT FROM THE ORIGINAL
    // ONE UNTIL WE SORT OUT THIS ISSUE.

    // Get latitude/longitude values
    MvLocation myLocation(value("latitude"), value("longitude"));  //-- "high accuracy"
    if (myLocation.latitude() == kBufrMissingValue || myLocation.longitude() == kBufrMissingValue) {
        // myLocation.set( value( 5002L ), value( 6002L ) );     //-- "coarse accuracy"
    }  //-- hopefully not missing

#if 0  // F add prepBUFR later
   // This one is for those quirky NCEP PrepBUFR msgs
   if( myLocation.latitude() != kBufrMissingValue &&
       myLocation.longitude() == kBufrMissingValue )
   {
      //-- lat OK, try NCEP PrepBUFR local descriptor 0'06'240 for lon
      myLocation.set( myLocation.latitude(), value( 6240L ) );
   }
#endif

#ifdef MV_BUFRDC_TEST
    MvLocation myLocation1(value(5001L), value(6001L));  //-- "high accuracy"

    if (myLocation1.latitude() == kBufrMissingValue || myLocation1.longitude() == kBufrMissingValue) {
        myLocation1.set(value(5002L), value(6002L));  //-- "coarse accuracy"
    }                                                 //-- hopefully not missing

    //-- this one is for those quirky NCEP PrepBUFR msgs
    if (myLocation1.latitude() != kBufrMissingValue && myLocation1.longitude() == kBufrMissingValue) {
        //-- lat OK, try NCEP PrepBUFR local descriptor 0'06'240 for lon
        myLocation1.set(myLocation1.latitude(), value(6240L));
    }
    //   return myLocation;

    if (myLocation.distanceInMeters(myLocation1)) {
        std::cout << "MvObs::location() -> different values" << std::endl;
        exit(0);
    }
#endif

    return myLocation;
}

//____________________________________________________________________ unit
string MvObs::unit(long aDescriptor) {
    string skey  = key(aDescriptor) + "->units";
    string sunit = stringValue(skey);

#ifdef MV_BUFRDC_TEST
    // return msg_ok() ? _bufrIn->unit( aDescriptor ) : MESSED_UP;
    string vold = msg_ok() ? _bufrIn->unit(aDescriptor) : MESSED_UP;
    TEMPCHECKVALUESTRING(sunit, vold);
#endif

    return sunit;
}

//____________________________________________________________________ unit
string MvObs::unit() {
    string skey  = _currentKey + "->units";
    string sunit = stringValue(skey);

#ifdef MV_BUFRDC_TEST
// e remove test because values can be different between BUFRDC and ecCodes,
// e.g., YEAR and a
// return msg_ok() ? _bufrIn->unit() : MESSED_UP;
//   string vold = msg_ok() ? _bufrIn->unit() : MESSED_UP;
//   TEMPCHECKVALUESTRING(sunit,vold);
//   std::cout << "values in ecCodes and BUFRDC are differents: " << sunit << " " << vold << std::endl;
#endif

    return sunit;
}

//____________________________________________________________________ name
string MvObs::name(long aDescriptor) {
    string skey = key(aDescriptor);

#ifdef MV_BUFRDC_TEST
    // return msg_ok() ? _bufrIn->name( aDescriptor ) : MESSED_UP;
    string vold = msg_ok() ? _bufrIn->name(aDescriptor) : MESSED_UP;
    TEMPCHECKVALUESTRING(skey, vold);
#endif

    return skey;
}

//____________________________________________________________________ name
string MvObs::name() {
#ifdef MV_BUFRDC_TEST
    // return msg_ok() ? _bufrIn->name() : MESSED_UP;
    string vold = msg_ok() ? _bufrIn->name() : MESSED_UP;
    TEMPCHECKVALUESTRING(_currentKey, vold);
#endif

    return _currentKey;
}

//----------------------------------------------------------------------------
//-- APIs for requesting info from the Header, BUFR Section 0,1,2,3 --//

//___________________________________________________________________ init
void MvObs::init() {
    // Read initial variables
    masterTableVersion();
    localTableVersion();
    msgSubsetCount();
    _compressed_data = intValue("compressedData");

    return;
}

//_________________________________________________________  messageTotalLen()
int MvObs::messageTotalLen() {
    if (_messageTotalLen == -1)
        _messageTotalLen = intValue("totalLength");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->totalLen();
    TEMPCHECKVALUELONG(_messageTotalLen, vold);
#endif

    return (int)_messageTotalLen;
}

//___________________________________________________________ editionNumber
int MvObs::editionNumber() {
    if (_editionNumber == -1)
        _editionNumber = intValue("edition");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->fSec0->editionNr;
    TEMPCHECKVALUELONG(_editionNumber, vold);
#endif

    return (int)_editionNumber;
}

//_______________________________________________________________ msgSubsetCount
int MvObs::msgSubsetCount() {
    if (_number_of_subsets == -1)
        _number_of_subsets = intValue("numberOfSubsets");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->subsetCount();
    TEMPCHECKVALUELONG(_number_of_subsets, vold);
#endif

    // Variable initialised at init()
    return (int)_number_of_subsets;
}

//_________________________________________________________________ messageType
int MvObs::messageType() {
    if (_messageType == -1)
        _messageType = intValue("dataCategory");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->msgType();
    TEMPCHECKVALUELONG(_messageType, vold);
#endif

    return (int)_messageType;
}

//______________________________________________________________ messageSubtype
int MvObs::messageSubtype() {
    // FI 20170925: ecCodes does not have a flag to indicate a missing value
    // indicator as BUFRDC (cOctetMissingIndicator).
    // Update this code when ecCodes can handle a missing value indicator
    // for a octet.
    long lval = messageSubtypeInternational();
    if (lval == kBufrMissingIntValue || lval == 255)
        lval = messageSubtypeLocal();

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->msgSubtype();
    TEMPCHECKVALUELONG(lval, vold);
#endif

    return (int)lval;
}

//______________________________________________________________ messageSubtype
int MvObs::messageRdbtype() {
    if (_rdbType == -1)
        _rdbType = intValue("rdbType");

    return (int)_rdbType;
}

//_________________________________________________ messageSubtypeInternational
int MvObs::messageSubtypeInternational() {
    if (_subTypeInternational == -1)
        _subTypeInternational = intValue("internationalDataSubCategory");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->msgSubtypeWMO();
    if (_subTypeInternational != vold) {
        if (_subTypeInternational != kBufrMissingIntValue || vold != cOctetMissingIndicator) {
            printf("Different values -> MvObs::messageSubtypeInternational %ld %ld /n", _subTypeInternational, vold);
            exit(0);
        }
    }
// TEMPCHECKVALUELONG(_subTypeInternational,vold);
#endif

    return (int)_subTypeInternational;
}

//_________________________________________________________ messageSubtypeLocal
int MvObs::messageSubtypeLocal() {
    if (_subTypeLocal == -1)
        _subTypeLocal = intValue("dataSubCategory");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->msgSubtypeLocal();
    TEMPCHECKVALUELONG(_subTypeLocal, vold);
#endif

    return (int)_subTypeLocal;
}

//___________________________________________________________ originatingCentre
int MvObs::originatingCentre() {
    if (_originatingCentre == -1)
        _originatingCentre = intValue("bufrHeaderCentre");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->origCentre();
    TEMPCHECKVALUELONG(_originatingCentre, vold);
#endif

    return (int)_originatingCentre;
}

const std::string& MvObs::originatingCentreAsStr() {
    if (_originatingCentreStr.empty())
        _originatingCentreStr = stringValue("bufrHeaderCentre");

    return _originatingCentreStr;
}


//___________________________________________________________ originatingSubCentre
int MvObs::originatingSubCentre() {
    if (_originatingSubCentre == -1)
        _originatingSubCentre = intValue("bufrHeaderSubCentre");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->origSubCentre();
    TEMPCHECKVALUELONG(_originatingSubCentre, vold);
#endif

    return (int)_originatingSubCentre;
}

//___________________________________________________________ masterTable
int MvObs::masterTable() {
    if (_masterTable == -1)
        _masterTable = intValue("masterTableNumber");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->masterTable();
    TEMPCHECKVALUELONG(_masterTable, vold);
#endif

    return (int)_masterTable;
}

//___________________________________________________________ masterTableVersion
int MvObs::masterTableVersion() {
    if (_masterTableVersion == -1)
        _masterTableVersion = intValue("masterTablesVersionNumber");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->masterTableVersion();
    TEMPCHECKVALUELONG(_masterTableVersion, vold);
#endif

    return (int)_masterTableVersion;
}

//___________________________________________________________ localTableVersion
int MvObs::localTableVersion() {
    if (_localTableVersion == -1)
        _localTableVersion = intValue("localTablesVersionNumber");

#ifdef MV_BUFRDC_TEST
    long vold = _bufrIn->Sec1->localTableVersion();
    TEMPCHECKVALUELONG(_localTableVersion, vold);
#endif

    return (int)_localTableVersion;
}

const std::string& MvObs::headerIdent() {
    if (headerIdent_ == "__UNDEF__") {
        if (hasSection2() && originatingCentre() == 98)
            headerIdent_ = stringValue("ident");
        else
            headerIdent_ = std::string();
    }
    return headerIdent_;
}

//_________________________________________________________________ msgTime
TDynamicTime MvObs::msgTime() {
    // Get values from the Header
    if (_lyear == -1) {
        _lyear   = intValue("typicalYear");
        _lmonth  = intValue("typicalMonth");
        _lday    = intValue("typicalDay");
        _lhour   = intValue("typicalHour");
        _lminute = intValue("typicalMinute");

#ifdef MV_BUFRDC_TEST
        TDynamicTime temp = _bufrIn->msgTime();
        TEMPCHECKVALUELONG(_lyear, temp.GetYear());
        TEMPCHECKVALUELONG(_lmonth, temp.GetMonth());
        TEMPCHECKVALUELONG(_lday, temp.GetDay());
        TEMPCHECKVALUELONG(_lhour, temp.GetHour());
        TEMPCHECKVALUELONG(_lminute, temp.GetMin());
#endif
    }

    // Return time
    return TDynamicTime((short)_lyear, (short)_lmonth, (short)_lday, (short)_lhour, (short)_lminute, 0);
}
//----------------------------------------------------------------------------

TDynamicTime MvObs::obsTime(int occurrence) {
    // Get values
    long lyear   = intValue("year", occurrence);
    long lmonth  = intValue("month", occurrence);
    long lday    = intValue("day", occurrence);
    long lhour   = intValue("hour", occurrence);
    long lminute = intValue("minute", occurrence);
    long lsecond = intValue("second", occurrence);

    // Check values
    lminute = (lminute == kBufrMissingIntValue) ? 0 : lminute;
    lsecond = (lsecond == kBufrMissingIntValue) ? 0 : lsecond;

    // NCEP PrepBUFR may not contain date/time information
    if (lyear == kBufrMissingIntValue || lmonth == kBufrMissingIntValue || lday == kBufrMissingIntValue)
        return this->msgTime();  // take date from section 1

#ifdef MV_BUFRDC_TEST
    TDynamicTime old = _bufrIn->obsTime(_subsetNr);
    TDynamicTime ect =
        TDynamicTime((short)lyear, (short)lmonth, (short)lday, (short)lhour, (short)lminute, (short)lsecond);
    if (old != ect) {
        std::cout << "MvObs::obsTime -> TIMES different: " << std::endl;
        exit(0);
    }
#endif

    return TDynamicTime((short)lyear, (short)lmonth, (short)lday, (short)lhour, (short)lminute, (short)lsecond);
}

//----------------------------------------------------------------------------
//-- APIs for converting Descriptors to keys --//

std::string MvObs::keyC(const std::string& descriptor, const int index) {
    // Check only positive integer values; otherwise, use "-.0123456789"
    if (strspn(descriptor.c_str(), "0123456789") == descriptor.size())
        return key(atol(descriptor.c_str()), index);
    else
        return descriptor;
}

std::string MvObs::key(const int descriptor, const int index) {
    if (!_edition)
        _edition = MvBufrEdition::find(_masterTable, _masterTableVersion, _localTableVersion, _originatingCentre,
                                       _originatingSubCentre);

    MvBufrElementTable* tbl = MvBufrElementTable::find(_edition);
    assert(tbl);

    std::string skey = tbl->keyName(descriptor);

    // Build the key
    if (!skey.empty() && index > 0)
        skey = key(skey, index);

    return skey;
}

bool MvObs::descriptorToKey(const long descriptor, string& key) {
    codes_handle* dkH = NULL;
    size_t size       = 1;
    char* strVal[1]   = {
        0,
    };
    bool ret = true;

    dkH      = codes_bufr_handle_new_from_samples(NULL, "BUFR4");
    int err1 = codes_set_long(dkH, "masterTablesVersionNumber", _masterTableVersion);
    int err2 = codes_set_long(dkH, "localTablesVersionNumber", _localTableVersion);
    int err3 = codes_set_long(dkH, "unexpandedDescriptors", descriptor);
    if (err1 || err2 || err3) {
        fprintf(stderr, "MvObs::descriptorToKey: Key not found from Descriptor: %ld\n", descriptor);
        key = "";
        ret = false;
    }

    codes_get_size(dkH, "expandedAbbreviations", &size);
    if (size == 1) {
        codes_get_string_array(dkH, "expandedAbbreviations", strVal, &size);
        key = strVal[0];
    }
    else {
        fprintf(stderr, "MvObs::descriptorToKey: Invalid Descriptor: %ld\n", descriptor);
        key = "";
        ret = false;
    }

    codes_handle_delete(dkH);

    return ret;
}

bool MvObs::descriptor_to_key(const long descriptor, std::string& key) {
    // Get BUFR key iterator
    codes_bufr_keys_iterator* kiter = NULL;
    kiter                           = codes_bufr_keys_iterator_new(*_ecH, 0);
    if (!kiter) {
        std::cout << "ERROR MvObs::descriptor_to_key(): Unable to create BUFR keys iterator" << std::endl;
        key = "";
        return false;
    }

    bool flag = false;
    string name, name_code;
    int err;

    // Loop through the keys
    while (codes_bufr_keys_iterator_next(kiter)) {
        long codeVal = 0;
        name         = codes_bufr_keys_iterator_get_name(kiter);
        name_code    = name + "->code";
        err          = codes_get_long(*_ecH, name_code.c_str(), &codeVal);
        if (!err && codeVal == descriptor) {
            // Remove the prefix #n#
            std::size_t ipos = -1;
            if (name[0] == '#')
                ipos = name.find('#', 1);

            key  = name.substr(ipos + 1);
            flag = true;
            break;
        }
    }

    // Delete key iterator
    codes_bufr_keys_iterator_delete(kiter);

    return flag;
}

string MvObs::key(const string& ikey, const int occurrence) {
    // Return original key
    if (occurrence < 1)
        return ikey;

    // Build the key
    std::ostringstream sstream;
    sstream << "#" << occurrence << "#";
    string key = sstream.str() + ikey;

    return key;
}

std::string MvObs::keyWithoutOccurrenceTag(const std::string& key) {
    // Remove the prefix #n#
    if (!key.empty() && key[0] == '#') {
        std::size_t ipos = key.find('#', 1);
        if (ipos != std::string::npos)
            return key.substr(ipos + 1);
    }

    return key;
}

int MvObs::occurenceFromKey(const std::string& key) {
    if (!key.empty() && key[0] == '#') {
        std::size_t ipos = key.find('#', 1);
        if (ipos != std::string::npos) {
            return atoi(key.substr(1, ipos - 1).c_str());
        }
    }

    return -1;
}

//----------------------------------------------------------------------------

#ifdef MV_BUFRDC_TEST
double MvObs::feedbackValue(int col) {
    std::cout << " Method MvObs::feedbackValue() not implemented yet" << std::endl;
    exit(0);

    return _bufrIn->feedbackValue(col, _subsetNr);
}

double MvObs::feedbackValue(int row, int col) {
    std::cout << " Method MvObs::feedbackValue(int,int) not implemented yet" << std::endl;
    exit(0);

    return _bufrIn->feedbackValue(row, col, _subsetNr);
}

string MvObs::feedbackItemName(int row) {
    std::cout << " Method MvObs::feedbackItemName(int) not implemented yet" << std::endl;
    exit(0);

    return _bufrIn->feedbackItemName(row, _subsetNr);
}

string MvObs::feedbackItemUnit(int row) {
    std::cout << " Method MvObs::feedbackItemUnit(int) not implemented yet" << std::endl;
    exit(0);

    return _bufrIn->feedbackItemUnit(row, _subsetNr);
}
#endif

//______________________________________________________________ hasConfidences
bool MvObs::hasConfidences() {
    std::cout << "MvObs :: hasConfidences() -> not yet implemented" << std::endl;
    exit(0);

    return _confidence->hasConfidences();
}
//__________________________________________________________________ confidence
int MvObs::confidence() {
    std::cout << "MvObs :: confidence() -> not yet implemented" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    return _bufrIn->_currentDescrInd > -1 ? _confidence->confidenceByIndex(_bufrIn->_currentDescrInd) : -1;
#endif
}

void MvBufrSubsetData::addLongData(const std::string& key, long val) {
    std::vector<long> vec;
    vec.push_back(val);
    longData_[key] = vec;
}

void MvBufrSubsetData::addLongData(const std::string& key, long* val, size_t num) {
    if (num > 0)
        longData_[key] = std::vector<long>(val, val + num);
}

void MvBufrSubsetData::addDoubleData(const std::string& key, double val) {
    std::vector<double> vec;
    vec.push_back(val);
    doubleData_[key] = vec;
}

void MvBufrSubsetData::addDoubleData(const std::string& key, double* val, size_t num) {
    if (num > 0)
        doubleData_[key] = std::vector<double>(val, val + num);
}

const std::vector<long>& MvBufrSubsetData::longData(const std::string& key) const {
    std::map<std::string, std::vector<long> >::const_iterator it = longData_.find(key);
    if (it != longData_.end())
        return it->second;

    static std::vector<long> emptyVec;
    return emptyVec;
}

const std::vector<double>& MvBufrSubsetData::doubleData(const std::string& key) const {
    std::map<std::string, std::vector<double> >::const_iterator it = doubleData_.find(key);
    if (it != doubleData_.end())
        return it->second;

    static std::vector<double> emptyVec;
    return emptyVec;
}


#ifdef METVIEW
//---
//-- Extracts a 4 or 8 bit OPERA radar image
//-- from a BUFR message into 'unsigned char' array
//--
//-- ( NOTE: U N F I N I S H E D ! ! ! )
//____________________________________________________________________ OperaRadarImage
unsigned char* MvObs::OperaRadarImage() {
    std::cout << " Method MvObs::OperaRadarImage() not implemented yet" << std::endl;
    exit(0);

#if 0  // FAMI20171011
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
   // the following line assigns an unused variable, but the line is still essential
//   int rowRepeatCount = (*this)[firstRowStartIndex]; //-- operator[i], i=1,2,3,...
   long currentDescr = currentDescriptor();
   if( currentDescr != 31002 )
   {
      throw MvException( "MvObs::OperaRadarImage: internal error - not 031002" );
   }

   //---
   //-- Still OK, now create the pixel array and start decoding the BUFR msg
   //-

   unsigned char* radimg = new unsigned char[n_rows*n_cols];
   std::cout << "radimg size: " << n_rows << "*" << n_cols << " = " << n_rows*n_cols << std::endl;

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
                 << nextCurrDescr << std::endl;
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
   std::cout << "Last pixel pos: " << pixelPos << std::endl;
   std::cout << "Max pixel value: " << imax << std::endl;

   return radimg;
#else
    unsigned char* str = (unsigned char*)' ';
    return str;
#endif  // FAMI20171011
}
//---
//-- Extracts OPERA radar image metadata that is required
//-- for instance for geolocating the image
//--
//-- ( NOTE: U N F I N I S H E D ! ! ! )
//____________________________________________________________________ OperaRadarMetadata
bool MvObs::OperaRadarMetadata(/* <aki> arguments? */) {
    return false;
}
#endif  // METVIEW

#ifdef MV_BUFRDC_TEST
//________________________________________________________________ subsetOffset
int MvObs::subsetOffset() const {
    std::cout << "MvObs :: subsetOffset() -> not implemented yet" << std::endl;
    exit(0);

    return (_subsetNr - 1) * In_KELEM;
}
//________________________________________________________________ writeConfidenceValues
bool MvObs::writeConfidenceValues(ostream& aStream) {
    std::cout << "MvObs::writeConfidenceValues() -> not implemented yet" << std::endl;
    exit(0);

    if (!msg_ok() || !_confidence->hasConfidences())
        return false;

    if (_bufrIn->_inState != kBufrIn_DataAndDescriptorsDecoded)
        _bufrIn->ExpandDescriptors(_subsetNr);

    if (_bufrIn->_inState == kBufrIn_DataAndDescriptorsDecoded) {
        long myStartingIndex = _confidence->lastDataIndex() + 1;
        long myEndingIndex   = In_KTDEXL;
        writeValues(aStream, myStartingIndex, myEndingIndex);
        return true;
    }

    return false;
}
#endif  // MV_BUFRDC_TEST

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

#ifdef MV_BUFRDC_TEST
MvBufrConfidence ::MvBufrConfidence(MvBufr* aBufr, int aSubsetNr)
#else
MvBufrConfidence ::MvBufrConfidence(int aSubsetNr)
#endif

{
    std::cout << " Method MvBufrConfidence::MvBufrConfidence() not implemented yet" << std::endl;
    // e   exit(0);

#ifdef MV_BUFRDC_TEST
    _bufr = aBufr;
    if (_bufr)
        _bufr->attach();
    _subsetNr           = aSubsetNr;
    _state              = kBCS_unknown;
    _startOfDataPresent = -2;
    _startOfConfidences = -2;
#endif
}
//___________________________________________________________ ~MvBufrConfidence

MvBufrConfidence ::~MvBufrConfidence() {
// e   std::cout << " Method MvBufrConfidence::~MvBufrConfidence() not implemented yet" << std::endl;
// e   exit(0);
#ifdef MV_BUFRDC_TEST
    if (_bufr)
        _bufr->detach();
#endif
}
//______________________________________________________________ hasConfidences
bool MvBufrConfidence ::hasConfidences() {
    std::cout << " Method MvBufrConfidence::hasConfidences() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    if (!_bufr)
        return false;

    if (_state == kBCS_unknown)
        _state = _bufr->descriptorToFortranIndex(222000L) > 0 ? kBCS_exists : kBCS_missing;

    return _state == kBCS_exists ? true : false;
#endif
}
//_______________________________________________________________ confidence
int MvBufrConfidence ::confidence(long aDescr) {
    std::cout << " Method MvBufrConfidence::confidence() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    int myDataIndex = _bufr->descriptorToFortranIndex(aDescr);
    return myDataIndex > -1 ? confidenceByIndex(myDataIndex) : -1;
#endif
}
//___________________________________________________________ confidenceByIndex
int MvBufrConfidence ::confidenceByIndex(int aDataInd) {
    std::cout << " Method MvBufrConfidence::confidenceByIndex() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    int myConfidenceInd = -1;

    if (hasConfidences()) {
        //--    case a:        case b:
        //--        . . .          . . .
        //--       0xxyyy         0xxyyy    <- 0xxyyy: element descriptors
        //--       0xxyyy         222000
        //--       222000         03100x
        //--       031031         031031    <- startOfDataPresent()
        //--       031031         031031
        //--        . . .          . . .
        //--
        if (aDataInd < (startOfDataPresent() - 1) &&  //-- skip 222000
            _bufr->CurrentDescriptor() != 222000L)    //-- if previous was 03100*
        {
            if (_bufr->DataValue(aDataInd, _subsetNr) != kFortranBufrMissingValue)
                myConfidenceInd = startOfConfidences() + delta(aDataInd);
        }
    }

    return myConfidenceInd > -1 ? (int)(_bufr->PeekDataValue(myConfidenceInd, _subsetNr)) : -1;
#endif
}
//_______________________________________________________________ lastDataIndex
int MvBufrConfidence ::lastDataIndex() {
    std::cout << " Method MvBufrConfidence::lastDataIndex() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    return hasConfidences() ? _bufr->descriptorToFortranIndex(222000L) - 1 : -1;
#endif
}
//__________________________________________________________ startOfDataPresent
// Q&D hack !!!

int MvBufrConfidence ::startOfDataPresent() {
    std::cout << " Method MvBufrConfidence::startOfDataPresent() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    if (_startOfDataPresent == -2) {
        if (hasConfidences()) {
            _startOfDataPresent = _bufr->descriptorToFortranIndex(31031L);
            if (_startOfDataPresent < 1) {
                cerr << "[MvBufrConfidence::startOfDataPresent] Q&D hack does not work!!" << std::endl;
                _state = kBCS_missing;
            }
        }
    }

    return hasConfidences() ? _startOfDataPresent : -1;
#endif
}
//_______________________________________________________________ startOfConfidences
// Q&D hack !!!

int MvBufrConfidence ::startOfConfidences() {
    std::cout << " Method MvBufrConfidence::startOfConfidences() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    if (_startOfConfidences == -2) {
        if (hasConfidences()) {
            _startOfConfidences = _bufr->descriptorToFortranIndex(33007L);
            if (_startOfConfidences < 1) {
                cerr << "[MvBufrConfidence::startOfConfidences] Q&D hack does not work!!" << std::endl;
                _state = kBCS_missing;
            }
        }
    }

    return hasConfidences() ? _startOfConfidences : -1;
#endif
}
//_______________________________________________________________ delta
int MvBufrConfidence ::delta(int anInd) {
    std::cout << " Method MvBufrConfidence::delta() not implemented yet" << std::endl;
    exit(0);

#ifdef MV_BUFRDC_TEST
    if (hasConfidences()) {
        int myDelta = 0;

        for (int i = 0; i < anInd; i++)
            if (_bufr->PeekDataValue(startOfDataPresent() + i, _subsetNr) == 0)
                myDelta++;

        return myDelta;
    }
    else
        return -1;
#endif
}

//________________________________________________________________________
//======================================================================== MvBufrParam
//________________________________________________________________________

MvBufrParam ::MvBufrParam(const char* aParamName) {
    std::cout << " Method MvBufrParam::MvBufrParam() not implemented yet" << std::endl;
    exit(0);

    descriptorStruct* par = knownParams;
    while (par->descriptor != 0) {
        if (strcmp(aParamName, par->name) == 0) {
            fDescriptor = par->descriptor;
            return;
        }
        ++par;
    }
    std::cerr << " >>> MvBufrParam::MvBufrParam: param not defined: " << aParamName << std::endl;
    fDescriptor = 0;
}
//_____________________________________________________________ PrintAllKnownParameters

void MvBufrParam ::PrintAllKnownParameters() const {
    std::cout << " Method MvBufrParam::PrintAllKnownParameters() not implemented yet" << std::endl;
    exit(0);

    descriptorStruct* par = knownParams;
    std::cout << " The Known Parameters of class MvBufrParam and the corresponding Descriptors:\n";
    while (par->descriptor != 0) {
        std::cout << "\n";
        std::cout.width(16);
        std::cout.fill(' ');
        std::cout << par->name << " = ";
        std::cout.width(6);
        std::cout.fill('0');
        std::cout << par->descriptor;
        ++par;
    }
    std::cout << std::endl;
}

#ifdef MV_BUFRDC_TEST
//-------------------- Helper functions to redirect stdout ----------------
// This function will duplicate the file descriptor for stdout, and keep
// it for later. Then stdout is connected to the file given as arg.
// The first time the function is run, it tries to find a temporary dir.
// to use ( class could be used outside metview ).

bool redirect_6(const char* fname) {
    std::cout << " Method redirect_6() BUFRDC implementation" << std::endl;

    std::cout << " redirect_6: redirect stdout into file " << fname << std::endl;
    char* tmp_dir;
    // Set the output dir. if it's not set.
    if (redirect_dir == string("")) {
        tmp_dir = getenv("METVIEW_TMPDIR");
        if (!tmp_dir) {
            tmp_dir = getenv("TMPDIR");
            if (!tmp_dir)
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

    std::cout << "new_fd: " << new_fd << std::endl;

    close(STDOUT_FILENO);

    string tmp_name;
    tmp_name = redirect_dir + "/" + fname;
    // As 1 is just closed, it will be the fd used.
    // Will fail if stdin is explicitly closed and no fopens are done
    if (!(fopen(tmp_name.c_str(), "w"))) {
        std::cerr << " >>> MvObs::redirect_6 - ERROR opening file \'" << tmp_name << "\' - " << std::strerror(errno)
                  << std::endl;
        return false;
    }

    else
        return true;
}

//
// Will reconnect stdout by using a file descriptor saved from earlier.
//
bool reconnect_6() {
    std::cout << " Method reconnect_6() BUFRDC implementation" << std::endl;

    // Make sure we get everything before closing.
    fflush(NULL);
    close(STDOUT_FILENO);

    int ret = dup2(new_fd, STDOUT_FILENO);
    close(new_fd);
    if (ret < 0) {
        cerr << " reconnect_6: reconnecting into stdout FAILED!" << std::endl;
        std::cout << " reconnect_6: reconnecting into stdout FAILED!" << std::endl;
        return false;
    }
    std::cout << " reconnect_6: reconnected into stdout!" << std::endl;
    return ret > 0;
}

//
// Read a file and write the contents to given stream
//
bool file_to_stream(const char* fname, ostream& aStream, int skip) {
    std::cout << " Method file_to_stream() BUFRDC implementation" << std::endl;

    const int MAX_LINE_LEN = 512;
    char supposedToBeANewLine;
    string myTmpFileName;

    myTmpFileName = redirect_dir + "/" + fname;

    ifstream myTmpFile(myTmpFileName.c_str());

    if (!myTmpFile) {
        aStream << "Can not read file " << (const char*)myTmpFileName.c_str() << std::endl;
        return false;
    }

    char myLine[MAX_LINE_LEN];

    myTmpFile.get(myLine, MAX_LINE_LEN, '\n');
    myTmpFile.get(supposedToBeANewLine);
    int i = 0;
    while (myTmpFile && !myTmpFile.eof()) {
        if (i++ >= skip)
            aStream << myLine << std::endl;

        myTmpFile.get(myLine, MAX_LINE_LEN, '\n');
        myTmpFile.get(supposedToBeANewLine);
    }

    myTmpFile.close();

    return true;
}

void delete_print_file(const char* name) {
    std::cout << " Method delete_print_file() BUFRDC implementation" << std::endl;

    string fname = redirect_dir + "/" + name;
    unlink(fname.c_str());
}

void eraseWhiteSpaceFromStringEnd(string& str) {
    std::cout << " Method eraseWhiteSpaceFromStringEnd() BUFRDC implementation" << std::endl;

    static string whitespaces(" \t\f\v\n\r");
    size_t found = str.find_last_not_of(whitespaces);
    if (found != string::npos)
        str.erase(found + 1);
    else
        str.clear();
}

string intToString(int i) {
    std::cout << " Method intToString() BUFRDC implementation" << std::endl;

    stringstream out;
    out << i;
    return out.str();
}

string floatToString(float f) {
    std::cout << " Method floatToString() BUFRDC implementation" << std::endl;

    stringstream out;
    out << f;
    return out.str();
}

void keyToStringMap(map<string, string>& data, string keyName, int* keyArray, int fortIndex) {
    std::cout << " Method keyToStringMap(1) BUFRDC implementation" << std::endl;

    data[keyName] = intToString(keyArray[fortIndex - 1]);
}

void keyToStringMap(map<string, string>& data, string keyName, float keyValue) {
    std::cout << " Method keyToStringMap(2) BUFRDC implementation" << std::endl;

    data[keyName] = floatToString(keyValue);
}
#endif  // MV_BUFRDC_TEST


#ifdef MV_BUFRDC_TEST
// ec ---------------------------------------------------------
// temporary functions

void TEMPCHECKVALUEDOUBLE(double myValue, double myValue1, const string& name, long descriptor) {
    double dd    = 1.e+20;
    double delta = 0.001;
    // if ( myValue > kBufrMissingValue-dd && myValue1 > kFortranBufrMissingValue-dd )
    if (myValue > dd && myValue1 > dd)
        return;

    if (myValue > dd || myValue1 == kBufrMissingValue) {
        printf("Different values but I think bufrdc is wrong -> continue: %s %ld %f %f \n", name.c_str(), descriptor,
               myValue, myValue1);
        return;
    }

    if (fabs(myValue - myValue1) > delta) {
        if (descriptor == 5001 || descriptor == 5002 || descriptor == 6001 || descriptor == 6002) {
            printf("Different values but I think bufrdc is wrong -> continue: %s %ld %f %f \n", name.c_str(),
                   descriptor, myValue, myValue1);
            return;
        }
        printf("Different values: %s %ld %f %f \n", name.c_str(), descriptor, myValue, myValue1);
        exit(0);
    }
}

void TEMPCHECKVALUELONG(long myValue, long myValue1, bool isLevel) {
    if (myValue == CODES_MISSING_LONG && myValue1 == kFortranBufrMissingIntValue)
        return;

    // if it is Level values related, BUFRDC uses hardcoded value 07004 for
    // parameter "pressure". However, ecCodes also uses other descriptor values,
    // e.g. 10004, which also translate to parameter "pressure". Therefore,
    // in some cases (synop, for example) function numberOfPressureLevels returns
    // 0 in BUFRDC and 1 in ecCodes (descriptor 10004 is translated to key "pressure").
    if (isLevel) {
        if (myValue != myValue1 && myValue != (myValue1 + 1)) {
            printf("Different values (Pressure level): %ld %ld \n", myValue, myValue1);
            exit(0);
        }
        else
            return;
    }
    if (myValue < myValue1 || myValue > myValue1) {
        printf("Different values: %ld %ld \n", myValue, myValue1);
        exit(0);
    }
}

void TEMPCHECKVALUESTRING(string& myValue, string& myValue1) {
#if 0
// remove blanks and transform to lowercase
string c1;
for(int i=0; i<myValue.size(); i++)
{
   char cc = myValue[i];
   if (cc != ' ')
      c1 += tolower(cc);
}

// remove blanks and transform to lowercase
string c2;
for(int i=0; i<myValue1.size(); i++)
{
   char cc = myValue1[i];
   if (cc != ' ')
      c2 += tolower(cc);
}

// remove occurrence tag, if exists
std::size_t found = c1.find("#1#");
if (found!=std::string::npos)
   c1 = c1.substr(found+3);

if ( c1 != c2 )
{
   if ( (c1 == "centre" && c2 == "identificationoforiginating/generatingcentre") ||
        c2.find("latitude") != std::string::npos ||
        c2.find("longitude") != std::string::npos ||
        (c1 == "blocknumber" && c2 == "wmoblocknumber") ||
        (c1 == "stationnumber" && c2 == "wmostationnumber") ||
        c1.find("codetable") != std::string::npos ||
        c1.find("stationtype") != std::string::npos
      )
      return;

   std::cout << "STRINGS DIFFERENTS: " << c1.c_str() << " " << c2.c_str() << std::endl;
   exit(0);
}
#endif
}

#endif  // MV_BUFRDC_TEST
