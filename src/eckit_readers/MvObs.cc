/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

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

//____________________________________________________________________
//====================================================================== MvBufrOut
// This class is kept for backwards compatibility. At the
// moment, ecCodes is only using method "add". This method is called from
// class MvObsSet and contains only one command which calls back class
// MvObsSet. Maybe, this class can be removed in the future.
//______________________________________________________________________

MvBufrOut::MvBufrOut(MvObsSet* aSet) : _outSet(aSet) {}

MvBufrOut::~MvBufrOut() {
    //   _outSet->close();  // the owner should be responsible to close this file
}

void MvBufrOut::add(MvObs& anObs) {
    _outSet->write(anObs);
}

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

MvObs::MvObs(MvEccHandle_ptr ecH, int subset_current, bool unpacked, bool cacheCompressedData) :
    _subsetNr(subset_current), _unpacked(unpacked), cacheCompressedData_(cacheCompressedData), _ecH(ecH) {
    if (_ecH && _ecH->handle())
        init();
}

MvObs::MvObs(const MvObs& obs) {
    _copy(obs);
}

//___________________________________________________________________Destructor
MvObs::~MvObs() {
    clear();
}

//___________________________________________________________________ _copy
void MvObs::_copy(const MvObs& b) {
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
    // Delete iterator
    if (_ecH && _ecH->handle() && _ecIter) {
        codes_bufr_keys_iterator_delete(_ecIter);
        _ecIter = 0;
    }

    // Delete handle/iterator/buffer related to a subset
    if (_bufferSS)
        _bufferSS = 0;

    if (_ecH && _ecH->handle() && _ecHSS) {
        codes_handle_delete(_ecHSS);
        _ecHSS = nullptr;
    }

    _ecH.reset();
}

//___________________________________________________________________ operator=
MvObs& MvObs::operator=(const MvObs& b) {
    clear();
    _copy(b);
    return *this;
}

//___________________________________________________________________ operator void*
MvObs::operator void*() {
    return (_ecH) ? (_ecH->handle()) : nullptr;
}

//___________________________________________________________________ operator!
bool MvObs::operator!() {
    return !(_ecH && _ecH->handle());
}

codes_handle* MvObs::getHandle() const {
    return (_ecH) ? (_ecH->handle()) : nullptr;
}

//___________________________________________________________________ msg_ok
//
bool MvObs::msg_ok() const {
    return (_ecH && _ecH->handle());
}

//___________________________________________________________________ Advance
bool MvObs::Advance() {
    _subsetNr++;
    return _subsetNr <= _number_of_subsets;
}

//____________________________________________________________________ operator[]
double MvObs::operator[](int index)  //-- index starts from 1: 1,2,...,n
{
    std::cout << "MvObs::operator[] -> not yet implemented" << std::endl;
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
    return myValue;
}

double MvObs::value(long aDescriptor) {
    // Build key and get value
    string skey    = this->key(aDescriptor);
    double myValue = value(skey);
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
    codes_get_size(_ecH->handle(), skey.c_str(), &nelems);

    // No elements found
    if (nelems == 0)
        return kBufrMissingValue;

    // There is only one element
    double dvalue = CODES_MISSING_DOUBLE;
    if (nelems == 1) {
        codes_get_double(_ecH->handle(), skey.c_str(), &dvalue);
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

        codes_get_size(_ecH->handle(), sskey.c_str(), &nelems);
        if (nelems == 1)  // get the unique element
        {
            codes_get_double(_ecH->handle(), sskey.c_str(), &dvalue);
            if (cacheCompressedData_) {
                compressedData_.addDoubleData(sskey, dvalue);
            }
            return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
        }

        // retrieve the element related to the current subset number
        double* v1 = new double[nelems];
        codes_get_double_array(_ecH->handle(), sskey.c_str(), v1, &nelems);
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
            codes_get_size(_ecH->handle(), sskey.c_str(), &nn);
            if (nn == 0)
                sskey = skey;  // retrieve using the original key
            else if (nn == 1) {
                codes_get_double(_ecH->handle(), sskey.c_str(), &dvalue);
                return dvalue == CODES_MISSING_DOUBLE ? kBufrMissingValue : dvalue;
            }
            else
                nelems = nn;
        }

        double* v1 = new double[nelems];
        codes_get_double_array(_ecH->handle(), sskey.c_str(), v1, &nelems);
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
    codes_get_size(_ecH->handle(), keyName.c_str(), &valLen);

    // No elements found
    if (valLen == 0)
        return;

    // There is only one element
    double val = CODES_MISSING_DOUBLE;
    if (valLen == 1) {
        codes_get_double(_ecH->handle(), keyName.c_str(), &val);
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
                codes_get_size(_ecH->handle(), rKeyName.c_str(), &valLen);

                if (valLen == 0)
                    break;

                // Single value
                if (valLen == 1) {
                    codes_get_double(_ecH->handle(), rKeyName.c_str(), &val);
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
                    codes_get_double_array(_ecH->handle(), rKeyName.c_str(), valArr, &valLen);
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

        codes_get_size(_ecH->handle(), rKeyName.c_str(), &valLen);
        assert(!valArr);
        if (valLen == 1) {
            codes_get_double(_ecH->handle(), rKeyName.c_str(), &val);
            vals.push_back((val == CODES_MISSING_DOUBLE) ? kBufrMissingValue : val);
        }
        // Array
        else {
            assert(!valArr);
            valArr    = new double[valLen];
            valArrNum = valLen;
            codes_get_double_array(_ecH->handle(), rKeyName.c_str(), valArr, &valLen);
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
    return myValue;
}

long MvObs::intValue(const long aDescriptor) {
    // Build key and get value
    string skey  = this->key(aDescriptor);
    long myValue = intValue(skey);
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
    codes_get_size(_ecH->handle(), skey.c_str(), &nelems);

    // No elements found
    if (nelems == 0)
        return kBufrMissingIntValue;

    // There is only one element
    long value = CODES_MISSING_LONG;
    if (nelems == 1) {
        codes_get_long(_ecH->handle(), skey.c_str(), &value);
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
        codes_get_size(_ecH->handle(), sskey.c_str(), &nelems);
        if (nelems == 1)  // get the unique element
        {
            codes_get_long(_ecH->handle(), sskey.c_str(), &value);
            if (cacheCompressedData_) {
                compressedData_.addLongData(sskey, value);  // add to cache
            }
            return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
        }

        // retrieve the element related to the current subset number
        long* v1 = new long[nelems];
        codes_get_long_array(_ecH->handle(), sskey.c_str(), v1, &nelems);
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
            codes_get_size(_ecH->handle(), sskey.c_str(), &nn);
            if (nn == 0)
                sskey = skey;  // retrieve using the original key
            else if (nn == 1) {
                codes_get_long(_ecH->handle(), sskey.c_str(), &value);
                return value == CODES_MISSING_LONG ? kBufrMissingIntValue : value;
            }
            else
                nelems = nn;
        }

        long* v1 = new long[nelems];
        codes_get_long_array(_ecH->handle(), sskey.c_str(), v1, &nelems);
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
    codes_get_size(_ecH->handle(), keyName.c_str(), &valLen);

    // No elements found
    if (valLen == 0)
        return;

    // There is only one element
    long val = CODES_MISSING_LONG;
    if (valLen == 1) {
        codes_get_long(_ecH->handle(), keyName.c_str(), &val);
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
                codes_get_size(_ecH->handle(), rKeyName.c_str(), &valLen);

                if (valLen == 0)
                    break;

                // Single value
                if (valLen == 1) {
                    codes_get_long(_ecH->handle(), rKeyName.c_str(), &val);
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
                    codes_get_long_array(_ecH->handle(), rKeyName.c_str(), valArr, &valLen);
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

        codes_get_size(_ecH->handle(), rKeyName.c_str(), &valLen);
        assert(!valArr);
        if (valLen == 1) {
            codes_get_long(_ecH->handle(), rKeyName.c_str(), &val);
            vals.push_back((val == CODES_MISSING_LONG) ? kBufrMissingIntValue : val);
        }
        // Array
        else {
            assert(!valArr);
            valArr    = new long[valLen];
            valArrNum = valLen;
            codes_get_long_array(_ecH->handle(), rKeyName.c_str(), valArr, &valLen);
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
    return myValue;
}

string MvObs::stringValue(const long aDescriptor) {
    // Build key and get value
    string skey    = this->key(aDescriptor);
    string myValue = stringValue(skey);
    return myValue;
}

string MvObs::stringValue(const string& key, const int occurrence) {
    // Build key and get value
    string skey = this->key(key, occurrence);
    return stringValue(skey);
}

string MvObs::stringValue(const string& skeyi) {
    // skeyi could be a numerical descriptor coded as a string
    string skey = keyC(skeyi);

    // Check input key
    if (skey.empty())
        return string("");

    // Get number of elements
    size_t nelems;
    codes_get_size(_ecH->handle(), skey.c_str(), &nelems);

    // No elements found
    if (nelems == 0)
        return string("");

    // There is only one element
    char buf[1024];
    size_t len = 1024;
    if (nelems == 1) {
        codes_get_string(_ecH->handle(), skey.c_str(), buf, &len);
        // buf[len] = 0;  //???
        if (buf[0] == -1)  // missing value - cannot convert to string
            return string("");
        else
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
        codes_get_size(_ecH->handle(), sskey.c_str(), &nelems);
        if (nelems == 1)  // get the unique element
        {
            codes_get_string(_ecH->handle(), sskey.c_str(), buf, &len);
            // buf[len] = 0;  //???
            return string(buf);
        }

        // There are number_of_subsets values
        cValues = new char*[nelems];
        for (unsigned int i = 0; i < nelems; ++i)
            cValues[i] = new char[isize];

        // Get all values and select the required one
        size_t itotal = isize * nelems;
        codes_get_string_array(_ecH->handle(), sskey.c_str(), cValues, &itotal);
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
            codes_get_size(_ecH->handle(), sskey.c_str(), &nn);
            if (nn == 0)
                sskey = skey;  // retrieve using the original key
            else if (nn == 1) {
                codes_get_string(_ecH->handle(), sskey.c_str(), buf, &len);
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
        codes_get_string_array(_ecH->handle(), sskey.c_str(), cValues, &itotal);
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
    codes_get_size(_ecH->handle(), keyName.c_str(), &valLen);

    // No elements found
    if (valLen == 0)
        return;

    // There is only one element
    std::string val;
    char buf[1024];
    std::size_t sLen = 1024;
    if (valLen == 1) {
        codes_get_string(_ecH->handle(), keyName.c_str(), buf, &sLen);
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

            codes_get_size(_ecH->handle(), rKeyName.c_str(), &valLen);

            if (valLen == 0)
                break;

            // Single value
            if (valLen == 1) {
                codes_get_string(_ecH->handle(), rKeyName.c_str(), buf, &sLen);
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
                codes_get_string_array(_ecH->handle(), rKeyName.c_str(), valArr, &sTotal);
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

        codes_get_size(_ecH->handle(), rKeyName.c_str(), &valLen);
        assert(!valArr);
        if (valLen == 1) {
            codes_get_string(_ecH->handle(), rKeyName.c_str(), buf, &sLen);
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
            codes_get_string_array(_ecH->handle(), rKeyName.c_str(), valArr, &sTotal);
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
            codes_set_long(_ecH->handle(), "skipExtraKeyAttributes", 1);
        }
        codes_set_long(_ecH->handle(), "unpack", 1);
        _unpacked = true;
    }

    // Initialise iterator
    _ecIter = codes_bufr_data_section_keys_iterator_new(_ecH->handle());
    if (!_ecIter) {
        std::cout << "ERROR MvObs::setFirstDescriptor() -> Unable to create BUFR keys iterator" << std::endl;
        return false;
    }

    // Set first key/descriptor
    if (!setNextDescriptor())
        return false;

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
    if (!_unpacked && _ecH && _ecH->handle()) {
        if (useSkipExtraAttributes_) {
            codes_set_long(_ecH->handle(), "skipExtraKeyAttributes", 1);
        }
        codes_set_long(_ecH->handle(), "unpack", 1);
        _unpacked = true;
    }
}

long MvObs::currentDescriptor() {
    string skey     = _currentKey + "->code";
    long descriptor = intValue(skey);
    return descriptor;
}

const std::string& MvObs::currentKey() {
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
    return myValue;
}

double MvObs::nextValue() {
    std::cout << "MvObs :: nextValue() -> not yet implemented" << std::endl;
    exit(0);
}

MvObs MvObs::cloneSubset(long subset_number) {
    if (!_ecH || !_ecH->handle())
        return MvObs(nullptr);

    // Check if the input subset number is vali
    if (subset_number > msgSubsetCount()) {
        std::cout << "ERROR MvObs::cloneSubset() -> invalid input subset number" << std::endl;
        return MvObs(nullptr);
    }

    if (_ecHSS) {
        codes_handle_delete(_ecHSS);
        _ecHSS    = 0;
        _bufferSS = 0;
    }

    // Clone, unpack and extract that particular subset
    // h2 is a temporary handle; it will be deleted at the end of this function
    codes_handle* h2 = codes_handle_clone(_ecH->handle());
    assert(h2);
    codes_set_long(h2, "skipExtraKeyAttributes", 1);
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
    h2     = 0;
    auto p = std::make_shared<MvEccHandle>(_ecHSS);
    return MvObs(p, 1);
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
    return itype;
}

int MvObs::elementValueType(const string& skeyi) {
    // skeyi could be a numerical descriptor coded as a string
    string skey = keyC(skeyi);

    int itype;
    codes_get_native_type(_ecH->handle(), skey.c_str(), &itype);

    return itype;
}

int MvObs::elementValueType() {
    //   return elementValueType(_currentKey);
    int itype = elementValueType(_currentKey);
    return itype;
}

//______________________________________________________ numberOfPressureLevels
int MvObs::numberOfPressureLevels() {
    int npl = numberOfLevels(sPressureCoordinate);
    return npl;
}

//______________________________________________________ numberOfLevels
int MvObs::numberOfLevels(long levelDescriptor) {
    // Build key and get value
    string skey = this->key(levelDescriptor);
    int nelems  = numberOfLevels(skey);
    return nelems;
}

//______________________________________________________ numberOfLevels
int MvObs::numberOfLevels(const string& skey) {
    // Get number of elements
    size_t nelems;
    codes_get_size(_ecH->handle(), skey.c_str(), &nelems);

    //   _currentLevelOccurrence = 0;

    return (int)nelems;
}

//__________________________________________________________ firstPressureLevel
double MvObs::firstPressureLevel() {
    return pressureLevel(1);
}

//______________________________________________________________ pressureLevel
// Parameter indexValue must start from 1...N
double MvObs::pressureLevel(int indexValue) {
    _currentLevelKey        = sPressureCoordinate;
    _currentLevelOccurrence = indexValue;
    double myLevelValue     = level(_currentLevelKey, _currentLevelOccurrence);

    return myLevelValue == kBufrMissingValue ? kBufrMissingValue : myLevelValue / 100.;
}

//___________________________________________________________ nextPressureLevel
double MvObs::nextPressureLevel() {
    double myLevelValue = pressureLevel(_currentLevelOccurrence + 1);
    return myLevelValue;
}

//__________________________________________________________ firstLevel
double MvObs::firstLevel(long levelDescriptor) {
    string skey         = key(levelDescriptor);
    double myLevelValue = firstLevel(skey);
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
    return myLevelValue;
}

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
    return value;
}

double MvObs::valueBySpecifier(const string& coordinateKey, double coordinateValue, const string& paramKey) {
    // This is a temporary solution while the above code is not available
    // Find a coordinate key whose value matches the input value
    this->setFirstDescriptor();  // initialise key iterator
    double precision = 0.01;     // 0.000001;  // maybe needs to be adjusted
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
    return value;
}

double MvObs::valueByLevelRange(const std::string& s1key, float level1, float level2, const std::string& s2key) {
    // Get number of elements
    size_t nelems, len;
    codes_get_size(_ecH->handle(), s1key.c_str(), &nelems);

    // Allocate memory for the values to be read
    double* dlevels = new double[nelems];

    // Get values
    double myValue = kBufrMissingValue;
    len            = nelems;
    int ierr       = codes_get_double_array(_ecH->handle(), s1key.c_str(), dlevels, &len);
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
    return kBufrMissingValue;  //-- Not Found or Troubled Msg --
}

//______________________________________________________________ printAllValues
bool MvObs::printAllValues() {
    std::ostream* myStream = &std::cout;
    return writeAllValues(*myStream);
}

//_______________________________________________________________ writeAllValues
bool MvObs::writeAllValues(std::ostream& aStream) {
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
                if (dval == kBufrMissingValue)
                    aStream << "   ~~~";
                else
                    aStream << dval;

                break;
            case CODES_TYPE_STRING:
                aStream.width(6);
                aStream.fill(' ');
                aStream << stringValue();
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

//______________________________________________________________ WmoIdentNumber
long MvObs::WmoIdentNumber() {
    return WmoBlockNumber() * 1000 + WmoStationNumber();
}

//______________________________________________________________ WmoBlockNumber
int MvObs::WmoBlockNumber() {
    long myValue = intValue("blockNumber");
    return myValue == kBufrMissingIntValue ? 0 : (int)myValue;
}

//____________________________________________________________ WmoStationNumber
int MvObs::WmoStationNumber() {
    long myValue = intValue("stationNumber");
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

    int idVals = sizeof(idList) / sizeof(idList[0]);
    for (int i = 0; i < idVals; ++i) {
        string descr = idList[i];
        long myValue = intValue(descr);
        if (myValue != kBufrMissingIntValue) {
            std::ostringstream oss;
            oss << std::setw(5) << std::setfill('0') << myValue;
            return oss.str();
        }
    }

    return string("id???");
}

//____________________________________________________________________ location
MvLocation MvObs::location() {
    // F NEW CODE
    // ERROR   ERROR   ERROR    ERROR
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

    return myLocation;
}

//____________________________________________________________________ unit
string MvObs::unit(long aDescriptor) {
    string skey  = key(aDescriptor) + "->units";
    string sunit = stringValue(skey);
    return sunit;
}

//____________________________________________________________________ unit
string MvObs::unit() {
    string skey  = _currentKey + "->units";
    string sunit = stringValue(skey);
    return sunit;
}

//____________________________________________________________________ name
string MvObs::name(long aDescriptor) {
    string skey = key(aDescriptor);
    return skey;
}

//____________________________________________________________________ name
string MvObs::name() {
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

    return (int)_messageTotalLen;
}

//___________________________________________________________ editionNumber
int MvObs::editionNumber() {
    if (_editionNumber == -1)
        _editionNumber = intValue("edition");

    return (int)_editionNumber;
}

//_______________________________________________________________ msgSubsetCount
int MvObs::msgSubsetCount() {
    if (_number_of_subsets == -1)
        _number_of_subsets = intValue("numberOfSubsets");

    // Variable initialised at init()
    return (int)_number_of_subsets;
}

//_________________________________________________________________ messageType
int MvObs::messageType() {
    if (_messageType == -1)
        _messageType = intValue("dataCategory");

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

    return (int)_subTypeInternational;
}

//_________________________________________________________ messageSubtypeLocal
int MvObs::messageSubtypeLocal() {
    if (_subTypeLocal == -1)
        _subTypeLocal = intValue("dataSubCategory");

    return (int)_subTypeLocal;
}

//___________________________________________________________ originatingCentre
int MvObs::originatingCentre() {
    if (_originatingCentre == -1)
        _originatingCentre = intValue("bufrHeaderCentre");

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

    return (int)_originatingSubCentre;
}

//___________________________________________________________ masterTable
int MvObs::masterTable() {
    if (_masterTable == -1)
        _masterTable = intValue("masterTableNumber");

    return (int)_masterTable;
}

//___________________________________________________________ masterTableVersion
int MvObs::masterTableVersion() {
    if (_masterTableVersion == -1)
        _masterTableVersion = intValue("masterTablesVersionNumber");

    return (int)_masterTableVersion;
}

//___________________________________________________________ localTableVersion
int MvObs::localTableVersion() {
    if (_localTableVersion == -1)
        _localTableVersion = intValue("localTablesVersionNumber");

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
    }

    // Return time
    return TDynamicTime((short)_lyear, (short)_lmonth, (short)_lday, (short)_lhour, (short)_lminute, 0);
}
//----------------------------------------------------------------------------

TDynamicTime MvObs::obsTime() {
    // Get values
    long lyear   = intValue("year");
    long lmonth  = intValue("month");
    long lday    = intValue("day");
    long lhour   = intValue("hour");
    long lminute = intValue("minute");
    long lsecond = intValue("second");

    // Check values
    lminute = (lminute == kBufrMissingIntValue) ? 0 : lminute;
    lsecond = (lsecond == kBufrMissingIntValue) ? 0 : lsecond;

    // NCEP PrepBUFR may not contain date/time information
    if (lyear == kBufrMissingIntValue || lmonth == kBufrMissingIntValue || lday == kBufrMissingIntValue)
        return this->msgTime();  // take date from section 1

    return TDynamicTime((short)lyear, (short)lmonth, (short)lday, (short)lhour, (short)lminute, (short)lsecond);
}


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
    kiter                           = codes_bufr_keys_iterator_new(_ecH->handle(), 0);
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
        err          = codes_get_long(_ecH->handle(), name_code.c_str(), &codeVal);
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
    unsigned char* str = (unsigned char*)' ';
    return str;
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

MvBufrConfidence ::MvBufrConfidence(int aSubsetNr) {
    std::cout << " Method MvBufrConfidence::MvBufrConfidence() not implemented yet" << std::endl;
    // e   exit(0);
}
//___________________________________________________________ ~MvBufrConfidence

MvBufrConfidence ::~MvBufrConfidence() {
    // e   std::cout << " Method MvBufrConfidence::~MvBufrConfidence() not implemented yet" << std::endl;
    // e   exit(0);
}
//______________________________________________________________ hasConfidences
bool MvBufrConfidence ::hasConfidences() {
    std::cout << " Method MvBufrConfidence::hasConfidences() not implemented yet" << std::endl;
    exit(0);
}
//_______________________________________________________________ confidence
int MvBufrConfidence ::confidence(long aDescr) {
    std::cout << " Method MvBufrConfidence::confidence() not implemented yet" << std::endl;
    exit(0);
}
//___________________________________________________________ confidenceByIndex
int MvBufrConfidence ::confidenceByIndex(int aDataInd) {
    std::cout << " Method MvBufrConfidence::confidenceByIndex() not implemented yet" << std::endl;
    exit(0);
}
//_______________________________________________________________ lastDataIndex
int MvBufrConfidence ::lastDataIndex() {
    std::cout << " Method MvBufrConfidence::lastDataIndex() not implemented yet" << std::endl;
    exit(0);
}
//__________________________________________________________ startOfDataPresent
// Q&D hack !!!

int MvBufrConfidence ::startOfDataPresent() {
    std::cout << " Method MvBufrConfidence::startOfDataPresent() not implemented yet" << std::endl;
    exit(0);
}
//_______________________________________________________________ startOfConfidences
// Q&D hack !!!

int MvBufrConfidence ::startOfConfidences() {
    std::cout << " Method MvBufrConfidence::startOfConfidences() not implemented yet" << std::endl;
    exit(0);
}
//_______________________________________________________________ delta
int MvBufrConfidence ::delta(int anInd) {
    std::cout << " Method MvBufrConfidence::delta() not implemented yet" << std::endl;
    exit(0);
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
