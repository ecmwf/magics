/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#ifndef MvBufrObs_DEFINED
#define MvBufrObs_DEFINED

#include "MvLocation.h"
#include "fmettim.h"

#include <eccodes.h>

#include <map>
#include <memory>
#include <sstream>
#include <vector>

#ifdef METVIEW
#include "MvDate.h"
#include "MvRequest.h"
#endif

#ifdef ECCODES_UI  //#ifndef METVIEW   //ECCODES_UI
#include "FortranTypes.h"
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------- MvBufrConfidence
// Q&D hack for QC Feed Back using descr operator 222000; vk Apr-95

//! \enum ESelectFilterState Used internally by MvBufrConfidence
enum EBufrConfState
{
    kBCS_unknown,
    kBCS_missing,
    kBCS_exists
};

//------------------------------------------------------------------------------
//! Auxiliary class to handle quality control feedback
/*! Warning: This is Q&C ("quick&dirty") hack to access
 *  QC FB data behind operator descriptor 222000.
 */
class MvBufrConfidence {
public:
    MvBufrConfidence(int aSubsetNr);
    ~MvBufrConfidence();

    bool hasConfidences();
    int confidence(long aDescriptor);
    int confidenceByIndex(int anIndex);
    int lastDataIndex();

protected:
    int startOfDataPresent();
    int startOfConfidences();
    int delta(int anIndex);

private:
    int _subsetNr;
    int _startOfDataPresent;
    int _startOfConfidences;
    EBufrConfState _state;
};
#endif  // DOXYGEN_SHOULD_SKIP_THIS

const float kBufrMissingValue  = 1.7e38;
const int kBufrMissingIntValue = 2147483647;

class MvEccHandle {
public:
    MvEccHandle(codes_handle* ecH) : ecH_(ecH) {}
    codes_handle* handle() const { return ecH_; }
    void clear() { ecH_ = nullptr; }

protected:
    codes_handle* ecH_;
};

typedef std::shared_ptr<MvEccHandle> MvEccHandle_ptr;


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

class MvBufrEdition;

class MvBufrSubsetData {
    friend class MvObs;

public:
    MvBufrSubsetData() {}
    void clear() {
        longData_.clear();
        doubleData_.clear();
    }
    bool isEmpty() const { return longData_.empty() && doubleData_.empty(); }

    void addLongData(const std::string& key, long val);
    void addLongData(const std::string& key, long* val, size_t num);
    void addDoubleData(const std::string& key, double val);
    void addDoubleData(const std::string& key, double* val, size_t num);

    const std::vector<long>& longData(const std::string& key) const;
    const std::vector<double>& doubleData(const std::string& key) const;

protected:
    std::map<std::string, std::vector<long> > longData_;
    std::map<std::string, std::vector<double> > doubleData_;
};

class MvObs {
    friend class MvObsSet;
    friend class MvObsSetIterator;
    void _copy(const MvObs& b);

public:
    //! Constructors
    /*! Arguments are mainly used by MvObsSet and MvObsSetIterator.
     *  Applications normally call this constructor without arguments.
     */
    MvObs(MvEccHandle_ptr, int subset_current = 1, bool unpacked = false, bool cacheCompressedData = true);
    MvObs() {}

    //! Copy constructor
    MvObs(const MvObs&);

    //! Destructor
    ~MvObs();

    //! Assignment operator
    MvObs& operator=(const MvObs&);

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

    // Clear/initialise variable members
    void clear();

    // Clone one subset
    MvObs cloneSubset(long);

    //! Initialise some variables
    void init();

    codes_handle* getHandle() const;
    //    {
    //        return *_ecH;
    //    }

    //    void setHandle(codes_handle** h)
    //    {
    //        _ecH = h;
    //    }

    //----------------------------------------------------------------------------
    //-- APIs for requesting info from the Header, BUFR Section 0,1,2 --//

    //! Returns the total length of the message from BUFR section 0
    int messageTotalLen();

    //! Returns the Edition number from BUFR section 0
    int editionNumber();

    //! Returns the number of subsets available in this BUFR message
    int msgSubsetCount();

    //! Returns the current subset number (in a multisubset BUFR message)
    //! Indexing start at 1.
    int subsetNumber() const { return _subsetNr; }

    //! Returns the message type code
    int messageType();

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
    int messageSubtype();

    //! Returns the locally defined Message Subtype code from BUFR section 1
    int messageSubtypeLocal();

    //! Returns the international Message Subtype code (WMO defined) from BUFR section 1
    /*! Note that this code is available only in BUFR Edition 4 messages. For Edition 3
     *  messages a value of 255 is returned (255 corresponds to an octet with all bits '1').
     */
    int messageSubtypeInternational();

    int messageRdbtype();

    //! Returns the Originating Centre code from BUFR section 1
    int originatingCentre();
    const std::string& originatingCentreAsStr();

    //! Returns the Originating Subcentre code from BUFR section 1
    int originatingSubCentre();

    //! Returns the Master Table code from BUFR section 1
    /*! Master Table 0 is for Meteorology, 10 is for Oceanography
     */
    int masterTable();

    //! Returns the Master Table version from BUFR section 1
    int masterTableVersion();

    //! Returns the Local Table version from BUFR section 1
    int localTableVersion();

    //! Returns the time
    TDynamicTime msgTime();

    //----------------------------------------------------------------------------

    //! Returns the time from the observation report (from the data: BUFR section 4)
    TDynamicTime obsTime();
    TDynamicTime obsTime(int occurrence);

    //! Advances to the next subset in a multisubset BUFR message
    /*! Returns 'false' if current subset is the last (or the only)
     *  one. Returns 'true' on success.
     */
    bool Advance();

    //! Checks whether this MvObs contains a BUFR message or is empty
    bool operator!();

    //! Checks that this MvObs contains a valid decodable BUFR message
    // ec maybe can be removed?
    bool msg_ok() const;  // { return _bufr_id == _bufrIn->currentBufrId(); }

    // ----  A P I   f u n c t i o n s :  ---- //

    //-- APIs for requesting parameter values --//

    //! Returns the value of an element defined by 'aDescriptor'
    /*! Also sets Current Descriptor.
     *  Missing value indicator is returned if the observation report
     *  does not contain elements of type 'aDescriptor', or if the element
     *  does not have value.
     */
    double valueC(const std::string&);  // input can be a number (descriptor) or a key
    double value(const long, const int occurrence);
    double value(const long);
    double value(const std::string&, const int);
    double value(const std::string&);
    void allValues(const std::string& keyName, std::vector<double>& vals);

    //! Returns the value of an element defined by 'aDescriptor' as 'long'
    /*! Also sets Current Descriptor.
     */
    long intValue(const long, const int occurrence);
    long intValue(const long);
    long intValue(const std::string&, const int);
    long intValue(const std::string&);
    void allIntValues(const std::string& keyName, std::vector<long>& vals);

    //! Returns the value of Current Descriptor as a long
    long currentIntValue();

    //! Returns the value of an element defined by 'aDescriptor' as 'string'
    /*! Also sets Current Descriptor.
     */
    std::string stringValue(const long, const int occurrence);
    std::string stringValue(const long);
    std::string stringValue(const std::string&, const int);
    std::string stringValue(const std::string&);
    void allStringValues(const std::string& keyName, std::vector<std::string>& vals);

    //! Returns the value of Current Descriptor as a string
    std::string stringValue();

    //! Returns the value corresponding to the 'n'th occurrence of descriptor 'descr'
    /*! Method searches the current subset from the beginning looking
     *  for descriptors 'descr'. If BUFR message contains 'n' (or more)
     *  occurrences of descriptor 'descr', the corresponding data value
     *  is returned, otherwise 'kBufrMissingValue' is returned.
     */
    double valueByOccurrenceC(int, const std::string&);  // input can be a number (descriptor) or a key
    double valueByOccurrence(int, const std::string&);
    double valueByOccurrence(int, long);

    //! Index access operator returns the 'n'th data value
    /*! This operator treats BUFR message as an array
     *  and returns the value of the 'n'th element as 'double'.
     *  Index 'n' starts from '1', i.e. n=1,2,3,...
     */
    double operator[](int n);  //-- n starts from 1: 1,2,...,n

    //-- APIs for requesting parameter metadata --//

    // Returns the name of the data referenced by 'aDescriptor' (from BUFR Table B)
    std::string name(long);

    // Returns the name of the data referenced by Current Descriptor
    std::string name();

    // Returns the unit of the data referenced by 'aDescriptor' (from BUFR Table B)
    std::string unit(long);

    // Returns the unit of the data referenced by Current Descriptor
    std::string unit();

    // Returns the compress data indicator (true: compress, false: uncompress)
    bool compressData() { return _compressed_data; }

    // Expand message
    void expand();

    //------------------------------------------------------------------
    // APIs for iteracting through all parameters within a message
    //------------------------------------------------------------------

    void clearIterator();

    // Sets the first expanded descriptor in the message as current Descriptor
    // Returns 'false' in case of decoding problems
    bool setFirstDescriptor(bool skipConfidence = true);

    // Advances Current Descriptor to the next expanded descriptor in the message
    // Returns 'false' if message contains no more data (no more descriptors)
    bool setNextDescriptor();

    // Returns current Descriptor/Key/Value
    long currentDescriptor();
    const std::string& currentKey();
    const std::string currentKeyWithoutRank();
    double currentValue();

    // Returns the value of the next element with the same descriptor
    // Uses Current Descriptor to start looking for the next occurrence
    // of the same descriptor later in the message.
    //  Updates Current Descriptor.
    //  Returns 'kBufrMissingValue' if no such element is found
    //  or if such an element has no value.
    double nextValue();

    //-- APIs for requesting parameter types --//

    // Returned values: CODES_TYPE_LONG, CODES_TYPE_DOUBLE, CODES_TYPE_STRING,
    //                  CODES_TYPE_MISSING, CODES_TYPE_UNDEFINED

    //! Returns the type of data related to descriptor 'aDescriptor'
    int elementValueType(long);
    int elementValueType(const std::string&);

    //! Returns the type of data related to Current Descriptor
    int elementValueType();

    //! Returns the location from the observation report (from the data: BUFR section 4)
    MvLocation location();

    //-- APIs for requesting weather station values --//
    //! Returns the 5 digit WMO station identifier
    /*! Looks for WMO block number 'BB' and station identifier 'SSS'
     *  and returns the value of 'BBSSS' if found. Returns zero if
     * 'BB' or 'SSS' is not found.
     */
    long WmoIdentNumber();

    //! Returns the 2 digit WMO block identifier 'BB'
    /*! Returns zero if 'BB' not found.
     */
    int WmoBlockNumber();

    //! Returns the 3 digit WMO station identifier 'SSS'
    /*! Returns zero if 'SSS' not found.
     */
    int WmoStationNumber();

    //! Returns the the value of the "ident" key from ECMWF (centre=98) local section
    /*! Returns an empty string if "ident" is not defined.
     */
    const std::string& headerIdent();

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
    std::string findSomeIdent();

    //-- APIs for accessing replicated parameters --//

    //! Returns the number of times level coordinator descriptor 'levelDescriptor' is found
    int numberOfLevels(long);
    int numberOfLevels(const std::string&);

    //! Returns the first value related to 'levelDescriptor', i.e. value of the first level
    double firstLevel(long);
    double firstLevel(const std::string&);

    //! Returns the value of the next level related to Current Level Descriptor
    double nextLevel();

    //! Returns the value of the specified data for the specified level
    /*! First looks for a data block related to level 'aLevel' as the
     *  value of the data element related to level descriptor 'aLevelDescriptor',
     *  and then looks for a data element related to descriptor 'aDescriptor'
     *  within this level.
     */
    double valueByLevelC(const std::string&, float, const std::string&);  // input can be a number (descriptor) or a key
    double valueByLevel(long, float, long);
    double valueByLevel(const std::string&, float, const std::string&);

    double valueByLevelRangeC(const std::string&, float, float,
                              const std::string&);  // input can be a number (descriptor) or a key
    double valueByLevelRange(long, float, float, long);
    double valueByLevelRange(const std::string&, float, float, const std::string&);

    //! Returns the number of pressure levels found in the observation report
    /*! Pressure level descriptor is 007004, key "pressure", so this is an
     * alias for member function numberOfLevels("pressure").
     */
    int numberOfPressureLevels();

    //! Returns the value of the first pressure level in hPa
    /*! Original pressure values are stored in Pa, so this is
     *  the same as 100.0*firstLevel(7004).
     */
    double firstPressureLevel();

    //! Returns the value of the next pressure level in hPa
    /*! Original pressure values are stored in Pa, so this is
     *  the same as 100.0*nextLevel(7004).
     */
    double nextPressureLevel();

    //! Returns the value of the data corresponding to 'aDescriptor' on level 'aLevel'
    /*! Here 'aLevel' is given in hPa. Original pressure values are stored in Pa,
     *  so this is the same as \n
     * <PRE>
     *      valueByLevel( 7004, 100.0*aLevel, aDescriptor )
     * </PRE>
     */
    double valueByPressureLevelC(float, const std::string&);  // input can be a number (descriptor) or a key
    double valueByPressureLevel(float, long);
    double valueByPressureLevel(float, const std::string&);

    // e This function needs to be updated later.
    //! Returns the value of the data corresponding to 'aDescriptor' in a layer
    /*! Level values are for the top and the bottom pressure values of a layer
     *  and they are given in hPa. \n \n
     *  This method looks for two consecutive pressure coordinate descriptors
     *  007004 with the given values (hPa is first converted to Pa). If such
     *  a layer is found and the layer contains 'aDescriptor' then the corresponding
     *  data value is returned, otherwise 'kBufrMissingValue' is returned.
     */
    float valueByLayerC(float, float, const std::string&);  // input can be a number (descriptor) or a key
    float valueByLayer(float, float, long);

    //! Prints all data values into standard output
    /*! For output format see method 'writeAllValues' below
     */
    bool printAllValues();

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
    bool writeAllValues(std::ostream&);

    //! Writes all data values into file 'aPathName'
    /*! For output format see the version of method 'writeAllValues' above
     */
    bool writeAllValues(const char*);

    //! Returns 'true' if BUFR message contains local section 2, 'false' if not
    bool hasSection2();

    //-- APIs for accessing confidence values --//

    //! Checks whether the BUFR message contains confidence values or not
    /*! Returns 'true' if operator descriptor 222000 is found in the
     *  message, otherwise 'false'.
     */
    bool hasConfidences();

    //! Returns the confidence value for the current data, if exists
    /*! Otherwise returns -1.
     */
    int confidence();

    //-- Q&D: valueBySpecifier & specifierIndex made public
    //--      so that ObsPicker can use them freely (vk/Jul)
    double valueBySpecifier(long, double, long, int firstIndexValue = 0);
    double valueBySpecifier(const std::string&, double, const std::string&);

#ifdef METVIEW
    //! Decodes OPERA BUFR radar data into unsigned char array
    unsigned char* OperaRadarImage(/* <aki> add arguments? */);
    //! Retrieves metadata for OPERA radar image
    bool OperaRadarMetadata(/* <aki> add arguments? */);
#endif

    static std::string keyWithoutOccurrenceTag(const std::string&);
    static int occurenceFromKey(const std::string& key);

    // APIs for converting Descriptors to keys
    std::string keyC(const std::string&, const int occurrence = -1);

private:
    bool writeValues(std::ostream&, int, int);

    double level(const std::string&, int);
    double pressureLevel(int);

    // APIs for converting Descriptors to keys
    std::string key(const int, const int occurrence = -1);
    std::string key(const std::string&, const int occurrence = -1);
    bool descriptorToKey(const long, std::string&);
    bool descriptor_to_key(const long, std::string&);

protected:
    // It is implemeted in MvMiscellanous, but because of magics we cannot
    // use  MvMisc ... here!
    template <typename TYPE>
    inline std::string toString(const TYPE& in) {
        std::ostringstream os;
        os << in;
        return os.str();
    }


    bool _skipConfidence{true};      // true: skip Confidence values
    std::string _currentKey;         // mainly to be used in the key iterator within a message
    std::string _currentLevelKey;    // Level parameter name
    int _currentLevelOccurrence{0};  // Level parameter occurrence (1..N)
    int _subsetNr{1};                // current subset (1.._number_of_subsets)

    // Info from the Header, BUFR Section 1
    bool _compressed_data{false};  // true: compressed, false: uncompress
    bool _unpacked{false};         // true: data unpacked, false: data packed
    long _messageTotalLen{-1};
    long _editionNumber{-1};
    long _number_of_subsets{-1};
    long _messageType{-1};
    long _subTypeInternational{-1};
    long _subTypeLocal{-1};
    long _rdbType{-1};
    long _originatingCentre{-1};
    long _originatingSubCentre{-1};
    std::string _originatingCentreStr;
    long _masterTable{-1};
    long _masterTableVersion{-1};
    long _localTableVersion{-1};
    long _lyear{-1};
    long _lmonth{-1};
    long _lday{-1};
    long _lhour{-1};
    long _lminute{-1};
    std::string headerIdent_{"__UNDEF__"};

    MvBufrEdition* _edition{nullptr};
    MvBufrConfidence* _confidence{nullptr};

    // use eccodes optimisation
    bool useSkipExtraAttributes_{true};

    // For compressed messages
    bool cacheCompressedData_{true};
    MvBufrSubsetData compressedData_;

    MvEccHandle_ptr _ecH;

    codes_handle* _ecHSS{nullptr};  // handle access to a subset.
                                    // function Destructor should delete it

    codes_bufr_keys_iterator* _ecIter{nullptr};  // eccodes key iterator within a message
    const void* _bufferSS{nullptr};              // auxiliary pointer
};


#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------- MvBufrParam

class MvBufrParam {
public:
    MvBufrParam(const char* aParamName);
    MvBufrParam(const long anIntAsDescriptor) { fDescriptor = anIntAsDescriptor; }

    long Descriptor(void) const { return fDescriptor; }
    void PrintAllKnownParameters(void) const;

    operator int(void) const { return fDescriptor; }

private:
    long fDescriptor;
};

#endif  // DOXYGEN_SHOULD_SKIP_THIS


//--------------------------------------------------------------- MvBufrOut
// A simple class capable of producing BUFR code only from 'MvObs'
// objects i.e. usable in filtering applications which read a
// BUFR file and write a new file with less messages...
//---------------------------------------------------------------

class MvObsSet;

class MvBufrOut {
    friend class MvObs;
    friend class MvObsSet;

protected:
    MvBufrOut(MvObsSet*);
    ~MvBufrOut(void);

    void add(MvObs& anObs);
    //   void  write( MvObs& anObs );   //FAMI20171023 maybe we do not need this function or maybe
    // we could remove the previous function (add) and use this one.

protected:
    MvObsSet* _outSet;
};

#endif  // MvBufrObs_DEFINED
