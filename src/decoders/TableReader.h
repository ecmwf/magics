/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

/*******************************  LICENSE  *******************************


 Copyright 2011 European Centre for Medium-Range Weather Forecasts (ECMWF)
 
 Licensed under the Apache License, Version 2.0 (the "License"); 
 you may not use this file except in compliance with the License. 
 You may obtain a copy of the License at 
 
 	http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software 
 distributed under the License is distributed on an "AS IS" BASIS, 
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 See the License for the specific language governing permissions and 
 limitations under the License.


 *******************************  LICENSE  *******************************/

#ifndef TableReader_H
#define TableReader_H

#include <vector>
#include <fstream>
#include <cstdlib>
#include <string>
#include <map>

using namespace std;


/*! -----------------------------------------------------------------
    \class TableFormatAttributes
    Base lass to handle the different ways of specifying how a table
    file is formatted.
   ----------------------------------------------------------------- */

class TableFormatAttributes
{
public:

    TableFormatAttributes () : delimiter_(','), headerRow_(1), consecutiveDelimitersAsOne_(false), dataRowOffset_(0) {}

    void setPath          (string &path)     { path_          = path;      }
    void setPath          (const char *path) { path_          = string(path); }
    void setDelimiter     (char delimiter)   { delimiter_     = delimiter; }
    void setHeaderRow     (int  headerRow)   { headerRow_     = headerRow; }
    void setDataRowOffset (int  offset)      { dataRowOffset_ = offset; }
    void setConsecutiveDelimitersAsOne (bool consecutive) { consecutiveDelimitersAsOne_ = consecutive; }
    void setUserMetaDataRows (vector<int> &rows){ userMetaDataRows_ = rows; }

    string path ()                     { return path_;      }
    int    headerRow()                 { return headerRow_; }
    int    dataRowOffset()             { return dataRowOffset_; }
    vector<int> userMetaDataRows()     { return userMetaDataRows_; }


protected       :
    string path_;
    char delimiter_;
    int  headerRow_;
    bool consecutiveDelimitersAsOne_;
    int  dataRowOffset_;
    vector<int> userMetaDataRows_;
};




/*! -----------------------------------------------------------------
    \class TableElementDecoder
    Base class to handle the decoding of a single element in a table
    (e.g. CSV). Derived classes should handle the decoding of
    particular data types (e.g. double, string).
   ----------------------------------------------------------------- */

class TableElementDecoder
{
public:
    TableElementDecoder() : currentIndex_(0) {};
    virtual ~TableElementDecoder(){}

    virtual void initialise    (int numValues) = 0;
    virtual void addValue      (char  *value)  = 0;
    //virtual void decodeElement (char *element) = 0;

protected:
    int currentIndex_;
};


/*! -----------------------------------------------------------------
    \class TableDoubleVectorElementDecoder
    Derived class to handle the decoding of numeric data from a 
    table file. The data will be put into a vector of doubles.
   ----------------------------------------------------------------- */

class TableDoubleVectorElementDecoder : public TableElementDecoder
{
public:
    TableDoubleVectorElementDecoder(vector<double>& target, double outMiss) : target_(target), outputMissingIndicator_(outMiss)  {};

    void initialise (int numValues) { target_.reserve(numValues); }
    void addValue   (char *value)   { target_.push_back((value[0] != '\0') ? atof(value) : outputMissingIndicator_); }

private:
    vector<double>& target_;
    double outputMissingIndicator_;
};



/*! -----------------------------------------------------------------
    \class TableStringVectorElementDecoder
    Derived class to handle the decoding of string data from a 
    table file. The data will be put into a vector of strings.
   ----------------------------------------------------------------- */

class TableStringVectorElementDecoder : public TableElementDecoder
{
public:
    TableStringVectorElementDecoder(vector<string>& target, string outMiss) : target_(target), outputMissingIndicator_(outMiss) {};

    void initialise (int numValues) { target_.reserve(numValues); }
    void addValue   (char *value)   { target_.push_back((value[0] != '\0') ? value : outputMissingIndicator_); }

private:
    vector<string>& target_;
    string outputMissingIndicator_;
};


typedef vector<TableElementDecoder *> TableElementDecoders;  // for shorthand



/*! -----------------------------------------------------------------
    \class TableReader
    Handles the reading of a table file, such as CSV.
    To use it, set its path and any other formatting attributes
    as available in the TableFormatAttributes class, and then
    for each field (column) you wish to store, supply a container
    such as a vector of numbers. Only those fields for which you
    supply a container will be read. Fields can be specified by
    index (0-based) or by name.

   ----------------------------------------------------------------- */

class TableReader : public TableFormatAttributes
{
public:

    enum eTableReaderFieldType {TABFIELD_NUMBER, TABFIELD_STRING};

    TableReader ()              {gotMetaData_ = false; errorCode_ = 0;}
    TableReader (string &path)  {setPath(path);}

    void setFieldContainer (int index, string &name, vector<double>& container, double outputMissingIndicator);
    void setFieldContainer (int index, string &name, vector<string>& container, string outputMissingIndicator);

    // setFieldContainer (string &name, vector<double>);
    // setFieldContainer (string &name, vector<string>);

    bool getMetaData(string &errorMessage);
    bool read (string &errorMessage);  // read and parse the file; the supplied cointainers will be filled

    vector<eTableReaderFieldType>& fieldTypes();
    vector<string>&                fieldNames();
    map<string, string>&           userMetaData();
    int                            numRecords();

private:

    void resizeDecoders (unsigned int numNeeded);
    int  nextLineTokens (char *line, size_t sizeOfLine, vector<char *>& tokens);  // reads the next line and splits into tokens
    void splitLine      (char *line, vector<char *>& tokens);                     // splits a line into tokens based on the current settings
    void splitLineConsecutiveDelimiters (char *line, vector<char *>& tokens);     // splits a line into tokens based on the current settings
    bool readUserMetaData(char *line, size_t sizeOfLine, string &errorMessage);   // reads user meta-data into a map
    int  indexOfField   (string &name);                                           // returns the index of the field with a given name (or -1)
    void setError       (string msg)  {errorCode_ = 1; errorMsg_ = msg;};         // set an error message to be used later
    void clearError     ()            {errorCode_ = 0;};                          // clear error message and code to be used later
    void ensureHaveMetaData();                                                    // loads the meta-data if not already there
    void skipLines      (int linesToSkip, char *line, size_t sizeOfLine);         // skips a user-defined number of rows
    TableReader::eTableReaderFieldType guessFieldType(char *str);                 // tries to determine whether the field is, for example, string or number

    vector<TableElementDecoders>  decoderSets_; // each column can have multiple decoders attached to it
    vector<string *>              names_;
    vector<string>                namesAll_;
    vector<eTableReaderFieldType> types_;
    bool                          gotMetaData_;
    ifstream                      f_;
    streampos                     dataStart_;
    int                           numRecords_;
    map<string, string>           userMetaData_;
    int                           errorCode_;  // some functions will set these to be used later
    string                        errorMsg_;   // some functions will set these to be used later

};

#endif
