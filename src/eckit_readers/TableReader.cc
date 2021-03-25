/***************************** LICENSE START ***********************************

 Copyright 2016 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#include "TableReader.h"


void TableReader::resizeDecoders(unsigned int numNeeded) {
    // do we need to resize our list of decoders?

    if (numNeeded > decoderSets_.size()) {
        decoderSets_.resize(numNeeded);
        names_.resize(numNeeded, NULL);
        types_.resize(numNeeded, TABFIELD_NUMBER);
        namesAll_.resize(numNeeded, "");
    }
}


void TableReader::setFieldContainer(int index, string& name, vector<double>& container, double outputMissingIndicator) {
    if (index == -1)  // is the user wanting to find a field with the given name?
    {
        int foundIndex = indexOfField(name);  // try to find it

        if (foundIndex == -1)  // not found?
        {
            setError("Cannot find field with name " + name + " -  will not be stored.");
            return;
        }

        else {
            index = foundIndex;  // we found it - so use this index
        }
    }


    // do we need to resize our list of decoders to accommodate this one?

    resizeDecoders(index + 1);  // e.g. if index==2, then we need 3 decoders (0-indexed)


    // add the decoder to the appropriate place in our list

    TableDoubleVectorElementDecoder* decoder = new TableDoubleVectorElementDecoder(container, outputMissingIndicator);
    TableElementDecoder* genericDecoder      = dynamic_cast<TableElementDecoder*>(decoder);
    decoderSets_[index].push_back(genericDecoder);  //.push_back(genericDecoder);
    names_[index] = &name;
}


void TableReader::setFieldContainer(int index, string& name, vector<string>& container, string outputMissingIndicator) {
    if (index == -1)  // is the user wanting to find a field with the given name?
    {
        int foundIndex = indexOfField(name);  // try to find it

        if (foundIndex == -1)  // not found?
        {
            setError("Cannot find field with name " + name + " -  will not be stored.");
            return;
        }

        else {
            index = foundIndex;  // we found it - so use this index
        }
    }

    // do we need to resize our list of decoders to accommodate this one?

    resizeDecoders(index + 1);  // e.g. if index==2, then we need 3 decoders (0-indexed)


    // add the decoder to the appropriate place in our list

    TableStringVectorElementDecoder* decoder = new TableStringVectorElementDecoder(container, outputMissingIndicator);
    TableElementDecoder* genericDecoder      = dynamic_cast<TableElementDecoder*>(decoder);
    decoderSets_[index].push_back(genericDecoder);  //.push_back(genericDecoder);
    names_[index] = &name;
}


// ---------------------------------------------------------------------------
// TableReader::splitLine
// takes a text line and finds all the delimited strings within it.
//  NOTE: this function is destructive to the input string!
// ---------------------------------------------------------------------------

void TableReader::splitLine(char* line, vector<char*>& tokens) {
    char* current = line;
    char* token   = current;

    while (*current != '\0') {
        if (*current == delimiter_)  // reached the end of a token?
        {
            *current = '\0';          // yes - terminate the token string
            tokens.push_back(token);  // store a pointer to it
            token = current + 1;      // start the next token
        }

        current++;  // advance to the next character
    }

    tokens.push_back(token);  // store a pointer to the last token
}


// ---------------------------------------------------------------------------
// TableReader::splitLineConsecutiveDelimiters
// Takes a text line and finds all the delimited strings within it.
// Version which allows consecutive delimiters to be considered as one.
//  NOTE: this function is destructive to the input string!
// ---------------------------------------------------------------------------

void TableReader::splitLineConsecutiveDelimiters(char* line, vector<char*>& tokens) {
    char* current = line;
    char* token   = current;

    if (*current == '\0')  // return nothing if it is a blank line
        return;


    while (*current != '\0') {
        if (*current == delimiter_)  // reached the end of a token?
        {
            *current = '\0';  // yes - terminate the token string

            current++;                                          // quick look at the next character
            while (*current != '\0' && *current == delimiter_)  // skip all consecutive delimiters
                current++;
            current--;  // step back one character

            if (token[0] != '\0')         // when treating consecutive delimiters as one, we do not have missing values
                tokens.push_back(token);  // store a pointer to it
            token = current + 1;          // start the next token
        }

        current++;  // advance to the next character
    }

    if (token[0] != '\0')         // safeguard against stray delimiters at the end of the line
        tokens.push_back(token);  // store a pointer to the last token
}


void TableReader::ensureHaveMetaData() {
    if (!gotMetaData_)  // caller should have retrieved the meta-data already
    {
        string msg;

        if (!getMetaData(msg)) {
            setError(msg);
        }
    }
}

vector<TableReader::eTableReaderFieldType>& TableReader::fieldTypes() {
    ensureHaveMetaData();

    return types_;
}


vector<string>& TableReader::fieldNames() {
    ensureHaveMetaData();

    return namesAll_;
}


map<string, string>& TableReader::userMetaData() {
    ensureHaveMetaData();

    return userMetaData_;
}


int TableReader::numRecords() {
    ensureHaveMetaData();

    return numRecords_;
}


int TableReader::indexOfField(string& name) {
    ensureHaveMetaData();


    // find which field has the name we're looking for

    for (vector<string*>::size_type i = 0; i < names_.size(); i++) {
        if (namesAll_[i] == name)
            return i;  // found it - return its index
    }


    // if we got this far, then we did not find it

    return -1;
}


// ---------------------------------------------------------------------------
// TableReader::guessFieldType
// takes an element and tries to decide which data type it is, e.g. number
// ---------------------------------------------------------------------------

TableReader::eTableReaderFieldType TableReader::guessFieldType(char* str) {
    if (str) {
        char* pch = str;

        while (*pch) {
            int ch = *pch;  // any character not part of a proper number format means this is a string

            if (!isdigit(ch) && (ch != 'e') && (ch != 'E') && (ch != '.') && (ch != '+') && (ch != '-'))
                return TableReader::TABFIELD_STRING;

            pch++;
        }
    }

    return TableReader::TABFIELD_NUMBER;
}


int TableReader::nextLineTokens(char* line, size_t sizeOfLine, vector<char*>& tokens) {
    // skip blank lines

    /*    bool gotLine = true;
        line[0] = '\0';

        while (line[0] == '\0' && gotLine)
        {
            gotLine = f_.getline(line, sizeOfLine);
            numBadLines_++;
        }
    */

    // read the next line into our buffer

    //    if (gotLine)
    if (f_.getline(line, sizeOfLine)) {
        // parse the line into tokens

        streamsize numread = f_.gcount();

        if ((numread >= 2) && (line[numread - 1] == '\0') &&
            (line[numread - 2] == '\r'))  // remove extraneous newline characters from Windows
            line[numread - 2] = '\0';

        if (consecutiveDelimitersAsOne_)
            splitLineConsecutiveDelimiters(line, tokens);
        else
            splitLine(line, tokens);

        return true;
    }

    else {
        return 0;
    }
}


// ---------------------------------------------------------------------------
// TableReader::readUserMetaData
// reads all the lines of user meta-data and stores them in userMetaData_.
// Meta-data must be of the form
// PARAM1=VALUE1 PARAM2=VALUE2
// ---------------------------------------------------------------------------

bool TableReader::readUserMetaData(char* line, size_t sizeOfLine, string& errorMessage) {
    int currentRow      = 1;
    char oldDelimiter   = delimiter_;                   // store because we will over-ride it
    bool oldConsecutive = consecutiveDelimitersAsOne_;  // store because we will over-ride it

    // we parse each line in two steps:
    //   1) split into a vector of PARAMx=VALUEX strings
    //   2) split each of these into 2 strings and add them to the meta-data map

    setConsecutiveDelimitersAsOne(true);  // allows the user more flexibility

    for (vector<int>::size_type r = 0; r < userMetaDataRows_.size(); r++)  // for each line of meta-data...
    {
        skipLines(userMetaDataRows_[r] - currentRow, line, sizeOfLine);  // get to the right line
        setDelimiter(' ');


        vector<char*> tokens;

        if (nextLineTokens(line, sizeOfLine, tokens) > 0) {
            // now have one token per PARAM=VALUE - split each one into a pair

            setDelimiter('=');

            for (vector<string*>::size_type j = 0; j < tokens.size(); j++) {
                vector<char*> tokens2;

                splitLineConsecutiveDelimiters(tokens[j], tokens2);

                if (tokens2.size() != 2)  // should only be 2 tokens: PARAM and VALUE
                {
                    char msg[2000];
                    sprintf(msg, "Error parsing parameter %lu: '%s'", j + 1, tokens[j]);
                    errorMessage = msg;
                    return false;
                }
                else {
                    userMetaData_[tokens2[0]] = tokens2[1];
                }
            }
        }


        currentRow = userMetaDataRows_[r] + 1;  // store which line we're at
    }


    setDelimiter(oldDelimiter);                     // restore
    setConsecutiveDelimitersAsOne(oldConsecutive);  // restore

    return true;
}


// ---------------------------------------------------------------------------
// TableReader::skipLines
// reads and ignore the specified number of rows. Requires a buffer to read
// into.
// ---------------------------------------------------------------------------

void TableReader::skipLines(int linesToSkip, char* line, size_t sizeOfLine) {
    for (int i = 0; i < linesToSkip; i++) {
        f_.getline(line, sizeOfLine);
    }
}


// ---------------------------------------------------------------------------
// TableReader::getMetaData
// reads the first few lines of the file and determines the number and type
// of columns. Also, optionally, reads any lines of user meta-data from the
// file.
// ---------------------------------------------------------------------------

bool TableReader::getMetaData(string& errorMessage) {
    if (gotMetaData_)  // we should only need to get the meta-data once
        return true;


    // check for previous errors which we should report

    if (errorCode_) {
        errorMessage = errorMsg_;
        clearError();
        return false;
    }


    const int MAX_CHARS_IN_LINE = 1024 * 10;
    char line[MAX_CHARS_IN_LINE];


    // basic sanity checks

    if (path().empty()) {
        errorMessage = "TableReader: Path is empty - will not read.";
        return false;
    }


    // try to open the file for reading

    f_.open(path().c_str());

    if (f_.fail()) {
        errorMessage = "TableReader: Could not open table file: " + path();
        return false;
    }


    // count the number of lines

    int numLines = 0;

    while (f_.getline(line, sizeof(line)))  //-- first count the lines
        numLines++;

    numRecords_ = numLines - headerRow() - dataRowOffset() + 1;  // how many actual records are there?


    // rewind the file pointer before we start again

    f_.clear();
    f_.seekg(0, ios::beg);


    // read the user meta-data if it exists

    if (!userMetaDataRows_.empty()) {
        if (!readUserMetaData(line, sizeof(line), errorMessage)) {
            return false;
        }


        // rewind the file pointer before we start again

        f_.clear();
        f_.seekg(0, ios::beg);
    }


    // get to either the header row (if there is one) or the start of the data

    if (headerRow() > 0)  // there is a header row
        skipLines(headerRow() - 1, line, sizeof(line));
    else if (dataRowOffset() > 1)  // no header - get to start of data
        skipLines(dataRowOffset() - 1, line, sizeof(line));


    dataStart_ = f_.tellg();  // make a note of where the data starts (may be changed if we have a header line)


    int numLinesToCheck = 1 + (headerRow() > 0 ? 1 : 0);  // how many lines to read?

    if (numLinesToCheck > numLines)
        numLinesToCheck = numLines;


    for (int i = 0; i < numLinesToCheck; i++) {
        // read the line into a buffer and split it into tokens

        vector<char*> tokens;

        if (nextLineTokens(line, sizeof(line), tokens) > 0) {
            if (i == 0) {
                resizeDecoders(tokens.size());  // ensure our array of decoders is big enough to handle all the fields
            }

            if ((i == 0) && headerRow() > 0)  // parse a header row?
            {
                for (vector<string*>::size_type j = 0; j < tokens.size(); j++) {
                    namesAll_[j] = tokens[j];
                }

                skipLines(dataRowOffset() - 1, line, sizeof(line));  // go to start of data
                dataStart_ = f_.tellg();                             // make a note of where the data starts
            }

            else  // this is a data row
            {
                if (types_.size() != tokens.size()) {
                    errorMessage =
                        "Not the same number of elements in the first 2 lines of table. Check file format parameters.";
                    f_.close();  // close the input file
                    return false;
                }


                // determine which data type each token is

                for (vector<eTableReaderFieldType>::size_type j = 0; j < tokens.size(); j++) {
                    types_[j] = guessFieldType(tokens[j]);
                }
            }
        }
    }


    gotMetaData_ = true;

    return true;
}


bool TableReader::read(string& errorMessage) {
    const int MAX_CHARS_IN_LINE = 1024 * 10;
    char line[MAX_CHARS_IN_LINE];


    // check for previous errors which we should report

    if (errorCode_) {
        errorMessage = errorMsg_;
        clearError();
        return false;
    }


    // basic sanity checks

    if (path().empty()) {
        errorMessage = "TableReader: Path is empty - will not read.";
        return false;
    }

    if (decoderSets_.size() == 0) {
        errorMessage = "TableReader: No decoders installed - will not read.";
        return false;
    }


    if (!getMetaData(errorMessage))  // ensure we have the meta-data. This will also open the file (f_)
        return false;


    // set the header names?

    for (vector<string*>::size_type j = 0; j < namesAll_.size(); j++) {
        if (names_[j] != NULL) {
            *(names_[j]) = namesAll_[j];
        }
    }


    // initialise our containers to hold the correct number of elements

    //    for (i = 0 ; i < decoders_.size(); i++)
    for (vector<TableElementDecoders>::iterator decoderSet = decoderSets_.begin(); decoderSet != decoderSets_.end();
         ++decoderSet) {
        for (TableElementDecoders::iterator decoder = (*decoderSet).begin(); decoder != (*decoderSet).end();
             ++decoder) {
            if (*decoder != NULL) {
                (*decoder)->initialise(numRecords_);
            }
        }
    }


    // rewind the file pointer before we start again

    f_.clear();
    f_.seekg(dataStart_, ios::beg);  // set to start of the data (determined by getMetaData())


    vector<char*> tokens;

    for (int i = 0; i < numRecords_; i++) {
        // read the line into a buffer and split it into tokens

        tokens.clear();

        if (nextLineTokens(line, sizeof(line), tokens) > 0) {
            if (tokens.size() != decoderSets_.size())  // only do something if we have the right number of tokens
            {
                if (tokens.size() == 0)  // blank line?
                {
                    // if we are only reading one column, then it could be a missing value
                    if (decoderSets_.size() == 1) {
                        char* empty = "";
                        tokens.push_back(empty);
                    }

                    // otherwise ignore it  (is this too generous of us??)
                    else {
                        continue;
                    }
                }
                else  // otherwise, the disparity in numbers indicates a problem
                {
                    char msg[128];
                    sprintf(msg, "TableReader: record %d has %u elements, but the first record has %u. Failed to read.",
                            i + 1, (unsigned int)tokens.size(), (unsigned int)decoderSets_.size());
                    errorMessage = msg;
                    f_.close();  // close the input file
                    return false;
                }
            }


            // decode the data with their various decoders - one set of decoders for each field

            vector<char*>::iterator s = tokens.begin();

            for (vector<TableElementDecoders>::iterator decoderSet = decoderSets_.begin();
                 decoderSet != decoderSets_.end(); ++decoderSet) {
                for (TableElementDecoders::iterator decoder = (*decoderSet).begin(); decoder != (*decoderSet).end();
                     ++decoder) {
                    if (*decoder != NULL) {
                        // string str(*s);
                        (*decoder)->addValue(*s);
                    }
                }
                s++;
            }
        }
    }

    f_.close();  // close the input file

    return true;
}
