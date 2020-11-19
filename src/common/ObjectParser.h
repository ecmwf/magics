/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @date Jun 2012

#ifndef magics_ObjectParser_h
#define magics_ObjectParser_h

#include "StreamParser.h"
#include "Value.h"

namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class ObjectParser : public StreamParser {
public:  // methods
    virtual ~ObjectParser();

    virtual Value parse();

protected:
    ObjectParser(std::istream& in, bool comments, bool yaml);

protected:  // methods
    virtual Value parseTrue();
    virtual Value parseFalse();
    virtual Value parseNull();
    virtual Value parseValue() = 0;
    virtual Value parseObject();
    virtual Value parseArray();
    virtual Value parseString(char quote = '"');
    virtual Value parseNumber();

    virtual Value parseJSON();

    virtual void parseKeyValue(ValueMap&, ValueList&);

    virtual std::string parserName() const = 0;

private:
    std::string unicode();
    bool yaml_;
};


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics

#endif
