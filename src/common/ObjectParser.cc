/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   ObjectParser.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   Jun 2012

#include <locale>
#include <codecvt>

#include "ObjectParser.h"
#include "Translator.h"
#include "Value.h"

namespace magics {


ObjectParser::~ObjectParser() {}

//----------------------------------------------------------------------------------------------------------------------

Value ObjectParser::parseTrue() {
    consume("true");
    return Value(true);
}

Value ObjectParser::parseFalse() {
    consume("false");
    return Value(false);
}

Value ObjectParser::parseNull() {
    consume("null");
    return Value();
}

Value ObjectParser::parseNumber() {
    bool real   = false;
    bool string = false;

    std::string s;
    char c = next();
    if (c == '-') {
        s += c;
        c = next();
    }

    switch (c) {
    case '0':
        s += c;
        break;
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        s += c;
        while (isdigit(peek())) {
            s += next();
        }
        break;
    default:
        throw StreamParser::Error(std::string("ObjectParser::parseNumber invalid char '") + c + "'");
    }

    if (peek() == '.') {
        real = true;
        s += next();
        c = next();
        if (!isdigit(c))
            throw StreamParser::Error(std::string("ObjectParser::parseNumber invalid char '") + c + "'");
        s += c;
        while (isdigit(peek())) {
            s += next();
        }
    }


    c = peek();
    if (c == 'e' || c == 'E') {
        real = true;
        s += next();

        c = next();
        if (c == '-' || c == '+') {
            s += c;
            c = next();
        }

        if (!isdigit(c))
            throw StreamParser::Error(std::string("ObjectParser::parseNumber invalid char '") + c + "'");
        s += c;
        while (isdigit(peek())) {
            s += next();
        }
    }

    if (string) {
        return Value(s);
    }

    if (real) {
        double d = Translator<std::string, double>()(s);
        return Value(d);
    }
    else {
        long long d = Translator<std::string, long long>()(s);
        return Value(d);
    }
}

static std::string utf8(uint32_t code) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
    return conv.to_bytes( char32_t(code));
}

std::string ObjectParser::unicode() {

    std::string tmp;

    while(true) {
        char c = peek();

        if ( (c >= '0' && c <= '9') ||
             (c >= 'a' && c <= 'f') ||
             (c >= 'A' && c <= 'F')
            ) {
            consume(c);
            tmp += c;
        }
        else {
            break;
        }
    }


    std::istringstream iss(tmp);
    uint32_t code;
    iss >> std::hex >> code;

    // std::cout << " [" << code << ", " << utf8(code) << "]" << std::endl;

    return utf8(code);
}

Value ObjectParser::parseString(char quote) {
    consume(quote);
    std::string s;
    for (;;) {
        char c = next(true);

        if (c == '\\') {
            c = next(true);
            switch (c) {

            case '\\':
                s += '\\';
                break;

            case '/':
                s += '/';
                break;

            case 'b':
                s += '\b';
                break;

            case 'f':
                s += '\f';
                break;

            case 'n':
                s += '\n';
                break;

            case 'r':
                s += '\r';
                break;

            case 't':
                s += '\t';
                break;

            case 'u':
                s += unicode();
                break;

            default:
                if (c == quote) {
                    s += c;
                }
                else {
                    throw StreamParser::Error(std::string("ObjectParser::parseString invalid escaped char '") + c +
                                              "'");
                }
                break;
            }
        }
        else {
            if (c == quote) {
                return Value(s);
            }
            s += c;
        }
    }
}

static void set_(ValueMap& m, ValueList& l, const Value& k, const Value& v) {

    if (m.find(k) == m.end()) {
        l.push_back(k);
    }

    m[k] = v;
}

void ObjectParser::parseKeyValue(ValueMap& m, ValueList& l) {
    Value k = parseString();
    consume(':');
    Value v = parseValue();

    set_(m, l, k, v);
}

Value ObjectParser::parseObject() {
    consume("{");
    char c = peek();
    if (c == '}') {
        consume(c);
        return Value::makeOrderedMap();
    }

    ValueMap m;
    ValueList l;

    for (;;) {

        parseKeyValue(m, l);

        char c = peek();
        if (c == '}') {
            consume(c);
            return Value::makeOrderedMap(m, l);
        }

        consume(',');
    }
}

Value ObjectParser::parseArray() {
    consume("[");
    char c = peek();
    if (c == ']') {
        consume(c);
        // cout << "ObjectParser::parseArray <== " << std::endl;;
        return Value::makeList();
    }

    ValueList l;
    for (;;) {

        l.push_back(parseValue());

        char c = peek();
        if (c == ']') {
            consume(c);
            // cout << "ObjectParser::parseArray <== " << std::endl;;
            return Value::makeList(l);
        }

        consume(',');
    }
}


Value ObjectParser::parseJSON() {
    char c = peek();
    switch (c) {

    case 't':
        return parseTrue();
    case 'f':
        return parseFalse();
    case 'n':
        return parseNull();
    case '{':
        return parseObject();
    case '[':
        return parseArray();
    case '\"':
        return parseString();

    case '-':
        return parseNumber();
    case '0':
        return parseNumber();
    case '1':
        return parseNumber();
    case '2':
        return parseNumber();
    case '3':
        return parseNumber();
    case '4':
        return parseNumber();
    case '5':
        return parseNumber();
    case '6':
        return parseNumber();
    case '7':
        return parseNumber();
    case '8':
        return parseNumber();
    case '9':
        return parseNumber();

    default: {
        std::ostringstream oss;
        oss << parserName() << " ObjectParser::parseValue unexpected char ";
        if (isprint(c) && !isspace(c)) {
            oss << "'" << c << "'";
        }
        else {
            oss << int(c);
        }
        throw StreamParser::Error(oss.str());
    }
    }
}


ObjectParser::ObjectParser(std::istream& in, bool comments) : StreamParser(in, comments) {}

Value ObjectParser::parse() {
    Value v = parseValue();
    char c  = peek();
    if (c != 0) {
        std::ostringstream oss;
        oss << parserName() << " ObjectParser::parseValue extra char ";
        if (isprint(c) && !isspace(c)) {
            oss << "'" << c << "'";
        }
        else {
            oss << int(c);
        }
        throw StreamParser::Error(oss.str());
    }
    return v;
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
