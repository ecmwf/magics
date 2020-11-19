/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   YAMLParser.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   Jun 2012

#include <fstream>

#include "Content.h"
#include "MagRegex.h"
#include "Translator.h"
#include "YAMLParser.h"


namespace magics {

static Value toValue(const std::string& s);

struct YAMLItem : public Counted {
    long indent_;
    Value value_;


    virtual void print(std::ostream& s) const = 0;

    virtual Value value(YAMLParser& parser) const = 0;

    virtual Value parse(YAMLParser& parser) const {
        attach();  // Don't get deleted
        Value v = value(parser);
        detach();
        return v;
    }

    YAMLItem(long indent = 0, const Value& value = Value()) : indent_(indent), value_(value) {}

    virtual ~YAMLItem() {
        // std::cout << "~YAMLItem " << value_ << std::endl;
    }

    friend std::ostream& operator<<(std::ostream& s, const YAMLItem& item) {
        item.print(s);
        return s;
    }


    virtual bool isStartDocument() const { return false; }
    virtual bool isEndDocument() const { return false; }
    virtual bool isEOF() const { return false; }
};


class YAMLItemLock {
    const YAMLItem* item_;

public:
    YAMLItemLock(const YAMLItem* item) : item_(0) { set(item); }
    ~YAMLItemLock() { set(0); }

    void set(const YAMLItem* item) {
        if (item != item_) {
            if (item_)
                item_->detach();
            item_ = item;
            if (item_)
                item_->attach();
        }
    }
};


struct YAMLItemEOF : public YAMLItem {
    virtual void print(std::ostream& s) const { s << "YAMLItemEOF"; }

    virtual Value value(YAMLParser& parser) const { return Value(); }

    YAMLItemEOF() : YAMLItem(-1) {}
    virtual bool isEOF() const { return true; }
};


struct YAMLItemStartDocument : public YAMLItem {
    virtual void print(std::ostream& s) const { s << "YAMLItemStartDocument"; }


    Value value(YAMLParser& parser) const {
        ValueList l;

        bool more = true;
        while (more) {
            l.push_back(parser.parseValue());

            for (;;) {
                const YAMLItem& next = parser.peekItem();
                if (next.isEOF()) {
                    more = false;
                    break;
                }

                if (!next.isEndDocument()) {
                    break;
                }

                parser.nextItem();
            }
        }

        if (l.size() == 1) {
            return l[0];
        }

        return Value::makeList(l);
    }


    YAMLItemStartDocument() : YAMLItem(-1) {}


    virtual bool isStartDocument() const { return true; }
};


struct YAMLItemValue : public YAMLItem {
    virtual void print(std::ostream& s) const {
        s << "YAMLItemValue[value=" << value_ << ", indent=" << indent_ << "]";
    }

    virtual Value value(YAMLParser& parser) const { return value_; }

    YAMLItemValue(size_t indent, const Value& value) : YAMLItem(indent, value) {}
};

struct YAMLItemAnchor : public YAMLItem {
    virtual void print(std::ostream& s) const {
        s << "YAMLItemAnchor[value=" << value_ << ", indent=" << indent_ << "]";
    }

    virtual Value value(YAMLParser& parser) const {
        Value v = parser.nextItem().value(parser);
        parser.anchor(value_, v);
        return v;
    }

    YAMLItemAnchor(size_t indent, const Value& value) : YAMLItem(indent, value) {}
};

struct YAMLItemReference : public YAMLItem {
    virtual void print(std::ostream& s) const {
        s << "YAMLItemReference[value=" << value_ << ", indent=" << indent_ << "]";
    }

    virtual Value value(YAMLParser& parser) const { return parser.anchor(value_); }

    YAMLItemReference(size_t indent, const Value& value) : YAMLItem(indent, value) {}
};

struct YAMLItemKey : public YAMLItem {
    virtual void print(std::ostream& s) const {
        for (long i = 0; i < indent_; i++) {
            s << ' ';
        }
        s << "YAMLItemKey[value=" << value_ << ", indent=" << indent_ << "]";
    }

    YAMLItemKey(YAMLItem* item) : YAMLItem(item->indent_, item->value_) {
        YAMLItemLock lock(item);  // Trigger deletion

        std::string v(value_);
        ASSERT(v.size());
        value_ = toValue(v.substr(0, v.size() - 1));
    }


    void set(ValueMap& m, ValueList& l, const Value& k, const Value& v, bool unique = false) const {
        // std::cout << "SET " << k << " " << v << std::endl;

        if (m.find(k) == m.end()) {
            l.push_back(k);
        }
        else {
            if (unique) {
                return;
            }
        }

        m[k] = v;
    }


    Value value(YAMLParser& parser) const {
        ValueMap _m;
        ValueList _l;

        YAMLItemLock lock(this);


        const YAMLItem* key = this;

        bool more = true;
        while (more) {
            const YAMLItem& next = parser.peekItem();
            YAMLItemLock nextLock(&next);

            // std::cout << "key " << *key << " next " << next << std::endl;

            if (next.indent_ == key->indent_) {
                // Special case
                set(_m, _l, key->value_, Value(), true);  // null

                key = &parser.nextItem();
                ASSERT(dynamic_cast<const YAMLItemKey*>(key));
                lock.set(key);
                continue;
            }

            if (next.indent_ < key->indent_) {
                // Special case
                set(_m, _l, key->value_, Value(), true);  // null

                more = false;
                continue;
            }

            if (next.indent_ > key->indent_) {
                static Value import("<<");
                Value k = key->value_;
                Value v = parser.nextItem().parse(parser);

                if (k == import) {
                    Value keys = v.keys();
                    for (size_t i = 0; i < keys.size(); ++i) {
                        Value k = keys[i];
                        set(_m, _l, k, v[k]);
                    }
                }
                else {
                    set(_m, _l, k, v);
                }
            }

            const YAMLItem& peek = parser.peekItem();
            // std::cout << "key " << *key << " peek " << peek << std::endl;

            if (peek.indent_ < key->indent_) {
                more = false;
                continue;
            }

            if (peek.indent_ == key->indent_) {
                key = &parser.nextItem();
                ASSERT(dynamic_cast<const YAMLItemKey*>(key));
                lock.set(key);
                continue;
            }

            if (next.value_.isString() && peek.indent_ > key->indent_ && peek.value_.isString()) {
                std::ostringstream oss;
                oss << next.value_;
                for (;;) {
                    const YAMLItem& peek = parser.peekItem();

                    if (!(peek.indent_ > key->indent_ && peek.value_.isString())) {
                        break;
                    }

                    oss << ' ' << parser.nextItem().value_;
                }

                set(_m, _l, key->value_, oss.str());
                continue;
            }


            std::ostringstream oss;
            oss << "Invalid sequence " << *key << " then " << next << " then " << peek << std::endl;
            throw MagicsException(oss.str());
        }

        return Value::makeOrderedMap(_m, _l);
    }
};


struct YAMLItemEntry : public YAMLItem {
    virtual void print(std::ostream& s) const { s << "YAMLItemEntry[indent=" << indent_ << "]"; }

    YAMLItemEntry(size_t indent) : YAMLItem(indent) {}

    Value value(YAMLParser& parser) const {
        ValueList l;

        YAMLItemLock lock(this);

        bool more = true;
        while (more) {
            const YAMLItem& next = parser.peekItem();

            if (next.indent_ == indent_) {
                // Special case
                l.push_back(Value());  // null
                const YAMLItem* advance = &parser.nextItem();
                ASSERT(dynamic_cast<const YAMLItemEntry*>(advance));
                continue;
            }

            if (next.indent_ < indent_) {
                // Special case
                l.push_back(Value());  // null
                more = false;
                continue;
            }

            if (next.indent_ > indent_) {
                l.push_back(parser.nextItem().parse(parser));
            }

            const YAMLItem& peek = parser.peekItem();

            if (peek.indent_ < indent_) {
                more = false;
                continue;
            }

            if (peek.indent_ == indent_) {
                const YAMLItem* advance = &parser.nextItem();
                // std::cout << "advance " << *advance << std::endl;
                ASSERT(dynamic_cast<const YAMLItemEntry*>(advance));
                continue;
            }

            std::ostringstream oss;
            oss << "Invalid sequence " << *this << " then " << next << " then " << peek << std::endl;
            throw MagicsException(oss.str());
        }

        return Value::makeList(l);
    }
};


struct YAMLItemEndDocument : public YAMLItem {
    virtual void print(std::ostream& s) const { s << "YAMLItemEndDocument"; }

    virtual Value value(YAMLParser& parser) const { return Value(); }

    YAMLItemEndDocument() : YAMLItem(-1) {}


    virtual bool isEndDocument() const { return true; }
};


YAMLParser::YAMLParser(std::istream& in) : ObjectParser(in, true, true), last_(0) {
    stop_.push_back(0);
    comma_.push_back(0);
    colon_.push_back(0);
}

YAMLParser::~YAMLParser() {
    for (std::deque<YAMLItem*>::iterator j = items_.begin(); j != items_.end(); ++j) {
        // magics::Log::warning() << "YAMLParser::~YAMLParser left over: " << *(*j) << std::endl;
        (*j)->detach();
    }
    if (last_) {
        last_->detach();
    }
}

Value YAMLParser::decodeFile(const std::string& path) {
    std::ifstream in(std::string(path).c_str());
    if (!in)
        throw CannotOpenFile(path);
    return YAMLParser(in).parse();
}

Value YAMLParser::decodeString(const std::string& str) {
    std::istringstream in(str);
    return YAMLParser(in).parse();
}

Value YAMLParser::parseString(char quote) {
    bool ignore;
    return parseStringOrNumber(ignore);
}


Value YAMLParser::parseNumber() {
    bool ignore;
    return parseStringOrNumber(ignore);
}


static Value toValue(const std::string& s) {
    static Regex real("^[-+]?[0-9]+\\.?[0-9]+([eE][-+]?[0-9]+)?$", false);
    static Regex integer("^[-+]?[0-9]+$", false);
    static Regex hex("^0x[0-9a-zA-Z]+$", false);
    static Regex octal("^0[0-9]+$", false);
    static Regex time("[0-9]+:[0-9]+:[0-9]+$", false);

    /*
    if (time.match(s)) {
        return Value(Time(s));
    }
    */

    // std::cout << "TO VALUE " << s << std::endl;

    if (octal.match(s)) {
        return Value(strtol(s.c_str(), 0, 0));
    }

    if (hex.match(s)) {
        return Value(strtol(s.c_str(), 0, 0));
    }

    if (integer.match(s)) {
        long long d = Translator<std::string, long long>()(s);
        return Value(d);
    }

    if (real.match(s)) {
        double d = Translator<std::string, double>()(s);
        return Value(d);
    }

    if (s == "null") {
        return Value();
    }

    if (s == "false") {
        return Value(false);
    }

    if (s == "true") {
        return Value(true);
    }

    if (s.length()) {
        ASSERT(s[0] != '"');
        ASSERT(s[0] != '\'');
    }


    return Value(s);
}


std::string YAMLParser::nextWord() {
    std::string word;
    char c = peek(true);

    while (!(::isspace(c) || c == 0 || c == '\n')) {
        word += next();
        c = peek(true);
    }

    return word;
}

Value YAMLParser::consumeJSON(char ket) {
    stop_.push_back(ket);
    comma_.push_back(',');
    colon_.push_back(':');
    Value v = parseJSON();
    stop_.pop_back();
    comma_.pop_back();
    colon_.pop_back();
    return v;
}


size_t YAMLParser::consumeChars(char which) {
    char c     = peek(true);
    size_t cnt = 0;

    while (c == which) {
        consume(which);
        c = peek(true);
        cnt++;
    }

    if (c == ' ' || c == '\n') {
        return cnt;
    }

    while (cnt-- > 0) {
        putback(which);
    }
    return 0;
}

bool YAMLParser::endOfToken(char c) {
    return (c == '\n' || c == 0 || c == stop_.back() || c == comma_.back() || c == colon_.back());
}

Value YAMLParser::parseStringOrNumber(bool& isKey) {
    bool multi  = false;
    bool folded = false;
    bool string = false;
    char c      = peek();

    if (c == '"' || c == '\'') {
        return ObjectParser::parseString(c);
    }

    if (c == '|') {
        consume('|');
        multi  = true;
        string = true;
    }

    if (c == '>') {
        consume('>');
        folded = true;
        string = true;
    }


    c             = peek();
    size_t indent = pos_;
    size_t line   = line_;

    std::string result;

    bool was_indented = false;

    while (pos_ >= indent) {
        size_t start    = pos_;
        bool add_cr     = (folded && (pos_ != indent)) || multi || was_indented;
        bool add_indent = (folded && (pos_ != indent)) || multi;
        std::string s;
        size_t last = 0;
        size_t i    = 0;

        bool colon = (c == ':');

        isKey = false;

        while (!endOfToken(c)) {
            char p = next(true);
            s += p;
            if (!::isspace(p)) {
                last = i;
            }
            c = peek(true);
            i++;

            // std::cout << "++++ " << s << " " << colon << " " << (endOfToken(c) || c == ' ') << std::endl;

            if (colon && (endOfToken(c) || c == ' ')) {
                while (s.length() >= 2 && isspace(s[s.length() - 2])) {
                    s = s.substr(0, s.length() - 1);
                    last--;
                    s[s.length() - 1] = ':';
                }

                // std::cout << s << std::endl;

                isKey = true;
                break;
            }

            colon = (c == ':');
        }

        if (result.size()) {
            if (add_cr) {
                for (size_t i = line; i < line_; i++) {
                    result += '\n';
                }
            }
            else {
                result += ' ';
            }
        }

        if (add_indent) {
            for (size_t i = indent; i < start; i++) {
                result += ' ';
            }
        }


        result += s.substr(0, last + 1);
        line = line_;

        if (isKey) {
            return Value(result);
        }


        c = peek();

        if (endOfToken(c)) {
            break;
        }

        was_indented = add_indent;
    }

    if (string) {
        for (size_t i = line; i < line_; i++) {
            result += '\n';
        }
        return Value(result);
    }

    return toValue(result);
}

void YAMLParser::loadItem() {
    if (!items_.empty()) {
        return;
    }

    char c = peek();

    size_t indent = pos_;

    YAMLItem* item = 0;
    std::string key;
    size_t cnt = 0;
    bool isKey = false;


    switch (c) {
        case 0:
            item = new YAMLItemEOF();
            break;

        case '{':
            item = new YAMLItemValue(indent, consumeJSON('}'));
            break;

        case '[':
            item = new YAMLItemValue(indent, consumeJSON(']'));
            break;

        case '"':
            item = new YAMLItemValue(indent, parseString('"'));
            break;

        case '\'':
            item = new YAMLItemValue(indent, parseString('\''));
            break;

        case '-':

            cnt = consumeChars('-');

            switch (cnt) {
                case 1:
                    item = new YAMLItemEntry(indent + 1);
                    break;

                case 3:
                    item = new YAMLItemStartDocument();
                    break;

                default:
                    while (cnt--) {
                        putback('-');
                    }
                    item = new YAMLItemValue(indent, parseStringOrNumber(isKey));
                    break;
            }

            break;

        case '.':

            cnt = consumeChars('.');

            switch (cnt) {
                case 3:
                    item = new YAMLItemEndDocument();
                    break;

                default:
                    while (cnt--) {
                        putback('.');
                    }
                    item = new YAMLItemValue(indent, parseStringOrNumber(isKey));
                    break;
            }

            break;

        case '&':
            consume('&');
            item = new YAMLItemAnchor(indent, nextWord());
            break;

        case '*':
            consume('*');
            item = new YAMLItemReference(indent, nextWord());
            break;

        default:
            item = new YAMLItemValue(indent, parseStringOrNumber(isKey));
            break;
    }

    ASSERT(item);

    if (isKey) {
        std::string v(item->value_);

        if (v.size() && v[v.size() - 1] == ':') {
            item = new YAMLItemKey(item);
        }
    }

    // std::cout << "YAMLItem -> " << (*item) << std::endl;

    item->attach();
    items_.push_back(item);
}

void YAMLParser::anchor(const Value& key, const Value& value) {
    anchors_[key] = value;
}

Value YAMLParser::anchor(const Value& key) const {
    std::map<Value, Value>::const_iterator j = anchors_.find(key);
    if (j == anchors_.end()) {
        std::ostringstream oss;
        oss << "YAMLParser: cannot find anchor [" << key << "]";
        throw MagicsException(oss.str());
    }
    return (*j).second;
}

const YAMLItem& YAMLParser::nextItem() {
    loadItem();
    ASSERT(!items_.empty());

    if (last_) {
        last_->detach();
    }

    last_ = items_.front();
    // last_->attach();

    items_.pop_front();

    return *last_;
}

const YAMLItem& YAMLParser::peekItem() {
    loadItem();
    ASSERT(!items_.empty());
    return *items_.front();
}

Value YAMLParser::parseValue() {
    Value v = nextItem().parse(*this);
    // ASSERT(items_.empty());
    return v;
}

std::string YAMLParser::parserName() const {
    return "YAMLParser";
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
