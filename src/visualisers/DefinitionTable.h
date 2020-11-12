/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef DefinitionTable_H
#define DefinitionTable_H

#include "expat.h"
#include "magics.h"

namespace magics {

class BaseTable {
public:
    BaseTable(const string& definition) : definition_(definition) {}
    virtual ~BaseTable() {}
    const string& info() const { return definition_; }
    string definition_;
    virtual void add(const map<string, string>&) = 0;
};

template <class D>
class DefinitionTable : public BaseTable, public map<int, D*> {
public:
    DefinitionTable(const string&, const string&);
    virtual ~DefinitionTable() override;
    virtual void toxml(ostream&, int) const {}


    const D& definition(int code) const {
        typename map<int, D*>::const_iterator param = map<int, D*>::find(code);
        if (param == this->end())
            return unknown_;
        return *(param->second);
    }

    static const DefinitionTable<D>& definitionTable(const string&, const string&);
    static const D& definitionInfo(const string&, const string&, int code);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string definition_;

    void add(const map<string, string>& def) override {
        D* param               = new D(def);
        (*this)[param->code()] = param;
    }

    static map<string, DefinitionTable<D>*>* tables_;
    static D unknown_;


private:
    //! Copy constructor - No copy allowed
    DefinitionTable(const DefinitionTable<D>&);
    //! Overloaded << operator to copy - No copy allowed
    DefinitionTable& operator=(const DefinitionTable<D>&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const DefinitionTable<D>& p) {
        p.print(s);
        return s;
    }
};

// #include "LocalTable.h"
// #include "MagException.h"
// #include <stdio.h>
// #include <string>


static inline void XMLCALL startElement(void* userData, const char* name, const char** atts) {
    BaseTable* table = (BaseTable*)userData;
    if (string(name) == table->info()) {
        map<string, string> def;
        while (*atts) {
            def[*(atts)] = *(atts + 1);
            atts += 2;
        }
        table->add(def);
    }
}

static inline void XMLCALL endElement(void*, const char*) {}

// template <class D>
// map<string, DefinitionTable<D>* >* DefinitionTable<D>::tables_ = 0;

template <class D>
D DefinitionTable<D>::unknown_;

template <class D>
DefinitionTable<D>::DefinitionTable(const string& file, const string& keyword) : BaseTable(keyword) {
    string filename = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + file;
    char buf[BUFSIZ];
    XML_Parser parser = XML_ParserCreate(NULL);
    int done;
    XML_SetUserData(parser, this);
    XML_SetElementHandler(parser, startElement, endElement);

    FILE* in = fopen(filename.c_str(), "r");

    if (!in)
        return;

    do {
        size_t len = fread(buf, 1, sizeof(buf), in);
        done       = len < sizeof(buf);
        if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR) {
            ostringstream s;
            s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser)) << " at line  "
              << XML_GetCurrentLineNumber(parser) << ends;
            cerr << s.str() << "\n";
            // throw MagicsException(s.str());
        }
    } while (!done);
    XML_ParserFree(parser);
    fclose(in);
}

template <class D>
const DefinitionTable<D>& DefinitionTable<D>::definitionTable(const string& file, const string& keyword) {
    if (!tables_)
        tables_ = new map<string, DefinitionTable<D>*>();
    typename map<string, DefinitionTable<D>*>::const_iterator local = tables_->find(file);
    if (local == tables_->end()) {
        DefinitionTable<D>* newlocal = new DefinitionTable<D>(file, keyword);
        (*tables_)[file]             = newlocal;
        return *newlocal;
    }
    return *(local->second);
}

template <class D>
const D& DefinitionTable<D>::definitionInfo(const string& file, const string& keyword, int code) {
    const DefinitionTable<D>& local = definitionTable(file, keyword);
    return local.definition(code);
}

template <class D>
DefinitionTable<D>::~DefinitionTable() {}

/*!
 Class information are given to the output-stream.
*/
template <class D>
void DefinitionTable<D>::print(ostream& out) const {
    out << "DefinitionTable[";
    out << "]";
}

}  // namespace magics
#endif
