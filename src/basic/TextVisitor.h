/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TextVisitor.h
    \brief Definition of the Template class TextNode.

    Magics Team - ECMWF 2007

    Started: Tue 6-Mar-2007

    Changes:

*/

#ifndef TextVisitor_H
#define TextVisitor_H

#include "magics.h"

#include "SceneVisitor.h"
#include "TagHandler.h"
#include "TextVisitorAttributes.h"
#include "XmlBasicNodeAttributes.h"

namespace magics {

class TextEntry {
public:
    TextEntry(const string entry = "") : entry_(entry) {}
    ~TextEntry() {}
    string entry();
    string entry_;
    void add(const string& info) { entry_ += info; }
};

struct KeyInfo {
    string key_;
    string what_;
    string format_;
};

class TextVisitor : public TextVisitorAttributes,
                    public TagHandler,
                    public LayoutVisitor,
                    public BasicPositionalObject,
                    public map<string, vector<TextEntry*> > {
public:
    TextVisitor();
    virtual ~TextVisitor() override;

    TextVisitor* clone();
    void add(const string& line, TextEntry* entry);
    void add(TextEntry* entry) { add("<magics_title/>", entry); }
    void addAutomaticTitle(const string&);


    void addToTags(const string&, const string&) override;

    virtual void titles(vector<string>&);
    virtual Layout& layout() const override { return LayoutVisitor::layout(); }
    virtual Layout* layoutPtr() const { return LayoutVisitor::layoutPtr(); }


    map<string, vector<Text*> > texts() { return currentTexts_; }
    void visit();
    void visit(BasicSceneObject&) override;
    void finish(BasicGraphicsObjectContainer&);
    void update(vector<Text*>&);
    void start();

    void visit(MetaDataVisitor&) override;
    bool positional() const { return positional_; }
    virtual void getReady() override;
    void update(const string& family, const string& definition, const string& value) {
        TagHandler::update(family, definition, value);
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    virtual void decode() {}

    bool positional_;
    double font_size_;

    static map<string, string> tags_;
    void extract(const string&, vector<KeyInfo>&);


    void set(const map<string, string>& map) override { TextVisitorAttributes::set(map); }
    void set(const XmlNode& node) override { TextVisitorAttributes::set(node); }
    string label_;
    mutable vector<Text*> texts_;
    mutable map<string, vector<Text*> > currentTexts_;
    AutoVector<TextEntry> currentLine_;


private:
    //! Copy constructor - No copy allowed
    TextVisitor(const TextVisitor&);
    //! Overloaded << operator to copy - No copy allowed
    TextVisitor& operator=(const TextVisitor&);
};

class XmlTextVisitor : public TextVisitor, public XmlBasicNodeAttributes {
public:
    XmlTextVisitor();
    ~XmlTextVisitor() override {}
    void set(const XmlNode&) override;
    void getReady() override;
    void decode() override;
};

class FortranTextVisitor : public TextVisitor {
public:
    FortranTextVisitor();
    ~FortranTextVisitor() override {}

protected:
    void decode() override;
    void interpret(string&, stringarray&);
};

class FortranAutomaticTextVisitor : public FortranTextVisitor {
public:
    FortranAutomaticTextVisitor();
    ~FortranAutomaticTextVisitor() override {}
    void getReady() override;
};


class FortranPositionalTextVisitor : public FortranTextVisitor {
public:
    FortranPositionalTextVisitor();
    ~FortranPositionalTextVisitor() override {}
    void getReady() override;
};
/*
class MvTextVisitor :
{
    MvTextVisitor();
    ~MvTextVisitor() override

}
*/
}  // namespace magics
#endif
