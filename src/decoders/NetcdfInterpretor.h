/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfInterpretor.h
    \brief Definition of the Template class NetcdfInterpretor.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#ifndef NetcdfInterpretor_H
#define NetcdfInterpretor_H

#include "CustomisedPoint.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "MatrixHandler.h"
#include "NetcdfInterpretorAttributes.h"
#include "TagHandler.h"
#include "TextVisitor.h"
#include "UserPoint.h"
#include "magics.h"

namespace magics {

class RasterData;

class Netcdf;
class NetcdfInterpretor : public NetcdfInterpretorAttributes {
public:
    NetcdfInterpretor();
    virtual ~NetcdfInterpretor();
    virtual void visit(Transformation&) {}
    virtual void getReady(const Transformation&) {}
    virtual bool interpretAsMatrix(Matrix**) {
        MagLog::dev() << "Method  NetcdfInterpretor::interpretAsMatrix() --> Not yet implemented.\n";
        return false;
    }
    virtual bool interpretAsVectors(Matrix**, Matrix**) {
        MagLog::dev() << "Method  NetcdfInterpretor::interpretAsVectors() --> Not yet implemented.\n";
        return false;
    }
    virtual bool interpretAsRaster(RasterData&) {
        MagLog::dev() << "Method  NetcdfInterpretor::interpretAsRaster() --> Not yet implemented.\n";
        return false;
    }
    virtual bool interpretAsPoints(PointsList&) {
        MagLog::dev() << "Method  NetcdfInterpretor::interpretAsPoints() --> Not yet implemented.\n";
        return false;
    }
    virtual void customisedPoints(const std::set<string>&, CustomisedPointsList&) {
        MagLog::dev() << "Method  NetcdfInterpretor::customisedPoints() --> Not yet implemented.\n";
    }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int thinning) {
        MagLog::dev() << "Method  NetcdfInterpretor::customisedPoints() --> Not yet implemented.\n";
    }

    virtual bool interpretAsPoints(PointsList& points, const Transformation&) { return interpretAsPoints(points); }
    virtual void set(const map<string, string>& params) { NetcdfInterpretorAttributes::set(params); }
    virtual void set(const XmlNode& node) { NetcdfInterpretorAttributes::set(node); }
    virtual bool accept(const string& node) { return NetcdfInterpretorAttributes::accept(node); }
    virtual NetcdfInterpretor* clone() const {
        NetcdfInterpretor* object = new NetcdfInterpretor();
        object->copy(*this);
        return object;
    }

    virtual void statsData(map<string, vector<double> >&) {}
    virtual void visit(MetaDataCollector&) {}
    virtual void visit(ValuesCollector&, PointsList&){};
    virtual void visit(TextVisitor&);
    double missing(Netcdf&) const;
    string getAttribute(const string&, const string&, const string&);
    bool cf_date(Netcdf& netcdf, const string&, const string&, string&, vector<double>&, vector<double>&);
    // return true, if the the data is date CF-compliant and the date axis has been set
    bool reference_date(Netcdf& netcdf, const string&, const string&, string&, vector<double>&, vector<double>&);
    // return true, if the the data is Metview-date compliant and the date axis has been set

    string getTime(const string& format, const string& def);
    string getNumber(const string& format, const string& def);
    string getLevel(const string& format, const string& def);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    void setDimensions(const stringarray&, map<string, string>& first, map<string, string>& last);
    void getAttributes(Netcdf&, const string&, string&, string&);

    string baseDateX_;
    string baseDateY_;
    string refDateX_;
    string refDateY_;


private:
    //! Copy constructor - No copy allowed
    NetcdfInterpretor(const NetcdfInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfInterpretor& operator=(const NetcdfInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NetcdfInterpretor& p) {
        p.print(s);
        return s;
    }
};

class NetcdfGuessInterpretor : public NetcdfInterpretor {
public:
    NetcdfGuessInterpretor();
    virtual ~NetcdfGuessInterpretor();
    virtual void visit(Transformation&) {}  // delegate_->visit(transformation); }
    virtual void getReady(const Transformation&) {}
    virtual bool interpretAsMatrix(Matrix** matrix) { return guess()->interpretAsMatrix(matrix); }
    virtual bool interpretAsVectors(Matrix** u, Matrix** v) { return guess()->interpretAsVectors(u, v); }
    virtual bool interpretAsRaster(RasterData&) {
        ASSERT(false);
        return false;
    }
    virtual bool interpretAsPoints(PointsList& out) {
        ASSERT(false);
        return false;
    }
    virtual void customisedPoints(const std::set<string>& needs, CustomisedPointsList& out) {
        guess()->customisedPoints(needs, out);
    }
    virtual void customisedPoints(const Transformation& transformation, const std::set<string>& needs,
                                  CustomisedPointsList& out, int thinning) {
        guess()->customisedPoints(transformation, needs, out, thinning);
    }

    virtual bool interpretAsPoints(PointsList& points, const Transformation&) { return interpretAsPoints(points); }
    virtual void set(const map<string, string>& params) { NetcdfInterpretorAttributes::set(params); }
    virtual void set(const XmlNode& node) { NetcdfInterpretorAttributes::set(node); }
    virtual bool accept(const string& node) { return NetcdfInterpretorAttributes::accept(node); }
    virtual NetcdfInterpretor* clone() const {
        NetcdfInterpretor* object = new NetcdfInterpretor();
        object->copy(*this);
        return object;
    }

    virtual void statsData(map<string, vector<double> >&) {}
    virtual void visit(MetaDataCollector& info);
    virtual void visit(ValuesCollector&, PointsList&){};
    virtual void visit(TextVisitor& text) { guess()->visit(text); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const {}
    void setDimensions(const stringarray&, map<string, string>& first, map<string, string>& last);
    void getAttributes(Netcdf&, const string&, string&, string&);
    NetcdfInterpretor* guess() const;

    mutable NetcdfInterpretor* delegate_;

private:
    //! Copy constructor - No copy allowed
    NetcdfGuessInterpretor(const NetcdfInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfGuessInterpretor& operator=(const NetcdfInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const NetcdfGuessInterpretor& p) {
        p.print(s);
        return s;
    }
};
template <>
class MagTranslator<string, NetcdfInterpretor> {
public:
    NetcdfInterpretor* operator()(const string& val) { return SimpleObjectMaker<NetcdfInterpretor>::create(val); }

    NetcdfInterpretor* magics(const string& param) {
        NetcdfInterpretor* object;
        ParameterManager::update(param, object);
        return object;
    }
};


class NetcdfTag : public XmlNodeVisitor {
public:
    NetcdfTag(NetcdfInterpretor& netcdf, TagHandler& title) : netcdf_(netcdf), title_(title) {}

    ~NetcdfTag() {}

    void visit(const XmlNode& node);

    void decode(const string& line);
    string str() const { return out.str(); }

protected:
    NetcdfInterpretor& netcdf_;
    TagHandler& title_;
    ostringstream out;
};
}  // namespace magics
#endif
