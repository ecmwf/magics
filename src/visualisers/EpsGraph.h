/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EpsGraph.h
    \brief Definition of the Template class EpsGraph.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef EpsGraph_H
#define EpsGraph_H

#include "magics.h"


#include "CapeBoxAttributes.h"
#include "CdfGraphAttributes.h"
#include "EfiGraphAttributes.h"
#include "EpsCloudAttributes.h"
#include "EpsDirectionAttributes.h"
#include "EpsGraphAttributes.h"
#include "EpsPlumeAttributes.h"
#include "EpsShadeAttributes.h"
#include "EpsWindAttributes.h"
#include "EpsWaveAttributes.h"


#include "BasicGraphicsObject.h"
#include "Polyline.h"
#include "Visdef.h"
#include "magics.h"

namespace magics {

class XmlNode;


class EpsGraph : public EpsGraphAttributes, public Visdef {
public:
    EpsGraph();
    virtual ~EpsGraph() override;


    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;
    bool needLegend() override { return legend_; }

    // Implements the set method ...
    void set(const map<string, string>& map) override { EpsGraphAttributes::set(map); }
    void set(const XmlNode& node) override { EpsGraphAttributes::set(node); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    double resolution_;
    bool forecast_;
    bool control_;
    bool fullEps_;
    bool eps_;
    Polyline* newForecast();
    Polyline* newControl();
    void pushControl(Polyline*, BasicGraphicsObjectContainer&);
    void pushForecast(Polyline*, BasicGraphicsObjectContainer&);


private:
    //! Copy constructor - No copy allowed
    EpsGraph(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
    EpsGraph& operator=(const EpsGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsGraph& p) {
        p.print(s);
        return s;
    }
};


class EpsLight : public Visdef {
public:
    EpsLight() {}
    virtual ~EpsLight() override {}
    // Implements the set method ...
    void set(const map<string, string>& map) {}
    void set(const XmlNode&) {}


    void operator()(Data&, BasicGraphicsObjectContainer&) override;
    void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;


private:
    //! Copy constructor - No copy allowed
    EpsLight(const EpsLight&);
    //! Overloaded << operator to copy - No copy allowed
    EpsLight& operator=(const EpsLight&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsLight& p) {
        p.print(s);
        return s;
    }
};


class CapeBox : public CapeBoxAttributes, public Visdef {
public:
    CapeBox() {}
    virtual ~CapeBox() override {}
    // Implements the set method ...
    void set(const map<string, string>& map) override { CapeBoxAttributes::set(map); }
    void set(const XmlNode& node) override { CapeBoxAttributes::set(node); }


    void operator()(Data&, BasicGraphicsObjectContainer&) override;
    void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    void box(CustomisedPoint&, BasicGraphicsObjectContainer& visitor);
    int cape0_;


private:
    //! Copy constructor - No copy allowed
    CapeBox(const EpsLight&);
    //! Overloaded << operator to copy - No copy allowed
    CapeBox& operator=(const EpsLight&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const CapeBox& p) {
        p.print(s);
        return s;
    }
};


class EpsWind : public Visdef, public EpsWindAttributes {
public:
    EpsWind() {}
    virtual ~EpsWind() override {}
    // Implements the set method ...
    void set(const map<string, string>& map) override { EpsWindAttributes::set(map); }
    void set(const XmlNode& node) override { EpsWindAttributes::set(node); }


    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    void triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor,
                  double pos, double max);


private:
    //! Copy constructor - No copy allowed
    EpsWind(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
    EpsWind& operator=(const EpsGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsWind& p) {
        p.print(s);
        return s;
    }
};

class EpsCloud : public Visdef, public EpsCloudAttributes {
public:
    EpsCloud() {}
    virtual ~EpsCloud() override {}
    // Implements the set method ...
    void set(const map<string, string>& map) override { EpsCloudAttributes::set(map); }
    void set(const XmlNode& node) override { EpsCloudAttributes::set(node); }


    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    void triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor,
                  double pos);


private:
    //! Copy constructor - No copy allowed
    EpsCloud(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
    EpsCloud& operator=(const EpsGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsCloud& p) {
        p.print(s);
        return s;
    }
};
class EpsBar : public Visdef, public EpsCloudAttributes {
public:
    EpsBar() {}
    virtual ~EpsBar() override {}
    // Implements the set method ...
    void set(const map<string, string>& map) override { EpsCloudAttributes::set(map); }
    void set(const XmlNode& node) override { EpsCloudAttributes::set(node); }


    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;


private:
    //! Copy constructor - No copy allowed
    EpsBar(const EpsGraph&);
    //! Overloaded << operator to copy - No copy allowed
    EpsBar& operator=(const EpsGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsBar& p) {
        p.print(s);
        return s;
    }
};

class EpsWave : public EpsWaveAttributes, public Visdef {
public:
    EpsWave() {}
    virtual ~EpsWave() override {}
    // Implements the set method ...
    void set(const map<string, string>&) override {}  // EpsWindAttributes::set(map); }
    void set(const XmlNode&) override {}              // EpsWindAttributes::set(node); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    EpsWave(const EpsWave&);
    //! Overloaded << operator to copy - No copy allowed
    EpsWave& operator=(const EpsWave&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsWave& p) {
        p.print(s);
        return s;
    }
};


class EfiGraph : public Visdef, public EfiGraphAttributes {
public:
    EfiGraph();
    virtual ~EfiGraph() override;

    void set(const XmlNode& node) override { EfiGraphAttributes::set(node); }
    void set(const map<string, string>& map) override { EfiGraphAttributes::set(map); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override { /*EfiGraphAttributes::print(out);*/
    }


private:
    //! Copy constructor - No copy allowed
    EfiGraph(const EfiGraph&);
    //! Overloaded << operator to copy - No copy allowed
    EfiGraph& operator=(const EfiGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EfiGraph& p) {
        p.print(s);
        return s;
    }
};

class CdfGraph : public Visdef, public CdfGraphAttributes {
public:
    CdfGraph();
    virtual ~CdfGraph() override;

    void set(const XmlNode& node) override { CdfGraphAttributes::set(node); }
    void set(const map<string, string>& map) override { CdfGraphAttributes::set(map); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override { /*EfiGraphAttributes::print(out);*/
    }
    vector<string> legends_;
    vector<string> usedColours_;
    vector<string> usedStyle_;
    vector<int> usedThickness_;
    string climateLegend_;
    vector<pair<string, double>> keys_;
    typedef void (CdfGraph::*Setter)(const string&);
    std::map<string, Setter> setters_;

    void setMedium(const string&);
    void setExtended(const string&);

private:
    //! Copy constructor - No copy allowed
    CdfGraph(const CdfGraph&);
    //! Overloaded << operator to copy - No copy allowed
    CdfGraph& operator=(const CdfGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const CdfGraph& p) {
        p.print(s);
        return s;
    }
};

class EpsShade : public Visdef, public EpsShadeAttributes {
public:
    EpsShade();
    virtual ~EpsShade() override;

    void set(const XmlNode& node) override { EpsShadeAttributes::set(node); }
    void set(const map<string, string>& map) override { EpsShadeAttributes::set(map); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& s) const override {
        // EfiGraphAttributes::print(s);
    }


private:
    //! Copy constructor - No copy allowed
    EpsShade(const EpsShade&);
    //! Overloaded << operator to copy - No copy allowed
    EpsShade& operator=(const EpsShade&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsShade& p) {
        p.print(s);
        return s;
    }
};

class EpsDirection : public Visdef, public EpsDirectionAttributes {
public:
    EpsDirection() {}
    virtual ~EpsDirection() override {}

    void set(const XmlNode& node) override { EpsDirectionAttributes::set(node); }
    void set(const map<string, string>& map) override { EpsDirectionAttributes::set(map); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { EpsDirectionAttributes::print(out); }


private:
    //! Copy constructor - No copy allowed
    EpsDirection(const EpsDirection&);
    //! Overloaded << operator to copy - No copy allowed
    EpsDirection& operator=(const EpsDirection&);
};

class EpsPlume : public Visdef, public EpsPlumeAttributes {
public:
    EpsPlume();
    virtual ~EpsPlume() override {}

    void set(const XmlNode& node) override { EpsPlumeAttributes::set(node); }
    void set(const map<string, string>& map) override { EpsPlumeAttributes::set(map); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    void visit(LegendVisitor&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { EpsPlumeAttributes::print(out); }
    typedef void (EpsPlume::*Method)(Data&, BasicGraphicsObjectContainer&);

    std::map<string, Method> methods_;
    vector<Colour> shading_legend_;
    void timeserie(Data&, BasicGraphicsObjectContainer&);
    void verticalprofile(Data&, BasicGraphicsObjectContainer&);
    void background(BasicGraphicsObjectContainer& visitor);
private:
    //! Copy constructor - No copy allowed
    EpsPlume(const EpsPlume&);
    //! Overloaded << operator to copy - No copy allowed
    EpsShade& operator=(const EpsPlume&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EpsPlume& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
