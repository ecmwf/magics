/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef ObsItemFamily_H
#define ObsItemFamily_H


#include "ObsItem.h"
#include "ObsPlotting.h"
#include "Symbol.h"
#include "UserPoint.h"

class ObsItemBox : public ObsItem {
public:
    ObsItemBox() {}
    ~ObsItemBox() {}

    virtual void set(const map<string, string>& def);

protected:
    int row_;
    int column_;
    string colour_;
    string key_;
    string format_;
    Justification justification_;
};

class ObsStationRing : public ObsItemBox {
public:
    ObsStationRing() {}
    ~ObsStationRing() {}
    void visit(std::set<string>& tokens) {
        if (!owner_->station_ring_visible_)
            return;
        tokens.insert("latitude");
        tokens.insert("longitude");
        tokens.insert("total_cloud");
    }

    void operator()(CustomisedPoint&, ComplexSymbol& symbol) const {
        if (!owner_->station_ring_visible_)
            return;
        SymbolItem* station = new SymbolItem();
        station->x(column_);
        station->y(row_);
        station->colour(*owner_->station_ring_colour_);
        station->symbol("circle");
        station->height(owner_->ring_size_);
        symbol.add(station);
    }

protected:
    void print(ostream& out) const { out << "ObsStationRing"; }
};
class ObsStationTriangle : public ObsItemBox {
public:
    ObsStationTriangle() {}
    ~ObsStationTriangle() {}
    void visit(std::set<string>& tokens) {
        if (!owner_->station_ring_visible_)
            return;
    }

    void operator()(CustomisedPoint&, ComplexSymbol& symbol) const {
        if (!owner_->station_ring_visible_)
            return;
        SymbolItem* station = new SymbolItem();
        station->x(column_);
        station->y(row_);
        station->colour(*owner_->station_ring_colour_);
        station->symbol("triangle");  // triangle
        station->height(owner_->ring_size_ * 0.5);
        symbol.add(station);
    }

protected:
    void print(ostream& out) const { out << "ObsStationTriangle"; }
};

class ObsTimePlot : public ObsItemBox {
public:
    ObsTimePlot() {}
    ~ObsTimePlot() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsTimePlot"; }
};


class ObsWind : public ObsItemBox {
public:
    ObsWind() { setOrigins(); }
    ~ObsWind() {}
    void visit(std::set<string>& tokens);

    virtual void operator()(CustomisedPoint&, ComplexSymbol&) const;
    virtual void set(const map<string, string>& def) {
        row_       = atoi(find(def, "row").c_str());
        column_    = atoi(find(def, "column").c_str());
        colour_    = find(def, "colour");
        speed_     = find(def, "wind_speed", "wind_speed");
        direction_ = find(def, "wind_direction", "wind_direction");
    }

protected:
    string speed_;
    string direction_;
    void setOrigins();
    void print(ostream& out) const { out << "ObsWind"; }
};

class ObsCloudAndWind : public ObsItemBox {
public:
    ObsCloudAndWind() { setOrigins(); }
    ~ObsCloudAndWind() {}
    void visit(std::set<string>& tokens) {
        if (!owner_->wind_visible_)
            return;
        tokens.insert("wind_speed");
        tokens.insert("wind_direction");
        tokens.insert("total_cloud");
        tokens.insert("low_cloud");
        tokens.insert("medium_cloud");
        tokens.insert("high_cloud");
    }

    virtual void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void setOrigins();
    void print(ostream& out) const { out << "ObsWind"; }

    static map<int, string> origins_;
};


class ObsTemperature : public ObsItemBox {
public:
    ObsTemperature() {}
    ~ObsTemperature() {}
    void visit(std::set<string>& tokens) {
        if (!owner_->temperature_visible_)
            return;
        tokens.insert("temperature");
    }
    void operator()(CustomisedPoint&, ComplexSymbol& symbol) const;

protected:
    void print(ostream& out) const { out << "ObsTemperature"; }
};

class ObsSeaTemperature : public ObsItemBox {
public:
    ObsSeaTemperature() {}
    ~ObsSeaTemperature() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol& symbol) const;

protected:
    void print(ostream& out) const { out << "ObsSeaTemperature"; }
};
class ObsWave : public ObsItemBox {
public:
    ObsWave() {}
    ~ObsWave() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol& symbol) const;

protected:
    void print(ostream& out) const { out << "ObsWave"; }
};
class ObsPressure : public ObsItemBox {
public:
    ObsPressure() {}
    ~ObsPressure() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsPressure"; }
};


class ObsPressureLevel : public ObsItemBox {
public:
    ObsPressureLevel() {}
    ~ObsPressureLevel() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsPressureLevel"; }
};


class ObsPressureTendency : public ObsItemBox {
public:
    ObsPressureTendency() {}
    ~ObsPressureTendency() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsPressureTendency"; }
};


class ObsThickness : public ObsItemBox {
public:
    ObsThickness() {}
    ~ObsThickness() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsThickness"; }
};

class ObsDewPoint : public ObsItemBox {
public:
    ObsDewPoint() {}
    ~ObsDewPoint() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsDewPoint"; }
};


class ObsHeight : public ObsItemBox {
public:
    ObsHeight() {}
    ~ObsHeight() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsHeight"; }
};


class ObsVisibility : public ObsItemBox {
public:
    ObsVisibility() {}
    ~ObsVisibility() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsVisibility"; }
};


class ObsPresentWeather : public ObsItemBox {
public:
    ObsPresentWeather() {}
    ~ObsPresentWeather() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsPresentWeather"; }
};


class ObsIdentifier : public ObsItemBox {
public:
    ObsIdentifier() {}
    ~ObsIdentifier() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsIdentifier"; }
};


class ObsPastWeather : public ObsItemBox {
public:
    ObsPastWeather() {}
    ~ObsPastWeather() {}
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsPastWeather"; }
};


class ObsCloud : public ObsItemBox {
public:
    ObsCloud() {}
    ~ObsCloud() {}
    void set(const map<string, string>& def);
    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsCloud"; }
    int lowRow_;
    int lowColumn_;
    int mediumRow_;
    int mediumColumn_;
    int highRow_;
    int highColumn_;
};

class ObsDemoItem1 : public ObsItemBox {
public:
    ObsDemoItem1() {}
    ~ObsDemoItem1() {}

    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsDemoItem1"; }
};
class ObsDemoItem2 : public ObsItemBox {
public:
    ObsDemoItem2() {}
    ~ObsDemoItem2() {}

    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsDemoItem2"; }
};

class ObsEra : public ObsItemBox {
public:
    ObsEra() {}
    ~ObsEra() {}

    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsEra"; }
};
class ObsString : public ObsItemBox {
public:
    ObsString() {}
    ~ObsString() {}

    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsNumber"; }
    std::set<string> keys_;
};
class ObsNumber : public ObsItemBox {
public:
    ObsNumber() {}
    ~ObsNumber() {}

    void visit(std::set<string>& tokens);
    void operator()(CustomisedPoint&, ComplexSymbol&) const;

protected:
    void print(ostream& out) const { out << "ObsNumber"; }
};
#endif
