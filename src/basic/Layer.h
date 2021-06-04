/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BasicGraphicsObject.h
    \brief Implementation of BasicGraphicsObject class.
    \author Meteorological Visualisation Section, ECMWF

    Started: March 2004

*/
#ifndef Layer_H
#define Layer_H

#include "magics.h"

#include "BasicGraphicsObject.h"
#include "BasicSceneObject.h"
#include "MagDateTime.h"
#include "MagLog.h"

namespace magics {

class SceneLayer;
class AnimationRules;
class Text;
class TextVisitor;

enum LayerState
{
    new_layer,
    geometry_changed
};

class Layer : public BasicGraphicsObjectContainer {
public:
    Layer();
    Layer(BasicSceneObject*);
    virtual ~Layer() override;
    void print(ostream& out) const override;
    void parent(SceneLayer* layer) { parent_ = layer; }
    SceneLayer* parent() const { return parent_; }
    virtual void redisplay(const BaseDriver& driver) const override;
    virtual void execute(const BaseDriver&) const;
    virtual void execute(int, const BaseDriver&, const Layout&) const;
    virtual void getInfo(int, const BaseDriver&) const {}
    virtual void getReady(int) const;
    virtual void getReady() const;
    //! get the metada for the layer
    virtual void collect(MetaDataCollector&) {}
    //! get the values for the list of position ( in user coordinates)
    virtual void collect(ValuesCollector& values);
    //! get the data indexes for the layer
    virtual void collect(DataIndexCollector&) {}
    /*! Prepare the list of positions (symbol) of the data points
     and send them back to the specified driver! */
    virtual void magnify(const BaseDriver&, float, float) {}
    /* Prepare Histogram for a layer and send it to the specified driver*/
    virtual void histogram(const BaseDriver&, const string&, const string&) {}

    virtual void newLayer(const BaseDriver& driver);
    virtual void closeLayer(const BaseDriver& driver);
    virtual Layer* get() { return 0; }
    virtual Layer* get(int) { return 0; }
    virtual Layer* baseLayer() { return this; }

    void visibility(bool visibility) { visibility_ = visibility; }
    void zindex(int zindex) { zindex_ = zindex; }
    void transparency(int transparency) { transparency_ = transparency; }
    void validTime(const DateTime& from, const DateTime& to) {
        from_ = from;
        to_   = to;
    }

    bool visibility() const { return visibility_; }
    int transparency() const { return transparency_; }
    int zindex() const { return zindex_; }

    const string& id() const { return id_; }
    void id(const string& id) { id_ = id; }

    const string& uniqueId() const { return uniqueId_; }
    void uniqueId(const string& id) { uniqueId_ = id; }

    const string& name() const { return name_; }
    void name(const string& id) { name_ = id; }

    virtual int size() { return 1; }


    bool operator<(const Layer& other) const { return zindex_ < other.zindex_; }

    void collectText(vector<TextVisitor*>&, LegendVisitor*);  // update the text informations!
    vector<Text*>& updateText(TextVisitor* text, const string& line) { return myTexts_[text][line]; }

    string timeStamp(const string&) const;
    string timeBegin(const string&) const;
    string timeEnd(const string&) const;

    string timeBegin() const;
    string timeEnd() const;
    string timeStamp() const;

    const string& metadata(const string&);
    void metadata(const string&, const string&);
    virtual void update(const Layout&) {}

    vector<MetviewIcon>::const_iterator iconsBegin() { return icons_.begin(); }
    vector<MetviewIcon>::const_iterator iconsEnd() { return icons_.end(); }
    void icon(const string& iconname, const string& iconclass, const string& iconid) {
        icons_.push_back(MetviewIcon(iconname, iconclass, iconid));
    }
    void icon(const MetviewIcon& icon) {
        MetviewIcon add;
        add.icon(icon);
        icons_.push_back(add);
        visibility_   = icon.visibility();
        transparency_ = icon.transparency();
        // F20161116				zindex_ = icon.zindex();
        id_   = icon.id();
        name_ = icon.name();
    }

    // void setInfo(const string& name, const string& value) { information_[name]=value; }
    // virtual const map<string, string>& getInfos(bool =false) const  override { return information_ ; }

    LayerState state() { return state_; }
    void state(LayerState state) { state_ = state; }
    virtual void release() override;

protected:
    bool visibility_;
    int zindex_;
    int transparency_;
    string name_;
    LayerState state_;


    // For Metview ...
    vector<MetviewIcon> icons_;

    map<string, string> metadata_;

    DateTime from_;
    DateTime to_;
    string id_;
    string uniqueId_;
    SceneLayer* parent_;
    BasicSceneObject* object_;

    mutable map<TextVisitor*, map<string, vector<Text*> > > myTexts_;
};


/*
 * A StepLayer maintains animation.
 */
class StepLayer;

class SingleLayer : public Layer {
public:
    SingleLayer(StepLayer*, BasicSceneObject*);
    ~SingleLayer() override;

    void print(ostream& out) const override;
    void redisplay(const BaseDriver& driver) const override;
    void execute(const BaseDriver&) const override;
    void getReady() const override;
    void set(LayoutVisitor*) const;
    void collect(MetaDataCollector&) override;
    void collect(ValuesCollector&) override;
    void collect(DataIndexCollector&) override;
    // const map<string, string>& getInfos(bool collect=false) const;
    void magnify(const BaseDriver&, float xres, float yres) override;
    void histogram(const BaseDriver&, const string&, const string&) override;
    void update(const Layout&) override;
    Layer* baseLayer() override;
    LevelDescription& dataLevel() const;
    DateDescription& timeStamp() const;
    void release() override;


protected:
    mutable Layout* objects_;
    StepLayer* parentLayer_;
    mutable LevelDescription level_;
    mutable DateDescription stamp_;
};


class StepLayer : public Layer {
public:
    StepLayer();
    ~StepLayer() override;
    void print(ostream& out) const override;
    int size() override;
    void redisplay(const BaseDriver& driver) const override;
    void newLayer(const BaseDriver& driver) override;
    void closeLayer(const BaseDriver& driver) override;
    void execute(int, const BaseDriver&, const Layout&) const override;
    void getReady(int) const override;
    Layer* get(int) override;

    void addStep(BasicSceneObject*, Data*);
    vector<SingleLayer*>::iterator firstStep() { return steps_.begin(); }
    vector<SingleLayer*>::iterator endStep() { return steps_.end(); }

    void addVisitor(LayoutVisitor*);
    vector<LayoutVisitor*>::iterator firstVisitor() { return visitors_.begin(); }
    vector<LayoutVisitor*>::iterator endVisitor() { return visitors_.end(); }

protected:
    vector<SingleLayer*> steps_;
    vector<LayoutVisitor*> visitors_;
};


class LayoutVisitor;


class StaticLayer : public Layer {
public:
    StaticLayer();
    StaticLayer(BasicSceneObject*);
    StaticLayer(const Layout&);
    ~StaticLayer() override;
    void print(ostream& out) const override;

    void redisplay(const BaseDriver& driver) const override;
    void newLayer(const BaseDriver& driver) override;
    void closeLayer(const BaseDriver& driver) override;

    void histogram(const BaseDriver& driver, const string&, const string&) override;
    void update(const Layout&) override;

    void execute(const BaseDriver&) const override;
    void getReady() const override;
    Layer* get() override { return this; }
    void clean();
    void set(LayoutVisitor*);
    void add(BasicGraphicsObject*);
    void collect(MetaDataCollector&) override;
    void collect(ValuesCollector&) override;
    void collect(DataIndexCollector&) override;


protected:
    Layout* layer_;
    mutable bool updateText_;
};

/*
 * A NoDataLayer is a static layer that do not contain any data information :ie some driver like KML may
 * not need to handle them
 */
class NoDataLayer : public StaticLayer {
public:
    NoDataLayer() {}
    NoDataLayer(BasicSceneObject*);
    ~NoDataLayer() override;

    void redisplay(const BaseDriver&) const override;
    void getReady() const override {}
    void print(ostream& s) const override { s << "NoDataLayer"; }
};


class TextLayer : public StepLayer {
public:
    TextLayer() {}
    ~TextLayer() override {}
    void getReady() const override;
    void getReady(int i) const override { Layer::getReady(i); }
    void execute(const BaseDriver&) const override;
    void execute(int, const BaseDriver&) const;
    void getInfo(int, const BaseDriver&) const override;
    void collectText(vector<TextVisitor*>&, LegendVisitor*);  // update the text informations!
    void print(ostream& s) const override { s << "TextLayer"; }
};

class LegendLayer : public StepLayer {
public:
    LegendLayer() {}
    ~LegendLayer() override {}
    void getReady() const override;
    void execute(const BaseDriver&) const override;
    void execute(int, const BaseDriver&) const;
    void getInfo(int, const BaseDriver&) const override;
    void print(ostream& s) const override { s << "LegendLayer"; }
};
/*
 * A SceneLayer is attach to a SceneNode...
 * It contains the list of layers needed to perform a plot.
 * Some layers can have steps...
 */
class SceneLayer : public BasicGraphicsObjectContainer {
public:
    SceneLayer();
    ~SceneLayer() override;
    void print(ostream& out) const override;
    // Number of frames in the serie!
    int numberOfSteps() const;
    void rules(AnimationRules* rules) { rules_ = rules; }

    void execute(int, const BaseDriver&) const;

    void execute(Layer*, int, const BaseDriver&) const;

    Layer* findLayer(Layer*, int) const;

    LegendVisitor* legend(LegendVisitor* legend);

    void text(TextVisitor* text);


    void getReady(int) const;

    bool buildTree(const Layout&, unsigned int, const BaseDriver&) const override;

    vector<Layer*>::iterator beginLayer() const;
    vector<Layer*>::iterator endLayer() const;

    vector<Layer*>::iterator beginLayer(int) const;
    vector<Layer*>::iterator endLayer(int) const;

    vector<Layer*>& prepare(int) const;
    void redisplay(const BaseDriver& driver) const override;
    void redisplayAll(const BaseDriver& driver) const;
    void add(Layer*);
    Layout* layoutPtr() {
        ASSERT(layout_);
        return layout_;
    }
    BasicGraphicsObjectContainer* parent() { return parent_; }
    void addVisitor(LayoutVisitor* visitor) { visitors_.insert(visitor); }
    void setMagicsMode(MagicsMode mode) { mode_ = mode; }
    vector<TextVisitor*>& texts() { return textVisitors_; }
    void cleanText();
    void finishText(Layout& layout);
    void collectText();
    void executeInfo(int, const BaseDriver&) const;

    LegendVisitor* legend() { return legend_; }
    LayerState state() { return state_; }
    void state(LayerState state) { state_ = state; }

protected:
    mutable Layout* layout_;
    AnimationRules* rules_;
    int currentIndex_;

    mutable map<int, vector<Layer*> > steps_;
    mutable vector<Layer*> layers_;
    mutable std::set<LayoutVisitor*> visitors_;
    mutable vector<TextVisitor*> textVisitors_;
    mutable TextLayer textHandler_;
    mutable LegendLayer legendHandler_;

    mutable LegendVisitor* legend_;
    MagicsMode mode_;
    int currentFrame_;
    LayerState state_;
};


class MagnifierCollector : public vector<PaperPoint> {
public:
    MagnifierCollector() : transformation_(0), layout_(0) {}
    ~MagnifierCollector() {}
    void transformation(const Transformation* transformation) { transformation_ = transformation; }
    void visit(const BaseDriver&);
    void setLayout(Layout* layout) { layout_ = layout; }
    void setParent(BasicGraphicsObjectContainer* parent) { layout_->parent(parent); }
    const Transformation& transformation() { return *transformation_; }

protected:
    const Transformation* transformation_;
    Layout* layout_;
};


class ValuesCollectorData;
class ValuesCollectorUVData;
class ValuesCollectorSDData;

class ValuesCollectorVisitor {
public:
    ValuesCollectorVisitor();
    virtual void visit(const ValuesCollectorData& data);
    virtual void visit(const ValuesCollectorUVData& data);
    virtual void visit(const ValuesCollectorSDData& data);
};

class ValuesCollectorData {
public:
    ValuesCollectorData(double x, double y, double value, double distance, int index = -1) :
        x_(x), y_(y), value_(value), distance_(distance), missing_(false), index_(index) {}

    double x() const { return x_; }
    double y() const { return y_; }
    double value() const { return value_; }
    double distance() const { return distance_; }
    void setScaledValue(double d) { scaledValue_ = d; }
    double scaledValue() const { return scaledValue_; }
    bool missing() const { return missing_; }
    void setMissing(bool b) { missing_ = b; }
    int index() const { return index_; }
    virtual void visit(ValuesCollectorVisitor& visitor) { visitor.visit(*this); }

protected:
    virtual void print(ostream&) const;
    double x_;
    double y_;
    double value_;
    double distance_;
    double scaledValue_;
    bool missing_;
    int index_;

    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ValuesCollectorData& p) {
        p.print(s);
        return s;
    }
};

class ValuesCollectorUVData : public ValuesCollectorData {
public:
    ValuesCollectorUVData(double x, double y, double u, double v, double distance, int index = -1) :
        ValuesCollectorData(x, y, 0, distance, index), xComponent_(u), yComponent_(v) {}

    virtual void visit(ValuesCollectorVisitor& visitor) override { visitor.visit(*this); }

    double xComponent() const { return xComponent_; }
    double yComponent() const { return yComponent_; }
    double scaledXComponent() const { return scaledXComponent_; }
    double scaledYComponent() const { return scaledYComponent_; }

protected:
    virtual void print(ostream&) const override;
    double xComponent_;
    double yComponent_;
    double scaledXComponent_;
    double scaledYComponent_;
};

class ValuesCollectorSDData : public ValuesCollectorData {
public:
    ValuesCollectorSDData(double x, double y, double s, double d, double distance, int index = -1) :
        ValuesCollectorData(x, y, 0, distance, index), speed_(s), direction_(d) {}
    virtual void visit(ValuesCollectorVisitor& visitor) override { visitor.visit(*this); }
    double speed() const { return speed_; }
    double direction() const { return direction_; }
    double scaledSpeed() const { return scaledSpeed_; }
    double scaledDirection() const { return scaledDirection_; }

protected:
    virtual void print(ostream&) const override;
    double speed_;
    double direction_;
    double scaledSpeed_;
    double scaledDirection_;
};

class ValuesCollectorPoint : public vector<ValuesCollectorData*> {
public:
    enum Mode
    {
        PositionMode,
        IndexMode
    };

    ValuesCollectorPoint(double x, double y) : mode_(PositionMode), x_(x), y_(y), index_(-1) {}
    ValuesCollectorPoint(int index) : mode_(IndexMode), index_(index) {}
    ~ValuesCollectorPoint();

    Mode mode() const { return mode_; }
    double x() const { return x_; }
    double y() const { return y_; }
    int index() const { return index_; }

protected:
    Mode mode_;
    double x_;
    double y_;
    int index_;
};


class ValuesCollector : public vector<ValuesCollectorPoint> {
public:
    ValuesCollector(string name = string()) :
        name_(name),
        scaled_(false),
        collected_(false),
        searchRadiusX_(2.),
        searchRadiusY_(2.),
        hasValue_(true),
        multiData_(false) {}
    ~ValuesCollector() {}
    void transformation(const Transformation* transformation) { transformation_ = transformation; }
    const Transformation& transformation() const { return *transformation_; }
    const string& name() const { return name_; }
    const string& scaledUnits() const { return scaledUnits_; }
    void setScaledUnits(string s) { scaledUnits_ = s; }
    const string& units() const { return units_; }
    void setUnits(string s) { units_ = s; }
    bool scaled() const { return scaled_; }
    void setScaled(bool b) { scaled_ = b; }
    bool collected() const { return collected_; }
    void setCollected(bool b) { collected_ = b; }
    void setSearchRadius(double rx, double ry) {
        searchRadiusX_ = rx;
        searchRadiusY_ = ry;
    }
    double searchRadiusX() const { return searchRadiusX_; }
    double searchRadiusY() { return searchRadiusY_; }
    void setHasValue(bool b) { hasValue_ = b; }
    bool hasValue() const { return hasValue_; }
    void setMultiData(bool b) { multiData_ = b; }
    bool multiData() const { return multiData_; }

protected:
    const Transformation* transformation_;
    string name_;
    string units_;
    string scaledUnits_;
    bool scaled_;
    bool collected_;
    double searchRadiusX_;
    double searchRadiusY_;
    bool hasValue_;
    bool multiData_;
};

class DataIndexCollector {
public:
    DataIndexCollector() {}
    ~DataIndexCollector() {}

    void setDataIndex(const vector<int>& d) { dataIndex_ = d; }
    const vector<int>& dataIndex() const { return dataIndex_; }

protected:
    vector<int> dataIndex_;
};


class MetaDataAttribute {
public:
    enum Source
    {
        AnySource,
        InfoSource,
        GribApiSource
    };
    enum Group
    {
        NoGroup,
        StatsGroup
    };
    enum Type
    {
        NumberType,
        StringType
    };

    MetaDataAttribute() : source_(InfoSource), group_(NoGroup), type_(StringType){};
    void setSource(Source s) { source_ = s; }
    void setGroup(Group g) { group_ = g; }
    void setType(Type t) { type_ = t; }
    Source source() const { return source_; }
    Type type() const { return type_; }
    Group group() const { return group_; }

protected:
    Source source_;
    Group group_;
    Type type_;
};


class MetaDataCollector : public map<string, string> {
public:
    MetaDataCollector() : transformation_(0){};
    ~MetaDataCollector(){};

    void transformation(const Transformation* transformation) { transformation_ = transformation; }
    const Transformation& transformation() {  ASSERT(transformation_); return *transformation_; }
    void reset() {
        clear();
        attributes_.clear();
    }
    void setAttribute(const string& key, const MetaDataAttribute& attr) { attributes_[key] = attr; }
    const map<string, MetaDataAttribute>& attributes() { return attributes_; }
    bool hasAttribute(const string& key) { return (attributes_.find(key) != attributes_.end()); }
    const MetaDataAttribute& attribute(const string& key) { return attributes_[key]; }

protected:
    map<string, MetaDataAttribute> attributes_;
    const Transformation* transformation_;
};

}  // namespace magics
#endif
