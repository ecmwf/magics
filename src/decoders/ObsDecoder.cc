/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsDecoder.cc
    \brief Implementation of the Template class ObsDecoder.

    Magics Team - ECMWF 2005

    Started: Wed 23-Mar-2005

    Changes:

*/

#include "ObsDecoder.h"
#include "CustomisedPoint.h"
#include "Factory.h"
#include "MagicsGlobal.h"
#include "MvObs.h"
#include "TextVisitor.h"
#include "expat.h"

using namespace magics;

map<int, bool> multilevels_;

class BufrSubType : public map<string, string> {
public:
    BufrSubType() {}
    ~BufrSubType() {}

protected:
    void print(ostream& out) const {
        for (const_iterator subtype = begin(); subtype != end(); ++subtype)
            out << "subtype[" << subtype->first << "=" << subtype->second << "]";
    }
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BufrSubType& p) {
        p.print(s);
        return s;
    }
};


class BufrFamily : public map<string, BufrSubType> {
public:
    BufrFamily(const string&);
    ~BufrFamily(){};

    void currentType(const map<string, string>& def) { type_ = def.find("value")->second; }

    void currentSubtype(const map<string, string>& def) {
        map<string, string>::const_iterator value = def.find("value");
        if (value == def.end()) {
            MagLog::warning() << "BufrFamily> No code defined for subtype!\n";
            subtype_ = "???";
        }
        else
            subtype_ = def.find("value")->second;

        map<string, string>::const_iterator tmpl = def.find("template");
        template_                                = (tmpl != def.end()) ? tmpl->second : "";
        subtypeToBeSet_                          = true;
    }

    void addSubtype(const string& value) {
        if (template_ == "")
            template_ = value;
        (*this)[type_].insert(make_pair(subtype_, template_));
        subtypeToBeSet_ = false;
    }

    bool subtype() { return subtypeToBeSet_; }

    void subtypeToReset() { subtypeToBeSet_ = false; }

    string getType(const string& type, const string& subtype) const {
        const_iterator t = find(type);
        if (t != end()) {
            BufrSubType::const_iterator s = t->second.find(subtype);
            if (s != t->second.end())
                return s->second;
        }
        return "";
    }

protected:
    void print(ostream& out) const {
        for (const_iterator type = begin(); type != end(); ++type)
            out << type->first << "=[" << type->second << "]";
    }

    string type_;
    string subtype_;
    string template_;
    bool subtypeToBeSet_;
    string centre_;

    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BufrFamily& p) {
        p.print(s);
        return s;
    }
};


class BufrFamilyTable : public map<string, BufrFamily*> {
public:
    BufrFamilyTable() {}
    ~BufrFamilyTable() {}
    static const BufrFamily& get(const string& centre) {
        const_iterator table = table_.find(centre);
        if (table != table_.end())
            return *(table->second);
        BufrFamily* idents = new BufrFamily(centre);
        table_[centre]     = idents;
        return *idents;
    }

    static BufrFamilyTable table_;
};


static void XMLCALL startElement(void* userData, const char* name, const char** atts) {
    BufrIdentifiers* ident = (BufrIdentifiers*)userData;

    while (*atts) {
        if (string(*atts) == "descriptor") {
            ident->insert(make_pair(name, *(atts + 1)));
        }
        atts += 2;
    }
}

static void XMLCALL endElement(void*, const char*) {}


static void XMLCALL startFamilyElement(void* userData, const char* name, const char** atts) {
    BufrFamily* ident = (BufrFamily*)userData;
    string token(name);
    ident->subtypeToReset();
    map<string, string> def;
    if (token != "type" && token != "subtype")
        return;
    while (*atts) {
        def.insert(make_pair(*atts, *(atts + 1)));
        atts += 2;
    }
    if (token == "type") {
        ident->currentType(def);
    }
    if (token == "subtype") {
        ident->currentSubtype(def);
    }
}

static void XMLCALL endFamilyElement(void*, const char*) {}

static void XMLCALL character(void* userData, const char* s, int len) {
    // int *depthPtr = (int*)userData;
    BufrFamily* object = (BufrFamily*)userData;
    if (std::string(s, len) == "\n")
        return;
    if (object->subtype()) {
        object->addSubtype(string(s, len));
    }
}


BufrIdentifiers::BufrIdentifiers(int centre) : centre_(centre) {
    ostringstream file, deffile;
    file    << buildSharePath("bufr_" + to_string(centre) + ".xml");
    deffile << buildSharePath("bufr_98.xml");
    char buf[BUFSIZ];
    XML_Parser parser = XML_ParserCreate(NULL);
    int done;
    XML_SetUserData(parser, this);
    XML_SetElementHandler(parser, startElement, endElement);

    FILE* in = fopen(file.str().c_str(), "r");

    if (!in) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(file.str());
        }

        // Open the default template for 98!
        // and send a big warning!
        in = fopen(deffile.str().c_str(), "r");
        MagLog::warning() << "No definition file for [" << centre << "]: We use ECMWF definitions " << endl;

        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(deffile.str());
        }
    }

    do {
        size_t len = fread(buf, 1, sizeof(buf), in);
        done       = len < sizeof(buf);
        if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR) {
            ostringstream s;
            s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser)) << " at line  "
              << XML_GetCurrentLineNumber(parser) << ends;
            cerr << s.str() << "\n";
        }
    } while (!done);
    XML_ParserFree(parser);
    fclose(in);
}


int BufrIdentifiers::ident(const string& token) const {
    const_iterator ident = find(token);
    if (ident != end())
        return atoi(ident->second.c_str());
    if (centre_ == 98)
        return -1;
    else
        return BufrIdentTable::get(98).ident(token);
}


BufrIdentTable BufrIdentTable::table_;
BufrFamilyTable BufrFamilyTable::table_;

BufrFamily::BufrFamily(const string& centre) : centre_(centre) {
    ostringstream file;
    file << buildSharePath("bufr_" + centre + ".xml");
    char buf[BUFSIZ];
    XML_Parser parser = XML_ParserCreate(NULL);
    int done;
    XML_SetUserData(parser, this);
    XML_SetElementHandler(parser, startFamilyElement, endFamilyElement);
    XML_SetCharacterDataHandler(parser, character);

    FILE* in = fopen(file.str().c_str(), "r");
    if (!in) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(file.str());
        }
        return;
    }

    do {
        size_t len = fread(buf, 1, sizeof(buf), in);
        done       = len < sizeof(buf);
        if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR) {
            ostringstream s;
            s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser)) << " at line  "
              << XML_GetCurrentLineNumber(parser) << ends;
            cerr << s.str() << "\n";
        }
    } while (!done);
    XML_ParserFree(parser);
    fclose(in);
}

const BufrIdentifiers& BufrIdentTable::get(int centre) {
    const_iterator table = table_.find(centre);
    if (table != table_.end())
        return *(table->second);
    BufrIdentifiers* idents = new BufrIdentifiers(centre);
    table_[centre]          = idents;
    return *idents;
}


class BufrAccessor {
public:
    BufrAccessor() { init(); }

    void init() {
        if (translator_.empty()) {
            translator_["total_cloud"]                      = "cloudCoverTotal";
            translator_["horizontal_visibility"]            = "horizontalVisibility";
            translator_["msl_pressure"]                     = "pressureReducedToMeanSeaLevel";
            translator_["pressure"]                         = "nonCoordinatePressure";
            translator_["present_weather"]                  = "presentWeather";
            translator_["past_weather_1"]                   = "pastWeather1";
            translator_["past_weather_2"]                   = "pastWeather2";
            translator_["pressure_tendency_amount"]         = "3HourPressureChange";
            translator_["pressure_tendency_characteristic"] = "characteristicOfPressureTendency";
        }
    }

    BufrAccessor(const string& descriptor) : descriptor_(descriptor) {
        init();
        auto token = translator_.find(descriptor_);
        if (token == translator_.end())
            MagLog::warning() << "Could not find eccodes key for " << descriptor_ << endl;
        eccodes_ = (token == translator_.end()) ? descriptor_ : token->second;
    }
    virtual ~BufrAccessor(){};
    virtual void operator()(const ObsDecoder&, MvObs&, string&) const {}
    virtual void operator()(const ObsDecoder&, MvObs& obs, double& val) const { val = obs.value(eccodes_); }
    virtual void print() {}
    const string& keyword() { return descriptor_; }

protected:
    string descriptor_;
    string eccodes_;
    static map<string, string> translator_;
};
map<string, string> BufrAccessor::translator_;

class BufrMultiValueAccessor : public BufrAccessor {
public:
    BufrMultiValueAccessor(const string& descriptor, int index) : BufrAccessor(descriptor), index_(index) {}
    virtual ~BufrMultiValueAccessor(){};
    virtual void operator()(const ObsDecoder&, MvObs& obs, double& val) const {
        const BufrIdentifiers& table = BufrIdentTable::get(obs.originatingCentre());
        val                          = obs.valueByOccurrence(index_, descriptor_);
        // FMagLog::dev() << "BufrMultiValueAccessor-Descriptor--->" << descriptor_ << " INDEX--->" << index_
        // F              << " Value--->" << val << endl;
    }
    virtual void print() {}

protected:
    int index_;
};

class BufrLowCloudAccessor : public BufrMultiValueAccessor {
public:
    BufrLowCloudAccessor() : BufrMultiValueAccessor("cloudType", 1) {}
    virtual ~BufrLowCloudAccessor() {}
};

class BufrMediumCloudAccessor : public BufrMultiValueAccessor {
public:
    BufrMediumCloudAccessor() : BufrMultiValueAccessor("cloudType", 2) {}
    virtual ~BufrMediumCloudAccessor() {}
};

class BufrHighCloudAccessor : public BufrMultiValueAccessor {
public:
    BufrHighCloudAccessor() : BufrMultiValueAccessor("cloudType", 3) {}
    virtual ~BufrHighCloudAccessor() {}
};

class BufrLowCloudNebulosityAccessor : public BufrMultiValueAccessor {
public:
    BufrLowCloudNebulosityAccessor() : BufrMultiValueAccessor("cloudAmount", 1) {}
    virtual ~BufrLowCloudNebulosityAccessor() {}
};

class BufrMediumCloudNebulosityAccessor : public BufrMultiValueAccessor {
public:
    BufrMediumCloudNebulosityAccessor() : BufrMultiValueAccessor("cloudAmount", 2) {}
    virtual ~BufrMediumCloudNebulosityAccessor() {}
};

class BufrHighCloudNebulosityAccessor : public BufrMultiValueAccessor {
public:
    BufrHighCloudNebulosityAccessor() : BufrMultiValueAccessor("cloudAmount", 3) {}
    virtual ~BufrHighCloudNebulosityAccessor() {}
};
class BufrLowCloudHeightAccessor : public BufrMultiValueAccessor {
public:
    BufrLowCloudHeightAccessor() : BufrMultiValueAccessor("heightOfBaseOfCloud", 1) {}
    virtual ~BufrLowCloudHeightAccessor() {}
};

class BufrMediumCloudHeightAccessor : public BufrMultiValueAccessor {
public:
    BufrMediumCloudHeightAccessor() : BufrMultiValueAccessor("heightOfBaseOfCloud", 2) {}
    virtual ~BufrMediumCloudHeightAccessor() {}
};

class BufrHighCloudHeightAccessor : public BufrMultiValueAccessor {
public:
    BufrHighCloudHeightAccessor() : BufrMultiValueAccessor("heightOfBaseOfCloud", 3) {}
    virtual ~BufrHighCloudHeightAccessor() {}
};


class BufrTypeAccessor : public BufrAccessor {
public:
    BufrTypeAccessor() { descriptor_ = "type"; };
    virtual ~BufrTypeAccessor(){};
    void operator()(const ObsDecoder&, MvObs& obs, string& val) const {
        val = obs.messageType();

        pair<string, string> type = make_pair(tostring(obs.messageType()), tostring(obs.messageSubtype()));
        map<pair<string, string>, string>::iterator value = types_.find(type);
        if (value == types_.end()) {
            string centre            = tostring(obs.originatingCentre());
            const BufrFamily& family = BufrFamilyTable::get(centre);

            val = family.getType(type.first, type.second);
            if (val == "") {
                MagLog::warning() << "BufrTypeAccessor> No template type for [" << centre << ", " << type.first << ", "
                                  << type.second << "][centre, type, subtype]" << endl;
                val = "position";
            }
            types_[type] = val;
        }
        else {
            val = value->second;
        }
    }
    static map<pair<string, string>, string> types_;
};

map<pair<string, string>, string> BufrTypeAccessor::types_;

class BufrThicknessAccessor : public BufrAccessor {
public:
    BufrThicknessAccessor() {
        descriptor_ = "thickness";
        keyword_    = "geopotential";
    }

    virtual ~BufrThicknessAccessor() {}
    void operator()(const ObsDecoder& decoder, MvObs& obs, double& val) const {
        const BufrIdentifiers& table = BufrIdentTable::get(obs.originatingCentre());
        int type                     = obs.messageType();

        map<int, bool>::const_iterator multilevel = multilevels_.find(type);
        if (multilevel == multilevels_.end()) {
            MagLog::warning() << "BufrThicknessAccessor> Unknown observation type [" << val << "]\n";
            val = kBufrMissingValue;
        }
        if (type == 0 || type == 1) {
            // surface data
            val = 0;
        }
        else {
            // FMagLog::dev()<< " look for --->" << table.ident(altitude_) << " at " << decoder.getLevel();
            // upper-air data
            if (type == 5 || type == 4)
                val = 0;
            else  // Multi-level data {
                val = abs(obs.valueByPressureLevel(decoder.level_, table.ident(keyword_)) -
                          obs.valueByPressureLevel(decoder.level2_, table.ident(keyword_)));
            // FMagLog::dev()<< " : get --->" << val << endl;
        }
    }

protected:
    string keyword_;
};


class BufrMultiLevelAccessor : public BufrAccessor {
public:
    BufrMultiLevelAccessor(const string& descriptor, const string& surface, const string& altitude) :
        surface_(surface), altitude_(altitude) {
        descriptor_ = descriptor;
    }
    BufrMultiLevelAccessor(const string& descriptor) : surface_(descriptor), altitude_(descriptor) {
        descriptor_ = descriptor;
    }
    virtual ~BufrMultiLevelAccessor() {}
    void operator()(const ObsDecoder& decoder, MvObs& obs, double& val) const {
        const BufrIdentifiers& table = BufrIdentTable::get(obs.originatingCentre());
        int type                     = obs.messageType();

        map<int, bool>::const_iterator multilevel = multilevels_.find(type);
        if (multilevel == multilevels_.end()) {
            MagLog::warning() << "BufrMultiLevelAccessor> Unknown observation type [" << val << "]\n";
            val = kBufrMissingValue;
        }
        if (type == 0 || type == 1) {
            // surface data
            val = obs.value(surface_);
        }
        else {
            // MagLog::dev()<< " look for --->" << table.ident(altitude_) << " at " << decoder.getLevel();
            // upper-air data
            if (type == 5 || type == 4)
                val = obs.value(altitude_);
            else {  // Multi-level data {
                val = obs.valueByPressureLevel(decoder.level_, altitude_);
            }
            // MagLog::dev()<< " : get --->" << val << endl;
        }
    }

protected:
    string surface_;
    string altitude_;
};

class BufrGeopotentialAccessor : public BufrMultiLevelAccessor {
public:
    BufrGeopotentialAccessor() : BufrMultiLevelAccessor("geopotential") {}
    virtual ~BufrGeopotentialAccessor() {}
};

class BufrTemperatureAccessor : public BufrMultiLevelAccessor {
public:
    BufrTemperatureAccessor() : BufrMultiLevelAccessor("airTemperature", "airTemperatureAt2M", "airTemperature") {}
    virtual ~BufrTemperatureAccessor() {}
};
class BufrDewPointAccessor : public BufrMultiLevelAccessor {
public:
    BufrDewPointAccessor() :
        BufrMultiLevelAccessor("dewpointTemperature", "dewpointTemperatureAt2M", "dewpointTemperature") {}
    virtual ~BufrDewPointAccessor() {}
};

class BufrWindSpeedAccessor : public BufrMultiLevelAccessor {
public:
    BufrWindSpeedAccessor() : BufrMultiLevelAccessor("windSpeed", "windSpeedAt10M", "windSpeed") {}
    virtual ~BufrWindSpeedAccessor() {}
};

class BufrWindDirectionAccessor : public BufrMultiLevelAccessor {
public:
    BufrWindDirectionAccessor() : BufrMultiLevelAccessor("windDirection", "windDirectionAt10M", "windDirection") {}
    virtual ~BufrWindDirectionAccessor() {}
};

SimpleObjectMaker<BufrTypeAccessor, BufrAccessor> type_accessor("type");
SimpleObjectMaker<BufrTemperatureAccessor, BufrAccessor> temperature_accessor("temperature");
SimpleObjectMaker<BufrDewPointAccessor, BufrAccessor> dewpoint_accessor("dewpoint");
SimpleObjectMaker<BufrGeopotentialAccessor, BufrAccessor> geopotential_accessor("geopotential");
SimpleObjectMaker<BufrThicknessAccessor, BufrAccessor> thickness_accessor("thickness");

SimpleObjectMaker<BufrWindSpeedAccessor, BufrAccessor> wind_speed_accessor("wind_speed");
SimpleObjectMaker<BufrWindDirectionAccessor, BufrAccessor> wind_direction_accessor("wind_direction");

SimpleObjectMaker<BufrLowCloudAccessor, BufrAccessor> low_cloud_accessor("low_cloud");
SimpleObjectMaker<BufrMediumCloudAccessor, BufrAccessor> medium_cloud_accessor("medium_cloud");
SimpleObjectMaker<BufrHighCloudAccessor, BufrAccessor> high_cloud_accessor("high_cloud");

SimpleObjectMaker<BufrLowCloudHeightAccessor, BufrAccessor> low_cloud_nebulosity_accessor("low_cloud_height");
SimpleObjectMaker<BufrMediumCloudHeightAccessor, BufrAccessor> medium_cloud_nebulosity_accessor("medium_cloud_height");
SimpleObjectMaker<BufrHighCloudHeightAccessor, BufrAccessor> high_cloud_nebulosity_accessor("high_cloud_height");

SimpleObjectMaker<BufrLowCloudNebulosityAccessor, BufrAccessor> low_cloud_height_accessor("low_cloud_nebulosity");
SimpleObjectMaker<BufrMediumCloudNebulosityAccessor, BufrAccessor> medium_cloud_height_accessor(
    "medium_cloud_nebulosity");
SimpleObjectMaker<BufrHighCloudNebulosityAccessor, BufrAccessor> high_cloud_heightaccessor("high_cloud_nebulosity");


/*!

\class ObsDecoder

*/
ObsDecoder::ObsDecoder() {
    if (multilevels_.empty()) {
        multilevels_[0] = false;
        multilevels_[1] = false;
        multilevels_[2] = true;
        multilevels_[3] = true;
        multilevels_[4] = false;
        multilevels_[5] = false;
    }
}

ObsDecoder::~ObsDecoder() {}

void ObsDecoder::decode() {
    // Read observation file
    MvObsSet obsSet(file_name_.c_str());
    if (obsSet.messageCount() <= 0) {
        valid_ = false;
        return;
    }
    // Test
    MvObsSetIterator obsIterator(obsSet);
    MvObs obs = obsIterator();

    bool first = true;
    while (obs) {
        obs.expand();  // message needs to be expanded from here

        if (first) {
            ostringstream title;
            title << "Observation: " << obs.obsTime() << " [ type = " << obs.messageType()
                  << " , subtype = " << obs.messageSubtype() << "]";
            title_ = title.str();
            first  = false;
        }

        MvLocation location = obs.location();


        push_back(new UserPoint(location.x(), location.y(), obs.value("airTemperature")));
        obs.clearIterator();  // must do this before the msg handle is deleted by the next assignment
        obs = obsIterator();
    }
}


bool ObsDecoder::findInTypes(const string& val) {
    if (types_.empty())  // No restriction specified by the user --> we accept all the types.
        return true;

    for (stringarray::const_iterator type = types_.begin(); type != types_.end(); ++type) {
        return magCompare(*type, val);
    }
    return false;
}


bool ObsDecoder::checkLevel(double level) {
    if (level == kBufrMissingValue)
        return true;      // Surface data ....
    level = level / 100;  // levels are expressed in Pascal...
    return (level_ - tolerance_ < level && level < level_ + tolerance_);
}


void ObsDecoder::getInfo(const std::set<string>& tokens, multimap<string, string>& values) {
    MvObsSet obsSet(file_name_.c_str());
    if (obsSet.messageCount() <= 0)
        return;
    for (std::set<string>::const_iterator token = tokens.begin(); token != tokens.end(); ++token) {
        std::set<string> noduplicate;
        string value;
        try {
            BufrAccessor* accessor = SimpleObjectMaker<BufrAccessor>::create(*token);
            MvObsSetIterator obsIterator(obsSet);
            MvObs obs;
            accessor->print();
            obs = obsIterator();
            while (obs) {
                // Probably bufr message does not need to be expanded
                (*accessor)(*this, obs, value);
                if (value != "")
                    noduplicate.insert(value);
                obs.clearIterator();  // must do this before the msg handle is deleted by the next assignment
                obs = obsIterator();
            }
        }
        catch (NoFactoryException&) {
            if (MagicsGlobal::strict()) {
                throw;
            }
        }

        for (std::set<string>::const_iterator no = noduplicate.begin(); no != noduplicate.end(); ++no) {
            // FMagLog::debug() << " ObsDecoderToken: " << *token << " -> " << *no << "\n";

            // here we add it to the list, only if it is the obs_types_list!
            if (findInTypes(*no))
                values.insert(make_pair(*token, *no));
        }
    }
}

/*!
   For Each points ... create an CustomisedPoints using the tokens..
   then add it to the list!
*/
void ObsDecoder::customisedPoints(const Transformation& transformation, const std::set<string>& tokens,
                                  CustomisedPointsList& values) {
    MvObsSet obsSet(file_name_.c_str());

    if (obsSet.messageCount() <= 0) {
        valid_ = false;
        return;
    }

    // Test
    MvObsSetIterator obsIterator(obsSet);
    MvObs obs = obsIterator();

    map<string, BufrAccessor*> accessors;
    for (std::set<string>::const_iterator token = tokens.begin(); token != tokens.end(); ++token) {

        try {
            BufrAccessor* accessor = SimpleObjectMaker<BufrAccessor>::create(*token);
            accessors.insert(make_pair(*token, accessor));
        }
        catch (NoFactoryException&) {
            BufrAccessor* accessor = new BufrAccessor(*token);
            accessors.insert(make_pair(*token, accessor));
        }

    }

    // Create the type accessor!
    BufrAccessor* type_accessor = SimpleObjectMaker<BufrAccessor>::create("type");
    string type;
    BufrAccessor* level_accessor = new BufrAccessor("pressure");

    bool first = true;
    double level;
    while (obs) {
        (*type_accessor)(*this, obs, type);

        if (findInTypes(type)) {
            // At this point, the bufr message needs to be expanded
            obs.expand();

            // Create title
            if (first) {
                ostringstream title;
                title << "[ type = " << obs.messageType() << " , subtype = " << obs.messageSubtype() << "]";
                title_ = title.str();
                // FMagLog::debug() << " ObsTitle: " << title_ << "\n";
                first = false;
            }

            (*level_accessor)(*this, obs, level);

            // if (multilevels_[obs.messageType()] == false || checkLevel(level) )
            {
                MvLocation loc = obs.location();
                double value;
                UserPoint geo(loc.x(), loc.y());
                PaperPoint point = transformation(geo);
                if (transformation.in(point)) {
                    CustomisedPoint* point = new CustomisedPoint(loc.x(), loc.y(), obs.findSomeIdent());
                    point->type(type);

                    for (auto accessor = accessors.begin(); accessor != accessors.end(); ++accessor) {
                        (*accessor->second)(*this, obs, value);
                        if (value != kBufrMissingValue)
                            (*point)[accessor->first] = value;
                    }
                    if (!(*point).empty()) {
                        TMetTime date    = obs.msgTime();
                        (*point)["time"] = date.GetHour() * 100 + date.GetMin();
                        values.push_back(point);
                    }
                    else
                        delete point;
                }
            }
        }
        obs.clearIterator();  // must do this before the msg handle is deleted by the next assignment
        obs = obsIterator();
    }
    // clear accessorts
    for (auto accessor = accessors.begin(); accessor != accessors.end(); ++accessor)
        delete accessor->second;
}

PointsHandler& ObsDecoder::points() {
    decode();

    pointsHandlers_.push_back(new PointsHandler(*this));
    return *(pointsHandlers_.back());
}

/*!
 Class information are given to the output-stream.
*/
void ObsDecoder::print(ostream& out) const {
    out << "ObsDecoder[";
    out << "]";
}

void ObsDecoder::visit(TitleNode& title) {
    // title.add(new TitleEntry(title_));
}
