/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file FortranMagics.cc
    \brief Implementation of the Template class FortranMagics.

    Magics Team - ECMWF 2007

    Started: Fri 9-Mar-2007

    Changes:

*/

#include "FortranMagics.h"

#include "Axis.h"
#include "BoxPlotDecoder.h"
#include "BoxPlotVisualiser.h"
#include "Coastlines.h"
#include "CompatibilityHelper.h"
#include "Contour.h"
#include "GeoPointsDecoder.h"
#include "GraphPlotting.h"
#include "GribDecoder.h"
#include "ImportAction.h"
#include "ImportObjectHandler.h"
#include "ImportPlot.h"
#include "InputMatrix.h"
#include "LegendVisitor.h"
#include "MagicsGlobal.h"
#include "MapGenDecoder.h"
#include "MetaData.h"
#include "RootSceneNode.h"
#include "SceneNode.h"
#include "SimplePolylineInput.h"
#include "SimplePolylineVisualiser.h"
#include "SymbolInput.h"
#include "SymbolPlotting.h"
#include "TaylorGrid.h"
#include "TextVisitor.h"
#include "Timer.h"
#include "TitleTemplate.h"
#include "UserPoint.h"
#include "ViewNode.h"
#include "VisualAction.h"
#include "Wind.h"
#include "XYList.h"
#include "MagicsGlobal.h"

#ifdef HAVE_NETCDF
#include "NetcdfDecoder.h"
#endif

#include "EpsBufr.h"
#include "ObsDecoder.h"


#include "ObsJSon.h"
#include "ObsPlotting.h"

using namespace magics;

FortranMagics::FortranMagics() :
    drivers_(0),
    output_(0),
    action_(0),
    root_(0),
    empty_(true),
    gribindex_(0),
    legend_todo_(false),
    symbolinput_todo_(false),
    matrixinput_todo_(false),
    polyinput_todo_(false),
    axisContainer_(0)

{
    reset();
}

FortranMagics::~FortranMagics() {
    reset();
}

void FortranMagics::reset() {
    delete drivers_;
    drivers_ = 0;

    delete output_;
    output_ = 0;

    delete root_;
    root_ = 0;

    empty_            = true;
    gribindex_        = 0;
    legend_todo_      = false;
    symbolinput_todo_ = false;
    matrixinput_todo_ = false;
    polyinput_todo_   = false;


    while (actions_.size()) {
        actions_.pop();
    }

    // TODO: clear memory
    texts_.clear();
    legends_.clear();
    later_.clear();
    while (axis_.size()) {
        axis_.pop();
    }


    ParameterManager::reset();
    Layout::reset();
    CompatibilityHelper::resetAll();
}

/*!
 Class information are given to the output-stream.
*/
void FortranMagics::print(ostream& out) const {
    out << "FortranMagics[";
    out << "]";
}

void FortranMagics::popen() {
    reset();

    MagLog::info() << "popen()" << endl;


    if (getEnvVariable("MAGPLUS_QUIET").empty() && !MagicsGlobal::silent()) {
        MagLog::userInfo() << "----------------------------------------------------"
                              "--------------\n";
        MagLog::userInfo() << "\n";
        MagLog::userInfo() << "			  " << getMagicsVersionString() << "\n";
        MagLog::userInfo() << "\n";
        MagLog::userInfo() << " Meteorological Applications Graphics Integrated Colour System\n";
        MagLog::userInfo() << "\n";
        MagLog::userInfo() << "			    Developed By\n";
        MagLog::userInfo() << "\n";
        MagLog::userInfo() << "   The European Centre for Medium-Range Weather Forecasts\n";
        MagLog::userInfo() << "\n";
        // MagLog::userInfo() << "		      Copyright ECMWF
        // "<<MAGICS_COPYRIGHT_PERIOD<<"\n";
        MagLog::userInfo() << "\n";
        MagLog::userInfo() << "----------------------------------------------------"
                              "--------------\n";
    }
    // actions_.push(&FortranMagics::legend);
    actions_.push(&FortranMagics::subpage);
    actions_.push(&FortranMagics::page);
    actions_.push(&FortranMagics::superpage);
    actions_.push(&FortranMagics::drivers);
}

/*! \brief Main dispatch method
  Here is where the real magics is happen. Everything is dispatched, followed
  by a comprehensive clean-up.
*/
void FortranMagics::pclose() {
    MagLog::info() << "pclose()" << endl;

    if (!empty_) {
        finish();
        dispatch();
    }

    if (root_ && drivers_) {
        BasicGraphicsObject* object = root_->close();
        if (object) {
            /***   Start clean-up  ***/
            drivers_->dispatch(object);
            drivers_->closeDrivers();

            delete root_;
            delete drivers_;
            delete output_;

            drivers_ = 0;
            root_    = 0;
            output_  = 0;
        }
    }

    ParameterManager::reset();
    Layout::reset();


    // the Magics log messages are not broadcast until the next log event -
    // therefore, the last log message will not be broadcast. We fix that by
    // flushing the message streams
    // - we only need to do one of them, and all will be flushed behind the
    // scenes.
    MagLog::info().flush();


    if (getEnvVariable("MAGPLUS_QUIET").empty() && !MagicsGlobal::silent()) {
        MagLog::userInfo() << "----------------------------------------------------"
                              "--------------\n";
        MagLog::userInfo() << "    COMPLETED\n";
        MagLog::userInfo() << "\n";
        MagLog::userInfo() << "    Any problems or suggestions? Please contact us at\n";
        MagLog::userInfo() << "                   magics@ecmwf.int\n";
        MagLog::userInfo() << "----------------------------------------------------"
                              "--------------\n";
    }
}

void FortranMagics::drivers() {
    if (!drivers_)
        drivers_ = new DriverManager();
    else
        drivers_->clearDrivers();
    if (!output_)
        output_ = new OutputHandler();
    output_->set(*drivers_);
}

void FortranMagics::subpage() {
    axisContainer_ = new FortranViewNode();

    axisContainer_->push_back(new MetaDataVisitor());
    top()->push_back(axisContainer_);
    axisContainer_->getReady();
    push(axisContainer_);

    while (!axis_.empty()) {
            axisContainer_->push_back(axis_.top());
            axis_.pop();
    }
}

void FortranMagics::page() {
    if (empty())
        return;

    while (top() != root_) {
        pop();
        if (empty())
            break;
    }

    FortranSceneNode* node = new FortranSceneNode();
    root_->insert(node);
    // Just a test for metadata collection
    MetaDataVisitor* meta = new MetaDataVisitor();
    node->push_back(meta);
    push(node);
}

void FortranMagics::newpage() {
    if (empty())
        return;
    BasicSceneObject* to = top();

    while (to != root_) {
        pop();
        to = top();
        if (empty())
            break;
    }

    root_->newpage();

    // push(node);
}

void FortranMagics::superpage() {
    root_ = new FortranRootSceneNode();

    push(root_);

    root_->getReady();
    drivers_->setDriversWidth(root_->absoluteWidth());
    drivers_->setDriversHeight(root_->absoluteHeight());
    drivers_->openDrivers();
}

void FortranMagics::simplelegend() {
    // used fronm the python interface!
    // add a new legend!
    legends_.clear();

    legend();
}

void FortranMagics::legend() {
    if (legends_.empty() == false)
        return;

    if (!legend_todo_)
        return;

    legend_todo_ = false;
    string mode;
    ParameterManager::get("legend_box_mode", mode);

    if (magCompare(mode, "positional"))
        legends_.push_back(new FortranPositionalLegendVisitor());
    else
        legends_.push_back(new FortranAutomaticLegendVisitor());
}

void FortranMagics::plegend() {
    legend_todo_ = true;
    legends_.clear();
    actions_.push(&FortranMagics::legend);
}

void FortranMagics::poverlay() {
    actions();

    action_             = new VisualAction();
    ImportAction* input = new ImportAction();
    ImportPlot* plot    = new ImportPlot();
    top()->push_back(action_);
    action_->data(input);

    action_->visdef(plot);
}

void FortranMagics::pimport() {
    actions();
    ImportObjectHandler* object = new ImportObjectHandler();
    if (object->overlay_)
        later_.push_back(object);
    else
        top()->push_back(object);
}

void FortranMagics::pnew(const string& type) {
    MagLog::info() << "pnew(" << type << ")" << endl;

    if (magCompare(type, "subpage")) {
        if (empty_)
            return;
        finish();
        pop();
        actions_.push(&FortranMagics::subpage);
    }
    if (magCompare(type, "page")) {
        if (empty_)
            return;
        finish();
        dispatch();
        empty_ = true;
        pop();
        actions_.push(&FortranMagics::legend);
        actions_.push(&FortranMagics::subpage);
        actions_.push(&FortranMagics::page);
    }
    if (magCompare(type, "super_page") || magCompare(type, "superpage")) {
        if (empty_) {
            actions_.push(&FortranMagics::newpage);
            return;
        }
        finish();
        dispatch();
        empty_ = true;
        actions_.push(&FortranMagics::legend);
        actions_.push(&FortranMagics::subpage);
        actions_.push(&FortranMagics::page);
        actions_.push(&FortranMagics::newpage);
    }
    // WE reset !
    axisContainer_ = 0;
    action_        = 0;

    string legend;
    ParameterManager::get("legend", legend);
    legend_todo_ = magCompare(legend, "on");
}

void FortranMagics::actions() {
    Timer timer("magics", "setting");
    while (!actions_.empty()) {
        Action action = actions_.top();
        (this->*action)();
        actions_.pop();

        empty_ = false;
    }

}

void FortranMagics::pcoast() {
    actions();

    Coastlines* coastlines = new Coastlines();
    axisContainer_->push_back(coastlines);
}

void FortranMagics::ptaylor() {
    actions();

    TaylorGrid* taylor = new TaylorGrid();
    top()->push_back(taylor);
}
#include "TephiGrid.h"
void FortranMagics::ptephi() {
    actions();

    TephiGrid* tephi = new TephiGrid();
    top()->push_back(tephi);
}
void FortranMagics::pobs() {
    actions();
    action_         = new VisualAction();
    ObsDecoder* obs = new ObsDecoder();
    if (obs->defined()) {
        action_->data(obs);
        top()->push_back(action_);
        action_->visdef(new ObsPlotting());
        return;
    }
    action_ = new VisualAction();
    action_->data(new ObsJSon());
    top()->push_back(action_);
    action_->visdef(new ObsPlotting());
    return;
}

#include "MatrixTestDecoder.h"
void FortranMagics::ptest() {
    actions();
    action_ = new VisualAction();
    action_->data(new MatrixTestDecoder());
    top()->push_back(action_);
}

/*!  \brief Finish plot by checking axis, legend and texts
 *
 */
void FortranMagics::finish() {
    if (!empty_) {
        actions();  // The flag to force the generation of the plot has been set!

    }

    if (!axisContainer_)
        return;
    // check if we have to add a legend!
    if (!legends_.empty() && axisContainer_ && !axisContainer_->items_empty()) {
        legend();
        for (vector<LegendVisitor*>::iterator legend = legends_.begin(); legend != legends_.end(); ++legend) {
            top()->legend(*legend);
        }
        legends_.clear();
    }

    // Check any text
    for (vector<BasicSceneObject*>::iterator other = later_.begin(); other != later_.end(); ++other) {
        top()->push_back(*other);
    }
    later_.clear();

    for (vector<FortranTextVisitor*>::iterator text = texts_.begin(); text != texts_.end(); ++text) {
        top()->text(*text);
    }
    texts_.clear();
}

/*!  \brief Dispatch scene graph to drivers
 * \sa DriverManager::dispatch()
 * \sa FortranRootSceneNode::visualise()
 */
void FortranMagics::dispatch() {
    if (!root_)  // Nothing has been done so far!
        return;
    drivers_->dispatch(root_->visualise());
    MetaDataVisitor::collect();
    root_->release();
}

void FortranMagics::pmapgen() {
    actions();

    action_ = new VisualAction();
    action_->data(new MapGenDecoder());
    top()->push_back(action_);
}

#include "ContourLibrary.h"
#include "MetaData.h"
const char* FortranMagics::metagrib() {
    GribDecoder grib;
    ContourLibrary* library = MagTranslator<string, ContourLibrary>()("ecmwf");

    // Here we try call the Contour libry to set up visual properties...
    MetaDataCollector request, needAttributes;
    MagDef attributes;

    library->askId(request);
    grib.ask(request);

    StyleEntry style;
    library->getStyle(request, attributes, style);
    
     

    string print = getEnvVariable("MAGPLUS_DATA");
    if (print.size()) {
        cout << "Metadata for " << grib.file_name_ << endl;
        for (auto r = request.begin(); r != request.end(); ++r) {
            cout << r->first << "=" << r->second << endl;
        }
        cout << "-----------------------------" << endl;
    }
    ostringstream out;
    out << style;
    static string temp;
    temp = out.str();
    return temp.c_str();
}

const char* FortranMagics::knownDrivers() {
    vector<string> drivers;


    OutputHandler::drivers(drivers);

    ostringstream out;

    out << "{ \"drivers\" : [";

    string sep = "";

    for (auto& d : drivers) {
        out << sep << "\"" << d << "\"";

        sep = ", ";
    }
    out << "]}";


    static string results;
    results = out.str();


    return results.c_str();
}

#include "MagConfig.h"

const char* FortranMagics::detect(const string& data, const string& dim) {
    DimensionGuess json(data);

    NetcdfGuess guesser;
    static string empty;
    static string result;

    auto checks = guesser.guess_.find(dim);
    if (checks == guesser.guess_.end()) {
        return empty.c_str();
    }
    result.clear();

    for (auto check = checks->second.begin(); check != checks->second.end(); ++check) {
        vector<string> values = check->second;
        for (auto d = json.data_.begin(); d != json.data_.end(); ++d) {
            auto def   = d->second;
            auto found = def.find(check->first);
            if (found != def.end()) {
                string value = found->second;
                for (vector<string>::iterator v = values.begin(); v != values.end(); ++v) {
                    // string val = value.substr(0, v->size());
                    if (v->compare(value) == 0) {
                        result = d->first;
                        cout << " " << result << endl;
                        return result.c_str();
                    }
                }
            }
        }
    }

    return empty.c_str();
}

const char* FortranMagics::metanetcdf() {
#ifdef HAVE_NETCDF
    NetcdfDecoder netcdf;
    ContourLibrary* library = MagTranslator<string, ContourLibrary>()("ecmwf");

    // Here we try call the Contour libry to set up visual properties...
    MetaDataCollector request, needAttributes;
    MagDef attributes;

    library->askId(request);
    netcdf.visit(request);

    StyleEntry style;
    library->getStyle(request, attributes, style);
    ostringstream out;
    out << style;
    static string temp;
    temp = out.str();
    return temp.c_str();
#else
    return 0;
#endif
}

const char* FortranMagics::metainput() {
    InputMatrix data;
    ContourLibrary* library = MagTranslator<string, ContourLibrary>()("ecmwf");

    // Here we try call the Contour libry to set up visual properties...
    MetaDataCollector request, needAttributes;
    MagDef attributes;

    library->askId(request);
    data.visit(request);

    StyleEntry style;
    library->getStyle(request, attributes, style);
    ostringstream out;
    out << style;
    static string temp;
    temp = out.str();
    return temp.c_str();
}


void FortranMagics::pgrib() {
    actions();
    action_ = new VisualAction();
    static string gribfile;

    string grib;
    ParameterManager::get("grib_input_file_name", grib);
    int index;
    ParameterManager::get("grib_field_position", index);

    if (grib == gribfile && MagicsGlobal::compatibility()) {
        if (index == gribindex_) {
            gribindex_++;
        }
        else {
            gribindex_ = index;
        }
        ParameterManager::set("grib_field_position", gribindex_);
    }
    else {
        gribfile   = grib;
        gribindex_ = index;
    }

    action_->data(new GribDecoder());
    top()->push_back(action_);
}

void FortranMagics::pimage() {
    MagLog::warning() << " pimage is deprecated! Please use pcont." << endl;
}


void FortranMagics::pgeo() {
    actions();
    action_ = new VisualAction();
    action_->data(new GeoPointsDecoder());
    top()->push_back(action_);
}

void FortranMagics::pnetcdf() {
#ifdef HAVE_NETCDF
    actions();

    action_ = new VisualAction();
    action_->data(new NetcdfDecoder());
    top()->push_back(action_);
#endif
}

#include "InputData.h"
void FortranMagics::pinput() {
    actions();

    action_ = new VisualAction();
    action_->data(new InputData());
    top()->push_back(action_);
}
#include "TableDecoder.h"
void FortranMagics::ptable() {
    actions();

    action_ = new VisualAction();
    action_->data(new TableDecoder());
    top()->push_back(action_);
}

#ifdef HAVE_ODB
#include "OdaDecoder.h"
#endif
void FortranMagics::podb() {
    actions();
#ifdef HAVE_ODB

    action_ = new VisualAction();
    action_->data(new OdaGeoDecoder());
    top()->push_back(action_);

#endif
}

void FortranMagics::data(Data* data) {
    ASSERT(action_ == 0);
    action_ = new VisualAction();
    action_->data(data);
    top()->push_back(action_);
}

bool FortranMagics::geographical() {
    string projection;
    ParameterManager::get("subpage_map_projection", projection);
    if (magCompare(projection, "cartesian"))
        return false;
    if (magCompare(projection, "taylor"))
        return false;
    if (magCompare(projection, "thermo"))
        return false;
    if (magCompare(projection, "tephiinfo"))
        return false;
    return true;
}

template <class T>
void param(const string& from, const string& to, T& val) {
    ParameterManager::get(from, val);
    ParameterManager::set(to, val);
}

void FortranMagics::pcont() {
    // First check any split contour! I hate it!
    Timer timer("pcont", "setting");
    actions();

    if (!action_ || matrixinput_todo_) {
        action_            = new VisualAction();
        InputMatrix* input = new InputMatrix();
        matrixinput_todo_  = false;
        if (input->defined())
            action_->data(input);
        else {
            delete input;
            // Sylvie: Is this causing GribDecoder MagExceptions when matrx input is
            // faulty?
            action_->data(new GribDecoder());
        }
        top()->push_back(action_);
    }

    action_->visdef(new Contour());
}

void FortranMagics::pwind() {
    actions();
    if (matrixinput_todo_) {
        action_ = 0;
    }
    if (!action_) {
        action_            = new VisualAction();
        InputMatrix* input = new InputMatrix();
        matrixinput_todo_  = false;
        if (input->defined())
            action_->data(input);
        else {
            delete input;
            // Sylvie: Is this causing GribDecoder MagExceptions when matrx input is
            // faulty?
            GribDecoder* grib = new GribDecoder();
            grib->dimension(2);
            action_->data(grib);
        }
        top()->push_back(action_);
    }
    action_->set2D();
    action_->visdef(new Wind());
    action_ = 0;
}

void FortranMagics::ptext() {
    string mode;
    ParameterManager::get("text_mode", mode);

    FortranTextVisitor* node;
    if (magCompare(mode, "positional"))
        node = new FortranPositionalTextVisitor();
    else
        node = new FortranAutomaticTextVisitor();

    texts_.push_back(node);
    empty_ = false;
}

void FortranMagics::psymb() {
    actions();

    string mode;
    string wind;
    ParameterManager::get("symbol_position_mode", mode);
    ParameterManager::get("symbol_type", wind);
    if (magCompare(mode, "graph")) {
        action_            = new VisualAction();
        SymbolInput* input = new SymbolInput();
        top()->push_back(action_);
        action_->data(input);
        MagLog::dev() << *input << "\n";
        SymbolPlotting* symbol = new SymbolPlotting();
        MagLog::dev() << *symbol << "\n";
        action_->visdef(symbol);
        action_ = 0;
    }
    else {
        if (!action_ || symbolinput_todo_) {
            action_            = new VisualAction();
            SymbolInput* input = new SymbolInput();
            top()->push_back(action_);
            action_->data(input);
            MagLog::dev() << *input << "\n";
            symbolinput_todo_ = false;
        }
        if (magCompare(wind, "wind")) {
            Wind* wind = new Wind();
            MagLog::dev() << *wind << "\n";
            action_->visdef(wind);
        }
        else {
            SymbolPlotting* symbol = new SymbolPlotting();
            MagLog::dev() << *symbol << "\n";
            action_->visdef(symbol);
        }
        action_ = 0;
    }
}

void FortranMagics::pline() {
    actions();

    if (!action_ || polyinput_todo_) {
        action_ = new VisualAction();

        polyinput_todo_            = false;
        SimplePolylineInput* input = new SimplePolylineInput();
        top()->push_back(action_);
        action_->data(input);
    }

    action_->visdef(new SimplePolylineVisualiser());
    action_ = 0;
}
#include "GeoJSon.h"
void FortranMagics::geojson() {
    actions();

    action_ = new VisualAction();

    GeoJSon* geo = new GeoJSon();

    top()->push_back(action_);
    action_->data(geo);
}
#include "EpsGraph.h"
#include "EpsXmlInput.h"
#include "WrepJSon.h"
void FortranMagics::wrepjson() {
    actions();

    action_ = new VisualAction();

    WrepJSon* wrep = new WrepJSon();

    top()->push_back(action_);
    action_->data(wrep);
}
void FortranMagics::metbufr() {

    actions();
    action_ = new VisualAction();

    EpsBufr* bufr = new EpsBufr();

    top()->push_back(action_);
    action_->data(bufr);
    return;
}

void FortranMagics::epsinput() {
    actions();
    action_ = new VisualAction();

    EpsXmlInput* input = new EpsXmlInput();

    top()->push_back(action_);
    action_->data(input);
}
#include "MetgramGraph.h"
void FortranMagics::metgraph() {
    actions();

    ASSERT(action_);

    MetgramGraph* graph = new MetgramGraph();

    action_->visdef(graph);
}

void FortranMagics::epscloud() {
    actions();

    ASSERT(action_);

    EpsCloud* epscloud = new EpsCloud();

    action_->visdef(epscloud);
}
void FortranMagics::epsplumes() {
    actions();

    ASSERT(action_);

    EpsPlume* plumes = new EpsPlume();

    action_->visdef(plumes);
}

void FortranMagics::epsgraph() {
    actions();

    ASSERT(action_);

    EpsGraph* epsgraph = new EpsGraph();
    action_->visdef(epsgraph);
}

void FortranMagics::epslight() {
    actions();

    ASSERT(action_);

    EpsLight* epslight = new EpsLight();
    action_->visdef(epslight);
}

void FortranMagics::epswave() {
    actions();

    ASSERT(action_);

    EpsWave* eps = new EpsWave();
    action_->visdef(eps);
}

void FortranMagics::epswind() {
    actions();

    ASSERT(action_);

    EpsWind* epswind = new EpsWind();
    action_->visdef(epswind);
}

void FortranMagics::epsbar() {
    EpsBar* epsbar = new EpsBar();
    action_->visdef(epsbar);
}
void FortranMagics::epsshading() {
    actions();

    ASSERT(action_);

    EpsShade* eps = new EpsShade();
    action_->visdef(eps);
}

void FortranMagics::paxis() {


    try {
        string orientation;

        ParameterManager::get("axis_orientation", orientation);
        //
        if (magCompare(orientation, "vertical")) {
            Axis* vaxis = new VerticalAxis();
            MagLog::dev() << *vaxis << "\n";
            if (axisContainer_)
                axisContainer_->push_back(vaxis);
            else
                axis_.push(vaxis);

        }
        else {
            Axis* haxis = new HorizontalAxis();
            MagLog::dev() << *haxis << "\n";
            if (axisContainer_)
                axisContainer_->push_back(haxis);
            else
                axis_.push(haxis);

        }
    }
    catch (MagicsException& e) {
        if (MagicsGlobal::strict()) {
            throw;
        }

        MagLog::error() << e << "\n";
    }
    empty_ = false;  // Force the generation of the plot!
}

void FortranMagics::prepare() {
    actions();
}

void FortranMagics::pgraph() {
    actions();

    GraphPlotting* graph = new GraphPlotting();

    // do we need a new Action?
    // we check the previous one!
    // if it looks ok we create a new one

    if ((action_ && action_->isValid()) || !action_) {
        action_ = new VisualAction();
        top()->push_back(action_);
        XYList* input = new XYList();
        action_->data(input);
        MagLog::dev() << *input << "\n";
    }

    action_->visdef(graph);
}

#include "TileDecoder.h"
void FortranMagics::ptile() {
    actions();

    action_            = new VisualAction();
    TileDecoder* input = new TileDecoder();
    cout << "FortranMagics::ptile()" << endl;
    if (input->ok()) {
        action_->data(input);
    }
    else {
        MagLog::error() << "Can not create tile" << endl;
        delete input;
        GribDecoder* grib = new GribDecoder();
        action_->data(grib);
    }
    top()->push_back(action_);
}

void FortranMagics::pboxplot() {
    actions();

    action_ = new VisualAction();

    BoxPlotDecoder* input   = new BoxPlotDecoder();
    BoxPlotVisualiser* plot = new BoxPlotVisualiser();
    top()->push_back(action_);
    action_->data(input);
    MagLog::dev() << *input << "\n";
    action_->visdef(plot);
}

FortranMagics& FortranMagics::instance() {
    static FortranMagics instance_;
    return instance_;
}
