/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file XmlMagics.cc
    \brief Implementation of the Template class XmlMagics.
    \author Meteorological Visualisation Section, ECMWF

    Started: Apr-2007

*/


#include "XmlMagics.h"
#include "MagException.h"
#include "Timer.h"
#include "XmlReader.h"

#include "BinaryObject.h"
#include "Coastlines.h"
#include "RootSceneNode.h"
#include "SceneNode.h"
#include "TaylorGrid.h"
#include "TephiGrid.h"
#include "ViewNode.h"
#ifdef HAVE_GRIB
#include "GribDecoder.h"
#endif
#include "Contour.h"
#include "EpsXmlInput.h"
#include "GeoJSon.h"
#include "SymbolPlotting.h"
#include "TableDecoder.h"
#include "VisualAction.h"
#include "Wind.h"
#include "WrepJSon.h"
#include "XYList.h"
//#include "Histogram.h"

#include "LegendVisitor.h"
#include "TextVisitor.h"

#include "Axis.h"
#include "MatrixTestDecoder.h"
#include "UserPoint.h"

#include "SimplePolylineInput.h"
#include "SimplePolylineVisualiser.h"

#include "MetaData.h"


#ifdef HAVE_SPOT
#include "ClassicMtgDecoder.h"
#include "EpsgramDecoder.h"

#endif

#include "EpsGraph.h"
#include "MetgramGraph.h"

using namespace magics;

XmlMagics::XmlMagics() : root_(0), gribloop_(0), geographical_(true), driversToSet_(true) {
    actions_["magics"] = &XmlMagics::magics;
    actions_["page"]   = &XmlMagics::page;

    actions_["mgb"] = &XmlMagics::binary;

    actions_["text"]      = &XmlMagics::text;
    actions_["map"]       = &XmlMagics::map;
    actions_["matrix"]    = &XmlMagics::matrix;
    actions_["cartesian"] = &XmlMagics::cartesian;
    actions_["taylor"]    = &XmlMagics::cartesian;


    actions_["cylindrical"]         = &XmlMagics::geographical;
    actions_["polar_stereographic"] = &XmlMagics::geographical;

    actions_["horizontal_axis"] = &XmlMagics::horizontalAxis;
    actions_["vertical_axis"]   = &XmlMagics::verticalAxis;
    actions_["coastlines"]      = &XmlMagics::coastlines;
    actions_["taylorgrid"]      = &XmlMagics::taylor;
    actions_["tephigrid"]       = &XmlMagics::tephigrid;
    actions_["thermo"]          = &XmlMagics::cartesian;
    actions_["tephigram"]       = &XmlMagics::cartesian;
    actions_["grib"]            = &XmlMagics::grib;
    actions_["netcdf"]          = &XmlMagics::netcdf;
    actions_["table"]           = &XmlMagics::table;

    actions_["odb"]     = &XmlMagics::odb;
    actions_["xyinput"] = &XmlMagics::xyinput;
    actions_["input"]   = &XmlMagics::input;

    actions_["wind"]      = &XmlMagics::wind;
    actions_["image"]     = &XmlMagics::image;
    actions_["symbol"]    = &XmlMagics::symbol;
    actions_["contour"]   = &XmlMagics::contour;
    actions_["histogram"] = &XmlMagics::histogram;
    actions_["legend"]    = &XmlMagics::legend;
    actions_["meta"]      = &XmlMagics::metadata;

    actions_["drivers"] = &XmlMagics::driver;

    actions_["plot"]      = &XmlMagics::layer;
    actions_["geopoints"] = &XmlMagics::geopoints;
    actions_["cdfgraph"]  = &XmlMagics::cdfgraph;
    actions_["efigraph"]  = &XmlMagics::efigraph;
    actions_["efigram"]   = &XmlMagics::efigram;

    actions_["metgraph"] = &XmlMagics::metgraph;
    actions_["metgram"]  = &XmlMagics::metgram;

    actions_["gribloop"] = &XmlMagics::gribloop;

    actions_["epsxml"]   = &XmlMagics::epsxml;
    actions_["wrepjson"] = &XmlMagics::wrepjson;
    actions_["geojson"]  = &XmlMagics::geojson;

    actions_["epsbufr"]      = &XmlMagics::epsbufr;
    actions_["epsgraph"]     = &XmlMagics::epsgraph;
    actions_["epsgram"]      = &XmlMagics::epsgram;
    actions_["epswind"]      = &XmlMagics::epswind;
    actions_["capebox"]      = &XmlMagics::capebox;
    actions_["epscloud"]     = &XmlMagics::epscloud;
    actions_["epsbar"]       = &XmlMagics::epsbar;
    actions_["epswave"]      = &XmlMagics::epswave;
    actions_["epsshade"]     = &XmlMagics::epsshading;
    actions_["epsplume"]     = &XmlMagics::epsplume;
    actions_["epsdirection"] = &XmlMagics::epsdirection;

    actions_["mapgen"] = &XmlMagics::mapgen;
    actions_["graph"]  = &XmlMagics::graph;

    actions_["polyline_input"] = &XmlMagics::polyinput;
    actions_["polyline"]       = &XmlMagics::polyline;

    actions_["layer"]  = &XmlMagics::split;
    actions_["thread"] = &XmlMagics::thread;
    actions_["import"] = &XmlMagics::import;
}


XmlMagics::~XmlMagics() {}

/*!
 Class information are given to the output-stream.
*/
void XmlMagics::print(ostream& out) const {
    out << "XmlMagics[";
    out << "]";
}

void XmlMagics::execute(XmlTree& tree) {
    {
        try {
            tree.visit(*this);
        }
        catch (MagicsException& e) {
            MagLog::debug() << e.what() << endl;
        }

        if (driversToSet_)
            output_.set(drivers_);

        ASSERT(root_);

        root_->execute();

        if (root_->scalingFactor() != 1)
            drivers_.setOutputWidth(root_->scalingFactor());


        drivers_.setDriversWidth(root_->absoluteWidth());
        drivers_.setDriversHeight(root_->absoluteHeight());

        {
            Timer timer("drivers", "rendering of the graphical tree");
            drivers_.openDrivers();
            drivers_.dispatch(root_->root());

            drivers_.closeDrivers();
        }
    }
    MetaDataVisitor::collect();
    // root_->metadata(meta);
    /* later
        std::map<string, string>::const_iterator filename = meta.find("filename");
        if ( filename != meta.end() && !filename->second.empty()) {
            ofstream file (filename->second.c_str());

            for (std::map<string, string>::const_iterator d = meta.begin(); d != meta.end(); ++d) {
                if ( d != filename )
                    file << d->first << " = " << d->second << "; " << endl;
            }
        }
    */
    output_.clear();
    // ParameterManager::release();
    delete root_;
    root_ = 0;
}

void XmlMagics::execute(const string& file, std::map<string, string>&) {
    XmlReader parser(true);
    XmlTree tree;
    ParameterManager::set(string("layout"), string("magml"));
    try {
        parser.interpret(file, &tree);
        execute(tree);
    }
    catch (MagicsException& e) {
        MagLog::debug() << e.what() << endl;
    }
}

void XmlMagics::display(const string& file) {
    XmlReader parser(true);
    XmlTree tree;

    try {
        parser.interpret(file, &tree);
        tree.visit(*this);
    }
    catch (MagicsException& e) {
        MagLog::debug() << e.what() << endl;
    }

    if (driversToSet_)
        output_.set(drivers_);

    ASSERT(root_);
    root_->getReady();
    drivers_.setDriversWidth(root_->absoluteWidth());
    drivers_.setDriversHeight(root_->absoluteHeight());

    root_->execute();
    drivers_.openDrivers();
    drivers_.dispatch(root_->root());

    drivers_.closeDrivers();
}

void XmlMagics::visit(const XmlNode& node) {
    std::map<string, Action>::iterator action = actions_.find(node.name());

    if (action == actions_.end()) {
        MagLog::dev() << " MagML - ignoring tag: " << node.name() << endl;
        return;
    }
    (this->*action->second)(node);
}

void XmlMagics::magics(const XmlNode& node) {
    ParameterManager::set(string("layout"), string("magml"));
    const float version_ = 3.0;
    MagLog::debug() << " You are using the " << fixed << setprecision(1) << version_
                    << " version of the magml interpreter\n";
    string version = node.getAttribute("version");
    if (version.empty()) {
        MagLog::error() << " No version defined in your magml file\n"
                        << " Compatibilty issue: check your magml file and add a version number" << endl;
        return;
    }
    float v;
    std::stringstream sv(version);
    sv >> v;
    if (v < version_) {
        MagLog::error() << " The version defined in the file is " << fixed << setprecision(1) << v
                        << "\n Compatibilty issue: check your magml file and update the version number" << endl;
        return;
    }
    string method = node.getAttribute("method");

    if (method == "wrep")
        root_ = new WrepRootSceneNode();
    else if (method == "legacy")
        root_ = new LegacyRootSceneNode();
    else
        root_ = new XmlRootSceneNode();

    push(root_);
    root_->set(node);
    root_->getReady();
    node.visit(*this);
}


void XmlMagics::driver(const XmlNode& node) {
    for (auto& driver : node.elements())
        output_.set(*driver, drivers_);
    driversToSet_ = false;
}


void XmlMagics::split(const XmlNode& node) {
    /*
        LayerNode* layer = new LayerNode();
        layer->set(node);
        top()->push_back(layer);
        push(layer);
        node.visit(*this);
        pop();
    */
    node.visit(*this);
}


void XmlMagics::thread(const XmlNode&) {
    /* later
        ThreadNode* thread = new ThreadNode();
        thread->set(node);
        top()->push_back(thread);
        push(thread);
        node.visit(*this);
        pop();
    */
}

void XmlMagics::metadata(const XmlNode& node) {
    MetaDataVisitor* meta = new MetaDataVisitor();
    meta->set(node);
    top()->push_back(meta);
}


void XmlMagics::page(const XmlNode& node) {
    XmlSceneNode* page = new XmlSceneNode();

    page->set(node);
    top()->insert(page);
    push(page);
    node.visit(*this);
    pop();
}

void XmlMagics::legend(const XmlNode& node) {
    LegendVisitor* legend = new XmlLegendVisitor();
    legend->set(node);

    top()->legend(legend);
    node.visit(*this);
}

void XmlMagics::text(const XmlNode& node) {
    XmlTextVisitor* text = new XmlTextVisitor();
    text->set(node);
    top()->text(text);

    node.visit(*this);
}

void XmlMagics::map(const XmlNode& node) {
    XmlViewNode* view = new XmlViewNode();
    view->set(node);
    BasicSceneObject* parent = top()->insert(view);
    if (parent != top()) {
        pop();
        push(parent);
    }

    push(view);
    node.visit(*this);
    pop();
}

void XmlMagics::binary(const XmlNode& node) {
    BinaryObject* binary = new BinaryObject();
    binary->set(node);
    top()->push_back(binary);
}

void XmlMagics::coastlines(const XmlNode& node) {
    Coastlines* coast = new Coastlines();
    coast->set(node);
    top()->push_back(coast);
}

void XmlMagics::taylor(const XmlNode& node) {
    TaylorGrid* grid = new TaylorGrid();
    grid->set(node);
    top()->push_back(grid);
}
void XmlMagics::tephigrid(const XmlNode& node) {
    TephiGrid* grid = new TephiGrid();
    grid->set(node);
    top()->push_back(grid);
}
void XmlMagics::horizontalAxis(const XmlNode& node) {
    HorizontalAxis* axis = new HorizontalAxis();
    axis->set(node);
    top()->push_back(axis);
}

void XmlMagics::verticalAxis(const XmlNode& node) {
    VerticalAxis* axis = new VerticalAxis();
    axis->set(node);
    top()->push_back(axis);
}

void XmlMagics::cartesian(const XmlNode&) {
    geographical_ = false;
}

void XmlMagics::geographical(const XmlNode&) {
    geographical_ = true;
}

void XmlMagics::layer(const XmlNode& node) {
    if (geographical_) {
        VisualAction* action = new VisualAction();
        action->set(node);
        top()->push_back(action);
        push(action);
    }
    else {
        VisualAction* action = new VisualAction();
        action->set(node);
        top()->push_back(action);
        push(action);
    }

    node.visit(*this);
    pop();
}

#ifdef HAVE_GRIB
GribDecoder* grib_handler;

void XmlMagics::gribloop(const XmlNode& node) {
    if (gribloop_)
        delete gribloop_;
    gribloop_ = new GribLoop();
    gribloop_->set(node);

    actions_["grib"]  = &XmlMagics::gribinloop;
    actions_["layer"] = &XmlMagics::splitinloop;

    while (gribloop_->hasMore()) {
        node.visit(*this);
    }
}


void XmlMagics::splitinloop(const XmlNode&) {
    /* later!
    ASSERT(gribloop_);

    LayerNode* layer = new LayerNode();
    layer->set(node);
    gribloop_->set(*layer);
    top()->push_back(layer);
    push(layer);
    this->layer(node);
    pop();
    */
}

void XmlMagics::gribinloop(const XmlNode&) {
    ASSERT(gribloop_);
    top()->data(gribloop_->current());
}

#include "TileDecoder.h"

void XmlMagics::grib(const XmlNode& node) {
    /*
    string tile = node.getAttribute("tile");
    if (magCompare(tile, "on")) {
        TileDecoder* tile = new TileDecoder();
        tile->set(node);
        if (tile->ok()) {
            top()->data(tile);
            return;
        }
        cout << "TRYING DELETE" << endl;

        delete tile;
    }
    cout << "creating grib" << endl;
    GribDecoder* grib = new GribDecoder();
    grib->set(node);
    cout << "grib created and set" << endl;
    top()->data(grib);
    cout << "action updated" << endl;
    */
    cout << node << endl;
    GribDecoder* grib = new GribDecoder();
    grib->set(node);

    top()->data(grib);
}
#else
void XmlMagics::gribloop(const XmlNode&) {
    MagLog::warning() << " Attempt to call GribLoop but GRIB support is disabled!" << endl;
}

void XmlMagics::gribinloop(const XmlNode&) {
    MagLog::warning() << " Attempt to call GribInLoop but GRIB support is disabled!" << endl;
}

void XmlMagics::grib(const XmlNode&) {
    MagLog::warning() << " Attempt to call Grib decoding but GRIB support is disabled!" << endl;
}
#endif


#include "GeoPointsDecoder.h"
void XmlMagics::geopoints(const XmlNode& node) {
    GeoPointsDecoder* geo = new GeoPointsDecoder();
    geo->set(node);
    top()->data(geo);
}

#include "MapGenDecoder.h"
void XmlMagics::mapgen(const XmlNode& node) {
    if (geographical_) {
        MapGenDecoder* mapgen = new MapGenDecoder();
        mapgen->set(node);
        top()->data(mapgen);
    }
    else {
        MapGenDecoder* mapgen = new MapGenDecoder();
        mapgen->set(node);
        top()->data(mapgen);
    }
}

void XmlMagics::xyinput(const XmlNode& node) {
    if (geographical_) {
        XYList* list = new XYList();
        list->set(node);
        top()->data(list);
    }
    else {
        XYList* list = new XYList();
        list->set(node);
        top()->data(list);
    }
}

#include "InputData.h"
void XmlMagics::input(const XmlNode& node) {
    InputData* data = new InputData();
    data->set(node);
    top()->data(data);
}

#include "GraphPlotting.h"
void XmlMagics::graph(const XmlNode& node) {
    GraphPlotting* graph = new GraphPlotting();
    graph->set(node);
    top()->visdef(graph);
}

void XmlMagics::histogram(const XmlNode& node) {
    /*
    Histogram* histogram =  new Histogram();
    histogram->set(node);
    top()->visdef(histogram);
    */
}


/***************************************************************************

    O D B / O D A

***************************************************************************/

#ifdef HAVE_ODB
#include "OdaDecoder.h"
#endif

void XmlMagics::odb(const XmlNode& node) {
#ifdef HAVE_ODB
    OdaGeoDecoder* odb = new OdaGeoDecoder();
    odb->set(node);
    top()->data(odb);
#else
    MagLog::warning() << "ODB support is NOT enabled!\n";
#endif
}

/* ********************************************************************* */


void XmlMagics::efigram(const XmlNode& node) {
#ifdef HAVE_SPOT
    EfigramDecoder* efi = new EfigramDecoder();
    efi->set(node);
    top()->data(efi);
#endif
}

void XmlMagics::cdfgraph(const XmlNode& node) {
    CdfGraph* cdf = new CdfGraph();
    cdf->set(node);
    top()->visdef(cdf);
}

void XmlMagics::efigraph(const XmlNode& node) {
    EfiGraph* efi = new EfiGraph();
    efi->set(node);
    top()->visdef(efi);
}


void XmlMagics::epsplume(const XmlNode& node) {
    EpsPlume* plume = new EpsPlume();
    plume->set(node);
    top()->visdef(plume);
}

void XmlMagics::epsdirection(const XmlNode& node) {
    EpsDirection* direction = new EpsDirection();
    direction->set(node);
    top()->visdef(direction);
}

void XmlMagics::metgram(const XmlNode& node) {
#ifdef HAVE_SPOT
    ClassicMtgDecoder* metgram = new ClassicMtgDecoder();
    metgram->set(node);
    top()->data(metgram);
#endif
}

void XmlMagics::metgraph(const XmlNode& node) {
    MetgramGraph* metgraph = new MetgramGraph();
    metgraph->set(node);
    top()->visdef(metgraph);
}


void XmlMagics::epsgram(const XmlNode& node) {
#ifdef HAVE_SPOT
    EpsgramDecoder* eps = new EpsgramDecoder();
    eps->set(node);
    top()->data(eps);
#endif
}

void XmlMagics::epsxml(const XmlNode& node) {
    EpsXmlInput* eps = new EpsXmlInput();
    eps->set(node);
    top()->data(eps);
}
void XmlMagics::wrepjson(const XmlNode& node) {
    WrepJSon* wrep = new WrepJSon();
    wrep->set(node);
    top()->data(wrep);
}
void XmlMagics::geojson(const XmlNode& node) {
    GeoJSon* geo = new GeoJSon();
    geo->set(node);
    top()->data(geo);
}

#ifdef HAVE_BUFR
#include "EpsBufr.h"
#endif
void XmlMagics::epsbufr(const XmlNode& node) {
#ifdef HAVE_BUFR
    EpsBufr* eps = new EpsBufr();
    eps->set(node);
    top()->data(eps);
#endif
}

void XmlMagics::epsgraph(const XmlNode& node) {
    EpsGraph* epsgraph = new EpsGraph();
    epsgraph->set(node);
    top()->visdef(epsgraph);
}

void XmlMagics::epswave(const XmlNode& node) {
    EpsWave* eps = new EpsWave();
    eps->set(node);
    top()->visdef(eps);
}

void XmlMagics::epswind(const XmlNode& node) {
    EpsWind* epswind = new EpsWind();
    epswind->set(node);
    top()->visdef(epswind);
}

void XmlMagics::epscloud(const XmlNode& node) {
    EpsCloud* epscloud = new EpsCloud();
    epscloud->set(node);
    top()->visdef(epscloud);
}
void XmlMagics::epsbar(const XmlNode& node) {
    EpsBar* epsbar = new EpsBar();
    epsbar->set(node);
    top()->visdef(epsbar);
}
void XmlMagics::capebox(const XmlNode& node) {
    CapeBox* cape = new CapeBox();
    cape->set(node);
    top()->visdef(cape);
}
void XmlMagics::epsshading(const XmlNode& node) {
    EpsShade* eps = new EpsShade();
    eps->set(node);
    top()->visdef(eps);
}

#ifdef HAVE_NETCDF
#include "NetcdfDecoder.h"
#endif
void XmlMagics::netcdf(const XmlNode& node) {
#ifdef HAVE_NETCDF
    if (geographical_) {
        NetcdfDecoder* netcdf = new NetcdfDecoder();
        netcdf->set(node);
        top()->data(netcdf);
    }
    else {
        NetcdfDecoder* netcdf = new NetcdfDecoder();
        netcdf->set(node);
        top()->data(netcdf);
    }
#endif
}

void XmlMagics::table(const XmlNode& node) {
    TableDecoder* table = new TableDecoder();
    table->set(node);
    top()->data(table);
}

#include "InputMatrix.h"
void XmlMagics::matrix(const XmlNode& node) {
    // MatrixTestDecoder* matrix = new MatrixTestDecoder();
    // matrix->set(node);
    if (geographical_) {
        InputMatrix* data = new InputMatrix();
        data->set(node);
        top()->data(data);
    }
    else {
        InputMatrix* data = new InputMatrix();
        data->set(node);
        top()->data(data);
    }
}

void XmlMagics::polyinput(const XmlNode& node) {
    SimplePolylineInput* input = new SimplePolylineInput();
    input->set(node);
    top()->data(input);
}

void XmlMagics::polyline(const XmlNode& node) {
    SimplePolylineVisualiser* poly = new SimplePolylineVisualiser();
    poly->set(node);
    top()->visdef(poly);
}


void XmlMagics::symbol(const XmlNode& node) {
    if (geographical_) {
        SymbolPlotting* symbol = new SymbolPlotting();
        symbol->set(node);
        top()->visdef(symbol);
    }
    else {
        SymbolPlotting* symbol = new SymbolPlotting();
        symbol->set(node);
        top()->visdef(symbol);
    }
}


void XmlMagics::contour(const XmlNode& node) {
    if (geographical_) {
        Contour* contour = new Contour();
        contour->set(node);
        top()->visdef(contour);
    }
    else {
        Contour* contour = new Contour();
        contour->set(node);
        top()->visdef(contour);
    }
}


void XmlMagics::image(const XmlNode& node) {
    MagLog::warning() << " Image has been deprecated! Please use contour." << endl;
}

#include "ImportObjectHandler.h"
void XmlMagics::import(const XmlNode& node) {
    ImportObjectHandler* object = new ImportObjectHandler();
    object->set(node);
    top()->push_back(object);
}
void XmlMagics::wind(const XmlNode& node) {
    if (geographical_) {
        Wind* wind = new Wind();
        wind->set(node);
        top()->visdef(wind);
    }
    else {
        MagLog::warning() << " wind not yet implemented for cartesian system" << endl;
    }
#ifdef HAVE_GRIB
    if (gribloop_)
        gribloop_->next();
#endif
}
