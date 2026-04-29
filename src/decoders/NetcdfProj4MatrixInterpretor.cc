/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file NetcdfProj4MatrixInterpretor.cc
    \brief Implementation of a Netcdf interpreter that works on any NetCDF file
           which provides a PROJ.4 definition (global attribute, variable
           attribute, grid‑mapping variable, or a dedicated "proj4" variable).

    Magics Team – extended by ChatGPT (2024)
*/

#include "NetcdfProj4MatrixInterpretor.h"
#include "NetcdfData.h"
#include "MagLog.h"
#include "Proj4Matrix.h"

using namespace magics;

/* ------------------------------------------------------------------
   Helper: try several common places where a proj4 definition can be stored.
   Returns an empty string if nothing is found.
   ------------------------------------------------------------------ */
static std::string findProj4(Netcdf& netcdf, const std::string& field) {
    // 1) Global attribute "proj4"
    std::string proj4 = netcdf.getAttribute("proj4", std::string(""));
    if (!proj4.empty())
        return proj4;

    // 2) Variable attribute "proj4" (some tools attach it to the data variable)
    proj4 = netcdf.getVariableAttribute(field, "proj4", std::string(""));
    if (!proj4.empty())
        return proj4;

    // 3) Grid‑mapping variable – look for the attribute "proj4_params"
    std::string mapping = netcdf.getVariableAttribute(field, "grid_mapping", std::string(""));
    if (!mapping.empty()) {
        proj4 = netcdf.getVariableAttribute(mapping, "proj4_params", std::string(""));
        if (!proj4.empty())
            return proj4;
    }

    // 4) A dedicated variable called "proj4" (some pipelines store the string
    //    as a scalar variable).  We try to read it as a string variable.
    try {
        NetVariable v = netcdf.getVariable("proj4");
        if (v.type() == NC_CHAR || v.type() == NC_STRING) {
            // First try an attribute named "proj4"
            std::string txt;
            v.getAttribute("proj4", txt);
            if (!txt.empty())
                return txt;
            // Otherwise read the scalar value directly
            std::vector<std::string> vec(1);
            v.get(vec);
            if (!vec.empty())
                return vec[0];
        }
    }
    catch (...) {
        // variable does not exist – silently ignore
    }

    return ""; // nothing found
}

/* ------------------------------------------------------------------
   1) static guess – called by NetcdfGuessInterpretor
   ------------------------------------------------------------------ */
NetcdfInterpretor* NetcdfProj4MatrixInterpretor::guess(const NetcdfInterpretor& from) {
    // We need at least a data variable to inspect
    if (from.field_.empty() && (from.x_component_.empty() || from.y_component_.empty()))
        return nullptr;

    Netcdf netcdf(from.path_, from.dimension_method_);

    // Try to locate a proj4 definition.  The helper checks many places.
    std::string proj4 = findProj4(netcdf, from.field_.empty() ? from.x_component_ : from.field_);
    if (proj4.empty())
        return nullptr; // not a proj4‑grid file

    // We have a proj4 definition → create the interpreter.
    NetcdfProj4MatrixInterpretor* p = new NetcdfProj4MatrixInterpretor();
    p->NetcdfInterpretor::copy(from);   // copy generic members (path, field, scaling…)
    p->proj4_ = proj4;

    // Determine which variables contain the X/Y coordinates.  We first try the
    // standard CF latitude/longitude names; if they are missing we fall back to
    // the projection‑coordinate names that many NWP models use.
    std::string lat = netcdf.detect(from.field_, "latitude");
    std::string lon = netcdf.detect(from.field_, "longitude");
    if (!lon.empty() && !lat.empty()) {
        p->xVar_ = lon;
        p->yVar_ = lat;
    } else {
        // projection_x_coordinate / projection_y_coordinate are common in
        // rotated or LCC grids.
        p->xVar_ = netcdf.detect(from.field_, "projection_x_coordinate");
        p->yVar_ = netcdf.detect(from.field_, "projection_y_coordinate");
        if (p->xVar_.empty()) p->xVar_ = "x";
        if (p->yVar_.empty()) p->yVar_ = "y";
    }

    return p;
}

/* ------------------------------------------------------------------
   2) interpretAsMatrix – read the data and build a Proj4Matrix
   ------------------------------------------------------------------ */
bool NetcdfProj4MatrixInterpretor::interpretAsMatrix(Matrix** matrix) {
    if (*matrix) return false; // already allocated elsewhere

    Netcdf netcdf(path_, dimension_method_);

    // Build a Proj4Matrix – this class knows how to turn a proj4 string into
    // proper axis values.
    matrix_.reset(new Proj4Matrix(proj4_));
    *matrix = matrix_.get();

    try {
        // Missing value handling – same as the GeoMatrix interpreter.
        double missing = netcdf.getMissing(field_, missing_attribute_);
        if (std::isnan(missing))
            missing = std::numeric_limits<double>::max();

        // Determine the slice we need (time/level/realisation etc.)
        map<string, string> first, last;
        setDimensions(dimension_, first, last);

        // Load the X and Y coordinate axes.  If the file does not contain
        // dedicated coordinate variables, the call will still work because the
        // dimension names themselves are used as the axis values.
        netcdf.get(xVar_, matrix_->columnsAxis(), first, last);
        netcdf.get(yVar_, matrix_->rowsAxis(),    first, last);

        // Load the actual data field.
        vector<double> data;
        netcdf.get(field_, data, first, last);

        matrix_->missing(missing);
        matrix_->reserve(data.size());
        for (double v : data) {
            if (std::isnan(v) || std::isinf(v))
                matrix_->push_back(missing);
            else
                matrix_->push_back(v);
        }

        // Apply optional scaling / offset.
        matrix_->multiply(scaling_);
        matrix_->plus(offset_);

        // Finalise the axis maps (lon/lat <-> x/y).
        matrix_->setMapsAxis();
    }
    catch (MagicsException& e) {
        if (MagicsGlobal::strict()) throw;
        MagLog::error() << e << std::endl;
        matrix_.reset(nullptr);
        return false;
    }

    return true;
}

/* ------------------------------------------------------------------
   3) Optional meta‑data – makes debugging easier
   ------------------------------------------------------------------ */
void NetcdfProj4MatrixInterpretor::visit(MetaDataCollector& mdc) {
    NetcdfInterpretor::visit(mdc);
    mdc["proj4"]        = proj4_;
    mdc["x_coordinate"] = xVar_;
    mdc["y_coordinate"] = yVar_;
}

// Optional factory registration – allows users to force this interpreter via
// <netcdf interpreter="proj4" …>.  If you do not need that feature you can
// comment the line out.
static SimpleObjectMaker<NetcdfProj4MatrixInterpretor,
                         NetcdfInterpretor> netcdf_proj4_interpretor("proj4");
