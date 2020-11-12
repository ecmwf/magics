/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsCalls.cc
    \brief Implementation of Fortran and C interface

 To use the C interface "magics_api.h" must be included.

 Changes: 13-06-2006 Added C interface (Stephan)

 \sa magics_api.h
*/

#include "MagicsCalls.h"
#include "CompatibilityHelper.h"
#include "FortranMagics.h"
#include "MagicsSettings.h"


namespace magics {


const char* MagicsCalls::knowndrivers() {
    return FortranMagics::instance().knownDrivers();
}

const char* MagicsCalls::metagrib() {
    return FortranMagics::instance().metagrib();
}

const char* MagicsCalls::metainput() {
    return FortranMagics::instance().metainput();
}

const char* MagicsCalls::metanetcdf() {
    return FortranMagics::instance().metanetcdf();
}

const char* MagicsCalls::keep_compatibility() {
    NOTIMP;
}

// =================================================================
void MagicsCalls::axis() {
    FortranMagics::instance().paxis();
}

void MagicsCalls::boxplot() {
    FortranMagics::instance().pboxplot();
}

void MagicsCalls::close() {
    FortranMagics::instance().pclose();
}

void MagicsCalls::coast() {
    FortranMagics::instance().pcoast();
}

void MagicsCalls::cont() {
    FortranMagics::instance().pcont();
}

void MagicsCalls::eps() {
    NOTIMP;
}

void MagicsCalls::epsbar() {
    FortranMagics::instance().epsbar();
}

void MagicsCalls::epscloud() {
    FortranMagics::instance().epscloud();
}

void MagicsCalls::epsgraph() {
    FortranMagics::instance().epsgraph();
}

void MagicsCalls::epsinput() {
    FortranMagics::instance().epsinput();
}

void MagicsCalls::epslight() {
    FortranMagics::instance().epslight();
}

void MagicsCalls::epsplumes() {
    FortranMagics::instance().epsplumes();
}

void MagicsCalls::epsshading() {
    FortranMagics::instance().epsshading();
}

void MagicsCalls::epswave() {
    FortranMagics::instance().epswave();
}

void MagicsCalls::epswind() {
    FortranMagics::instance().epswind();
}

void MagicsCalls::geo() {
    FortranMagics::instance().pgeo();
}

void MagicsCalls::geojson() {
    NOTIMP;
}

void MagicsCalls::graph() {
    FortranMagics::instance().pgraph();
}

void MagicsCalls::grib() {
    FortranMagics::instance().pgrib();
}

void MagicsCalls::image() {
    FortranMagics::instance().pimage();
}

void MagicsCalls::import() {
    FortranMagics::instance().pimport();
}

void MagicsCalls::info() {
    MagLog::userInfo() << "INFO:\n"
                       << "INFO: " << getMagicsVersionString() << "\n"
                       << "INFO:\n"
                       << "INFO: Machine: " << getEnvVariable("HOSTNAME") << " is running " << getEnvVariable("VENDOR")
                       << " " << getEnvVariable("OSTYPE") << " " << getEnvVariable("MACHTYPE") << "\n"
                       << "INFO:\n"
                       << "INFO: $MAGPLUS_HOME    = " << getEnvVariable("MAGPLUS_HOME") << "\n"
                       << "INFO: $TMPDIR          = " << getEnvVariable("TMPDIR") << "\n"
                       << "INFO: $ODB_LIBS        = " << getEnvVariable("ODB_LIBS") << "\n"
                       << "INFO: $LD_LIBRARY_PATH = " << getEnvVariable("LD_LIBRARY_PATH") << "\n"
                       << "INFO:\n";
}

void MagicsCalls::input() {
    FortranMagics::instance().pinput();
}

void MagicsCalls::legend() {
    // FIXME: why?
    // FortranMagics::instance().plegend();
    FortranMagics::instance().simplelegend();
}

void MagicsCalls::line() {
    FortranMagics::instance().pline();
}

void MagicsCalls::mapgen() {
    FortranMagics::instance().pmapgen();
}

void MagicsCalls::metbufr() {
    FortranMagics::instance().metbufr();
}

void MagicsCalls::metgraph() {
    FortranMagics::instance().metgraph();
}

void MagicsCalls::mute() {
    MagicsSettings::silent(true);
}

void MagicsCalls::netcdf() {
    FortranMagics::instance().pnetcdf();
}

void MagicsCalls::obs() {
    FortranMagics::instance().pobs();
}

void MagicsCalls::odb() {
#ifdef HAVE_ODB
    FortranMagics::instance().podb();
#else
    if (MagicsSettings::strict()) {
        throw NotSupported("ODB support is NOT enabled!");
    }
    MagLog::warning() << "ODB support is NOT enabled!\n";
#endif
}

void MagicsCalls::open() {
    FortranMagics::instance().popen();
}

void MagicsCalls::overlay() {
    FortranMagics::instance().poverlay();
}

void MagicsCalls::plot() {
    NOTIMP;
}

void MagicsCalls::print() {
    NOTIMP;
}

void MagicsCalls::raw() {
    NOTIMP;
}

void MagicsCalls::set_python() {
    strict(true);
}

void MagicsCalls::symb() {
    FortranMagics::instance().psymb();
}

void MagicsCalls::table() {
    FortranMagics::instance().ptable();
}

void MagicsCalls::taylor() {
    FortranMagics::instance().ptaylor();
}

void MagicsCalls::tephi() {
    FortranMagics::instance().ptephi();
}

void MagicsCalls::text() {
    FortranMagics::instance().ptext();
}

void MagicsCalls::tile() {
    FortranMagics::instance().ptile();
}

void MagicsCalls::unmute() {
    MagicsSettings::silent(false);
}

void MagicsCalls::wind() {
    FortranMagics::instance().pwind();
}

void MagicsCalls::wrepjson() {
    FortranMagics::instance().wrepjson();
}

void MagicsCalls::page(const std::string& page) {
    FortranMagics::instance().pnew(page);
}

void MagicsCalls::reset(const std::string& name) {
    CompatibilityHelper::reset(name);
    ParameterManager::reset(name);
}

void MagicsCalls::resets() {
    ParameterManager::reset();
}

//=================================

void MagicsCalls::setc(const std::string& name, const std::string& value) {
    if (CompatibilityHelper::check(name, value))
        return;
    ParameterManager::set(name, value);
}

void MagicsCalls::setc(const std::string& name, const char* value) {
    ASSERT(value);
    setc(name, std::string(value));
}

void MagicsCalls::set1c(const std::string& name, const char** data, const int dim) {
    ASSERT(data);

    //	MagLog::dev() << "entry in the new mag_set1c\n";
    //	MagLog::dev() << "\tmag_set1c("  << dim << " entries);\n";
    stringarray values;

    for (int i = 0; i < dim; i++) {
        ASSERT(data[i]);
        values.push_back(data[i]);
    }

    set1c(name, values);
}

//=================================


void MagicsCalls::setr(const std::string& name, double value) {
    if (CompatibilityHelper::check(name, value))
        return;
    ParameterManager::set(name, value);
}


void MagicsCalls::set1r(const std::string& name, const std::vector<double>& data) {
    set1r(name, data.data(), data.size());
}

void MagicsCalls::set1r(const std::string& name, const double* data, const int dim1) {
    ASSERT(data);

    floatarray values;
    for (int i = 0; i < dim1; i++) {
        values.push_back(data[i]);
    }

    if (CompatibilityHelper::check(name, values))
        return;

    ParameterManager::set(name, values);
}

void MagicsCalls::set2r(const std::string& name, const double* data, const int dim1, const int dim2) {
    ASSERT(data);

    Matrix matrix;
    for (int i = 0; i < dim2 * dim1; i++) {
        matrix.push_back(data[i]);
    }

    matrix.set(dim2, dim1);

    if (CompatibilityHelper::check(name, matrix))
        return;

    ParameterManager::set(name, matrix);

    MagLog::dev() << "Parameter " << name << " set to " << matrix << "\n";
}

void MagicsCalls::set2r(const std::string& name, const std::vector<double>& data, const int dim1, const int dim2) {
    ASSERT(data.size() == dim2 * dim1);

    Matrix matrix;
    for (int i = 0; i < dim2 * dim1; i++) {
        matrix.push_back(data[i]);
    }

    matrix.set(dim2, dim1);

    if (CompatibilityHelper::check(name, matrix))
        return;

    ParameterManager::set(name, matrix);

    MagLog::dev() << "Parameter " << name << " set to " << matrix << "\n";
}

void MagicsCalls::set3r(const std::string& name, const double* data, const int dim1, const int dim2, const int dim3) {
    NOTIMP;
}

//=================================


void MagicsCalls::seti(const std::string& name, int value) {
    if (CompatibilityHelper::check(name, value))
        return;
    ParameterManager::set(name, value);
}


void MagicsCalls::set1i(const std::string& name, const std::vector<int>& data) {
    set1i(name, data.data(), data.size());
}

void MagicsCalls::set1i(const std::string& name, const int* data, const int dim1) {
    ASSERT(data);

    intarray values;
    for (int i = 0; i < dim1; i++) {
        values.push_back(data[i]);
    }

    if (CompatibilityHelper::check(name, values))
        return;

    ParameterManager::set(name, values);
}

void MagicsCalls::set2i(const std::string& name, const int* data, const int dim1, const int dim2) {
    ASSERT(data);

    Matrix matrix;
    for (int i = 0; i < dim2 * dim1; i++) {
        matrix.push_back(data[i]);
    }

    matrix.set(dim2, dim1);

    if (CompatibilityHelper::check(name, matrix))
        return;

    ParameterManager::set(name, matrix);

    MagLog::dev() << "Parameter " << name << " set to " << matrix << "\n";
}

void MagicsCalls::set3i(const std::string& name, const int* data, const int dim1, const int dim2, const int dim3) {
    NOTIMP;
}

//=================================


void MagicsCalls::enqr(const std::string& n, double* value) {
    std::string name = n;

    ASSERT(value);

    vector<string> special;
    special.push_back("subpage_x_position");
    special.push_back("subpage_y_position");
    special.push_back("subpage_x_length");
    special.push_back("subpage_y_length");
    // parameters needs magics to get reday!

    string projection;

    ParameterManager::get("subpage_map_projection", projection);

    for (vector<string>::iterator param = special.begin(); param != special.end(); ++param) {
        if (magCompare(name, *param)) {
            double val;
            ParameterManager::get(name, val);
            if (!magCompare(projection, "cartesian")) {
                FortranMagics::instance().prepare();
                name = name + "_internal";
            }
        }
    }
    double magics;
    ParameterManager::get(name, magics);
    *value = magics;
    MagLog::dev() << "mag_enqr->" << name << " = " << magics << endl;
}

void MagicsCalls::enqi(const std::string& name, int* value) {
    ASSERT(value);

    int magics;
    ParameterManager::get(name, magics);
    *value = magics;
}

void MagicsCalls::enqc(const std::string& name, char* value) {
    ASSERT(value);

    string magics;

    if (magCompare(name, "magics_version")) {
        magics = getMagicsVersionString();
    }
    else
        ParameterManager::get(name, magics);

    strcpy(value, magics.c_str());
}


void MagicsCalls::set1c(const std::string& name, const std::vector<std::string>& values) {
    if (CompatibilityHelper::check(name, values))
        return;

    ParameterManager::set(name, values);
}

void MagicsCalls::add_warning_listener(void* data, void (*cb)(void*, const char*)) {
    MagLog::addWarningListener(data, cb);
}
void MagicsCalls::add_error_listener(void* data, void (*cb)(void*, const char*)) {
    MagLog::addErrorListener(data, cb);
}
void MagicsCalls::add_info_listener(void* data, void (*cb)(void*, const char*)) {
    MagLog::addInfoListener(data, cb);
}

void MagicsCalls::add_debug_listener(void* data, void (*cb)(void*, const char*)) {
    MagLog::addDebugListener(data, cb);
}

void MagicsCalls::clear_listeners() {
    MagLog::clearListeners();
}

const char* MagicsCalls::version() {
    static string version = getMagicsVersionString();
    return version.c_str();
}

const char* MagicsCalls::home() {
    static string home = getEnvVariable("MAGPLUS_HOME");
    return home.c_str();
}

const char* MagicsCalls::detect(const std::string& data, const std::string& dimension) {
    return FortranMagics::instance().detect(data, dimension);
}


void MagicsCalls::strict(bool on) {
    // TODO: Come back
    MagicsSettings::compatibility(!on);
    MagicsSettings::strict(on);
}

}  // namespace magics
