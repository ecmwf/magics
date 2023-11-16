/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <string>
#include <vector>

namespace magics {

class MagicsCalls {
public:
    static const char* knowndrivers();
    static const char* metagrib();
    static const char* metainput();
    static const char* metanetcdf();
    static const char* keep_compatibility();
    static const char* long_parameters();

    // =================================================================
    static void axis();
    static void boxplot();
    static void close();
    static void coast();
    static void cont();
    static void eps();
    static void epsbar();
    static void epscloud();
    static void epsgraph();
    static void epsinput();
    static void epslight();
    static void epsplumes();
    static void epsshading();
    static void epswave();
    static void epswind();
    static void geo();
    static void geojson();
    static void graph();
    static void grib();
    static void image();
    static void import();
    static void input();
    static void legend();
    static void line();
    static void mapgen();
    static void metbufr();
    static void metgraph();
    static void mute();
    static void netcdf();
    static void obs();
    static void odb();
    static void open();
    static void overlay();
    static void plot();
    static void print();
    static void raw();
    static void set_python();
    static void symb();
    static void table();
    static void taylor();
    static void tephi();
    static void text();
    static void tile();
    static void unmute();
    static void wind();
    static void wrepjson();
    static void info();

    static void new_page() { page("page"); }

    static void page(const std::string& page);
    static void reset(const std::string& name);
    static void resets();

    static void setc(const std::string& name, const char* value);
    static void setc(const std::string& name, const std::string& value);
    static void set1c(const std::string& name, const char** data, const int dim1);
    static void set1c(const std::string& name, const std::vector<std::string>& data);

    static void seti(const std::string& name, int value);
    static void setli(const std::string& name, unsigned long long value);
    static void set1i(const std::string& name, const int* data, const int dim1);
    static void set1i(const std::string& name, const std::vector<int>& data);
    static void set2i(const std::string& name, const int* data, const int dim1, const int dim2);
    static void set3i(const std::string& name, const int* data, const int dim1, const int dim2, const int dim3);

    static void setr(const std::string& name, double value);
    static void set1r(const std::string& name, const double* data, const int dim1);
    static void set1r(const std::string& name, const std::vector<double>& data);
    static void set2r(const std::string& name, const double* data, const int dim1, const int dim2);
    static void set2r(const std::string& name, const std::vector<double>& data, const int dim1, const int dim2);
    static void set3r(const std::string& name, const double* data, const int dim1, const int dim2, const int dim3);

    static void enqr(const std::string& name, double* value);
    static void enqi(const std::string& name, int* value);
    static void enqc(const std::string& name, char* value);

    static void add_warning_listener(void* data, void (*cb)(void*, const char*));
    static void add_error_listener(void* data, void (*cb)(void*, const char*));
    static void add_info_listener(void* data, void (*cb)(void*, const char*));
    static void add_debug_listener(void* data, void (*cb)(void*, const char*));
    static void clear_listeners();

    static const char* home();
    static const char* version();

    static const char* detect(const std::string& data, const std::string& dimension);

    static void strict_mode();
};


}  // namespace magics
