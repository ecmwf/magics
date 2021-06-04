/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

//! \file Netcdf.cc
/*!
 Sylvie Lamy-Thepaut - ECMWF Apr 02

 Changes:

   Apr 06: update for GCC 4.0 (Stephan)
*/
#include <MagConfig.h>
#include <MagException.h>
#include <MagLog.h>
#include <NetcdfData.h>
#include <algorithm>


using namespace magics;


static bool isVariable(int netcdf, int var) {
    int dims;
    nc_inq_varndims(netcdf, var, &dims);
    if (dims != 1)
        return true;
    return false;
}

template <class From, class To>
Convertor<From, To>::Convertor(NetVariable& var) : variable_(var) {
    To scale(1);
    To offset(0);
    scale_factor_ = variable_.getAttribute("scale_factor", scale);
    add_offset_   = variable_.getAttribute("add_offset", offset);
    missing_      = variable_.getMissing();
}

template <class F, class T>
void TypedAccessor<F, T>::operator()(vector<T>& to, vector<size_t>& start, vector<size_t>& edges,
                                     NetVariable& var) const {
    vector<F> from(to.size());
    var.get(from, start, edges);

    // Convert the data....
    std::transform(from.begin(), from.begin() + to.size(), to.begin(), Convertor<F, T>(var));
    // for (auto x = start.begin(); x != start.end(); ++x) {   cout << "start " << *x << endl; }
    // for (auto x = edges.begin(); x != edges.end(); ++x) {   cout << "edges " << *x << endl; }
    
}

template <class F, class T>
void TypedAccessor<F, T>::get(vector<F>& from, vector<size_t>& start, vector<size_t>& edges, NetVariable& var) const {
    var.get(&from.front(), start, edges);
    
   
}

Netcdf::Netcdf(const string& path, const string& method) : file_(-1) {
    int status = nc_open(path.c_str(), NC_NOWRITE, &file_);

    if (status != NC_NOERR) {
        fprintf(stderr, "ERROR while opening NetCDF file - %s\n", nc_strerror(status));
        throw NoSuchNetcdfFile(path);
    }

    int num_var;
    int var_ids[NC_MAX_VAR_DIMS];
    nc_inq_varids(file_, &num_var, var_ids);

    for (int v = 0; v < num_var; v++) {
        // get the name
        char tmp[NC_MAX_NAME + 1];
        int id = var_ids[v];
        nc_inq_varname(file_, id, tmp);

        string name(tmp);

        variables_.insert(std::make_pair(name, NetVariable(name, v, this, method)));
        if (isVariable(file_, var_ids[v]))
            dataset_.insert(std::make_pair(name, NetVariable(name, var_ids[v], this, method)));
    }

    MagLog::debug() << "Initialisation of Netcdf [" << path << "] OK! "
                    << "\n";

    int num_atts;
    nc_inq_varnatts(file_, NC_GLOBAL, &num_atts);
    for (int v = 0; v < num_atts; v++) {
        char tmp[NC_MAX_NAME + 1];
        nc_inq_attname(file_, NC_GLOBAL, v, tmp);
        string name(tmp);
        attributes_.insert(std::make_pair(name, NetAttribute(name, file_, NC_GLOBAL)));
    }

    int num_dims;
    nc_inq_ndims(file_, &num_dims);
    for (int d = 0; d < num_dims; d++) {
        char tmp[NC_MAX_NAME + 1];
        nc_inq_dimname(file_, d, tmp);
        string name(tmp);
        dimensions_.insert(std::make_pair(name, NetDimension(this, name)));
    }
}


Netcdf::~Netcdf() {
    if (file_ > -1) {
        int status = nc_close(file_);
        if (status != NC_NOERR) {
            fprintf(stderr, "ERROR while closing NetCDF file - %s\n", nc_strerror(status));
        }
    }
}

double Netcdf::getMissing(const string& var, const string& attr) {
    missing_ = getAttribute(attr, getDefaultMissing(var));
    missing_ = getVariableAttribute(var, attr, missing_);
    return missing_;
}

void Netcdf::print(ostream& out) const {
    out << "print Netcdf: "
        << "\n";
    out << "Variables: "
        << "\n";
    for (map<string, NetVariable>::const_iterator var = variables_.begin(); var != variables_.end(); ++var) {
        out << (*var).second;
    }
    out << "Dataset: "
        << "\n";
    for (map<string, NetVariable>::const_iterator var = dataset_.begin(); var != dataset_.end(); ++var) {
        out << (*var).second;
    }
}


NetDimension::NetDimension(Netcdf* netcdf, const string& name, int index, int variable) :
    parent_(netcdf), name_(name), first_(0), index_(index), variable_(variable) {
    netcdf_ = parent_->file();
    nc_inq_dimid(netcdf_, name_.c_str(), &id_);
    nc_inq_dimlen(netcdf_, id_, &size_);
    dim_ = size_;
}


int NetDimension::index(const string& val) {
    int index = atoi(val.c_str());
    return index;
}

int NetDimension::value(const string& val) {
    
    if (variable_ != -1) {
        // int index = Index::get(variable_->type(), val, variable_->values(), variable_->num_vals());
        NetVariable var(name_, variable_, parent_, "index");
    
        return var.find(val);
    }

    // we assume the user is using index! ..
    int index = atoi(val.c_str());
    MagLog::warning() << " Could not find variable return index instead " << index << endl;
    return index;
}

void NetDimension::first(const string& val) {
    first_ = (magCompare(method_, "value")) ? value(val) : index(val);
}


void NetDimension::last(const string& val) {
    int last = (magCompare(method_, "value")) ? value(val) : index(val);
    if (last < first_) {
        MagLog::warning() << "last position (" + val + ") < first position: exchange "
                          << "\n";
        int tmp = first_;
        first_  = last;
        last    = tmp;
    }
    dim_ = last - first_ + 1;
}


NetVariable::NetVariable(const string& name, int id, Netcdf* parent, const string& method) :
    name_(name), id_(id), parent_(parent), missing_(std::numeric_limits<double>::max()) {
    netcdf_ = parent_->file();
    int num_dims;
    nc_inq_varndims(netcdf_, id_, &num_dims);
    int* dims = new int[num_dims];
    nc_inq_vardimid(netcdf_, id_, dims);

    for (int d = 0; d < num_dims; d++) {
        char tmp[NC_MAX_NAME + 1];
        nc_inq_dimname(netcdf_, dims[d], tmp);
        string name(tmp);

        int var = -1;
        // Try to find if a variable is defined with this name.
        int num_var;
        int var_ids[NC_MAX_VAR_DIMS];
        nc_inq_varids(netcdf_, &num_var, var_ids);

        for (int v = 0; v < num_var; v++) {
            // get the name
            char tmp[NC_MAX_NAME];
            int id = var_ids[v];
            nc_inq_varname(netcdf_, id, tmp);
            string current(tmp);
            if (current == name) {
                var = id;
                break;
            }
        }
        dimensions_.insert(std::make_pair(name, NetDimension(parent_, name, d, var)));
        dimensions_[name].method_ = method;
    }

    int num_atts;
    nc_inq_varnatts(netcdf_, id_, &num_atts);
    for (int v = 0; v < num_atts; v++) {
        char tmp[NC_MAX_NAME];
        nc_inq_attname(netcdf_, id_, v, tmp);
        string name(tmp);
        attributes_.insert(std::make_pair(name, NetAttribute(name, netcdf_, id_)));
    }
    missing_ = getDefaultMissing();
    delete[] dims;
}

template <class T>
int find(const T& value, vector<T>& values) {
    if (values.size() == 1 && values[0] == value)
        return 0;
    for (int i = 0; i < values.size() - 1; i++) {
        if (values[i] == value)
            return i;
        if (values[i + 1] == value)
            return i + 1;
        if (values[i] < value && value < values[i + 1])
            return i;
        if (values[i + 1] < value && value < values[i])
            return i + 1;
    }
    return -1;
}

#include "MagDateTime.h"
#include "Tokenizer.h"
string NetVariable::interpretTime(const string& val) {
    string time = parent_->detect(name_, "time");
    if (time.empty())
        return val;

    // try to convert the time !
    static map<string, long> factors;
    if (factors.empty()) {
        factors["hours"] = 3600;
        factors["days"]  = 24 * 3600;
    }
    string units = getAttribute("units", string(""));
    if (units.empty())
        return val;

    // Now we parse the string !
    vector<string> tokens;
    Tokenizer tokenizer(" ");
    tokenizer(units, tokens);
    string basedate = tokens[2];
    string unit     = tokens[0];

    try {
        DateTime user      = DateTime(val);
        DateTime reference = DateTime(basedate);
        long diff          = user - reference;
        DateTime x         = reference + diff;

        map<string, long>::iterator f = factors.find(unit);
        long factor                   = (f != factors.end()) ? f->second : 1;
        diff                          = diff / factor;

        return tostring(diff);
    }
    catch (...) {
        return val;
    }
}

int NetVariable::find(const string& value) {
    // First , is the variable a time variable:

    string val = value;

    try {
        val = interpretTime(value);
    }
    catch (...) {
        val = value;
    }


    nc_type t = type();
    if (t == NC_DOUBLE) {
        vector<double> values;
        values.resize(getSize());
        get(values);
        double dval = tonumber(val);
        return ::find(dval, values);
    }
    if (t == NC_INT) {
        vector<int> values;
        values.resize(getSize());
        get(values);
        int dval = tonumber(val);

        return ::find(dval, values);
    }
    if (t == NC_INT64) {
        vector<int> values;
        values.resize(getSize());
        get(values);
        int dval = tonumber(val);

        return ::find(dval, values);
    }
    if (t == NC_FLOAT) {
        vector<float> values;
        values.resize(getSize());
        getValues(values);
        float dval = tonumber(val);
        return ::find(dval, values);
    }
    if (t == NC_SHORT) {
        vector<short> values;
        values.resize(getSize());
        get(values);
        short dval = tonumber(val);
        return ::find(dval, values);
    }
    if (t == NC_UBYTE) {
        vector<short> values;
        values.resize(getSize());
        get(values);
        short dval = tonumber(val);
        return ::find(dval, values);
    }
    if (t == NC_CHAR) {
        vector<size_t> dims;
        getDimensions(dims);
        for ( int i = 0; i < dims[0]; i++ ) {
            char text[256];
            nc_get_var_text(netcdf_, id_, text);
            if (string(text) ==  val)
                return i;

        }
    }

    if (t == NC_STRING) {
        int x = getSize();
        char* values[1024];
        ASSERT(x < sizeof(values));
        nc_get_var_string(netcdf_, id_, values);
        for (int i = 0; i < x; i++) {
            if (string(values[i]) == val)
                return i;
        }

    }

    return 0;
}


double NetVariable::getDefaultMissing() {
    int t = type();
    switch(t) {
        case NC_CHAR: return NC_FILL_CHAR; break;
        case NC_BYTE: return NC_FILL_BYTE; break;
        case NC_UBYTE: return NC_FILL_UBYTE; break;
        case NC_INT: return NC_FILL_INT; break;
        case NC_UINT: return NC_FILL_UINT; break;
        case NC_SHORT: return NC_FILL_SHORT; break;
        case NC_USHORT: return NC_FILL_USHORT; break;
        case NC_INT64: return NC_FILL_INT64; break;
        case NC_UINT64: return NC_FILL_UINT64; break;
        case NC_FLOAT: return NC_FILL_FLOAT; break;
        case NC_DOUBLE: return NC_FILL_DOUBLE; break;
    }

    MagLog::warning() << "NetVariable: No default missing value defined for " << nc_type_to_name(t) << std::endl;
    return NC_FILL_FLOAT;

}


string Netcdf::detect(const string& var, const string& type) const {
    NetVariable variable      = getVariable(var);
    vector<string> dimensions = variable.dimensions();

    NetcdfGuess guesser;

    map<string, map<string, vector<string> > >::iterator checks = guesser.guess_.find(type);
    if (checks == guesser.guess_.end())
        return "";

    for (map<string, vector<string> >::iterator check = checks->second.begin(); check != checks->second.end();
         ++check) {
        vector<string> values = check->second;
        for (vector<string>::iterator dim = dimensions.begin(); dim != dimensions.end(); ++dim) {
            string value = getVariable(*dim).getAttribute(check->first, string(""));

            for (vector<string>::iterator v = values.begin(); v != values.end(); ++v) {
                string val = value.substr(0, v->size());

                if (v->compare(val) == 0) {
                    return *dim;
                }
            }
        }
    }
    return "";
}


namespace magics {
template <>
map<nc_type, Accessor<double>*>* Accessor<double>::accessors_ = 0;
template <>
map<nc_type, Accessor<float>*>* Accessor<float>::accessors_ = 0;
template <>
map<nc_type, Accessor<int>*>* Accessor<int>::accessors_ = 0;
}  // namespace magics


static TypedAccessor<signed char, float> u_byte_float_accessor(NC_UBYTE);
static TypedAccessor<signed char, float> byte_float_accessor(NC_BYTE);
static TypedAccessor<short, float> short_float_accessor(NC_SHORT);
static TypedAccessor<unsigned short, float> u_short_float_accessor(NC_USHORT);
static TypedAccessor<int, float> int_float_accessor(NC_INT);
static TypedAccessor<long, double> long_double_accessor(NC_INT64);
static TypedAccessor<float, float> float_float_accessor(NC_FLOAT);
static TypedAccessor<double, float> double_float_accessor(NC_FLOAT);

static TypedAccessor<signed char, double> u_byte_double_accessor(NC_UBYTE);
static TypedAccessor<signed char, double> byte_double_accessor(NC_BYTE);
static TypedAccessor<short, double> u_short_double_accessor(NC_SHORT);
static TypedAccessor<unsigned short, double> short_double_accessor(NC_USHORT);
static TypedAccessor<int, double> int_double_accessor(NC_INT);
static TypedAccessor<float, double> float_double_accessor(NC_FLOAT);
static TypedAccessor<double, double> double_double_accessor(NC_DOUBLE);

namespace magics {

const char* nc_type_to_name(int n) {

    switch (n) {
        case NC_CHAR: return "NC_CHAR"; break;
        case NC_BYTE: return "NC_BYTE"; break;
        case NC_UBYTE: return "NC_UBYTE"; break;
        case NC_INT: return "NC_INT"; break;
        case NC_UINT: return "NC_UINT"; break;
        case NC_SHORT: return "NC_SHORT"; break;
        case NC_USHORT: return "NC_USHORT"; break;
        case NC_INT64: return "NC_INT64"; break;
        case NC_UINT64: return "NC_UINT64"; break;
        case NC_FLOAT: return "NC_FLOAT"; break;
        case NC_DOUBLE: return "NC_DOUBLE"; break;
        case NC_STRING: return "NC_STRING"; break;
    }

    static char unknown[20];
    sprintf(unknown, "UNKNOWN(%d)", n);
    return unknown;
}
}
