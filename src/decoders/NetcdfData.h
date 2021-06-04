/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Netcdf.h
    \brief Definition of the Netcdf access tools.

    Magics Team - ECMWF 2004

    Started: Fri 16-Jan-2004

    Changes:

*/

#ifndef Netcdf_H
#define Netcdf_H

#include "magics.h"
#include "netcdf.h"

#include "MagException.h"
#include "MagLog.h"

namespace magics {

class NoSuchNetcdfVariable : public MagicsException {
public:
    NoSuchNetcdfVariable(const string& var) :
        MagicsException("Netcdf MagException:  Can not find variable ---> " + var) {
        MagLog::warning() << what() << "\n";
    }
};
class NoSuchNetcdfDimension : public MagicsException {
public:
    NoSuchNetcdfDimension(const string& dim) :
        MagicsException("Netcdf MagException :  Can not find dimension ---> " + dim) {
        MagLog::warning() << what() << "\n";
    }
};

class NoSuchNetcdfFile : public MagicsException {
public:
    NoSuchNetcdfFile(const string& file) :
        MagicsException("Netcdf MagException: The file " + file + " does not exist or is not a valid netcdf file") {
        MagLog::error() << what() << "\n";
    }
};
class Netcdf;
struct NetDimension {
    string name_;
    size_t size_;
    size_t first_;
    size_t dim_;
    size_t index_;
    string method_;


    int id_;
    int variable_;

    Netcdf* parent_;
    int netcdf_;



    NetDimension() {}
    NetDimension(Netcdf* netcdf, const string& name, int index = 0, int variable = -1);


    void first(const string&);
    void last(const string&);

    int index(const string&);
    int value(const string&);

    void print(ostream& s) const {
        s << name_ << "(" << size_ << ", " << index_ << ", " << first_ << ", " << dim_ << ")";
    }
    friend ostream& operator<<(ostream& s, const NetDimension& p) {
        p.print(s);
        return s;
    }
};


struct NetAttribute {
    string name_;
    int id_;
    int netcdf_;
    NetAttribute(const string name, int netcdf, int id) : name_(name), netcdf_(netcdf), id_(id) {}
    NetAttribute() {}
    void get(double& val) {
        double tmp;
        nc_get_att_double(netcdf_, id_, name_.c_str(), &tmp);
        val = tmp;
    }
    void get(float& val) {
        float tmp;
        nc_get_att_float(netcdf_, id_, name_.c_str(), &tmp);
    }
    void get(string& val) {
        size_t len;
        nc_inq_attlen(netcdf_, id_, name_.c_str(), &len);

        char* tmp = new char[len];
        nc_get_att_text(netcdf_, id_, name_.c_str(), tmp);
        val = string(tmp, len);
        delete[] tmp;
    }
    void get(char*& val) {
        size_t len;
        nc_inq_attlen(netcdf_, id_, name_.c_str(), &len);
        nc_get_att_text(netcdf_, id_, name_.c_str(), (char*)val);
    }
};

struct NetVariable;


template <class From, class To>
struct Convertor {
    Convertor(NetVariable&);
    To operator()(From from) { return (from != missing_) ? from * scale_factor_ + add_offset_ : missing_; }

    NetVariable& variable_;
    To scale_factor_;
    To add_offset_;
    From missing_;
};


template <class T>
class Accessor {
public:
    Accessor(nc_type type) {
        if (!accessors_)
            accessors_ = new map<nc_type, Accessor<T>*>;
        accessors_->insert(std::make_pair(type, this));
    }
    virtual ~Accessor() {}

    virtual void operator()(vector<T>&, vector<size_t>&, vector<size_t>&, NetVariable&) const {}

    static map<nc_type, Accessor<T>*>* accessors_;
    static void release() {
        if (accessors_)
            for (typename map<nc_type, Accessor<T>*>::iterator a = accessors_->begin(); a != accessors_->end(); ++a) {
                Accessor<T>* accessor = a->second;
                a->second             = 0;
                delete accessor;
            }
    }

    static void access(vector<T>& data, vector<size_t>& start, vector<size_t>& edges, NetVariable& var);
};

template <class F, class T>
class TypedAccessor : public Accessor<T> {
public:
    TypedAccessor(nc_type type) : Accessor<T>(type) {}

    void operator()(vector<T>& to, vector<size_t>& start, vector<size_t>& edges, NetVariable& var) const;
    void get(vector<F>& from, vector<size_t>& start, vector<size_t>& edges, NetVariable& var) const;
};


class Netcdf;

struct NetVariable {
    string name_;
    int id_;

    Netcdf* parent_;
    int netcdf_;
    map<string, NetDimension> dimensions_;
    map<string, NetAttribute> attributes_;
    double missing_;

    NetVariable(const string& name, int id, Netcdf* parent, const string& method);

    void getStartingPoint(vector<size_t>& dims) {
        dims.resize(dimensions_.size());
        for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim) {
            dims[(*dim).second.index_] = (*dim).second.first_;
        }
    }

    void getDimensions(vector<size_t>& dims) {
        dims.resize(dimensions_.size());
        for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim) {
            dims[(*dim).second.index_] = (*dim).second.dim_;
        }
    }

    size_t getSize() {
        vector<size_t> dims;
        getDimensions(dims);
        return getSize(dims);
    }

    void setFirstPoint(const string& name, const string& first) {

        map<string, NetDimension>::iterator dim = dimensions_.find(name);
        if (dim == dimensions_.end())
            return;
         (*dim).second.first(first);


    }

    void setLastPoint(const string& name, const string& last) {

        map<string, NetDimension>::iterator d = dimensions_.find(name);
        if (d == dimensions_.end())
            return;
        (*d).second.last(last);

    }

    size_t getSize(const vector<size_t>& dims) {
        size_t size = 1;
        for (unsigned int i = 0; i < dims.size(); i++) {
            size = (dims[i]) * size;
        }

        return size;
    }
    string interpretTime(const string&);

    void get(vector<double>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_double(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }

    void get(vector<float>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_float(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }
    void get(vector<long>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_long(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }

    void get(vector<int>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_int(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }
    void get(vector<short>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_short(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }
    void get(vector<unsigned short>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_ushort(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }
    void get(vector<signed char>& data, vector<size_t>& start, vector<size_t>& edges) {
        nc_get_vara_schar(netcdf_, id_, &start.front(), &edges.front(), &data.front());
    }
    void get(vector<double>& data) { nc_get_var_double(netcdf_, id_, &data.front()); }

    void get(vector<float>& data) { nc_get_var_float(netcdf_, id_, &data.front()); }

    void get(vector<int>& data) { nc_get_var_int(netcdf_, id_, &data.front()); }
    void get(vector<short>& data) { nc_get_var_short(netcdf_, id_, &data.front()); }
    void get(vector<signed char>& data) { nc_get_var_schar(netcdf_, id_, &data.front()); }



    void print(ostream& s) const {
        s << name_ << "[";
        string sep = "";
        for (map<string, NetDimension>::const_iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim) {
            s << sep << (*dim).second;
            sep = ", ";
        }

        s << "]"
          << "\n";
    }

    nc_type type() {
        nc_type type;
        nc_inq_vartype(netcdf_, id_, &type);
        return type;
    }

    int find(const string& value);

    double getMissing() { return missing_; }

    template <class T>
    T getAttribute(const string& name, T def) {
        T val = def;
        map<string, NetAttribute>::iterator attr = attributes_.find(name);
        if (attr == attributes_.end())
            return def;
        (*attr).second.get(val);
        return val;
    }
    string getAttribute(const string& name, const char* def) { return getAttribute(name, string(def)); }
    string getAttribute(const string& name, const string& def) {
        map<string, NetAttribute>::iterator attr = attributes_.find(name);
        if (attr == attributes_.end())
            return def;
        string val;
        (*attr).second.get(val);
        return val;
    }

    double getDefaultMissing();
    double getMissing(const string&);

    template <class T>
    void get(vector<T>& vals, map<string, string> first, map<string, string> last) {
        for (map<string, string>::const_iterator f = first.begin(); f != first.end(); ++f) {
            setFirstPoint((*f).first, (*f).second);
        }
        for (map<string, string>::const_iterator f = last.begin(); f != last.end(); ++f) {
            setLastPoint((*f).first, (*f).second);
        }
        getValues(vals);
    }

    template <class T>
    void getValues(vector<T>& vals) {
        vector<size_t> start;
        getStartingPoint(start);
        vector<size_t> end;
        getDimensions(end);

        vals.resize(getSize(end));
        Accessor<T>::access(vals, start, end, *this);
    }

    vector<string> dimensions() {
        vector<string> dims;
        for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim)
            dims.push_back(dim->first);
        return dims;
    }

    void default2D() {
        int nb = dimensions_.size();
        for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim)
            if ( dim->second.index_ < nb-2)
                dim->second.dim_ = 1;


        auto dim = dimensions_.begin();
        for (int i = 0; i < nb-2; ++i) {
            dim->second.dim_ = 1;
            dim++;
        }

    }
    void default1D() {
         int nb = dimensions_.size();
        for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim)
            if ( dim->second.index_ < nb-1)
                dim->second.dim_ = 1;

    }


    friend ostream& operator<<(ostream& s, const NetVariable& p) {
        p.print(s);
        return s;
    }
};


class Netcdf {
public:
    Netcdf(const string&, const string&);

    virtual ~Netcdf();

    typedef map<string, NetVariable> VariableMap;

    double getDefaultMissing(const string& name) {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        return var->second.getDefaultMissing();
    }

    double getMissing(const string&, const string&);

    string detect(const string& var, const string& type) const;


    template <class T>
    void get(const string& name, vector<T>& vals, map<string, string> first, map<string, string> last) {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        (*var).second.missing_ = missing_;
        (*var).second.get(vals, first, last);
    }
    int file() const { return file_; }

    template <class T>
    void get(const string& name, vector<T>& vals) {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        (*var).second.get(vals);
    }

    int getDimension(const string& name) {

        map<string, NetDimension>::iterator dim = dimensions_.find(name);
        if (dim == dimensions_.end()) {
            MagLog::error() << name << " : do not find such dimension\n" << endl;

            throw NoSuchNetcdfDimension(name);
        }
        return dim->second.size_;
    }

    template <class T>
    T getVariableAttribute(const string& name, const string& attr, T def) {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        return (*var).second.getAttribute(attr, def);
    }


    string getVariableAttribute(const string& name, const string& attr, const string& def) {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        return (*var).second.getAttribute(attr, def);
    }
    template <class T>
    T getAttribute(const string& name, T def) {
        T val = def;
        map<string, NetAttribute>::iterator attr = attributes_.find(name);
        if (attr == attributes_.end())
            return def;
        (*attr).second.get(val);
        return val;
    }

    string getAttribute(const string& name, const string& def) {
        map<string, NetAttribute>::iterator attr = attributes_.find(name);
        if (attr == attributes_.end())
            return def;
        string val;
        (*attr).second.get(val);

        return strdup(val.c_str());
    }


    NetVariable getVariable(const string& name) const {
        map<string, NetVariable>::const_iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        return (*var).second;
    }

    void setDefault2D(const string& name)  {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        var->second.default2D();
    }
    void setDefault1D(const string& name) {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if (var == variables_.end())
            throw NoSuchNetcdfVariable(name);
        var->second.default1D();
    }

    map<string, NetAttribute> getAttributes() { return attributes_; }


protected:
    virtual void print(ostream&) const;
    map<string, NetVariable> variables_;
    map<string, NetDimension> dimensions_;
    map<string, NetVariable> dataset_;
    map<string, NetAttribute> attributes_;
    double missing_;

private:
    int file_;

    friend ostream& operator<<(ostream& s, const Netcdf& p) {
        p.print(s);
        return s;
    }
};


#ifdef __clang__
template <class T>
map<nc_type, Accessor<T>*>* Accessor<T>::accessors_ = nullptr;
#endif

const char* nc_type_to_name(int);

inline const char* type_name(double*) { return "double"; }
inline const char* type_name(float*) { return "float"; }
inline const char* type_name(long*) { return "long"; }
inline const char* type_name(int*) { return "int"; }

template <class T>
void Accessor<T>::access(vector<T>& data, vector<size_t>& start, vector<size_t>& edges, NetVariable& var) {
    typename map<nc_type, Accessor<T>*>::const_iterator accessor = accessors_->find(var.type());
    if (accessor == accessors_->end()) {
        std::ostringstream oss;
        oss << "NetcdfDecoder: no accessor from '" << nc_type_to_name(var.type()) << "' converting to '" << type_name((T*)0) << "'";
        throw MagicsException(oss.str());
    }

    (*(*accessor).second)(data, start, edges, var);
}


}  // namespace magics


#endif
