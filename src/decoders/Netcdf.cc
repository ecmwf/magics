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
#include <algorithm>
#include <Netcdf.h>
#include <MagException.h>
#include <MagLog.h>
 

using namespace magics;


static bool isVariable(NcVar* var)
{
    if (var->num_dims() != 1) return true;
    
    string name0(var->name());
    string name1(var->get_dim(0)->name());
    if ( name0 == name1) return false;
    return true; 
}

template <class From, class To>
Convertor<From,To>::Convertor(NetVariable& var) : variable_(var)
{
	To scale(1);
	To offset(0);
	scale_factor_ = variable_.getAttribute("scale_factor", scale);
	add_offset_ = variable_.getAttribute("add_offset", offset);
	missing_ = variable_.getMissing();
}



template <class F, class T>   
void TypedAccessor<F,T>::operator() (vector<T>& to, vector<long>& start, vector<long>& edges, NetVariable& var) const
{
	F* from = new F[to.size()];
	var.id_->set_cur(&start[0]);
	var.id_->get(from, &edges[0]);
	// Convert the data....       
	std::transform(from, from + to.size(), to.begin(), Convertor<F, T>(var));
	delete[] from;
}

template <class F, class T> 
void TypedAccessor<F,T>::get (vector<F>& from, vector<long>& start, vector<long>& edges, NetVariable& var)const
{
	var.id_->set_cur(&start[0]);
	var.id_->get(from, &edges[0]);
} 

Netcdf::Netcdf(const string& path, const string& method) : file_(path.c_str())
{
	if (file_.is_valid() == false) {
		throw NoSuchNetcdfFile(path);
	}
	for ( int v = 0; v < file_.num_vars(); v++)
	{ 
		NcVar* var = file_.get_var(v); 
		variables_.insert(std::make_pair(var->name(), NetVariable(var->name(), var, file_, method)));

		if (isVariable(var)) dataset_.insert(std::make_pair(var->name(), NetVariable(var->name(), var, file_, method)));
	}
	MagLog::debug() << "Initialisation of  Netcdf [" << path << "] OK! " << "\n";  
	for ( int v = 0; v < file_.num_atts(); v++)
		{
			NcAtt* attr = file_.get_att(v);
			attributes_.insert(std::make_pair(attr->name(), NetAttribute(attr->name(), attr)));
		}
	for ( int d = 0; d < file_.num_dims(); d++)
		{
			NcDim* var = file_.get_dim(d);
			dimensions_.insert(std::make_pair(var->name(), NetDimension(var)));
		}
}


Netcdf::~Netcdf() 
{	
}
double Netcdf::getMissing(const string& var, const string& attr)
{

	missing_ = getAttribute(attr, getDefaultMissing(var));
	missing_ = getVariableAttribute(var, attr, missing_);
	return missing_;
}

void Netcdf::print(ostream& out)  const
{
	out << "print Netcdf: " << "\n";
	out << "Variables: " << "\n";
	for (map<string, NetVariable>::const_iterator var = variables_.begin(); var != variables_.end(); ++var)
	{
		out << (*var).second;
	}
	out << "Dataset: " << "\n";
	for (map<string, NetVariable>::const_iterator var = dataset_.begin(); var != dataset_.end(); ++var)
	{
		out << (*var).second;
	}
}


struct Index
{
	static map<NcType, Index*>* tools_;
	Index(NcType type)
	{
		if ( tools_ == 0) tools_ = new  map<NcType, Index*>();
		tools_->insert(std::make_pair(type, this));
	}
	virtual int operator()(const string& val,  NcValues* values, long nb )
	{
		ASSERT(false);
	}
	static int get(const NcType& type, const string& val,  NcValues* values, long nb)
	{
		map<NcType, Index*>::const_iterator tool = tools_->find(type);
		if ( tool == tools_->end() ) {
			ASSERT(false);
			throw new MagicsException("No Index available");
		}
        
		return (*(*tool).second)(val, values, nb);
	}
};

struct FloatIndex: public Index
{
	FloatIndex() : Index(ncFloat) {}
    
	virtual int operator()(const string& val,  NcValues* values, long nb )
	{
		float value = atof(val.c_str());
                if ( nb == 1 && values->as_float(0) == value) return 0;
		for (int i = 0; i < nb - 1; i++) {
			if (values->as_float(i) == value) return i;
			if (values->as_float(i+1) == value) return i+1;
			if (values->as_float(i) < value && value  < values->as_float(i+1) ) return i;  
			if (values->as_float(i+1) < value && value  < values->as_float(i) ) return i+1;     
		}
		throw MagicsException("No such value : " + val);
	}
};

struct DoubleIndex: public Index
{
	DoubleIndex() : Index(ncDouble) {}
    
	virtual int operator()(const string& val,  NcValues* values, long nb )
	{
		double value = tonumber(val);
        if ( nb == 1 && values->as_double(0) == value) return 0;
		for (int i = 0; i < nb - 1; i++) {
			if (values->as_double(i) == value) return i;
			if (values->as_double(i+1) == value) return i+1;
			if (values->as_double(i) < value && value  < values->as_double(i+1) ) return i;  
			if (values->as_double(i+1) < value && value  < values->as_double(i) ) return i+1;     
		}
		throw MagicsException("No such value : " + val);
	}
};


struct IntIndex: public Index
{
	IntIndex() : Index(ncInt) {}
    
	virtual int operator()(const string& val,  NcValues* values, long nb )
	{
		int value = atoi(val.c_str());
        	 if ( nb == 1 && values->as_int(0) == value) return 0;

		for (int i = 0; i < nb - 1; i++)
		{
			if (values->as_int(i) == value) return i;
			if (values->as_int(i+1) == value) return i+1;
			if ( values->as_int(i) < value && value < values->as_int(i+1) ) return i;   
			if ( values->as_int(i+1) < value && value < values->as_int(i) ) return i+1;     
		}
		throw MagicsException("No such value : " + val);
	}
};

struct StringIndex: public Index
{
	StringIndex() : Index(ncChar) {}
    
	virtual int operator()(const string& val,  NcValues* values, long nb )
	{

		for (int i = 0; i < nb; i++)
		{
			string read(values->as_string(i));

			if ( read == val) {

				return i/64;
			}
		}
		throw MagicsException("No such value : " + val);
	}
};

map<NcType, Index*>*  Index::tools_ = 0;

static FloatIndex float_index;
static IntIndex int_index;
static DoubleIndex double_index;
static StringIndex string_index;

int  NetDimension::index(const string& val)
{
	int index = atoi(val.c_str());
	// if (index < )
	return index;
}

int  NetDimension::value(const string& val)
{
	if ( variable_ ) {
		int index = Index::get(variable_->type(), val, variable_->values(), variable_->num_vals());

		return index;
	}
	// we assume the user is using a simple ..
	int index = atoi(val.c_str());
	MagLog::warning() << " Could not find variable return index instead " << index << endl;
	//if (index < )
	return index;
}

void NetDimension::first(const string& val)
{

	first_ = ( magCompare(method_, "value") ) ? value(val) : index(val);

}


void NetDimension::last(const string& val)
{

	int last =  (magCompare(method_, "value")) ? value(val) : index(val);
	if ( last < first_ )
	{
		MagLog::warning() << "last position (" + val + ") < first position: exchange " << "\n";
		int tmp = first_;
		first_ = last;
		last = tmp;
	} 
	dim_ = last - first_ + 1;   
}


NetVariable::NetVariable(const string& name, NcVar* id, const NcFile& file, const string& method): name_(name), id_(id)
	{
		for (int d = 0; d < id_->num_dims(); d++)
		{
			NcDim* dim = id_->get_dim(d);
			NcVar* var = 0;
			string dim_name = dim->name();
		    for (int v = 0; v < file.num_vars(); v++) {
		    	 string var_name = file.get_var(v)->name();
		         if (var_name == dim_name) var = file.get_var(dim->name());
		         
		    }
			dimensions_[dim->name()]= NetDimension(dim, var, d); 
			dimensions_[dim->name()].method_ = method;
		}
		for (int a = 0; a < id_->num_atts(); a++)
		{
			NcAtt* att = id_->get_att(a);
			attributes_[att->name()] = NetAttribute(att->name(), att); 
		}
	}

double NetVariable::getDefaultMissing()
{

	if (id_->type() == ncDouble)
		return NC_FILL_DOUBLE;
	return NC_FILL_FLOAT;
}



namespace magics {
	template<> map<NcType, Accessor<double>*>*  Accessor<double>::accessors_ = 0;
	template<> map<NcType, Accessor<float>*>*  Accessor<float>::accessors_ = 0;
} // end namespace


static TypedAccessor<short, float>  short_float_accessor(ncShort);
static TypedAccessor<int, float>  int_float_accessor(ncInt);
static TypedAccessor<float, float>  float_float_accessor(ncFloat);
static TypedAccessor<double, float> double_float_accessor(ncFloat);

static TypedAccessor<ncbyte, double>  byte_double_accessor(ncByte);
static TypedAccessor<short, double>  short_double_accessor(ncShort);
static TypedAccessor<int, double>  int_double_accessor(ncInt);
static TypedAccessor<float, double>  float_double_accessor(ncFloat);
static TypedAccessor<double, double> double_double_accessor(ncDouble);

