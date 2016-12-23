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


static bool isVariable(int netcdf, int var)
{
	int dims;
    nc_inq_varndims(netcdf, var,  &dims);
       
    if (dims != 1) return true;

   return false;
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
void TypedAccessor<F,T>::operator() (vector<T>& to, vector<size_t>& start, vector<size_t>& edges, NetVariable& var) const
{
	vector<F> from(to.size());
	var.get(from, start, edges);
	// Convert the data....       
	std::transform(from.begin(), from.begin() + to.size(), to.begin(), Convertor<F, T>(var));
	
}

template <class F, class T> 
void TypedAccessor<F,T>::get (vector<F>& from, vector<size_t>& start, vector<size_t>& edges, NetVariable& var)const
{
	var.get(&from.front(), start, edges);
} 

Netcdf::Netcdf(const string& path, const string& method)
{
	int error = nc_open(path.c_str(), NC_NOWRITE, &file_);
		
	if (error) {
		throw NoSuchNetcdfFile(path);
	}

	int num_var;
	int var_ids[100];
	nc_inq_varids(file_, &num_var, var_ids);

	for ( int v = 0; v < num_var; v++)
	{ 
		// get the name 
		string tmp;
		int id = var_ids[v];
		nc_inq_varname 	(file_,  id, &tmp[0]);

		string name(tmp.c_str());
		
		variables_.insert(std::make_pair(name, NetVariable(name, v, file_, method)));
		cout << id << "--->" << name << "----> " << variables_.size() << endl;

		if (isVariable(file_, var_ids[v])) dataset_.insert(std::make_pair(name, NetVariable(name, var_ids[v], file_, method)));
	}


	MagLog::debug() << "Initialisation of  Netcdf [" << path << "] OK! " << "\n";  

	int num_atts;
	nc_inq_varnatts(file_, NC_GLOBAL, &num_atts);
	for ( int v = 0; v < num_atts; v++)
	{

			string tmp;
			nc_inq_attname(file_, NC_GLOBAL, v, &tmp[0]);
			string name(tmp.c_str());
			attributes_.insert(std::make_pair(name, NetAttribute(name, file_, NC_GLOBAL)));
	}

	int num_dims;
	nc_inq_ndims(file_, &num_dims);
	for ( int d = 0; d < num_dims; d++)
	{
			string tmp;
			nc_inq_dimname(file_, d, &tmp[0]);
			string name(tmp.c_str());
			dimensions_.insert(std::make_pair(name, NetDimension(file_, name)));
	}

	cout << *this << endl;

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




int  NetDimension::index(const string& val)
{
	int index = atoi(val.c_str());
	return index;
}

int  NetDimension::value(const string& val)
{
	if ( variable_ != -1 ) {

		//int index = Index::get(variable_->type(), val, variable_->values(), variable_->num_vals());
		NetVariable var(name_, variable_, netcdf_, "index");
		
		return var.find(val);
	}

	// we assume the user is using index! ..
	int index = atoi(val.c_str());
	MagLog::warning() << " Could not find variable return index instead " << index << endl;
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


NetVariable::NetVariable(const string& name, int id, int file, const string& method): name_(name), id_(id), netcdf_(file)
{
	int num_dims;
	nc_inq_varndims(netcdf_, id_, &num_dims);
	int dims[num_dims];
	nc_inq_vardimid(netcdf_, id_, dims);


	for (int d = 0; d < num_dims; d++)
	{
		string tmp;
		nc_inq_dimname(netcdf_, dims[d], &tmp[0]);
		string name(tmp.c_str());
		int var = -1;
		// Try to find if a variable is defined with this name.
		int num_var;
		int var_ids[100];
		nc_inq_varids(netcdf_, &num_var, var_ids);

		for ( int v = 0; v < num_var; v++)
		{ 
			// get the name 
			string tmp;
			int id = var_ids[v];
			nc_inq_varname 	(netcdf_,  id, &tmp[0]);
			string current(tmp.c_str());
			if ( current == name ) {
				var = id;
				cout << "Found it !" << name << endl;
				break;
			}
		
		}   
		dimensions_.insert(std::make_pair(name, NetDimension(netcdf_, name, d, var)));
		dimensions_[name].method_ = method;
	}


	
	int num_atts;
	nc_inq_varnatts(netcdf_, id_, &num_atts);
	for ( int v = 0; v < num_atts; v++)
	{

			string tmp;
			nc_inq_attname(netcdf_, id_, v, &tmp[0]);
			string name(tmp.c_str());
			attributes_.insert(std::make_pair(name, NetAttribute(name, netcdf_, id_)));
	}
		
	missing_ = getDefaultMissing();
}

template <class T>
int find(const T& value, vector<T>&  values)
{
    if ( values.size() == 1  && values[0] == value) 
    	return 0;
	for (int i = 0; i < values.size() - 1; i++) {
		if (values[i] == value) return i;
		if (values[i+1] == value) return i+1;
		if (values[i] < value && value  < values[i+1] ) return i;  
		if (values[i+1] < value && value  < values[i] ) return i+1;     
	}
}

int NetVariable::find(const string& val)
{
	nc_type t = type();
	if ( t == NC_DOUBLE ) {
		vector<double> values;
		values.resize(getSize());
		get(values);
		double dval = tonumber(val);
		return ::find(dval, values);

	}
	if ( t == NC_INT ) {
		vector<int> values;
		values.resize(getSize());
		get(values);
		int dval = tonumber(val);
		return ::find(dval, values);

	}
	if ( t == NC_FLOAT ) {
		vector<float> values;
		values.resize(getSize());
		getValues(values);
		float dval = tonumber(val);
		return ::find(dval, values);

	}
	if ( t == NC_SHORT ) {
		vector<short> values;
		values.resize(getSize());
		get(values);
		short dval = tonumber(val);
		return ::find(dval, values);

	}

}


double NetVariable::getDefaultMissing()
{

	if ( type() == NC_DOUBLE)
		return NC_FILL_DOUBLE;
	return NC_FILL_FLOAT;
}



namespace magics {
	template<> map<nc_type, Accessor<double>*>*  Accessor<double>::accessors_ = 0;
	template<> map<nc_type, Accessor<float>*>*  Accessor<float>::accessors_ = 0;
} // end namespace


static TypedAccessor<short, float>  short_float_accessor(NC_SHORT);
static TypedAccessor<int, float>  int_float_accessor(NC_INT);
static TypedAccessor<float, float>  float_float_accessor(NC_FLOAT);
static TypedAccessor<double, float> double_float_accessor(NC_FLOAT);

//static TypedAccessor<nc_byte, double>  byte_double_accessor(NC_BYTE);
static TypedAccessor<short, double>  short_double_accessor(NC_SHORT);
static TypedAccessor<int, double>  int_double_accessor(NC_INT);
static TypedAccessor<float, double>  float_double_accessor(NC_FLOAT);
static TypedAccessor<double, double> double_double_accessor(NC_DOUBLE);

