/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file Netcdf.h
    \brief Definition of the Netcdf access tools.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 16-Jan-2004
    
    Changes:
    
*/

#ifndef Netcdf_H
#define Netcdf_H

#include "magics.h"
#include "netcdfcpp.h"

#include "MagException.h"
#include "MagLog.h"

namespace magics {

class NoSuchNetcdfVariable : public MagicsException
{
public:
	 NoSuchNetcdfVariable( const string& var ):
		MagicsException("Netcdf MagException:  Can not find variable ---> " + var) 
		{	MagLog::warning() << what_ << "\n"; }
}; 
class NoSuchNetcdfDimension : public MagicsException
{
public:
	 NoSuchNetcdfDimension( const string& dim ):
		MagicsException("Netcdf MagException :  Can not find dimension ---> " + dim) 
		{	MagLog::warning() << what_ << "\n"; }
}; 

class NoSuchNetcdfFile : public MagicsException
{
public:
	 NoSuchNetcdfFile( const string& file ):
		MagicsException("Netcdf MagException: The file " + file + " does not exist or is not a valid netcdf file")
		{	MagLog::error() << what_ << "\n"; }
}; 

struct NetDimension 
{
    string name_;
    long   size_; 
    long   first_;
    long   dim_;
    long   index_;
    string method_;

    NcDim* id_;
    NcVar* variable_;

    NetDimension() {}
    NetDimension(NcDim* id): name_(id->name()), size_(id->size()),
            first_(0), dim_(size_), index_(0),
            id_(id), variable_(0) {}
    NetDimension(NcDim* id, NcVar* variable, long index) : 
        name_(id->name()), size_(id->size()), 
        first_(0), dim_(size_), index_(index), 
        id_(id), variable_(variable)
        {}
        
    void first(const string&);
    void last(const string&);

    int  index(const string&);
    int  value(const string&);
    
    void print(ostream& s) const
    {
        s << name_ << "(" << size_ << ", " << index_ << ", " << first_ << ", " << dim_ << ")";
    }
    friend ostream& operator<<(ostream& s,const NetDimension& p)
		{ p.print(s); return s; }
};   


struct NetAttribute 
{
	string name_;
	NcAtt* id_;
	NetAttribute(const string name, NcAtt* id) : name_(name), id_(id) {} 
	NetAttribute() {}
	void get(double& val)      { val =  id_->as_double(0); }
	void get(float& val)       { val =  id_->as_float(0); }
	void get(const char*& val) { val =  id_->as_string(0); }

};

class NetVariable;

template <class From, class To>
struct Convertor
{
	Convertor(NetVariable& );
	To operator()(From from)
	{      
		return from * scale_factor_ + add_offset_;
	}  

	NetVariable& variable_;
	To    scale_factor_;
	To    add_offset_;
};


template <class T>
class Accessor
{
public:
    Accessor(NcType type) {
        if ( !accessors_) accessors_ = new map<NcType, Accessor<T>*>;
        accessors_->insert(std::make_pair(type, this));
    }
    virtual ~Accessor() {    }
       
    virtual void operator() (vector<T>&,  vector<long>& , vector<long>&, NetVariable&) const {}
    
    static map<NcType, Accessor<T>*>* accessors_;
    static void release() {
 		if ( accessors_ ) 
 			for ( typename map<NcType, Accessor<T>*>::iterator a = accessors_->begin(); a != accessors_->end(); ++a) {
 				Accessor<T>* accessor = a->second;
 				 a->second = 0;
 				 delete accessor;
    		}
	}
    

    static void access(vector<T>& data, vector<long> start, vector<long> edges, NetVariable& var); 
    static void access(vector<T>& data, NetDimension& dim){}
};

template <class F, class T>
class TypedAccessor : public Accessor<T>
{
public:
	TypedAccessor(NcType type) : Accessor<T>(type) {}

	void operator() (vector<T>& to, vector<long>& start, vector<long>& edges, NetVariable& var) const;
	void get (vector<F>& from, vector<long>& start, vector<long>& edges, NetVariable& var) const;
};



struct NetVariable 
{
	string name_;
	NcVar* id_;
	map<string, NetDimension> dimensions_;
	map<string, NetAttribute> attributes_;
    
	NetVariable(const string& name, NcVar* id, const NcFile& file, const string& method);

	void getStartingPoint(vector<long>& dims)
	{
		dims.resize(dimensions_.size());
		for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim)
		{
			dims[(*dim).second.index_] = (*dim).second.first_;
		}
	}
    
	void getDimensions(vector<long>& dims)
	{
		dims.resize(dimensions_.size());
		for (map<string, NetDimension>::iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim)
		{
			dims[(*dim).second.index_] = (*dim).second.dim_;
		}
	}
    
 
    void setFirstPoint(const string& name, const string& first)
    {
        map<string, NetDimension>::iterator dim = dimensions_.find(name);   
        if ( dim == dimensions_.end() ) return;
        (*dim).second.first(first);
    }
   
    void setLastPoint(const string& name, const string& last)
    {
        map<string, NetDimension>::iterator d = dimensions_.find(name);   
        if ( d == dimensions_.end() ) return;
        (*d).second.last(last);
    }
    
    long getSize(const vector<long>& dims)
    {
       long size = 1;
       for (unsigned int i = 0; i < dims.size(); i++)
           size = (dims[i] ) * size;
       return size;    
    }
    void print(ostream& s) const
    {
        s << name_ << "[";
        string sep = "";
        for (map<string, NetDimension>::const_iterator dim = dimensions_.begin(); dim != dimensions_.end(); ++dim) 
        {
            s << sep << (*dim).second;
            sep = ", ";
        }
        
        s << "]" << "\n";
        
    }
    
    template <class T> 
    T  getAttribute(const string& name, T def) 
    {
        T val;
        map<string, NetAttribute>::iterator attr = attributes_.find(name);
        if ( attr == attributes_.end() ) return def;
        (*attr).second.get(val);
        return val;
   
    } 
    
    double getDefaultMissing();
    double getMissing(const string&);

    template <class T>
    void get(vector<T>& vals, map<string, string> first, map<string, string> last)
    {
        for (map<string, string>::const_iterator f = first.begin(); f != first.end(); ++f) {
            setFirstPoint((*f).first, (*f).second);
        }
        for (map<string, string>::const_iterator f = last.begin(); f != last.end(); ++f) {
            setLastPoint((*f).first, (*f).second);
        }
        get(vals);
    }
    
    template <class T>
    void get(vector<T>& vals)
    {
        vector<long> start;
        getStartingPoint(start);
        vector<long> end;
        getDimensions(end);
        
        vals.resize(getSize(end));
        Accessor<T>::access(vals, start, end, *this);
    
        
    }
    
    friend ostream& operator<<(ostream& s,const NetVariable& p)
		{ p.print(s); return s; }
     
};

   
class Netcdf { 
public:

    Netcdf(const string&, const string&);

    virtual ~Netcdf();
	
    typedef map<string, NetVariable> VariableMap;
    
    double getDefaultMissing(const string& name)
    {
    	  map<string, NetVariable>::iterator var = variables_.find(name);
    	  if ( var == variables_.end() ) throw NoSuchNetcdfVariable(name);
    	  return var->second.getDefaultMissing();
    }

    double getMissing(const string&, const string&);



    template <class T>
    void get(const string& name, vector<T>& vals, 
                                map<string, string> first, 
                                map<string, string> last)
    {
    
        map<string, NetVariable>::iterator var = variables_.find(name);
        if ( var == variables_.end() ) throw NoSuchNetcdfVariable(name);
        (*var).second.get(vals, first, last);
    }


    template <class T>
    void get(const string& name, vector<T>& vals)
    {
    
        map<string, NetVariable>::iterator var = variables_.find(name);
        if ( var == variables_.end() ) throw NoSuchNetcdfVariable(name);
        (*var).second.get(vals);
    }
    
    int getDimension(const string& name)
    {
    	  map<string, NetDimension>::iterator dim = dimensions_.find(name);
    	  if ( dim == dimensions_.end() ) {
    		  MagLog::error() << name << " : do not find such dimension\n" << endl;

    		  throw NoSuchNetcdfDimension(name);
    	  }
    	  return dim->second.size_;
    }

    template <class T>
    T getVariableAttribute(const string& name, const string& attr, T def)
    {
        map<string, NetVariable>::iterator var = variables_.find(name);
        if ( var == variables_.end() ) throw NoSuchNetcdfVariable(name);
        return (*var).second.getAttribute(attr, def);
    }      
    template <class T>
    T getAttribute(const string& name, T def)
    {
    	  T val;
    	  map<string, NetAttribute>::iterator attr = attributes_.find(name);
    	  if ( attr == attributes_.end() ) return def;
    	        (*attr).second.get(val);
    	        return val;

     }
     NetVariable getVariable(const string& name)
     {
		map<string, NetVariable>::iterator var = variables_.find(name);
         	if ( var == variables_.end() ) throw NoSuchNetcdfVariable(name);
	 	return (*var).second;
     }
     
     map<string, NetAttribute> getAttributes()
     {
		return attributes_;
     }
	 
      		 
	 
	 
protected:
	virtual void print(ostream&) const; 
	map<string, NetVariable> variables_;
	map<string, NetDimension> dimensions_;
	map<string, NetVariable> dataset_;
	map<string, NetAttribute> attributes_;
     
private:
	NcFile    file_;

	friend ostream& operator<<(ostream& s,const Netcdf& p)
		{ p.print(s); return s; }
};


template <class From, class To> 
class DataAccessor
{
public:
    DataAccessor(Netcdf& netcdf) : netcdf_(netcdf) {}
    void operator()(const string& name, vector<To>& to)
    {
        vector<From> from;
        netcdf_.get(name, from, start_, end_);
        
        for (typename vector<From>::const_iterator val = from.begin(); val != from.end(); ++val) {
            To add = To(*val);
         
            to.push_back(To(*val));
        }
    }
    
   void setDimension(const string& name, long from, long dim) {
        start_.insert(std::make_pair(name, from));
        end_.insert(std::make_pair(name, dim));
   }
   
   void setDimension(const string& name, long val) {
        start_.insert(std::make_pair(name, val));
        end_.insert(std::make_pair(name, 1));
   }
    
   map<string, long> start_;
   map<string, long> end_;
   Netcdf& netcdf_;
};

template <class T> 
void Accessor<T>::access(vector<T>& data, vector<long> start, vector<long> edges, NetVariable& var) 
{
	typename map<NcType, Accessor<T>*>::const_iterator accessor = accessors_->find(var.id_->type());
	if ( accessor == accessors_->end() ) throw new MagicsException("No accessor available");

	(*(*accessor).second)(data, start, edges, var);
}

} // Namespace Magics




#endif
