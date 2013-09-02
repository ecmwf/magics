/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeBaseSTInstance.h
	\brief This file contains a base class called TeBaseSTInstance that represents 
	an instance in time of a geographical object or element.  
*/

#ifndef  __TERRALIB_INTERNAL_BASESTINSTANCE_H
#define  __TERRALIB_INTERNAL_BASESTINSTANCE_H

#include "TeCoord2D.h"
#include "TeGeometryAlgorithms.h"
#include "TeAttribute.h"
#include "TeSharedPtr.h"

#include <string>
#include <map> 
#include <vector>

using namespace std;

class TeTheme;

/*! \class TeBaseSTInstance
	\brief A base class that represents an instance in a time of a spatial object.

	A spatio-temporal instance (STInstance) is composite of an attribute set and geometries 
	of a spatial element or object that are valid in a specific time. This class implements
	a base generic spatio-temporal instance that can be specialized according to the
	types that represent its geometries and its valid time.
	
*/
template<typename GeometryType, typename TimeType>
class TeBaseSTInstance
{
	
protected:
	//! object identification
	string					object_id_;		
	//! unique identification in each attribute table
	vector<string>			unique_id_;	
	//! set of properties or attributes values
	vector<string>			properties_;	
	//! geometries
	GeometryType			geometries_;	
	//! valid time 
	TimeType				time_;		
	//! the slice or group that contains this instance  
	int						slice_;		
	//! a shared pointer to the descriptions of all attributes
	TeSharedPtr<TeAttributeList> 	attrList_;			
		
public:		

	//! Empty constructor
	TeBaseSTInstance() : object_id_(""), slice_(-1), attrList_(0)
	{ }

	//! Constructor
	TeBaseSTInstance (const string& object_id, const vector<string>& prop, TeAttributeList* attList = 0, const int& s = -1) : 
		object_id_(object_id), 
		properties_(prop),
		slice_(s),
		attrList_(TeSharedPtr<TeAttributeList>(attList))
		{ } 

	//! Constructor
	TeBaseSTInstance (const string& object_id, const GeometryType& geometries, const TimeType& time, 
		const int& s = -1) : 
		object_id_(object_id), 
		geometries_(geometries),
		time_(time),
		slice_(s),
		attrList_(0)
		{ } 

	//! Constructor
	TeBaseSTInstance (const string& object_id, const vector<string>& prop, TeAttributeList* attList,
		const GeometryType& geometries,	const int& slice, const TimeType& time ) :
		object_id_(object_id), 
		properties_(prop),
		geometries_(geometries),
		time_(time),
		slice_(slice),
		attrList_(TeSharedPtr<TeAttributeList>(attList))
		{ } 
			
	//! Destructor
	virtual ~TeBaseSTInstance() 
	{ } 

	//! Equal operator 
	virtual bool operator== (const TeBaseSTInstance<GeometryType, TimeType>& other);

	//! Returns the object identification
	virtual string getObjectId () 
	{	return object_id_;	}
	
	//! Sets the object identification
	virtual void setObjectId (const string& id) 
	{	object_id_ = id;	}

	//! Deprecated: Returns the object identification 
	virtual string objectId () 
	{	return getObjectId();	}
	
	//! Deprecated: Sets the object identification 
	virtual void objectId (const string& id) 
	{	setObjectId(id);	}

	//! Returns the unique identification in all attribute tables
	virtual vector<string>& getUniqueId() 
	{	return unique_id_;	}

	//! Returns the unique identification of the i-th attribute table
	virtual string getUniqueId(const int& i); 
	
	//! Sets the unique identification in all attribute tables
	virtual void setUniqueId (const vector<string>& id) 
	{	unique_id_ = id;	}

	//! Adds an unique identification
	virtual void addUniqueId (const string& id) 
	{	unique_id_.push_back(id);	}

	//! Deprecated: Returns the instance identification in all attribute tables
	virtual vector<string>& uniqueId () 
	{	return getUniqueId();	}

	//! Deprecated: Returns the instance identification in the index-th attribute table
	virtual string uniqueId (int index) 
	{	return getUniqueId(index);	}
	
	//! Deprecated: Sets the instance identifications in all attribute tables
	virtual void uniqueId (const vector<string>& ids) 
	{	setUniqueId(ids);	}

	//! Returns the valid time 
	virtual TimeType getTime() 
	{	return time_;	}
	
	//! Sets the valid time 
	virtual void setTime (const TimeType& t) 
	{	time_ = t;	}

	//! Sets the property value vector 
	virtual void setProperties(const vector<string>& p) 
	{	properties_ = p;	}

	//! Deprecated: Sets the property vector to the ST instance 
	virtual void setProperties(TePropertyVector& p); 

	//! Sets the value (as a string) of the i-th property 
	virtual bool setPropertyValue (const int& i, const string& val); 

	//! Sets the value (as a string) of a property named 'name'
	virtual bool setPropertyValue (const string& name, const string& val); 

	//! Adds a new property value
	virtual void addPropertyValue (const string& val)
	{	properties_.push_back (val); }

	//! Removes the i-th property value
	virtual bool removePropertyValue (const int& i); 

	//! Adds a new property or set its value   
	/*!
		Verifies if there is this property in the attribute list.
		If not, adds it in the ST instance and in the attribute list. 
		Otherwise, set the property value
	*/
	virtual bool addProperty(TeProperty& prop); 

	//! Adds a new attribute in the attribute list 
	virtual bool addProperty(TeAttribute& rep); 
	
	//! Returns the property value vector 
	virtual vector<string>& getProperties()
	{	return properties_;	}

	//! Gets the property vector 
	virtual void getPropertyVector(TePropertyVector& propVec); 

	//! Deprecated: Returns the property vector from the ST instance
	TePropertyVector getPropertyVector(); 
	
	//! Gets the i-th property
	virtual bool getProperty (TeProperty& prop, unsigned int i = 0);	
	
	//! Gets the property named "name"
	virtual bool getProperty (TeProperty& prop, string name); 
	
	//! Gets the value (as a string) of the i-th property
	virtual bool getPropertyValue (string& val, const int& i = 0); 

	//! Gets the the value (as a string) of a property named "name"
	virtual bool getPropertyValue (const string& name, string& val); 
	
	//! Returns the value (as a double) of the i-th property
	virtual double operator[](int i); 

	//! Deprecated: Sets the property vector to the ST instance
	virtual void properties(TePropertyVector& p)
	{	setProperties(p); }

	//! Returns the geometries
	virtual GeometryType& getGeometries() 
	{	return geometries_;}

	//! Sets the geometries  
	virtual void setGeometry(const GeometryType& g)
	{	geometries_ = g; }

	//! Deprecated: Returns the geometries
	virtual GeometryType& geometries() 
	{	return getGeometries();}

	//! Sets the group or slice that contains this instance 
	virtual void setSlice (int s) 
	{	slice_ = s; }

	//! Returns the group or slice that contains this instance
	virtual int getSlice()
	{	return slice_; }

	//! Deprecated: Sets the group or slice that contains this instance 
	virtual void slice (int s) 
	{	setSlice(s); }

	//! Deprecated: Returns the group or slice that contains this instance
	virtual int slice()
	{	return getSlice(); }

	//! Gets a pointer to attribute desciptions
	const TeAttributeList*	getAttrList()
	{	return attrList_.nakedPointer(); }

	//! Creates a new internal copy of the attribute description
	void setAttrList(const TeAttributeList& attList); 

	//! Sets a shared pointer to attribute descriptions
	void setAttrList(TeSharedPtr<TeAttributeList>& attrList);

	//! Clears the instance, its attributes. It must be implemented by specialized classes to clear its geometries.
	virtual void clear();

	//! Returns a centroid associated to the geometries of the instance
	virtual TeCoord2D getCentroid();

	//! Returns an area the geometries of the instance
	virtual double getArea();

	//! Verifies if the time associated with this instance is valid. It must be implemented by specialized classes.
	virtual bool isTimeValid()
	{	return false; } 
	
	//! Deprecated: Returns the theme pointer that contains this instance. 
	virtual TeTheme* theme() 
	{ return 0; }

	//! Deprecated: Sets the theme that contains this instance.
	virtual void theme(TeTheme*)  
	{  }
};

template<typename GeometryType, typename TimeType> bool
TeBaseSTInstance<GeometryType, TimeType>::operator== (const TeBaseSTInstance<GeometryType, TimeType>& other)
{
	return ( (object_id_ == other.object_id_) && 
			 (time_ == other.time_) &&
			 (slice_ == other.slice_));
}

template<typename GeometryType, typename TimeType> string 
TeBaseSTInstance<GeometryType, TimeType>::getUniqueId(const int& i) 
{	
	if(i<(int)unique_id_.size())
		return unique_id_[i];	
	return "";
}

template<typename GeometryType, typename TimeType> void 
TeBaseSTInstance<GeometryType, TimeType>::setAttrList(const TeAttributeList& attList)
{
	attrList_.reset(new TeAttributeList()); 
	*attrList_ = attList;
}

template<typename GeometryType, typename TimeType> void 
TeBaseSTInstance<GeometryType, TimeType>::setAttrList(TeSharedPtr<TeAttributeList>& attList)
{
	attrList_ = attList;
}

template<typename GeometryType, typename TimeType> void 
TeBaseSTInstance<GeometryType, TimeType>::setProperties(TePropertyVector& p)
{
	properties_.clear();
	TePropertyVector::iterator it = p.begin();
	while(it!=p.end())
	{
		addPropertyValue(it->value_); 
		++it;
	}
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::setPropertyValue (const int& i, const string& val)
{	
	if(i<0 || i>=(int)properties_.size())
		return false;
	properties_[i] = val;
	return true;
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::setPropertyValue (const string& name, const string& val)
{	
	if(!attrList_.isActive())
		return false;

	string newName = TeConvertToUpperCase(name); 
	size_t pos = name.find(".", 0, 1);
	if (pos != string::npos)
		newName = TeConvertToUpperCase((name.substr(pos+1)));

	for(unsigned int i=0; i<attrList_->size(); ++i)
	{
		string s = TeConvertToUpperCase((attrList_->operator[](i)).rep_.name_);
		if( (s == TeConvertToUpperCase(name)) || (s == newName))
		{
			if(i>=properties_.size())
				return false;
			properties_[i] = val;
			return true;
		}
	}
	return false; 
}

template<typename GeometryType, typename TimeType> void  
TeBaseSTInstance<GeometryType, TimeType>::getPropertyVector(TePropertyVector& propVec)
{
	if(!attrList_.isActive())
		return;
    if(properties_.size()!=attrList_->size())
		return;
	propVec.clear();
	for(unsigned int i=0; i<properties_.size(); ++i)
	{
		TeProperty p;
		p.value_ = properties_[i];
		p.attr_ = attrList_->operator[](i);
		propVec.push_back(p);
	}
}

template<typename GeometryType, typename TimeType> TePropertyVector  
TeBaseSTInstance<GeometryType, TimeType>::getPropertyVector()
{
	TePropertyVector vec; 
	this->getPropertyVector(vec);
	return vec;
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::getProperty (TeProperty& prop, unsigned int i)
{
	if(!attrList_.isActive())
		return false;

	if(i>=properties_.size() || i>=attrList_->size())
		return false;
	prop.value_ = properties_[i];
	prop.attr_ = (*attrList_)[i];
	return true;
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::getProperty (TeProperty& prop, string name)
{
	if(!attrList_.isActive())
		return false;
	 if(properties_.size()!=attrList_->size())
		return false;

	string newName = TeConvertToUpperCase(name); 
	size_t pos = name.find(".", 0, 1);
	if (pos != string::npos)
		newName = TeConvertToUpperCase(name.substr(pos+1));

	for(unsigned int i=0; i<attrList_->size(); ++i)
	{
		string s = TeConvertToUpperCase((attrList_->operator[](i)).rep_.name_); 
		if((s == TeConvertToUpperCase(name)) || (s == newName))
		{
			prop.value_ = properties_[i];
			prop.attr_ = attrList_->operator[](i);
			return true;
		}
	}
	return false; 
}
	
template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::getPropertyValue (string& val, const int& i)
{
	if(i<0 || i>= (int)properties_.size())
		return false;
	val = properties_[i];
	return true;
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::getPropertyValue (const string& name, string& val)
{	
	if(!attrList_.isActive())
		return false;

	if(properties_.size()!=attrList_->size())
		return false;

	string newName = TeConvertToUpperCase(name); 
	size_t pos = name.find(".", 0, 1);
	if (pos != string::npos)
		newName = TeConvertToUpperCase(name.substr(pos+1));

	for(unsigned int i=0; i<attrList_->size(); ++i)
	{
		string s = TeConvertToUpperCase((attrList_->operator[](i)).rep_.name_); 
		if((s == TeConvertToUpperCase(name)) || (s == newName))
		{
			val = properties_[i];
			return true;
		}
	}
	return false; 
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::addProperty(TeProperty& prop)
{
	if(!attrList_.isActive())
		return false;

	string newName = TeConvertToUpperCase(prop.attr_.rep_.name_); 
	size_t pos = (prop.attr_.rep_.name_).find(".", 0, 1);
	if (pos != string::npos)
		newName = TeConvertToUpperCase(prop.attr_.rep_.name_.substr(pos+1));

	for(unsigned int i=0; i<attrList_->size(); ++i)
	{
		string s = TeConvertToUpperCase((attrList_->operator[](i)).rep_.name_); 
		if((s == TeConvertToUpperCase(prop.attr_.rep_.name_)) || (s == newName))
		{
			//the property already exists
			if(i>=properties_.size())
				properties_.push_back (prop.value_);
			else
				properties_[i] = prop.value_;
			return true;
		}
	}

	// Adds a new property
	attrList_->push_back(prop.attr_);
	properties_.push_back (prop.value_);
	return true; 
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::addProperty(TeAttribute& attr)
{
	if(!attrList_.isActive())
		return false;
	attrList_->push_back(attr);
	return true; 
}

template<typename GeometryType, typename TimeType> bool 
TeBaseSTInstance<GeometryType, TimeType>::removePropertyValue(const int& i)
{
	int index = 0;
	vector<string>::iterator it = properties_.begin();
	while(it!=properties_.end())
	{
		if(index==i)
		{
			properties_.erase(it);
			return true;
		}
		++it;
		++index;
	}
	return false;
}

template<typename GeometryType, typename TimeType> double 
TeBaseSTInstance<GeometryType, TimeType>::operator[](int i)
{	
	double val = TeMAXFLOAT; 
	if(i<0 || i>=(int)properties_.size() || properties_[i].empty())
		return val;
	val = atof(properties_[i].c_str()); 
	return val;  
}
	
template<typename GeometryType, typename TimeType> void
TeBaseSTInstance<GeometryType, TimeType>::clear()
{
	object_id_ = ""; 
	slice_ = -1; 
    properties_.clear(); 
	unique_id_.clear();
	attrList_.reset(0);
}


template<typename GeometryType, typename TimeType> TeCoord2D
TeBaseSTInstance<GeometryType, TimeType>::getCentroid()
{
	return TeFindCentroid(this->getGeometries());
}

template<typename GeometryType, typename TimeType> double
TeBaseSTInstance<GeometryType, TimeType>::getArea()
{
	return TeGeometryArea(this->getGeometries());
}

			
#endif 
