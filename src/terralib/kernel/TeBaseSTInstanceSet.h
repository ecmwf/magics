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
/*! \file TeBaseSTInstanceSet.h
	\brief This file contains structures to deal with a set of spatio-temporal 
	instances. These instances can belong to a specific layer or theme.
*/

#ifndef  __TERRALIB_INTERNAL_STINSTANCESET_H
#define  __TERRALIB_INTERNAL_STINSTANCESET_H

#include "TeBaseSTInstance.h"
#include "TeRTree.h"
#include "TeTheme.h"
#include "TeLayer.h"
#include "TeQuerierParams.h"
#include "TeQuerier.h"
#include "TeSharedPtr.h"

/*! \class TeBaseSTInstanceSet
	\brief A abstract class that represents a set of spatial temporal instances.

	This abstract class implements a generic spatio-temporal instance set. It must 
	be specialized according to the instance type (its geometry type, its time type
	and its own type). 

	\sa TeBaseSTInstance TeTheme TeLayer
	
*/
template< typename GeometryType, typename TimeType, typename InstanceType >
class TeBaseSTInstanceSet  
{

protected:

	//! Set of spatio temporal instances
	vector<InstanceType>	instances_;		

	//! Minimal time associated to spatial temporal instances
	TimeType				minTime_;
	//! Maximal time associated to spatial temporal instances
	TimeType				maxTime_;

	//! Description of all attributes 
	TeSharedPtr<TeAttributeList>	attrList_;

	//! A pointer to a theme
	TeTheme*				theme_;
	//! A pointer to a layer
	TeLayer*				layer_;	
	//! A bounding box that contains all geometries of the spatial temporal instances
	TeBox					box_;
	
	//! A map from object identity to its associated instances in the set 
	map<string, vector<int> >		objectIdToInstances_;
	//! A map from time to its associated instances in the set
	map<TimeType, vector<int> >		timeToInstances_;
	//! A map from slice to its associated instances in the set
	map<int, vector<int> >			sliceToInstances_;
	//! A struture to index the geometries: RTree
	TeSAM::TeRTree<int>*			rTree_;		

	//! Builds the set using a given querier and a specific slice
	virtual bool buildImpl(TeQuerier* querier, const int& slide = -1) = 0;

public:

	//! An iterator that traverse each instance in the set
	typedef typename vector<InstanceType>::iterator iterator;
	
	//! Constructor. It does not initialize the rTree. 
	TeBaseSTInstanceSet(); 

	//! Constructor. It initializes the rTree based on theme box.
	TeBaseSTInstanceSet(TeTheme* theme, const TeAttributeList& attrList = TeAttributeList()); 

	//! Constructor. It initializes the rTree based on layer box.
	TeBaseSTInstanceSet(TeLayer* layer, const TeAttributeList& attrList = TeAttributeList());  

	//! Constructor. It initializes the rTree based on the given box. 
	TeBaseSTInstanceSet(const TeBox& box, const TeAttributeList& attrList = TeAttributeList());

	//! Copy constructor
	TeBaseSTInstanceSet (const TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>& other); 

	//! Destructor
	virtual ~TeBaseSTInstanceSet();  
	
	//! Assignment operator
	virtual TeBaseSTInstanceSet& operator= (const TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>& other);

	//! Gets minimal time
	virtual TimeType getMinTime()
	{ return minTime_; }

	//! Sets minimal time
	virtual void setMinTime(TimeType& t)
	{ minTime_ = t; } 

	//! Gets maximal time
	virtual TimeType getMaxTime()
	{ return maxTime_; }

	//! Sets maximal time
	virtual void setMaxTime(TimeType& t)
	{ maxTime_ = t; } 

	//! Gets the attribute list, that is, the description of all attributes
	virtual TeAttributeList& getAttributeList()
	{	return (*attrList_);  }

	//! Sets the attribute list, that is, the description of all attributes
	virtual void setAttributeList(TeAttributeList& attrs); 

	//! Returns the index of an attribute named "attrName"
	virtual int getAttributeIndex(const string& attrName);

	//! Gets theme pointer
	virtual TeTheme* getTheme()
	{	return theme_;  }

	//! Sets theme pointer
	virtual void setTheme(TeTheme* t)
	{	theme_ = t; }

	//! Deprecated: Returns a theme pointer
	TeTheme* theme()
	{	return getTheme(); }

	//! Gets layer pointer 
	virtual TeLayer* getLayer()
	{	return layer_;  }

	//! Sets layer pointer
	virtual void setLayer(TeLayer* l)
	{	layer_ = l; }
	
	//! Gets a reference to the instance set 
	virtual vector<InstanceType>& getSTInstances()
	{	return instances_; }

	//! Gets a pointer to the index-th ST instance
	virtual InstanceType* getSTInstance(const unsigned int& index);

	//! Gets a pointer to a ST instance of an object in a specific slice  
	virtual InstanceType* getSTInstance(const string& objectId, const int& slice = -1);

	//! Gets a pointer to a ST instance of an object in a specific time and slice
	virtual InstanceType* getSTInstance(const string& objectId, TimeType& time, const int& slice = -1);

	//! Gets pointers to all ST instances of a specific object in a specific slice
	virtual void getSTInstances(vector<InstanceType*>& set, const string& objectId, const int& slice = -1);

	//! Gets pointers to all ST instances of a specific object in a specific time and slice
	virtual void getSTInstances(vector<InstanceType*>& set, const string& objectId, TimeType& time, const int& slice = -1);
	
	///! Gets pointers to all ST instances of a specific time
	virtual void getSTInstances(vector<InstanceType*>& set, TimeType& time, const int& slice = -1);

	//! Gets pointers to all ST instances of a specific slice
	virtual void getSTInstances(vector<InstanceType*>& set, const int& slice);

	//! Inserts a new ST instance in the set 
	virtual bool insertSTInstance (InstanceType& object);
		
	//! Returns the number of instances 
	virtual int numSTInstance(); 
	
	//! Returns the number of instances of an object or element
	virtual int numSTInstance(const string& objectId); 

	//! Returns the number of instances in a specific slice
	virtual int numSTInstance(const int& slice);

	//! Returns the number of instances in a specific time
	virtual int numSTInstance(TimeType& time);

	//! Returns the number of elements or objects in the set
	virtual int numElements()
	{	return objectIdToInstances_.size(); }

	//! Sets a geometry of an object in a specific slice
	virtual bool setGeometry(const string& object_id, GeometryType& geom, const int& slice = -1);

	//! Sets a geometry of an object in a specific time and slice
	virtual bool setGeometry(const string& object_id, GeometryType& geom, TimeType& time,  const int& slice = -1);

	//! Gets the geometry of an object in a specific slice 
	virtual bool getGeometry(const string& object_id, GeometryType& geom, const int& slice = -1);

	//! Gets the geometry of an object in a specific time and slice
	virtual bool getGeometry(const string& object_id, GeometryType& geom, TimeType& time, const int& slice = -1);

	//! Gets a vector of attributes or properties of a object that is valid in a slice 
	virtual bool getPropertyVector (const string& object_id, TePropertyVector& propVec, const int& slice = -1);

	//! Gets a vector of attributes or properties of a object that is valid in a time and slice
	virtual bool getPropertyVector (const string& object_id, TePropertyVector& propVec, TimeType& time, const int& slice = -1);

	//! Sets a vector of attribute values of a object that is valid in a slice 
	virtual bool setProperties (const string& object_id, const vector<string>& values, const int& slice = -1);

	//! Sets a vector of attribute values of a object that is valid in a time and slice
	virtual bool setProperties (const string& object_id, const vector<string>& values, TimeType& time, const int& slice = -1);
	
	//! Gets a vector of attribute values of a object that is valid in a slice 
	virtual bool getProperties (const string& object_id, vector<string>& values, const int& slice = -1);

	//! Gets a vector of attribute values of a object that is valid in a time and slice
	virtual bool getProperties (const string& object_id, vector<string>& values, TimeType& time, const int& slice = -1);

	//! Sets the value of the attribute named "attr_name" of an object that is valid in a slice 
	virtual bool setAttributeValue (const string& object_id, const string& attr_name,  const string& val, const int& slice = -1);

	//! Sets the value of the attribute named "attr_name" of an object that is valid in a time and slice
	virtual bool setAttributeValue (const string& object_id, const string& attr_name,  const string& val, TimeType& time, const int& slice = -1);

	//! Sets the value of the i-th attribute of an object that is valid in a slice 
	virtual bool setAttributeValue (const string& object_id, const int& i, const string& val, const int& slice = -1); 

	//! Sets the value of the i-th attribute of an object that is valid in a time and slice
	virtual bool setAttributeValue (const string& object_id, const int& i, const string& val, TimeType& time, const int& slice = -1); 
	
	//! Get the value of the attribute named "attr_name" of an object that is valid in a slice 
	virtual bool getAttributeValue (const string& object_id, const string& attr_name,  string& val, const int& slice = -1);

	//! Get the value of the attribute named "attr_name" of an object that is valid in a time and slice
	virtual bool getAttributeValue (const string& object_id, const string& attr_name,  string& val, TimeType& time, const int& slice = -1);

	//! Gets the value of the i-th attribute of an object that is valid in a slice 
	virtual bool getAttributeValue (const string& object_id, const int& i, string& val, const int& slice = -1); 

	//! Gets the value of the i-th attribute of an object that is valid in a time and slice
	virtual bool getAttributeValue (const string& object_id, const int& i, string& val, TimeType& time, const int& slice = -1); 

	//! Adds a new attribute in the attribute list and adds it in all instances in the set using a default value
	virtual bool addProperty(TeAttributeRep& attr, const string& defaultValue);

	//! Adds a new attribute only in the attribute list
	virtual bool addProperty(TeAttribute& attr);

	/*!
		\brief Adds the property or sets its value in the object in a valid slice.

		If there is already the property in the attribute list, sets the value of the instances of an object  
		valid in a specific slice. If there is no this property in the attribute list, adds it in the attribute
		list and in all instances in the set. After that, sets the value of the instances of an object  
		valid in a specific slice.
	*/
	virtual bool addProperty(const string& object_id, TeProperty& prop, const int& slice=-1);

	/*!
		\brief Adds the property or sets its value in the object in a valid time and slice.

		If there is already the property in the attribute list, sets the value of the instances of an object  
		valid in a specific time and slice. If there is no this property in the attribute list, adds it in the attribute
		list and in all instances in the set. After that, sets the value of the instances of an object  
		valid in a specific time and slice.
	*/
	virtual bool addProperty(const string& object_id, TeProperty& prop, TimeType& time, const int& slice = -1);

	//! Removes an attribute of the attribute list and removes it of all instances in the set 
	virtual void removeProperty(TeAttributeRep& attr);

	//! Calculates the bounding box of all instances of all objects included in the set
	virtual TeBox& getBox();

	//! Sets the bounding box of all instances of all objects included in the set
	virtual void setBox(TeBox& b)
	{	box_ = b; }

	//! Clears the set 
	virtual void clear();  
		
	//! Returns an iterator to the first element of the set
	virtual iterator begin()
	{	return instances_.begin(); }
	
	//! Returns an iterator to the one past last element of the set
	virtual iterator end()
	{	return instances_.end(); }

	//! Searchs the instances which bounding boxes intersect a specific bounding box, using the R Tree.
	virtual bool search(const TeBox& b, vector<InstanceType* >& result);

	//! Fills the ST instance set from a layer or theme, using the given filled parameters
	virtual bool build(bool loadGeometries=false, bool loadAllAttributes=true, vector<string> attrNames=vector<string>(), int slide=-1); 

	//! Fills the ST instance set from a layer or theme, using the given filled parameters
	virtual bool build(TeGroupingAttr& groupAttr, bool loadGeometries=false, int slide=-1); 

	
	/*! \class propertyIterator
		\brief This class implements an iterator concept over the instance set.
		
		A structure that allows the traversal ST element set
		in a similar way as the STL iterators. This iterator passes over each 
		ST instance of the ST element set and returns a specific numerical property.
	*/
	class propertyIterator
	{
		public:
			//! Constructor
			propertyIterator (TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>* setIt,
				typename std::vector<InstanceType>::iterator it,  
				const string& attrName):
				attrName_(attrName), attrIndex_(-1), it_(it), setIt_(setIt)
				{
					attrIndex_ = setIt_->getAttributeIndex(attrName);
				}

			//! Constructor
			propertyIterator (TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>* setIt,
				typename std::vector<InstanceType>::iterator it,
				const int& attrIndex):
				attrName_ (""), attrIndex_(attrIndex), it_(it), setIt_(setIt)
				{ }

			//! Constructor
			propertyIterator (typename vector<InstanceType>::iterator it,
				const int& attrIndex):
				attrName_ (""), attrIndex_(attrIndex), it_(it)
				{}
		
			//! Moves to the next attribute in the set
			propertyIterator& operator++()
			{	
				++it_;
				return (*this);
			}

			//! Returns the current attribute as double
			double operator*()
			{	return (*it_)[attrIndex_]; 	}
			
			//! Sets a new value to the attrIndex_-th attribute of the element pointed by the iterator
			void setValue(const double& val)
			{	it_->setPropertyValue(attrIndex_, Te2String(val)); }

			//! Returns a pointer to the instance set 
			TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>* elemSet()
			{	return setIt_; 	}

			//! Equal operator
			bool operator==(const propertyIterator& rhs) const
			{	return (this->attrIndex_ == rhs.attrIndex_ && this->it_== rhs.it_); }

			//! Unequal operator
			bool operator!=(const propertyIterator& rhs) const
			{	return (this->attrIndex_ != rhs.attrIndex_ || this->it_!= rhs.it_);	}

			//! Deprecated: Returns the numerical property of the current instance.
			bool getProperty(TeProperty& prop)
			{	return (*it_).getProperty(prop, attrIndex_); }
				
		protected:
			string										attrName_;	//<! the numerical property name
			int											attrIndex_;	//<! the numerical property index
			typename vector<InstanceType>::iterator		it_; 
			TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>* setIt_; 

	};

	//! Returns a property iterator to the attribute named "attName" of the first instance in the set.
	propertyIterator begin(const string& attName)
	{
		return propertyIterator(this, instances_.begin(), attName); 
	}

	//! Returns a property iterator to the attIndex-th attribute of the first instance in the set.
	propertyIterator begin(const int& attIndex)
	{
		return propertyIterator(this, instances_.begin(), attIndex); 
	}

	//! Returns a property iterator to the attribute named "attName" of the one past last element of the set
	propertyIterator end(const string& attName)
	{
		return propertyIterator(this, instances_.end(), attName); 
	}

	//! Returns a property iterator to the attIndex-th attribute of the one past last element of the set
	propertyIterator end(const int& attIndex)
	{
		return propertyIterator(this, instances_.end(), attIndex); 
	}
};

template<typename GeometryType, typename TimeType, typename InstanceType>
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::TeBaseSTInstanceSet() :
	  theme_(0),  layer_(0), rTree_(0)
{ 
	TeAttributeList* att = new TeAttributeList();
	attrList_ = TeSharedPtr<TeAttributeList>(att);
}


template<typename GeometryType, typename TimeType, typename InstanceType>
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::TeBaseSTInstanceSet(TeTheme* theme, const TeAttributeList& attrList) 
{
	layer_ = 0;
	theme_ = 0;
	rTree_ = 0;
	TeAttributeList* att = new TeAttributeList(attrList);
	attrList_ = TeSharedPtr<TeAttributeList>(att);
	if(!theme)
		return;
	theme_ = theme;
	rTree_ = new TeSAM::TeRTree<int>(theme_->box());
}

template<typename GeometryType, typename TimeType, typename InstanceType>
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::TeBaseSTInstanceSet(TeLayer* layer, const TeAttributeList& attrList) 
{
	layer_ = 0;
	theme_ = 0;
	rTree_ = 0;
	TeAttributeList* att = new TeAttributeList(attrList);
	attrList_ = TeSharedPtr<TeAttributeList>(att);
	if(!layer)
		return;
	layer_ = layer;
	rTree_ = new TeSAM::TeRTree<int>(layer_->box());
}


template<typename GeometryType, typename TimeType, typename InstanceType>
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::TeBaseSTInstanceSet(const TeBox& box, const TeAttributeList& attrList)
{
	layer_ = 0;
	theme_ = 0;
	rTree_ = 0;
	box_ = box;
	TeAttributeList* att = new TeAttributeList(attrList);
	attrList_ = TeSharedPtr< TeAttributeList >(att);
	if(!box_.isValid())
		return;
	rTree_ = new TeSAM::TeRTree<int>(box);
}

template<typename GeometryType, typename TimeType, typename InstanceType>
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::TeBaseSTInstanceSet(const TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>& other) 
{
	instances_ = other.instances_;		
	minTime_ = other.minTime_;
	maxTime_ = other.maxTime_;
	attrList_ = other.attrList_;
	theme_ = other.theme_;	
	layer_ = other.layer_;	
	box_ = other.box_;
	objectIdToInstances_ = other.objectIdToInstances_;
	timeToInstances_ = other.timeToInstances_;
	sliceToInstances_ = other.sliceToInstances_;
	rTree_ = 0;
		
	if(other.rTree_)
	{
		//Operador de copia nao implementado!
		//rTree_ = new TeSAM::TeRTree<int>(other.rTree_->getBox());
		//*rTree_ = *other.rTree_; 
	}
}

template<typename GeometryType, typename TimeType, typename InstanceType>
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::~TeBaseSTInstanceSet() 
{
	if(rTree_)
		delete rTree_;
}

template<typename GeometryType, typename TimeType, typename InstanceType> TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>& 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::operator= (const TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>& other)
{
	if ( this != &other )
	{
		instances_ = other.instances_;		
		minTime_ = other.minTime_;
		maxTime_ = other.maxTime_;
		attrList_ = other.attrList_;
		theme_ = other.theme_;	
		layer_ = other.layer_;	
		box_ = other.box_;
		objectIdToInstances_ = other.objectIdToInstances_;
		timeToInstances_ = other.timeToInstances_;
		sliceToInstances_ = other.sliceToInstances_;
		if(rTree_)
			delete rTree_;
		rTree_ = 0;
		
		if(other.rTree_)
		{
			//Operador de copia nao implementado!
			//rTree_ = new TeSAM::TeRTree<int>(other.rTree_->getBox());
			//*rTree_ = *other.rTree_; 
		}
	}
	return *this;
}

template<typename GeometryType, typename TimeType, typename InstanceType> void 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setAttributeList(TeAttributeList& attrs)
{	
	if(!attrList_.isActive())
		return;
	
	attrList_->clear();
	TeAttributeList::iterator it = attrs.begin();
	while(it!=attrs.end())
	{
		attrList_->push_back(*it);
		++it;
	}
}

template<typename GeometryType, typename TimeType, typename InstanceType> int
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getAttributeIndex(const string& attrName) 
{
	string newName = TeConvertToUpperCase(attrName); 
	size_t pos = newName.find(".", 0, 1);
	if (pos != string::npos)
		newName = TeConvertToUpperCase(attrName.substr(pos+1));
	
	for(unsigned int i=0; i<attrList_->size(); ++i)
	{
		string s = TeConvertToUpperCase((*attrList_)[i].rep_.name_); 
		if((s == TeConvertToUpperCase(attrName)) || (s == newName))
			return i;
	}
	return -1;
}



template<typename GeometryType, typename TimeType, typename InstanceType> InstanceType* 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstance(const unsigned int& index)
{
	if(index<instances_.size())
		return &(instances_[index]);

	return 0;
}

template<typename GeometryType, typename TimeType, typename InstanceType> InstanceType* 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstance(const string& objectId, const int& slice)
{
	map<string, vector<int> >::iterator itObj = objectIdToInstances_.find(objectId);
	if(itObj==objectIdToInstances_.end())
		return 0;

	if(slice<0)
	{
		//The slice is not considered
		vector<int>::iterator it = itObj->second.begin();
		if(it==itObj->second.end())
			return 0;
		return getSTInstance(*it);
	}

	map<int, vector<int> >::iterator itSlice = sliceToInstances_.find(slice);
	if(itSlice==sliceToInstances_.end())
		return 0;

	vector<int>::iterator it = itObj->second.begin();
	while(it != itObj->second.end())
	{
		vector<int>::iterator it2;
		it2 = find(itSlice->second.begin(), itSlice->second.end(), *it);
		if(it2!=itSlice->second.end())
			return getSTInstance(*it2);
		++it;
	}
	return 0;		
}

template<typename GeometryType, typename TimeType, typename InstanceType> InstanceType* 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstance(const string& objectId, TimeType& time, const int& slice)
{
	map<string, vector<int> >::iterator itObj = objectIdToInstances_.find(objectId);
	if(itObj==objectIdToInstances_.end())
		return 0;

	typename map<TimeType, vector<int> >::iterator itTime = timeToInstances_.find(time);
	if(itTime==timeToInstances_.end())
		return 0;
	
	if(slice<0)
	{
		//The slice is not considered
		vector<int>::iterator it = itObj->second.begin();
		while(it != itObj->second.end())
		{
			vector<int>::iterator it2;
			it2 = find(itTime->second.begin(), itTime->second.end(), *it);
			if(it2!=itTime->second.end())
				return getSTInstance(*it2);
			++it;
		}
		return 0;
	}
	
	map<int, vector<int> >::iterator itSlice = sliceToInstances_.find(slice);
	if(itSlice==sliceToInstances_.end())
		return 0;

	vector<int>::iterator it = itObj->second.begin();
	while(it != itObj->second.end())
	{
		vector<int>::iterator it2, it3;
		it2 = find(itTime->second.begin(), itTime->second.end(), *it);
		it3 = find(itSlice->second.begin(), itSlice->second.end(), *it);
		if((it2!=itTime->second.end()) && (it3!=itSlice->second.end()))
			return getSTInstance(*it2);
		++it;
	}
	return 0;
}

template<typename GeometryType, typename TimeType, typename InstanceType> void
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstances(vector<InstanceType* >& set, const string& objectId, const int& slice)
{
	set.clear();
	map<string, vector<int> >::iterator itObj = objectIdToInstances_.find(objectId);
	if(itObj==objectIdToInstances_.end())
		return;

	vector<int>::iterator it = itObj->second.begin();
	while(it != itObj->second.end())
	{
		InstanceType* inst = getSTInstance(*it);
		if((slice<0) || (inst->getSlice()==slice))
			set.push_back(inst);
		++it;
	}
	return;
}

template<typename GeometryType, typename TimeType, typename InstanceType> void
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstances(vector<InstanceType* >& set, const string& objectId, TimeType& time, const int& slice)
{
	set.clear();
	map<string, vector<int> >::iterator itObj = objectIdToInstances_.find(objectId);
	if(itObj==objectIdToInstances_.end())
		return;

	vector<int>::iterator it = itObj->second.begin();
	while(it != itObj->second.end())
	{
		InstanceType* inst = getSTInstance(*it);
		if(((slice<0) || (inst->getSlice()==slice)) && (inst->getTime()== time))
			set.push_back(inst);
		++it;
	}
	return;
}
	
template<typename GeometryType, typename TimeType, typename InstanceType> void
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstances(vector<InstanceType* >& set, TimeType& time, const int& slice)
{
	set.clear();
	typename map<TimeType, vector<int> >::iterator itTime = timeToInstances_.find(time);
	if(itTime==timeToInstances_.end())
		return;

	vector<int>::iterator it = itTime->second.begin();
	while(it != itTime->second.end())
	{
		InstanceType* inst = getSTInstance(*it);
		if((slice<0) || (inst->getSlice()==slice))
			set.push_back(inst);
		++it;
	}
	return;

}

template<typename GeometryType, typename TimeType, typename InstanceType> void
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getSTInstances(vector<InstanceType* >& set, const int& slice)
{
	set.clear();
	map<int, vector<int> >::iterator itSlice = sliceToInstances_.find(slice);
	if(itSlice==sliceToInstances_.end())
		return;

	vector<int>::iterator it = itSlice->second.begin();
	while(it != itSlice->second.end())
	{
		set.push_back(getSTInstance(*it));
		++it;
	}
	return;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::insertSTInstance (InstanceType& inst)
{
	inst.setAttrList(attrList_);
	instances_.push_back(inst);
	int index = (instances_.size()-1);
	vector<int> aux;
	aux.push_back(index);
	
	//object identity information
	map<string, vector<int> >::iterator itObj = objectIdToInstances_.find(inst.getObjectId());
	if(itObj!=objectIdToInstances_.end())
		itObj->second.push_back(index);
	else
		objectIdToInstances_[inst.getObjectId()] = aux;

	//time information
	if(inst.isTimeValid())
	{
		typename map<TimeType, vector<int> >::iterator itTime = timeToInstances_.find(inst.getTime());
		if(itTime!=timeToInstances_.end())
			itTime->second.push_back(index);
		else
			timeToInstances_[inst.getTime()] = aux;
	}

	//slice information
	if(inst.getSlice()>=0)
	{
		map<int, vector<int> >::iterator itSlice = sliceToInstances_.find(inst.getSlice());
		if(itSlice!=sliceToInstances_.end())
			itSlice->second.push_back(index);
		else
			sliceToInstances_[inst.getSlice()] = aux;
	}
	
	//insert in the RTree
	TeBox b = inst.getGeometries().box();
	if(b.isValid())
	{
		if(rTree_)
			rTree_->insert(b, index);
		updateBox(box_, b);
	}
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> int 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::numSTInstance()
{
	return instances_.size();
}
		
template<typename GeometryType, typename TimeType, typename InstanceType> int 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::numSTInstance(const string& objectId)
{
	map<string, vector<int> >::iterator it = objectIdToInstances_.find(objectId);
	if(it==objectIdToInstances_.end())
		return 0;
	return it->second.size();

}

template<typename GeometryType, typename TimeType, typename InstanceType> int 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::numSTInstance(const int& slice)
{
	map<int, vector<int> >::iterator it = sliceToInstances_.find(slice);
	if(it==sliceToInstances_.end())
		return 0;
	return it->second.size();
}

template<typename GeometryType, typename TimeType, typename InstanceType> int 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::numSTInstance(TimeType& time)
{
	typename map<TimeType, vector<int> >::iterator it = timeToInstances_.find(time);
	if(it==timeToInstances_.end())
		return 0;
	return it->second.size();
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setGeometry(const string& object_id, GeometryType& geom, const int& slice)
{
	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	aux->setGeometry(geom);
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setGeometry(const string& object_id, GeometryType& geom, TimeType& time, const int& slice)
{
	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	aux->setGeometry(geom);
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getGeometry(const string& object_id, GeometryType& geom, const int& slice)
{
	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	geom = aux->getGeometries(); 
	return true;

}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getGeometry(const string& object_id, GeometryType& geom, TimeType& time, const int& slice)
{
	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	geom = aux->getGeometries(); 
	return true;
}


template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getPropertyVector (const string& object_id, TePropertyVector& propVec, const int& slice)
{
	propVec.clear();
	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	
	for(unsigned int i=0; i<aux->getProperties().size(); ++i)
	{
		TeProperty prop;
		prop.value_ = aux->getProperties()[i];
		if(i<attrList_->size())
			prop.attr_ = (*attrList_)[i];
		propVec.push_back(prop);
	}
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getPropertyVector (const string& object_id, 
	TePropertyVector& propVec, TimeType& time, const int& slice)
{
	propVec.clear();
	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	
	for(unsigned int i=0; i<aux->getProperties().size(); ++i)
	{
		TeProperty prop;
		prop.value_ = aux->getProperties()[i];
		if(i<attrList_->size())
			prop.attr_ = (*attrList_)[i];
		propVec.push_back(prop);
	}
	return true;

}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setProperties (const string& object_id, const vector<string>& values, const int& slice)
{
	//the number of attributes in each instance must be equal to the number of attibutes
	//in the attribute list. 
	if(values.size() != attrList_->size())
		return false;

	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	aux->setProperties(values);
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setProperties (const string& object_id, const vector<string>& values, TimeType& time, const int& slice)
{
	//the number of attributes in each instance must be equal to the number of attibutes
	//in the attribute list. 
	if(values.size() != attrList_->size())
		return false;

	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	aux->setProperties(values);
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getProperties (const string& object_id, vector<string>& values, const int& slice)
{
	values.clear();
	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	values = aux->getProperties();
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getProperties (const string& object_id, vector<string>& values, TimeType& time, const int& slice)
{
	values.clear();
	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	values = aux->getProperties();
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setAttributeValue (const string& object_id, const string& attr_name,  const string& val, const int& slice)
{
	int index = this->getAttributeIndex(attr_name);
	return this->setAttributeValue(object_id, index, val, slice);
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setAttributeValue (const string& object_id, const string& attr_name, const string& val, TimeType& time, const int& slice)
{
	int index = this->getAttributeIndex(attr_name);
	return this->setAttributeValue(object_id, index, val, time, slice);
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setAttributeValue (const string& object_id, const int& i, const string& val, const int& slice)
{
	if(i<0 || i>=(int)attrList_->size()) //if there is not this attribute, return false 
		return false;
	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	aux->setPropertyValue(i, val); 
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::setAttributeValue (const string& object_id, const int& i, const string& val, TimeType& time, const int& slice)
{
	if(i<0 || i>=(int)attrList_->size()) //if there is not this attribute, return false 
		return false;
	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	aux->setPropertyValue(i, val); 
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getAttributeValue (const string& object_id, const string& attr_name,  string& val, const int& slice)
{
	int index = this->getAttributeIndex(attr_name);
	return this->getAttributeValue(object_id, index, val, slice);
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getAttributeValue (const string& object_id, const string& attr_name,  string& val, TimeType& time, const int& slice)
{
	int index = this->getAttributeIndex(attr_name);
	return this->getAttributeValue(object_id, index, val, time, slice);
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getAttributeValue (const string& object_id, const int& i, string& val, const int& slice)
{
	if(i<0 || i>=(int)attrList_->size())
		return false;
	InstanceType* aux = this->getSTInstance(object_id, slice);
	if(!aux)
		return false;
	val = aux->getProperties()[i];
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getAttributeValue (const string& object_id, const int& i, string& val, TimeType& time, const int& slice)
{
	if(i<0 || i>=(int)attrList_->size())
		return false;
	InstanceType* aux = this->getSTInstance(object_id, time, slice);
	if(!aux)
		return false;
	val = aux->getProperties()[i];
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::addProperty(TeAttributeRep& attr, const string& defaultValue)
{
	//verify if there is this attribute
	if(getAttributeIndex(attr.name_)>=0) 
		return true;

	TeAttribute at;
	at.rep_ = attr;
	attrList_->push_back(at);
	typename vector<InstanceType>::iterator it = instances_.begin();	
	
	//the number of attributes in each instance must be equal to the number of attibutes
	//in the attribute list.
	if((attrList_->size()-1) != it->getProperties().size())
		return false;
	while(it!=instances_.end())
	{	
		it->addPropertyValue(defaultValue);
		++it;
	}
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::addProperty(TeAttribute& attr)
{
	//verify if there is this attribute
	if(getAttributeIndex(attr.rep_.name_)<0) 
		attrList_->push_back(attr);
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::addProperty(const string& object_id, TeProperty& prop, const int& slice)
{
	int index = this->getAttributeIndex(prop.attr_.rep_.name_); 
	if(index<0)
	{
		//adds this new attribute in the attr list and in all instances
		attrList_->push_back(prop.attr_);
		typename vector<InstanceType>::iterator it =	instances_.begin();		
		while(it!=instances_.end())
		{
			if( (object_id == it->getObjectId()) && ((slice<0) || (slice==it->getSlice())))  
				it->addPropertyValue(prop.value_);
			else
				it->addPropertyValue(string(""));
			++it;
		}
		return true;
	}
	
	//Sets this attribute value
	typename vector<InstanceType>::iterator it =	instances_.begin();		
	while(it!=instances_.end())
	{
		if( (object_id == it->getObjectId()) && ((slice<0) || (slice==it->getSlice())))  
			it->setPropertyValue(index, prop.value_);
		++it;
	}
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::addProperty(const string& object_id, TeProperty& prop, TimeType& time, const int& slice)
{
	int index = this->getAttributeIndex(prop.attr_.rep_.name_); 
	if(index<0)
	{
		//adds this new attribute in the attr list and in all instances
		attrList_->push_back(prop.attr_);
		typename vector<InstanceType>::iterator it =	instances_.begin();		
		while(it!=instances_.end())
		{
			if( (object_id == it->getObjectId()) && 
				(time == it->getTime()) && 
				((slice<0) || (slice==it->getSlice())))  
				it->addPropertyValue(prop.value_);
			else
				it->addPropertyValue(string(""));
			++it;
		}
		return true;
	}
	
	//Sets this attribute value
	typename vector<InstanceType>::iterator it =	instances_.begin();		
	while(it!=instances_.end())
	{
		if( (object_id == it->getObjectId()) && 
			(time == it->getTime()) && 
			((slice<0) || (slice==it->getSlice())))  
			it->setPropertyValue(index, prop.value_);
		++it;
	}
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> void
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::removeProperty(TeAttributeRep& attr)
{
	//Remove of the attribute list
	string newName = TeConvertToUpperCase(attr.name_); 
	size_t pos = attr.name_.find(".", 0, 1);
	if (pos != string::npos)
		newName = TeConvertToUpperCase(attr.name_.substr(pos+1));
	
	unsigned int index = 0;
	TeAttributeList::iterator it = attrList_->begin();
	while(it!=attrList_->end())
	{
		string s = TeConvertToUpperCase(it->rep_.name_); 
		if((s == TeConvertToUpperCase(attr.name_)) || (s == newName))
		{
			attrList_->erase(it);
			break;
		}
		++it;
		++index;
	}

	//Remove of all instances
	typename vector<InstanceType>::iterator it2 = instances_.begin();		
	while(it2!=instances_.end())
	{
		it2->removePropertyValue(index);
		++it;
	}
}

template<typename GeometryType, typename TimeType, typename InstanceType> TeBox& 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::getBox()
{
	if(box_.isValid())
		return box_;

	if (instances_.size() <= 0)
		return box_;

	typename TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::iterator it = this->begin();
	
	while (it != this->end())
	{
		updateBox(box_,it->getGeometries().box());
		++it;
	}
	return box_;
}

template<typename GeometryType, typename TimeType, typename InstanceType> void 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::clear()
{
	instances_.clear();
	attrList_->clear();
	objectIdToInstances_.clear();
	timeToInstances_.clear();
	sliceToInstances_.clear();
	if(rTree_)
		rTree_->clear(); 
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::search(const TeBox& b, vector<InstanceType* >& result)
{
	if(!rTree_ )
		return false;

	vector<int> intResult;
	rTree_->search(b, intResult);
	for(unsigned int i=0; i<intResult.size(); ++i)
		result.push_back(getSTInstance(intResult[i])); 
	return true;
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::build(bool loadGeometries, bool loadAllAttributes, vector<string> attrNames, int slide)
{
	TeQuerierParams param;
	if(theme_)
		param.setParams(theme_);
	else if(layer_)
		param.setParams(layer_);
	else
		return false;

	param.setFillParams(loadGeometries, loadAllAttributes, attrNames);
	TeQuerier querier(param);
	return(buildImpl(&querier, slide));
}

template<typename GeometryType, typename TimeType, typename InstanceType> bool 
TeBaseSTInstanceSet<GeometryType, TimeType, InstanceType>::build(TeGroupingAttr& groupAttr, bool loadGeometries, int slide)
{
	TeQuerierParams param;
	if(theme_)
		param.setParams(theme_);
	else if(layer_)
		param.setParams(layer_);
	else
		return false;

	param.setFillParams(loadGeometries, groupAttr);
	TeQuerier querier(param);
	return(buildImpl(&querier, slide));
}


#endif

