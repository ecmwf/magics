/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeProxMatrixImplementation.h
    \brief This file contains structures and definitions about different representations of proximity matrix 
*/

#ifndef TeProxMatrixImplementation_H
#define TeProxMatrixImplementation_H

#include "TeNeighbours.h"
#include "graph.h"

//! An abstract class to represent proximity matrix   
class TL_DLL TeProxMatrixImplementation  
{
protected:
	//! Type of the representation
	TeGPMImplementation type_;
	
	//! Empty constructor 
	TeProxMatrixImplementation(const TeGPMImplementation& type): type_(type)
		{}

public:
	//! Verify if two objects are neighbour or connected
	bool isConnected (const string& object_id1, const string& object_id2) 
	{
		TeProxMatrixAttributes attr; 
		return getConnectionAttributes (object_id1, object_id2, attr);
	}

	//! Connect two objects
	virtual void connectObjects (const string& , const string& , const TeProxMatrixAttributes& ) = 0;

	//! Disconnect two objects
	virtual bool disconnectObjects (const string& , const string& ) { return false; }

	//! Remove an object
	virtual bool removeObject (const string& ) { return false; }

	//! Get connection attributes
	virtual bool getConnectionAttributes (const string&, const string&, TeProxMatrixAttributes& )= 0;

	//! Set connection attributes
	virtual bool setConnectionAttributes (const string&, const string&, const TeProxMatrixAttributes&) =0;

	//! Get the neighbours of an object
	virtual bool getNeighbours (const string& , TeNeighbours& )=0;

	//! Get the obj-th neighbour of an object 
	virtual bool getNeighbours (int, string& , TeNeighbours& )=0;	

	//! Get the neighbours of an object
	virtual bool getNeighboursNeighbours (const string&, TeNeighbours&, int /* max_order */ = 2)=0;

	//! Return the number of objects
	virtual int  numberOfObjects()=0;

	//! Return the type of the representation 
	TeGPMImplementation type() {return type_;}

	//! Create a copy
	virtual TeProxMatrixImplementation* createCopy ()=0;
	
	//! Verify if is equal
	virtual bool isEqual (TeProxMatrixImplementation& other)
	{
		if (type_ == other.type_) 
			return true; 
		return false;
	}

	//! Equal operator
	virtual bool operator== (const TeProxMatrixImplementation& ) const { return false; } 
		
	//! Save the proximity matrix in a text file
	virtual bool saveTextFile (const string&, map<string, string>*)=0;

	//! Save the proximity matrix in a GAL format text file 
	virtual bool saveGALFile (const string&, map<string, string>*)=0;

	//! Save the proximity matrix in a GWT format text file 
	virtual bool saveGWTFile (const string&, map<string, string>*)=0;

	//! Save the proximity matrix in a text file
	virtual bool saveTextFile (const string&, vector<string>*)=0;

	//! Save the proximity matrix in a GAL format text file 
	virtual bool saveGALFile (const string&, vector<string>*)=0;

	//! Save the proximity matrix in a GWT format text file 
	virtual bool saveGWTFile (const string&, vector<string>*)=0;

	//! Destructor
	virtual ~TeProxMatrixImplementation(){}
};


//! A class to represent proximity matrix utilising the Breymann graph
class TL_DLL TeProxMatrixGraphBreymann : public  TeProxMatrixImplementation
{
private:
	br_stl::Graph<string, TeProxMatrixAttributes> graph_;

	typedef map<string, int> Object_id_map_type;
	Object_id_map_type  map_;

	typedef Object_id_map_type::iterator map_iterator;
	typedef Object_id_map_type::const_iterator map_const_iterator;

	bool getNeighboursNeighbours (const string& object_id, TeNeighbours& neigh, int max_order = 2, int current_order = 1);

public:
	//! Empty constructor - graph must be directed then the graph constructor receive true 
	TeProxMatrixGraphBreymann (); 
																	 
	//! Copy constructor
	TeProxMatrixGraphBreymann(TeProxMatrixGraphBreymann& imp);

	//! Connect two objects
	virtual void connectObjects (const string& object_id1, const string& object_id2, const TeProxMatrixAttributes& attr);
	
	//! Get connection attributes 
	virtual bool getConnectionAttributes (const string& object_id1, const string& object_id2, TeProxMatrixAttributes& attr);
	
	//! Set connection attributes 
	virtual bool setConnectionAttributes (const string& object_id1, const string& object_id2, const TeProxMatrixAttributes& attr);

	//! Get the neighbours of an object
	virtual bool getNeighbours (const string& object_id, TeNeighbours& neigh);

	//! Get the obj-th neighbour of an object 
	virtual bool getNeighbours (int obj, string& object_id, TeNeighbours& neigh); 
	
	//! Get the neighbours of an object
	virtual bool getNeighboursNeighbours (const string& object_id, TeNeighbours& neigh, int max_order = 2);

	//! Return the number of the objects
	virtual int  numberOfObjects () {return graph_.size();}

	//! Create a copy
	virtual TeProxMatrixImplementation* createCopy ();

	//! Verify if is equal
	virtual bool isEqual (TeProxMatrixImplementation& other) 
	{
		if (type_ == other.type()) 
			return (*this == (TeProxMatrixGraphBreymann&)other); 
		return false;
	}

	//! Assignment operator 
	TeProxMatrixGraphBreymann& operator= (TeProxMatrixGraphBreymann& imp); 
	
	//! Destructor
	virtual ~TeProxMatrixGraphBreymann() {}

	//! Save the proximity matrix in a text file 
	virtual bool saveTextFile (const string& name, map<string, string>* ids=0);

	//! Save the proximity matrix in a GAL text file 
	virtual bool saveGALFile (const string& name, map<string, string>* ids=0);

	//! Save the proximity matrix in a GWT text file 
	virtual bool saveGWTFile (const string& name, map<string, string>* ids=0);

	//! Save the proximity matrix in a text file
	virtual bool saveTextFile (const string& name, vector<string>* ids);

	//! Save the proximity matrix in a GAL format text file 
	virtual bool saveGALFile (const string& name, vector<string>* ids);

	//! Save the proximity matrix in a GWT format text file 
	virtual bool saveGWTFile (const string& name, vector<string>* ids);
};


//! An abstract factory of proximity matrix representations  
class TL_DLL TeProxMatrixAbstractFactory  
{
public:
	static TeProxMatrixImplementation* MakeConcreteImplementation (const TeGPMImplementation& impl_type = TeGraphBreymann)
	{
		if (impl_type == TeGraphBreymann) 
				return new TeProxMatrixGraphBreymann();

		return new TeProxMatrixGraphBreymann();
	}
	
};

/*! \example createProximityMatrix.cpp
	This is an example of how to  how to create a proximity matrix from a  Spatial Temporal Element Set (STElementSet)
 */
#endif
