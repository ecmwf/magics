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
/*! \file TeMetaModelCache.h
    \brief This file contains structures and definitions about TerraLib database meta model.
*/
#ifndef  __TERRALIB_INTERNAL_METAMODELCACHE_H
#define  __TERRALIB_INTERNAL_METAMODELCACHE_H

#include "TeLayer.h"
#include "TeAbstractTheme.h"
#include "TeView.h"
#include "TeProject.h"

//! A class for storing TerraLib's metamodel objects (TeTheme, TeLayer, ...)
/*!
	An instance of this class keeps track of 
	meta objects stored in a TerraLib Database.
	This instance can be shared by all database connections.

  \sa TeTheme, TeView, TeLayer, TeAttribute
*/
class TL_DLL TeMetaModelCache
{
	public: 


        //! Empty constructor
        TeMetaModelCache()
		{}

        //! Destructor
        ~TeMetaModelCache()
		{ clear(); }

		//! Clears metadata
		void clear();

		//! Returns the  map of layers in the database
		TeLayerMap&	layerMap () 
		{ return layerMap_; }

		//! Returns the  map of views in the database
		TeViewMap&	viewMap () 
		{ return viewMap_; }

		//! Returns the  map of themes in the database
		TeThemeMap&	themeMap () 
		{ return themeMap_; }

		//! Returns the  map of invalid themes in the database
		TeThemeMap&	invalidThemeMap () 
		{ return invalidThemeMap_; }

		//! Returns the  map of projects in the database
		TeProjectMap&  projectMap ()
		{ return projectMap_; }

		//! Returns the  map of legends in the database
		TeLegendEntryMap& legendMap () 
		{ return legendMap_; }

		//! Returns the set of relations between tables
		multiset<int>& relationMSet () 
		{ return relationMSet_; }

		map<int, map<string, string> >& mapThemeAlias()
		{return mapThemeAlias_;}

    private:

        //! Copy constructor not allowed.
        TeMetaModelCache(const TeMetaModelCache& rhs);

		//! Assignment operator not allowed.
        TeMetaModelCache& operator=(const TeMetaModelCache& rhs);

	private:

		TeLayerMap			layerMap_;			//!< layer map	  
        TeViewMap			viewMap_;			//!< view map	  
        TeThemeMap			themeMap_;			//!< theme map	
		TeThemeMap			invalidThemeMap_;	//!< invalid theme map
        TeLegendEntryMap	legendMap_;			//!< view map
        TeProjectMap		projectMap_;		//!< project map
        multiset<int>		relationMSet_;		//!< multiset of relations between tables
		/*! \brief An associative container from theme identifier (int)
		           to legend alias (map<string, string>). The theme alias
                   is another container, where key is a string with a column name
                   used in a group that maps to a legend alias (string value).
		 */
		map<int, map<string, string> > mapThemeAlias_;
};



#endif


