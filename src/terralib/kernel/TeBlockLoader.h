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
/*! \file TeBlockLoader.h
    \brief This file contains structures and definitions for loading data blocks.
*/
#ifndef  __TERRALIB_INTERNAL_BLOCKLOADER_H
#define  __TERRALIB_INTERNAL_BLOCKLOADER_H

/*
** ----------------------------------------------------------------------------
** Includes:
*/
#include "TeDatabase.h"
#include "TeBox.h"
#include "TeTime.h"
#include <string>
#include <vector>

/*
** ----------------------------------------------------------------------------
** Definitions:
*/

typedef struct 
{
    TeDatabase* db;

    std::string table_name;
    TeBox selection_box;

    bool load_all_attrs;
	std::vector<std::string> attrs;
    std::string attrs_rest;

    std::set<int> dont_load_ids;
	
} TeBlockLoaderParams;

class TL_DLL TeBlockLoader
{
public:

    /// Default constructor.
	TeBlockLoader(const TeBlockLoaderParams& params);

    /// Virtual destructor.
    virtual ~TeBlockLoader();

	bool loadBlocks();
		
	bool fetchNext();

    int getID();
	void getSpatialData(unsigned char* &data, long& size);

    char* getData(const std::string& name);
    int getInt(const std::string& name);
    double getDouble(const std::string& name);
    bool getBool(const std::string& name);
    TeTime getDate(const std::string& name);
    std::string getDateAsString(const std::string& name);

protected:

    TeBlockLoaderParams params_;
    TeDatabasePortal* portal_;

};

#endif // __TERRALIB_INTERNAL_BLOCKLOADER_H

/*
** ----------------------------------------------------------------------------
** End:
*/