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

/*
** ----------------------------------------------------------------------------
** Includes:
*/

#include "TeBlockLoader.h"

/*
** ----------------------------------------------------------------------------
** Definitions:
*/

/*
** ----------------------------------------------------------------------------
** Methods Implementation:
*/

TeBlockLoader::TeBlockLoader(const TeBlockLoaderParams& params)
{
    params_ = params;
}

TeBlockLoader::~TeBlockLoader()
{
}

bool
TeBlockLoader::loadBlocks()
{
    portal_ = params_.db->getPortal();
    portal_->freeResult();

    if (!portal_)
    {
        return false; // Error opening portal.
    }

    // Set columns to be retrieved.
    std::string cols;
    if (params_.load_all_attrs)
    {
        cols = "*";
    }
    else
    {
        cols = "";
        std::vector<std::string>::iterator it = params_.attrs.begin();
        std::vector<std::string>::iterator end = params_.attrs.end();
        while(it != end)
        {
            cols.append(*(it++));
            if (it != end)
            {
                cols.append(", ");
            }
        }
    }

    // Set spatial restrictions.
    std::string rest;
    rest += " NOT( ";
    rest += "lower_x > " + Te2String(params_.selection_box.x2_, 12) + " OR ";
    rest += "upper_x < " + Te2String(params_.selection_box.x1_, 12) + " OR ";
    rest += "lower_y > " + Te2String(params_.selection_box.y2_, 12) + " OR ";
    rest += "upper_y < " + Te2String(params_.selection_box.y1_, 12) + " )";

    // Set block id restrictions.
    if (params_.dont_load_ids.size() > 0)
    {
        std::set<int>::iterator it = params_.dont_load_ids.begin();
        std::set<int>::iterator end = params_.dont_load_ids.end();
        while (it != end)
        {
            rest += " AND block_id <> " + Te2String(*(it++));
        }
    }

    // Set other restrictions.
    if (params_.attrs_rest.length() > 0)
    {
        rest += " AND (" + params_.attrs_rest + ")";
    }

    // Mount SQL query.
    std::string sql = "SELECT " + cols + " FROM " + params_.table_name + " WHERE " + rest;

    return portal_->query(sql);

}
		
bool
TeBlockLoader::fetchNext()
{
    return portal_->fetchRow();
}

int
TeBlockLoader::getID()
{
    return portal_->getInt("block_id");
}

void
TeBlockLoader::getSpatialData(unsigned char* &data, long& size)
{
    portal_->getBlob("spatial_data", data, size);
}

char*
TeBlockLoader::getData(const std::string& name)
{
    return portal_->getData(name);
}

int
TeBlockLoader::getInt(const std::string& name)
{
    return portal_->getInt(name);
}

double
TeBlockLoader::getDouble(const std::string& name)
{
    return portal_->getDouble(name);
}

bool
TeBlockLoader::getBool(const std::string& name)
{
    return portal_->getBool(name);
}

TeTime
TeBlockLoader::getDate(const std::string& name)
{
    return portal_->getDate(name);
}

std::string
TeBlockLoader::getDateAsString(const std::string& name)
{
    return portal_->getDateAsString(name);
}

/*
** ----------------------------------------------------------------------------
** End:
*/
