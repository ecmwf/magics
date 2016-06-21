/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MagLog.h"
#include "FortranMagics.h"
#include "XmlMagics.h"
#include "OdaDecoder.h"
/*
 These functions are overwritting empty functions in main library 
*/

extern "C" {

#include <magics_api.h>

Data* calloda(const XmlNode& node)
{
	OdaGeoDecoder* oda = new OdaGeoDecoder();
	oda->set(node);
	return oda;
}

}
