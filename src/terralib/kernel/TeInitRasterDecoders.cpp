/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2004 INPE and Tecgraf/PUC-Rio.

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

#include "TeInitRasterDecoders.h"
#include "TeDecoderMemoryMap.h"
#include "TeDecoderMemory.h"
#include "TeDecoderSmartMem.h"

#define NO_TETIFF
#define NO_TEJPEG

#ifndef NO_TETIFF //FAMI
#include "TeDecoderTIFF.h"
#endif

#ifndef NO_TEJPEG //FAMI
#include "TeDecoderJPEG.h"
#endif

#include "TeDecoderDatabase.h"
#include "TeDecoderFile.h"
#include "TeDecoderSPR.h"
#include "TeDecoderASCIIGrid.h"

#include <map>

void 
TeInitRasterDecoders()
{
	static bool TeRasterDecoderFactoryInitalized__ = false;

	if (!TeRasterDecoderFactoryInitalized__)
	{
		TeRasterDecoderFactoryInitalized__ = true;

		static TeDecoderDatabaseFactory theDecoderDatabaseFactory("DB");

		static TeDecoderMemoryFactory theDecoderMemoryFactory("MEM");

		static TeDecoderSmartMemFactory teDecoderSmartMemFactory("SMARTMEM");

		static TeDecoderMemoryMapFactory theDecoderMemoryMapFactory("MEMMAP");

#ifndef NO_TETIFF //FAMI
		static TeDecoderTIFFFactory theDecoderTIFFFactory("TIF");
#endif

		static TeDecoderSPRFactory theDecoderSPRFactory("SPR");

		static TeDecoderASCIIGridFactory theDecoderASCIIGridFactory("ASCIIGRID");

#ifndef NO_TEJPEG //FAMI
		static TeDecoderJPEGFactory theDecoderJPEGFactory("JPEG");
#endif

#ifdef WIN32
		static TeDecoderFileFactory theDecoderFileFactory("RAW");
#endif	
   }
}
