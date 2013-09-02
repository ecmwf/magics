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

#include "TeUtils.h"
#include "TeDefines.h"
#include "TeException.h"
#include "TeAgnostic.h"

#include <cstdlib>
#include <stdio.h>
#include <ctype.h>
#include <cstring> //FAMI

/* The following includes are needed for pysical memory checking */
#if TePLATFORM == TePLATFORMCODE_MSWINDOWS
  #include <windows.h>
  #include <winbase.h>
#elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  #include <unistd.h>
  #include <string.h>
  #include <errno.h>
  
  #if defined( __FreeBSD__ ) || defined( __APPLE_CC__ )
    #include <sys/sysctl.h> /* BSD workaround */
  #endif
#else
  #error "Unsuported plataform for physical memory checking"
#endif


string
Te2String ( const int value )
{
	char name [ TeNAME_LENGTH ];
	sprintf ( name, "%d", value );
	return name;
}

string
Te2String ( const unsigned int value )
{
	char name [ TeNAME_LENGTH ];
	sprintf ( name, "%u", value );
	return name;
}

string
Te2String ( const long value )
{
	char name [ TeNAME_LENGTH ];
	sprintf ( name, "%ld", value );
	return name;
}

string
Te2String ( const unsigned long value )
{
	char name [ TeNAME_LENGTH ];
	sprintf ( name, "%lu", value );
	return name;
}

void
TeTrim(string &str)
{
//empty strings are bad
    if(str.empty())
		return;
//remove left whitespace, \t and \n
	string::iterator it;
    for(it=str.begin();it!=str.end() &&
		((*it) == ' ' ||  (*it) == '\t' || (*it) == '\n');it++);
		str.erase(str.begin(),it);
//empty strings are bad
    if(str.empty())
		return;
//remove right whitespace, \t and \n
	it=str.end();
    for(--it;it!=str.begin() && 
	   ((*it) == ' ' ||  (*it) == '\t' || (*it) == '\n');--it);
		str.erase(++it,str.end());
}

string
TeRemoveSpecialChars ( string& str)
{
	string aux; 
	string::iterator it;
    for(it=str.begin();it!=str.end();it++)
	{
		if (isalnum(*it))
			aux+=(*it);
	}
	return aux;
}

string
Te2String ( const double value, int precision )
{
	char name [ TeNAME_LENGTH ];
	sprintf(name, "%.*f", precision, value );
	return name;
}


string
Te2String ( const double value )
{
	char name [ TeNAME_LENGTH ];
	sprintf ( name, "%e", value ); 
	return name;
}

string 
TeGetExtension ( const char* value )
{
	if (!value)
		return string("");
	string name = string(value);
	int len = strlen ( value );
	unsigned int ip = name.rfind('.');
	if (ip == std::string::npos)
		return "";
	else
		return name.substr(ip+1,len-1);
}

string 
TeGetName ( const char* value )
{
	if (!value)
		return string("");
	string name = string(value);
	int len = strlen ( value );

	int sp = name.rfind('\\')+1;
	int ip = (name.substr(sp,len-1)).rfind('.');
	if (ip>0)
		return name.substr(0,sp+ip);
	else 
		return name;
}

string
TeGetBaseName ( const char* value )
{
	if (!value)
		return string("");
	string name = string(value);
	int ip = name.rfind('.');
	unsigned int is = name.rfind('\\');
	if (is == std::string::npos)
		is = name.rfind('/');
	return name.substr(is+1,ip-is-1);
}

string 
TeGetPath(const char* value)
{
	if (!value)
		return string(".");
	string name = string (value);
	std::string::size_type ip = name.rfind('/');
	if (ip == std::string::npos)
		return string(".");
	else
		return name.substr(0,ip);
}

void
TeConvertToUpperCase ( const string& name, char upName[] )
{
	
	const char* old = name.c_str();

	const char* p;
	int i = 0;

	for( p = old; p < old + strlen ( old) ; p++ )
	{
		if ( islower ( *p  ) )
			upName[i] = toupper( *p );
		else
			upName [i] = *p;
		i++;
	}
	upName[i] = '\0';
}

string
TeConvertToUpperCase (const string &name)
{
	string aux = "";
	for(unsigned int i=0; i < strlen(name.c_str()); i++)
	{
		if((name[i] >= 97) && (name[i] <= 122))
			aux += name[i] - 32;
       else
			aux += name[i];
    }
	return aux;
}


string
TeConvertToLowerCase (const string &name)
{
	string aux = "";
	for(unsigned int i=0; i < strlen(name.c_str()); i++)
	{
		if((name[i] >= 65) && (name[i] <= 90))
			aux += name[i] + 32;
		else
			aux += name[i];
    }
	return aux;
}


//-----------------------------------------------------------------------
//
//      Purpose:        STL split string utility
//      Author:         Paul J. Weiss
//      Extracted from: The Code Project (http://www.codeproject.com/)
//
//------------------------------------------------------------------------

int TeSplitString(const string& input, const string& delimiter, vector<string>& results)
{
   int iPos = 0;
   int newPos = -1;
   int sizeS2 = delimiter.size();
   int isize = input.size();

   if (input.empty())
	   return 0;
  
   vector<int> positions;

   newPos = input.find (delimiter, 0);

   if( newPos < 0 ) 
   { 
       results.push_back(input);
	   return 1;
   }

   int numFound = 0;
   while( newPos >= iPos )
   {
       numFound++;
       positions.push_back(newPos);
       iPos = newPos;
       newPos = input.find (delimiter, iPos+sizeS2+1);
   }

   for( unsigned int i=0; i <= positions.size(); i++ )
   {
       string s;
       if( i == 0 )
	   {
	   s = input.substr( i, positions[i] );
	   }
	   else
	   {
       int offset=0;
	   if(i>0)	 offset= positions[i-1] + sizeS2;
       if( offset < isize )
       {
           if( i == positions.size() )
           {
		s = input.substr(offset);
           }
           else if( i > 0 )
           {
                s = input.substr( positions[i-1] + sizeS2, positions[i] - positions[i-1] - sizeS2 );
           }
       }
	   }
       if( s.size() > 0 )
       {
               results.push_back(s);
       }
   }
   return numFound;
}

bool TeNoCaseCmp(const char&  c1, const char& c2)
{
	return toupper(c1) == toupper(c2);
}


bool
TeStringCompare(const string& str1, const string& str2, bool caseS)
{
	if (!caseS)
	{
		return ((str1.size() == str2.size()) &&
			    equal(str1.begin(),str1.end(),str2.begin(),TeNoCaseCmp));
	}
	else
		return (str1 == str2);
}

double 
TeRoundD(double val, int precision)
{
	char name [ TeNAME_LENGTH ];
	sprintf ( name, "%.*f", precision, val);
	return atof(name);

}

bool
TeCompareDouble(double a, double b, int precision)
{
	char bufa [ TeNAME_LENGTH ];
	char bufb [ TeNAME_LENGTH ];
	if (precision == 0)
	{
		sprintf ( bufa, "%f", a );
		sprintf ( bufb, "%f", b );
	}
	else
	{
		sprintf ( bufa, "%.*f", precision, a );
		sprintf ( bufb, "%.*f", precision, b );
	}

	string A = bufa;
	string B = bufb;
	return (A == B);
}

void TeWriteToFile(const string& fileName, const string& text, const string& mode)
{
	FILE *f;

	f = fopen(fileName.c_str(), mode.c_str());

	fprintf(f, "%s", text.c_str());

	fclose(f);

	return;
}

double TeAdjustToPrecision(double val, int precision, bool reduce)
{
	double p = pow(10.0, (double)-precision);
	
	if (reduce)
		return (val - p);

	return (val + p);
}

string TeCheckName(const string& name, bool& changed, string& invalidChar)
{
	string newName = name;
	if(newName[0] >= 0x30 && newName[0] <= 0x39)
	{
		newName[0] = '_';
		invalidChar = "begin with a numeric character";
	}

	int ff = newName.find(" ");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find(" ");
		invalidChar = "blank space";
	}

	ff = newName.find(".");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find(".");
		invalidChar = "dot .";
	}

	ff = newName.find("*");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("*");
		invalidChar = "mathematical symbol *";
	}

	ff = newName.find("/");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("/");
		invalidChar = "mathematical symbol /";
	}

	ff = newName.find("(");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("(");
		invalidChar = "parentheses (";
	}

	ff = newName.find(")");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find(")");
		invalidChar = "parentheses )";
	}
	ff = newName.find("-");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("-");
		invalidChar = "mathematical symbol -";
	}

	ff = newName.find("+");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("+");
		invalidChar = "mathematical symbol +";
	}

	ff = newName.find("%");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("%");
		invalidChar = "mathematical symbol %";
	}

	ff = newName.find(">");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find(">");
		invalidChar = "mathematical symbol >";
	}

	ff = newName.find("<");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("<");
		invalidChar = "mathematical symbol <";
	}

	ff = newName.find("&");
	while(ff >= 0)
	{
		newName.replace(ff, 1, "_");
		ff = newName.find("&");
		invalidChar = "mathematical symbol <";
	}

	string u = TeConvertToUpperCase(newName);
	if(u=="OR" || u=="AND" || u=="NOT" || u=="LIKE" ||
	   u=="SELECT" || u=="FROM" || u=="UPDATE" || u=="DELETE" ||u=="BY" || u=="GROUP" || u=="ORDER" ||
	   u=="DROP" || u=="INTO" || u=="VALUE" || u=="IN" || u=="ASC" || u=="DESC"|| u=="COUNT" || u=="JOIN" ||
	   u=="LEFT" || u=="RIGHT" || u=="INNER" || u=="UNION" || u=="IS" || u=="NULL" || u=="WHERE" ||
	   u=="BETWEEN" || u=="DISTINCT" || u=="TRY" || u=="IT" || u=="INSERT" || u=="ALIASES" || u=="CREATE" ||
	   u=="ALTER" || u=="TABLE" || u=="INDEX" || u=="ALL" || u=="HAVING" || u=="EXEC" || u== "SET" ||
	   u == "AVG" || u == "MAX" || u == "MIN" || u == "SUM")
	{
		invalidChar = newName;	
		newName += "_";
	}

	// check against geometry tables field names
	string n = TeConvertToLowerCase(newName); 

	if (n == "x" || n == "y" || n == "object_id" ||
	   n == "geom_id" || n == "num_coords" || 
	   n == "lower_x" || n == "lower_y" ||
	   n == "upper_x" || n == "upper_y" ||
	   n == "ext_max" || n == "spatial_data" ||
	   n == "num_holes" || n == "parent_id" ||
	   n == "col_number" || n == "row_number" || 
	   n == "text_value" || n == "angle" ||
	   n == "height" || n == "alignment_vert" ||
	   n == "alignment_horiz" || n=="from_node" ||
	   n == "to_node")
	{
			invalidChar = newName;
			newName += "_";
	}

	// reserved words
	if( (n=="zone") || (n=="comp") || (n=="no") || (n=="local") ||
		(n=="level") || (n=="long"))
	{
		invalidChar = newName;
		newName += "_";
	}

	changed = true;
	if(name == newName)
		changed = false;
	return newName;
}


unsigned long int TeGetFreePhysicalMemory()
{
  unsigned long int freemem = 0;

  #if defined __unix__
    #if defined( __FreeBSD__ ) || defined( __APPLE_CC__ )
      /* BSD workaround */
      
      unsigned int usermem;
      size_t usermem_len = sizeof( usermem );
      int mib[2] = { CTL_HW, HW_USERMEM };
      
      if( sysctl( mib, ( 2 * sizeof(int) ), &usermem, &usermem_len, NULL, 0 ) 
        == 0 ) {
        
        freemem = (unsigned long int)usermem;
      } else {
        throw( "TeGetFreePhysicalMemory error" );
      }
    #else
      /* Other linux stuff */
      
      freemem = (unsigned long int) sysconf( _SC_PAGESIZE ) *
        (unsigned long int) sysconf( _SC_AVPHYS_PAGES );
    #endif
  #elif defined WIN32
      LPMEMORYSTATUS status_buffer = new MEMORYSTATUS;
      GlobalMemoryStatus( status_buffer );
      freemem = (unsigned long int) status_buffer->dwAvailPhys;
      delete status_buffer;
  #else
      #error "Unsuported plataform for physical memory checking"
  #endif

  return freemem;
}


unsigned long int TeGetTotalPhysicalMemory()
{
  unsigned long int totalmem = 0;

  #if defined __unix__
    #if defined( __FreeBSD__ ) || defined( __APPLE_CC__ )
      /* BSD workaround */
      
      unsigned int physmem;
      size_t physmem_len = sizeof( physmem );
      int mib[2] = { CTL_HW, HW_PHYSMEM };
      
      if( sysctl( mib, ( 2 * sizeof(int) ), &physmem, &physmem_len, NULL, 0 ) 
        == 0 ) {
        
        totalmem = (unsigned long int)physmem; 
      } else {
        throw( "TeGetTotalPhysicalMemory error" );
      }
    #else
      /* Other linux stuff */  
  
      totalmem = (unsigned long int) sysconf( _SC_PAGESIZE ) *
        (unsigned long int) sysconf( _SC_PHYS_PAGES );
    #endif
  #elif defined WIN32
    LPMEMORYSTATUS status_buffer = new MEMORYSTATUS;
    GlobalMemoryStatus( status_buffer );
    totalmem = (unsigned long int) status_buffer->dwTotalPhys;
    delete status_buffer;
  #else
    #error "Unsuported plataform for physical memory checking"
  #endif

  return totalmem;
}


unsigned int TeGetPhysProcNumber()
{
  unsigned int procnmb = 0;
  
  #if TePLATFORM == TePLATFORMCODE_MSWINDOWS
    SYSTEM_INFO siSysInfo;
    GetSystemInfo(&siSysInfo);
    procnmb = (unsigned int)siSysInfo.dwNumberOfProcessors;
  #elif TePLATFORM == TePLATFORMCODE_LINUX || TePLATFORM == TePLATFORMCODE_AIX
  
    procnmb = (unsigned int)sysconf(_SC_NPROCESSORS_ONLN );
  #else
    #error "ERROR: Unsupported platform"
  #endif    

  return procnmb;
}
  
  
bool TeGetTempFileName( std::string& filename )
{
    cerr << "ERROR - TeGetTempFileName() should have never be called!!!"<< endl;
    return false;
}


unsigned long int TeGetFileSize( const std::string& filename )
{
  FILE* fileptr = fopen( filename.c_str(), "r" );
  
  if( fileptr == 0 ) {
    throw TeException( CANNOT_OPEN_FILE, "File not found", false );
  }
  
  fseek( fileptr, 0, SEEK_END );
  
  unsigned long int filesize = ( unsigned long int ) ftell( fileptr );
  
  fclose( fileptr );
  
  return filesize;
}


bool TeCheckFileExistence( const std::string& filename )
{
  FILE* fileptr = fopen( filename.c_str(), "r" );
  
  if( fileptr == 0 ) {
    return false;
  } else {
    fclose( fileptr );
    return true;
  }
}
