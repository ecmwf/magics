//***********************************************************************
//      TerraLib is a GIS Classes and Functions Library that 
//      strongly explores Spatial Database Technologies 
//
//      Copyright © 2002 INPE and Tecgraf/PUC-Rio. 
//
//      This library is free software; you can redistribute it 
//      and/or modify it under the terms of the GNU Lesser General 
//      Public License as published by the Free Software Foundation
//      version 2.1.(http://www.opensource.org/licenses/lgpl-license.php)
//
//      
//
//      Send questions or suggestions about the TerraLib Project 
//      to terralib@dpi.inpe.br .
//**************************************************************************//
/*! \file TeConsoleErrorMessage.h
    This file deals with the display of error messages in a console
*/
#ifndef TeConsoleErrorMessage_H
#define TeConsoleErrorMessage_H

#include <iostream>
#include "TeErrorMessage.h"

using namespace std;

//! A class to handle the display of error messages in a console
class TeConsoleErrorMessage: public TeErrorMessage
{
public:
   
	//! Constructor
	TeConsoleErrorMessage()
	{}

	//! Destructor
	~TeConsoleErrorMessage()
	{}

	//! Display the message
	virtual TeMessageReturn apply ( 
				  const string& appName, 
				  const string & msgText, 
				  TeMessageType	msgType,
				  TeMessageIcon iconType )
	{ cout << appName << msgText; return Te_IDOK; }

};

//! A prototype of a Console Error Message handler
class TeConsoleErrorMessagePrototype: public TeErrorMessagePrototype
{
	virtual TeErrorMessage* build()
	{ return new TeConsoleErrorMessage(); }

};

//! A single instance of the console error message handler
static TeConsoleErrorMessagePrototype consoleInstance;
#endif

