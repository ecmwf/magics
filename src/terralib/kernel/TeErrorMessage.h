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
/*! \file TeErrorMessage.h
    \brief This file contains some definitions issuing error messages to the user
*/
#ifndef TeErrorMessage_H
#define TeErrorMessage_H

#include <string>
using namespace std;

#include "TePrototype.h"
#include "TeSingleton.h"

/*!
\enum TeMessageType 

Te_ABORTRETRYIGNORE   
	The message box contains three pushbuttons: Abort, Retry, and Ignore.
Te_OK   The message box contains one pushbutton: OK.
Te_OKCANCEL   The message box contains two pushbuttons: OK and Cancel.
Te_RETRYCANCEL   The message box contains two pushbuttons: Retry and Cancel.
Te_YESNO   The message box contains two pushbuttons: Yes and No.
Te_YESNOCANCEL   The message box contains three pushbuttons: Yes, No, and Cancel. 

---*/
enum TeMessageType
{ Te_ABORTRETRYIGNORE, Te_OK, Te_OKCANCEL, Te_RETRYCANCEL, Te_YESNO, Te_YESNOCANCEL };

  
/*!
\enum TeMessageIcon

Te_ICONEXCLAMATION   An exclamation-point icon appears in the message box.
Te_ICONINFORMATION   An icon consisting of an i in a circle appears in the message box.
Te_ICONQUESTION   A question-mark icon appears in the message box.
Te_ICONSTOP   A stop-sign icon appears in the message box. 
-----*/
  
enum TeMessageIcon
{	Te_ICONEXCLAMATION, Te_ICONINFORMATION, Te_ICONQUESTION, Te_ICONSTOP };

/*---------
Return Value

Zero if there is not enough memory to display the message box; 
otherwise one of the following values is returned: 

Te_IDABORT   The Abort button was selected.
Te_IDCANCEL   The Cancel button was selected.
Te_IDIGNORE   The Ignore button was selected.
Te_IDNO   The No button was selected.
Te_IDOK   The OK button was selected.
Te_IDRETRY   The Retry button was selected.
Te_IDYES   The Yes button was selected. 

----*/

enum TeMessageReturn
{ Te_IDABORT, Te_IDCANCEL, Te_IDIGNORE, Te_IDNO, Te_IDOK, Te_IDRETRY, Te_IDYES };

//!  Provides a convenience class for issuing error messages to the user
/*
	  Based on a configuration parameter, provide implementations
	  for console (stdio), MFC and QT
  
	  \sa TeException 
*/
class TL_DLL TeErrorMessage
{
public:
	//! Constructor
	TeErrorMessage(){}

	virtual ~TeErrorMessage(){}

	virtual TeMessageReturn apply ( 
				  const string& appName, 
				  const string & msgText, 
				  TeMessageType	msgType,
				  TeMessageIcon iconType ) = 0; 
};

class TL_DLL TeErrorMessagePrototype: public TePrototype<TeErrorMessage>
{
	virtual TeErrorMessage* build() = 0;
};

#endif

