#include <iostream>
#include "TeErrorMessage.h"

using namespace std;

class TeConsoleErrorMessage: public TeErrorMessage
{
public:
   
	TeConsoleErrorMessage(){}
	~TeConsoleErrorMessage(){}
	virtual TeMessageReturn apply ( 
				  const string& appName, 
				  const string & msgText, 
				  TeMessageType	/* msgType */,
				  TeMessageIcon /* iconType */ )
	{ cout << appName << msgText; return Te_IDOK; }



};


class TeConsoleErrorMessagePrototype: public TeErrorMessagePrototype
{
	virtual TeErrorMessage* build()
	{ return new TeConsoleErrorMessage(); }

}consoleInstance;
