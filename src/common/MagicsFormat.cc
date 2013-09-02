/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

#include "MagicsFormat.h"
#include <ios>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <MagLog.h>

using namespace std;
using namespace magics;

bool MagicsFormat::valid(ostream& out) const
{
	if ( magCompare(format_, "(automatic)") || magCompare(format_, "automatic" ) )
	{
		char bufr[256];
		double trVal=trunc(value_);
		if(value_ == trVal  &&  static_cast<double>(static_cast<int>(trVal)) == trVal)
		{
			sprintf(bufr, "%d", static_cast<int>(trVal));
		}
		else
		{
		  	sprintf(bufr, "%g", value_);
		}	
			
		out << bufr; 
		return true;
	}
    
	string print =  "%g";
	string flags;
	string width;
	string precision;
	string specifier = "g";
	string:: const_iterator c = format_.begin();

	int state = 0;
	bool valid = true;
	int space = 0;

	while (c != format_.end() && valid)
	{
		switch (state)
		{
			case 0 :
				if (*c == '(') {
					state = 1;
					break;
				}
			case 1 : 
				switch (*c) {
					case 'B':
					case 'b': state = 3; break; // Wait n/z
					case 'S': 
					case 's': state = 4; break; // Wait p
					case 'E':
					case 'e': state = 5; break; // Wait number
					case 'G':
					case 'g': state = 6; break; // Wait number
					case 'I':
					case 'i': state = 7; break; // Wait number
					case 'F': 
					case 'f': state = 8; break; // Wait number
					default:
						if ( isdigit(*c) ) {
							state = 2; // Wait X								
							space = atoi(&(*c));						
						}
						else valid = false;
						break;
					}
				break;
			 case 2 : // Format ---> nX Specifies that "n" spaces will appear in the output field.
			 	if ( isdigit(*c) ) break;
				switch (*c) {						
					case 'X': 	
					case 'x': 
					          for ( int i = 0; i < space; i++) out << " ";
						      state = 1;
						      MagLog::debug() << state << endl;
					          break;						
					default:
					    valid=false;
						break;
					}
					break;
			  case 3 : // Format BN Leading blank in an numerical output are removed
			  	 switch (*c) {
					case 'N':
					case 'n': 
						
						//out.left; 
						state = 1; 
						break;
					case 'Z':
					case 'z': 
					    
						//out.right(); 
						state = 1; 
						break; 
					default :
						valid = false;
						break;
				 }
				break;
			   case 5 : // Format Ew.d[E.e] Plots real values in exponential form.				
				 specifier = "e";
				 if (isdigit(*c) ) {
					 width = tostring( atoi(&(*c))); 
					state = 51;
					break;
				 }					
				 valid = false;
				 break;
				case 51 : // Format Ew.d[E.e] Waiting for a . 
					if (isdigit(*c) ) break;
					if (*c == '.') {
						state = 52;
						break;
					}
					if ( *c == 'E' || *c == 'e') {
						state = 54;
						break;
					}
					if ( *c == '(') {
						state = 101;
						break;
					}
					valid=false;
					break;
				case 52 : // // Format Ew.d[E.e] Waiting for a number d
					if (isdigit(*c) ) {
						out.precision(atoi(&(*c)));
						state = 53;
						break;
					}
					valid = false;
					break;
				case 53 : // Format Ew.d[E.e] Waiting for a E or for the end of the format!
					if (isdigit(*c) ) break; 
					if ( *c == 'E' || *c == 'e') {
						state = 54;
						break;
					}
					if ( *c == '(') {
						state = 101;
						break;
					}
					valid = false;
				 	break;
				 case 54 : // Format Ew.d[E.e] Waiting for a number e
				 	 if (isdigit(*c) ) {
					
						state = 100;
						break;
					}
					valid = false;
					break;
					
				case 6 : // Format Gw.d[Ee]
			    
			  	 out << scientific;
				 if (isdigit(*c) ) {
				    // to d
					state = 9;
					break;
				 }
				 valid = false;
				 break;
			    case 7 : // Format Iw[.m]
			    	specifier ="d";
			     
				 if (isdigit(*c) ) {
					  width = tostring( atoi(&(*c))); 
					state = 71;
					break;
				 }
				 valid = false;
				 break;
				case 71 : // Format Iw[.m] -->Wait for a .
					if ( *c == '.') { 
						
						state = 72;
						break;
					}
					if ( *c == ')') {
						// set the width.
						char bufr[256];
						string print = "%" + width + precision + specifier;
						sprintf(bufr, print.c_str(), int(value_));
						out << bufr; 
						break;
					}
					valid = false;
				 	break;
				 case 72 : // Format Iw[.m] -->Wait for a number m [the least number of digits in the field] 
					specifier = "i";
					 
					 if (isdigit(*c) ) {
						 precision =  "." + tostring( atoi(&(*c)));
						state = 100;
						break;
					 }				
				
				     state = 100;
				 	valid = false;
				 	break;
				 case 100 : // Wait for a ) to finish the format
				  	if (isdigit(*c) ) break;
				  	if ( *c == ')' ) {
				    	// set the width.
				  		char bufr[1024];
				  			string print = "%" + width + precision + specifier;
				  			
				  			sprintf(bufr, print.c_str(), value_);
				  			
				  			out << bufr; 
				  			
						state = 102;
						break;
				 	}
				 	valid = false;
				 	break;
				 case 102 :
					 break;
				 case 101 :
				 	valid = false;
				 	break;

                case 8: // Format F(w.d)           
                    specifier = "f";

                    if (isdigit(*c) ) {
					    width = tostring( atoi(&(*c))); 
					    state = 81;
					    break;
				    }
                    valid=false;
                    break;
			    case 81 : // Format  F(w.d) wait for a .
			    	if (isdigit(*c) ) break;
					if ( *c == '.' ) {     
				       state = 82;
				       break;
					}
					if ( *c == ')' ) {
						state = 101;
						break;
				 	}
					valid = false;
					break;
		    case 82: 
		    	if ( isdigit(*c) )
			{
		    		precision =  "." + tostring( atoi(&(*c)));
		    		state =100;
		    		break;
		    	}
		    	if ( *c == ')' )
			{ 
				state = 101;
				break;
			}
			default: 
				break;
		}
		c++;
	}
	return valid;
}

ostream& MagicsFormat::format(ostream& out) const
{
	//MagLog::debug() << "MAGICS FORMAT to APPLY --->" << format_ << "=====";
	if ( !valid(out) ) { 
		MagLog::warning() << " Format \"" << format_ << "\" is not valid: will be reset to automatic " << endl; 
		return out;
	}
	//out.width(20);
	//out << scientific; // exposant
	//out << showpos; // sign +
	//out << fixed;
	//out.width(8); 
	//out.precision(3); // nombre apres la virgule
	return out;
}
