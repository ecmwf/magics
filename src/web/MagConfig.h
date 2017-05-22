/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagConfig_H
#define MagConfig_H

#include "magics.h"



#include "json_spirit.h"
#include <limits>

namespace magics {
class MagConfig
{
public:
	MagConfig();
	~MagConfig();

	string convert(const json_spirit::Value&);

	virtual void callback(const string&, const json_spirit::Value&) = 0;

};


class MagConfigHandler

{
public:
	MagConfigHandler(const string& config, MagConfig& object);
	virtual ~MagConfigHandler();

	void dig(const json_spirit::Value&);
	void ignore(const json_spirit::Value&) {}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 


private:
    //! Copy constructor - No copy allowed
	MagConfigHandler(const MagConfigHandler&);
    //! Overloaded << operator to copy - No copy allowed
	MagConfigHandler& operator=(const MagConfigHandler&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MagConfigHandler& p)
		{ p.print(s); return s; }

};











} // namespace magics
#endif
