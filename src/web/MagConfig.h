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


class StyleLibrary : public MagConfig
{
public:
	StyleLibrary(const string& theme, const string& family): theme_(theme), family_(family) { init(); }
	StyleLibrary(const string& family): family_(family) { init(); }
	~StyleLibrary() {}

	void callback(const string& name, const json_spirit::Value& value);
	void init();
	
	map<string, map<string, string> > library_;
	map<string, string> empty_;

	string theme_;
	string family_;

	const map<string, string>& get(const string& name) const;
	
};

class NetcdfGuess : public MagConfig
{
public:
	NetcdfGuess(const string& name): name_(name)   { init(); }
	NetcdfGuess(): name_("netcdf-convention") { init(); }
	~NetcdfGuess() {}

	void callback(const string& name, const json_spirit::Value& value);
	void init();

	string name_;

	map<string, map<string, vector<string> > > guess_;	
	const map<string, string>& get(const string& name) const;
	
};










} // namespace magics
#endif
