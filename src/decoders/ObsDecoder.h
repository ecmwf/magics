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

/*! \file ObsDecoder.h
    \brief Definition of the Template class ObsDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 23-Mar-2005
    
    Changes:
    
*/

#ifndef ObsDecoder_H
#define ObsDecoder_H

#include "magics.h"

#include "ObsDecoderAttributes.h"
#include "Decoder.h"
#include "Data.h"

namespace magics {

class BufrIdentifiers : public map<string, string>
{
public:
	BufrIdentifiers(int centre);
	~BufrIdentifiers() {}
	void set(const map<string, string>&);
	int ident(const string&) const; 	// Return -1 if ident does not exist.
protected:
	int centre_;
};

class BufrIdentTable : public map<int, BufrIdentifiers*>
{
public:
	BufrIdentTable() {}
	static const BufrIdentifiers& get(int centre);

protected: 	
	static  BufrIdentTable table_;
};



class TitleNode;

/*! \brief Reader for obs data in BUFR
  
*/
class ObsDecoder: 
	public ObsDecoderAttributes, 
	public Decoder, 
	public Data,
	public PointsList
{
public:
	ObsDecoder();
	virtual ~ObsDecoder();
	virtual void set(const map<string, string>& map) 
		{  ObsDecoderAttributes::set(map); }

	virtual void decode();

	bool defined() { return file_name_.empty() == false; }
	void getInfo(const std::set<string>&, multimap<string, string>&);	
	void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
	void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool)
	{
		customisedPoints(t, n, out);
	}
	virtual PointsHandler& points();
	PointsHandler& points(const Transformation&, bool) { return  points(); }
	void visit(TitleNode&);

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	string   title_;

	bool findInTypes(const string&); 
	bool checkLevel(double);

private:
	//! Copy constructor - No copy allowed
	ObsDecoder(const ObsDecoder&);
	//! Overloaded << operator to copy - No copy allowed
	ObsDecoder& operator=(const ObsDecoder&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ObsDecoder& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
