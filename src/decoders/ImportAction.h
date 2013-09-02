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

/*! \file ImportAction.h
    \brief Definition of the Template class ImportAction.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 6-Apr-2005
    
    Changes:
    
*/

#ifndef ImportAction_H
#define ImportAction_H

#include "magics.h"

#include "ImportActionAttributes.h"
#include "Data.h"
#include "TextVisitor.h"
namespace magics {


class ImportAction: public ImportActionAttributes, public Data {

public:
	ImportAction() {}
	virtual ~ImportAction() {} 
	void set(const map<string, string>& map) { ImportActionAttributes::set(map); }
	
	
	string path() { return path_; }
	
	void visit(MetaDataCollector& collector) {
		MetviewIcon::visit(collector);

		MetaDataCollector::iterator format = collector.find("MV_Format");
		if ( format != collector.end() ) {
			format->second = service_;
		}

		MetaDataCollector::iterator name = collector.find("shortName");
		if ( name != collector.end() ) {
			name->second = short_name_;
		}
	 }

	 void visit(TextVisitor& text)
	 {
		MetaDataCollector collector;
		collector["service_name"]="";
		collector["service_title"]="";
		collector["title"]="";
		collector["date"]="";
		collector["time"]="";

		MetviewIcon::visit(collector);

		if(collector["service_name"] == "WMS")
		{
			string s="WMS: " + collector["title"] + " " + collector["date"] + " " + collector["time"];

			text.addAutomaticTitle(s);
		}	
		else if(!url_.empty() && !service_.empty() )
		{
			text.addAutomaticTitle(service_ + " :" + url_);
		} 
	 }
	   virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool ) {}
	    //! Method to access the data as a list of points
	     // needMissing : if true the list will contain all the points (If they are outside the area: They will be flagged missing)
	     virtual PointsHandler& points(const Transformation&, bool) { assert(false); }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "ImportAction["; ImportActionAttributes::print(out); out << "]"; }

private:
    //! Copy constructor - No copy allowed
	ImportAction(const ImportAction&);
    //! Overloaded << operator to copy - No copy allowed
	ImportAction& operator=(const ImportAction&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ImportAction& p)
		{ p.print(s); return s; }
};


class ImportLoop : public DataLoop, public vector<Data * >
{
public :
	ImportLoop() {}
	virtual ~ImportLoop() {}
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual void set(LayerNode&) {}
	
	virtual void setToFirst() { current_ = this->begin(); }
	virtual Data* current() { return *current_; }
	virtual bool         hasMore() { return current_!= this->end(); }
	virtual void         next() { ++current_; }
	void add(Data* data) { push_back(data); }
protected:
	vector<Data * >::iterator current_;
};
} // namespace magics

#endif
