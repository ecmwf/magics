/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ContourLibrary.cc
    \brief Implementation of the Template class ContourLibrary.
    
    Magics Team - ECMWF 2010
    
    Started: Fri 16-Jul-2010
    
    Changes:
    
*/



#include "ContourLibrary.h"
#include "Layer.h"
#include "VisDefInfo.h"
#include "MetaData.h"

using namespace magics;

VisDefInfoBase* ContourLibrary::info_=0;

ContourLibrary::ContourLibrary() 
{	
}


ContourLibrary::~ContourLibrary() 
{	
}

/*!
 Class information are given to the output-stream.
*/		
void ContourLibrary::print(ostream& out)  const
{
	out << "ContourLibrary[";
	out << "]";
}

// sete meta["shortName"] =the meta dat to be collected 
void ContourLibrary::askId(MetaDataCollector& meta)
{
	meta["observationDiagnostic"]="";

}


bool ContourLibrary::checkId(MetaDataCollector& metaId,MetaDataCollector& metaKey)
{
   	//Obstat
	if(metaId["observationDiagnostic"] !="")
	{		
		if(!setInfoObject("ObstatGrib"))
		{
			return false;
		}
	}
	else
	{
		return false;
	}

	for(unsigned int i=0; i < info_->keys().size(); i++)
	{
		metaKey[info_->keys().at(i)]="";
		if(metaId["observationDiagnostic"] !="")
		{
			MetaDataAttribute attr;
			attr.setType(MetaDataAttribute::NumberType);
			metaKey.setAttribute(info_->keys().at(i),attr);
		}
	}

	return true;


}


	
// se the map to set the contour!
void ContourLibrary::getStyle(MetaDataCollector& meta, map<string, string>& attributes, StyleEntry&)
{
	MagLog::dev() << "ContourLibrary::set-->" << endl;

	//Obstat
	if(info_)
	{
		for(map<string,string>::iterator it=meta.begin(); it != meta.end(); it++)
		{
			MagLog::dev() << it->first << "--> " << it->second << endl; 
		}

		info_->getAttributes(meta,attributes);
	}

	
}	

bool ContourLibrary::setInfoObject(string type)
{
	if(info_ && info_->type() != type)
	{
		delete info_;
		info_=0;
	}
	if(!info_)
	{
		info_=VisDefInfoFactory::makeItem(type);
	}

	if(!info_)
		return false;
	else if(info_->isLoaded())
		return true;
	else 
		return false;
}

#include "MagConfig.h"

void  EcChartData::callback(const string& name, const json_spirit::Value& value)
{
	int iname= atoi(name.c_str());
	json_spirit::Object object = value.get_value<json_spirit::Object>();
	data_.insert(make_pair(iname, map<string, string>()));
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
			 data_[iname].insert(make_pair(entry->name_, convert(entry->value_)));
	}
}

map<string,string> EcChartData::getMap(const int key)
{
	return data_[key];
}

void  EcChartSetData::callback(const string& name, const json_spirit::Value& value)
{
	json_spirit::Array values = value.get_value<json_spirit::Array>();
	data_.insert(make_pair(name, vector<int>()));

	for (unsigned int i = 0; i < values.size(); i++) {
		data_[name].push_back(values[i].get_value<int>());
	}
}

bool EcChartSetData::hasKey(const string& key)
{
	return data_.find(key) != data_.end();
}

vector<int> EcChartSetData::getSet(const string& key)
{
	return data_[key];
}

EcChartLibrary::EcChartLibrary():
			contours_("contours"),
			default_set_("default")
{
	keys_.push_back("paramId");
	keys_.push_back("units");
	keys_.push_back("typeOfLevel");
	keys_.push_back("level");
	keys_.push_back("marsClass");
	keys_.push_back("marsType");
	keys_.push_back("marsStream");

	for (int i = 0; i <keys_.size(); i++){	
        index_.insert(make_pair(keys_[i],EcChartSetData(keys_[i])));
    }
}

EcChartLibrary::~EcChartLibrary()
{
	
}

void EcChartLibrary::setCriteria(MetaDataCollector& request, const string& criteria)
{
	request[criteria] = "";
	MetaDataAttribute attribute;
	attribute.setSource(MetaDataAttribute::GribApiSource);
	request.setAttribute(criteria, attribute);
}

void EcChartLibrary::askId(MetaDataCollector& request)
{
	//main keywords
	setCriteria(request, "paramId");
	setCriteria(request, "typeOfLevel");
	setCriteria(request, "level");
	setCriteria(request, "units");

	//auxiliary keywords
	setCriteria(request, "stepRange");
	setCriteria(request, "number");
	setCriteria(request, "marsClass");
	setCriteria(request, "marsType");
	setCriteria(request, "marsStream");
}

// se the map to set the contour!
void EcChartLibrary::getStyle(MetaDataCollector& data, map<string, string>& contour, StyleEntry&)
{
	
	//find the best contour definition
	vector<int>::iterator it;
	vector<int> result_set;
	map<string,EcChartSetData>::iterator ikey;

	//initial result set is units's contours (OBLIGATORY)
	ikey= index_.find("units");
	//if (data["units"]=="" || !ikey->second.hasKey(data["units"])) return;
	result_set= vector<int>(ikey->second.getSet(data["units"]));

	//interset with paramId's contours (OPTIONAL)
	if (result_set.size()>1)
	{
		ikey= index_.find("paramId");
		if (data["paramId"]!="" && ikey->second.hasKey(data["paramId"]))
		{
			vector<int> first_set= vector<int>(result_set);
			vector<int> second_set= vector<int>(ikey->second.getSet(data["paramId"]));
			it=std ::set_intersection (first_set.begin(),first_set.end(),second_set.begin(),second_set.end(),result_set.begin());
			result_set.resize(it-result_set.begin());
			if (result_set.size()==0) result_set= first_set;
		}
	}

	//interset with eccharts layers's default contours (OPTIONAL)
	if (result_set.size()>1 && default_set_.hasKey("default"))
	{
		vector<int> first_set= vector<int>(result_set);
		vector<int> second_set= vector<int>(default_set_.getSet("default"));
		it=std ::set_intersection (first_set.begin(),first_set.end(),second_set.begin(),second_set.end(),result_set.begin());
    	result_set.resize(it-result_set.begin());
		if (result_set.size()==0) result_set= first_set;
	}


	//for each GRIB key (excluding paramId and units that have just been checked, OPTIONAL)
	for (int i = 2; i<keys_.size() && result_set.size()>1; i++)
	{
		//result set is intersection with GRIB key contours
		string key= keys_[i];
		ikey= index_.find(key);
		if (data[key]!="" && ikey->second.hasKey(data[key]))
		{
			vector<int> first_set= vector<int>(result_set);
			vector<int> second_set= vector<int>(ikey->second.getSet(data[key]));
			it=std ::set_intersection (first_set.begin(),first_set.end(),second_set.begin(),second_set.end(),result_set.begin());
			result_set.resize(it-result_set.begin());

			//if intersection is empty, restore previous result set
			if (result_set.size()==0) result_set= first_set;
		}
	}


	//Is there at least one contour left?
	if (result_set.size()>0)
	{
		//set the contour parameters
		map<string, string> cont= contours_.getMap(result_set[0]);
		for (map<string, string>::const_iterator i = cont.begin(); i != cont.end(); ++i)
		{
			contour[i->first]= i->second;
			MagLog::debug() << " EcChartLibrary::getAttributes contour[" << i->first << "]= " << i->second << endl;
		}
	}
	else
		MagLog::info() <<" EcChartLibrary::getAttributes: NO CONTOUR MATCHED!" << endl;
}

void EcChartLibrary::print(ostream&) const
{


}

WebLibrary::WebLibrary() 
{
	
	styles_.init(library_path_);
}

WebLibrary::~WebLibrary()
{
}


// set the meta data to be collected
void WebLibrary::askId(MetaDataCollector& request)
{
//main keywords
	std::set<string> criteria;

	criteria.insert("units");
	criteria.insert("parameterUnits"); // for grib
	styles_.getCriteria(criteria);
	for ( auto c = criteria.begin(); c != criteria.end(); ++c) {
		setCriteria(request, *c);
		cout << " asking for " << *c << endl;
	}
	

	
}

void WebLibrary::setCriteria(MetaDataCollector& request, const string& criteria)
{
	request[criteria] = "";
	MetaDataAttribute attribute;
	attribute.setSource(MetaDataAttribute::GribApiSource);
	request.setAttribute(criteria, attribute);

}

// set the map to set the contour!
void WebLibrary::getStyle(MetaDataCollector& data, map<string, string>& contour, StyleEntry& info)
{

		
		map<string, string> style;

		if ( styles_.findStyle(data, style, info) )
			contour = style;
	
}

void  WebLibrary::getStyle(const string& name, map<string, string>& info) {
	
	styles_.findStyle(name, info);
}
// set the map to set the contour!
void WebLibrary::getScaling(MetaDataCollector& data, double& scaling, double& offset)
{

		
		map<string, string> values;
		StyleEntry info;
		scaling = 1;
		offset = 0;

		cout << "SCALING" << endl;
		

		auto unit = data.find("units");
		if ( unit == data.end() ) 
		 	unit = data.find("parameterUnits");
		if ( unit == data.end() )
			return;
		cout << " Found Unit " << unit->second << endl;
		bool found = styles_.findStyle(data, values, info);
		if ( !found ) {
			cout << "Can not find style" << endl;	
			return;
		}
		cout << " TRYRING to scale " << unit->second << endl;
		for ( auto x = values.begin(); x != values.end(); ++x)
			cout << x->first << "--->" << x->second << endl;
		auto need = values.find("required_units");
		if ( need == values.end() )
			return; 
		
		
		UnitsLibrary converter;

		string whitespaces (" \t\f\v\n\r");
		string clean = unit->second;
  		std::size_t pos = clean.find_last_not_of(whitespaces);
 		if (pos!=std::string::npos) {
    		//cout << "clean" << pos << endl;
    		clean = clean.substr(0, pos);
 		}
       // str is all whitespace
		
		//cout << "CLEAN " << clean << ": " << clean.size() << endl;
		converter.find(need->second, clean, scaling, offset); 
		cout << "Need " << need->second << " get " << unit->second << "--->APPLY " << scaling << " and " << offset << endl;
	
}

void WebLibrary::print(ostream&) const
{

}