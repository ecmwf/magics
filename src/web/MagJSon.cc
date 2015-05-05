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

#include "MagJSon.h"


using namespace magics;
using namespace json_spirit;
#include "XmlMagics.h"
#include "XmlTree.h"

#include "Timer.h"
MagJSon::MagJSon()
{
	patchs_["drivers"] = &MagJSon::drivers;
	patchs_["definition"] = &MagJSon::definitions;
}

void MagJSon::execute(const string& magml, const map<string, string>& params)
{
	TempFile file;
	
	prepare(magml, params, file);
	parse(file.name());
}

void MagJSon::parse(const string& file)
{
     MagLog::dev()<< "parse-->" << file.c_str() << endl; 

		ifstream is( file.c_str());
		json_spirit::Value value;      
		json_spirit::read_or_throw( is, value );
		magics(value);
}

void MagJSon::interpret(const string& def)
{
     MagLog::dev()<< "interpret-->" << def << endl;
     istringstream is(def);

	json_spirit::Value value;
	json_spirit::read_or_throw(is, value );
	ASSERT( value.type() == obj_type );
	Object object = value.get_value< Object >();
	//buils the Magics XmlNode!
	build(*tree_.root(), "magics", object);
}

void MagJSon::drivers(XmlNode& parent, const json_spirit::Value& value)
{
	ASSERT (value.type() == array_type);
	XmlNode* drivers = new XmlNode("drivers");
	parent.push_back(drivers);
	 Array all =  value.get_value< Array >();
	
	 for (Array::iterator entry = all.begin(); entry != all.end(); ++entry) {
		 ASSERT( entry->type() == obj_type);
		 Object driver = entry->get_value< Object >();	     		
		 map<string, string> attributes;
		 for (vector<Pair>::const_iterator elt = driver.begin(); elt !=  driver.end(); ++elt) {
	    	ASSERT(elt->value_.type() == str_type);
	    	attributes.insert(make_pair(elt->name_, elt->value_.get_value< string >()));
	     }
		 map<string, string>::iterator format = attributes.find("format");
		 ASSERT(format!=attributes.end());
		 drivers->push_back(tree_.newNode(format->second, attributes));
	  }
}


void MagJSon::definitions(XmlNode& parent, const json_spirit::Value& value)
{
	ASSERT (value.type() == array_type);

	XmlNode* definitions = new XmlNode("definition");
	tree_.definition(definitions);
	
	 Array all =  value.get_value< Array >();
	 for (Array::iterator entry = all.begin(); entry != all.end(); ++entry) {
		 ASSERT( entry->type() == obj_type);
		 Object def = entry->get_value< Object >();	     		
		 map<string, string> attributes;
		 for (vector<Pair>::const_iterator elt = def.begin(); elt !=  def.end(); ++elt) {
	    	ASSERT(elt->value_.type() == str_type);
	    	attributes.insert(make_pair(elt->name_, elt->value_.get_value< string >()));
	     }
		 map<string, string>::iterator type = attributes.find("class");
		 ASSERT(type!=attributes.end());
		 definitions->push_back(tree_.newNode(type->second, attributes));
	  }
}


void MagJSon::build(XmlNode& parent, const string& name, Object& object)
{
	map<string, string> attributes;
	
	for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry)
	{
		      if ( entry->value_.type() == str_type) {
		    	 attributes.insert(make_pair(entry->name_, entry->value_.get_value<string>()));
		      }
		      if ( entry->value_.type() == bool_type) {
		    	  string value = entry->value_.get_value<bool>() ? "on" : "off";
		    	  attributes.insert(make_pair(entry->name_, value));
		      }
		      if ( entry->value_.type() == int_type) {
		    	  string value = tostring(entry->value_.get_value<int>());
		    	  attributes.insert(make_pair(entry->name_, value));	    		    	
		      }
		      if ( entry->value_.type() ==real_type) {
		    	  string value = tostring(entry->value_.get_value<double>());
		    	  attributes.insert(make_pair(entry->name_, value));
		      }
	}
	XmlNode* node = tree_.newNode(name, attributes);
	parent.push_back(node);
	for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
			      
			      // We can apply a patch ..
			      map<string,  Patch >::iterator patch = patchs_.find(entry->name_);
			      if ( patch != patchs_.end() ) {
			    	  ( (this->*patch->second)(*node, entry->value_) );
			            continue;
			      }  	
			     
			      if  ( entry->value_.type() == obj_type) {
			    	  Object object = entry->value_.get_value< Object >();
			    	  build(*node, entry->name_, object);
			      }
			      if  ( entry->value_.type() == array_type) {
			    	  
			    	  	Array object =  entry->value_.get_value< Array >();
			     		    for (Array::iterator val = object.begin(); val != object.end(); ++val) {
			     		    	if ( val->type() == obj_type) {
			     		    		Object object = val->get_value< Object >();	     		    		
			     		    		build(*node, entry->name_, object);
			     		    	}
			     		    }
			     	}
		}
}


void MagJSon::magics(const json_spirit::Value& value)
{
	ASSERT( value.type() == obj_type );
	Object object = value.get_value< Object >();
	
	XmlMagics magics;
	// buils the Magics XmlNode! 
	build(*tree_.root(), "magics", object);
	Timer timer("total", "execution");
	magics.execute(tree_);
}
