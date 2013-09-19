#include <algorithm>

#include "Layer.h"

#include "VisDefInfo.h"


//===============================================================
//
// VisDefInfoBase
//
//===============================================================

VisDefInfoBase::VisDefInfoBase(string fConf, DataType dataType) : fConf_(fConf), dataType_(dataType)
{
	loaded_=false;
	dataTypeName_[GribType]="GRIB";
}


VisDefInfoBase::~VisDefInfoBase()
{
	clear();
}

void VisDefInfoBase::clear()
{
	for(unsigned int i=0; i < items_.size(); i++)
	{
		delete items_[i];
	}
	items_.clear();
}

VisDefInfoItem* VisDefInfoBase::addItem(string name)
{
	VisDefInfoItem* item=new VisDefInfoItem(name);
	items_.push_back(item);
	return item;
}

void VisDefInfoBase::deleteItem(int /*index*/)
{
	//vector<MvKeyProfile*>::iterator it=begin()+index;
	//delete (*it);
	//erase(it);	
}	
	
void VisDefInfoBase::collectKeys()
{	
	keys_.clear();
	for(vector<VisDefInfoItem*>::iterator it=items_.begin(); it != items_.end(); it++)
	{		
		for(map<string,vector<string> >::const_iterator itK=(*it)->keys().begin(); itK != (*it)->keys().end(); itK++)
		{

			if(find(keys_.begin(), keys_.end(), itK->first) == keys_.end())
			{
				keys_.push_back(itK->first);
			}

		}
	}	


}


//===============================================================
//
// MvQObstatVisDefInfo
//
//===============================================================


ObstatVisDefInfo::ObstatVisDefInfo(string fConf, DataType dataType) : VisDefInfoBase(fConf,dataType) 
{
	fConf_ = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "ObstatGribVisDef.txt";

	loadItems();

	type_="ObstatGrib";

	baseAttributes_["contour"] = "OFF";
	//req("CONTOUR_MIN_LEVEL") = "10";
	baseAttributes_["contour_level_selection_type"] = "INTERVAL";
	//req("CONTOUR_INTERVAL") = "10";
	baseAttributes_["contour_shade"] = "ON";
	baseAttributes_["contour_shade_technique"] = "CELL_SHADING";
	baseAttributes_["contour_shade_min_level_colour"] = "BLUE";
	baseAttributes_["contour_shade_max_level_colour"] = "RED";
	baseAttributes_["contour_shade_colour_direction"] = "ANTICLOCKWISE";
	baseAttributes_["contour_shade_method"] = "AREA_FILL";
	baseAttributes_["contour_label"] = "OFF";
	baseAttributes_["contour_hilo"] = "OFF";

}

void ObstatVisDefInfo::loadItems()
{
	if(loaded_)
		return;

	//Obs        fgdep       bcor       Obstdv       fgdStdv    BcorSTDV      Count     Unit
	const char *statIdDef[]={"2","4","18","3","5","19","1"};
	vector<string> statId(statIdDef,statIdDef + sizeof(statIdDef)/sizeof(char*));

	ifstream in;
	in.open(fConf_.c_str(),ifstream::in);

	if(in.fail())
	{
		MagLog::warning() << "ObstatVisDefInfo::loadItems() --> Cannot open visdef file " << fConf_ << "\n";
		return;
	}

        while(!in.eof())
        {
	        string line;
		getline(in,line); 
	       
		if(line.find("#") == string::npos)
		{
			vector<string> lst;
			string str;
			stringstream iss(line,istringstream::in);
			while( iss >> str )     
			{
				lst.push_back(str);

			}
		
                   	if(lst.size() ==0 || lst[0].size() < 7)
				continue;	

			str=lst[0];
			string id=removeZerosFromNumber(str.substr(0,3));
			string sensor=removeZerosFromNumber(str.substr(3,3));
			string channel=removeZerosFromNumber(str.substr(6));
		
			int statCnt=0;
			for(unsigned int i=2; i < lst.size() && statCnt < 8; i+=4)
			{
				if(lst[i] !="|" && i+2 < lst.size())
				{
					VisDefInfoItem *visDef= new VisDefInfoItem("obstat");
					items_.push_back(visDef);				
					
					string keyName, keyValue;

					keyName="platform";
					keyValue=id;
					visDef->addKey(keyName,keyValue);

					keyName="instrument";
					keyValue=sensor;
					visDef->addKey(keyName,keyValue);
												
					keyName="scaledValueOfFirstFixedSurface";
					keyValue=channel;
					visDef->addKey(keyName,keyValue);
					
					keyName="observationDiagnostic";
					keyValue=statId[statCnt];
					visDef->addKey(keyName,keyValue);

					visDef->addAttribute("contour_min_level",lst[i]);
					visDef->addAttribute("contour_interval",lst[i+1]);
			
					statCnt++;
				}
			}
		}

        }

	in.close();

	loaded_=true;

	collectKeys();
}
	 
void ObstatVisDefInfo::getAttributes(MetaDataCollector& meta, map<string, string>& attributes)
{
	for(vector<VisDefInfoItem*>::iterator it=items_.begin(); it != items_.end(); it++)
	{
		bool found;
		for(map<string,vector<string> >::const_iterator itK=(*it)->keys().begin(); itK != (*it)->keys().end(); itK++)
		{
			string name=itK->first;
			const vector<string>& value=itK->second;

			found=false;
			if(meta.find(name) != meta.end() &&
			   find(value.begin(),value.end(),meta[name]) != value.end())
			{
					found=true;
			}
			else
			{
				break;
			}
		}
		
 		if(found == true)
		{			
			for(map<string,string>::iterator itA=baseAttributes_.begin(); 
			    itA != baseAttributes_.end(); itA++)
			{
				attributes[itA->first]=itA->second;
			}

			map<string,string> vAttr=(*it)->attributes();

			if(vAttr["contour_min_level"] != "-99")
				attributes["contour_min_level"]=vAttr["contour_min_level"];

			attributes["contour_interval"]=vAttr["contour_interval"];

			return;

		}

	}
}

string ObstatVisDefInfo::removeZerosFromNumber(string s)
{
	istringstream iss(s);
	int num;
	iss >> num;

	stringstream ss;
   	ss << num;
   	return ss.str();
}

//===================================================
//
// VisDefInfoFactory
//
//===================================================

VisDefInfoBase* VisDefInfoFactory::makeItem(string type)
{
	if(type == "ObstatGrib")
  	{
		return new ObstatVisDefInfo("",VisDefInfoBase::GribType);
	}	

	return 0;
}
