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

/*! \file Data.h
    \brief Definition of the Abstract template class Data.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 16-Jan-2004
    
    Changes:
    
*/


#include "Data.h"

void Data::computeStats()
{
	map<string,vector<double> >::iterator itX, itY, itV;	
	itX=stats_.find("x");
	itY=stats_.find("y");
	itV=stats_.find("value");

	//X and Y for scatterplots
	if(info("statsType") == "scatter" && 
	   itX != stats_.end() && itY != stats_.end())
	{
		DataStats stX(itX->second);
		DataStats stY(itY->second);	

		std::map<string,DataStats*> st;
		st["_x"]=&stX;
		st["_y"]=&stY;
	  	
		for(std::map<string,DataStats*>::iterator it=st.begin(); it != st.end(); it++)
		{
		 	setInfo("stats::points" + it->first,tostring(it->second->num()));
			setInfo("stats::min" + it->first, tostring(it->second->min()));	
			setInfo("stats::max" + it->first, tostring(it->second->max()));
			setInfo("stats::avg" + it->first, tostring(it->second->mean()));
			if(it->second->hasStDev()) setInfo("stats::stdev" + it->first,tostring(it->second->stDev()));
				else setInfo("stats::stdev" + it->first,"-");
		}
		
		setInfo("stats::correlation",tostring(DataStats::correlation(itX->second,itY->second,stX,stY)));
	}
	
	else if(info("statsType") == "vector" && 
	        itX != stats_.end() && itY != stats_.end())
	{		
		DataStats stX(itX->second);
		DataStats stY(itY->second);
	  	
		std::map<string,DataStats*> st;
		st["_x"]=&stX;
		st["_y"]=&stY;
	  	
		for(std::map<string,DataStats*>::iterator it=st.begin(); it != st.end(); it++)
		{
		 	setInfo("stats::points" + it->first,tostring(it->second->num()));
			setInfo("stats::min" + it->first, tostring(it->second->min()));	
			setInfo("stats::max" + it->first, tostring(it->second->max()));
			setInfo("stats::avg" + it->first, tostring(it->second->mean()));
			setInfo("stats::stdev" + it->first,"-");
			setInfo("stats::skewness" + it->first,"-");		
			setInfo("stats::kurtosis" + it->first,"-");	
	
			if(it->second->hasStDev())
			{
				setInfo("stats::stdev" + it->first,tostring(it->second->stDev()));
				if(it->second->hasThirdMoment())
				{
					setInfo("stats::skewness" + it->first,tostring(it->second->skewness()));
					setInfo("stats::kurtosis" + it->first,tostring(it->second->kurtosis()));
				}
			}	
		}
	}

        //Value		
	if(itV != stats_.end())
	{
		if(itV->second.size() == 0)	
		{
			setInfo("stats::min","");
    			setInfo("stats::max","");
			setInfo("stats::avg","");
    			setInfo("stats::points","");
			return;
		}	

		DataStats st(itV->second);
		setInfo("stats::points",tostring(st.num()));
		setInfo("stats::min", tostring(st.min()));
    		setInfo("stats::max", tostring(st.max()));
		setInfo("stats::avg", tostring(st.mean()));
		setInfo("stats::stdev","-");
		setInfo("stats::skewness","-");		
		setInfo("stats::kurtosis","-");

		if(st.hasStDev())
		{
			setInfo("stats::stdev",tostring(st.stDev()));
			if(st.hasThirdMoment())
			{
				setInfo("stats::skewness",tostring(st.skewness()));
				setInfo("stats::kurtosis",tostring(st.kurtosis()));
			}
		}
	}

	stats_.clear();	
}

int Data::uniqueOwnerId_ = 0;
DataList::DataList()
{
	current_ = data_.begin();
}
DataList::~DataList()
{

}

void DataList::setToFirst()
{
	current_ = data_.begin();
}

Data* DataList::current()
{
	return *current_;
}

bool DataList::hasMore()
{
	return ( current_ != data_.end() );
}

void DataList::next()
{
	++current_;
}

void DataList::add(Data* data)
{
	data_.push_back(data);
}
