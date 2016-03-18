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


/*! \file SpotDecoder.h
    \brief Implementation of the Template class SpotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/
 

#include <locale>
#include "EpsBufr.h"
#include "MetaData.h"
#include "CustomisedPoint.h"
#include "DateTime.h"
#include "TextVisitor.h"
#include "MvObs.h"

using namespace magics;

    
void EpsBufr::visit(Transformation& transformation)
{
	decode();
	transformation.setDataMinMaxX((minstep_ - shift_) * 3600, (maxstep_ + 6) * 3600, base_);
	if ( same(miny_, maxy_) )
	        	maxy_ = miny_ +5.;
	transformation.setDataMinMaxY(miny_, maxy_);
}
		
void EpsBufr::decode()
{
	if ( !points_.empty()) return;
	
     MagLog::dev() << "EpsBufr::decode()-->" << *this << endl;		
     // Test
     MvObsSet set(path_.c_str());
     
     MvObsSetIterator   filter(set);
     MvLocation ll(latitude_-0.01, longitude_-0.01);
     MvLocation ur(latitude_+0.01, longitude_+0.01);
     filter.setArea(ll, ur);
     
     MvObs obs = filter();
    
     map<int, float > curve1; 
     map<int, float > curve2; 
     
     minstep_ = std::numeric_limits<double>::max();
     maxstep_ = -std::numeric_limits<double>::max();
     miny_ = std::numeric_limits<double>::max();
     maxy_ = -std::numeric_limits<double>::max();

     double previous = 0;
     while (obs)
     {
             //MagLog::dev()<< "obs---------------------------------------------------------------------------" << endl;		
     		MvLocation loc = obs.location();
//      	  int subsets = obs.msgSubsetCount();

       		float value = obs.value(param_descriptor_ );

        	if ( value == kBufrMissingValue )
		{
        		obs = filter(NR_returnMsg);         	// Were going to the next message!
        		continue;
        	}
        	
    		int i = 0;
    		 
    		while ( ++i  )
		{
    			// 0424 is the bufr descriptor for Time Period Or Displacement [HOUR]
                
    			float step =  obs.valueByOccurrence(i, 4024);  
    			float value =  obs.valueByOccurrence(i, param_descriptor_);
    			if ( value == kBufrMissingValue ) {
    				obs = filter(); // We going to the next subset!
    				break;
    			}
    			value = (value * param_scaling_factor_ ) + param_offset_factor_;
    			
    			base_ = DateTime(obs.obsTime().CharValue());

    		
    			if ( accumulated_) {
    				double total = value;
    				value = value - previous;
    				previous = total;
    			}

    			if  ( minstep_ > step ) minstep_ = step;
    			if  ( maxstep_ < step ) maxstep_ = step;
    			if  ( miny_ > value ) miny_ = value;
    			if  ( maxy_ < value ) maxy_ = value;

    			curve1[step] = value;
    			MagLog::dev() << param_descriptor_ << " : " << step << "--->" << value << endl;
    		}
     	}
     if ( param_descriptor_2_) { 
    	 MvObsSet set(path_.c_str());
     
     MvObsSetIterator   filter(set);
     MvLocation ll(latitude_-0.1, longitude_-0.1);
     MvLocation ur(latitude_+0.1, longitude_+0.1);
     filter.setArea(ll, ur);
    MvObs obs2 = filter();
     while (obs2)
         {
                 //MagLog::dev()<< "obs---------------------------------------------------------------------------" << endl;		
         	MvLocation loc = obs2.location();
    //        int subsets = obs.msgSubsetCount();

            float value = obs2.value( param_descriptor_2_ );
            
            	if ( value == kBufrMissingValue )
    		{
            		obs2 = filter(NR_returnMsg);         	// Were going to the next message!
            		continue;
            	}
            	
        		int i = 0;
        		 
        		while ( ++i  )
    		{
        			// 0424 is the bufr descriptor for Time Period Or Displacement [HOUR]
        			float step =  obs2.valueByOccurrence(i, 4024); 
        			obs2.valueByOccurrence(i, 5195);  			
        			float value =  obs2.valueByOccurrence(i, param_descriptor_2_);
        			if ( value == kBufrMissingValue ) {
        				obs2 = filter(); // We going to the next subset!
        				break;
        			}

        			curve2[step] = value;
        		}
         	}
     }
    
	for (map<int, float>::const_iterator step = curve1.begin(); step != curve1.end(); ++step) {
		CustomisedPoint* point = new CustomisedPoint();		
		point->longitude(step->first * 3600);		 
		point->latitude(step->first * 3600);		 
		DateTime date  = base_ +  Second(step->first * 3600);    
		(*point)["step"]    = step->first * 3600;
		(*point)["shift"] = minstep_ - shift_ *3600;
		MagLog::dev() << step->first << "--->" << step->second << endl;
		(*point)["year"]    = date.date().year();
		(*point)["month"]   = date.date().month();
		(*point)["day"]     = date.date().day();
		(*point)["hours"]   = date.time().hours();
		(*point)["minutes"] = date.time().minutes();
		(*point)["seconds"] = date.time().seconds();
		(*point)["width"]    = 1 * 3600;
		(*point)["control"] = step->second;	
		(*point)["curve1"] = step->second;	
		if ( param_descriptor_2_ ) 
			(*point)["curve2"] = curve2[step->first];	
		points_.push_back(point);
	}
}

void EpsBufr::customisedPoints(const std::set<string>&, CustomisedPointsList& out)
{
	MagLog::dev() << "EpsBufr::customisedPoints-->need to be implemented" <<endl;
	MagLog::dev() << *this << endl;
	decode();

	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point)
			out.push_back(*point);	
}

PointsHandler& EpsBufr::points()
{
}

void EpsBufr::visit(TextVisitor& title)
{
	decode();
	title.update("json", "parameter_info", param_title_);
	ostringstream station;
	UserPoint position(longitude_, latitude_);
	station << station_name_ << "(" << position.asLatitude() << ", " << position.asLongitude() << ")" << endl;
	title.update("json", "station_info", station.str());
	title.update("json", "date", base_.tostring("ECMWF Forecast from %A %e %B %Y %H UTC"));


	return;

}

void EpsBufr::visit(MetaDataVisitor&)
{
}
	
void EpsBufr::print(ostream& out) const
{
	out << "EpsBufr[";
	EpsBufrAttributes::print(out);
	out << "]";
}

EpsBufr::EpsBufr() : shift_(12)
{
}

EpsBufr::~EpsBufr()
{
}
