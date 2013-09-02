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

/*! \file TableDecoder.cc
    \brief Implementation of the Template class TableDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 6-May-2004
    
    Changes:
    
*/

#include "TableDecoder.h"
#include "TableReader.h"
#include "Tokenizer.h"
#include "TextVisitor.h"
#include "SciMethods.h"

using namespace::magics;


TableDecoder::TableDecoder() : matrix_(0)
{
	MetviewIcon::setInfo("_datatype","TABLE");
	MetviewIcon::setInfo("_description","Table Data");
}


/*!
 Class information are given to the output-stream.
*/
void TableDecoder::print(ostream& out)  const
{
	out << "TableDecoder[";
    TableDecoderAttributes::print(out);
	out << "]";
}


void  TableDecoder::dateSetting(vector<string>& dates, vector<double>& values, DateTime& base)
{
	if ( dates.empty() )
		return;
	base = DateTime(dates.front());
	for (vector<string>:: iterator date = dates.begin(); date != dates.end(); ++ date ) {
		DateTime d(*date);
		values.push_back(d-base);
	}
}


void  TableDecoder::numberSetting(vector<double>& from, vector<double>& values)
{
	std::copy(from.begin(), from.end(), back_inserter(values));
}



namespace magics {

void TableDecoder::nameGeoMode(TableReader& reader)
{
	x_name_ = this->lon_;
	y_name_ = this->lat_;
	reader.setFieldContainer(-1, y_name_, this->y_values_, -999);
	reader.setFieldContainer(-1, x_name_, this->x_values_, -999);


	if ( !this->values_.empty() && this->values_ != "-1") {
		v_name_ = this->values_;
		reader.setFieldContainer(-1, v_name_, this->v_values_, -999);
	}
	if ( !this->x_component_.empty() && this->x_component_ != "-1") {
		xc_name_ = this->x_component_;
		reader.setFieldContainer(-1, xc_name_, this->xc_values_, -999);
	}
	if ( !this->y_component_.empty()  && this->y_component_ != "-1") {
		yc_name_ = this->y_component_;
		reader.setFieldContainer(-1, yc_name_, this->yc_values_, -999);
	}
}


void TableDecoder::indexGeoMode(TableReader& reader)
{
	int x_index_ = indexToNumber(this->lon_) -1;
	int y_index_ = indexToNumber(this->lat_) -1;
	int val_index= indexToNumber(this->values_);
	int xc_index= indexToNumber(this->x_component_);
	int yc_index= indexToNumber(this->y_component_);

	reader.setFieldContainer(x_index_, x_name_, this->x_values_, -999);
	reader.setFieldContainer(y_index_, y_name_, this->y_values_, -999);


	if ( val_index != -1 ) {
		reader.setFieldContainer(val_index -1, v_name_, this->v_values_, -999);
	}
	if ( xc_index != -1 ) {
		reader.setFieldContainer(xc_index -1, xc_name_, this->xc_values_, -999);
	}
	if ( yc_index != -1 ) {
		reader.setFieldContainer(yc_index -1, yc_name_, this->yc_values_, -999);
	}
}


void  TableDecoder::prepareGeo()
{
	  TableReader reader;

	  bool ok;
	  string error;
	  reader.setPath(this->path_);
	  reader.setHeaderRow(this->header_row_);
	  if ( !this->delimiter_.empty() )
		  reader.setDelimiter(this->delimiter_[0]);
	  reader.setConsecutiveDelimitersAsOne(this->combine_delimiters_);
	  reader.setDataRowOffset(this->data_row_offset_);
	  reader.setUserMetaDataRows(this->meta_data_rows_);

	  vector<string> names = reader.fieldNames();


  if ( !this->x_values_.empty() ) return;

  // first set the containers!!
  if ( magCompare(this->name_mode_, "name" ) )
	  nameGeoMode(reader);
  else
	  indexGeoMode(reader);


  // Then read and interpret!
   ok = reader.read(error);
   if ( !ok ) {
  	 MagLog::error() << error << endl;
  	 return;
   }
  
  vector<double>::iterator x = this->x_values_.begin();
	vector<double>::iterator y = this->y_values_.begin();
	vector<double>::iterator v = this->v_values_.begin();

  while ( x != this->x_values_.end() && x != this->x_values_.end() ) {
  	double val = 0;
  	if ( v != v_values_.end() ) {
  		val = *v;
  		++v;
  	}
  	push_back(new UserPoint(*x, *y, val));
  	++x;
  	++y;
  }

  	//Meta-data
	map<string,string> meta=reader.userMetaData();	
	
	map<string,string>::const_iterator metaIt;
  	if( (metaIt=meta.find("Metview::type")) != meta.end() &&
      	     metaIt->second == "FLEXTRA")
  	{
		setInfo("_datatype","TABLE_flextra");
		setInfo("_description","FLEXTRA Trajectories");
	}
	
	for(map<string,string>::const_iterator it=meta.begin(); it != meta.end(); it++)
  	{
    		setInfo(it->first,it->second);
  	}	
}


void TableDecoder::nameXYMode(TableReader& reader)
{
	 x_name_ = this->x_;
	 y_name_ = this->y_;
	 if ( magCompare(this->x_type_, "date" ) ) {
		 reader.setFieldContainer(-1, x_name_, this->x_date_values_, "none");
	 }
	 else {
		 reader.setFieldContainer(-1, x_name_, this->x_values_, -999);
	 }
	 if ( magCompare(this->y_type_, "date" ) ) {
		 reader.setFieldContainer(-1, y_name_, this->y_date_values_, "none");
	  }
	  else {
		reader.setFieldContainer(-1, y_name_, this->y_values_, -999);
	  }

	 if ( !this->values_.empty() && this->values_ != "-1" ) {
		 v_name_ = this->values_;
		 reader.setFieldContainer(-1, v_name_, this->v_values_, -999);
	 }
	 if ( !this->x_component_.empty() && this->x_component_ != "-1" ) {
		 xc_name_ = this->x_component_;
		 reader.setFieldContainer(-1, xc_name_, this->xc_values_, -999);
	 }
	 if ( !this->y_component_.empty() && this->y_component_ != "-1") {
		 yc_name_ = this->y_component_;
		 reader.setFieldContainer(-1, yc_name_, this->yc_values_, -999);
	 }
}


void TableDecoder::indexXYMode(TableReader& reader)
{
	int x_index = indexToNumber(this->x_) -1;
	int y_index = indexToNumber(this->y_) -1;
	int val_index= indexToNumber(this->values_);
	int xc_index= indexToNumber(this->x_component_);
	int yc_index= indexToNumber(this->y_component_);

	if ( magCompare(this->x_type_, "date" ) ) {

	    		reader.setFieldContainer(x_index, x_name_, this->x_date_values_, "none");
	     }
	     else {
	    	 reader.setFieldContainer(x_index, x_name_, this->x_values_, -999);
	     }
	     if ( magCompare(this->y_type_, "date" ) ) {
	    	 reader.setFieldContainer(y_index, y_name_, this->y_date_values_, "none");
	      }
	      else {
	    	reader.setFieldContainer(y_index, y_name_, this->y_values_, -999);
	      }

	     if ( val_index != -1 ) {
	    	 reader.setFieldContainer(val_index-1, v_name_, this->v_values_, -999);
	     }
	     if ( xc_index != -1 ) {
	    	 reader.setFieldContainer(xc_index-1, xc_name_, this->xc_values_, -999);
	     }
	     if ( yc_index != -1 ) {
	    	 reader.setFieldContainer(yc_index-1, yc_name_, this->yc_values_, -999);
	     }
}


void  TableDecoder::prepareXY()
{
	  TableReader reader;

	  bool ok;
	  string error;
	  reader.setPath(this->path_);
	  reader.setHeaderRow(this->header_row_);
	  if ( !this->delimiter_.empty() )
		  reader.setDelimiter(this->delimiter_[0]);
	  reader.setConsecutiveDelimitersAsOne(this->combine_delimiters_);
	  reader.setDataRowOffset(this->data_row_offset_);
	  reader.setUserMetaDataRows(this->meta_data_rows_);

	  vector<string> names = reader.fieldNames();


    if ( !this->x_values_.empty() ) return;

    // first set the containers!!
    // first set the containers!!
    if ( magCompare(this->name_mode_, "name" ) )
    	  nameXYMode(reader);
      else
    	  indexXYMode(reader);
    // Then read and interpret!
     ok = reader.read(error);
     if ( !ok) {
    	 MagLog::error() << error << endl;
    	 return;
     }

     // Now we interpret
    if ( magCompare(this->x_type_, "date" ) ) {
    	this->dateSetting(this->x_date_values_, this->x_values_, baseDateX_);
    }

    if ( magCompare(this->y_type_, "date" ) ) {
      	this->dateSetting(this->y_date_values_, this->y_values_, baseDateY_);

     }

    vector<double>::iterator x = this->x_values_.begin();
	vector<double>::iterator y = this->y_values_.begin();
	vector<double>::iterator v = this->v_values_.begin();

    while ( x != this->x_values_.end() && x != this->x_values_.end() ) {
    	double val = 0;
    	if ( v != v_values_.end() ) {
    		val = *v;
    		++v;
    	}
    	push_back(new UserPoint(*x, *y, val));
    	if ( same(*x, x_missing_) ||  same(*y, y_missing_) )
    		back()->flagMissing();
    	++x;
    	++y;
    }

	//Meta-data
	map<string,string> meta=reader.userMetaData();	
	
	map<string,string>::const_iterator metaIt;
  	if( (metaIt=meta.find("Metview::type")) != meta.end() &&
      	     metaIt->second == "FLEXTRA")
  	{
		setInfo("_datatype","TABLE_flextra");
		setInfo("_description","FLEXTRA Trajectories");
	}
	
	for(map<string,string>::const_iterator it=meta.begin(); it != meta.end(); it++)
  	{
    		setInfo(it->first,it->second);
  	}	
}


void TableDecoder::customisedPoints(const Transformation& transformation, const std::set<string>&, CustomisedPointsList& out)
{
	if ( transformation.coordinateType() == Transformation::GeoType )
		prepareGeo();
	else
		prepareXY();

	vector<double>::const_iterator x = x_values_.begin();
	vector<double>::const_iterator y = y_values_.begin();
	vector<double>::const_iterator xc = this->xc_values_.begin();
	vector<double>::const_iterator yc = this->yc_values_.begin();
	vector<double>::const_iterator v = this->v_values_.begin();
	while (  x != x_values_.end() && y != y_values_.end()  ) {
				double valx = *x;
				double valy = *y;
				if ( magCompare(this->x_type_, "date" ) ) {
					DateTime ref(transformation.getReferenceX());
					double shift = ref - this->baseDateX_;
					valx -= shift;
				}
				if ( magCompare(this->y_type_, "date" ) ) {
					DateTime ref(transformation.getReferenceY());
					double shift = ref - this->baseDateY_;
					valy -= shift;
				}
		    	CustomisedPoint* point = new CustomisedPoint();
		    	bool todelete = true;
		    	if ( transformation.in(valx, valy) ) {
		    		point->longitude(valx);
		    		point->latitude(valx);
		    		out.push_back(point);
		    		todelete = false;
		    	}


		    	(*point)["x"] = valx;
		    	++x;
		    	(*point)["y"] = valy;
		    	++y;


		        double speed = 0;
		    	if ( xc != this->xc_values_.end() && yc != this->yc_values_.end() ) {
		    		speed = sqrt( (*xc * *xc)  +  (*yc * *yc) );
		    		(*point)["x_component"] = *(xc++);
		    		(*point)["y_component"] = *(yc++);
		    	}
		    	if ( v != this->v_values_.end() ) {
		    		(*point)["colour_component"] = *(v++);
		    	}
		    	else
		    		(*point)["colour_component"] = speed;

		        if ( todelete ) delete point;
		    	
		    }

}
  	
  	
MatrixHandler& TableDecoder::matrix()
{



	if  ( !matrix_ ) {
		prepareXY();
		matrix_ = (*table_binning_)(*this);
	}

	this->matrixHandlers_.push_back(new MatrixHandler(*matrix_));

	return *(this->matrixHandlers_.back());
}


void TableDecoder::getReady(const Transformation& transformation)
{
	/*
	try {
		for ( vector<string>::const_iterator x = date_x_.begin(); x != date_x_.end(); ++x )
			x_.push_back(transformation.x(*x));
		}

	catch (...) {}
	try {
		for ( vector<string>::const_iterator y = date_y_.begin(); y != date_y_.end(); ++y )
			y_.push_back(transformation.y(*y));
	}
	catch (...) {}
	*/
}

void TableDecoder::visit(Transformation& transformation)
{
	// get the data ...
	try {
		if ( transformation.coordinateType() == Transformation::GeoType )
			prepareGeo();
		else
			prepareXY();

		if ( transformation.getAutomaticX() ) {

			if ( magCompare(this->x_type_, "date" ) ) {
				double min = ( this->x_values_.empty() ) ? 0 : *std::min_element(this->x_values_.begin(), this->x_values_.end());
				double max = ( this->x_values_.empty() ) ? 24*3600 : *std::max_element(this->x_values_.begin(), this->x_values_.end());
				transformation.setDataMinX(min, this->baseDateX_);
				transformation.setDataMaxX(max, this->baseDateX_);
			}
			else {
				double min = ( this->x_values_.empty() ) ? 0 : *std::min_element(this->x_values_.begin(), this->x_values_.end());
				double max = ( this->x_values_.empty() ) ? 100 : *std::max_element(this->x_values_.begin(), this->x_values_.end());
				transformation.setMinX(min);
				transformation.setMaxX(max);
			}
		}
		if ( transformation.getAutomaticY() ) {
			if ( magCompare(this->y_type_, "date" ) ) {
				double min = ( this->y_values_.empty() ) ? 0 : *std::min_element(this->y_values_.begin(), this->y_values_.end());
				double max = ( this->y_values_.empty() ) ? 24*3600 : *std::max_element(this->y_values_.begin(), this->y_values_.end());

				transformation.setDataMinY(min, this->baseDateY_);
				transformation.setDataMaxY(max, this->baseDateY_);
			}
			else {
				double min = ( this->y_values_.empty() ) ? 0 : *std::min_element(this->y_values_.begin(), this->y_values_.end());
				double max = ( this->y_values_.empty() ) ? 100 : *std::max_element(this->y_values_.begin(), this->y_values_.end());

				transformation.setMinY(min);
				transformation.setMaxY(max);
			}
		}
	}
	catch ( ... ) {}
}


void TableDecoder::customisedPoints(const std::set<string>&, CustomisedPointsList& out)
{
	prepareXY();
	vector<double>::const_iterator x = x_values_.begin();
	vector<double>::const_iterator y = y_values_.begin();
	vector<double>::const_iterator xc = this->xc_values_.begin();
	vector<double>::const_iterator yc = this->yc_values_.begin();
	vector<double>::const_iterator v = this->v_values_.begin();
	while (  x != x_values_.end() || y != y_values_.end()  ) {
		    	CustomisedPoint* point = new CustomisedPoint();

		    		point->longitude(*x);
		    		point->latitude(*y);
		    		out.push_back(point);

		    	if ( x != x_values_.end() ) (*point)["x"] = *(x++);
		    	if ( y != y_values_.end() ) (*point)["y"] = *(y++);
		        double speed = 0;
		    	if ( xc != this->xc_values_.end() && yc != this->yc_values_.end() ) {
		    		speed = sqrt( (*xc * *xc)  +  (*yc * *yc) );
		    		(*point)["x_component"] = *(xc++);
		    		(*point)["y_component"] = *(yc++);
		    	}
		    	if ( v != this->v_values_.end() ) {
		    		(*point)["colour_component"] = *(v++);
		    	}
		    	else
		    		(*point)["colour_component"] = speed;
	}
}


void TableDecoder:: visit(TextVisitor& title)
{
	//if ( !title_.empty() )
	//	title.add(new TextEntry(title_));

	if(MetviewIcon::info("_datatype") == "TABLE_flextra")  
	{	
	  	string s="FLEXTRA: ";		

		string date=MetviewIcon::info("startDate");
		string time=MetviewIcon::info("startTime");
		string lat=MetviewIcon::info("startLat");
		string lon=MetviewIcon::info("startLon");
		string lev=MetviewIcon::info("startZ");
				
		s+=MetviewIcon::info("direction") + " " + MetviewIcon::info("type");
		
		if(!date.empty())
		{
			s+= " " + date;
			if(!time.empty())
			{
				s+= " " + time;
			}	
		}	
			
		if(!lev.empty())
		{
			s+= " " + lev +  "m";
		}
		
		if(!lat.empty() && !lon.empty())
		{
			s+= " " + MetviewIcon::info("startComment") + " (" + lon + ", " + lat + ")";
		}


		title.addAutomaticTitle(s);
	}
}


string TableDecoder::legendText(double valMin,double valMax)
{
	if(MetviewIcon::info("_datatype") == "TABLE_flextra")  
	{
		ostringstream sst;
	    	sst << valMin; 
		string valStr=sst.str();
		
		string id,date,time,lev;		
		id=MetviewIcon::info("id_TR");
		date=MetviewIcon::info("startDate_TR");
		time=MetviewIcon::info("startTime_TR");
		lev=MetviewIcon::info("startZ_TR");

		if(id.empty() || time.empty() || date.empty() || lev.empty())
		{
		  	return string();
		}
		
		Tokenizer tk("/");
		vector<string> idVec,dateVec,timeVec,levVec;
		tk(id,idVec);
		tk(date,dateVec);
		tk(time,timeVec);
		tk(lev,levVec);		
		
		if(idVec.size() != timeVec.size() || timeVec.size() != dateVec.size() || 
		     dateVec.size() != levVec.size() )
		{
		  	return string();
		}
		
		for(unsigned int i=0; i < idVec.size(); i++)
		{
		  	if(idVec[i] == valStr)
			{
				return dateVec[i].substr(dateVec[i].size()-2,2) + "/" + timeVec[i].substr(0,2) + 
				" " + levVec[i] + " m";
			}
		}
	}
	return string();
}


PointsHandler& TableDecoder::points(const Transformation& transformation, bool all)
{

	// fisrt we adjust point!
	// we create another poin list with adjisted points!
	PointsList* list = new PointsList();

	for (vector<UserPoint*>::iterator point = begin(); point != end(); ++point) {
		UserPoint *pt = new UserPoint(**point);
		stack<UserPoint> duplicates;
		check(transformation, pt, duplicates);
		while ( !duplicates.empty() ) {
			list->push_back(new UserPoint(duplicates.top()));
			duplicates.pop();
		}
	}

	// then we filter them  we filter the points
	if ( all )
		this->pointsHandlers_.push_back(new PointsHandler(*list));
	else
		this->pointsHandlers_.push_back(new BoxPointsHandler(*list, transformation, true));

	return *(this->pointsHandlers_.back());
}


bool TableDecoder::check(const Transformation& transformation, UserPoint* point,stack<UserPoint>& duplicates )
{
	// We need to adjust the point if we have a date axis!
	if ( magCompare(x_type_, "date" ) ) {
		DateTime ref(transformation.getReferenceX());
		double shift = ref - baseDateX_;
		point->x_ -= shift;
	}
	if ( magCompare(y_type_, "date" ) ) {
		DateTime ref(transformation.getReferenceY());
		double shift = ref - baseDateY_;
		point->y_ -= shift;
	}

	transformation.wraparound(*point, duplicates);
	if ( duplicates.empty()  ) {
		// we falg the point and add to the list!
		point->flagMissing();
		duplicates.push(*point);
		return false;// point has been adjusted and is not in the transformation view
	}
	return true; // point and eventual duplicates! has been adjusted and is in the transformation view!
}


void TableDecoder::customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool)
{
	customisedPoints(t, n, out);
}


void TableDecoder::visit(ValuesCollector& points)
{

	const Transformation& transformation = points.transformation();
	
	points.setCollected(true);
  	 
	if(points.size() <=0 || size() == 0)
	  	return;
	
	if(values_.empty())
		points.setHasValue(false);
		
	for (ValuesCollector::iterator point =  points.begin(); point != points.end(); ++point)
	{
	  	double y=(*point).y(); //lat
	  	double x=(*point).x(); //lon
		
		vector<int> idxV;		
		for(int i=0; i < size(); i++)
		{
			if(fabs(at(i)->y()-y) < points.searchRadiusY() &&
			   fabs(at(i)->x()-x) <  points.searchRadiusX())
			{
			  	idxV.push_back(i);
			}
		}
		
		if(idxV.size() ==0)
			continue;  
		
		double dist=10000000.;
		int minIdx=-1;
		
		//MagLog::debug() << "odb collect idxV : " << lat << " " << lon << " " << idxV.size() << endl;
 		
		for(int i=0; i < idxV.size(); i++)
		{  			
		  	int idx=idxV[i];
			double d;
			if(transformation.coordinateType() == Transformation::GeoType )
			{  
				d=magics::geoDistanceInKm(at(idx)->y(),at(idx)->x(),y,x);
			}
			else
			{
			  	d=(at(idx)->x()-x)*(at(idx)->x()-x) +
			        	 (at(idx)->y()-y)*(at(idx)->y()-y);
			}
			  					
			if(d < dist)
			{
			  	minIdx=idx;
				dist=d;
			}			
		}	
		if(minIdx>=0)  
			(*point).push_back(new ValuesCollectorData(at(minIdx)->x(),
							       at(minIdx)->y(),
							       at(minIdx)->value(),
							       dist));					     			
	}	  
}

double TableDecoder::indexToNumber(const string& str)
{
	if ( str.empty() )
		return -1;
	
	double r;
	std::stringstream ss(str);
	ss >> r;
	return r;
}

} //end namespace
