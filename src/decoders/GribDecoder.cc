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

/*! \file GribDecoder.cc
    \brief Implementation of the Template class GribDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 16-Mar-2004
    
    Changes:
    
*/
 
#include "GribDecoder.h"
#include "Factory.h"
#include <limits>
#include <unistd.h>
#include "TitleTemplate.h"
#include "LocalTable.h"
#include "DateTime.h"
#include "GribTables.h"
#include "GribInterpretor.h"
#include "XmlReader.h"
#include "TextVisitor.h"
#include "Timer.h"
#include "VisualAction.h"
#include "AnimationRules.h"
#include "Transformation.h"
/*
#include "grib_api_internal.h"


string getLongNameCentre(int centre)
{
        grib_context* context = grib_context_get_default();
        grib_codetable* t = context->codetable;
        while(t)
        {
            grib_codetable* s = t->next;
            for(int i = 0; i < t->size; i++)
            {
                char* the_title = t->entries[i].title;
                char* the_recomposed_name = t->recomposed_name[0];
                if (strcmp(the_recomposed_name, "grib1/0.table") ==0 )
                {
                    if (the_title && i==centre)
                    {
                        printf("title for centre %d is %s\n", i, t->entries[i].title);
                        return t->entries[i].title;
                    }
                }
            }
            t = s;
        }
}
*/
using namespace magics;

int  GribDecoder::count_ = 0;

GribDecoder::GribDecoder() :  matrix_(0),  xComponent_(0), yComponent_(0),
		colourComponent_(0), handle_(0), interpretor_(0),
		field_(0), component1_(0), component2_(0), colour_(0)
{
	count_++;
	title_ = "grib_" + tostring(count_);
	version();
}

void GribDecoder::version()
{
	static bool done = false;
	if ( done ) return;
	done = true;
	MagLog::info() << "GribAPI Version :" << grib_get_api_version() << endl;
}

GribDecoder::~GribDecoder() 
{
	if ( matrix_) delete matrix_;
	if ( xComponent_ ) delete xComponent_;
	if ( yComponent_ ) delete yComponent_;
	if ( handle_ ) {

		grib_handle_delete (handle_);

	}
	for (PointsList::iterator point = points_.begin(); point != points_.end(); ++point) {
		delete *point;
		*point = 0;
	}
	if ( interpretor_ ) delete interpretor_;

	//context is a reference to a global context and should not be deleted
}

void GribDecoder::set(const GribLoop& loop, int index)
{
	// Copy the information about scaling...
	scaling_         = loop.scaling_;
	derived_scaling_ = loop.derived_scaling_;
	scaling_offset_  = loop.scaling_offset_;
	scaling_factor_  = loop.scaling_factor_;
	index_           = loop.uniqueId_;
	wind_mode_       = auto_ptr<WindMode>(loop.wind_mode_->clone());
	internalIndex_ = index;

}

long GribDecoder::getLong(const string& key, bool warnIfKeyAbsent) const
{
	long val;
	map<string, long>::const_iterator lk = lKeys_.find(key);
	if ( lk != lKeys_.end() ) {
		return lk->second;
	}
	int err = grib_get_long(handle_, key.c_str(), &val);
	if ( err )
	{
		if (warnIfKeyAbsent)
		{
			MagLog::warning() << "Grib API: can not find key [" << key << "]\n"
			               << grib_get_error_message(err) <<"\n";
		}
		return 0;
	}
	lKeys_.insert(make_pair(key, val));
	return val;
}

string GribDecoder::getstring(const string& key, bool warnIfKeyAbsent, bool cache) const
{

	if ( cache ) {
		map<string, string>::const_iterator sk = sKeys_.find(key);
		if ( sk != sKeys_.end() ) {
			return sk->second;
		}
	}
	char val[1024];
	size_t length = 1024;

	int err = grib_get_string(handle_, key.c_str(), val, &length);

	if ( err )
	{
		if (warnIfKeyAbsent)
		{
				MagLog::warning() << "Grib API: can not find key [" << key << "]\n"
				               << grib_get_error_message(err) <<"\n";
			}
			return "";
	}
	if ( cache )
		sKeys_.insert(make_pair(key, val));
	return string(val);
}

string GribDecoder::getString(const string& key, bool warnIfKeyAbsent) const
{

	if ( Data::dimension_ == 1 )
		return getstring(key, warnIfKeyAbsent, false);

	string value;
	grib_handle* handle = handle_;
	// otherwise we build a name...

	GribDecoder* grib = const_cast<GribDecoder*>(this);
	grib->openFirstComponent();
	string name1 = getstring(key, warnIfKeyAbsent, false);
	grib->openSecondComponent();
	string name2 = getstring(key, warnIfKeyAbsent, false);
	grib->handle(handle);
	value = (name1==name2) ? name1 : name1 + "/" + name2;

	if ( Data::dimension_ == 3 ) {
		grib->openThirdComponent();
		string name3 = getstring(key, warnIfKeyAbsent, false);
		grib->handle(handle);
		if ( value != name3 )
			value = value + "/" + name3;
	}
	return value;
}


double GribDecoder::getDouble(const string& key, bool warnIfKeyAbsent) const
{
	map<string, double>::const_iterator dk = dKeys_.find(key);
		if ( dk != dKeys_.end() ) {
			return dk->second;
		}
	double val;
	int err = grib_get_double(handle_, key.c_str(), &val);
	if ( err )
	{
		if (warnIfKeyAbsent)
		{
			MagLog::warning() << "Grib API: can not find key [" << key << "]\n"
			               << grib_get_error_message(err) <<"\n";
		}
		return 0;
	}
	dKeys_.insert(make_pair(key, val));
	return val;
}


void   GribDecoder::setDouble(const string& key, double val) const
{

	int err = grib_set_double(handle_, key.c_str(), val);
	if ( err )
	{
		MagLog::warning() << "Grib API: can not find key [" << key << "]\n"
		               << grib_get_error_message(err) <<"\n";
	}
}


void GribDecoder::read(Matrix **matrix)
{

	if ( handle_ <=0 ) {
		*matrix = 0;
		return;
	}
	long repres;
	grib_get_long(handle_,"dataRepresentationType",&repres);
	const string representation = getString("typeOfGrid");

	try {
		if ( !interpretor_ ) {
			interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
		}
		interpretor_->interpretAsMatrix(*this, matrix);
		if ( *matrix == 0 ) {
			 valid_ = false;
			 ostringstream msg;
			 msg << "Grib Decoder: Representation [" << representation << "] not yet fully implemented";
			 MagLog::error() <<  msg.str() <<endl;
			 throw MagicsException(msg.str());
		}

		interpretor_->scaling(*this, matrix);
	}
	catch (NoFactoryException&)
	{
		ostringstream msg;
		msg << "Grib Decoder: Representation [" << representation << "] not yet supported";
		MagLog::error() << msg.str() << endl;
		valid_ = false;
		throw MagicsException(msg.str());
	}
}


void GribDecoder::read(Matrix **matrix, const Transformation&  transformation)
{
	if ( handle_ <=0 ) {
		*matrix = 0;
		return;
	}
	long repres;    
	grib_get_long(handle_,"dataRepresentationType",&repres);
	const string representation = getString("typeOfGrid");

	try {
		if ( !interpretor_ ) {
					interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
				}
		interpretor_->interpretAsMatrix(*this, matrix, transformation);
		interpretor_->scaling(*this, matrix);
	}
	catch (NoFactoryException&)
	{
		MagLog::error() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
		valid_ = false;
		throw MagicsException("Grib Decoder: Representation [] not yet supported.");
	}
}


/*!
 Class information are given to the output-stream.
*/		
void GribDecoder::print(ostream& out)  const
{
	out << "GribDecoder[";
	GribDecoderAttributes::print(out);
	out << "] ";
}

void GribDecoder::release()
{
	if ( matrix_ ) {
		delete matrix_;
		matrix_ = 0;
	}
	if ( xComponent_ ) {
		delete xComponent_;
		xComponent_ = 0;
	}
	if ( yComponent_ ) {
		delete yComponent_;
		yComponent_ = 0;
	}
	if ( colourComponent_ ) {
			delete colourComponent_;
			colourComponent_ = 0;
		}

	for (PointsList::iterator point = points_.begin(); point != points_.end(); ++point) {
				delete *point;
				*point = 0;
		}
	points_.clear();

}
void GribDecoder::visit(Transformation& transformation)
{

	if(transformation.coordinateType() == Transformation::GeoType )
		return;
	decode();
	// Here are in a dump ode .. the coordinates are pixels.
	if ( transformation.getAutomaticX() ) {
		transformation.setMinX(1);
		transformation.setMaxX(matrix_->columns());
	}
	if ( transformation.getAutomaticY() ) {
		transformation.setMinY(1);
		transformation.setMaxY(matrix_->rows());
	}



}
void GribDecoder::decode2D() 
{

	if (xComponent_) return;
	Matrix     *w1 = 0;
	Matrix     *w2 = 0;
	colourComponent_ = 0;
	const string representation = getString("typeOfGrid");
	try {

		if ( !interpretor_ ) {
					interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
		}
		interpretor_->keepOriginal(true);

	}
	catch (NoFactoryException&)
	{
		MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
		valid_ = false;

	}
	readColourComponent();
	openFirstComponent();
	read(&w1);
	openSecondComponent();
	read(&w2);
    Data::dimension_ = ( colourComponent_ ) ? 3 : 2;



	wind_mode_->x(&xComponent_, &yComponent_, w1, w2);
	interpretor_->keepOriginal(false);
}


void GribDecoder::customisedPoints(const AutomaticThinningMethod& thinning, const Transformation& transformation, const std::set<string>& request, CustomisedPointsList& points)
{

	openFirstComponent();
	long repres;    
	grib_get_long(handle_,"dataRepresentationType",&repres);
	const string representation = getString("typeOfGrid");
	try {
		if ( !interpretor_ ) {
			interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
		}

		// Compute the thinning factor...



		double max = transformation.getMaxY();
		double min = transformation.getMinY();


		double res = interpretor_->XResolution(*this);


		bool thinit =  ( (max-min)/thinning.y() > 2*res );

		double xpoints = (thinit) ? thinning.x() : 0;
		double ypoints = (thinit) ? thinning.y() : 0;


		customisedPoints(transformation, points, xpoints, ypoints);



	}
	catch (NoFactoryException&)
	{
		MagLog::error() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
		throw MagicsException("Grib Decoder: Representation [] not yet supported.");
	}
}






bool compare(const pair<double, double>& pt1, const pair<double, double>& pt2)
{
	if ( pt1.second != pt2.second)
		return false;

	return pt1.second < pt2.second;
}





void GribDecoder::newPoint(const Transformation& transformation, double lat, double lon, double uc, double vc, double cc, vector<CustomisedPoint*>& points, double grid)
{



	std::stack<UserPoint>   duplicates;
  	UserPoint geo(lon, lat);




  	 transformation.wraparound(geo, duplicates);
		 while (duplicates.empty() == false) {
			UserPoint pt = duplicates.top();
			// we try to see if no to near neighborg ...

			map<double, std::set<double> >::iterator row =  positions_.find(pt.y());
			if ( row == positions_.end() )
				positions_.insert(make_pair(pt.y(), std::set<double>()));
			row =  positions_.find(pt.y());
			std::set<double>::iterator low =  row->second.lower_bound(pt.x());
			bool add = true;
			if ( low != row->second.end() ) {
				double diff = fabs(*low-pt.x());
				if ( diff <= grid )
					add = false;
				++low;
				if ( low != row->second.end() ) {
					double diff = fabs(*low -pt.x());
					if ( diff <= grid )
						add = false;
				}
			}
			if ( !add ) {
				duplicates.pop();
				continue;
			}

			row->second.insert(pt.x());

			CustomisedPoint *point = new CustomisedPoint(pt.x(), pt.y(), "");

			point->insert(make_pair("x_component", uc));
            point->insert(make_pair("y_component", vc));
		    points.push_back(point);
		    if ( cc != -9999 ) {
				point->insert(make_pair("colour_component", cc));

			}
		    duplicates.pop();

		 }
}
void GribDecoder::customisedPoints(const Transformation& transformation, CustomisedPointsList& out, double xpts, double ypts)
{
	bool thinit = (xpts && ypts);

	vector<UserPoint> xin, yin, cin;


	string xc = "x_component";
	string yc = "y_component";
	string cc = "colour_component";
	interpretor_->raw(*this, transformation, xc, xin);

	readColourComponent();
	if ( colourComponent_ ) {
		interpretor_->raw(*this, transformation, cc, cin);

	}
	openSecondComponent();
	interpretor_->raw(*this, transformation, yc, yin);

	vector<UserPoint>::iterator x = xin.begin();
	vector<UserPoint>::iterator y = yin.begin();
	vector<UserPoint>::iterator c = cin.begin();

	if (thinit)
		transformation.thin(xpts, ypts, xin);
	out.reserve(yin.size());
	while ( x != xin.end() && y!= yin.end() )
	{
		if ( x->missing() || !thinit ) {
			assert(x->x_ == y->x_);
			assert(x->y_ == y->y_);
			CustomisedPoint* point = new CustomisedPoint(x->x_, x->y_, "");
			pair<double, double> val = (*wind_mode_)(x->value_, y->value_);
			point->insert(make_pair(xc, val.first));
			point->insert(make_pair(yc, val.second));
			if ( c != cin.end() ) {
				point->insert(make_pair(cc, c->value_));
			}
			out.push_back(point);
		}
		x++;
		y++;
		if ( c != cin.end() ) {
			c++;
		}
	}



}

void GribDecoder::customisedPoints(const BasicThinningMethod& thinning, const Transformation& transformation, const std::set<string>&, CustomisedPointsList& points)
{

	openFirstComponent();
	long repres;
	grib_get_long(handle_,"dataRepresentationType",&repres);
	const string representation = getString("typeOfGrid");
	try {
		if ( !interpretor_ ) {
			interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
		}

		// Compute the thinning factor...

		double ypoints= 0;
		double xpoints= 0;
		if ( thinning.factor() > 1 ) {
			double x1 = 0;
			double y1 = 60;
			double x2 = interpretor_->XResolution(*this);
			double y2 = 60+interpretor_->XResolution(*this);

			transformation.fast_reproject(x1, y1);
			transformation.fast_reproject(x2, y2);

			double ymax = transformation.getMaxPCY();
			double ymin = transformation.getMinPCY();

			ypoints = abs((ymax-ymin)/((y2-y1)*thinning.factor()));


			double xmax = transformation.getMaxPCX();
			double xmin = transformation.getMinPCX();
			xpoints = abs((ymax-ymin)/((x2-x1)*thinning.factor()));




		}

		customisedPoints(transformation, points, xpoints, ypoints);



	}
	catch (NoFactoryException&)
	{
		MagLog::error() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
		throw MagicsException("Grib Decoder: Representation [] not yet supported.");
	}
}

void GribDecoder::decode2D(const Transformation&) 
{	
	Data::dimension_ = 2;
	if (xComponent_) return;
	Matrix     *w1 = 0;
	Matrix     *w2 = 0;



	const string representation = getString("typeOfGrid");

	try {
		if ( !interpretor_ ) {
					interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
				}
		readColourComponent();

			openFirstComponent();
			read(&w1);
			openSecondComponent();
			read(&w2);

	}
	catch (NoFactoryException&)
	{
		MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;

	}

	wind_mode_->x(&xComponent_, &yComponent_, w1, w2);
}


void GribDecoder::openFirstComponent() 
{


	grib_field_position_ =  position_1_;

	component1_ = open(component1_);

}
grib_handle* GribEntryDecoder::open(grib_handle* handle, bool) {
		if ( !handle_ ) {
			return handle_;
		}
		return handle;
}

void GribDecoder::openSecondComponent() 
{

	grib_field_position_ =  position_2_;
	component2_ = open(component2_);

}
void GribDecoder::openThirdComponent()
{
	grib_field_position_ =  colour_position_;
	colour_ = open(colour_, false);

}
void GribDecoder::readColourComponent()
{
	grib_field_position_ =  colour_position_;
	try {
		colour_ = open(colour_, false);
		read(&colourComponent_);

	}

	catch (...) {
		colourComponent_ = 0;
		valid_ = true;
	}



}

grib_handle*  GribDecoder::open(grib_handle* grib, bool sendmsg)
{
	if ( grib ) {
		handle(grib);
		return grib;
	}


	FILE* file = fopen(file_name_.c_str(),"r");
	handle_ = 0;
	if (!file)
	{
		MagLog::error() << "file can not be opened [" << file_name_ << "]" <<endl;
		MagLog::broadcast();
		valid_ = false;
		return 0;
	}

	handle_ = (*address_mode_)(0, file, grib_field_position_);

	if (handle_<=0 )
	{
		if (sendmsg) {
			MagLog::error() << "can not access position [" << grib_field_position_<<" in " << file_name_ << "]" <<endl;
			MagLog::broadcast();
			valid_ = false;
			return 0;
		}
	}
	fclose(file);

	return handle_;
}


bool GribDecoder::id(const string& id, const string& where) const
{
	if ( id_.empty() && id.empty() ){

		return ( verify(where) );
	}
	return magCompare(id_, id);
}
bool GribDecoder::verify(const string& val) const
{
	// we except a string with the following format "key1=val,key2=val2,...,keyn=valn"
			Tokenizer tokenizer(",= ");
			vector<string> tokens;
			map<string, string> where;
			tokenizer(val, tokens);
			vector<string>::iterator token = tokens.begin();

			string key, value;
			while (true) {
				if (token == tokens.end() )
					break;
				key = *token;
				++token;
				if (token == tokens.end() )
					break;
				value = *token;
				where.insert(make_pair(key, value));

				++token;
			}
	for (  map<string, string>::const_iterator w = where.begin(); w != where.end(); ++w) {
		string val = getString(w->first);
		if ( magCompare(val, w->second) == false )
			return false;
	}
	return true;
}

void GribDecoder::decodePoints()
{
	if ( !points_.empty() ) return;
	unsigned long flags=0;
	int error;

	if ( Data::dimension_ == 1 ) {
		double scaling;
		double offset;
		field_ = open(field_);
		const string representation = getString("typeOfGrid");
		double missing = getDouble("missingValue");
		try {
			if ( !interpretor_ ) {
						interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
					}
			interpretor_->scaling(*this, scaling, offset);
		}
		catch (NoFactoryException&)
		{
			MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
			scaling =1 ; offset =0;
		}

		grib_iterator* iter = grib_iterator_new(handle_, flags, &error);



		if (!iter)
		{
			MagLog::error() << "Grib Iterator not yet supported on this kind of grib\n";
			throw MagicsException("Grib Iterator not yet supported.");
		}

		double lat;
		double lon;
		double val;
		while (grib_iterator_next(iter,&lat,&lon,&val))
		{
			if ( val != missing)
				points_.push_back(new UserPoint(lon, lat, (val*scaling) + offset));
		}
		return;
	}

	openFirstComponent();
	const string representation = getString("typeOfGrid");
	double missing = getDouble("missingValue");

	grib_iterator* iter1 = grib_iterator_new(handle_, flags, &error);
	openSecondComponent();
	grib_iterator* iter2 = grib_iterator_new(handle_, flags, &error);
	if (!iter1 || !iter2)
	{
		MagLog::error() << "Grib Iterator not yet supported on this kind of grib\n";
		throw MagicsException("Grib Iterator not yet supported.");
	}

	double lat1, lat2;
	double lon1, lon2;
	double val1, val2, norm;
	while ( grib_iterator_next(iter1,&lat1,&lon1,&val1) && grib_iterator_next(iter2,&lat2,&lon2,&val2) )
	{
		if ( lat1 == lat2 && lon1 == lon2 )
			if ( val1 != missing && val2 != missing) {
				norm=wind_mode_->norm(val1,val2);
				points_.push_back(new UserPoint(lon1, lat1, norm));
			}
	}	
}





GribLoop::~GribLoop()
{
    for (vector<GribDecoder*>::const_iterator g = gribs_.begin(); g != gribs_.end(); ++g)
    {
        delete *g;
    }
}


Data* GribLoop::current()
{
	return currentgrib_;
}


map<string, string> GribLoop::ids_;
int GribLoop::index_ = 0;
void GribLoop::next() {}


GribLoop::GribLoop():  currentgrib_(0), file_(0)
{
	currentDim_ = dimension_.begin();
	currentPos_ = dim_.begin();
	gribs_.clear();
	uniqueId_ = Data::getUniqueOwnerId();
	counter_ = 0;
}


void GribLoop::setToFirst() 
{
	currentDim_ = dimension_.begin();
	currentPos_ = dim_.begin();

}


bool  GribLoop::hasMore()
{

	if (file_ == 0 ) {
		file_ = fopen(path_.c_str(),"r");
		if (!file_) {
			MagLog::error() << "file can not be opened [" << path_ <<  "]" <<endl;
			throw GribFileMagException(path_, 0);
		}	
	}
	
    

    // Now we have to find the right Entry!!! 
    

   if ( currentDim_ == dimension_.end() )
	   return false;
    
   
    if ( *currentDim_ == 1 ) {
    	if (  dim_.empty() ) {
    		 // case 1 dimension= 1 and loop on all the fields! 
    		int error;
    		grib_handle* handle = grib_handle_new_from_file(0, file_, &error) ;
    		if (handle <=0)
    			return false;
    		currentgrib_ = new GribEntryDecoder(handle);

    		currentgrib_->set(*this, counter_++);

            gribs_.push_back(currentgrib_);
    	}
    	else {
    		if ( currentPos_ == dim_.end() )
    			return false;
    		 // Case 3 Dimension = 1 but we only used  a subset of fields

    		grib_handle* handle =  (*address_mode_)(0, file_, *currentPos_);
    		currentPos_++;
    		if (handle <=0)  
    			return false;
    		currentgrib_ = new GribEntryDecoder(handle);
    		currentgrib_->set(*this, counter_++);
            gribs_.push_back(currentgrib_);
    	}
    }
    
   
    if ( *currentDim_  == 2)
    {
    	if ( dim_.empty()  )
    	{
    		// case 2 Dimension = 2 and loop on all the field!
    		int error;
    		grib_handle* handle1 = grib_handle_new_from_file(0, file_, &error) ;
    		if (handle1 <=0)  return false;
    		grib_handle* handle2 = grib_handle_new_from_file(0, file_, &error) ;
    		if (handle2 <=0)  return false;
    		currentgrib_ = new GribEntryDecoder(handle1, handle2);
    		currentgrib_->set(*this, counter_++);
    		gribs_.push_back(currentgrib_);
    	}
    	else {
    		// Case 4 Dimesnion = 2 and we only used a subset of fields!
    		vector<int>::iterator dim1 =  currentPos_;
    		if ( currentPos_ ==  dim_.end() )
    			return false;
    		currentPos_++;
    		vector<int>::iterator dim2 =  currentPos_;
    		if ( currentPos_ ==  dim_.end() )
    			return false;
    		currentPos_++;


    		grib_handle* handle1 =  (*address_mode_)(0, file_, *dim1);
    		grib_handle* handle2 =  (*address_mode_)(0, file_, *dim2);
    		if ( handle1 <=0 )
    			return false;
    		if ( handle2 <=0 )
    			return false;
    		currentgrib_ = new GribEntryDecoder(handle1, handle2);
    		currentgrib_->set(*this, counter_++);

    		gribs_.push_back(currentgrib_);

    	}
    }

	if ( *currentDim_  == 3)
		{
			if ( dim_.empty()  )
			{
				// case 2 Dimension = 2 and loop on all the field!
	    			int error;
	       			grib_handle* handle1 = grib_handle_new_from_file(0, file_, &error) ;
	       			if (handle1 <=0)  return false;
	       			grib_handle* handle2 = grib_handle_new_from_file(0, file_, &error) ;
	        		if (handle2 <=0)  return false;
	        		grib_handle* handle3 = grib_handle_new_from_file(0, file_, &error) ;
	        		if (handle3 <=0)  return false;
	        		currentgrib_ = new GribEntryDecoder(handle1, handle2, handle3);
	        		currentgrib_->set(*this, counter_++);
	        		gribs_.push_back(currentgrib_);
			}
			else {
	        		// Case 4 Dimesnion = 2 and we only used a subset of fields!
					vector<int>::iterator dim1 =  currentPos_;
					if ( currentPos_ ==  dim_.end() )
						return false;
					currentPos_++;
					vector<int>::iterator dim2 =  currentPos_;
					if ( currentPos_ ==  dim_.end() )
						return false;
					currentPos_++;
					vector<int>::iterator dim3 =  currentPos_;
										if ( currentPos_ ==  dim_.end() )
											return false;
										currentPos_++;

	        		grib_handle* handle1 =  (*address_mode_)(0, file_, *dim1);
	        		grib_handle* handle2 =  (*address_mode_)(0, file_, *dim2);
	        		grib_handle* handle3 =  (*address_mode_)(0, file_, *dim3);

	        		if ( handle1 <=0 )
	        			return false;
	        		if ( handle2 <=0 )
	        		       return false;
	        		if ( handle3 <=0 )
	        			        		       return false;
	        		currentgrib_ = new GribEntryDecoder(handle1, handle2, handle3);
	        		currentgrib_->set(*this, counter_++);

	        		gribs_.push_back(currentgrib_);

			}
		}
	currentDim_++;
	currentgrib_->setPath(path_);
	if ( iconName_.empty() )
	{
    		map<string, string>::iterator id = ids_.find(path_);
    		if ( id == ids_.end() )
		{
    		    	iconName_ = "Grib" + tostring(index_);
    		    	index_++;
    		    	ids_.insert(make_pair(path_, iconName_));  		
    		 }
    		 else 
    		    	iconName_ = id->second;
	}
	currentgrib_->icon(*this);
	return true;
}

void GribLoop::print(ostream&) const {}

void GribDecoder::handle(grib_handle* handle)
{
	handle_ = handle;
	lKeys_.clear();
	sKeys_.clear();
	dKeys_.clear();
}

class GribTag: public XmlNodeVisitor
{
public:
	GribTag(GribDecoder& grib, TagHandler& title) : grib_(grib), title_(title) {
	}
	
	~GribTag() {}
	string baseDate(const XmlNode& node)
	{
		string format= node.getAttribute("format");
		if ( format.empty() )  
			format =  "%A %d %B %Y at %H UTC";
		const long day  = grib_.getLong("date");  
		const long hour = grib_.getLong("hour");  
		const long mn   = grib_.getLong("minute"); 
		MagDate part1 = MagDate(day);
		MagTime part2 = MagTime(hour, mn, 0);
		DateTime full(part1, part2);
	    
		return full.tostring(format);
		
	}

	string startDate(const XmlNode& node)
	{
			string format= node.getAttribute("format");
			if ( format.empty() )  
				format =  "%A %d %B %Y at %H UTC";
			const long day  = grib_.getLong("date");  
			const long hour = grib_.getLong("hour");  
			const long mn   = grib_.getLong("minute");
			const long step = grib_.getLong("startStep");  // default is in hours. Set 'stepUnits' to change.

			MagDate part1 = MagDate(day);
			MagTime part2 = MagTime(hour, mn, 0);
			DateTime full(part1, part2);	
			full = full + (step*3600);
			    
			return full.tostring(format);
	}

	string validDate(const XmlNode& node)
	{
			string format= node.getAttribute("format");
			if ( format.empty() )  
				format =  "%A %d %B %Y at %H UTC";
			const long day =  grib_.getLong("date");  
			const long hour = grib_.getLong("hour");  
			const long mn =  grib_.getLong("minute");
			const long step =  grib_.getLong("stepRange");  // default is in hours. Set 'stepUnits' to change.
			

						
			MagDate part1 = MagDate(day);
			MagTime part2 = MagTime(hour, mn, 0);
			DateTime full(part1, part2);	
			full = full + (step*3600);

			return full.tostring(format);
			
	}

	string endDate(const XmlNode& node)
	{
		string format=  node.getAttribute("format");
		if ( format.empty() )  
			format =  "%A %d %B %Y at %H UTC";
		const long day =  grib_.getLong("date");  
		const long hour = grib_.getLong("hour");  
		const long mn =  grib_.getLong("minute");
		const long step =  grib_.getLong("endStep");  // default is in hours. Set 'stepUnits' to change.

		MagDate part1 = MagDate(day);
		MagTime part2 = MagTime(hour, mn, 0);
		DateTime full(part1, part2);	
		full = full + ( step*3600 );
		    
		return full.tostring(format);
		
	}



	void visit(const XmlNode& node)
	{
		if ( magCompare(node.name(), "grib_info") )
		{
			string grib = node.getAttribute("id");
			string where = node.getAttribute("where");
			if ( !grib_.id(grib, where)) {

					return;
			}
			string def = node.getAttribute("key");
			if (def.empty()) {
				 def = node.getAttribute("definition");
				 // for backward compatibility with the first version! 
			}
			if ( def== "valid-date") {
							title_.update("grib"+grib, def, validDate(node));
							return;
						}
			
			if ( def == "base-date") {
				title_.update("grib"+grib, def, baseDate(node));
				return;
			}
			if ( def == "MV_Format" ) {
				title_.update("grib"+grib, def, "grib");
				return;
			}
			if ( def == "MV_Index"  || def == "MV_Frame" ||  def == "MV_Value" ) {
				// this is handled by Metview, so we just ignore it here
				return;
			}
			if ( def == "start-date" ) {
				title_.update("grib"+grib, def, startDate(node));
				return;
			}
			if ( def == "end-date" ) {
				title_.update("grib"+grib, def, endDate(node));
				return;
			}	
			string val; 
			string readAsLong = node.getAttribute("readAsLong");
			if(readAsLong != "yes")
			{
				val =  grib_.getString(def);
				string format = node.getAttribute("format"); 
				if ( !format.empty() ) {
					char tmp[256];
					sprintf(tmp, format.c_str(), val.c_str());
					val = tmp;
					
				}
			}
			else
			{
				long longVal = grib_.getLong(def);
				std::stringstream sst ;
				sst << longVal;
				val=sst.str(); 
			}
	
			if ( val.empty() ) 
				val =  node.getAttribute("default");
			title_.update("grib"+grib, def, val);
		}		
		
		if ( magCompare(node.name(), "magics_title") )
		{
			string grib = node.getAttribute("id");
			string where = node.getAttribute("where");
			if ( !grib_.id(grib, where)) return;

			vector<string> lines;
			TitleTemplate::title(lines, grib_);
			for (unsigned int i = 0; i < lines.size(); i++)
			{
				string id = grib_.title() + "_" + tostring(i);
				title_.update("grib"+grib, id, lines[i]);		
				string entry = "<grib_info definition=\'" + id + "\'/>";
				title_.addToTags("<magics_title/>", entry);
			}
			//title_.addLines(lines.size());
		}
		node.visit(*this);	
	}
	
	void decode(const string& line)
	{
		XmlReader parser;
		XmlTree tree;
	
		ostringstream xml;
		xml << "<?xml version='1.0' ?> \n";		
		xml << "<xml> \n";
		xml << line;
		xml << "\n</xml>";

		try {
			parser.decode(xml.str(), &tree);		
			tree.visit(*this);
		}
		catch (MagicsException& e) {
			MagLog::debug() << e.what() << endl;
		}	
     } 
     string str() const { return out.str(); }
protected :
	GribDecoder& grib_;
	TagHandler& title_;
	ostringstream out;
};

void GribDecoder::visit(AnimationRules& )
{
}

void GribDecoder::visit(ValuesCollector& points)
{
	field_ = open(field_);
	const Transformation& transformation = points.transformation();
		
	points.setCollected(true);

	int nb = points.size();
	double inlats[nb];
	double inlons[nb];
	double outlats[nb];
	double outlons[nb];
	double values[nb];
	double x[nb];
	double y[nb];
	double distances[nb];
	int indexes[nb];
	double scaling, offset;
	string oriUnits, derivedUnits;
	string representation = getString("typeOfGrid");
		
	//Scaling works only for scalar data!!!
	
	try 
	{
		if ( !interpretor_ ) {
					interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
				}
		interpretor_->scaling(*this, scaling, offset,oriUnits,derivedUnits);
	}      
	catch (NoFactoryException&)
	{
		MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
		scaling =1 ; offset =0;
	}
 	
	for (int i =0; i < nb; i++)
	{
		 
	  	 inlats[i] = points[i].y();
		 inlons[i] = std::fmod(points[i].x(),360.);
		 if(inlons[i] < 0.) inlons[i]+=360.;
		 i++;
	}
 
	double missing = getDouble("missingValue");
		
	if ( Data::dimension_ == 1 ) {
	  	bool scaled=(scaling==1 && offset == 0)?false:true;
		points.setScaled(scaled);
		points.setUnits(oriUnits);
		points.setScaledUnits(derivedUnits);
	  
		field_ = open(field_);
		grib_nearest_find_multiple(handle_, 0, inlats, inlons, nb, outlats, outlons, values, distances, indexes);
		for (int i =0; i < nb; i++) 
		{
			points[i].push_back(new ValuesCollectorData(outlons[i],outlats[i],values[i],distances[i]));
			if(scaled)
				points[i].back()->setScaledValue(scaling*values[i] + offset);
			if(values[i] == missing)
			  	points[i].back()->setMissing(true);
		}
	}
	else if ( Data::dimension_ == 2  ) {
		bool scaled=(scaling==1 && offset == 0)?false:true;		
		oriUnits=getString("units",false);
		if(oriUnits.find("/") == string::npos)
		{
		  	oriUnits=oriUnits + "/" + oriUnits;
		}	
		points.setUnits(oriUnits);
		points.setScaledUnits("/");
				  			
		
		openFirstComponent();
		grib_nearest_find_multiple(handle_, 0, inlats, inlons, nb, outlats, outlons, x, distances, indexes);
		openSecondComponent();
		grib_nearest_find_multiple(handle_, 0, inlats, inlons, nb, outlats, outlons, y, distances, indexes);
		for (int i =0; i < nb; i++) 
		{
			points[i].push_back(wind_mode_->values(outlons[i],outlats[i],x[i],y[i], distances[i]));
			if(x[i] == missing || y[i] == missing)
				points[i].back()->setMissing(true);	  
		}
	}
		else   {
			bool scaled=(scaling==1 && offset == 0)?false:true;
					oriUnits=getString("units",false);
					if(oriUnits.find("/") == string::npos)
					{
					  	oriUnits=oriUnits + "/" + oriUnits;
					}
					points.setUnits(oriUnits);
					points.setScaledUnits("/");


					openFirstComponent();
					grib_nearest_find_multiple(handle_, 0, inlats, inlons, nb, outlats, outlons, x, distances, indexes);
					openSecondComponent();
					grib_nearest_find_multiple(handle_, 0, inlats, inlons, nb, outlats, outlons, y, distances, indexes);
					for (int i =0; i < nb; i++)
					{
						points[i].push_back(wind_mode_->values(outlons[i],outlats[i],x[i],y[i], distances[i]));
						if(x[i] == missing || y[i] == missing)
							points[i].back()->setMissing(true);
					}
		}

}

void GribDecoder::visit(MagnifierCollector& magnifier)
{
	const Transformation& transformation = magnifier.transformation(); 
	PointsHandler& points = this->points(transformation);
	points.setToFirst();

	while  ( points.more() )
	{
		magnifier.push_back(transformation(points.current()));
		points.advance();
	}
}
const DateDescription& GribDecoder::timeStamp()
{
	vector<string> need;
	need.push_back("<grib_info key='valid-date' format='%Y-%m-%d %H:%M:00'/>");
	need.push_back("<grib_info key='level'/>");
	need.push_back("<grib_info key='typeOfLevel'/>");

	TagHandler helper;
	GribTag tag1(*this, helper);
	for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
			tag1.decode(*t);


	timeStamp_ = DateDescription(helper.get("grib", "valid-date"), index_, internalIndex_);
	dataLevel_ = LevelDescription::level(helper.get("grib", "typeOfLevel"), tonumber(helper.get("grib", "level")), index_, internalIndex_);


	return timeStamp_;

}

const LevelDescription& GribDecoder::level()
{
	timeStamp();
	return dataLevel_;
}

void GribDecoder::visit(MetaDataCollector& step)
{
	// Here we gather information for the label!
	const Transformation& transformation = step.transformation();
	
	field_ = open(field_); // just to be sure the file is opened!

	initInfo();

	 //Collect infos
	if(step.empty())
	{
		MetviewIcon::visit(step);
	   	return;
	}
	   
	try {	  
		bool members=false;
		vector<string> need;
		if(name_.empty())
		{
			members=true;
		  	need.push_back("<grib_info key='shortName'/>");
			need.push_back("<grib_info key='level'/>");
			need.push_back("<grib_info key='start-date' format='%Y-%m-%d %H:%M:00'/>");
			need.push_back("<grib_info key='end-date' format='%Y-%m-%d %H:%M:00'/>");
		}
		
		for(map<string, string>::iterator key = step.begin(); key != step.end(); ++key )
		{	    
			//If key is not found in information we use gribapi
		  	if(information_.find(key->first) == information_.end())
			{  
				//Compute stats
				if (step.attribute(key->first).group() == MetaDataAttribute::StatsGroup)
				{
					stats_.clear();
					

					PointsHandler& points = this->points(transformation, false);

					points.setToFirst();
	
					while( points.more() )
					{
						stats_["value"].push_back(points.current().value());						
						points.advance();
					}
					
					computeStats();
					

				}
				//We use gribapi
				else if(step.attribute(key->first).source() == MetaDataAttribute::AnySource ||
				   step.attribute(key->first).source() == MetaDataAttribute::GribApiSource)
				{  					  
					if(step.attribute(key->first).type() != MetaDataAttribute::NumberType)   
					{
						need.push_back("<grib_info key='"+key->first+ "'/>");
		   			}
					else
					{
						need.push_back("<grib_info key='"+key->first+ "' readAsLong='yes'/>");
					}

				}
				else if(key->first == "scaling_formula" ||key->first == "scaled_units" )
				{
					double scaling, offset;
					string oriUnits, derivedUnits;
					string representation = getString("typeOfGrid");
					try 
					{
						auto_ptr<GribInterpretor> interpretor_(SimpleObjectMaker<GribInterpretor>::create(representation));
						interpretor_->scaling(*this, scaling, offset,oriUnits,derivedUnits);
						if(scaling==1 && offset == 0)
						{
							information_["scaling_formula"]="";
							information_["scaled_units"]="";
						}
						else
						{  
							string offsetSign=(offset >=0)?"+":"-";
							information_["scaling_formula"]="(value * " + tostring(scaling) + ") " + offsetSign + " " + tostring(fabs(offset));
							information_["scaled_units"]= derivedUnits;
						}
					}	
					catch (NoFactoryException&)
					{
						MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n"<<endl;;
						information_[key->first]="N/A";
					}
					
				}       	
				
			}	
			//If key is found in information_ we copy it
			else
			{
				  	key->second=information_[key->first];
			}	
		}
		
		
		if(!need.empty())	
		{
			TagHandler helper;
			GribTag tag1(*this, helper);
			for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t ) 
			{
	   			tag1.decode(*t);
			}	
				
	   		if(members)
			{  
	   			name_ = helper.get("grib", "shortName") +  " " +  helper.get("grib", "level");

	   			from_ = DateTime(helper.get("grib", "start-date"));
	   			to_ =  DateTime(helper.get("grib", "end-date"));
			}
			
			for(map<string, string>::iterator key = step.begin(); key != step.end(); ++key )
			{	    
				if(information_.find(key->first) == information_.end())
				{  
					if(step.attribute(key->first).source() == MetaDataAttribute::AnySource ||
				   	   step.attribute(key->first).source() == MetaDataAttribute::GribApiSource)
					{  					  
					 	key->second = helper.get("grib", key->first);
						setInfo(key->first,key->second);
					}	
				} 

			}	  
			
	   	}
	}

	catch (...) {}

}
MatrixHandler& GribDecoder::direction() {

	decode2D();
	// Now X et X components are ready ..
	// We compute the direction in matrix_	and send it back.

	matrix_ = xComponent_;

	vector<double>::const_iterator x = xComponent_->begin();
	vector<double>::const_iterator y = yComponent_->begin();
	vector<double> directions;
	//	MagLog::dev()<< "missing1-->" << in1->missing() << endl;
	//	MagLog::dev()<< "missing2-->" << in2->missing() << endl;
		while ( x != xComponent_->end() &&  x != yComponent_->end() ) {
			if ( *x == xComponent_->missing() || *y == yComponent_->missing() )
	    	   directions.push_back(xComponent_->missing());
			else
				directions.push_back(atan2((*y), (*x)) );
			++x;
			++y;
		}
		matrix_->clear();
		xComponent_ = 0;



		for (vector<double>::iterator d = directions.begin(); d != directions.end(); ++d) {
				matrix_->push_back(*d);
		}

		matrixHandlers_.push_back(new MatrixHandler(*matrix_));
		return *(matrixHandlers_.back());

}
void GribDecoder::decode(const Transformation& transformation) 
{
	if (matrix_) return;

	field_ = open(field_);

		read(&matrix_, transformation);
	if (!matrix_) return;
	
	// here we build information for the layers!
	TagHandler helper; 
	vector<string> need;
	need.push_back("<grib_info id=\'" + id_ +"\' key='shortName'/>");
	need.push_back("<grib_info id=\'" + id_ +"\' key='level'/>");
	need.push_back("<grib_info id=\'" + id_ +"\'  key='start-date' format='%Y-%m-%d %H:%M:00'/>");
	need.push_back("<grib_info id=\'" + id_ +"\' key='end-date' format='%Y-%m-%d %H:%M:00'/>");
	GribTag tag1(*this, helper);

	for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
	{
		tag1.decode(*t);
	}

	name_ = helper.get("grib"+id_, "shortName") +  "-" +  helper.get("grib"+id_, "level");
	name_ = iconName_;
	layerId_ = name_ + file_name_;
	from_ = DateTime(helper.get("grib"+id_, "start-date"));
	to_ =  DateTime(helper.get("grib"+id_, "end-date"));	
}

void GribDecoder::decode() 
{
	
	if ( dimension_ == 1) {
		if (matrix_) return;
		field_ = open(field_);
		read(&matrix_);
		if (!matrix_) return;
	}
	else {
		decode2D();
	}


	// here we build information for the layers!
	TagHandler helper; 
	vector<string> need;
	need.push_back("<grib_info id=\'" + id_ +"\' key='shortName'/>");
	need.push_back("<grib_info id=\'" + id_ +"\' key='level'/>");
	need.push_back("<grib_info id=\'" + id_ +"\'  key='start-date' format='%Y-%m-%d %H:%M:00'/>");
	need.push_back("<grib_info id=\'" + id_ +"\' key='end-date' format='%Y-%m-%d %H:%M:00'/>");
	GribTag tag1(*this, helper);

	for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
	{
		tag1.decode(*t);
	}

	name_ = helper.get("grib"+id_, "shortName") +  "-" +  helper.get("grib"+id_, "level");
	name_ = iconName_;
	layerId_ = name_ + file_name_;
	from_ = DateTime(helper.get("grib"+id_, "start-date"));
	to_ =  DateTime(helper.get("grib"+id_, "end-date"));	
}



void GribDecoder::visit(TextVisitor& title) 
{
	try {
		field_ = open(field_);
	}
	catch ( ... )
	{
		return;
	}
	
	vector<string> titles;

	title.titles(titles);
	GribTag tag(*this, title);
	
	for ( vector<string>::const_iterator t = titles.begin(); t != titles.end(); ++t ) {
		tag.decode(*t);
	}
}



void GribDecoder::decodeRaster(const Transformation& transformation) 
{
	field_ = open(field_);
	
	string representation = getString("typeOfGrid");
	
	try {
		if ( !interpretor_ ) {
					interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
				}
		interpretor_->interpretAsRaster(*this, raster_, transformation);
	}
    
    catch (NoFactoryException&)
    {
    	MagLog::error() << "Grib Decoder: Representation [" << representation << "] not yet supported.\n";
    	throw MagicsException("Grib Decoder: Representation [] not yet supported.");
    }
}


void GribDecoder::initInfo()
{
	if(information_.find("_datatype") == information_.end())
	{
	  	setInfo("_datatype","GRIB");
		
		char buf[1024];
    		int count = readlink(file_name_.c_str(), buf, sizeof(buf));
    		if (count > 0)
		{
		    	buf[count] = '\0';
   			setInfo("path", string(buf));
		}	
		else
		{  
			setInfo("path", file_name_);
		}
		setInfo("MV_Format","GRIB");
	}
}



namespace magics {

class GribInfo
{
public:
	GribInfo() {}
	virtual ~GribInfo() {}
	virtual void operator()(ostream&, const GribDecoder&) = 0;
};

class GribParameter : public GribInfo
{
public: 
	GribParameter() {}
	~GribParameter() {}
	void operator()(ostream& out, const GribDecoder& grib)
	{
		string val = grib.getString("name");
 		out << val;
	}
};

class GribParamCriter : public MatchCriteria
{
public:
	GribParamCriter() {}
	~GribParamCriter() {}
	bool verify(const GribDecoder& grib, const string&, const string& val)
	{
		long param = grib.getLong("paramId");
		return (tostring(param) == val);
	}
};

class GribLocalCriter : public MatchCriteria
{
public:
	GribLocalCriter() {}
	~GribLocalCriter() {}
	bool verify(const GribDecoder& grib, const string& param, const string& val)
	{
		string key = param;
		string criter = grib.getString(key, false); // 'false' to avoid warnings if key is absent
		MagLog::debug() << "I am verifing " << param << " for a GribDecoder : " << criter << " ==  " << val << "???" << "\n";
		return (criter == val);
	}
};

class GribObsDiagCriter : public MatchCriteria
{
public:
	GribObsDiagCriter() {}
	~GribObsDiagCriter() {}
	bool verify(const GribDecoder& grib, const string&, const string& )
	{
		string param = grib.getString("observationDiagnostic", false); // do not warn if the key is absent
		return (param != "");
	}
};


class GribLocalDefHandler : public TitleFieldHandler
{
public:
	GribLocalDefHandler() {}
	~GribLocalDefHandler() {}

	void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
	{

		ostringstream out;
		string local = grib.getString("localDefinitionNumber");       
		out << "local definition =" << local << " ";        
		title.back() += out.str();
	}
};

class GribObsDiagHandler : public TitleFieldHandler
{
public:
	GribObsDiagHandler() {}
	~GribObsDiagHandler() {}

	void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
	{

        ostringstream out;
		string local = grib.getString("observationDiagnostic");       
        out << "diagnostic =" << local << " ";   
        title.back() += out.str();
	}
};

class GribObstatHandler : public TitleFieldHandler
{
public:
	GribObstatHandler() {}
	~GribObstatHandler() {}

	void operator()(TitleField&, vector<string>& /*title*/, const GribDecoder& grib)
	{


	}
};

class GribLocalHandler : public TitleFieldHandler
{
public:
	GribLocalHandler(const string& local) : local_(local) {}
	~GribLocalHandler() {}

	void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
	{

		ostringstream out;
		string code = grib.getString(local_);
		out << local_ << "=" << code  << " ";
		title.back()+= out.str();
	}
protected :
	string local_;
};

class GribStreamHandler : public GribLocalHandler 
{
public:
	GribStreamHandler() : GribLocalHandler("marsStream") {}
	~GribStreamHandler() {}
};

class GribClassHandler : public GribLocalHandler 
{
public:
	GribClassHandler() : GribLocalHandler("marsClass") {}
	~GribClassHandler() {}
};

class GribTypeHandler : public GribLocalHandler 
{
public:
	GribTypeHandler() : GribLocalHandler("marsType") {}
	~GribTypeHandler() {}
};


class GribParamHandler : public TitleFieldHandler
{
public:
	GribParamHandler() {}
	~GribParamHandler() {}
	virtual void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
	{

		string param = grib.getString("name");
 		title.back()+=param;
	}
};


class GribKeyHandler : public TitleFieldHandler
{
public:
	GribKeyHandler() {}
	~GribKeyHandler() {}
	void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
	{
		

		char x[256];
		
		string key = field.attribute("key", ""); 	
		string value  = grib.getString(key);       
		string format = field.attribute("format", "%s");
		sprintf(x, format.c_str(), value.c_str());

	    title.back() += string(x);

	}
};



class GribBaseDateHandler : public TitleFieldHandler
{
public:
	GribBaseDateHandler() {}
	~GribBaseDateHandler() {}
	void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
	{

		ostringstream out;

		long date = grib.getLong("date");    
		long hour = grib.getLong("hour");  
		long mn =  grib.getLong("minute"); 
		MagDate part1 = MagDate(date);
		MagTime part2 = MagTime(hour, mn, 0);
		DateTime full(part1, part2);
	
		string format = field.attribute("format", "%A %d %B %Y at %H UTC");
	
		title.back() += full.tostring(format);
	}	
};

class GribValidDateHandler : public TitleFieldHandler
{
public:
    GribValidDateHandler() {}
    ~GribValidDateHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {
	    ostringstream out;
        long date = grib.getLong("date");    
        long hour = grib.getLong("hour");  
        long mn   = grib.getLong("minute"); 
        long step = grib.getLong("step") * 3600; // needs steps in second!   // default is in hours. Set 'stepUnits' to change.
       
        MagDate part1 = MagDate(date);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);
        full = full + step;

        
	    string format = field.attribute("format", "%A %d %B %Y %H UTC");
	        
	    title.back() += full.tostring(format);
       

    }
};

class GribStepHandler : public TitleFieldHandler
{
public:
    GribStepHandler() {}
    ~GribStepHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {

        ostringstream out;
        long istep = grib.getLong("startStep");


        ostringstream step;
        step << istep;
        string format = field.attribute("format", "t+%s");
        out << SimpleStringFormat(step.str(), format);
        title.back() += out.str();
	}
};


class GribLevelHandler : public TitleFieldHandler
{
public:
    GribLevelHandler() { 
    	if (map_.empty()) {
    		map_["Surface"] = &GribLevelHandler::surface;
    		map_["Unknown"] = &GribLevelHandler::surface;
    		map_["isobaricInhPa"] = &GribLevelHandler::isobaricInhPa;
    		map_["heightAboveGround"] = &GribLevelHandler::heightAboveGround;
    		map_["heightAboveGround"] = &GribLevelHandler::heightAboveGround;
    		map_["hybrid"] = &GribLevelHandler::hybrid;
    	}
    }
    
    ~GribLevelHandler() {}
    typedef string (GribLevelHandler::*Builder)(const string& def, const GribDecoder& grib) const;
    
    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {
    	ostringstream out;

        string level = grib.getString("typeOfLevel");

        
        map<string,  GribLevelHandler::Builder>::iterator help = map_.find(level);
        if ( help != map_.end() ) out << (this->*help->second)(level, grib) << " ";
        else out << level << " ";    
        
        title.back() +=  out.str();
        
    }
    
protected: 
	static map<string, GribLevelHandler::Builder> map_;
	
	string surface(const string&, const GribDecoder& ) const
	{
		return "";
	}	
	string unknown(const string& level, const GribDecoder& ) const
	{
		ostringstream out;
		out << "Unknown type of level[" << level << "]";
		return out.str();
	}	
	string isobaricInhPa(const string& , const GribDecoder& grib) const
	{
		ostringstream out;
		long level = grib.getLong("level");
		out  << level  <<  " " << "hPa";
		return out.str();
	}	
	string pv(const string& , const GribDecoder& grib) const
		{
			ostringstream out;
			long level = grib.getLong("level");
			out  << level  <<  " mPVU";
			return out.str();
		}
	string heightAboveGround(const string& , const GribDecoder& grib) const
	{
		ostringstream out;
		long level = grib.getLong("level");
		out  << level  <<  " m";
		return out.str();
	}
	string hybrid(const string& , const GribDecoder& grib) const
	{
		ostringstream out;
		long level = grib.getLong("level");
		out  << "Model level " << level;
		return out.str();
	}
};
map<string, GribLevelHandler::Builder> GribLevelHandler::map_;


class GribTimeHandler : public TitleFieldHandler
{
public:
    GribTimeHandler() {}
    ~GribTimeHandler() {}
    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {
//        if (!grib.getText()) return;
//        ostringstream out;
//        grib_int_t idate;
//        grib_get(grib.id(),(grib_string_t*)"time","I",&idate);      
//     
//        out << "Time:" << idate;
//        title.add(out.str());

          title.back() +=  "Time? ";
    }
};

class GribCentreHandler : public TitleFieldHandler
{
public:
	GribCentreHandler() {}
	~GribCentreHandler() {}
	void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
	{
		string format = field.attribute("format", "%s");
		string style = field.attribute("style", "short");
		string centre = grib.getString("centre");

		title.back() += centre;
	}
};

class GribProductHandler : public TitleFieldHandler
{
public:
    GribProductHandler() {}
    ~GribProductHandler() {}
    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {

       long type = grib.getLong("type");

       GeneralDef def = TypeTable::definition(type);
       title.back() += def.longTitle();
    }
};


class GribPlotTypeHandler : public TitleFieldHandler
{
public:
    GribPlotTypeHandler() {}
    ~GribPlotTypeHandler() {}
    void operator()(TitleField&, vector<string>&,const GribDecoder&)
    {
         //MagLog::warning() << "Plot Type: not implemented--> wait for the specification." << "\n";
    }
};


class NewLineHandler : public TitleFieldHandler
{
public:
    NewLineHandler() {}
    ~NewLineHandler() {}
    void operator()(TitleField&, vector<string>& title,const GribDecoder&)
    {
        title.push_back("");
    }
};


class SatelliteHandler : public TitleFieldHandler
{
public:
    SatelliteHandler()  {}
    ~SatelliteHandler() {}
    void operator()(TitleField&, vector<string>& title,const GribDecoder& grib)
    {
       ostringstream out;
       long ident =  grib.getLong("ident");

       out << "Sat:" << ident << " ";
       title.back() += out.str();
    }
};

class ChannelHandler : public TitleFieldHandler
{
public:
    ChannelHandler()  {}
    ~ChannelHandler() {}
    void operator()(TitleField&, vector<string>& title,const GribDecoder& grib)
    {       
    	ostringstream out;
        long band = grib.getLong("obstype");
        out << "Band:" << band << " ";
        title.back() += out.str();
    }
};


class GribExpverHandler : public TitleFieldHandler
{
public:
    GribExpverHandler() {}
    ~GribExpverHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {

       if ( !grib.getExpver() ) return; 
       ostringstream out;    
       string expver = grib.getString("mars.experimentVersionNumber");
       string format = field.attribute("format", "Expver=%s");          
       out << SimpleStringFormat(expver, format);
       title.back() += out.str();
   }
};


class GribEpsNumberInfoHandler : public TitleFieldHandler
{
public:
    GribEpsNumberInfoHandler() {}
    ~GribEpsNumberInfoHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {
	    
//       if (!grib.getText()) return;
//        ostringstream out;
//       grib_int_t local;
//       grib_get(grib.id(),(grib_string_t*)"localDefinition","I","localDefinitionNumber",&local);
//       if (local != 1) return;
//
//       char number[1024];
//       grib_get(grib.id(),(grib_string_t*)"localDefinition","s","total",number);
//       string format = field.attribute("format", "(%s members)");     
//      
//      out << SimpleStringFormat(number, format);
//        title.add(out.str());

    	 title.back() +=  "epsnumber?";

    }
};


class GribUnitHandler : public TitleFieldHandler
{
public:
    GribUnitHandler() {}
    ~GribUnitHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {
/*
		if (!grib.getText()) return;
        if ( !grib.getUnits() ) return; 
        ostringstream out;      
     
        double id   = grib.getDouble("paramId");
        long centre  = grib.getLong("centre");
        				
        long param = (long) id;
        long table   = (id - param )*100;

          
        const ParamDef& parameter = LocalTable::localInfo(param, table, centre);
           
        string format = field.attribute("format", "Units:%s");           
        string unit = (grib.getScaling()) ? parameter.derivedUnit() :  parameter.originalUnit();
        out << SimpleStringFormat(unit, format);
      
        
        title.back() += out.str();
   */
    }
};

}// end namespace magics



static SimpleObjectMaker<GribParamCriter, MatchCriteria > gribparamcriter("parameter");
static SimpleObjectMaker<GribParamHandler, TitleFieldHandler > gribparamhandler("parameter");
static SimpleObjectMaker<GribBaseDateHandler, TitleFieldHandler > gribbasedatehandler("base_date");
static SimpleObjectMaker<GribValidDateHandler, TitleFieldHandler > gribvaliddatehandler("valid_date");
static SimpleObjectMaker<GribStepHandler, TitleFieldHandler > gribstephandler("step");
static SimpleObjectMaker<GribEpsNumberInfoHandler, TitleFieldHandler > gribepsnumberhandler("eps_number_info");

static SimpleObjectMaker<GribTimeHandler, TitleFieldHandler > gribTimehandler("time");
static SimpleObjectMaker<GribLevelHandler, TitleFieldHandler > gribLevelhandler("level");
static SimpleObjectMaker<NewLineHandler, TitleFieldHandler > newlinehandler("newline");
static SimpleObjectMaker<GribKeyHandler, TitleFieldHandler > gribkeyhandler("gribapi");

static SimpleObjectMaker<GribStreamHandler, TitleFieldHandler > gribstreamhandler("stream");
static SimpleObjectMaker<GribClassHandler, TitleFieldHandler > gribclasshandler("class");
static SimpleObjectMaker<GribTypeHandler, TitleFieldHandler > gribtypehandler("type");
static SimpleObjectMaker<GribLocalDefHandler, TitleFieldHandler > griblocaldefhandler("localdef");
static SimpleObjectMaker<GribCentreHandler, TitleFieldHandler > gribcentrehandler("centre");
static SimpleObjectMaker<GribProductHandler, TitleFieldHandler > gribproducthandler("product");
static SimpleObjectMaker<GribUnitHandler, TitleFieldHandler > gribunithandler("units");
static SimpleObjectMaker<GribExpverHandler, TitleFieldHandler > gribexpverhandler("expver");
static SimpleObjectMaker<GribPlotTypeHandler, TitleFieldHandler > gribplottypehandler("plot_type");
static SimpleObjectMaker<SatelliteHandler, TitleFieldHandler > satellitehandler("satellite");
static SimpleObjectMaker<ChannelHandler, TitleFieldHandler > channelhandler("channel");
static SimpleObjectMaker<GribBaseDateHandler, TitleFieldHandler > datehandler("date");


static SimpleObjectMaker<GribObsDiagCriter, MatchCriteria > gribobsdiagriter("observationDiagnostic");
static SimpleObjectMaker<GribObsDiagHandler, TitleFieldHandler > gribobsdiaghandler("observationDiagnostic");
static SimpleObjectMaker<GribObstatHandler, TitleFieldHandler > gribobstathandler("obstat");


static SimpleObjectMaker<GribLocalCriter, MatchCriteria > gribstreamcriter("stream");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > gribtypecriter("type");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > gribclasscriter("class");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > typeOfgeneratingProcess("typeOfGeneratingProcess");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > timeRangeIndicator("timeRangeIndicator");

#include "GribRegularInterpretor.h"
static SimpleObjectMaker<GribRegularInterpretor, GribInterpretor> regular_ll("regular_ll");
static SimpleObjectMaker<GribReducedLatLonInterpretor, GribInterpretor> reduced_ll("reduced_ll");
static SimpleObjectMaker<GribRegularGaussianInterpretor, GribInterpretor> regular_gg("regular_gg");
static SimpleObjectMaker<GribReducedGaussianInterpretor, GribInterpretor> reduced_gg("reduced_gg");
static SimpleObjectMaker<GribRotatedInterpretor, GribInterpretor> rotated_ll("rotated_ll");
static SimpleObjectMaker<GribLambertAzimutalInterpretor, GribInterpretor> lambert_azimuthal_equal_area("lambert_azimuthal_equal_area");
static SimpleObjectMaker<GribLambertInterpretor, GribInterpretor> lambert("lambert");


#include "GribSatelliteInterpretor.h"
static SimpleObjectMaker<GribSatelliteInterpretor, GribInterpretor> satellite("space_view");

