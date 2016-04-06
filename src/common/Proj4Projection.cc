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

/*! \file Proj4Projection.cc
    \brief Implementation of Proj4Projection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue May 18 17:39:58 2010

*/

#include <Proj4Projection.h>
#include <Polyline.h>
#include <GridPlotting.h>
#include <LabelPlotting.h>
#include <Text.h>
#include "MetaData.h"
#include "MagConfig.h"
#include <MatrixHandler.h>
#include <ParameterSettings.h>
#include <Layer.h>
#include <MagJSon.h>

namespace magics
{

class Epsg
{
public:
	Epsg(const string& name) : name_(name) {
		epsgs_.insert(make_pair(lowerCase(name), this));
		methods_["definition"] = &Epsg::definition;
		methods_["min_longitude"] = &Epsg::minlon;
		methods_["min_latitude"] = &Epsg::minlat;
		methods_["max_longitude"] = &Epsg::maxlon;
		methods_["max_latitude"] = &Epsg::maxlat;
		methods_["method"] = &Epsg::method;
		initMethods_["geos"] = &Epsg::geosinit;
	}
	string name_;
	string definition_;

	typedef void (Epsg::*InitMethod)(const Proj4Projection&);
	typedef void (Epsg::*Method)(const json_spirit::Value&);
	map<string,  Method> methods_;
	map<string,  InitMethod> initMethods_;

	void definition(const json_spirit::Value& value) {
		definition_ =  value.get_value<string>();
	}
	void minlon(const json_spirit::Value& value) {
		minlon_ =  value.get_value<double>();
	}
	void minlat(const json_spirit::Value& value) {
		minlat_ =  value.get_value<double>();
	}
	void maxlon(const json_spirit::Value& value) {
		maxlon_ =  value.get_value<double>();
	}
	void maxlat(const json_spirit::Value& value) {
		maxlat_ =  value.get_value<double>();
	}
	void method(const json_spirit::Value& value) {
		method_ =  value.get_value<string>();
	}

	void init(const Proj4Projection& from) {
		map<string,  InitMethod >::iterator initmethod = initMethods_.find(name_);
		if ( initmethod != initMethods_.end() ) {
			( this->*initmethod->second)(from) ;
		}
	}

	void geosinit(const Proj4Projection& from) {
		//cout <<  from.vertical_longitude_ << endl;
		minlon_ = from.vertical_longitude_ - 80;
		maxlon_ = from.vertical_longitude_ + 80;
		minlat_ = -80;
		maxlat_ = 80;
		//cout <<  minlon_ << "   " << maxlon_ << endl;
		ostringstream def;
		def << "+proj=geos +h=42164000 +ellps=WGS84 +lon_0=" << from.vertical_longitude_;

		definition_ = def.str();
		//cout << definition_ << endl;
	}


	double minlon_ ;
	double minlat_;
	double maxlon_ ;
	double maxlat_;

	string method_;

	static map<string, Epsg*> epsgs_;

	void  set(const json_spirit::Value&);

	static Epsg* find(const Proj4Projection& from) {
		string name = lowerCase(from.name());
		map<string, Epsg*>::iterator epsg = epsgs_.find(name);
		if ( epsg == epsgs_.end() )  {
			MagLog::warning() << "Can not find information on " << name << ": use epsg instead" << endl;
			return epsgs_.find("EPSG:4326")->second;
		}
		epsg->second->init(from);

		return epsg->second;
	}

	const char* definition() { return definition_.c_str(); }

};



class EpsgConfig : public MagConfig
{
public:
	EpsgConfig() {}
	~EpsgConfig() {}

	void callback(const string&, const json_spirit::Value&);
	void init();
	Epsg* epsg_;
};
}
using namespace magics;

map<string, Epsg*> Epsg::epsgs_;

void EpsgConfig::init()
{
	//methods_["epsg"] =  &EpsgConfig::epsg;
	MagConfigHandler(getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "/epsg.json", *this);
}
void Epsg::set(const json_spirit::Value& value)
{
	ASSERT (value.type() == json_spirit::obj_type);
	json_spirit::Object object =value.get_value< json_spirit::Object >();
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {

		map<string,  Method >::iterator method = methods_.find(entry->name_);
	    if ( method != methods_.end() ) {
	    	   ( (this->*method->second)(entry->value_) );
	    }

	}
}
void  EpsgConfig::callback(const string& name, const json_spirit::Value& value)
{

	// here we get an Array of epsg!

	ASSERT (value.type() == json_spirit::array_type);
	json_spirit::Array values = value.get_value<json_spirit::Array>();
	for (unsigned int i = 0; i < values.size(); i++) {
		json_spirit::Object object = values[i].get_value< json_spirit::Object >();
		 for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {

		 	Epsg* epsg = new Epsg(entry->name_);
		 	epsg->set( entry->value_);
		 }
	}

}

/*!
  \brief Constructor
*/
Proj4Projection::Proj4Projection(const string& definition) : definition_(definition),
						gridMinLon_(DBL_MAX),
						gridMinLat_(DBL_MAX),
						gridMaxLon_(-DBL_MAX),
						gridMaxLat_(-DBL_MAX)
{

	//init();
	EpsgConfig config;
	config.init();
}

Proj4Projection::Proj4Projection(): gridMinLon_(DBL_MAX),
		gridMinLat_(DBL_MAX),
		gridMaxLon_(-DBL_MAX),
		gridMaxLat_(-DBL_MAX)
{
	//init();
	EpsgConfig config;
	config.init();
}


/*!
  \brief Destructor
*/
Proj4Projection::~Proj4Projection() 
{	
}

void Proj4Projection::print(ostream& out) const
{
    out << "Proj4Projection[";
    Proj4ProjectionAttributes::print(out);
    out << "]"; 
} 

Polyline& Proj4Projection::getPCBoundingBox() const
{
	return *PCEnveloppe_;
}

Polyline& Proj4Projection::getUserBoundingBox() const
{
	return *userEnveloppe_;
}

void Proj4Projection::init()  
{
	MagLog::dev() << "Proj4Projection::init()" << *this << endl;

	from_ = pj_init_plus("+proj=longlat +ellps=WGS84 +datum=WGS84");
	projection_ = Epsg::find(*this);
	to_    = pj_init_plus(projection_->definition());
	if ( !to_) {
		MagLog::error() << pj_strerrno(pj_errno) << endl;
		MagLog::error() << " proj4 error " << projection_->definition() << endl;
		ASSERT(false);
	}

	methods_["geos"] = &Proj4Projection::geos;
	methods_["conic"] = &Proj4Projection::conic;
	methods_["simple"] = &Proj4Projection::simple;
	map<string,  InitMethod >::iterator method = methods_.find(projection_->method_);
	if ( method != methods_.end() )
		(this->*method->second)();
	else
		simple();


	helpers_["full"] = &Proj4Projection::full;
	helpers_["corners"] = &Proj4Projection::corners;
	helpers_["centre"] = &Proj4Projection::centre;

	map<string,  SettingHelper >::iterator helper = helpers_.find(lowerCase(setting_));

	if ( helper != helpers_.end() )
		(this->*helper->second)();
	else {
		MagLog::warning() << " Coud not fing method " << setting_ << " to set the geographical area"
				<< "  Going back to default area" << endl;

		full();
	}

	askedxmin_ =  std::min(min_pcx_, max_pcx_);
	askedxmax_ =  std::max(min_pcx_, max_pcx_);
	askedymin_ =  std::min(min_pcy_, max_pcy_);
	askedymax_ =  std::max(min_pcy_, max_pcy_);
}

void Proj4Projection::full()
{
}

void Proj4Projection::corners()
{
	// we have to update the PVBounding box!
		min_pcx_ = min_longitude_;
		min_pcy_ = min_latitude_;
		max_pcx_ = max_longitude_;
		max_pcy_ = max_latitude_;

		fast_reproject(min_pcx_, min_pcy_);
		fast_reproject(max_pcx_, max_pcy_);

		Polyline box;
		box.box(PaperPoint(min_pcx_, min_pcy_), PaperPoint(max_pcx_, max_pcy_));

		vector<Polyline> newbox;
		PCEnveloppe_->intersect(box, newbox);
		if ( newbox.empty() ) {
			MagLog::warning() << "Proj4 : the sub-area is not valid : use global view instead" << endl;
		}
		else {
			PCEnveloppe_ = newbox.front().clone();
		}
}

void Proj4Projection::centre()
{
}

PaperPoint Proj4Projection::operator()(const UserPoint& point)  const
{
	if ( ! from_ ) {
		from_ = pj_init_plus("+proj=longlat +ellps=WGS84 +datum=WGS84");
		projection_ = Epsg::find(*this);
		to_    = pj_init_plus(projection_->definition());
	}
	double x = point.x();
	double y = point.y();

	x *= DEG_TO_RAD;
	y *= DEG_TO_RAD;

    int error = pj_transform(from_, to_, 1, 1, &x, &y, NULL);
    if ( error ) {
		MagLog::debug() << pj_strerrno(error) << " for " << point << endl;
		return PaperPoint(-1000000, -10000000);
	}
	return PaperPoint(x, y, point.value_, point.missing(), point.border(), 0, point.name());
}

PaperPoint Proj4Projection::operator()(const PaperPoint& point)  const
{
	MagLog::dev() << "Proj4Projection::operator()(...) needs implementing." << endl;
	return Transformation::operator()(point);
}

void Proj4Projection::setNewPCBox(double minx, double miny, double maxx, double maxy)
{
	   PaperPoint p1(minx, miny);
	   PaperPoint p2(maxx, maxy);
	   UserPoint   ll, ur;

	   revert(p1, ll);
	   revert(p2, ur);

	   min_longitude_ =ll.x();
	   max_longitude_ = ur.x();
	   min_latitude_ = ll.y();
	   max_latitude_ = ur.y();
	min_pcx_ = minx;
	max_pcx_ = maxx;
	min_pcy_ = miny;
	max_pcy_ = maxy;
}

void Proj4Projection::revert(const PaperPoint& xy, UserPoint& point)  const
{
	double x = xy.x();
	double y = xy.y();

	int error = pj_transform(to_, from_, 1, 1, &x, &y, NULL );

	if ( error ) {
		MagLog::debug() << pj_strerrno(error) << endl;
		point = UserPoint(-1000, -1000);
		return;
	}

	x *= RAD_TO_DEG;
	y *= RAD_TO_DEG;
	point = UserPoint(x, y);
}


bool Proj4Projection::needShiftedCoastlines()  const
{
	// Will need w new parameter to know!
	return false;
}

void Proj4Projection::aspectRatio(double& width, double& height)  
{
	MagLog::dev() << "Proj4Projection::aspectRatio(...) needs implementing." << endl;
	Transformation::aspectRatio(width, height);
}


void Proj4Projection::add(double lon, double lat)
{
	double x = lon*DEG_TO_RAD;
	double y = DEG_TO_RAD*lat;
	int error =  pj_transform(from_, to_, 1, 1, &x, &y, NULL );
	userEnveloppe_->push_back(PaperPoint(lon, lat));
	PCEnveloppe_->push_back(PaperPoint(x, y));
	if ( x < min_pcx_ )  min_pcx_ = x;
	if ( y < min_pcy_ )  min_pcy_ = y;
	if ( x > max_pcx_ )  max_pcx_ = x;
	if ( y > max_pcy_ )  max_pcy_ = y;
	if ( lon < gridMinLon_)  gridMinLon_ = lon;
	if ( lat < gridMinLat_ )  gridMinLat_ = lat;
	if ( lon > gridMaxLon_)  gridMaxLon_ = lon;
	if ( lat > gridMaxLat_ )  gridMaxLat_ = lat;
}

void Proj4Projection::conic()
{
	userEnveloppe_->clear();
		PCEnveloppe_->clear();
	min_pcx_ = DBL_MAX;
		min_pcy_ = DBL_MAX;
		max_pcx_ = -DBL_MAX;
		max_pcy_ = -DBL_MAX;
	/*
	// left
	for ( int lat = projection_->minlat_; lat <= projection_->maxlat_; lat++) {
		add(projection_->minlon_, lat);
	}
	*/
	// top
	add( projection_->minlon_, projection_->maxlat_);
	for ( int lon = projection_->minlon_; lon <= projection_->maxlon_; lon++) {
		add(lon, projection_->minlat_);
	}
	add( projection_->maxlon_, projection_->maxlat_);
	PCEnveloppe_->correct();
	userEnveloppe_->correct();
	/*
	// right
	for ( int lat = projection_->maxlat_; lat >= projection_->minlat_; lat--) {
		add(projection_->maxlon_, lat);
	}
	// bottom
	for ( int lon = projection_->maxlon_; lon >= projection_->minlon_; lon--) {
			add(lon, projection_->minlat_);
		}
	*/
}



void Proj4Projection::simple()
{
	userEnveloppe_->clear();
	PCEnveloppe_->clear();
	min_pcx_ = DBL_MAX;
	min_pcy_ = DBL_MAX;
	max_pcx_ = -DBL_MAX;
	max_pcy_ = -DBL_MAX;
	add(projection_->minlon_, projection_->minlat_);
	add(projection_->minlon_, projection_->maxlat_);
	add(projection_->maxlon_, projection_->maxlat_);
	add(projection_->maxlon_, projection_->minlat_);
	add(projection_->minlon_, projection_->minlat_);
}

void Proj4Projection::geos()
{
	userEnveloppe_->clear();
	PCEnveloppe_->clear();
	// here we have to prepare the enveloppe!
	min_pcx_ = DBL_MAX;
		min_pcy_ = DBL_MAX;
		max_pcx_ = -DBL_MAX;
		max_pcy_ = -DBL_MAX;

		PCEnveloppe_->correct();
		userEnveloppe_->correct();

		map<double, vector<double> > helper;

		for ( int lat = projection_->minlat_; lat <= projection_->maxlat_; lat++) {
			helper.insert(make_pair(lat, vector<double>()));
			for ( int lon =  projection_->minlon_; lon <= projection_->maxlon_; lon++) {
				double x = lon*DEG_TO_RAD;
				double y = DEG_TO_RAD*lat;
				int error =  pj_transform(from_, to_, 1, 1, &x, &y, NULL );
				if ( !error) {
					helper[lat].push_back(lon);
				}
			}
		}
		// now we create the envelope...
		for ( map<double, vector<double> >::iterator lat = helper.begin(); lat != helper.end(); ++lat) {
			if ( lat->second.empty() )
				continue;
			add(lat->second.front(), lat->first);

		}
		// now reverse!
		for ( map<double, vector<double> >::reverse_iterator lat = helper.rbegin(); lat != helper.rend(); ++lat) {
			if ( lat->second.empty() )
				continue;
			add(lat->second.back(), lat->first);
			userEnveloppe_->push_back(PaperPoint(lat->second.back(), lat->first));
		}
}



void Proj4Projection::boundingBox(double& xmin, double& ymin, double& xmax, double& ymax)  const
{
	if ( ! from_ ) {
			from_ = pj_init_plus("+proj=longlat +ellps=WGS84 +datum=WGS84");
			projection_ = Epsg::find(definition_);
			to_    = pj_init_plus(proj4_definition_.c_str());
		}
	ymin = gridMinLat_;
	xmin = gridMinLon_-5;
	ymax = gridMaxLat_;
	xmax = gridMaxLon_+5;
	//cout << "Bounding box ->" << xmin << " " << xmax << endl;
}

double Proj4Projection::getMinX()  const
{
	return gridMinLon_;
}

double Proj4Projection::getMinY()  const
{
	return gridMinLat_;
}

double Proj4Projection::getMaxX()  const
{
	return gridMaxLon_;
}

double Proj4Projection::getMaxY()  const
{
	return gridMaxLat_;
}

void Proj4Projection::setMinX(double x)  
{
	min_longitude_ = x;
}

void Proj4Projection::setMinY(double y)  
{
	min_latitude_ = y;
}

void Proj4Projection::setMaxX(double x)  
{
	max_longitude_ = x;
}

void Proj4Projection::setMaxY(double y)  
{
	max_latitude_ = y;
}

double Proj4Projection::getMinPCX()  const
{
	return min_pcx_;
}

double Proj4Projection::getMinPCY()  const
{
	return min_pcy_;
}

double Proj4Projection::getMaxPCX()  const
{
	return max_pcx_;
}

double Proj4Projection::getMaxPCY()  const
{
	return max_pcy_;
}


void Proj4Projection::gridLongitudes(const GridPlotting& grid)  const
{
	Polyline boundaries;

	for (Polyline::MagLine::const_iterator point = PCEnveloppe_->begin(); point != PCEnveloppe_->end(); ++point )
	{
		boundaries.push_back(*point);
	}

	grid.add(boundaries);

	//return;
	vector<double> longitudes = grid.longitudes();


		const double step = 0.5;
		longitudes.push_back(180);
		const vector<double>::const_iterator lon_end =longitudes.end();

		for (vector<double>::const_iterator lon = longitudes.begin(); lon != lon_end; ++lon)
		{

			Polyline poly;
			poly.setAntiAliasing(false);

			for (double lat = gridMinLat_; (lat == gridMaxLat_ || lat < gridMaxLat_ + step); lat += step)
			{
				PaperPoint p(*lon, lat);
				if ( userEnveloppe_->within(p) )
					poly.push_back((*this)(UserPoint(*lon,lat)));
			}
			grid.add(poly);
		}
}

void Proj4Projection::gridLatitudes(const GridPlotting& grid)  const
{
	const vector<double>& latitudes = grid.latitudes();

	const double step = 0.5;
	const vector<double>::const_iterator lat_end = latitudes.end();
	for(vector<double>::const_iterator lat = latitudes.begin(); lat != lat_end; ++lat)
	{

		Polyline poly;
		poly.setAntiAliasing(false);
		for (double lon = gridMinLon_; lon <= gridMaxLon_ + step; lon += step)
		{
			PaperPoint p(lon, *lat);
			if ( userEnveloppe_->within(p) )
				poly.push_back((*this)(UserPoint(lon,*lat)));
		}
		grid.add(poly);
	}
}

void Proj4Projection::labels(const LabelPlotting& label, DrawingVisitor& visitor)  const
{
	vector<double> pro4_longitudes;
	pro4_longitudes.push_back(0);
	pro4_longitudes.push_back(90);
	pro4_longitudes.push_back(-90);
	pro4_longitudes.push_back(180);
	pro4_longitudes.push_back(-180);
	const vector<double>& longitudes = label.longitudes();
	const vector<double>& latitudes = label.latitudes();
	for (vector<double>::const_iterator lat = latitudes.begin(); lat != latitudes.end(); ++lat)
	{
		for (vector<double>::iterator lon = pro4_longitudes.begin(); lon != pro4_longitudes.end(); ++lon)
		{
			vector<double>::const_iterator ilon = find(longitudes.begin(), longitudes.end(), *lon);
			if ( ilon == longitudes.end() )
				continue;

			UserPoint point(*lon, *lat);
			PaperPoint xy = (*this)(point);

			if ( !in(xy) ) continue;

			Text *text = new Text();
			label.add(text);
			text->setText(writeLatitude(point));
			text->push_back(xy);
			text->setBlanking(true);
		}
	}
}


void Proj4Projection::labels(const LabelPlotting& label, LeftAxisVisitor& visitor)  const
{
	if ( false ) {
		const vector<double>& latitudes = label.latitudes();

		for (unsigned int lat = 0; lat < latitudes.size(); lat++ )
		{
			double lon = max_longitude_ - ((max_longitude_-min_longitude_)*.1);
			UserPoint point(lon,latitudes[lat]);
			PaperPoint xy = (*this)(point);
			if ( !in(xy) ) continue;
			Text *text = new Text();
			label.add(text);
			text->setText(writeLatitude(point));
			text->push_back(xy);
			text->setJustification(MRIGHT);
			text->setVerticalAlign(MHALF);
			text->setBlanking(true);
		}
	}
	else {
		double x = max_pcx_ - ((max_pcx_-min_pcx_)*.1);
		// we calculate the intersection of the longitudes with the left side
		verticalLabels(label, min_pcx_, x, MRIGHT);
	}
}

void Proj4Projection::labels(const LabelPlotting& label, RightAxisVisitor& visitor)  const
{
	if ( false ) {
		const vector<double>& latitudes = label.latitudes();
		for (unsigned int lat = 0; lat < latitudes.size(); lat++ )
		{
			double lon = min_longitude_ + ((max_longitude_-min_longitude_)*.1);
			UserPoint point(lon,latitudes[lat]);
			PaperPoint xy = (*this)(point);
			if ( !in(xy) ) continue;
			Text *text = new Text();
			label.add(text);
			text->setText(writeLatitude(point));
			text->push_back(xy);
			text->setJustification(MLEFT);
			text->setVerticalAlign(MHALF);
			text->setBlanking(true);
		}
	}
	else {
		// we calculate the intersection of the longitudes with the right side
		double x = min_pcx_ + ((max_pcx_-min_pcx_)*.1);
		verticalLabels(label, max_pcx_, x, MLEFT);
	}
}

void Proj4Projection::labels(const LabelPlotting& label, BottomAxisVisitor& visitor)  const
{
	if ( false ) {
		const vector<double>& longitudes = label.longitudes();
		const double lat = min_latitude_ + (max_latitude_-min_latitude_)*.8;
		for (unsigned int lon = 0; lon < longitudes.size(); lon++ )
		{
			UserPoint point(longitudes[lon],lat);
			PaperPoint xy = (*this)(point);
			if ( !in(xy) ) continue;
			Text *text = new Text();
			label.add(text);
			text->setText(writeLongitude(point));
			text->push_back(xy);
			text->setJustification(MCENTRE);
			text->setVerticalAlign(MTOP);
			text->setBlanking(true);
		}
	}
	else {
			// we calculate the intersection of the longitudes with the right side
		double y = min_pcy_ + ((max_pcy_-min_pcy_)*.8);
		horizontalLabels(label, min_pcy_, y, MTOP);
	}
}

inline double CA(PaperPoint& p1, PaperPoint& p2)
{
    return (p2.x() - p1.x()) ? (p2.y() - p1.y())/(p2.x() - p1.x()) : 0;
}

inline double CB(double a, PaperPoint& p)
{
    return p.y() - a * p.x();
}

inline double CX(double a, double b, double y)
{
    return (a) ? (y - b)/a : 0;
}

inline double CY(double a, double b, double x)
{
    return (a * x) + b;
}

inline bool between(double x, double x1, double x2)
{
	return ( std::min(x1, x2) <= x && x <= std::max(x1, x2) );
}

void Proj4Projection::verticalLabels(const LabelPlotting& label, double x, double pos, Justification justif)  const
{
    const vector<double>& longitudes = label.longitudes();
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon)
    {
        // find the equation of the line using 2 points : lon/-20 -->lon/ +20
    	for ( double lat1 = -90, lat2 = -80; lat2 < 90; lat1+=10, lat2+=10) {
    		UserPoint geo1(*lon, lat1);
    		UserPoint geo2(*lon, lat2);
    		PaperPoint xy1 = (*this)(geo1);
    		PaperPoint xy2 = (*this)(geo2);
    		if ( between(x, xy1.x_, xy2.x_) ) {
    			double a = CA(xy1, xy2);
    			double b = CB(a, xy1);
    			PaperPoint xy(x, CY(a, b, x));
    			if ( !in(xy) ) continue;
    			UserPoint geo;
    	        revert(xy, geo);
    	        xy.x(pos);
    			if ( !same(geo.x(),*lon ) ) continue;
    			Text* text = new Text();
    			label.add(text);
    			text->setJustification(justif);
    			text->setVerticalAlign(MHALF);
    		    text->setText(writeLongitude(geo));
    		    text->push_back(xy);
    		}
    	}
    }
}

void Proj4Projection::horizontalLabels(const LabelPlotting& label, double y, double pos, VerticalAlign align)  const
{
	const vector<double>& longitudes = label.longitudes();
	for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon) {
	    // find the equation of the line using 2 points : lon/-20 -->lon/ +20
		for ( double lat1 = -90, lat2 = -80; lat2 < 90; lat1+=10, lat2+=10) {
			UserPoint geo1(*lon, lat1);
			UserPoint geo2(*lon, lat2);
			PaperPoint xy1 = (*this)(geo1);
			PaperPoint xy2 = (*this)(geo2);
			if ( between(y, xy1.y_, xy2.y_) ) {
				double a = CA(xy1, xy2);
				double b = CB(a, xy1);
				PaperPoint xy(CX(a, b, y), y);
				if ( !in(xy) ) continue;
	        	UserPoint geo;
	        	revert(xy, geo);
	        	xy.y(pos);
	        	if ( !same(geo.x(), *lon ) ) continue;
	        	Text* text = new Text();
	        	label.add(text);
	        	text->setJustification(MCENTRE);
	        	text->setVerticalAlign(align);
	        	text->setText(writeLongitude(geo));
	        	text->push_back(xy);
			}
		}
	}
}

void Proj4Projection::labels(const LabelPlotting& label, TopAxisVisitor& visitor)  const
{
	if ( true ) {
		const vector<double>& longitudes = label.longitudes();
		const double lat = min_latitude_ + (max_latitude_-min_latitude_)*.2;
		for (unsigned int lon = 0; lon < longitudes.size(); lon++ )
		{
			UserPoint point(longitudes[lon],lat);
			PaperPoint xy = (*this)(point);
			if ( !in(xy) ) continue;
			Text *text = new Text();
			label.add(text);
			text->setText(writeLongitude(point));
			text->push_back(xy);
			text->setJustification(MCENTRE);
			text->setVerticalAlign(MBOTTOM);
			text->setBlanking(true);
		}
	}
	else {
		// we calculate the intersection of the longitudes with the right side
		double y = min_pcy_ + ((max_pcy_-min_pcy_)*.2);
		horizontalLabels(label, max_pcy_, y, MBOTTOM);
	}
}

void Proj4Projection::reprojectComponents(double& x, double& y, pair<double, double>& components) const
{
	const double speed = sqrt((components.first * components.first) + (components.second * components.second));
	const double angle = atan2(components.second,components.first);
	double ppx=x+cos(angle);
	double ppy=y+sin(angle);

	fast_reproject(ppx, ppy);
	fast_reproject(x, y);

	const double rotation = atan2((ppy - y), (ppx - x));

	// we the angle and the spped we compute u/v...
	components.first  = speed * cos(rotation);
	components.second = speed * sin(rotation);
}

void Proj4Projection::revert(const vector< std::pair<double, double> > & in, vector< std::pair<double, double> > & out) const
{
	const_cast<Proj4Projection*>(this)->init();
	out.reserve(in.size());
	for ( vector< std::pair<double, double> >::const_iterator pt = in.begin();  pt != in.end(); ++pt) {
		  double x = pt->first;
		  double y = pt->second;
		  PaperPoint p(x, y);

		  if ( PCEnveloppe_->within(p) == false ) {
				  out.push_back(make_pair(-1000, -1000));
			  continue;
		  }

		  int error =  pj_transform(to_, from_, 1, 1, &x, &y, NULL );

		  if ( error  ) {
			  MagLog::error() << pj_strerrno(error) << " for " << pt->first << " " << pt->second << endl;
			  out.push_back(make_pair(-1000, -1000));
		  }
		  else {
			  double lon = x*RAD_TO_DEG;
			  if ( lon > gridMaxLon_ ) lon -= 360.;
			  else if ( lon < gridMinLon_ ) lon += 360.;
			  double lat = y*RAD_TO_DEG;
			  out.push_back(make_pair(lon, lat));
		  }
	}
}

void Proj4Projection::coastSetting(map<string, string>& setting, double abswidth, double absheight) const
{
	// work out the ratios of geographical to paper lengths
	//const double xratio = ( xpcmax_ - xpcmin_ ) / abswidth;
	//const double yratio = ( ypcmax_ - ypcmin_ ) / absheight;

	// choose the smallest (smaller ratio means more detail required)
	const double ratio = 10;

	std::string resol = "110m";
	if ( ratio < 100000 )  // highest resolution
	{
		resol = "10m";
	}
	else if ( ratio < 300000)   // medium resolution
	{
		resol = "50m";
	}
	resol = "110m";
	setting["resolution"]      = resol;
	setting["land"]       = resol + "/ne_" + resol + "_land";
	setting["ocean"]      = resol + "/ne_" + resol + "_ocean";
	setting["coast"]      = resol + "/ne_" + resol + "_coastline";
	setting["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
	setting["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
	setting["administrative_boundaries"] = resol + "/ne_" + resol + "_admin_1_states_provinces";

	MagLog::dev() << "GeoRectangularProjection::coastSetting[" << abswidth << ", " << absheight << "]->" <<  ratio << " resol: "<<resol<< endl;
}

void Proj4Projection::visit(MetaDataVisitor& visitor, 
	double left, double top, 
	double width, double height,
	double iwidth, double iheight)
{
	ostringstream java;
	double w = getMaxPCX() - getMinPCX();
	double h = getMaxPCY() - getMinPCY();
	java << "{";
	java << "\"name\" : \"proj4\",";
    java << "\"definition\" : \"" << definition_ <<  "\",";
    java << "\"proj4_definition\" : \"" << projection_->definition_ <<  "\",";
	java << "\"top\" : \"" << top <<  "\",";
	java << "\"left\" : \"" << left <<  "\",";
	java << "\"width\" : \"" << width <<  "\",";
	java << "\"height\" : \"" << height <<  "\",";
	java << "\"img_width\" : \"" << iwidth <<  "\",";
	java << "\"img_height\" : \"" << iheight <<  "\",";
	java << "\"pcxmin\" : \"" << getMinPCX() <<  "\",";
	java << "\"pcymin\" : \"" << getMinPCY() <<  "\",";
	java << "\"pcwidth\" : \"" << w <<  "\",";
	java << "\"pcheight\" : \"" << h <<  "\"";
	java << "}";
	visitor.add("projection", java.str());
	ostringstream wf;
	wf << (w/width)<< endl;
	wf << "0\n0\n";
	wf << -(h/height) << endl;
	wf << getMaxPCY() - (h/height)/2<< endl;
	wf <<  getMinPCX() +  (w/width)/ 2<< endl;
	visitor.add("world_file", wf.str());
}

MatrixHandler* Proj4Projection::prepareData(const AbstractMatrix& matrix) const {
	return new GeoBoxMatrixHandler(matrix, *this);
}

void Proj4Projection::fast_reproject(double& x, double& y) const
{
	x *= DEG_TO_RAD;
	y *= DEG_TO_RAD;
	pj_transform(from_, to_, 1, 1, &x, &y, NULL );
}

double Proj4Projection::patchDistance(double res) const {
	double x1 = 0;
	double y1 = 60;
	double x2 = 0;
	double y2 = 61;
	fast_reproject(x1, y1);
	fast_reproject(x2, y2);

	double degree = ((x1-x2) * (x1-x2)) + ((y1-y2) * (y1-y2));
	return 1000000000;
}

void  Proj4Projection::collect(MetaDataCollector& collector) const
{

	collector["Projection"] = definition_;
	collector["Proj4  Definition"] = projection_->definition();
}

void Proj4Projection::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const
{
	map<string, string> def;
	def["subpage_map_projection"] = definition_;
	def["subpage_map_area_definition"] = "corners";
	def["subpage_lower_left_longitude"] = tostring(ll.x_);
	def["subpage_lower_left_latitude"] = tostring(ll.y_);
	def["subpage_upper_right_longitude"] = tostring(ur.x_);
	def["subpage_upper_right_latitude"] = tostring(ur.y_);
	::toxml(out, def);
	out = "{" + out + "}";
}

void Proj4Projection::setDefinition(const string& json)
{
	if (json.empty())
				return;

			MagJSon helper;
			helper.interpret(json);

			XmlNode node = **helper.tree_.firstElement();
			node.name("");
			set(node);
}
