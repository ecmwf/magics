/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GeoRectangularProjection.cc
    \brief Implementation of GeoRectangularProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jan 10 17:24:36 2008

*/

#include <GeoRectangularProjection.h>

#include <Polyline.h>
#include <Text.h>

#include <UserPoint.h>
#include <SceneVisitor.h>
#include <GridPlotting.h>
#include <LabelPlotting.h>
#include <MatrixHandler.h>
#include "ParameterSettings.h"
#include "MagJSon.h"

using namespace magics;

/*!
  \brief Constructor
*/
GeoRectangularProjection::GeoRectangularProjection() : projection_(0)
{
	init();
	// Tiling information

		originX_ = 0;
		originY_ = 0;
		tile_ = 256;
		unit_ = 360.;
		unitEpsilon_ = unit_ * 0.0001;

}

/*!
  \brief Destructor

  \todo do we need here a "delete projection_;" as in MercatorProjection::~MercatorProjection() ?
*/
GeoRectangularProjection::~GeoRectangularProjection()
{
}

void GeoRectangularProjection::print(ostream& o) const
{
	o << "GeoRectangularProjection[";
	GeoRectangularProjectionAttributes::print(o);
	o << "]";
}



PaperPoint GeoRectangularProjection::operator()(const UserPoint& point)  const
{
	if ( !projection_ ) {
		return  PaperPoint(point.x(), point.y(), point.value(), point.missing(), point.border(), 0, point.name());

	}

	TeCoord2D geo = TeCoord2D(point.x()*TeCDR, point.y()*TeCDR);
	TeCoord2D xy = projection_->LL2PC(geo);

	return PaperPoint(xy.x(), xy.y(), point.value(), point.missing(), point.border(), 0, point.name());
}

PaperPoint GeoRectangularProjection::operator()(const PaperPoint& point)  const
{
	MagLog::dev() << "GeoRectangularProjection::operator()(...) needs implementing." << endl;
	return Transformation::operator()(point);
}

void GeoRectangularProjection::revert(const PaperPoint& xy, UserPoint& point)  const
{
	if ( !projection_ )
	{
		point = UserPoint(xy.x(), xy.y());
		return;
	}
	TeCoord2D texy = TeCoord2D(xy.x(), xy.y());
	TeCoord2D geo = projection_->PC2LL(texy);
	point = UserPoint(geo.x()*TeCRD, geo.y()*TeCRD);
}



void GeoRectangularProjection::revert(const vector< std::pair<double, double> > & input, vector< std::pair<double, double> > & output) const
{
	output.reserve(input.size());
	const vector< std::pair<double, double> >::const_iterator in_end =input.end();
	if ( !projection_ )
	{
		for ( vector< std::pair<double, double> >::const_iterator pt = input.begin();  pt != in_end; ++pt)
		{
			output.push_back(*pt);
		}
		return;
	}
	ASSERT(projection_);

	for ( vector< std::pair<double, double> >::const_iterator pt = input.begin();  pt != in_end; ++pt)
	{
		TeCoord2D texy = TeCoord2D(pt->first, pt->second);
		TeCoord2D geo = projection_->PC2LL(texy);
		output.push_back(make_pair(geo.x()*TeCRD, geo.y()*TeCRD));
	}
}

bool GeoRectangularProjection::needShiftedCoastlines()  const
{
	return true;
}

void GeoRectangularProjection::boundingBox(double& xmin, double& ymin,
			double& xmax, double& ymax)  const
{
	xmin = std::min(min_longitude_, max_longitude_);
	xmax = std::max(min_longitude_, max_longitude_);
	ymin = std::min(min_latitude_, max_latitude_);
	ymax = std::max(min_latitude_, max_latitude_);
	const double tol = 1.;
	xmin = xmin - tol;
	xmax = xmax + tol;
	ymin = (ymin < (-90+tol) ) ? -90. : ymin - tol;
	ymax = (ymax > ( 90-tol) ) ?  90. : ymax + tol;
}

void GeoRectangularProjection::smallestBoundingBox(double& xmin, double& ymin,
			double& xmax, double& ymax)  const
{
	xmin = std::min(min_longitude_, max_longitude_);
	xmax = std::max(min_longitude_, max_longitude_);
	ymin = std::min(min_latitude_, max_latitude_);
	ymax = std::max(min_latitude_, max_latitude_);
}

bool GeoRectangularProjection::verifyDef(const string& def) const
{
	return ( def == "EPSG:4326" || def == "CRS:84");
}


void GeoRectangularProjection::aspectRatio(double& w, double& h)
{
	init();
	Transformation::aspectRatio(w, h);
}


double GeoRectangularProjection::getMinX()  const
{
	 return min_longitude_;
}

double GeoRectangularProjection::getMinY()  const
{
	return min_latitude_;
}

double GeoRectangularProjection::getMaxX()  const
{
	return max_longitude_;
}

double GeoRectangularProjection::getMaxY()  const
{
	return max_latitude_;
}

void GeoRectangularProjection::setMinX(double xx)
{
	min_longitude_ = xx;
}

void GeoRectangularProjection::setMinY(double yy)
{
	min_latitude_ = yy;
}

void GeoRectangularProjection::setMaxX(double xx)
{
	max_longitude_ = xx;
}

void GeoRectangularProjection::setMaxY(double yy)
{
	max_latitude_ = yy;
}

double GeoRectangularProjection::getMinPCX()  const
{
	return xpcmin_;
}

double GeoRectangularProjection::getMinPCY()  const
{
	return ypcmin_;
}

double GeoRectangularProjection::getMaxPCX()  const
{
	return xpcmax_;
}

double GeoRectangularProjection::getMaxPCY()  const
{
	return ypcmax_;
}

void GeoRectangularProjection::gridLongitudes(const GridPlotting& grid)  const
{
	const vector<double>& longitudes = grid.longitudes();
	const double min = std::max(min_latitude_, -90.);
	const double max = std::min(max_latitude_, 90.);
	const double step = (max - min)/20;
	const vector<double>::const_iterator lon_end =longitudes.end();
	for (vector<double>::const_iterator lon = longitudes.begin(); lon != lon_end; ++lon)
	{
		Polyline poly;
		poly.setAntiAliasing(false);

		for (double lat = min; lat <= max+step; lat += step)
		{
			( lat >  max ) ?
				poly.push_back((*this)(UserPoint(*lon, max))) :
				poly.push_back((*this)(UserPoint(*lon,lat)));
		}
		grid.add(poly);
	}
}

void GeoRectangularProjection::gridLatitudes(const GridPlotting& grid)  const
{
	const vector<double>& latitudes = grid.latitudes();

	const double step = (max_longitude_ - min_longitude_)/20;
	const vector<double>::const_iterator lat_end = latitudes.end();
	for(vector<double>::const_iterator lat = latitudes.begin(); lat != lat_end; ++lat)
	{
		if ( *lat < -90 ) continue;
		if ( *lat > 90 ) continue;
		Polyline poly;
		poly.setAntiAliasing(false);
		for (double lon = getMinX(); lon < getMaxX(); lon += step)
		{
			poly.push_back((*this)(UserPoint(lon,*lat)));
		}
		// add the last one!
		poly.push_back((*this)(UserPoint(getMaxX(),*lat)));
		grid.add(poly);
	}
}

/*!
 \brief generates text to mark longitudes at the top

 \sa Text
*/
void GeoRectangularProjection::labels(const LabelPlotting& label, TopAxisVisitor&)  const
{
	Text *text;
	const double cy = min_latitude_ + (max_latitude_-min_latitude_)*.2;
	const vector<double>& longitudes = label.longitudes();
	const vector<double>::const_iterator lon_end = longitudes.end();
	for(vector<double>::const_iterator lon = longitudes.begin(); lon != lon_end; ++lon)
	{
		if ( *lon > min_longitude_ &&  *lon < max_longitude_ )
		{
			UserPoint point(*lon, cy);
			text = new Text();
			label.add(text);
			text->setText(writeLongitude(point));
			text->setJustification(MCENTRE);
			text->setVerticalAlign(MBOTTOM);
			text->push_back((*this)(point));
		}
	}
}

/*!
 \brief generates text to mark longitudes at the bottom

 \sa Text
*/
void GeoRectangularProjection::labels(const LabelPlotting& label, BottomAxisVisitor&)  const
{
	Text *text;
	const double cy = min_latitude_ + (max_latitude_-min_latitude_)*.8;
	const vector<double>& longitudes = label.longitudes();
	const vector<double>::const_iterator lon_end = longitudes.end();
	for(vector<double>::const_iterator lon = longitudes.begin(); lon != lon_end; ++lon)
	{
		if ( *lon > min_longitude_ &&  *lon < max_longitude_ )
		{
			UserPoint point(*lon, cy);
			text = new Text();
			label.add(text);
			text->setText(writeLongitude(point));
			text->setJustification(MCENTRE);
			text->setVerticalAlign(MTOP);
			text->push_back((*this)(point));
		}
	}
}

/*!
 \brief generates text to mark latitudes at the left

 \sa Text
*/
void GeoRectangularProjection::labels(const LabelPlotting& label, LeftAxisVisitor&)  const
{
	Text *text;
	const vector<double>& latitudes = label.latitudes();
	const vector<double>::const_iterator lat_end = latitudes.end();
	const double lon = max_longitude_ - ((max_longitude_-min_longitude_)*.1);
	for(vector<double>::const_iterator lat = latitudes.begin(); lat != lat_end; ++lat)
	{
		if ( *lat > min_latitude_ &&  *lat < max_latitude_ )
		{
			UserPoint point(lon, *lat);
			text = new Text();
			label.add(text);
			text->setText(writeLatitude(point));
	        text->setJustification(MRIGHT);
			text->setVerticalAlign(MHALF);
			text->push_back((*this)(point));
		}
	}
}

void GeoRectangularProjection::labels(const LabelPlotting&, DrawingVisitor&)  const
{
}

/*!
 \brief generates text to mark latitudes at the right

 \sa Text
*/
void GeoRectangularProjection::labels(const LabelPlotting& label, RightAxisVisitor&)  const
{
	Text *text;
	const vector<double>& latitudes = label.latitudes();
	const vector<double>::const_iterator lat_end = latitudes.end();
	for(vector<double>::const_iterator lat = latitudes.begin(); lat != lat_end; ++lat)
	{
		if ( *lat > min_latitude_ &&  *lat < max_latitude_ )
		{
			const double lon = min_longitude_ + ((max_longitude_-min_longitude_)*.1);
			UserPoint point(lon, *lat);
			text = new Text();
			label.add(text);
			text->setText(writeLatitude(point));
	        text->setJustification(MLEFT);
			text->setVerticalAlign(MHALF);
			text->push_back((*this)(point));

		}
	}
}

double GeoRectangularProjection::patchDistance(double res) const
{
	return res;
}

void GeoRectangularProjection::init()
{
	// make sure min < max!
	// reset any previous setting
	userEnveloppe_->clear();
	PCEnveloppe_->clear();

	while (min_longitude_ > max_longitude_) {
		max_longitude_ += 360;
		MagLog::warning() << "lower_left_longitude > upper_right_longitude --> upper_right_longitude is change to " << max_longitude_ << endl;
	}

	if (min_latitude_ > max_latitude_) {
		MagLog::warning() << "lower_left_latitude > upper_right_latitude --> swap" << endl;
		std::swap(min_latitude_, max_latitude_);
	}

	if ( max_longitude_ - min_longitude_  < min_area_ ) {
		max_longitude_ = min_longitude_ + min_area_;
		MagLog::warning() << "The geographical area has been extented to respect the minimal dimension" << endl;
	}

	if ( max_latitude_ -  min_latitude_ < min_area_) {
		max_latitude_ = min_latitude_ + min_area_;
		MagLog::warning() << "The geographical area has been extented to respect the minimal dimension" << endl;
	}

	if ( max_longitude_ - min_longitude_  > 1080 )
		max_longitude_ = min_longitude_ + 1080;

	// Now 	we try to get the min longitudes in the the ranges -360/+720
	while ( min_longitude_ < -360) {
		min_longitude_ += 360;
		max_longitude_ += 360;
	}
	while ( max_longitude_ > 720 ) {
		min_longitude_ -= 360;
		max_longitude_ -= 360;
	}

//	if ( min_latitude_ < -90) min_latitude_ = -90.;
//	if ( max_latitude_ > 90 ) max_latitude_ = 90;

	xpcmin_ = min_longitude_;
	ypcmin_ = min_latitude_;
	xpcmax_ = max_longitude_;
	ypcmax_ = max_latitude_;

	userEnveloppe_->push_back(PaperPoint(min_longitude_, min_latitude_));
	userEnveloppe_->push_back(PaperPoint(min_longitude_, max_latitude_));
	userEnveloppe_->push_back(PaperPoint(max_longitude_, max_latitude_));
	userEnveloppe_->push_back(PaperPoint(max_longitude_, min_latitude_));
	userEnveloppe_->push_back(PaperPoint(min_longitude_, min_latitude_));

	PCEnveloppe_->push_back(PaperPoint(min_longitude_, min_latitude_));
	PCEnveloppe_->push_back(PaperPoint(min_longitude_, max_latitude_));
	PCEnveloppe_->push_back(PaperPoint(max_longitude_, max_latitude_));
	PCEnveloppe_->push_back(PaperPoint(max_longitude_, min_latitude_));
	PCEnveloppe_->push_back(PaperPoint(min_longitude_, min_latitude_));

	askedxmin_ =  std::min(xpcmin_, xpcmax_);
			askedxmax_ =  std::max(xpcmin_, xpcmax_);
			askedymin_ =  std::min(ypcmin_, ypcmax_);
			askedymax_ =  std::max(ypcmin_, ypcmax_);
}

MercatorProjection::MercatorProjection()
{
}

MercatorProjection::~MercatorProjection()
{
	delete projection_;
}

void MercatorProjection::print(ostream& o) const
{
	o << " mercator[";
	GeoRectangularProjection::print(o);
	o << "]";
}

void MercatorProjection::init()
{
	if (!projection_)
		projection_ = new TeMercator(TeDatum(), 0);
	// make sure min < max!
	if (min_longitude_ > max_longitude_)
	{
		MagLog::warning() << "lower_left_lon > upper_right_lon --> swap" << endl;
		std::swap(min_longitude_, max_longitude_);
	}
	if (min_latitude_ > max_latitude_)
	{
		MagLog::warning() << "lower_left_lat > upper_right_lat --> swap" << endl;
		std::swap(min_latitude_, max_latitude_);
	}

	const double t = 2;
	min_latitude_ = std::max(min_latitude_, -89.);
	max_latitude_ = std::min(max_latitude_, 89.);
	min_longitude_ = std::max(min_longitude_, -180.);
	max_longitude_ = std::min(max_longitude_, 720.);

	if ( max_longitude_ - min_longitude_  < t)
			max_longitude_ = min_longitude_ + t;
	if ( max_latitude_ -  min_latitude_ < t)
			max_latitude_ = min_latitude_ + t;

	UserPoint   ll(min_longitude_, min_latitude_);
	UserPoint   ur(max_longitude_, max_latitude_);

	PaperPoint xy;

	xy = (*this)(ll);
	xpcmin_ = xy.x();
	ypcmin_ = xy.y();

	xy = (*this)(ur);
	xpcmax_ = xy.x();
	ypcmax_ = xy.y();
	userEnveloppe_->clear();
	PCEnveloppe_->clear();
	askedxmin_ =  std::min(xpcmin_, xpcmax_);
				askedxmax_ =  std::max(xpcmin_, xpcmax_);
				askedymin_ =  std::min(ypcmin_, ypcmax_);
				askedymax_ =  std::max(ypcmin_, ypcmax_);
}

bool MercatorProjection::fast_reproject(double& x, double& y) const
{
	TeCoord2D geo = TeCoord2D(x*TeCDR, y*TeCDR);
	TeCoord2D xy = projection_->LL2PC(geo);

	x = xy.x();
	y = xy.y();
	return true;
}



void GeoRectangularProjection::setNewPCBox(double minx, double miny, double maxx, double maxy)
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
}


/*!
  \brief Set the resolution of Coastlines
*/
void GeoRectangularProjection::coastSetting(map<string, string>& setting, double abswidth, double absheight) const
{
	// work out the ratios of geographical to paper lengths
	const double xratio = ( max_longitude_ - min_longitude_ ) / abswidth;
	const double yratio = ( max_latitude_  - min_latitude_ )  / absheight;

	// choose the smallest (smaller ratio means more detail required)
	const double ratio = min(xratio, yratio);

	std::string resol = "110m";
	if ( ratio < 0.8 )  // highest resolution
	{
		resol = "10m";
	}
	else if ( ratio < 3.5)   // medium resolution
	{
		resol = "50m";
	}

	setting["resolution"] = resol;
	setting["land"]       = resol + "/ne_" + resol + "_land";
	setting["ocean"]      = resol + "/ne_" + resol + "_ocean";
	setting["coast"]      = resol + "/ne_" + resol + "_coastline";
	setting["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
	setting["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
	setting["administrative_boundaries"] = resol + "/ne_" + resol + "_admin_1_states_provinces";

	MagLog::dev() << "GeoRectangularProjection::coastSetting[" << abswidth << ", " << absheight << "]->" <<  ratio << " resol: "<<resol<< endl;
}


MatrixHandler* GeoRectangularProjection::prepareData(const AbstractMatrix& matrix) const
{
	//return new MatrixHandler(matrix);
	return new GeoBoxMatrixHandler(matrix, *this);
}

void GeoRectangularProjection::populate(double lon, double lat, double value, vector<UserPoint>& out) const
{
	 while ( lon < min_longitude_)
	      lon += 360;
	 while ( lon > min_longitude_ + 360. )
	      lon -= 360.;

	 if ( !in(lon, lat) )
	 		return;

	 out.push_back(UserPoint(lon, lat, value));
	double normlon = lon;
	lon += 360.;
	 while ( lon >  min_longitude_ && lon < max_longitude_) {
		 out.push_back(UserPoint(lon, lat, value));
	     lon += 360;
	 }
	 // To the West
	 lon = normlon;
	 lon -=  360.;
	 while (lon >  min_longitude_ && lon < max_longitude_) {
	 	out.push_back(UserPoint(lon, lat, value));
	 	lon -= 360;
	 }
}

void GeoRectangularProjection::wraparound(const UserPoint& origin, stack<UserPoint>& out) const
{
	 UserPoint point = origin;
	 if (point.y_ > max_latitude_ || point.y_ < min_latitude_ )
	    return;

	 while ( point.x_ < min_longitude_)
	      point.x_ += 360;
	 while ( point.x_ > min_longitude_ + 360. )
	      point.x_ -= 360.;


	 if ( point.x_ < min_longitude_ || point.x_ > max_longitude_) {

		 return;
	 }

	 out.push(point);

	 double lon = point.x_;
	 // To the East
	 point.x_ = lon + 360.;
	 while (point.x_ >  min_longitude_ && point.x_ < max_longitude_) {
		 out.push(point);
	     point.x_ += 360;
	 }
	 // To the West
	 point.x_ = lon - 360.;
	 while (point.x_ >  min_longitude_ && point.x_ < max_longitude_) {
	 	out.push(point);
	 	point.x_ -= 360;
	 }
}

Polyline& GeoRectangularProjection::getPCBoundingBox() const
{
	if ( PCEnveloppe_->empty() ) {
		PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmin_));
		PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmax_));
		PCEnveloppe_->push_back(PaperPoint(xpcmax_, ypcmax_));
		PCEnveloppe_->push_back(PaperPoint(xpcmax_, ypcmin_));
		PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmin_));
	}
	return *PCEnveloppe_;
}

Polyline& GeoRectangularProjection::getUserBoundingBox() const
{
	if ( userEnveloppe_->empty() ) {

		userEnveloppe_->push_back(PaperPoint(min_longitude_, min_latitude_));
		userEnveloppe_->push_back(PaperPoint(min_longitude_, max_latitude_));
		userEnveloppe_->push_back(PaperPoint(max_longitude_, max_latitude_));
		userEnveloppe_->push_back(PaperPoint(max_longitude_, min_latitude_));
		userEnveloppe_->push_back(PaperPoint(min_longitude_, min_latitude_));
	}
	return *userEnveloppe_;
}

void GeoRectangularProjection::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const
{
	map<string, string> def;
	def["subpage_map_projection"] = "cylindrical";
	def["subpage_map_area_definition"] = "corners";
	def["subpage_lower_left_longitude"] = tostring(ll.x_);
	def["subpage_lower_left_latitude"] = tostring(ll.y_);
	def["subpage_upper_right_longitude"] = tostring(ur.x_);
	def["subpage_upper_right_latitude"] = tostring(ur.y_);
	::toxml(out, def);
	out = "{" + out + "}";
}

void GeoRectangularProjection::setDefinition(const string& json)
{
	if (json.empty())
			return;

		MagJSon helper;
		helper.interpret(json);

		XmlNode node = **helper.tree_.firstElement();
		node.name("cylindrical");
		set(node);
}

double MercatorProjection::patchDistance(double) const
{
	return 1000000;
}

double GeoRectangularProjection::ratio() const
{
	// return domain/fulldomain!

	return (( max_longitude_ - min_longitude_ ) * ( max_latitude_ - min_latitude_) ) / (360*180);

}
