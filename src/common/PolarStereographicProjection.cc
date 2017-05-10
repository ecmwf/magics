/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PolarStereographicProjection.cc
    \brief Implementation of PolarStereographicProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri Jan 11 15:01:45 2008

*/

#include <PolarStereographicProjection.h>
#include <BasicSceneObject.h>
#include <GridPlotting.h>
#include <LabelPlotting.h>
#include <Text.h>
#include <MatrixHandler.h>
#include <MetaData.h>
#include <ParameterSettings.h>
#include <math.h>
#include <MagJSon.h>


using namespace magics;

/*!
  \brief Constructor
*/
PolarStereographicProjection::PolarStereographicProjection()  : projection_(0)
{
	projection_ = new TePolarStereographic(TeDatum(), vertical_longitude_*TeCDR, 0., 0., "Meters", TeNORTH_HEM);
	// Setup constnat variables for tiling
	originX_ = 0;
	originY_ = 0;
	tile_ = 256;

	TeCoord2D ur = TeCoord2D(45*TeCDR, 20*TeCDR);
	TeCoord2D ll = TeCoord2D(-135*TeCDR, 20*TeCDR);

	TeCoord2D llxy = projection_->LL2PC(ll);
	TeCoord2D urxy = projection_->LL2PC(ur);
	unit_ = (urxy.x() - llxy.x());
	unitEpsilon_ = unit_ * 0.000001;
	delete projection_;
	projection_ = 0;

}

/*!
  \brief Destructor
*/
PolarStereographicProjection::~PolarStereographicProjection()
{
	delete projection_;
}

void PolarStereographicProjection::print(ostream& out) const
{
	out << "PolarStereographicProjection[";
	PolarStereographicProjectionAttributes::print(out);
	out << "]"; 
} 

double PolarStereographicProjection::unitToCm(double width, double height) const
{
	// Reproject 2 point and find the unit
	const int hemis = (hemisphere_ ==  NORTH) ? 1 : -1;

	UserPoint ll1(0,  hemis*50);
	UserPoint ll2(0,  hemis*51);
	PaperPoint xy1 = (*this)(ll1);
	PaperPoint xy2 = (*this)(ll2);
	
	const double unit = abs(xy1.y() - xy2.y());
	const double wid  = getMaxPCY() - getMinPCY();
	if (!zero(wid))	return unit*(height/wid);
	return unit;
}

double PolarStereographicProjection::height() const { 
	// Reproject 2 point and find the unit
		int hemis = (hemisphere_ ==  NORTH) ? 1 : -1;
		bool pole;
		double height;
		UserPoint ll(0,  hemis*90);
		PaperPoint xy = (*this)(ll);

		pole =  ( xy.x() > xpcmin_ && xy.x() < xpcmax_  && xy.y() > ypcmin_ && xy.y() < ypcmax_ );
		
		if ( !pole )  { 
			height = (ymax_-ymin_); 
		}
		else {
			if (hemisphere_ ==  NORTH) {
				height = 180-ymax_-ymin_;
			}
			else {
			 height = 180+ymax_+ymin_;
			}
		}
	return height;
}

PaperPoint PolarStereographicProjection::operator()(const UserPoint& point)  const
{
	ASSERT(projection_);

	TeCoord2D geo = TeCoord2D(point.x()*TeCDR, point.y()*TeCDR);
	TeCoord2D xy = projection_->LL2PC(geo);

	return PaperPoint(xy.x(), xy.y(), point.value(), point.missing(), point.border(), 0, point.name());
}



PaperPoint PolarStereographicProjection::operator()(const PaperPoint& point)  const
{
	return Transformation::operator()(point);
}

void PolarStereographicProjection::revert(const vector< std::pair<double, double> > & in, vector< std::pair<double, double> > & out) const
{
	ASSERT(projection_);
	out.reserve(in.size());

	for ( vector< std::pair<double, double> >::const_iterator pt = in.begin();  pt != in.end(); ++pt) {
		TeCoord2D texy = TeCoord2D(pt->first, pt->second);
		TeCoord2D geo = projection_->PC2LL(texy);

		if ( texy.y() > 0 && same(texy.x(), 0) )
			out.push_back(make_pair(-180, geo.y()*TeCRD));
		else
			out.push_back(make_pair(geo.x()*TeCRD, geo.y()*TeCRD));
	}
}

void PolarStereographicProjection::revert(const PaperPoint& xy, UserPoint& point)  const
{
	ASSERT(projection_);
	TeCoord2D texy = TeCoord2D(xy.x(), xy.y());
	TeCoord2D geo = projection_->PC2LL(texy);
  
	point = UserPoint(geo.x()*TeCRD, geo.y()*TeCRD);
}



bool PolarStereographicProjection::needShiftedCoastlines()  const
{
	return false;
}

void PolarStereographicProjection::init(double width, double height)
{
	if ( !projection_ ) 
		projection_ = new TePolarStereographic(TeDatum(), vertical_longitude_*TeCDR, 0., 0., "Meters", (hemisphere_ == NORTH) ? TeNORTH_HEM : TeSOUTH_HEM);
	
	

	if ( magCompare(area_, "full") ) {
		ymin_ = ( hemisphere_ == NORTH ) ? -20. : 20.;
		ymax_ = ( hemisphere_ == NORTH ) ? -20. : 20.;
		xmin_ = ( hemisphere_ == NORTH ) ? -45. + vertical_longitude_ : 45. + vertical_longitude_;
		xmax_ = ( hemisphere_ == NORTH ) ? 135. + vertical_longitude_ : -135. + vertical_longitude_;
		
	}
	else
		magCompare(area_, "corners" ) ?  corners() : centre(width, height);
	double llx, urx , lly , ury;
		
	if ( magCompare(system_, "projection") == false )
	{
		TeCoord2D ll = TeCoord2D(xmin_*TeCDR, ymin_*TeCDR);
		TeCoord2D ur = TeCoord2D(xmax_*TeCDR, ymax_*TeCDR);

		TeCoord2D llxy = projection_->LL2PC(ll);
		TeCoord2D urxy = projection_->LL2PC(ur);
    
		TeCoord2D ell = TeCoord2D(-20*TeCDR, 40*TeCDR);
		TeCoord2D exy = projection_->LL2PC(ell);
    
		ell = projection_->PC2LL(exy);
    
		// Create a grid 100/100 to calculate the corners ...
     
		llx = ::min(urxy.x(), llxy.x());
		urx = std::max(urxy.x(), llxy.x());

		if ( (urx - llx) < 10) {
				urx = llx+10.;

		}

		lly = ::min(urxy.y(), llxy.y());
		ury = std::max(urxy.y(), llxy.y());
		if ( ( ury - lly ) < 10) {
			ury = lly+10.;

		}
		xmin_ = DBL_MAX;
		ymin_ = DBL_MAX;
		xmax_ = DBL_MIN;
		ymax_ = DBL_MIN;

		double stepx= (urx - llx)/100.;
		double stepy= (ury - lly)/100.;
		if (same(stepx, 0) )return;
		if (same(stepy, 0) ) return;;
		for (double x = llx; x <= urx; x += stepx )
		{
		   for (double y = lly; y <= ury; y += stepy )
		   {
		       TeCoord2D xy(x, y);
		       TeCoord2D ll = projection_->PC2LL(xy);
		       double xx = ll.x()*TeCRD;
		       double yy = ll.y()*TeCRD;

		       if (xx < xmin_) xmin_ = xx;
		       if (xx > xmax_) xmax_ = xx;
		       if (yy < ymin_) ymin_ = yy;
		       if (yy > ymax_) ymax_ = yy;
		   }
		}

		if (xmax_ - xmin_ > 358)
		{
			// Wrap around!
			xmax_=180;
			xmin_=-180; 
		}
	
		xpcmin_ = llx;
		ypcmin_ = lly;
		xpcmax_ = urx;
		ypcmax_ = ury; 
	}
	else {
		llx = ::min(xmin_, xmax_);
		urx = std::max(xmin_, xmax_);
		lly = ::min(ymin_, ymax_);
		ury = std::max(ymin_, ymax_);
		TeCoord2D llxy(llx, lly);
		TeCoord2D ll = projection_->PC2LL(llxy); 
		TeCoord2D urxy(urx, ury);
		TeCoord2D  ur = projection_->PC2LL(urxy); 
		         
		xmin_ =  ::min(ll.x()*TeCRD, ur.x()*TeCRD);
		xmax_ = std::max(ll.x()*TeCRD, ur.x()*TeCRD); 
		ymin_ =  ::min(ll.y()*TeCRD, ur.y()*TeCRD);
		ymax_ =  std::max(ll.y()*TeCRD, ur.y()*TeCRD);
		double stepx= (urx - llx)/100.;
		double stepy= (ury - lly)/100.;
		for (double x = llx; x <= urx; x += stepx )
		{
		   for (double y = lly; y <= ury; y += stepy )
		   {
        		  TeCoord2D xy(x, y);
        		  TeCoord2D ll = projection_->PC2LL(xy);
        		  double xx = ll.x()*TeCRD;
        		  double yy = ll.y()*TeCRD;

        		  if (xx < xmin_) xmin_ = xx;
        		  if (xx > xmax_) xmax_ = xx;
        		  if (yy < ymin_) ymin_ = yy;
        		  if (yy > ymax_) ymax_ = yy;
		   }
		}

		if (xmax_ - xmin_ > 358)
		{
			// Wrap around!
			xmax_=180;
			xmin_=-180; 
		}

		xpcmin_ = llx;
		ypcmin_ = lly;
		xpcmax_ = urx;
		ypcmax_ = ury; 

		PCEnveloppe_->clear();

		PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmin_));
		PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmax_));
		PCEnveloppe_->push_back(PaperPoint(xpcmax_, ypcmax_));
		PCEnveloppe_->push_back(PaperPoint(xpcmax_, ypcmin_));
		PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmin_));

		boost::geometry::correct(PCEnveloppe_->polygon_);


		MagLog::dev() << " Projection definition-->[" << ymin_ << ", " << xmin_ << ", " << xmax_ << ", " << ymax_ << "]" << endl;
	}



	// Tiling information
	askedxmin_ =  std::min(xpcmin_, xpcmax_);
	askedxmax_ =  std::max(xpcmin_, xpcmax_);
	askedymin_ =  std::min(ypcmin_, ypcmax_);
	askedymax_ =  std::max(ypcmin_, ypcmax_);


}

void PolarStereographicProjection::fill(double& width, double& height)  
{  
	init(width, height);
	Transformation::fill(width, height);
	init(width, height);
}

void PolarStereographicProjection::tile(double& width, double& height)
{
	init(width, height);
	Transformation::tile(width, height);
	init(width, height);
}

void PolarStereographicProjection::aspectRatio(double& width, double& height)  
{  
	init(width, height);
	Transformation::aspectRatio(width, height);
}

void PolarStereographicProjection::smallestBoundingBox(double& xmin, double& ymin, double& xmax, double& ymax)  const
{
		vector< std::pair<double, double> > geo;
		vector< std::pair<double, double> > xy;

		double xpcmax =  xpcmax_;
		double xpcmin =  xpcmin_;
		double ypcmax =  ypcmax_;
		double ypcmin =  ypcmin_;

		const double xs = (xpcmax- xpcmin)/99.;
		const double ys = (ypcmax- ypcmin)/99.;
		// Walk along the boundary...
		double x,y;
		for (int i = 0; i < 100; i++) {
			x = xpcmin +(i*xs);
			for (int i = 0; i < 100; i++) {
				y = ypcmin +(i*ys);
				xy.push_back(make_pair(x, y));
			}
		}
		revert(xy, geo);
		xmin=DBL_MAX;
		xmax=DBL_MIN;
		ymin=DBL_MAX;
		ymax=DBL_MIN;

		for (vector< std::pair<double, double> >::iterator point = geo.begin(); point != geo.end(); ++point) {
			if ( xmin > point->first) xmin = point->first;
			if ( xmax < point->first) xmax = point->first;
			if ( ymin > point->second) ymin = point->second;
			if ( ymax < point->second) ymax = point->second;
			//userEnveloppe_->push_back(PaperPoint(point->first, point->second));
		}


}

void PolarStereographicProjection::boundingBox(double& xmin, double& ymin, double& xmax, double& ymax)  const
{
	smallestBoundingBox(xmin, ymin, xmax, ymax);
	xmin -= 5;
	xmax += 5;
	ymin -= 5;
	ymax += 5;
	
	MagLog::dev() << " Projection bounding box-->[" << xmin << ", " << xmax << ", " << ymin << ", " << ymax << "]" << endl;
}

double PolarStereographicProjection::getMinX()  const
{
	return xmin_;
}

double PolarStereographicProjection::getMinY()  const
{
	return ymin_;
}

double PolarStereographicProjection::getMaxX()  const
{
	return xmax_;
}

double PolarStereographicProjection::getMaxY()  const
{
	return ymax_;
}

double PolarStereographicProjection::getMinPCX()  const
{
	return xpcmin_;
}

double PolarStereographicProjection::getMinPCY()  const
{
	return ypcmin_;
}

double PolarStereographicProjection::getMaxPCX()  const
{
	return xpcmax_;
}

double PolarStereographicProjection::getMaxPCY()  const
{
	return ypcmax_;
}

void PolarStereographicProjection::gridLongitudes(const GridPlotting& grid)  const
{
	const vector<double>& longitudes = grid.longitudes();
	const vector<double>& latitudes = grid.latitudes();
	for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon)
	{
		if (*lon < xmin_ ) continue;
		if (*lon > xmax_ ) continue;
		Polyline poly;

		double min = ::min(latitudes.front(), latitudes.back());
		double max = std::max(latitudes.front(), latitudes.back());
		

		for (double lat = min; lat <= max; lat += 1) {
			poly.push_back((*this)(UserPoint(*lon,lat)));
		}  
		grid.add(poly);     
	}
}

void PolarStereographicProjection::gridLatitudes(const GridPlotting& grid)  const
{
	const vector<double>& latitudes = grid.latitudes();
	for (vector<double>::const_iterator lat = latitudes.begin(); lat != latitudes.end(); ++lat)
	{
		if (*lat < ymin_ ) continue;
		if (*lat > ymax_ ) continue;
	    Polyline poly;        

            for (int lon = -180; lon <= 180; lon += 1) {
                poly.push_back((*this)(UserPoint(lon,*lat)));
                
            }	
	    grid.add(poly);        
	}
}

inline double CA(TeCoord2D& p1, TeCoord2D& p2)
{
    return (p2.x() - p1.x()) ? (p2.y() - p1.y())/(p2.x() - p1.x()) : 0;
}

inline double CB(double a, TeCoord2D& p)
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

void PolarStereographicProjection::horizontalLabels(const LabelPlotting& label, double y, double yy, bool top) const
{
	double xmin, ymin, xmax, ymax;
	boundingBox(xmin, ymin, xmax,ymax);

	vector<double> done;
    const vector<double>& longitudes = label.longitudes();
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon)
    {
     	// find the equation of the line using 2 points : lon/-20 -->lon/ +20
        TeCoord2D p1 = TeCoord2D((*lon)*TeCDR, 20*TeCDR);
        TeCoord2D p2 = TeCoord2D((*lon)*TeCDR, -20*TeCDR);

        TeCoord2D p1xy = projection_->LL2PC(p1);
        TeCoord2D p2xy = projection_->LL2PC(p2);

        double a = CA(p1xy, p2xy);
        double b = CB(a, p1xy);
            
        TeCoord2D xy = TeCoord2D(CX(a, b, y), y);   

        PaperPoint point(CX(a, b, y), y);

        if ( !in(point) ) continue;
        bool add = true;
        for ( vector<double>::iterator x = done.begin(); x != done.end(); ++x) {
        	if ( same(*x, point.x_ ) ) {
        		add = false;
        		break;
        	}
        }

        if ( !add ) continue;
        done.push_back(point.x_);
    	UserPoint geo;
       	revert(point, geo);
       	point.y(yy);

        Text* text = new Text();
        text->setJustification(MCENTRE);
        text->setVerticalAlign(MBOTTOM);
        text->setVerticalAlign(top ? MBOTTOM : MTOP);
        label.add(text);
        text->setText(writeLongitude(geo));
        text->push_back(point);
     }
}

/*!
 Method to draw vertical labels.
 
 \sa LeftAxisVisitor RightAxisVisitor
 
*/
void PolarStereographicProjection::verticalLabels(const LabelPlotting& label, double x, double xx, bool left)  const
{
    const vector<double>& longitudes = label.longitudes();

    vector<double> done;
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon)
    {
        // find the equation of the line using 2 points : lon/-20 -->lon/ +20
        TeCoord2D p1 = TeCoord2D((*lon)*TeCDR, 20*TeCDR);
        TeCoord2D p2 = TeCoord2D((*lon)*TeCDR, -20*TeCDR);

        TeCoord2D p1xy = projection_->LL2PC(p1);
        TeCoord2D p2xy = projection_->LL2PC(p2);
    
        double a = CA(p1xy, p2xy);
        double b = CB(a, p1xy);
        if ( !a ) continue; // parallele line!
        
        TeCoord2D xy = TeCoord2D(x, CY(a, b, x) );       

        PaperPoint point(x, CY(a, b, x));
        if ( !in(point) ) continue;
        bool add = true;
        for ( vector<double>::iterator x = done.begin(); x != done.end(); ++x) {
            if ( same(*x, point.y_ ) ) {
                add = false;
                break;
             }
         }

         if ( !add ) continue;
         done.push_back(point.y_);

         UserPoint geo;
         revert(point, geo);
         point.x(xx);


        Text* text = new Text();
        label.add(text);
        text->setJustification(left ? MRIGHT: MLEFT);
        text->setVerticalAlign(MHALF);    
        text->setText(writeLongitude(geo));
        text->push_back(point);
     }
}


void PolarStereographicProjection::labels(const LabelPlotting& label, DrawingVisitor& )  const
{
	Text *text;
	const vector<double>& latitudes = label.latitudes();
	const vector<double>& grid = label.longitudes();
	vector<double> longitudes;
	longitudes.push_back(vertical_longitude_);
	for ( float i = 0; i < 360; i += 60.) {
		if ( find(grid.begin(), grid.end(), i) != grid.end() || 
			 find(grid.begin(), grid.end(), i-360) != grid.end() )
			longitudes.push_back(vertical_longitude_+i);
	}
	
	unsigned int flon = (unsigned int) std::max(1, (int) maground(longitudes.size()/4));

	for (unsigned int lat = 0; lat < latitudes.size(); lat++)
	{  
	    for (unsigned int lon = 0 ; lon < longitudes.size(); lon ++)
	    { 	   
	   	   UserPoint point(longitudes[lon],latitudes[lat]);
	   	   PaperPoint xy = (*this)(point);
	   	   
	   	   if ( !in(xy) ) continue;	   
	   
	   	   text = new Text();
	   	   label.add(text); // This will set the font!
	   	   text->setText(writeLatitude(point));
	       text->push_back(xy);
	    }
	}
}

void PolarStereographicProjection::labels(const LabelPlotting& label, TopAxisVisitor&)  const
{
	// Find intersection of the latitude line  with the top!
	// Y = top

	const double yy = ypcmin_ + ( ( ypcmax_ - ypcmin_  )*0.1);
	horizontalLabels(label, ypcmax_, yy, true);
}

void PolarStereographicProjection::labels(const LabelPlotting& label, BottomAxisVisitor&)  const
{
	// Find intersection of the latitude line  with the bottom!
	// Y = bottom
     
	const double yy = ypcmax_ - ( ( ypcmax_ - ypcmin_  )*0.1);
	horizontalLabels(label, ypcmin_, yy, false);
}

void PolarStereographicProjection::labels(const LabelPlotting& label, LeftAxisVisitor&)  const
{
	// Find intersection of the latitude line  with the left!
	// X = left
    
	const double xx = xpcmax_ - ( ( xpcmax_ - xpcmin_  )*0.1);
	verticalLabels(label, xpcmin_, xx, true);
}

void PolarStereographicProjection::labels(const LabelPlotting& label, RightAxisVisitor& )  const
{
	// Find intersection of the latitude line  with the right!
	// X = right
	
	const double xx = xpcmin_ + ( ( xpcmax_ - xpcmin_  )*0.1);
	verticalLabels(label, xpcmax_, xx, false);
}

/*!
 This method generates meta output for web interaction through JavaScript...
*/
void  PolarStereographicProjection::visit(MetaDataVisitor& visitor, 
	double left, double top, 
	double width, double height,
	double img_width, double img_height) 
{
	ostringstream java;

	double w = getMaxPCX() - getMinPCX();
	double h = getMaxPCY() - getMinPCY();

	java << "{";
	projection_->LL2PC(java);
		
	java << "\"top\" : " << top <<  ",";		
	java << "\"left\" : " << left <<  ",";		
	java << "\"width\" : " << width <<  ",";	
	java << "\"img_width\" : " << img_width <<  ",";	
	java << "\"height\" : " << height <<  ",";	
	java << "\"img_height\" : " << img_height <<  ",";	
	
	java << "\"pcxmin\" : " << getMinPCX() <<  ",";		
	java << "\"pcymin\" : " << getMinPCY() <<  ",";		
	java << "\"pcwidth\" : " << w <<  ",";	
	java << "\"pcheight\" : " << h <<  ",";
	java << "\"inwidth\" : \"" << askedWidth_ <<  "\",";
	java << "\"inheight\" : \"" << askedHeight_ <<  "\",";
	java << "\"xorig\" : \"" << xTile_ <<  "\",";
	java << "\"yorig\" : \"" << yTile_ <<  "\",";
	java << "\"zoom_level\" : \"" << zoomLevel_ <<  "\"";
	java << "}";	
	visitor.add("projection", java.str());
}

void PolarStereographicProjection::corners()
{
	// For backwards compatibility!
	if  ( min_longitude_ == -180 && max_longitude_ == 180 &&
		   min_latitude_ == -90 && max_latitude_ == 90 ) {

		
		min_latitude_ = ( hemisphere_ == NORTH ) ? -20. : 20.;
		max_latitude_ = ( hemisphere_ == NORTH ) ? -20. : 20.;
		min_longitude_ = ( hemisphere_ == NORTH ) ? -45. + vertical_longitude_: 45.+ vertical_longitude_;
		max_longitude_ = ( hemisphere_ == NORTH ) ? 135. + vertical_longitude_: -135.+ vertical_longitude_;
	}

	xmin_ = min_longitude_;
	xmax_ = max_longitude_;
	ymin_ = min_latitude_;
	ymax_ = max_latitude_;
}



void PolarStereographicProjection::centre(double width, double height)
{
    PaperPoint centre = (*this)(UserPoint(centre_longitude_, centre_latitude_));
      
    double x = (width* map_scale_)/200;
    double y = (height * map_scale_)/200;
    
           
    PaperPoint llxy(centre.x() - x, centre.y() - y);
    PaperPoint urxy(centre.x() + x, centre.y() + y);
    
    UserPoint ll;
    revert(PaperPoint(centre.x() - x, centre.y() - y), ll);
    UserPoint ur;
    revert(PaperPoint(centre.x() + x, centre.y() + y), ur);    

    xmin_ = ll.x();
    ymin_ = ll.y();
    xmax_ = ur.x();
    ymax_ = ur.y();
}




/*!polar
 Read in the documentation:	
 For Polar Stereographic projections, the thinning factor is the distance, 
 in both X and Y directions, corresponding to the projected INPUT_FIELD_LONGITUDE_STEP ,
 along 60 degrees latitude, multiplied by the value of WIND_THINNING_FACTOR . 
 After plotting at a grid point, all subsequent grid points, 
 whose distance from the current grid point is less than the thinning factor, will be ignored. 
 The default value is 2.0, e.g. the statement
*/
void PolarStereographicProjection::thin(MatrixHandler& matrix, double x, double y, vector<UserPoint>& out) const
{
	Transformation::thin(matrix, x, y, out);
	return;
	int yfactor = (int) ceil((float) x);

	int columns = matrix.columns();
	int rows = matrix.rows();
 
	for ( int row = 0; row < rows; row+=yfactor)
	{
		const double lat = matrix.row(row, 0);
		int incrementx;
		if (abs(lat) == 90) {
			incrementx = columns;
		}
		else {
			double coeff = (1-((lat/90.)*(lat/90.)));
			ASSERT ( coeff != 0);
			incrementx = yfactor / coeff;
			if (incrementx < 0 )
				incrementx = 1;
		}
		for ( int column = 0; column < columns; column+=incrementx)
		{
			const double lon = matrix.column(row, column);

			UserPoint point(lon, lat, matrix(row, column));
			out.push_back(point); 
		}
	}
}


void PolarStereographicProjection::setNewPCBox(double minx, double miny, double maxx, double maxy)
{
	PaperPoint p1(minx, miny);
	PaperPoint p2(maxx, maxy);
	UserPoint   ll, ur;

	revert(p1, ll);
	revert(p2, ur);

	min_longitude_ = ll.x();
	max_longitude_ = ur.x();
	min_latitude_ = ll.y();
	max_latitude_ = ur.y(); 

	corners();

	xpcmin_ = minx;
	ypcmin_ = miny;
	xpcmax_ = maxx;
	ypcmax_ = maxy;

	PCEnveloppe_->clear();

	PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmin_));
	PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmax_));
	PCEnveloppe_->push_back(PaperPoint(xpcmax_, ypcmax_));
	PCEnveloppe_->push_back(PaperPoint(xpcmax_, ypcmin_));
	PCEnveloppe_->push_back(PaperPoint(xpcmin_, ypcmin_));
}

void PolarStereographicProjection::operator()(const UserPoint& geo, vector<PaperPoint>& out) const
{
	PaperPoint pp = (*this)(geo);
		if ( in(pp) ) 
			out.push_back(pp);
}

void PolarStereographicProjection::reprojectComponents(double& x, double& y, pair<double, double>& components) const
{

	double speed = sqrt((components.first * components.first) + (components.second * components.second));
	double angle = atan2(components.second,components.first);

	double ppx=x+0.5;
	double ppy=y;
	fast_reproject(ppx, ppy);
	fast_reproject(x, y);

	components.first = ppx - x;
	components.second = ppy - y;

	double rotation = atan2((ppy - y), (ppx - x));

	// we the angle and the spped we compute u/v...
	components.first = speed * cos(angle+rotation);
	components.second = speed * sin(angle+rotation);
}

void PolarStereographicProjection::reprojectSpeedDirection(const PaperPoint& point, pair<double, double>& wind) const
{

	double x = point.x_;
	double y =  point.y_;

	double u = x + (  sin( wind.second*DEG_TO_RAD) );
	double v = y + (  cos( wind.second*DEG_TO_RAD) );

	fast_reproject(x, y);
	fast_reproject(u, v);
	
	double rotation = atan2((u - x), (v - y));
	
	wind.second =   (rotation*RAD_TO_DEG);
}

void PolarStereographicProjection::coastSetting(map<string, string>& setting, double abswidth, double absheight) const
{
	// work out the ratios of geographical to paper lengths
	const double xratio = ( xpcmax_ - xpcmin_ ) / abswidth;
	const double yratio = ( ypcmax_ - ypcmin_ ) / absheight;

	// choose the smallest (smaller ratio means more detail required)
	const double ratio = min (xratio, yratio);

	std::string resol = "110m";
	if ( ratio < 100000 )  // highest resolution
	{
		resol = "10m";
	}
	else if ( ratio < 300000)   // medium resolution
	{
		resol = "50m";
	}
	setting["resolution"]      = resol;
	setting["land"]       = resol + "/ne_" + resol + "_land";
	setting["ocean"]      = resol + "/ne_" + resol + "_ocean";
	setting["coast"]      = resol + "/ne_" + resol + "_coastline";
	setting["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
	setting["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
	
	//! \note Administraive borders hardcoded to 10m resolution (low res version do not contain all info)
	setting["administrative_boundaries"] = "10m/ne_10m_admin_1_states_provinces";

	MagLog::dev() << "GeoRectangularProjection::coastSetting[" << abswidth << ", " << absheight << "]->" <<  ratio << " resol: "<<resol<< endl;
}

MatrixHandler* PolarStereographicProjection::prepareData(const AbstractMatrix& matrix) const
{ return new GeoBoxMatrixHandler(matrix, *this); }

void PolarStereographicProjection::wraparound(const UserPoint& point, stack<UserPoint>& duplicates) const
{
	if ( in(point) ) {
		duplicates.push(point);
	}
}

Polyline& PolarStereographicProjection::getPCBoundingBox() const
{
	if ( PCEnveloppe_->empty() ) {
		getUserBoundingBox();
	}
	return *PCEnveloppe_;
}

Polyline& PolarStereographicProjection::getUserBoundingBox() const
{
	const double minlat = -90.;
	const double maxlat =  90.; 
	const double minlon = -180.;
	const double maxlon =  180.; 

	if ( userEnveloppe_->empty() ) {
		// left
		for ( int lat = minlat; lat <= maxlat; lat++) {
			userEnveloppe_->push_back(PaperPoint(minlon,lat));
		}
		// top
		for ( int lon = minlon; lon <= maxlon; lon++) {
			userEnveloppe_->push_back(PaperPoint(lon, maxlat));
		}
		// right
		for ( int lat = maxlat; lat >= minlat; lat--) {

			userEnveloppe_->push_back(PaperPoint(maxlon, lat));
		}
		// bottom
		for ( int lon = maxlon; lon >= minlon; lon--) {
			userEnveloppe_->push_back(PaperPoint(lon, minlat));
		}
	}
	return *userEnveloppe_;
}

#include <boost/geometry/algorithms/distance.hpp>
double PolarStereographicProjection::patchDistance(double res) const
{
	/*
	UserPoint geo1(0,60);
	UserPoint geo2(0,61);

	PaperPoint xy1 = (*this)(geo1);
	PaperPoint xy2 = (*this)(geo2);

	double x = xy1.distance(xy2);
	*/
	return 100000*res;
}

bool  PolarStereographicProjection::fast_reproject(double& x, double& y) const
{
	TeCoord2D geo = TeCoord2D(x*TeCDR, y*TeCDR);
	TeCoord2D xy = projection_->LL2PC(geo);
	x = xy.x();
	y = xy.y();
	return true;
}

void PolarStereographicProjection::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const
{
	map<string, string> def;
	def["subpage_map_projection"] = "polar_stereographic";
	def["subpage_map_area_definition"] = "corners";

	def["subpage_map_hemisphere"] = (hemisphere_ == NORTH) ? "north" : "south";
	def["subpage_map_vertical_longitude"] = tostring(vertical_longitude_);
	def["subpage_lower_left_longitude"] = tostring(ll.x_);
	def["subpage_lower_left_latitude"] = tostring(ll.y_);
	def["subpage_upper_right_longitude"] = tostring(ur.x_);
	def["subpage_upper_right_latitude"] = tostring(ur.y_);
	::toxml(out, def);
	out = "{" + out + "}";
}

void PolarStereographicProjection::setDefinition(const string& json)
{
	if (json.empty())
				return;

	MagJSon helper;
	helper.interpret(json);

	XmlNode node = **helper.tree_.firstElement();
	node.name("polar_stereographic");
	set(node);
}

UserPoint PolarStereographicProjection::reference() const
{
	UserPoint ll = ( hemisphere_ ==  NORTH ) ? UserPoint(0, 60) : UserPoint(0, -60);
	return ll;
}

