/******************************** LICENSE ********************************


 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)
 
 Licensed under the Apache License, Version 2.0 (the "License"); 
 you may not use this file except in compliance with the License. 
 You may obtain a copy of the License at 
 
 	http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreedto in writing, software
 distributed under the License is distributed on an "AS IS" BASIS, 
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 See the License for the specific language governing permissions and 
 limitations under the License.


 ******************************** LICENSE ********************************/

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
	assert(projection_);

	TeCoord2D geo = TeCoord2D(point.x()*TeCDR, point.y()*TeCDR);
	TeCoord2D xy = projection_->LL2PC(geo);

	return PaperPoint(xy.x(), xy.y(), point.value(), point.missing(), point.border());
}



PaperPoint PolarStereographicProjection::operator()(const PaperPoint& point)  const
{
	return Transformation::operator()(point);
}

void PolarStereographicProjection::revert(const vector<pair<double, double> > & in, vector<pair<double, double> > & out) const
{
	assert(projection_);
	out.reserve(in.size());
	for ( vector<pair<double, double> >::const_iterator pt = in.begin();  pt != in.end(); ++pt) {
		TeCoord2D texy = TeCoord2D(pt->first, pt->second);
		TeCoord2D geo = projection_->PC2LL(texy);
		out.push_back(make_pair(geo.x()*TeCRD, geo.y()*TeCRD));
	}
}

void PolarStereographicProjection::revert(const PaperPoint& xy, UserPoint& point)  const
{
	assert(projection_);
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
		xmin_ = ( hemisphere_ == NORTH ) ? -45. : 45.;
		xmax_ = ( hemisphere_ == NORTH ) ? 135. : -135.;
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
		urx = ::max(urxy.x(), llxy.x());

		if ( (urx - llx) < 10) {
				urx = llx+10.;

		}

		lly = ::min(urxy.y(), llxy.y());
		ury = ::max(urxy.y(), llxy.y());
		if ( ( ury - lly ) < 10) {
			ury = lly+10.;

		}
		xmin_ = DBL_MAX;
		ymin_ = DBL_MAX;
		xmax_ = DBL_MIN;
		ymax_ = DBL_MIN;

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
	}
	else {
		llx = ::min(xmin_, xmax_);
		urx = ::max(xmin_, xmax_);
		lly = ::min(ymin_, ymax_);
		ury = ::max(ymin_, ymax_);
		TeCoord2D llxy(llx, lly);
		TeCoord2D ll = projection_->PC2LL(llxy); 
		TeCoord2D urxy(urx, ury);
		TeCoord2D  ur = projection_->PC2LL(urxy); 
		         
		xmin_ =  ::min(ll.x()*TeCRD, ur.x()*TeCRD);
		xmax_ = ::max(ll.x()*TeCRD, ur.x()*TeCRD); 
		ymin_ =  ::min(ll.y()*TeCRD, ur.y()*TeCRD);
		ymax_ =  ::max(ll.y()*TeCRD, ur.y()*TeCRD);
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
		vector<pair<double, double> > geo;
		vector<pair<double, double> > xy;

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

		for (vector<pair<double, double> >::iterator point = geo.begin(); point != geo.end(); ++point) {
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

void PolarStereographicProjection::setMinX(double x)  
{
	MagLog::dev() << "PolarStereographicProjection::setMinX(...) needs implementing." << endl;
	Transformation::setMinX(x);
}

void PolarStereographicProjection::setMinY(double y)  
{
	MagLog::dev() << "PolarStereographicProjection::setMinY(...) needs implementing." << endl;
	Transformation::setMinY(y);
}

void PolarStereographicProjection::setMaxX(double x)  
{
	MagLog::dev() << "PolarStereographicProjection::setMaxX(...) needs implementing." << endl;
	Transformation::setMaxX(x);
}

void PolarStereographicProjection::setMaxY(double y)  
{
	MagLog::dev() << "PolarStereographicProjection::setMaxY(...) needs implementing." << endl;
	Transformation::setMaxY(y);
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
		double max = ::max(latitudes.front(), latitudes.back());
		

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
	//const vector<double>& longitudes = label.longitudes();
	vector<double> longitudes;
	longitudes.push_back(0);
	unsigned int flat = (unsigned int) std::max(1, (int) maground(latitudes.size()/4));
	unsigned int flon = (unsigned int) std::max(1, (int) maground(longitudes.size()/4));

	for (unsigned int lat = 0; lat < latitudes.size(); lat += flat)
	{  
	    for (unsigned int lon = 0 ; lon < longitudes.size(); lon += flon)
	    { 	   
	   	   UserPoint point(longitudes[lon],latitudes[lat]);
	   	   PaperPoint xy = (*this)(point);
	   	   
	   	   if ( !in(xy) ) continue;	   
	   
	   	   text = new Text();
	   	   label.add(text); // This will set the font!
	   	   text->setText(writeLatitude(point));
	       text->push_back(xy);
	       text->setBlanking(true);

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
		min_longitude_ = ( hemisphere_ == NORTH ) ? -45. : 45.;
		max_longitude_ = ( hemisphere_ == NORTH ) ? 135. : -135.;
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




/*!
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
			assert ( coeff != 0);
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
	double a = 90 - (wind.second); 
	const double x = 3.14/180.;
	a *= x;
	double xx =0, yy=0;
	const double speed =wind.first; 
	wind.first = speed-1 * cos(a);
	wind.second = speed-1 * sin(a);
	reprojectComponents(xx, yy, wind);
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
	setting["lakes"]      = resol + "/" + resol + "_lakes";
	setting["land"]       = resol + "/" + resol + "_land";
	setting["rivers"]     = resol + "/" + resol + "_rivers_lake_centerlines";
	setting["boundaries"] = resol + "/" + resol + "_admin_0_boundary_lines_land";
	
	//! \note Administraive borders hardcoded to 10m resolution (low res version do not contain all info)
	setting["administrative_boundaries"] = "10m/10m_admin_1_states_provinces_shp";

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

	double minlat = -90;
	double maxlat = 90; 
	double minlon = -180;
	double maxlon = 180; 

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

void  PolarStereographicProjection::fast_reproject(double& x, double& y) const
{
	TeCoord2D geo = TeCoord2D(x*TeCDR, y*TeCDR);
	TeCoord2D xy = projection_->LL2PC(geo);
	x = xy.x();
	y = xy.y();

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


