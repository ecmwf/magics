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

/*! \file SatelliteProjection.cc
    \brief Implementation of the Template class SatelliteProjection.
    
    Magics Team - ECMWF 2006
    
    Started: Fri 21-Apr-2006
    
    Changes:
    
*/



#include "SatelliteProjection.h"
#include "Polyline.h"
#include "Text.h"
#include "GridPlotting.h"

using namespace magics;

class OutMagException
{
public:
	OutMagException() {}
	~OutMagException() {}
};

SatelliteProjection::SatelliteProjection() :
//	TeSatelliteProjection(TeDatum(), 1, 1, 0.00012565,  0.00012565, 1250, 1250, 0, 0, 42164988.876, 0, 0)
//	TeSatelliteProjection(TeDatum("Spherical",6.371000e+06,0.,0.,0.,0.), 1, 1, 0.00017285102306, 0.00017294945759, 919, 921, 0, 0, 42164988.876, 0, 0) //works for met8.grb
	TeSatelliteProjection(TeDatum("Spherical",6.371000e+06,0.,0.,0.,0.), 1, 1, 0.0174527890614, 0.0174527890636, 928, 926, 0, 0, 42164988.876, 0, 0) //works for met9.grb
{
	
     minlon_ = -63;
     maxlon_ = 63;
     minlat_ = -80;
     maxlat_ = 80;
}

void SatelliteProjection::operator()(Layout& layout) const
{    
    TeCoord2D ll = TeCoord2D(minlon_*TeCDR, 0*TeCDR);
    TeCoord2D llxy = const_cast<SatelliteProjection*>(this)->LL2PC(ll);
    llx_ = llxy.x();
    ll = TeCoord2D(0*TeCDR, minlat_*TeCDR);
    llxy = const_cast<SatelliteProjection*>(this)->LL2PC(ll);
    lly_ = llxy.y();
    ll = TeCoord2D(maxlon_*TeCDR, 0*TeCDR);
    llxy = const_cast<SatelliteProjection*>(this)->LL2PC(ll);
    urx_ = llxy.x();
    ll = TeCoord2D(0*TeCDR, maxlat_*TeCDR);
    llxy = const_cast<SatelliteProjection*>(this)->LL2PC(ll);
    ury_ = llxy.y();
    
    ll = TeCoord2D(0*TeCDR, 0*TeCDR);
    llxy = TeCoord2D(0*TeCDR, maxlat_*TeCDR);

//TEST
#if 1
TeCoord2D f11 = TeCoord2D(-80*TeCDR, 0*TeCDR);
TeCoord2D f12 = const_cast<SatelliteProjection*>(this)->LL2PC(f11);
TeCoord2D f13 = const_cast<SatelliteProjection*>(this)->PC2LL(f12);

TeCoord2D f21 = TeCoord2D(80*TeCDR, 0*TeCDR);
TeCoord2D f22 = const_cast<SatelliteProjection*>(this)->LL2PC(f21);
TeCoord2D f23 = const_cast<SatelliteProjection*>(this)->PC2LL(f22);

TeCoord2D f31 = TeCoord2D(0*TeCDR, -85*TeCDR);
TeCoord2D f32 = const_cast<SatelliteProjection*>(this)->LL2PC(f31);
TeCoord2D f33 = const_cast<SatelliteProjection*>(this)->PC2LL(f32);

TeCoord2D f41 = TeCoord2D(0*TeCDR, 85*TeCDR);
TeCoord2D f42 = const_cast<SatelliteProjection*>(this)->LL2PC(f41);
TeCoord2D f43 = const_cast<SatelliteProjection*>(this)->PC2LL(f42);

f11 = TeCoord2D(-90*TeCDR, 0*TeCDR);
f12 = const_cast<SatelliteProjection*>(this)->LL2PC(f11);
f13 = const_cast<SatelliteProjection*>(this)->PC2LL(f12);

f21 = TeCoord2D(90*TeCDR, 0*TeCDR);
f22 = const_cast<SatelliteProjection*>(this)->LL2PC(f21);
f23 = const_cast<SatelliteProjection*>(this)->PC2LL(f22);

f31 = TeCoord2D(0*TeCDR, -90*TeCDR);
f32 = const_cast<SatelliteProjection*>(this)->LL2PC(f31);
f33 = const_cast<SatelliteProjection*>(this)->PC2LL(f32);

f41 = TeCoord2D(0*TeCDR, 90*TeCDR);
f42 = const_cast<SatelliteProjection*>(this)->LL2PC(f41);
f43 = const_cast<SatelliteProjection*>(this)->PC2LL(f42);
#endif

    double width = abs(urx_ - llx_);
    double shiftx = width*0.05; 
    double height = abs(ury_ - lly_);
    double shifty = height*0.05;
    
   
    width  += 2*shiftx;
    height += 2*shifty;
     llx_ -= shiftx;
    urx_ += shiftx;
    lly_ -= shifty;
    ury_ += shifty;
    
   
    
    setAspectRatio(layout, width, height, llx_, urx_, lly_, ury_);
    // The left bozx and right are 4 % ..
    double boxWidth_ = width/100*4;
    
   
    
    if (layout["drawing_area"]) layout["drawing_area"]->setCoordinates(llx_, urx_, lly_, ury_);
    if (layout["top_comment_box"]) layout["top_comment_box"]->setCoordinates(llx_, urx_, ury_- 100, ury_ + 100);
    if (layout["bottom_comment_box"]) layout["bottom_comment_box"]->setCoordinates(llx_, urx_, lly_- 100, lly_ + 100);
    if (layout["left_comment_box"]) layout["left_comment_box"]->setCoordinates(llx_-boxWidth_, llx_+boxWidth_, lly_, ury_);
    if (layout["right_comment_box"]) layout["right_comment_box"]->setCoordinates(urx_-boxWidth_, urx_+boxWidth_, lly_, ury_);
//    if (layout["title"]) layout["title"]->setCoordinates(0, 100, 0, 100);
//    if (layout["legend"]) layout["legend"]->setCoordinates(0, 100, 0, 100);


}

SatelliteProjection::~SatelliteProjection() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void SatelliteProjection::print(ostream& out)  const
{
	out << "SatelliteProjection[";
	out << "]";
}

void SatelliteProjection::operator()(const Polyline& from, BaseGraphicsObject::Container& out) const
{
	Polyline* to = new Polyline();
	to->copy(from);
    
    for (Polyline::const_iterator point = from.begin(); point != from.end(); ++point) {
        PaperPoint pp = (*this)(*point);
        if (pp.x() != TeMAXFLOAT && pp.y() != TeMAXFLOAT )   
          to->push_back(pp);
        else { 
        	if ( !to->empty()) {
        		out.push_back(to);
        		to = new Polyline();
				to->copy(from);
        	}
        }
        
    }
    if ( !to->empty()) 
    	out.push_back(to);
}

void SatelliteProjection::operator()(const Text<UserPoint>& from, Text& to) const
{
    for (Text<UserPoint>::const_iterator point = from.begin(); point != from.end(); ++point) {
      PaperPoint pp = (*this)(*point);
        if (pp.x() != TeMAXFLOAT && pp.y() != TeMAXFLOAT)   
          to.push_back((*this)(pp));
    }
}

void SatelliteProjection::operator()(const vector<UserPoint>& from, vector<PaperPoint>& to) const
{
    for (vector<UserPoint>::const_iterator point = from.begin(); point != from.end(); ++point) {
          PaperPoint pp = (*this)(*point);
          if (pp.x() != TeMAXFLOAT && pp.y() != TeMAXFLOAT)   
             to.push_back((*this)(pp));
    }
}

void SatelliteProjection::operator()(const vector<PaperPoint>&, vector<PaperPoint>&) const
{
}
    


Polyline* SatelliteProjection::reproject(const Polyline& from) const
{
	Polyline* to = new Polyline();
	
	//(*this)(from, *to);
	return to;
}
    
bool SatelliteProjection::in(double lon, double lat) const
{
	if ( lon < minlon_ ) return false;
	if ( lat < minlat_ ) return false;
	if ( lon > maxlon_ ) return true;
	if ( lat > maxlat_ ) return true;
	return true;
}


PaperPoint SatelliteProjection::operator()(const UserPoint& from) const
{
   TeCoord2D geo = TeCoord2D(RAD(from.x_), RAD(from.y_));
   TeCoord2D xy = const_cast<SatelliteProjection*>(this)->LL2PC(geo);
   //MagLog::dev()<< "SatelliteProjection::operator() " << from.x_ << " " << from.y_ << " " << xy.x() << " " << xy.y() << endl;
   return PaperPoint(xy.x(), xy.y());
}

PaperPoint SatelliteProjection::operator()(const PaperPoint& point) const
{
    return point;
}

UserPoint SatelliteProjection::revert(const PaperPoint& from) const
{
    TeCoord2D xy = TeCoord2D(from.x(), from.y());
    TeCoord2D geo = const_cast<SatelliteProjection*>(this)->PC2LL(xy);

    return UserPoint(geo.x()*TeCRD, geo.y()*TeCRD);
}

    
void SatelliteProjection::gridLongitudes(const GridPlotting& grid, Task& task) const
{
#if 1
	const vector<double>& longitudes = grid.longitudes();
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon) {
		Polyline* poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(*lon,lat));
		}  
  
	}


	Polyline* poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(-73,lat));
		}

	poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(73,lat));
		}
#endif
#if 0
	poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(-50,lat));
		}   
	poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(50,lat));
		}

	poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(-30,lat));
		}   
	poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(30,lat));
		}

	poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(-10,lat));
		}   

		poly = new Polyline(grid.layer(), "drawing_area");
		grid.add(poly, task);
		poly->setColour(Colour("green"));
		
		for (double lat = -80; lat <= 80; lat += 1) { 
			poly->push_back(UserPoint(10,lat));
		}
#endif
}

void SatelliteProjection::gridLatitudes(const GridPlotting& grid, Task& task) const
{
#if 1	
	 const vector<double>& latitudes = grid.latitudes();
	
	
     for (vector<double>::const_iterator lat = latitudes.begin(); lat != latitudes.end(); ++lat) {
      
	    Polyline* poly = new Polyline(grid.layer(), "drawing_area");        
            for (double lon = minlon_; lon <= maxlon_; lon += 1) { 
                poly->push_back(UserPoint(lon,*lat));
            }	
        grid.add(poly, task);        
	}
#endif

#if 0
//Test
Polyline* poly = new Polyline(grid.layer(), "drawing_area");        
		poly->setColour(Colour("yellow"));
for (double lon = -40; lon <= 40; lon += 1) { 
                poly->push_back(UserPoint(lon,0));
  }	
grid.add(poly, task);
#endif
}

    
void SatelliteProjection::topLabels(const LabelPlotting&, Task&) const
{
}

void SatelliteProjection::bottomLabels(const LabelPlotting&, Task&) const
{
}

void SatelliteProjection::leftLabels(const LabelPlotting&, Task&) const
{
}

void SatelliteProjection::rightLabels(const LabelPlotting&, Task&) const
{
}

