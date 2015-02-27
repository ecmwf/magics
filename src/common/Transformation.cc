
/******************************** LICENSE ********************************

  Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF) 

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

 ******************************** LICENSE ********************************/

// File Transformation.cc
// Magics Team - ECMWF 2004

#include "BasicGraphicsObject.h"
#include "Transformation.h"
#include "Layout.h"
#include <iomanip>
#include "MatrixHandler.h"
#include "PointsHandler.h"
#include "Timer.h"
#include "MetaData.h"
#define BOOST_GEOMETRY_OVERLAY_NO_THROW
#include "Polyline.h"



using namespace magics;


Transformation::Transformation() : 
	coordinateType_(GeoType),
	dataMinX_(std::numeric_limits<double>::max()),
	dataMaxX_(-std::numeric_limits<double>::max()),
	dataMinY_(std::numeric_limits<double>::max()),
	dataMaxY_(-std::numeric_limits<double>::min()),
	topAxis_(true), xTile_(0), yTile_(0)
{
	userEnveloppe_ = new Polyline();
	PCEnveloppe_ = new Polyline();
}

Transformation::~Transformation() 
{
	delete userEnveloppe_;
	delete PCEnveloppe_;
}

void Transformation::print(ostream& out)  const
{
	out << "Transformation";
}

void Transformation::forceNewArea(double xpcmin, double ypmin, double xpcmax, double ypcmax, double& width, double& height)
{
	
}
#define PRINT(w, a) MagLog::dev() << w << "---> " << #a << ": " << (a) << std::endl
void Transformation::fill(double& width, double& height)
{
    init();

		// with and height will only
		double w = getAbsoluteMaxPCX() - getAbsoluteMinPCX();
		double h = getAbsoluteMaxPCY() - getAbsoluteMinPCY();

		double minx =  getAbsoluteMinPCX();
		double maxx =  getAbsoluteMaxPCX();

		double miny =  getAbsoluteMinPCY();
		double maxy =  getAbsoluteMaxPCY();

		

		
		
			  	double nw = (width/height) *h;
			  	if ( nw > w) {
			  	// we need to extend in the x direction
			  		double more = (nw-w)/2;
			  		maxx = maxx + more;
			  		minx = minx - more;
			  	}
				else {
					double nh = (height/width)*w;
					double more = (nh-h)/2;
					maxy = maxy + more;
					miny = miny - more;
				}

			  	askedWidth_ = width*40;
			  		askedHeight_ = height*40;

			  		 PRINT ("fill", minx);
			  		    PRINT ("fill", miny);
			  		    PRINT ("fill", maxx);
			  		    PRINT ("fill", maxy);
			setNewPCBox(minx, miny, maxx, maxy);
}



void Transformation::tile(double& width, double& height)
{
	fill(width, height);

	// here we assume we are in the wrepMode, this should be checked!
	init();

	double minx =  getAbsoluteMinPCX();
	double maxx =  getAbsoluteMaxPCX();
	double miny =  getAbsoluteMinPCY();
	double maxy =  getAbsoluteMaxPCY();
	  PRINT ("B", minx);
	    PRINT ("Btile", miny);
	    PRINT ("Btile", maxx);
	    PRINT ("Btile", maxy);
    double pcwidth = maxx - minx;
    double pcheight = maxy - miny;

	double pxheight = height*40.;
	double pxwidth = width*40.;

	tile_ = 512;
	double tile = 512;

	int xtile = ceil(pxwidth/tile);
	int ytile = ceil(pxheight/tile);


	askedWidth_ = pxwidth;
	askedHeight_ = pxheight;

	width = xtile * tile;
	height = ytile * tile;


    double xoffset = ((width - pxwidth)/pxwidth)*pcwidth; 
    double yoffset = ((height - pxheight)/pxheight)*pcheight; 
    xoffset /=2;
    yoffset /=2;
    
    minx -= xoffset;
    miny -= yoffset;
    maxx += xoffset;
    maxy += yoffset;
    xTile_ = (width - pxwidth)/2;
    yTile_ = (height - pxheight)/2;
    // Now we fill

    PRINT ("tile", minx);
    PRINT ("tile", miny);
    PRINT ("tile", maxx);
    PRINT ("tile", maxy);
	width = width/40.;
	height = height/40.;
	setNewPCBox(minx, miny, maxx, maxy);


}

void Transformation::aspectRatio(double& width, double& height)
{
	askedWidth_ = width*40;
	askedHeight_ = height*40;
	init();
	double w = getAbsoluteMaxPCX() - getAbsoluteMinPCX();
	double h = getAbsoluteMaxPCY() - getAbsoluteMinPCY();
	if ( w/h >= width/height) {
		double nh = (h/w) * width;
		if ( nh <= height) {
			height = nh;
		}
		else {
			width = (w/h) * height;			
		}
	}
	else 
		width = (w/h) * height;


}




void Transformation::thin(MatrixHandler& matrix, double x, double y, vector<UserPoint>& out) const
{
	int xfactor = (int) ceil((float) x);
	int yfactor = (int) ceil((float) y);
	
	if(xfactor < 1)
	{
	  	xfactor=1;
		MagLog::warning() << "Ivalid x-thinning factor: " << x << "! Reverted back to 1" << endl;
	}
	if(yfactor < 1)
	{
	  	yfactor=1;
		MagLog::warning() << "Ivalid y-thinning factor: " << y << "! Reverted back to 1" << endl;
	}	
	
	ThinningMatrixHandler thin_matrix(matrix, xfactor , yfactor);
	//MatrixHandler* box_matrix = prepareData(thin_matrix);
	MatrixHandler* box_matrix = &thin_matrix;

		int columns =box_matrix->columns();
		int rows = box_matrix->rows();


			for ( int lat = 0; lat < rows; lat++)
				for ( int lon = 0; lon < columns; lon++)
				//if ( in(box_matrix->column(lat, lon), box_matrix.row(lat, lon)) )
					out.push_back(UserPoint(box_matrix->column(lat, lon), box_matrix->row(lat, lon), (*box_matrix)(lat, lon)));

}




ViewFilter::ViewFilter(double xmin, double xmax, double ymin, double ymax, double xres, double yres) : 
	xmin_(xmin), xmax_(xmax), ymin_(ymin), ymax_(ymax), 
	xres_(xres), yres_(yres) 
{
	xdim_ = (int) (xmax_-xmin_)/xres_;
	ydim_ = (int) (ymax_-ymin_)/yres_;

	for (int y = 0; y < ydim_; y++)
		for ( int x = 0; x < xdim_; x++)
			done.push_back(false);

}


bool ViewFilter::in(const PaperPoint& xy){
	if ( xy.x() < xmin_ ) return false;
	if ( xy.x() > xmax_ ) return false;
	if ( xy.y() < ymin_ ) return false;
	if ( xy.y() < ymin_ ) return false;
	return true;
}


void Transformation::init()
{
	PCEnveloppe_->clear();

	getPCBoundingBox();
	askedxmin_ =  std::min(getMinPCX(), getMaxPCX());
	askedxmax_ =  std::max(getMinPCX(), getMaxPCX());
	askedymin_ =  std::min(getMinPCY(), getMaxPCY());
	askedymax_ =  std::max(getMinPCY(), getMaxPCY());
}

void Transformation::cleaninit()
{

	PCEnveloppe_->clear();
	getPCBoundingBox();
	askedxmin_ =  std::min(getMinPCX(), getMaxPCX());
	askedxmax_ =  std::max(getMinPCX(), getMaxPCX());
	askedymin_ =  std::min(getMinPCY(), getMaxPCY());
	askedymax_ =  std::max(getMinPCY(), getMaxPCY());


}

void Transformation::setDataMinMaxX(double minx, double maxx, const string& ref) const
{ 
	// WE will have to take into acount the date!
	dataMinX_ = std::min(minx, dataMinX_); 
	dataMaxX_ = std::max(maxx, dataMaxX_);
	dataReferenceX_ = ref;
}



void Transformation::setDataMinMaxY(double miny, double maxy, const string& ref) const
{ 
	dataMinY_ = std::min(miny, dataMinY_);
	dataMaxY_ = std::max(maxy, dataMaxY_); 
	dataReferenceY_ = ref; 
}


void Transformation::visit(MetaDataVisitor& visitor, 
	double left, double top, double width, double height,  double imgwidth, double imgheight)
{
	ostringstream java;
	double w = getMaxPCX() - getMinPCX();
	double h = getMaxPCY() - getMinPCY();
	java << "{";
	java << "\"name\" : \"cylindrical\",";		

	java << "\"top\" : \"" << top <<  "\",";		
	java << "\"left\" : \"" << left <<  "\",";		

	java << "\"img_width\" : \"" << imgwidth <<  "\",";	
	java << "\"img_height\" : \"" << imgheight <<  "\",";	
	java << "\"width\" : \"" << width <<  "\",";	
	java << "\"height\" : \"" << height <<  "\",";	

	java << "\"pcxmin\" : \"" << getMinPCX() <<  "\",";		
	java << "\"pcymin\" : \"" << getMinPCY() <<  "\",";		
	java << "\"pcxmax\" : \"" << getMaxPCX() <<  "\",";
	java << "\"pcymax\" : \"" << getMaxPCY() <<  "\",";

	java << "\"pcwidth\" : \"" << w <<  "\",";	
	java << "\"pcheight\" : \"" << h <<  "\",";
	java << "\"inwidth\" : \"" << askedWidth_ <<  "\",";
	java << "\"inheight\" : \"" << askedHeight_ <<  "\",";
	java << "\"xorig\" : \"" << xTile_ <<  "\",";
	java << "\"yorig\" : \"" << yTile_ <<  "\",";
	java << "\"zoom_level\" : \"" << zoomLevel_ <<  "\"";
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


#include <boost/geometry/geometries/box.hpp>



void Transformation::operator()(const Polyline& from, BasicGraphicsObjectContainer& out) const
{	
	if (from.empty())
		return;
	PaperPoint ll(getMinPCX(), getMinPCY());
	PaperPoint ur(getMaxPCX(), getMaxPCY());
	boost::geometry::model::box<PaperPoint> box(ll, ur);
	boost::geometry::correct(box);




	vector<deque<PaperPoint> > holes;
	vector<Polyline*> forholes;

	if ( from.closed() ) {
	try {
		deque<PaperPoint> line;
			//line.reserve( from.size());
		for (unsigned i = 0; i < from.size(); i++) {
				line.push_back(from.get(i));

		}
		vector<deque<PaperPoint> > result;
		boost::geometry::correct(line);
		boost::geometry::intersection(box, line, result);

		// Now we feed the graphic container!

		for (vector<deque<PaperPoint> >::iterator l = result.begin(); l != result.end(); l++)
		{
			Polyline* poly = from.getNew();

			for (deque<PaperPoint>::iterator point = l->begin(); point != l->end(); ++point)
				poly->push_back(*point);

			if ( !poly->empty() ) {
				out.push_back(poly);
				forholes.push_back(poly);
			}
			// Now we add the holes
		}

		for (Polyline::Holes::const_iterator h = from.beginHoles(); h !=  from.endHoles(); ++h) {

			deque<PaperPoint> hole;
			for (deque<PaperPoint>::const_iterator pt = h->begin(); pt != h->end(); ++pt) {
				hole.push_back(*pt);
			}

			vector<deque<PaperPoint> > clip;
			boost::geometry::correct(hole);
			boost::geometry::intersection(box, hole, clip);
			for (vector<deque<PaperPoint> >::iterator l = clip.begin(); l != clip.end(); l++) {
				if ( l->size() ) {
					holes.push_back(deque<PaperPoint>());
					for (deque<PaperPoint>::iterator point = l->begin(); point != l->end(); ++point)
						holes.back().push_back(*point);
				}

			}
		}

		for ( vector<Polyline*>::const_iterator poly = forholes.begin(); poly != forholes.end(); ++poly)
			for (vector<deque<PaperPoint> >::iterator x = holes.begin(); x != holes.end(); x++) {

				if ( (*poly)->within(x->front()) ) {
					(*poly)->newHole();
					for (deque<PaperPoint>::iterator point = x->begin(); point != x->end(); ++point)
						(*poly)->push_back_hole(*point);
				}
			}
	}
	catch (...) {
		MagLog::warning() << "error clipping->line ignored" << endl;
	}
	return;
	}
	try {
			vector<PaperPoint> line;
				//line.reserve( from.size());
			for (unsigned i = 0; i < from.size(); i++) {
					line.push_back(from.get(i));

			}
			vector<vector<PaperPoint> > result;
			boost::geometry::correct(line);
			boost::geometry::intersection(box, line, result);

			// Now we feed the graphic container!

			for (vector<vector<PaperPoint> >::iterator l = result.begin(); l != result.end(); l++)
			{
				Polyline* poly = from.getNew();

				for (vector<PaperPoint>::iterator point = l->begin(); point != l->end(); ++point)
					poly->push_back(*point);
				if ( !poly->empty() )
					out.push_back(poly);

			}
	}

	catch (...) {
		MagLog::warning() << "error clipping->line ignored" << endl;
	}





}



void Transformation::boundingBox(double& minx, double& miny, double&maxx, double& maxy) const
{
	// Return exactly the box ... Perhaps could return a bit more to avoid side effect.
	minx= getMinX();
	miny= getMinY();
	maxx= getMaxX();
	maxy= getMaxY();


	PCEnveloppe_->push_back(PaperPoint(minx, miny));
	PCEnveloppe_->push_back(PaperPoint(minx, maxy));
	PCEnveloppe_->push_back(PaperPoint(maxx, maxy));
	PCEnveloppe_->push_back(PaperPoint(minx, maxy));
	PCEnveloppe_->push_back(PaperPoint(minx, miny));

	userEnveloppe_->push_back(PaperPoint(minx, miny));
	userEnveloppe_->push_back(PaperPoint(minx, maxy));
	userEnveloppe_->push_back(PaperPoint(maxx, maxy));
	userEnveloppe_->push_back(PaperPoint(minx, maxy));
	userEnveloppe_->push_back(PaperPoint(minx, miny));




}



bool Transformation::in(const UserPoint& point) const
{

	PaperPoint pp = (*this)(point);
	return in(pp);
}



bool Transformation::in(const PaperPoint& point) const
{
	if ( PCEnveloppe_->empty()) {
        
		PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
		PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMaxPCY()));
		PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMaxPCY()));
		PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMinPCY()));
		PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
	}

	return boost::geometry::covered_by(point, PCEnveloppe_->polygon_);
}


bool Transformation::in(double x, double y) const
{

	fast_reproject(x, y);
	if ( x < askedxmin_ ) return false;
	if ( x > askedxmax_ ) return false;
	if ( y < askedymin_ ) return false;
	if ( y > askedymax_ ) return false;
	return true;

}

void Transformation::thin(PointsHandler& points, vector<PaperPoint>& thin,  vector<PaperPoint>& all) const
{	

	BoxPointsHandler box(points, *this,  true);
	box.setToFirst();
	while (box.more()) {               		
		PaperPoint xy = (*this)(box.current());
		if ( view_.in(xy) ) {
			thin.push_back(xy);
		}
		all.push_back(xy);

		box.advance();		
	}  

}


void Transformation::thin(MatrixHandler& points, vector<PaperPoint>& thin,  vector<PaperPoint>& all) const
{	

	BoxMatrixHandler box(points, *this);
	int row = std::max(int(view_.yres_/abs(box.YResolution())), 1);
	int column = std::max(int(view_.xres_/abs(box.XResolution())), 1);
	ThinningMatrixHandler sample(box, row, column);

	box.setToFirst();
	while (box.more()) {               		
		PaperPoint xy = (*this)(box.current());	                   
		if ( view_.in(xy) ) 
			all.push_back(xy);	           
		box.advance();		
	}  
	sample.setToFirst();
	while (sample.more()) {               		
		PaperPoint xy = (*this)(sample.current());	           
		if ( view_.in(xy) ) 
			thin.push_back(xy);	           

		sample.advance();		
	}  







}


double Transformation::unitToCm(double width, double height) const
{
	
	return height/(getAbsoluteMaxPCY() - getAbsoluteMinPCY());
}

void Transformation::operator()(const UserPoint& geo, vector<PaperPoint>& out) const
{
	PaperPoint pp = (*this)(geo);
		if ( in(pp) ) 
			out.push_back(pp);

}
void Transformation::operator()(const UserPoint& geo, Polyline& out) const
{
	PaperPoint pp = (*this)(geo);
		if ( in(pp) )
			out.push_back(pp);

}


void Transformation::reprojectComponents(double& x, double& y, pair<double, double>&) const
{
	fast_reproject(x, y);
}

void Transformation::reprojectSpeedDirection(const PaperPoint& point, pair<double, double>&) const
{
	
}

void Transformation::revert(const vector< std::pair<double, double> > & in, vector< std::pair<double, double> > & out) const
{
	out.reserve(in.size());
	for (vector< std::pair<double, double> >::const_iterator p = in.begin(); p != in.end(); ++p)
		out.push_back(make_pair(this->rx(p->first), this->ry(p->second)));

}

string Transformation::writeLongitude(const UserPoint& point) const
{
		return point.asLongitude();
}
string Transformation::writeLatitude(const UserPoint& point) const
{
		return point.asLatitude();
}

void Transformation::wraparound(const UserPoint& point, stack<UserPoint>& duplicates) const
{
	if ( in(point) ) {
		duplicates.push(point);
	}
}

MatrixHandler* Transformation::prepareData(const AbstractMatrix& matrix) const {
	return new BoxMatrixHandler(matrix, *this);
}

typedef vector<UserPoint>::iterator PointIter;
class SortTool
{
public:
	SortTool(const Transformation& projection, double x, double y) : projection_(projection), x_(x), y_(y) {}
	double distance(const PointIter& pt)
	{
		double x = pt->x_;
		double y = pt->y_;
		projection_.fast_reproject(x, y);
		return (x-x_) * (x-x_) +  (y - y_) * (y -y_);
	}
	bool operator()(const PointIter& first, const PointIter& second) {
			return distance(first) < distance(second);
		}
	double x_;
	double y_;
	const Transformation& projection_;
};

void useRef(double ref, double inc, double& min, double& max)
{


}







void Transformation::thin(double step, PaperPoint& origin, vector<pair<double, double> >& points) const
{
	Timer timer("thinning", "");

	vector< std::pair<double, double> > xypoints;
	double minx = getMinPCX();
	double maxx = getMaxPCX();
	double miny = getMinPCY();
	double maxy = getMaxPCY();


	std::set<double> xl, yl;



	for ( double x = origin.x(), i = 1; x <= maxx; x = origin.x() + i*step, i++) {
		if ( x > minx )
			xl.insert(x);
	}
	for ( double x = origin.x(), i = 1; x >= minx; x = origin.x() - i*step, i++)
		if ( x < maxx )
			xl.insert(x);
	for ( double y = origin.y(), i = 1; y <= maxy; y = origin.y() + i*step, i++)
		if ( y > miny )
			yl.insert(y);
	for ( double y = origin.y(), i = 1; y >= miny; y = origin.y() - i*step, i++)
		if ( y < maxy ) {
			yl.insert(y);

		}
	for (std::set<double>::iterator x = xl.begin(); x != xl.end(); ++x)
		for (std::set<double>::iterator y = yl.begin(); y != yl.end(); ++y){
							xypoints.push_back(make_pair(*x, *y));
					}


	revert(xypoints, points);

}

void Transformation::thin(double step, PaperPoint& origin, Matrix& matrix, double missing) const
{
/*
	Timer timer("thinning", "");

	vector< std::pair<double, double> > xypoints;
	double minx = getMinPCX();
	double maxx = getMaxPCX();
	double miny = getMinPCY();
	double maxy = getMaxPCY();


	std::set<double> xl, yl;



	for ( double x = origin.x(), i = 1; x <= maxx; x = origin.x() + i*step, i++) {
		if ( x > minx )
			xl.insert(x);
	}
	for ( double x = origin.x(), i = 1; x >= minx; x = origin.x() - i*step, i++)
		if ( x < maxx )
			xl.insert(x);
	for ( double y = origin.y(), i = 1; y <= maxy; y = origin.y() + i*step, i++)
		if ( y > miny )
			yl.insert(y);
	for ( double y = origin.y(), i = 1; y >= miny; y = origin.y() - i*step, i++)
		if ( y < maxy ) {
			yl.insert(y);

		}

	matrix.set(x.size(), y.size(), missing);

	for ( std::set<double>::iterator x = xl.begin(), x != xl.end(); ++x )
		matrix.columns().push_back(*x);
	for ( std::set<double>::iterator y = yl.begin(), y != yl.end(); ++y )
			matrix.columns().push_back(*y);

	(*matrix)->setMapsAxis();
*/
}

UserPoint Transformation::reference() const
{
	PaperPoint xy((getMinPCX() +  getMaxPCX())/2., (getMinPCY() +  getMaxPCY())/2.);
	UserPoint ll;
	revert(xy, ll);
	return ll;
}

double Transformation::distance(UserPoint& point1, UserPoint& point2) const
{
	double x1 = point1.x_;
	double y1 = point1.y_;
	double x2 = point2.x_;
	double y2 = point2.y_;
	fast_reproject(x1, y1);
	fast_reproject(x2, y2);

	return sqrt (( x1-x2 ) *  ( x1-x2 ) + ( y1-y2 ) *  ( y1-y2 ));


}
