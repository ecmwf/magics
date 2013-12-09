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
		
			setNewPCBox(minx, miny, maxx, maxy);
}

#define PRINT(w, a) MagLog::dev() << w << "---> " << #a << ": " << (a) << std::endl


void Transformation::tile(double& width, double& height)
{
	init();
	// here we assume we are in the wrepMode, this should be checked!


	// with and height will only
	double awidth = getAbsoluteMaxPCX() - getAbsoluteMinPCX();
	double aheight = getAbsoluteMaxPCY() - getAbsoluteMinPCY();

	double minx =  getAbsoluteMinPCX();
	double maxx =  getAbsoluteMaxPCX();
	double miny =  getAbsoluteMinPCY();
	double maxy =  getAbsoluteMaxPCY();

	awidth = maxx -minx;
	aheight = maxy -miny;




			int tiles;
			double nw = (width/height) * aheight;











	double pxheight = height*40.;
	double pxwidth = width*40.;

	// Try to see if we have already a number of tile.
	if (same((pxheight/tile_ - int(pxheight)/tile_), 0)) {
		pxheight = int(pxheight);

	}
	if (same((pxwidth/tile_ - int(pxwidth)/tile_), 0)) {
		pxwidth = int(pxwidth);

	}

	askedWidth_ = pxwidth;
	askedHeight_ = pxheight;

	double pcwidth = maxx-minx;
	double pcheight = maxy-miny;
	PRINT("pcwidth", pcwidth);
	PRINT("pcheight", pcheight);
	vector<double> levels;
	double u = unit_;
	for ( float i = 6; i > 0; i--) {

		levels.push_back((u)/tile_);


		u = u/2.;
	}
	std::sort(levels.begin(), levels.end());


	for ( vector<double>::iterator l = levels.begin(); l != levels.end(); ++l)
		MagLog::debug()  << *l << ", ";

	double ratio = ( nw > awidth) ? pcheight/pxheight : pcwidth/pxwidth;
					  	// we need to extend in the x direction






	vector<double>::iterator l = levels.begin();
	while ( l != levels.end() ) {

		if ( ratio - *l   <  unitEpsilon_  ) {
			break;
		}
		++l;
	}




	double level = ( l == levels.end() ) ? levels.back() : *l;


	int ytiles = ceil((pcheight/level)/tile_);
	int xtiles = ceil((pcwidth/level)/tile_);

	zoomLevel_ = ytiles;


	pxwidth  = tile_*xtiles;
	pxheight = tile_*ytiles;


	// compute new pxminx, pxminy, pxmaxx, pxmaxy

	int pxminx, pxminy, pxmaxx, pxmaxy;

	pxminx = (minx-originX_)/level;
	pxminy = (miny-originY_)/level;;
	pxmaxx = (maxx-originX_)/level;
	pxmaxy = (maxy-originY_)/level;

	xTile_ = pxminx;
	yTile_ = pxmaxy;



	// compute  new minx

	pxminx = floor((float)pxminx/tile_)*tile_;
	pxminy = floor((float)pxminy/tile_)*tile_;

	pxmaxx =  ceil((float)pxmaxx/tile_)*tile_;
	pxmaxy =  ceil((float)pxmaxy/tile_)*tile_;




	// now we can compute the new minx,,,
	minx = (pxminx * level) + originX_;
	miny =  (pxminy * level) + originY_;
	maxx = (pxmaxx * level) + originX_;
	maxy = (pxmaxy * level) + originY_;
	// now we jsut have to count the number of tiles to find the width and the hieght

	width = (pxmaxx-pxminx);
	height = (pxmaxy-pxminy);

	if ( askedWidth_ > width ) {
		maxx += level*tile_;
		width += tile_;

	}
	if ( askedHeight_ > height ) {

			miny -= level*tile_;
			height +=tile_;

	}
	width /=40.;
	height /=40.;




	xTile_ = xTile_ - pxminx;
	yTile_ = pxmaxy - yTile_;




	setNewPCBox(minx, miny, maxx, maxy);
	init();
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

	PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMaxPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMaxPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMinPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
	askedxmin_ =  std::min(getMinPCX(), getMaxPCX());
	askedxmax_ =  std::max(getMinPCX(), getMaxPCX());
	askedymin_ =  std::min(getMinPCY(), getMaxPCY());
	askedymax_ =  std::max(getMinPCY(), getMaxPCY());


}

void Transformation::cleaninit()
{
	PCEnveloppe_->clear();

	PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMaxPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMaxPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMaxPCX(), getMinPCY()));
	PCEnveloppe_->push_back(PaperPoint(getMinPCX(), getMinPCY()));
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
	java << "\"pcxmin\" : \"" << getMinPCX() <<  "\",";
	java << "\"pcymin\" : \"" << getMinPCY() <<  "\",";

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
	if ( from.closed() ) {
		deque<PaperPoint> line;

		for (unsigned i = 0; i < from.size(); i++) {
			line.push_back(from.get(i));

		}

		boost::geometry::correct(line);
		vector<deque<PaperPoint> > result;
		try {
			boost::geometry::intersection(box, line, result);

			// Now we feed the graphic container!

			for (vector<deque<PaperPoint> >::iterator l = result.begin(); l != result.end(); l++)
			{
				Polyline* poly = from.getNew();

				for (deque<PaperPoint>::iterator point = l->begin(); point != l->end(); ++point)
					poly->push_back(*point);

				if ( !poly->empty() )
					out.push_back(poly);
			}
		}
		catch (...) {
			MagLog::warning() << "error clipping->line ignored" << endl;
		}
	}
	else {
		vector<PaperPoint> line;

		for (unsigned i = 0; i < from.size(); i++) {
			line.push_back(from.get(i));
		}
		boost::geometry::correct(line);
		vector<vector<PaperPoint> > result;
		try {
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


void Transformation::thin(int dimx, int dimy, vector<UserPoint>& in) const
{
	Timer timer("thiining", "");
    if ( dimx < 2 || dimy < 2)  {
        MagLog::info() << " No flags to plot." << endl;
        return;
        }

	double minx = getMinPCX();
	double maxx = getMaxPCX();
	double miny = getMinPCY();
	double maxy = getMaxPCY();

	double xref_ = 0;
	double yref_ = 0;

	vector<PointIter> helper(dimx*dimy, in.end());

	double stepx = (maxx-minx)/(dimx-1);
	double stepy = (maxy-miny)/(dimy-1);




	//minx = floor((minx)/stepx)*stepx;
	//maxx = ceil((maxx)/stepx)*stepx;
	//miny = floor((miny)/stepy)*stepy;
	//maxy = ceil((maxy)/stepy)*stepy;

	for ( PointIter pt = in.begin(); pt != in.end(); ++pt ) {
		double x = pt->x_;
		double y = pt->y_;

		fast_reproject(x, y);

		double xx = floor((x-minx)/stepx);
		double yy = floor((y-miny)/stepy);
		double x0 = minx + (xx*stepx) +(stepx/2);
		double y0 = miny + (yy*stepy)+(stepy/2);

        vector<pair<int, int> > check;
        check.push_back(make_pair(yy, xx));
        check.push_back(make_pair(yy, xx-1));
        check.push_back(make_pair(yy, xx+1));
        check.push_back(make_pair(yy+1, xx));
        check.push_back(make_pair(yy+1, xx-1));
        check.push_back(make_pair(yy+1, xx+1));
        check.push_back(make_pair(yy-1, xx));
        check.push_back(make_pair(yy-1, xx-1));
        check.push_back(make_pair(yy-1, xx+1));

        for ( vector<pair<int, int> >::iterator ind = check.begin(); ind != check.end(); ++ind) {
            int i = ind->first*dimx + ind->second;
            if ( i < 0 || i >= dimx*dimy) 
                continue;
             double x0 = minx + ( ind->second*stepx) +(stepx/2);
             double y0 = miny + ( ind->first*stepy)+(stepy/2);


		PointIter& last = helper[i];

		if ( last == in.end())
			helper[i ]= pt;
		else {

			double dist = sqrt(((x-x0) * (x-x0)) +  ((y - y0) * (y -y0)));
			double lastx = last->x_;
			double lasty = last->y_;
			fast_reproject(lastx, lasty);
			double min = sqrt(((lastx-x0) * (lastx-x0)) +  ((lasty - y0) * (lasty -y0)));
			if (dist < min)
				helper[i ] = pt;
		}
         }


	}

	for ( int j = 0; j < dimx*dimy; j++) {
			helper[j]->flagMissing();
	}

}
