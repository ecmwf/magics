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

/*! \file ShapeDecoder.cc
    \brief Implementation of the Template class ShapeDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 12-Dec-2005
    
    Changes:
    
*/


#include "ShapeDecoder.h"
#include "CustomisedPoint.h"
#include "shapefil.h"
#include "Polyline.h"

#include <boost/geometry/geometry.hpp>
#include <boost/geometry/algorithms/make.hpp>

// #define BOOST_VERSION 104700

ShapeDecoder::ShapeDecoder() :holes_(false)
{
}

ShapeDecoder::~ShapeDecoder() 
{
	MagLog::debug() << "clean ShapeDecoder->" << size() << endl;
	for ( iterator line = begin(); line != end(); ++line) {
		for ( PointsList::iterator point = (*line)->begin(); point != (*line)->end(); ++point) {
			delete *point;
			*point = 0;
		}
		delete *line;
		*line = 0;
	}

}


/*!
 Class information are given to the output-stream.
*/
void ShapeDecoder::print(ostream& out)  const
{
	out << "ShapeDecoder[";
	out << "]";
}

void ShapeDecoder::decode(const Transformation& transformation)
{
	vector<string> all;
	string no;
	decode(transformation, no, all);
}

/*! \brief Method to read llocation and names of state capitals
  
  \todo When we can handle Unicode we should change "nameascii" back to "name"
  
*/
void ShapeDecoder::customisedPoints(const std::set<string>&, CustomisedPointsList& out)
{
	try {
		SHPHandle hSHP;
		DBFHandle hDBF;
		char szTitle[12];

		int     nWidth, nDecimals;
		int     nShapeType, nEntities, i;
		double  adfMinBound[4], adfMaxBound[4];
		string shp = path_ + ".shp";
		string dbf = path_ + ".dbf";
		hSHP = SHPOpen( shp.c_str(), "rb" ); 
		hDBF = DBFOpen( dbf.c_str(), "rb" );
		if ( !hSHP || !hDBF ) {
	   		    MagLog::error() << "Can not open Shapefile " << path_ << endl;
	   		    return;
	   	}

		SHPGetInfo( hSHP, &nEntities, &nShapeType, adfMinBound, adfMaxBound );

		map<string, int> attributes;
		
		for( i = 0; i < DBFGetFieldCount(hDBF); i++ ) {
		            DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
		            attributes.insert(make_pair(lowerCase(szTitle), i));
		}
		
		map<string, int>::iterator index  = attributes.find("nameascii");
		map<string, int>::iterator indexc = attributes.find("featurecla");

		vector<string> needs;
		needs.push_back("Admin-0 capital");

		for( i = 0; i < nEntities; i++ )
		{
			SHPObject *psShape;
			string capital  =  DBFReadStringAttribute(hDBF, i, indexc->second);
			bool add =false;

			for (vector<string>::const_iterator need = needs.begin(); need != needs.end(); ++need)
			{
				if (*need == capital ) {
					add = true;
					break;
				}
			}

			if ( !add )
				continue;


			psShape = SHPReadObject( hSHP, i );
			string name = ( index != attributes.end() ) ? DBFReadStringAttribute(hDBF, i, index->second) : "?";

			if (psShape->nVertices == 1)
			{
			    	CustomisedPoint* point = new CustomisedPoint();
			    	point->latitude(psShape->padfY[0]);
			    	point->longitude(psShape->padfX[0]);
			    	point->identifier(name);
			    	out.push_back(point);
			}
			SHPDestroyObject( psShape );
		    }
		    SHPClose( hSHP ); 
            DBFClose ( hDBF ); 
	}
	catch (...)
	{
		MagLog::error() << "Can not open Shapefile " << path_ << endl;
	}
	MagLog::dev() << "Shape file--->" << this->size() << endl; 
}

/*

  \sa Boundaries::operator()
*/
void ShapeDecoder::decode(const Transformation& transformation, const string& filter, const vector<string>& values)
{
	if ( !this->empty() ) return;
	try {
		SHPHandle  hSHP;
		DBFHandle  hDBF;
		char    szTitle[12];
		double  minx, miny, maxx, maxy;
		transformation.smallestBoundingBox(minx, miny, maxx, maxy);

		Polyline& box = transformation.getUserBoundingBox();

		int     nWidth, nDecimals;
		int     nShapeType, nEntities, i, iPart;
		double  adfMinBound[4], adfMaxBound[4];

		string shp = path_ + ".shp";
		string dbf = path_ + ".dbf";
		hSHP = SHPOpen( shp.c_str(), "rb" ); 
		hDBF = DBFOpen( dbf.c_str(), "rb" );

		if ( !hSHP || !hDBF ) {
			MagLog::error() << "Can not open Shapefile " << shp << endl;
			return;
		}

		SHPGetInfo( hSHP, &nEntities, &nShapeType, adfMinBound, adfMaxBound );
		map<string, int> attributes;
		
		for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
		{
		            DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
		            attributes.insert(make_pair(lowerCase(szTitle), i));
		}
		map<string, int>::iterator index =  filter.empty() ? attributes.end() : attributes.find(filter);

		if ( index == attributes.end() && !filter.empty() ) {
			MagLog::info() << "ShapeDecoder: can not find attribute " << filter << " -> Data will not be filtered!" << endl;
		}
		SHPObject *psShape = 0;
		for( i = 0; i < nEntities; i++ )
		{
			int       j;
			if ( psShape ) {
								SHPDestroyObject(psShape);
							}

			psShape = SHPReadObject( hSHP, i );
		
			bool add = true;
			if ( index != attributes.end() )
			{
				string s = DBFReadStringAttribute(hDBF, i, index->second);
				add = false;

				for (vector<string>::const_iterator val = values.begin(); val != values.end(); ++val) {
					string ss = s.substr(0, val->length());
					if ( magCompare(*val, ss) ) {
						add = true;
						MagLog::debug() << "Found " << ss << endl;
						break;
					}
				}
			}

			if (  add)  {
				bool in = true;
				bool left = true;
				bool right = true;

				if ( psShape->dfYMax <= miny ) continue;
				if ( psShape->dfYMin >= maxy ) continue;
				if ( psShape->dfXMax <= minx ) in = false;
				if ( psShape->dfXMin >= maxx ) in = false;

				if ( psShape->dfXMax-360 < minx ) left = false;
				if ( psShape->dfXMin+360 > maxx ) right = false;

				if ( !in && !right && !left ) continue;


				PointsList *inlist, *leftlist, *rightlist;
				if (in) {
					push_back(new PointsList());
					inlist = back();
				}
				if (left) {
					push_back(new PointsList());
					leftlist = back();
				}
				if (right) {
					push_back(new PointsList());
					rightlist = back();
				}

//				bool hole = false;
				for( j = 0, iPart = 1; j < psShape->nVertices; j++ )
				{
					if( iPart < psShape->nParts && psShape->panPartStart[iPart] == j )
					{
						if ( holes_ == false ) break;
						iPart++;
						if (in) {
        						push_back(new PointsList());
							inlist = back();
						}
						if (left) {
							push_back(new PointsList());
							leftlist = back();
						}
						if (right) {
							push_back(new PointsList());
							rightlist = back();
						}
					}
					if (in) {
							inlist->push_back(new UserPoint(psShape->padfX[j], psShape->padfY[j], i));

					}
					if (left) {
						leftlist->push_back(new UserPoint(psShape->padfX[j]-360., psShape->padfY[j], i));
					}
					if (right) {
						rightlist->push_back(new UserPoint(psShape->padfX[j]+360., psShape->padfY[j], i));
					}
				}
			}

		}
		SHPDestroyObject(psShape);

		SHPClose( hSHP );
        DBFClose ( hDBF ); 
	}
	catch (...)
	{
		MagLog::error() << "Can not open Shapefile " << path_ << endl;
	}
	MagLog::dev() << "Shape file--->" << this->size() << endl; 
}





void ShapeDecoder::decode(vector<Polyline>& data, const Transformation& transformation)
{
	Timer timer("Read Shape file ", "read shape file" + path_);


		Polyline& geobox = transformation.getUserBoundingBox();
		Polyline& box = transformation.getPCBoundingBox();
		try {
			SHPHandle  hSHP;
			int	nShapeType, nEntities, i, iPart;
			bool hole=false;
			double 	adfMinBound[4], adfMaxBound[4];
			string shp = path_ + ".shp";
			string dbf = path_ + ".dbf";
			hSHP = SHPOpen( shp.c_str(), "rb" );
			if ( !hSHP  ) {
			    	MagLog::error() << "Can not open Shapefile " << path_ << endl;
			    	return;
			}
			data.clear();
			SHPGetInfo( hSHP, &nEntities, &nShapeType, adfMinBound, adfMaxBound );

			double south =  transformation.getMinY();
			double north =  transformation.getMaxY();
			double west =  transformation.getMinX();
			double east =  transformation.getMaxX();

			double shift = 0;

			if ( west < -180 )
				shift = -180;
			if ( east > 180 )
				shift = 180;

			if ( ( east - west ) > 360. )
				shift = 0;
			cout << west << " " << east<< " " << shift << endl;


			SHPObject	*psShape = 0;
			int nb  = 0;
			for( i = 0; i < nEntities; i++ )
			{
				int		j;


				SHPDestroyObject(psShape);


				psShape = SHPReadObject( hSHP, i );

				bool in = true;
				bool left = false;
				bool right = false;




				if ( psShape->dfYMax  <= south ) continue;
				if ( psShape->dfYMin  >= north ) continue;
				if ( psShape->dfXMax + shift  <= west) in = false;
				if ( psShape->dfXMin  + shift >=  east) in = false;
				if ( psShape->dfXMax-360 > transformation.getMinX() &&  !same(psShape->dfXMax-360, transformation.getMinX())) {
					        left = true;
				}
				if ( psShape->dfXMin+360 < transformation.getMaxX() && !same(psShape->dfXMin+360, transformation.getMaxX() ) ) {
						right = true;
				}

				if ( !in && !right && !left ) continue;
				VectorOfPointers<vector<Polyline *> > polys;
				Polyline* poly = 0;
                Polyline* polyleft = 0;
                Polyline* polyright = 0; 
                if ( in) {
                    poly  = new Polyline();
                    polys.push_back(poly);
                }
				if ( left ) {
                    polyleft  = new Polyline();
                    polys.push_back(polyleft);
                }
                if ( right ) {
                    polyright  = new Polyline();
                    polys.push_back(polyright);
                }
                
				left = false;
				right=false;

				for( j = 0, iPart = 1, hole = false; j < psShape->nVertices ; j++ )
				{
					bool patch = false;
					if( iPart < psShape->nParts && psShape->panPartStart[iPart] == j )
					{
						iPart++;
						hole=true;
						// We create a new hole!

						if (poly)
							poly->newHole();
						if (polyleft)
							polyleft->newHole();
						if (polyright)
							polyright->newHole();

					}

					else {
						double x = psShape->padfX[j];
							x += shift;
						double y = psShape->padfY[j];

						if ( iPart==1 ) {
							if ( poly ) {
								poly->push_back(PaperPoint(x, y));
							}

							if ( polyleft ) {

								polyleft->push_back(PaperPoint(x-360, y));
							}
							if ( polyright ) {
								polyright->push_back(PaperPoint(x+360,  y));
							}

						}
						else {
							if ( in ) {
								poly->push_back_hole(PaperPoint(x, y));
							}

							if ( polyleft ) {

								polyleft->push_back_hole(PaperPoint(x-360, y));

							}
							if ( polyright ) {
								polyright->push_back_hole(PaperPoint(x+360, y));
							}
						 }
					}



				}
				

	             /// first we clip
					for (vector<Polyline*>::iterator poly = polys.begin(); poly != polys.end(); ++poly ) {
						(*poly)->sanityCheck();
						vector<Polyline> clipped;
						geobox.intersect(**poly, clipped);

	                // then we reproject!
						for (vector<Polyline>::iterator clip = clipped.begin(); clip != clipped.end(); ++clip ) {
							clip->reproject(transformation);
							clip->sanityCheck();
							box.intersect(*clip, data);
						}

	                }




			}
			SHPDestroyObject(psShape);
			SHPClose( hSHP );
		}
		catch (std::exception e)
		{
			MagLog::error() << "Can not open Shapefile " << path_ << endl;
		}
}


