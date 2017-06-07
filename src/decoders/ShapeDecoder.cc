/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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

/*! \brief Method to read location and names of state capitals
  
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
		char    szTitle[12];
		double  minx, miny, maxx, maxy;
		transformation.smallestBoundingBox(minx, miny, maxx, maxy);

		Polyline& box = transformation.getUserBoundingBox();

		int     nWidth, nDecimals;
		int     nShapeType, nEntities, i, iPart;
		double  adfMinBound[4], adfMaxBound[4];

		string shp = path_ + ".shp";
		string dbf = path_ + ".dbf";
		const SHPHandle hSHP = SHPOpen( shp.c_str(), "rb" ); 
		const DBFHandle hDBF = DBFOpen( dbf.c_str(), "rb" );

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


/*! \brief Decoder to read land and lakes
 \sa CoastPlotting::decode(const Layout& parent )
*/
void ShapeDecoder::decode(vector<Polyline*>& data, const Transformation& transformation)
{
	Timer timer("Read Shape file ", "read shape file" + path_);

		Polyline& geobox = transformation.getUserBoundingBox();
		Polyline& box = transformation.getPCBoundingBox();
		try {
			SHPHandle  hSHP;
			int	nShapeType, nEntities, i, iPart;
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

			const double south = transformation.getMinY();
			const double north = transformation.getMaxY();
			const double west  = transformation.getMinX();
			const double east  = transformation.getMaxX();

			double shift = 0;

			if ( west < -180 )
				shift = 360;
			if ( east > 180 )
				shift = 360;
			if ( ( east - west ) > 360. )
				shift = 0;

			SHPObject *psShape = 0;
			int nb  = 0;
			for( i = 0; i < nEntities; i++ )
			{
				int	j;
				SHPDestroyObject(psShape);
				psShape = SHPReadObject( hSHP, i );

				bool in = true;
				bool left = false;
				bool right = false;

				if ( psShape->dfYMax  <= south ) continue;
				if ( psShape->dfYMin  >= north ) continue;
				if ( psShape->dfXMax + shift <= west) in = false;
				if ( psShape->dfXMin + shift >= east) in = false;
				if ( psShape->dfXMax + shift - 360 > transformation.getMinX() &&  !same(psShape->dfXMax-360, transformation.getMinX())) {
					        left = true;
				}
				if ( psShape->dfXMin + shift +360 < transformation.getMaxX() && !same(psShape->dfXMin+360, transformation.getMaxX() ) ) {
						right = true;
				}

				if ( !in && !right && !left ) continue;

				Polyline* poly = 0;
                Polyline* polyleft = 0;
                Polyline* polyright = 0; 
               
                if ( in) {
                    poly  = new Polyline();
                    data.push_back(poly);
                }
				if ( left ) {
                    polyleft  = new Polyline();
                    data.push_back(polyleft);
                }
                if ( right ) {
                    polyright  = new Polyline();
                    data.push_back(polyright);
                }
                
				left = false;
				right= false;
				int index = 0;
				bool rotate = 0;
				for( j = 0, iPart = 1; j < psShape->nVertices ; j++ )
				{
					if( iPart < psShape->nParts && psShape->panPartStart[iPart] == j )
					{
						iPart++;

						if (poly)	   poly->newHole();
						if (polyleft)  polyleft->newHole();
						if (polyright) polyright->newHole();
					}

					double x = psShape->padfX[j];
					x += shift;
					const double y = psShape->padfY[j];

					if ( iPart==1 ) {

							if ( poly )    {
								if (  same(x, -180.) || same(x, 180.) )  {
									if ( y > 0 ) {
										index = j;		
									}
								}
								poly->push_back(PaperPoint(x, y));
							}
							if ( polyleft )  polyleft->push_back( PaperPoint(x-360, y));
							if ( polyright ) polyright->push_back(PaperPoint(x+360, y));
					}
					else {
							if ( poly )      poly->push_back_hole(PaperPoint(x, y));
							if ( polyleft )  polyleft->push_back_hole( PaperPoint(x-360, y));
							if ( polyright ) polyright->push_back_hole(PaperPoint(x+360, y));
					}
				}
				if ( index ) {
						
					poly->rotate(index);
					// Clean the south pole ...

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


