/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file WindPlotting.cc
    \brief Implementation of the Template class Wind.

    Magics Team - ECMWF 2005

    Started: Thu 17-Mar-2005

    Changes:

*/

#include "Streamlines.h"
#include "LegendVisitor.h"
#include "CalcStreamlines.h"
#include "Timer.h"
#include "Polyline.h"
#include "GeoRectangularProjection.h"
using namespace magics;

bool Streamlines::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{
    Timer timer("Streamlines", "Streamlines");


    const Transformation& transformation = parent.transformation();

    GeoRectangularProjection geo;

    MatrixHandler& dir = data.direction();

    //Make sure that the data are prepared correcly for the Streamlines..

    if (dir.right() -  dir.left() < 350) {
		geo.setMinX(dir.left()+1);
		geo.setMaxX(dir.right()-1);
		geo.setMinY(dir.bottom()+1);
		geo.setMaxY(dir.top()-1);
	}

    MatrixHandler& handler = *geo.prepareData(data.direction());

    // We filter !
    cout << "left->" << handler.left() << endl;
    cout << "right->" << handler.right() << endl;
    cout << "top->" << handler.top() << endl;
    cout << "bottom->" << handler.bottom() << endl;

    float* direction = new float[handler.rows()*handler.columns()];
    int i = 0;
    for (int row = 0; row < handler.rows(); row++ )
        for (int column = 0; column < handler.columns(); column++ ) {
// ************ MF RV ***************
// CalcStreamlines.cc (line 281) -> only check NAN as missingValue
//    and shiftPeriod(missingValue) (lines 31,44) -> catastrophic!
//            direction[i] = (handler)(row, column);
            double d = (handler)(row, column);
            direction[i] = (d == handler.missing()) ? NAN : d;
// ************ MF RV ***************
            i++;
        }


    GSStruct *gs = new GSStruct();
    gs->nx = handler.columns();
    gs->ny = handler.rows();


    gs->startx = handler.column(0,0);
    gs->starty = handler.row(0,0);

    // Distance between the gridpoints
    gs->dx = handler.XResolution();
    gs->dy = -handler.YResolution();
    gs->period_x = 0.;
    transformation.geoProjection(gs->gs_geo);

    OneLineClass ** result = 0;
    int size;


    CalcStreamlines(min_density_, direction, gs, result, size);

    for(int l = 0; l < size; l++)
    {

        Polyline poly;
        poly.setColour(*colour_);
        poly.setThickness(thickness_);
        poly.setLineStyle(style_);


        ArrowProperties* arrow = new ArrowProperties();
        arrow->setHeadIndex(head_);
        arrow->setHeadRatio(ratio_);
        poly.setArrow(arrow);
        for(int i = 0; i < result[l]->Len; i++)
        {

                poly.push_back(transformation(UserPoint(result[l]->X[i], result[l]->Y[i])));

        }
        transformation(poly, parent);

    }
// ************ MF RV ***************
// Leak ;-)
    delete [] direction;
// ************ MF RV ***************
    return true;
}

void Streamlines::visit(LegendVisitor& legend)
{

}

void Streamlines::print(ostream& out) const
{
    out << "Streamlines[";
    StreamlinesAttributes::print(out);
    out << "]";
}


