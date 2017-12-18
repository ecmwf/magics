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

    // Il faudrait tout revoir, pour calculer les isolignes dans le plan projeté
    // et non en latlon ( Cela éviterait les problemes de densité d'isolignes
    // pres des poles et de jointure moche pour les projections 'proj4 (overlapped)'

    // Mais attention a toute modif de simplification:
    // => Plus d'1 journée de mise au point

    const Transformation& transformation = parent.transformation();

    double x1,  y1,  x2,  y2;
    transformation.boundingBox(x1, y1, x2, y2);
    PaperPoint pt1 = transformation(UserPoint((x1+x2)/2, (y1+y2)/2));
    PaperPoint pt2 = transformation(UserPoint((x1+x2)/2 + 360.0, (y1+y2)/2));
    bool overlapping = same(pt1.x(), pt2.x()) && same(pt1.y(), pt2.y());

    GeoRectangularProjection geo;

    MatrixHandler& dir = data.direction();

    //Make sure that the data are prepared correcly for the Streamlines..

    double minpcx = transformation.getAbsoluteMinPCX();
    double maxpcx = transformation.getAbsoluteMaxPCX();

    double minpcy = transformation.getAbsoluteMinPCY();
    double maxpcy = transformation.getAbsoluteMaxPCY();

    double minx = geo.getMinX();
    double maxx = geo.getMaxX();
    const double LONMIN = -3.0 * 360.0;
    const double LONMAX = 3.0 * 360.0;
    if (minx < LONMIN || minx > LONMAX || maxx < LONMIN || maxx > LONMAX) {
        // geo a recupéré les min max directement dans les clefs
        // 'subpage_lower_left_latitude', ., 'subpage_upper_right_longitude'
        // MAIS selon la projection et son mode de définition, ces clefs
        // ne sont pas forcement renseignées, ou alors exprimées en
        // coordonnées projetées au lieu de latlon (Stereopol, ou proj4 si
        //     subpage_map_area_coordinate_system == 'projection')

        minx = NAN;
        UserPoint leftpt;
        transformation.revert(PaperPoint(minpcx, minpcy), leftpt);
        double x = leftpt.x();
        if (x == x && x >= LONMIN && x <= LONMAX) // x == x => !NAN
            minx = x;
        transformation.revert(PaperPoint(minpcx, maxpcy), leftpt);
        x = leftpt.x();
        if (x == x && x >= LONMIN && x <= LONMAX) { // x == x => !NAN
            if( minx != minx || x < minx ) // minx != minx => NAN
                minx = x;
        }
        if(minx != minx)
            minx = -180.0;

        maxx = minx+360.0;
    }

    // Il reste encore un pb de "jointure moche" avec les proj "overlapped"
    // ( on s'arrange pour qu'elle soit la moins visible possible => on
    // la cale sur minx )

    double left = dir.left();
    double right = dir.right();

    if (right -  left < 350) {
        if( !overlapping ) {
            // Cas rencontré: Ex Mercator "Historique" (pas d'overlap)
            // si minx = -180, maxx = -135   et  donnees (left 173 -> right 230)
            // => Aucun tracé ( il faut ajuster les longitudes des données )

            // la meilleure solution mais potentiellement couteuse
            // ( ex: maille 0.01 de -1440 -> 1440 -> points en longitude )
            //     geo.setMinX(minx);
            //     geo.setMaxX(maxx);

            // A priori moins couteux, mais la grille n'est tracée qu'une seule fois si
            // maxx - minxx > 360.0. Mais c'est déja le cas pour les "overlapped"
            while(left > maxx) {
                left -= 360.0;
                right -= 360.0;
            }
            while(right < minx) {
                left += 360.0;
                right += 360.0;
            }
            geo.setMinX(left-1);
            geo.setMaxX(right+1);
        }
        else {
            geo.setMinX(left-1);
            geo.setMaxX(right+1);
        }
    }

    else {
        // grilles globales
        maxx = std::max(minx+360.0, maxx);	// max-min >= 360 nécessaire pour
        geo.setMinX(minx);					// les proj non rectangulaires
        geo.setMaxX(maxx);
    }

    // nécessaire pour proj non rectangulaires (Ex: stereopolaire)
    // sinon il manque une partie du tracé
    // ( Pas terrible, On doit pouvoir faire mieux !)
    const double LATMAX = 89.0;
    // top()/bottom() inversés ?
    geo.setMinY(std::max(-LATMAX, std::min(dir.bottom(), dir.top())-1));
    geo.setMaxY(std::min(LATMAX, std::max(dir.bottom(), dir.top())+1));

    MatrixHandler& handler = *geo.prepareData(data.direction());

    

    int nbcolumns = handler.columns();
    if (overlapping) {
        // Même avec -180.0, 180.0, il y a un overlap (cf Matrix.cc)
        double minlon;
        int column = 0;
        for (; column < handler.columns(); column++ ) {
            if (!column)
                minlon = handler.column(0,0);
            else if (handler.column(0, column) >= (minlon + 360.0))
                break;
        }
        nbcolumns = column;
    }

    float* direction = new float[handler.rows()*nbcolumns];
    int i = 0;
    for (int row = 0; row < handler.rows(); row++ )
        for (int column = 0; column < nbcolumns; column++ ) {
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
    gs->nx = nbcolumns;
    gs->ny = handler.rows();


    gs->startx = handler.column(0,0);
    gs->starty = handler.row(0,0);

    // Distance between the gridpoints
    gs->dx = handler.XResolution();
    gs->dy = -handler.YResolution();
    if (gs->dy < 0)		//reversed lat scan
        gs->dy = -gs->dy;
    gs->period_x = 0.;
    transformation.geoProjection(gs->gs_geo);
    gs->gs_geo = 0;  // RV
    OneLineClass ** result = 0;
    int size;


    CalcStreamlines(min_density_, direction, gs, result, size);

    for(int l = 0; l < size; l++)
    {

        Polyline poly;
        poly.setColour(*colour_);
        poly.setThickness(thickness_);
        poly.setLineStyle(style_);

        if (ratio_ > 0) { // si ratio_ == 0, il reste quand même une "trace"
            ArrowProperties* arrow = new ArrowProperties();
            arrow->setHeadIndex(head_);
            arrow->setHeadRatio(ratio_);
            poly.setArrow(arrow);
        }

        static const PaperPoint NOPT(1.0e50, 1.0e50);
        static const double MARGIN = 0.05;

        double width = maxpcx - minpcx;
        double xmin = minpcx - width * MARGIN;
        double xmax = maxpcx + width * MARGIN;

        double height = maxpcy - minpcy;
        double ymin = minpcy - height * MARGIN;
        double ymax = maxpcy + height * MARGIN;

        // MOUAIS .... C'est moche ( => à améliorer )
        // Il est difficile de calculer une distmax qui marche avec toutes les projections
        // => Stereopolaire (pas vraiment de distance max) car pas de saut de
        // longitude contrairement à une proj rectangulaire (type EPSG:3832) ou
        // encore une projection inclinée type skewmercator
        // Il faudrait balayer (par exemple de ° en °) toutes les longitudes par
        // rapport à un pt de ref et prendre la valeur max avec une marge mais
        // à quelle latitude le faire pour une projection conique type
        // stereopolaire
        double distmax = -1.0, distmax2 = -1.0;

        if (overlapping) {
            distmax = width * 95.0/100.0; // <= .... Bof
            distmax2 = distmax * distmax;
        }

        for (int i = 0; i < result[l]->Len;) {
            PaperPoint prevpt = NOPT;
            while(i < result[l]->Len)
            {
                PaperPoint pt = transformation(UserPoint(result[l]->X[i], result[l]->Y[i]));
                i++;
                // On elimine ainsi les points invalides
                // ( Pour lesquels la projection a "échoué" )
                // if (!transformation.in(pt) || => NON il peut manquer un segment sur les bords
                if (!pt.in(xmin, xmax, ymin, ymax) ||
                        pt.x() != pt.x() || pt.y() != pt.y() ) { // x != x => NAN (Cf mercator)
                    // Vire aussi les points valides vraiment hors domaine par
                    // effet induit. Mais ils auraient eté virés par le clipping
                    // effectué ci-dessous par transformation(poly, parent)
                    break;
                }
                // gestion "overlapping" proj4 => x(ll) = x(ll%360)
                // Pour éviter un trait "horizontal" entre 2 points valides
                // rapprochés en latlon mais près de 2 bords opposés plan
                // projeté suite au modulo 360 (ex EPSG::3832 (-30.5W, -29.5W))
                // Il manquera le segment (d'entrée ou sortie du domaine ),
                // mais on ne peut pas faire autrement.
                if(distmax > 0 && !(prevpt == NOPT)) {
//              if( pt.distance(prevpt) > distmax ) { NON -> distance() très couteux
                    double dist2 = (pt.x()-prevpt.x())*(pt.x()-prevpt.x()) +
                                (pt.y()-prevpt.y())*(pt.y()-prevpt.y());
                    if (dist2 > distmax2) {
                        i--;
                        break;
                    }
                }
                poly.push_back(pt);
                prevpt = pt;
            }
            if (poly.size() >= 2)
                transformation(poly, parent);
            poly.clear();
        }
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


