
/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <MagClipper.h>
#include <Polyline.h>

using namespace magics;

double scale_ = 100000;

int convert(const deque<PaperPoint>& in, ClipperLib::Path& out, bool print = false)
{
    out.reserve(in.size());
    if (print)
        cout << " -----------start---------------------------" << endl;
    for (auto pt = in.begin(); pt != in.end(); ++pt) {
        if (print)
            cout << "     subj.push_back(MyPoint(" << long(pt->x_ * scale_) << ", " << long(pt->y_ * scale_) << ").get());" << endl;
        out.push_back(ClipperLib::IntPoint(long(pt->x_ * scale_), long(pt->y_ * scale_)));
    }
    int orientation = ClipperLib::Orientation(out);
    if (print)
        cout << " -------------------" << orientation << "----" << out.size() << "---------------------" << endl;
}

int convert(const ClipperLib::Path& in, deque<PaperPoint>& out)
{

    for (auto pt = in.begin(); pt != in.end(); ++pt) {
        out.push_back(PaperPoint(pt->X / scale_, pt->Y / scale_));
    }
}

void MagClipper::clipOpened(const Polyline& subject, const Polyline& clip, vector<Polyline*>& out)
{
    ClipperLib::Path path_subject, path_clip;
    ClipperLib::PolyTree solution;
    ClipperLib::Clipper clipper;

    convert(subject.polygon(), path_subject);

    //cout << "Add line" << ClipperLib::Orientation(path_subject) << endl;
    clipper.AddPath(path_subject, ClipperLib::ptSubject, false);
    convert(clip.polygon(), path_clip);

    clipper.AddPath(path_clip, ClipperLib::ptClip, true);
    clipper.Execute(ClipperLib::ctIntersection, solution, ClipperLib::pftNonZero, ClipperLib::pftNonZero);

    ClipperLib::PolyNode* node = solution.GetFirst();

    while (node) {

        Polyline* poly = new Polyline();
        poly->copy(subject);
        convert(node->Contour, poly->polygon());
        out.push_back(poly);
        node = node->GetNext();
    }
}

void MagClipper::clip(const Polyline& subject, const Polyline& clip, vector<Polyline*>& out)
{

    if (subject.empty())
        return;
/*
    if ( subject.closed() ) {
    	cout << " clip" << endl;
    	return clipClosed(subject, clip, out);
    }
    cout << " igmore" << endl;
  */
   return subject.closed() ? clipClosed(subject, clip, out) : clipOpened(subject, clip, out);
}

void MagClipper::clip(const Polyline& subject, const PaperPoint& lowerleft, const PaperPoint& upperright, vector<Polyline*>& result)
{
    Polyline clip;
    clip.push_back(lowerleft);
    clip.push_back(PaperPoint(lowerleft.x_, upperright.y_));
    clip.push_back(upperright);
    clip.push_back(PaperPoint(upperright.x_, lowerleft.y_));
    clip.push_back(lowerleft);
    MagClipper::clip(subject, clip, result);
}

void MagClipper::clipClosed(const Polyline& subject, const Polyline& clip, vector<Polyline*>& out)
{
    // Create Path from outer and holes!
    //First Outer!
    ClipperLib::Path path_subject, hole_subject, path_clip;
    ClipperLib::Paths solution, holes;
    ClipperLib::Clipper clipper, clipper_holes;

    convert(subject.polygon(), path_subject);

    int orientation = ClipperLib::Orientation(path_subject);
    if (ClipperLib::Orientation(path_subject) == 0) {
        ClipperLib::ReversePath(path_subject);
    }
    //cout << "Add line" << ClipperLib::Orientation(path_subject) << endl;
    clipper.AddPath(path_subject, ClipperLib::ptSubject, true);
    convert(clip.polygon(), path_clip);
    for (auto hole = subject.beginHoles(); hole != subject.endHoles(); ++hole) {
        ClipperLib::Path path;
        convert(*hole, path);
        if (ClipperLib::Orientation(path) == 1) {
            ClipperLib::ReversePath(path);
        }
        //cout << "Add Hole" << ClipperLib::Orientation(path) << endl;
        clipper.AddPath(path, ClipperLib::ptSubject, true);
    }

    clipper.AddPath(path_clip, ClipperLib::ptClip, true);
    clipper.Execute(ClipperLib::ctIntersection, solution, ClipperLib::pftNonZero, ClipperLib::pftNonZero);

    Polyline* poly = 0;
    vector<ClipperLib::Paths::iterator> couters;
    map<ClipperLib::Paths::iterator, Polyline*> helper;
    vector<ClipperLib::Paths::iterator> choles;

    for (auto path = solution.begin(); path != solution.end(); ++path) {
        orientation = ClipperLib::Orientation(*path);
        if (orientation == 1) {
            couters.push_back(path);
            Polyline* poly = new Polyline();
            convert(*path, poly->polygon());
            poly->close();
            helper.insert(make_pair(path, poly));
        } else {
            choles.push_back(path);
        }
    }

    for (auto hole = choles.begin(); hole != choles.end(); ++hole) {
        Polyline poly;
        convert(**hole, poly.polygon());
        poly.close();
        for (auto outer = couters.begin(); outer != couters.end(); ++outer)
            if (ClipperLib::PointInPolygon((*hole)->front(), **outer)) {
                helper[*outer]->newHole(poly);

                break;
            }
    }

    for (auto outer = couters.begin(); outer != couters.end(); ++outer) {
        out.push_back(helper[*outer]);
    }
}
void MagClipper::add(const Polyline& subject, const Polyline& clip, vector<Polyline*>& out)
{
    // Create Path from outer and holes!
    //First Outer!
    ClipperLib::Path path_subject, hole_subject, path_clip;
    ClipperLib::Paths solution, holes;
    ClipperLib::Clipper clipper, clipper_holes;

    convert(subject.polygon(), path_subject);

    //cout << "Add line" << ClipperLib::Orientation(path_subject) << endl;
    clipper.AddPath(path_subject, ClipperLib::ptSubject, true);
    convert(clip.polygon(), path_clip, true);

    clipper.AddPath(path_clip, ClipperLib::ptClip, true);
    clipper.Execute(ClipperLib::ctUnion, solution, ClipperLib::pftNonZero, ClipperLib::pftNonZero);

    for (auto path = solution.begin(); path != solution.end(); ++path) {
        Polyline* poly = new Polyline();
        convert(*path, poly->polygon());
        out.push_back(poly);
    }
}

bool MagClipper::in(const Polyline& poly, const PaperPoint& point)
{
    ClipperLib::Path path;
    convert(poly.polygon(), path);

    return ClipperLib::PointInPolygon(ClipperLib::IntPoint(long(point.x_ * scale_), long(point.y_ * scale_)), path);
}

MagClipper::MagClipper()
{
}
MagClipper::~MagClipper()
{
}

void MagClipper::print(ostream&) const
{
}
