/*
 * (C) Copyright 1996-2012 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Segmenter.h
// Baudouin Raoult - ECMWF Aug 02

#ifndef SegmentJoiner_H
#define SegmentJoiner_H

#include "marsmachine.h"
#include "MagExceptions.h"
#include <cmath>


struct Point {
    double x_;
    double y_;

    Point(double x = 0, double y = 0):
        x_(x), y_(y)
    {
    }

    friend bool operator ==(const Point& a,const Point& b)
    {
        return (a.x_ == b.x_) && (a.y_ == b.y_);
    }

    friend ostream& operator <<(ostream& s,const Point& p)
    {
        return s << "(" << p.x_ << "," << p.y_ << ")";
    }
};



struct Segment {
    bool ok_;
    Point from_;
    Point to_;

    unsigned long phash_;

    vector<Segment*> before_; // Index of segment before in polyline
    vector<Segment*> after_;//  Index of segment after in polyline

    Segment* fnext_; // Next is the "from" hash table, in case of collisions
    Segment* tnext_; // Next is the "to" hash table, in case of collisions

    unsigned long fhash_; // Hash value of the "from_" point
    unsigned long thash_; // Hash value of the "to_" point


    Segment(const Point& from, const Point& to):
        ok_(true), from_(from), to_(to),  before_(0), after_(0), fnext_(0), tnext_(0){}

    bool cancels(const Segment& other) const
    { return from_ == other.to_ && to_ == other.from_; }

    friend ostream& operator <<(ostream& s,const Segment& p)
    { return s << "[from=" << p.from_ << ",to=" << p.to_ << "]"; }


    double crossProduct(const Segment& other) const
    {
        double  ux = to_.x_ - from_.x_;
        double  uy = to_.y_ - from_.y_;
        double  vx = other.to_.x_ - other.from_.x_;
        double  vy = other.to_.y_ - other.from_.y_;
        return ux*vy-uy*vx;
    }

    bool colinear(const Segment& other) const
    {
        return fabs(crossProduct(other)) < 1e-10;
    }
};

inline void reserve_(vector<Segment>& v,size_t s) { v.reserve(s); }
inline void reserve_(list<Segment>& v,size_t s) {  }
inline void reserve_(deque<Segment>& v,size_t s) {  }

class SegmentJoiner {
public:

    typedef deque<Segment> SegList;

    // -- Contructors

    SegmentJoiner(): dirty_(false) {}

    // -- Destructor

    ~SegmentJoiner() {}

    // -- Methods

    void reserve(size_t size)
    {
        reserve_(segments_,size);
    }


    size_t size() const
    { return segments_.size(); }

    void push_back(const Segment& s);

    void add(const SegmentJoiner& other)
    {
    	for (SegList::const_iterator segment = other.segments_.begin(); segment != other.segments_.end(); ++segment)
    	   push_back(*segment);
    }

    void push_back(const Point& from, const Point& to);


    void push_back(double x1, double y1, double x2, double y2)
    { segments_.push_back(Segment(Point(x1,y1),Point(x2,y2))); }

    // Call this one...
    double computePolygonLines(vector<vector<Point> >& result);

    double computeSegmentLines(list<deque<Segment> >& result);

    double punchHoles(vector<vector<Point> >& result);


    static double area(const vector<Point>& );
    static bool isHole(const vector<Point>& p) { return area(p) < 0; }

    static bool pointInPoly(const Point&,const vector<Point>& p);
    static void check(list<deque<Segment> >& lines);
    int index_;

private:

    // No copy allowed

    SegmentJoiner(const SegmentJoiner&);
    SegmentJoiner& operator=(const SegmentJoiner&);

    // -- Members

    SegList segments_;
    bool dirty_;


};

#endif
