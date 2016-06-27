/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "SegmentJoiner.h"

#include "marsmachine.h"
#include "Timer.h"
#include "MagExceptions.h"
#include <cmath>
#include <limits>

#define CHECK_COLINEAR
#define ONE_PASS
#define xxxPROBE 8

typedef uint64_t hash_t;

class Hasher {

    static const hash_t MIN_BITS = 8; // 256 entryes
    static const hash_t MAX_BITS = 22; // 4194304 entries

    hash_t bits_;
    hash_t hashSize_;
    hash_t hashMask_;

    double sfx_;
    double rvx_;
    double sfy_;
    double rvy_;

public:

    Hasher(const SegmentJoiner::SegList& v)
    {


        double minx =  numeric_limits<double>::infinity();
        double maxx = -numeric_limits<double>::infinity();
        double miny =  numeric_limits<double>::infinity();
        double maxy = -numeric_limits<double>::infinity();

        hash_t count = v.size();

        // Compute "best" hash size
        bits_= MIN_BITS;
        hashSize_ = (1<<bits_);

        // Size of the hash table

        while(hashSize_ <= count && bits_ < MAX_BITS) {
            bits_ += 2;
            hashSize_ = (1<<bits_);
        }
        hashMask_ = hashSize_-1;


#ifdef PROBE
        int miss = 0;
        // Compute min/max coordinates
        for(SegmentJoiner::SegList::const_iterator i = v.begin(); i != v.end(); ++i)
        {
            const Segment& s = (*i);
            double x = s.from_.x_;
            double y = s.from_.y_;
            if(x > maxx) { maxx = x; miss = 0; }
            if(x < minx) { minx = x;miss = 0; }

            if(y > maxy)  { maxy = y;miss = 0; }
            if(y < miny)  { miny = y;miss = 0; }

            x = s.to_.x_;
            y = s.to_.y_;
            if(x > maxx)  { maxx = x;miss = 0; }
            if(x < minx) { minx = x;miss = 0; }

            if(y > maxy) { maxy = y;miss = 0; }
            if(y < miny) { miny = y;miss = 0; }

            if(miss++ > PROBE) break;
        }

        miss = 0;
        // Compute min/max coordinates
        for(SegmentJoiner::SegList::const_reverse_iterator i = v.rbegin(); i != v.rend(); ++i)
        {
            const Segment& s = (*i);
            double x = s.from_.x_;
            double y = s.from_.y_;
            if(x > maxx) { maxx = x; miss = 0; }
            if(x < minx) { minx = x;miss = 0; }

            if(y > maxy)  { maxy = y;miss = 0; }
            if(y < miny)  { miny = y;miss = 0; }

            x = s.to_.x_;
            y = s.to_.y_;
            if(x > maxx)  { maxx = x;miss = 0; }
            if(x < minx) { minx = x;miss = 0; }

            if(y > maxy) { maxy = y;miss = 0; }
            if(y < miny) { miny = y;miss = 0; }

            if(miss++ > PROBE) break;
        }
#else
        // Compute min/max coordinates
        for(SegmentJoiner::SegList::const_iterator i = v.begin(); i != v.end(); ++i)
        {
            const Segment& s = (*i);
            double x = s.from_.x_;
            double y = s.from_.y_;
            if(x > maxx) maxx = x;
            if(x < minx) minx = x;

            if(y > maxy) maxy = y;
            if(y < miny) miny = y;

            x = s.to_.x_;
            y = s.to_.y_;
            if(x > maxx) maxx = x;
            if(x < minx) minx = x;

            if(y > maxy) maxy = y;
            if(y < miny) miny = y;
        }
#endif
        bits_ /= 2;

        // Compute scale factor/reference values
        sfx_ = 1;
        rvx_ = 0;
        if(minx != maxx) {
            sfx_ = double((1<<(bits_))-1) / (maxx - minx);
            rvx_ = -minx;
        }

        sfy_ = 1;
        rvy_ = 0;
        if(miny != maxy) {
            sfy_ = double((1<<(bits_))-1) / (maxy - miny);
            rvy_ = -miny;
        }

    }

    hash_t operator()(const Point& p) const {
        return (((hash_t)((p.x_ + rvx_) * sfx_) << (bits_)) | ((hash_t)((p.y_ + rvy_) * sfy_))) & hashMask_;
    }

    hash_t hashSize() const {
        return hashSize_;
    }

};

static Segment* ok( vector<Segment*>& v) {
    for(vector<Segment*>::iterator j = v.begin(); j != v.end(); ++j)
        if((*j)->ok_)
            return *j;
    return 0;
}

static void fill_after(Segment* s, deque<Segment>& p)
{
    while( s)
    {
        if(!s->ok_) break;
        s->ok_ = false;
#ifdef CHECK_COLINEAR

        if(!p.empty() && s->colinear(p.back()))
        {
            p.back().to_ = s->to_;
        }
        else
#endif
        {
            p.push_back(*s);
        }
        s = ok(s->after_);
    }
}

static void fill_before(Segment* s, deque<Segment>& p)
{
    while(s)
    {
        if(!s->ok_) break;
        s->ok_ = false;
#ifdef CHECK_COLINEAR
        if(!p.empty() && s->colinear(p.front()))
        {
            p.front().from_ = s->from_;
        }
        else
#endif
        {
            p.push_front(*s);
        }
        s = ok(s->before_);
    }
}

static double compute(SegmentJoiner::SegList& v,list<deque<Segment> >& lines)
{
    //magics::Timer total("SegmentJoiner:: compute", "");

    hash_t count = v.size();


    Hasher hash(v);

    hash_t  hashSize = hash.hashSize();

    vector<Segment*> fromHash(hashSize, 0);
    vector<Segment*> toHash(hashSize, 0);


    vector<hash_t> H; H.reserve(hashSize);


    {
        // Compute the hash value of every points

        //magics::Timer timer("SegmentJoiner:: hashing points", "");

        for(SegmentJoiner::SegList::iterator i = v.begin(); i != v.end(); ++i)
        {
            Segment& s = (*i);
            hash_t hf = s.fhash_ = hash(s.from_);
            hash_t ht = s.thash_ = hash(s.to_);
            s.fnext_ = fromHash[hf];
            s.tnext_ = toHash[ht];

          // if(fromHash[hf] == 0) { H.push_back(hf); }
            fromHash[hf] = toHash[ht] = &s;
        }
    }

    // << "HASH " << hashSize << " H " << H.size() << " " << double(H.size()) / double(hashSize)*100.0 << "%, avg= " << double(v.size())/double(H.size()) << endl;

    {


        int n = 0;

        for(hash_t h = 0; h < hashSize; h++)
           // for(vector<hash_t>::const_iterator j = H.begin(); j != H.end(); ++j)
        {
           // hash_t h = (*j);
            Segment* from  = fromHash[h];
            Segment* fprev = 0;

            while(from)
            {
                Segment& s1 = *from;

                if(s1.ok_) {
                    Segment* tprev = 0;
                    Segment* to    = toHash[h];

                    while(to)
                    {
                        Segment& s2 = *to;

                        if(s2.ok_)
                        {
#ifdef ONE_PASS
                            if(s1.from_ == s2.to_)
                            {
                                if(s1.to_ == s2.from_) {
                                    s1.ok_ = s2.ok_ = false;
                                    if(tprev) { tprev->tnext_ = s2.tnext_; } else { toHash[h] = s2.tnext_; }
                                    if(fprev) { fprev->fnext_ = s1.fnext_; } else { fromHash[h] = s1.fnext_; }
                                    n++;
                                }

                                else
                                {

                                	s1.before_.push_back(to);
                                	s2.after_.push_back(from);
                                }

                            }
#else
                            if(s1.cancels(s2)) {
                                s1.ok_ = s2.ok_ = false;
                                if(tprev) { tprev->tnext_ = s2.tnext_; } else { toHash[h] = s2.tnext_; }
                                if(fprev) { fprev->fnext_ = s1.fnext_; } else { fromHash[h] = s1.fnext_; }
                                n++;
                            }
#endif
                        }
                        else {
                            if(tprev) { tprev->tnext_ = s2.tnext_; } else { toHash[h] = s2.tnext_; }
                        }

                        if(s2.ok_) {
                            tprev = to;
                        }
                        to = s2.tnext_;
                    }
                }
                else
                {
                    if(fprev) { fprev->fnext_ = s1.fnext_; } else { fromHash[h] = s1.fnext_; }
                }

                if(s1.ok_) {
                    fprev = from;
                }
                from  = s1.fnext_;
            }
        }


    }
#ifndef ONE_PASS
    {


        for(hash_t h = 0; h < hashSize; h++)
        {
            Segment* from = fromHash[h];

            while(from)
            {
                Segment& s1 = *from;
                if(s1.ok_)
                {
                    //                ASSERT(s1.ok_);

                    Segment* to   = toHash[h];

                    while(to)
                    {
                        Segment& s2 = *to;
                        //                   ASSERT(s2.ok_);
                        if(s2.ok_) {
                            if(s1.from_ == s2.to_) {
                                //ASSERT(!(s1.before_ && !s2.after_) || (s1.before_ && s2.after_));

                                //if(!s1.before_ && !s2.after_)
                                {
                                    s1.before_.push_back(to);
                                    s2.after_ .push_back(from);
                                }
                                break;

                            }
                        }
                        to = s2.tnext_;
                    }
                }
                from = s1.fnext_;
            }
        }
    }
#endif
    {
        //magics::Timer timer("SegmentJoiner:: join all connecting lines", "");

        bool again = true;

        for(SegmentJoiner::SegList::iterator i = v.begin(); i != v.end(); ++i)
        {
            Segment& s = (*i);
            if(s.ok_) {
                s.ok_ = false;
                lines.push_back(deque<Segment>(1,s));
                deque<Segment>& p = lines.back();
                fill_before(ok(s.before_),p);
                fill_after(ok(s.after_),p);

                again = true;

                while(again)
                {
                    again = false;

                    Segment& s1 = p.back();
                    size_t h = s1.thash_;
                    Segment* from = fromHash[h];

                    while(from)
                    {
                        Segment& s2 = *from;
                        if(s2.ok_) {
                            if(s2.from_ == s1.to_) {
                                fill_after(from,p);
                                again = true;
                                break;
                            }
                        }
                        from = s2.fnext_;
                    }
                }

                again = true;

                while(again)
                {
                    again = false;

                    Segment& s1 = p.front();
                    size_t h = s1.fhash_;
                    Segment* to   = toHash[h];

                    while(to)
                    {
                        Segment& s2 = *to;
                        if(s2.ok_) {
                            if(s2.to_ == s1.from_) {
                                fill_before(to,p);
                                again = true;
                                break;
                            }
                        }
                        to = s2.tnext_;
                    }
                }
            }
        }
    }

#ifdef CHECK_COLINEAR

    // A bit of tidying up

    for(std::list<deque<Segment> >::iterator k = lines.begin(); k != lines.end(); ++k)
    {
        deque<Segment>& p = (*k);
        if(p.size() >= 2) {
            Segment& f = p.front();
            Segment& b = p.back();

            if(f.from_ == b.to_)
            {
                if(f.colinear(b))
                {
                    b.to_ = f.to_;
                    p.pop_front();
                }
            }
        }
    }
#endif
    // For debugging
    if(true) SegmentJoiner::check(lines);

    //return total.elapsed();
    return 0;
}


double SegmentJoiner::computeSegmentLines(list<deque<Segment> >& result)
{
    if(dirty_) {
        // In case the algo is ran again, the segments internal states need resetting
        dirty_ = false;
        for(SegmentJoiner::SegList::iterator j = segments_.begin(); j != segments_.end(); ++j) {
            Segment& s = (*j);
            s.ok_ = true;
            s.before_.clear();
            s.after_.clear();
            s.fnext_ = s.tnext_ = 0;
        }
    }

    result.clear(); // Just in case
    double e = compute(segments_,result);
    dirty_ = true; // The segements need reseting
    return e;
}

double SegmentJoiner::computePolygonLines(vector<vector<Point> >& result)
{
    result.clear(); // Just in case
    list<deque<Segment> > lines;

    double e = computeSegmentLines(lines);

    // magics::Timer timer("SegmentJoiner:: from segments to polygones", "");
    result.resize(lines.size());


    size_t i = 0;
    for(std::list<deque<Segment> >::iterator k = lines.begin(); k != lines.end(); ++k, ++i)
    {

        deque<Segment>& p = (*k);
        vector<Point>& w = result[i];
        w.reserve(p.size()+1);

        deque<Segment>::iterator j = p.begin();
        // For the segement point, we add both points
        w.push_back((*j).from_);
        w.push_back((*j).to_);
        ++j;

        for(; j != p.end(); ++j) {
            w.push_back((*j).to_);
        }

    }
    return e;
}

double SegmentJoiner::area(const vector<Point> & poly)
{
    double area = 0;

    int nb = poly.size()-1;


    for(int i = 0; i < nb; i++)
    {
        area += (poly[i].x_*poly[i+1].y_) -  (poly[i+1].x_*poly[i].y_) ;
    }

    return area / 2.0;
}



bool SegmentJoiner::pointInPoly(const Point &p, const vector<Point> &poly) {
    // Ray casting algo
	int n = 0;
	    int j = poly.size() - 1;
	    for(int i = 0; i < poly.size(); i++) {
	        if( (poly[i].y_ > p.y_) != (poly[j].y_ > p.y_) )
	            if( p.x_ < (poly[j].x_ - poly[i].x_) * (p.y_ - poly[i].y_) / ( poly[j].y_ - poly[i].y_) + poly[i].x_)
	            {
	                n++;
	            }
	        j = i;
	    }
	    return (n%2 == 1);
}

void SegmentJoiner::punchHoles(vector<vector<Point> > &result) {
    vector<vector<Point> > polys;
    list<vector<Point> > holes;
    for(vector<vector<Point> >::iterator j = result.begin() ; j != result.end(); ++j) {
        if(!isHole((*j))) {
            holes.push_back(vector<Point>());
            std::swap((*j),holes.back());
        }
        else {
            polys.push_back(vector<Point>());
            std::swap((*j),polys.back());
        }
    }

    for(vector<vector<Point> >::iterator j = polys.begin() ; j != polys.end(); ++j) {
        for(std::list<vector<Point> >::iterator h = holes.begin() ; h != holes.end(); ) {
            list<vector<Point> >::iterator next = h; next++;
            if(pointInPoly((*h).front(), *j)) {
                // If there are more holes, you need to choose different points
                Point& p = (*j).back();

                (*j).insert((*j).end(), (*h).rbegin(), (*h).rend());
                (*j).push_back(p);

                holes.erase(h);
            }
            h = next;
        }
    }

    std::swap(polys,result);
}

//=================================================================================================
// Below is just for debugging

void SegmentJoiner::check(list<deque<Segment> >& lines)
{

    for(std::list<deque<Segment> >::iterator k = lines.begin(); k != lines.end(); ++k) {
        deque<Segment>& p = (*k);
        ASSERT(p.size());
        deque<Segment>::iterator j = p.begin();
        deque<Segment>::iterator i = j;
        ++j;
        for(; j != p.end(); ++j) {
            ASSERT((*i).to_ == (*j).from_);
            i = j;
        }
    }


    for(std::list<deque<Segment> >::iterator j = lines.begin();  j != lines.end(); ++j)
    {
        deque<Segment>& p = (*j);

        for(std::list<deque<Segment> >::iterator k = lines.begin(); k != lines.end(); ++k)
        {
            deque<Segment>& q = (*k);
            if(k != j) {
                ASSERT(!(p.back().to_ == q.front().from_));
                ASSERT(!(q.back().to_ == p.front().from_));

            }
        }
    }

    int n =0;

    for(std::list<deque<Segment> >::iterator k = lines.begin(); k != lines.end(); ++k) {
        deque<Segment>& p = (*k);
        n += p.size();
    }


}

class checker {
    public:
	checker(const Segment& seg ) : segment_(seg) {}
       bool operator()(const Segment& other)
       { return segment_.cancels(other); }
    Segment segment_;
};

void SegmentJoiner::push_back(const Point& from, const Point& to)
{
	Segment add(from, to);
	checker check(add);
	deque<Segment>::iterator done =  std::remove_if(segments_.begin(), segments_.end(), check);

	if ( done == segments_.end() )
			 segments_.push_back(Segment(from,to));
	else		{
		segments_.erase(done, segments_.end());
	}
}

void SegmentJoiner::push_back(const Segment& s)
{
	if((s.from_ == s.to_))
		return;
	checker check(s);
   	deque<Segment>::iterator done =  std::remove_if(segments_.begin(), segments_.end(), check);

	if ( done == segments_.end() )
			 segments_.push_back(s);
	else{

		segments_.erase(done, segments_.end());
	}
}

