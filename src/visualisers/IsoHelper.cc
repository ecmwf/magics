/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IsoHelper.cc
    \brief Implementation of the Template class IsoHelper.
    
    Magics Team - ECMWF 2010
    
    Started: Thu 11-Mar-2010
    
    Changes:
    
*/

#include "IsoHelper.h"
#include "AutoLock.h"
#include "Timer.h"
using namespace magics;

#include "magics_windef.h"
#ifdef MAGICS_ON_WINDOWS
  #include <iterator>
#endif


IsoHelper::IsoHelper(int n, vector<magics::Polyline* >& lines, IsoData& segments) :
 n_(n), lines_(lines),data_(segments)
{
}


void IsoHelper::concatenate_back(vector<magics::Polyline* >& lines, magics::Polyline* poly) 
{
	double x = poly->back().x();
	double y = poly->back().y();

	for (vector<magics::Polyline* >::iterator line = lines.begin(); line != lines.end(); ++line) {
		if ( *line == poly || (*line)->empty() ) {
			continue;
		}

		if (same((*line)->front().x(), x) && same((*line)->front().y(), y) ) {
			poly->push_back(**line);

			delete (*line);
			*line = 0;
			lines.erase(line);
			return;
		}
	}
}

void IsoHelper::concatenate_front(vector<magics::Polyline* >& lines, magics::Polyline* poly) 
{
	double x = poly->front().x();
	double y = poly->front().y();

	vector<magics::Polyline* >::iterator todelete  = lines.end();
	for (vector<magics::Polyline* >::iterator line = lines.begin(); line != lines.end(); ++line) {
		if ( *line == poly || (*line)->empty() ) {
			continue;
		}

		if (same((*line)->back().x(), x) && same((*line)->back().y(), y) ) {
			poly->push_front(**line);
			delete (*line);
			*line = 0;
			lines.erase(line);
			return;
		}
	}

	if ( todelete !=   lines.end() )
		lines.erase(todelete);
}

/*!
 Class information are given to the output-stream.
*/
void IsoHelper::print(ostream& out)  const
{
	out << "IsoHelper[";
	out << "]";
}

static Mutex locklines_;

void IsoHelper::run()
{
	deque< std::pair<double, pair< std::pair<double, double>, pair<double, double > > > > todo;
	int last = 0;

	while (true) {
		{// Block
			AutoLock<MutexCond> lock(data_.cond_);
			while(data_.more_ && data_.segments_.size() < 2000 )
				data_.cond_.wait();

			if(!data_.more_ && data_.segments_.empty() ) {

				for (map<double, vector<magics::Polyline*> >::iterator level = helpers_.begin(); level != helpers_.end(); ++level)
					for (vector<magics::Polyline* >::iterator line = level->second.begin(); line != level->second.end(); ++line)

						if ( !(*line)->empty() ) {
							AutoLock<Mutex> locklines(locklines_);
							lines_.push_back(*line);
						}

				helpers_.clear();
				return;
			}
			{
				AutoLock<Mutex> locklines(locklines_);
				// Deal with the segments
				if (data_.segments_.size() > 2000|| !data_.more_) {
					todo.clear();
					std::copy(data_.segments_.begin(),  data_.segments_.end(), back_inserter(todo));
					data_.segments_.clear();
				}
			}
			data_.cond_.signal();
		}

		{AutoLock<Mutex> locklines(locklines_);
			for ( deque< std::pair<double, pair< std::pair<double, double>, pair<double, double > > > >::iterator x = todo.begin();
					x != todo.end(); ++x) {

				pair< std::pair<double, double>, pair<double, double > >& segment = x->second;

				double x1 = segment.first.first;
				double y1 = segment.first.second;
				bool missing1 = false;
				double x2 = segment.second.first;
				double y2 = segment.second.second;
				bool missing2 = false;
				bool doit = true;

				map<double, vector<magics::Polyline*> >::iterator h = helpers_.find(x->first);
				if ( h == helpers_.end() ) {
					helpers_.insert(make_pair(x->first,  vector<magics::Polyline*>()));
				}
				vector<magics::Polyline*>& helper = helpers_[x->first];

				int length = helper.size();

				for (unsigned int i = 0; i < helper.size(); i++ ) {
					int j = (i + last) % length;
					// try to add to the back !
					magics::Polyline* line = helper[j];

					if (same(line->back().x(), x1) && same(line->back().y(), y1) ) {
						line->push_back(PaperPoint(x2, y2, x->first, missing2));
						// Check if we can concatenate with another line...
						concatenate_back(helper, line);
						doit = false;
						last = j;
						break;
					}
					if (same(line->front().x(), x2) && same(line->front().y(), y2) ) {
						line->push_front(PaperPoint(x1, y1, x->first, missing1));
						concatenate_front(helper, line);
						doit = false;
						last = j;
						break;
					}
				}
				if (doit) {
					last = helper.size();
					helper.push_back(new magics::Polyline());
					helper.back()->push_back(PaperPoint(x1, y1, x->first, missing1));
					helper.back()->push_back(PaperPoint(x2, y2, x->first, missing2));
				}
			} // next todo
			todo.clear();
		}
	}
}
