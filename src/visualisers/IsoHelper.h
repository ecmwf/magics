/*! \file IsoHelper.h
    \brief Definition of the Template class IsoHelper.
    
    Magics Team - ECMWF 2010
    
    Started: Thu 11-Mar-2010
    
    Changes:
    
*/

#ifndef IsoHelper_H
#define IsoHelper_H

#include "magics.h"

#include "Thread.h"
#include "MutexCond.h"
#include "Polyline.h"
#include "IntervalMap.h"



namespace magics {



struct IsoData {
public:
    IsoData() { more_ = true; }
    deque<pair<double, pair<pair<double, double>, pair<double, double > > > > segments_;
    //deque<Shape*> shapes_;
    
    bool more_;
    MutexCond cond_;
};


class IsoHelper: public Thread {

public:
	
	
	IsoHelper(int, vector<Polyline* >&, IsoData& data);
	void run();	
	


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
    
    
	int      n_;
	
	vector<Polyline* >& lines_;
    map<double, vector<Polyline*> > helpers_;
  
	
	
	double level_;
	IsoData& data_; 
		

	
	//void reduce(vector<Shape*>&); 
    void concatenate_back(vector<Polyline* >& lines, Polyline* poly);
    void concatenate_front(vector<Polyline* >& lines, Polyline* poly);

private:
    //! Copy constructor - No copy allowed
	IsoHelper(const IsoHelper&);
    //! Overloaded << operator to copy - No copy allowed
	IsoHelper& operator=(const IsoHelper&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const IsoHelper& p)
		{ p.print(s); return s; }

};




} // namespace magics
#endif
