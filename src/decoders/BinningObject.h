/*! \file BinningObject.h
    \brief Definition of the Template class BinningObject.
    
    Magics Team - ECMWF 2011
    
    Started: Thu 7-Apr-2011
    
    Changes:
    
*/

#ifndef BinningObject_H
#define BinningObject_H

#include "magics.h"
#include "Matrix.h"
#include "BinningObjectAttributes.h"
#include "BasePointsHandler.h"
#include "IntervalMap.h"
#include "Factory.h"
#include "MagTranslator.h"

namespace magics {

  
class BinningObject: public BinningObjectAttributes {

public:
	BinningObject();
	~BinningObject();

	virtual BinningObject* clone() const { return new BinningObject(); }

	Matrix* operator()(PointsList& points);



protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 void print(ostream&) const;
	 typedef void (BinningObject::*binner)(vector<double>&, double, double);
	 		 map<string,  binner> binners_x_;
	 		 map<string,  binner> binners_y_;

	 	void build(vector<double>& vals, IntervalMap<int>& binns);
	 	 void countx(vector<double>&, double, double);
	 	 void listx(vector<double>&, double, double);
	 	 void intervalx(vector<double>&, double, double);

	 	 void county(vector<double>&, double, double);
	 	 void listy(vector<double>&, double, double);
	 	 void intervaly(vector<double>&, double, double);
private:
    //! Copy constructor - No copy allowed
	BinningObject(const BinningObject&);
    //! Overloaded << operator to copy - No copy allowed
	BinningObject& operator=(const BinningObject&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BinningObject& p)
		{ p.print(s); return s; }

};


class NoBinningObject: public BinningObject {

public:
	NoBinningObject() {}
	~NoBinningObject() {}
	BinningObject* clone() const { return new NoBinningObject(); }
};


template <>
class MagTranslator<string, BinningObject> {
public:
	BinningObject* operator()(const string& val )
	{
		return SimpleObjectMaker<BinningObject>::create(val);
	}

	BinningObject* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};
} // namespace magics
#endif
