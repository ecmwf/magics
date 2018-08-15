/*! \file MagClipper.h
    \brief Definition of the Template class MagClipper.
    
    Magics Team - ECMWF 2018
    
    Started: Mon 13-Aug-2018
    
    Changes:
    
*/

#ifndef MagClipper_H
#define MagClipper_H

#include "magics.h"
#include "clipper.hpp"


namespace magics {

class Polyline;

class MagClipper {

public:
	MagClipper();
	~MagClipper();

    void clip(const Polyline& subject, const Polyline& clip, vector<Polyline*>& result);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	MagClipper(const MagClipper&);
    //! Overloaded << operator to copy - No copy allowed
	MagClipper& operator=(const MagClipper&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MagClipper& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
