/*! \file BasicSceneVisitor.h
    \brief Definition of the Template class BasicSceneVisitor.
    
    Magics Team - ECMWF 2008
    
    Started: Fri 19-Dec-2008
    
    Changes:
    
*/

#ifndef BasicSceneVisitor_H
#define BasicSceneVisitor_H

#include "magics.h"


namespace magics {

class BasicSceneVisitor {

public:
	BasicSceneVisitor();
	virtual ~BasicSceneVisitor();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	BasicSceneVisitor(const BasicSceneVisitor&);
    //! Overloaded << operator to copy - No copy allowed
	BasicSceneVisitor& operator=(const BasicSceneVisitor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BasicSceneVisitor& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
