/*! \file ThreadNode.h
    \brief Definition of the Template class ThreadNode.
    
    Magics Team - ECMWF 2008
    
    Started: Tue 23-Sep-2008
    
    Changes:
    
*/

#ifndef ThreadNode_H
#define ThreadNode_H

#include "magics.h"

#include "BasicSceneObject.h"

namespace magics {

class ThreadNode: public BasicSceneObject {

public:
	ThreadNode();
	virtual ~ThreadNode();
	
	void visit(BasicGraphicsObjectContainer&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	ThreadNode(const ThreadNode&);
    //! Overloaded << operator to copy - No copy allowed
	ThreadNode& operator=(const ThreadNode&);



};

} // namespace magics
#endif
