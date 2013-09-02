/*! \file BinaryObject.h
    \brief Definition of the Template class BinaryObject.
    
    Magics Team - ECMWF 2010
    
    Started: Fri 8-Jan-2010
    
    Changes:
    
*/

#ifndef BinaryObject_H
#define BinaryObject_H

#include "magics.h"

#include "BasicGraphicsObject.h"
#include "BasicSceneObject.h"
#include "BinaryObjectAttributes.h"

namespace magics {

class BaseDriver;
class SceneLayer;

class BinaryObject: 
     public BasicGraphicsObject,
     public BasicSceneObject,
     public BinaryObjectAttributes 
{
public:
	BinaryObject();
	virtual ~BinaryObject();
	void redisplay(const BaseDriver&) const;
 
	void visit(SceneLayer&);
    
	void set(const map<string, string>& map ) { BinaryObjectAttributes::set(map); }
	void set(const XmlNode& node ) { BinaryObjectAttributes::set(node); }
	const string& getPath() const
			{ return path_; }


	double getTransparency() const { return transparency_; }
	double getMgb_x() const { return mgb_x_; }
	double getMgb_y() const { return mgb_y_; }
	double getMgb_width() const { return mgb_width_; }
	double getMgb_height() const { return mgb_height_; }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	BinaryObject(const BinaryObject&);
	//! Overloaded << operator to copy - No copy allowed
	BinaryObject& operator=(const BinaryObject&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BinaryObject& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
