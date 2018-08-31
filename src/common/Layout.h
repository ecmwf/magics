/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Layout.h
    \brief Definition of the Template class Layout.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 30-Jan-2004
    
    Changes:
    
*/

#ifndef Layout_H
#define Layout_H

#include "magics.h"
#include "BasicGraphicsObject.h"

#include "Colour.h"


namespace magics {


class PaperPoint;
class BasicSceneObject;
class AnimationRules;
class AnimationStep;
class LayoutVisitor;


class DriverInfo {
public:
	DriverInfo(double x, double y, double width, double height): 
		x_(x), y_(y), width_(width), height_(height) {}
	~DriverInfo() {}
	DriverInfo() {}

	void print(ostream& s) const
	{
		s << "<driver_info";
		s << " x = \'" << x_ << "\'";
		s << " y = \'" << y_ <<  "\'";
		s << " width = \'" << width_ << "\'";
		s << " height = \'" << height_ << "\'";
		s << "/>";
	}

	double x_;
	double y_;
	double width_;
	double height_;
	
	friend ostream& operator<<(ostream& s,const DriverInfo& p)
		{ p.print(s); return s; }
};

class Layout;
class LayoutVisitor;

class LayoutFrame 
{
public:
	LayoutFrame();
	~LayoutFrame();
	void blank(Layout&, const string&);
	void frame(Layout&);
protected:
	int thickness_;
	LineStyle style_;
	Colour colour_;
	Colour background_;
	
	bool blanking_;
	bool visible_;
	friend class Layout;
};


class StartPage : public BasicGraphicsObject
{
public:
	StartPage() {}
	virtual ~StartPage() {}
	void redisplay(const BaseDriver& driver) const;
};


class EndPage : public BasicGraphicsObject
{
public:
	EndPage() {}
	virtual ~EndPage() {}
	void redisplay(const BaseDriver& driver) const;
};


class Layout : public BasicGraphicsObjectContainer
{
public:
	Layout();
	virtual ~Layout();

	virtual Layout*  clone() const;
	virtual Layout*  newLayout() const { return new Layout(); }
	
	bool reproject(BasicGraphicsObjectContainer&) const;	
	void redisplay(const BaseDriver& driver) const;
	void redisplay(AnimationStep&, const BaseDriver& driver); 

	double x()    const { return x_ ; } // percentage of the parent
	double y()    const { return y_ ; };  // percentage of the parent
	double width()	 const { return width_ ; };  // percentage of the parent 
	double height() const { return height_ ; };  // percentage of the parent 
	
	void x(double x ) { x_ = x; }
	void y(double y ) { y_ = y; }
	
    double absoluteWidth(double);
	double absoluteHeight(double);
   
    
    
	void width(double width ) { width_ = width; }
	void height(double height ) { height_ = height; }
	
	
	void display(DisplayType display) { display_ = display; }
	DisplayType display() const { return display_; }

/* Methods needed to calculate the percentage when unit in cm!*/
	double absoluteX()      		const; //absolute position from the root
	double absoluteY()      		const; //absolute position from the root
	double absoluteWidth()  const; //absolute position from the root
	double absoluteHeight()  const; //absolute position from the root
	
	bool buildTree(const Layout&, unsigned int, const BaseDriver&) const;
	void release(); // Release the graphical objects!
	
	virtual Layout* execute(AnimationStep& step,  const Layout* visitor);

	
	double minX()  const { return xmin_; }
	double maxX()  const { return xmax_; }
	double minY()  const { return ymin_; }
	double maxY()  const { return ymax_; }
	void minX(double xmin)  { xmin_ = xmin; }
	void maxX(double xmax) {  xmax_ = xmax; }
	void minY(double ymin)  { ymin_ = ymin; }
	void maxY(double ymax) { ymax_ = ymax; }
	
	
	void setCoordinates(double xmin, double xmax, double ymin, double ymax) {
		xmin_ = xmin; xmax_ = xmax; ymin_ = ymin; ymax_ = ymax; 
	}

	static void reset()
	{
		driverInfos_.clear();
	}

	void pushDriverInfo(double x, double y, double width, double height) const 
	{
		driverInfos_.push_back(DriverInfo(x, y, width, height));
	} 
	
	void getDriverInfo(double& x, double& y, double& width, double& height)
	{
		if ( !driverInfos_.empty() ) {
			x = driverInfos_.front().x_;
			y = driverInfos_.front().y_;
			width = driverInfos_.front().width_;
			height = driverInfos_.front().height_;
		}
		else {
			if (parent_)
				parent_->getDriverInfo(x, y, width, height);
		}
	}

	void frame(const Layout& other)
	{
		frame_.blanking_ = other.frame_.blanking_;
		frame_.visible_ = other.frame_.visible_;
		frame_.colour_ = other.frame_.colour_;
		frame_.style_ = other.frame_.style_;
		frame_.thickness_ = other.frame_.thickness_;
		frame_.background_ = other.frame_.background_;
	}
	
	void frame(bool blank, bool visible, const Colour& colour, 
				LineStyle style, int thickness, const Colour& background) {
		frame_.blanking_ = blank;
		frame_.visible_ = visible;
		frame_.colour_ = colour;
		frame_.style_ = style;
		frame_.thickness_ = thickness;
		frame_.background_ = background;

	}
	
	void zoomable(bool zoom = true) { zoomable_ = zoom; }
	bool isZoomable() const { return zoomable_; }
	
	void setNavigable() { navigable_ = true; }
	bool isNavigable() const { return navigable_; }

	const string& id() const { return id_; }
	void id(const string& id) { id_ = id; }
    int zoomLevels() const { return zoomLevels_; }
    void zoomLevels(int levels)  { zoomLevels_ = levels; }
    int zoomCurrentLevel() const { return zoomCurrentLevel_; }
    void zoomCurrentLevel(int level)  { zoomCurrentLevel_ = level; }
 
    void animationRules(AnimationRules* rules) { animationRules_ = rules; }
    AnimationRules* animationRules() const { return animationRules_; }
    
    const Transformation& transformation() const { 
    	ASSERT(transformation_); 
    	return *transformation_; 
    }
    virtual bool childOfRoot() const { ASSERT(parent_); return parent_->root(); }
    
    void transformation(Transformation* );
    void transformation(Layout& layout) const
    { layout.transformation(transformation_);  }
  
    void frameIt() { frame_.frame(*this); }
    void clippIt(bool clipping) { clipping_ = clipping; }
    bool clipp() const { return clipping_; }
    void blankIt(const string& colour="white" ) { frame_.blank(*this, colour); }
    void resolve(bool resolve) { resolve_  = resolve; }
    bool resolve() const { return resolve_; }
    virtual void resize(double, double) {}
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

	BasicSceneObject* owner_;
	AnimationRules*   animationRules_; // will be deleted in the destructor!
	Transformation*   transformation_; // Just a reference! 

	//Dimension of the layout in percentage of the parent! 
	double width_; 
	double height_;
	double x_;
	double y_;
	DisplayType display_;
	
	// coordiantes system used in this layout!
	double xmin_;
	double xmax_;
	double ymin_; 
	double ymax_;
	
	bool zoomable_;
	bool navigable_;
	// Zoom information : only used if mode is ZOOMABLE
	int zoomLevels_;
	int zoomCurrentLevel_;
	string id_;
	
	bool resizable_;
	bool resolve_;
	
	bool clipping_;

	LayoutFrame frame_;
		
	static vector<DriverInfo> driverInfos_;
	friend class LayoutVisitor;
	friend class TopAxisVisitor;
	friend class BottomAxisVisitor;
	friend class LeftAxisVisitor;
	friend class RightAxisVisitor;
	
private:
	//! Copy constructor - No copy allowed
	Layout(const Layout&);
	//! Overloaded << operator to copy - No copy allowed
	Layout& operator=(const Layout&);
// 
};


class PreviewLayout : public Layout 
{
public:
	PreviewLayout();
	virtual ~PreviewLayout();
	void redisplay(const BaseDriver& driver) const;
	
};
class LegendLayout : public Layout
{
public:
	LegendLayout();
	virtual ~LegendLayout();
	void redisplay(const BaseDriver& driver) const;

};
class HistoLayout : public Layout 
{
public:
	HistoLayout();
	virtual ~HistoLayout();
	void redisplay(const BaseDriver& driver) const;
};

class SceneLayout : public Layout
{
public:
	SceneLayout();
	virtual ~SceneLayout();
	void redisplay(const BaseDriver& driver) const;
	
};



class MagnifierLayout : public Layout 
{
public:
	MagnifierLayout();
	virtual ~MagnifierLayout();
	void redisplay(const BaseDriver& driver) const;
	virtual void redisplay(const BaseDriver&, vector<PaperPoint>&,float,float) const {}
};

class RootLayout : public Layout
{
public:
	RootLayout(double width, double height);
	virtual ~RootLayout();
	double absoluteWidth() const { return absoluteWidth_; }
	double absoluteHeight() const { return absoluteHeight_; }
	bool childOfRoot() const { return false; }
	void redisplay(const BaseDriver& driver) const;

	 virtual void resize(double width, double height) { absoluteWidth_ = width; absoluteHeight_ = height; }

protected:
	double absoluteWidth_;
	double absoluteHeight_; 

};
class BasicLayout : public Layout
{
public:
	BasicLayout();
	virtual ~BasicLayout();
	bool buildTree(const Layout&, unsigned int, const BaseDriver&) const;
};
class LayoutVisitor;


class LayoutHelper
{
public:
	LayoutHelper();
	~LayoutHelper();
	void add(LayoutVisitor*);
	void attachTop(LayoutVisitor*);
	void attachNoConstraintTop(LayoutVisitor*);
	void attachLeft(LayoutVisitor*);
	void attachRight(LayoutVisitor*);
	void attachBottom(LayoutVisitor*);

protected :
	virtual void print(ostream&) const; 
	double xmin_;
	double xmax_;
	double ymin_;
	double ymax_;
	friend ostream& operator<<(ostream& s,const LayoutHelper& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
