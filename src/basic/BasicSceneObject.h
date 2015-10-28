/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file BasicSceneObject.h
    \brief Definition of the Template class BasicSceneObject.
    
    Magics Team - ECMWF 2007
    
    Started: Thu 1-Mar-2007
    
    Changes:
    
*/

#ifndef BasicSceneObject_H
#define BasicSceneObject_H

#include "magics.h"
#include "MagLog.h"
#include "VectorOfPointers.h"
#include "BasicGraphicsObject.h"
#include "Layout.h"
#include "DisplayManager.h"
#include "MagicsEvent.h"

//#include "SceneVisitor.h"

namespace magics {
	
class XmlNode;
class UserPoint;
class UserPoint;

class Transformation;
class AnimationRules;




class DrawingVisitor;
class TopAxisVisitor;
class BottomAxisVisitor;
class LeftAxisVisitor;
class RightAxisVisitor;
class BackgroundVisitor;
class FrameVisitor;
class TextVisitor;
class MetaDataVisitor;
class LegendVisitor;
class PreviewVisitor;
class HistoVisitor;
class MagnifierVisitor;
class SceneVisitor;
class LayoutManager;
class AnimationStep;
class SceneLayer;

class MetaDataCollector;
class MagnifierCollector;
class ValuesCollector;
class DataIndexCollector;
class BinningObject;

class Data;
class Visdef;

class BasicPositionalObject;
class BasicSceneNode;

enum MagicsMode { interactif, paper, basic};

class BasicSceneObject : public MetviewIcon {

public:
	BasicSceneObject(BasicSceneObject* parent = 0);
	virtual ~BasicSceneObject();
	
	virtual void push_back(BasicSceneObject* item) { 
		item->parent(this); 
		items_.push_back(item); 
	}
	
	bool items_empty() { return items_.empty(); }
	
	virtual void text(TextVisitor* text) { ASSERT(parent_); parent_->text(text); }
	virtual void legend(LegendVisitor* legend) { ASSERT(parent_); parent_->legend(legend); }
	virtual void  getReady(const LegendVisitor&);


	virtual void binning(BinningObject*)   { MagLog::warning() << "binning(BinningObject*) to be checked" << endl; }
	virtual void data(Data*)     { MagLog::warning() << "data(Data*) to be checked" << endl; }
	virtual void visdef(Visdef*) { MagLog::warning() << "visdef(Data*) to be checked" << endl; }
	

	
	void addVisitor(SceneVisitor* visitor) { visitors_.push_back(visitor); }
	
	void parent(BasicSceneObject* parent) { 

		parent_ = parent;  
	}
	void reparent(BasicSceneObject* parent) { 
			parent_ = parent;  
	}
	
	void orphan() { parent_ = 0; }
	BasicSceneObject& parent() const { ASSERT(parent_); return *parent_; }
	virtual void set(const XmlNode&) 
		{ MagLog::dev() << "Warning:  BasicSceneObject::set(const XmlNode&)-->Not implemented!" << endl; }
	virtual void resolve();
	virtual Layout* execute(AnimationStep&,  Layout&)
		{ MagLog::dev() << "Warning:  BasicGraphicsObject* BasicSceneObject::execute(AnimsationStep&))-->Not implemented!" << endl; return 0;}
	virtual void getReady() { dispatch(&BasicSceneObject::getReady); }
	virtual void release() { dispatch(&BasicSceneObject::release); }

	virtual void visit(MetaDataVisitor& meta)  		{ dispatch(meta); } 
	virtual void visit(PreviewVisitor& preview)  		{ dispatch(preview); }
	virtual void visit(HistoVisitor& histo)  		{ dispatch(histo); }
	virtual void visit(MagnifierVisitor& magnifier)  	{ dispatch(magnifier); }
	
	virtual void visit(DateDescription& timestamp)  	{ dispatch(timestamp); }
	virtual void visit(LevelDescription& level)  	{ dispatch(level); }

	virtual void visit(Transformation& transformation)  	{ dispatch(transformation); }
	virtual void visit(AnimationRules& rules)  { dispatch(rules); }
	
	virtual void visit(MetaDataCollector& infos)  { dispatch(infos); }
	virtual void visit(MagnifierCollector& infos)  { dispatch(infos); }
	virtual void visit(ValuesCollector& infos)  { dispatch(infos); }
	virtual void visit(DataIndexCollector& infos)  { dispatch(infos); }
	
	virtual void visit(DrawingVisitor& drawing)  { dispatch(drawing); }
	virtual void visit(TopAxisVisitor& top)  { dispatch(top); }
	virtual void visit(BottomAxisVisitor& bottom)  { dispatch(bottom); }
	virtual void visit(LeftAxisVisitor& left)  { dispatch(left); }
	virtual void visit(RightAxisVisitor& right)  { dispatch(right); }
	virtual void visit(BackgroundVisitor& background)  { dispatch(background); }
	virtual void visit(FrameVisitor& frame)  { dispatch(frame); }
	
	virtual void visit(TextVisitor& text)  { dispatch(text); }
	virtual void visit(LegendVisitor& legend)  { dispatch(legend); }
	virtual void visit(SceneLayer& tree)  { dispatch(tree); }
	virtual void visit(BasicGraphicsObjectContainer& tree)  { dispatch(tree); }
	virtual void visit(AnimationStep& step)  { dispatch(step); }
	virtual void visit(SceneLayer& visitor, vector<LayoutVisitor*>& args)  { dispatch(visitor, args); }

	virtual bool needLegend();

	virtual void execute()
	{
	  ASSERT(parent_); 
	  return parent_->execute(); 
	} 

	virtual  BasicGraphicsObject* visualise()
	{
		  ASSERT(parent_);
		  return parent_->visualise();
	}
	virtual MagicsMode mode() { ASSERT(parent_); return parent_->mode(); }

	virtual Transformation& transformation() const
		{ ASSERT(parent_); return parent_->transformation(); } 
	virtual const Layout& layout() const 
		{ ASSERT(parent_); return parent_->layout(); } 
	virtual BasicGraphicsObject* toDisplay()
		{ ASSERT(parent_); return parent_->toDisplay(); }
	virtual double absoluteWidth()    const 
		{ ASSERT ( parent_ ); return  parent_->absoluteWidth(); } 
   	virtual double absoluteHeight()   const 
		{ ASSERT ( parent_ ); return  parent_->absoluteHeight(); } 
   	virtual void absoluteRootWidth(double width)   
		{ ASSERT ( parent_ ); return  parent_->absoluteRootWidth(width); } 
   	virtual void absoluteRootHeight(double height)   
   		{ ASSERT ( parent_ ); return  parent_->absoluteRootHeight(height); }
 	virtual int rootWidthResolution()   const
		{ ASSERT ( parent_ ); return  parent_->rootWidthResolution(); } 
   	virtual int rootHeightResolution() const   
   		{ ASSERT ( parent_ ); return  parent_->rootHeightResolution(); }
   	virtual int widthResolution()   const
   			{ ASSERT ( parent_ ); return  parent_->rootWidthResolution(); } 
   	virtual int heightResolution() const   
   	   		{ ASSERT ( parent_ ); return  parent_->rootHeightResolution(); }
   
	const string& name() const { return name_; }
	void name(const string& name)  { name_ = name; }

	virtual BasicSceneNode* insert(BasicPositionalObject*) { ASSERT (false);  return 0;}


protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	typedef void (BasicSceneObject::*Function)();





	template <class T>
	void dispatch(T& visitor) {
		for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item) 
			(*item)->visit(visitor); 		
	}
	template <class T1, class T2>
		void dispatch(T1& visitor, T2& args) {
			for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item) 
				(*item)->visit(visitor, args); 		
		}
	void dispatch(Function function)
	{
		for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item) 
			((*item)->*function)(); 		
	}


	VectorOfPointers<vector<BasicSceneObject*> >          items_;
	vector<SceneVisitor*>     visitors_;	 
 
	 BasicSceneObject* parent_; //Do not delete! Only a reference

	 string name_;

private:
	BasicSceneObject(const BasicSceneObject&);
    //! Overloaded << operator to copy - No copy allowed
	BasicSceneObject& operator=(const BasicSceneObject&);
	
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BasicSceneObject& p)
		{ p.print(s); return s; }

};

class BasicPositionalObject : public BasicSceneObject
{
public: 
	BasicPositionalObject() {}
	virtual ~BasicPositionalObject() {}
	virtual Layout& layout() const = 0;
protected:
	double adjustDimension(double, double, double);
};

class BasicSceneNode : public BasicPositionalObject 
{
public:
	BasicSceneNode();
	BasicSceneNode(Layout*);
	virtual ~BasicSceneNode();

	virtual BasicSceneNode* insert(BasicPositionalObject*); // Return the node tinto which the object has been inserted! 
	Layout& layout() const
	{
	  { ASSERT(layout_); return *layout_; } 
	}    
	virtual void getReady();  
	virtual BasicSceneNode* clone();
	virtual BasicSceneNode* newNode(BasicPositionalObject*);

	virtual void visit(BasicGraphicsObjectContainer& tree)
	{
		tree.push_back(layout_);
		layout_->blankIt(); 
		dispatch(*layout_); 
		layout_->frameIt();  
	}
	void newpage();
	double absoluteWidth()    const;
	double absoluteHeight()   const;


	void manager(LayoutManager* manager) { manager_ = manager; }

protected:
	Layout* layout_;
	LayoutManager* manager_;
	friend class LayoutManager;
};

class EmptySceneObject: public BasicSceneObject {

public:
	EmptySceneObject();
	virtual ~EmptySceneObject();
    
 
    
    void visit(SceneLayer&, vector<LayoutVisitor*>&);
  
   

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
    
   
     
private:
    //! Copy constructor - No copy allowed
	EmptySceneObject(const EmptySceneObject&);
    //! Overloaded << operator to copy - No copy allowed
	EmptySceneObject& operator=(const EmptySceneObject&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EmptySceneObject& )
		{ s << "EmptySceneObject";  return s; }

};

} // namespace magics
#endif
