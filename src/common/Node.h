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

/*! \file Node.h
    \brief Definition of the Template class Node.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 11-Feb-2004
    
    Changes:
    
*/

#ifndef Node_H
#define Node_H

#include "magics.h"

#include "BaseSceneObject.h"
#include "BasicGraphicsObject.h"
#include "SuperPageAttributes.h"
#include "XmlPageAttributes.h"

   

namespace magics {


class FloatingNode : public BaseSceneObject
{
public:
	FloatingNode() : layout_(0) {}
	~FloatingNode() {}
	
	virtual void set(const map<string, string>&) 
		{ MagLog::warning() << "set(const map<string, string>&) not implemented for " << *this << "\n"; }
	virtual void set(const XmlNode&) 
		{ MagLog::warning() << "set(const XmlNode&) not implemented for " << *this << "\n"; }
	
	Layout& getLayout()  { return *layout_; }
	virtual void setFromFortran() {}
protected: 
	Layout* layout_;
	
};


class FrameNode;

class RootNode : public BaseSceneObject
{
public: 
    RootNode();
    ~RootNode();
    virtual void set(const map<string, string>&);
    virtual void set(const XmlNode&);
    
    // Ensure compatibilty width the old Magics, and fortran
    virtual void set(const SuperPageAttributes&);
    
    // Ensure compatibility with the New Magics.
    virtual void set(const XmlPageAttributes&);    
    
    void prepareGraphics();
    
    double getWidth() const    { return width_; }   
    double getHeight() const   { return height_; }
    inline double rootWidth() const  { return width_; }
    inline double rootHeight() const { return height_; }
    inline double absoluteX_() const  { return 0; }
    inline double absoluteY_() const { return 0; }
    BaseSceneObject* newXmlNode();
    BaseSceneObject* newFortranNode();
    BaseSceneObject* newMetviewNode(const SuperPageAttributes&);
    Layout& getLayout();
    void addChild(BaseSceneObject* child);
    virtual void clear();
    
      
protected:
	virtual void print(ostream&) const;
    FrameNode*           frameNode_;
    double             width_;
    double             height_;
    FrameBase*           frame_;
    MetaDataVisitor*            metaData_;
    bool                 needsNewPage_;
    
    friend ostream& operator<<(ostream& s,const RootNode& p)
		{ p.print(s); return s; }
};


} // namespace magics
#endif
