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

/*!
    \brief Definition of the Template class ViewNode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 6-Mar-2007
    
    Changes:
    
*/

#ifndef PreviewNode_H
#define PreviewNode_H

#include "magics.h"
#include "SceneVisitor.h"
#include "Coastlines.h"



namespace magics {


class NoPreviewVisitor: public SceneVisitor, public PreviewLayout
{
public:
	NoPreviewVisitor()  {}
	virtual ~NoPreviewVisitor() {}
	virtual void set(const XmlNode&) {}
	virtual void set(const map<string, string>&) {}
	virtual bool accept(const string&) { return false;}
	virtual void toxml(ostream&, int = 0) const {}
	virtual NoPreviewVisitor* clone() const { return new NoPreviewVisitor();}

	void visit(BasicSceneObject&) {}

protected:
	virtual void print(ostream& s) const;

	friend ostream& operator<<(ostream& s,const NoPreviewVisitor& p)
	{
		p.print(s); 
		return s;
	}
};


class PreviewVisitor: public NoPreviewVisitor
{
public:
	PreviewVisitor() ;
	~PreviewVisitor();
	NoPreviewVisitor* clone() const { return new PreviewVisitor(); }
	void visit(BasicGraphicsObjectContainer&);
	void redisplay(const BaseDriver& driver) const;
	void visit(BasicSceneObject& object);
	CoastPlotting& coastlines() { return coastlines_; }
	
protected: 
	void print(ostream& s) const;
	CoastPlotting coastlines_;
};


template <>
class MagTranslator<string, NoPreviewVisitor> { 
public:
	NoPreviewVisitor* operator()(const string& val )
	{
		return SimpleObjectMaker<NoPreviewVisitor>::create(val);
	}     

	NoPreviewVisitor* magics(const string& param)
	{
		NoPreviewVisitor* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
