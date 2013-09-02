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

#ifndef HistoNode_H
#define HistoNode_H

#include "magics.h"
#include "SceneVisitor.h"
#include "Coastlines.h"



namespace magics {


class HistoVisitor: public SceneVisitor, public HistoLayout
{
public:
	HistoVisitor();
	virtual ~HistoVisitor();
	virtual void set(const XmlNode&) {}
	virtual void set(const map<string, string>&) {}
	virtual bool accept(const string&) { return false;}
	virtual void toxml(ostream&, int = 0) const {}
	virtual HistoVisitor* clone() const { return new HistoVisitor();}
	void visit(BasicGraphicsObjectContainer&);
	void redisplay(const BaseDriver& driver) const;
	void visit(BasicSceneObject& object);
	void basic(bool basic) { basic_ = basic; }
	bool basic() const { return basic_; }
	void dataLayoutTransformation(const Transformation* dlt) {dataLayoutTransformation_=dlt;}
	const Transformation* dataLayoutTransformation() {return dataLayoutTransformation_;}
	void dataVisdefIcon(const MetviewIcon& icon) {dataVisdefIcon_.icon(icon);}
	const MetviewIcon& dataVisdefIcon() const {return dataVisdefIcon_;}

protected:
	virtual void print(ostream& s) const;

	friend ostream& operator<<(ostream& s,const HistoVisitor& p)
	{
		p.print(s); 
		return s;
	}
	bool basic_;
	MetviewIcon dataVisdefIcon_;
	const Transformation* dataLayoutTransformation_;
};




template <>
class MagTranslator<string, HistoVisitor> { 
public:
	HistoVisitor* operator()(const string& val )
	{
		return SimpleObjectMaker<HistoVisitor>::create(val);
	}     

	HistoVisitor* magics(const string& param)
	{
		HistoVisitor* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
