/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
