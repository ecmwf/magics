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

/*! \file VisualAction.h
    \brief Definition of the Template class ActionNode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 6-Mar-2007
    
    Changes:
    
*/

#ifndef ActionNode_H
#define ActionNode_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "Data.h"
#include "Visdef.h"

namespace magics {
class FrameLoop;
class HistoVisitor;



class VisualAction: public BasicSceneObject {

public:
	VisualAction();
	virtual ~VisualAction();
	
	void data(Data* data)     { data_ = data; }
	void set2D()     { data_->dimension(2); }
	void visdef(Visdef* visdef) { visdefs_.push_back(visdef); }
	void binning(BinningObject* binning)     { if (data_) data_->binning(binning); }
	const string name();
	const string id();

    // A visual Action is valid, if the data have been set and the list of visdef is not empty!
    bool isValid() const { return data_ && visdefs_.empty() == false; }

    bool needLegend();
    void  getReady(const LegendVisitor&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;
	 void getReady();
	 void release();
	 
	 void visit(DrawingVisitor&);
	 void visit(TopAxisVisitor&);
	 void visit(LegendVisitor&); 
	 void visit(TextVisitor&); 
	 void visit(MetaDataVisitor&); 
	 void visit(HistoVisitor&); 
	 
	 void visit(Transformation&);
	 
	 void visit(AnimationRules&);
	 
	 void visit(SceneLayer&, vector<LayoutVisitor*>&);
	 void visit(MetaDataCollector&);
	 void visit(ValuesCollector&);
	 void visit(DataIndexCollector&);
	 void visit(MagnifierCollector&);
	 
	 Data*                       data_;
	 vector<Visdef*>       visdefs_;
	 StaticLayer* layer_;

	 void visit(DateDescription&);
	 void visit(LevelDescription&);

private:
    //! Copy constructor - No copy allowed
	VisualAction(const VisualAction&);
    //! Overloaded << operator to copy - No copy allowed
	VisualAction& operator=(const VisualAction&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const VisualAction& p)
		{ p.print(s); return s; }
};



class VisualAnimation: public VisualAction,
	public vector<BasicSceneObject*>  {

public:
	VisualAnimation();
	virtual ~VisualAnimation();
	void loop(DataLoop* data)     { loop_ = data; }
	DataLoop& loop() { ASSERT(loop_); return *loop_; }
    
protected:
	
	void visit(MetaDataVisitor&); 
    void visit(Transformation&);
    void visit(AnimationRules&);
    void visit(SceneLayer&, vector<LayoutVisitor*>&);
    
    void prepare(); 
    
    DataLoop* loop_;
    StepLayer* layer_;
};

} // namespace magics

#endif
