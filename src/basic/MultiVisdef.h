/*! \file MultiVisdef.h
    \brief Definition of the Template class MultiVisdef.
    
    Magics Team - ECMWF 2012
    
    Started: Wed 25-Jan-2012
    
    Changes:
    
*/

#ifndef MultiVisdef_H
#define MultiVisdef_H

#include "magics.h"

#include "Visdef.h"

namespace magics {

class MultiVisdef: public Visdef {

public:
	MultiVisdef();
	virtual ~MultiVisdef();
	vector<Visdef*>* oneDimension() { return &one_d_; }
	vector<Visdef*>* twoDimension() { return &two_d_; }

	virtual void operator()(Data&, BasicGraphicsObjectContainer&);
		virtual void visit(LegendVisitor&);
		virtual void visit(Data&, LegendVisitor& legend);
		virtual void visit(Data&, HistoVisitor&);
	    virtual void visit(MetaDataVisitor&);
	    virtual void visit(TopAxisVisitor&);
	    virtual void visit(Transformation&, Data&);
	    virtual void visit(Layer& layer);
	    virtual void beanInfo(IntervalMap<Colour>&);
	    bool needLegend();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 vector<Visdef*> one_d_;
	 vector<Visdef*> two_d_;


private:
    //! Copy constructor - No copy allowed
	MultiVisdef(const MultiVisdef&);
    //! Overloaded << operator to copy - No copy allowed
	MultiVisdef& operator=(const MultiVisdef&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MultiVisdef& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
