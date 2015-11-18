/*! \file MultiVisdef.cc
    \brief Implementation of the Template class MultiVisdef.
    
    Magics Team - ECMWF 2012
    
    Started: Wed 25-Jan-2012
    
    Changes:
    
*/



#include "MultiVisdef.h"

using namespace magics;

MultiVisdef::MultiVisdef() 
{
}


MultiVisdef::~MultiVisdef() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void MultiVisdef::print(ostream& out)  const
{
	out << "MultiVisdef[";
	out << "]";
}

void MultiVisdef::operator()(Data& data, BasicGraphicsObjectContainer& out)
{
	vector<Visdef*>& visdefs = ( data.dimension() == 1 ) ? one_d_ : two_d_;

	for (vector<Visdef*>::iterator visdef = visdefs.begin(); visdef != visdefs.end(); ++visdef)
		(**visdef)(data, out);

}

bool MultiVisdef::needLegend()
{


	for (vector<Visdef*>::iterator visdef = one_d_.begin(); visdef != one_d_.end(); ++visdef)
		if ( (*visdef)->needLegend() )
			return true;
	for (vector<Visdef*>::iterator visdef = two_d_.begin(); visdef != two_d_.end(); ++visdef)
		if ( (*visdef)->needLegend() )
			return true;
	return false;

}

void MultiVisdef::visit(LegendVisitor& legend)
{

}

void MultiVisdef::visit(Data& data, LegendVisitor& legend)
{
	vector<Visdef*>& visdefs = ( data.dimension() == 1 ) ? one_d_ : two_d_;
	for (vector<Visdef*>::iterator visdef = visdefs.begin(); visdef != visdefs.end(); ++visdef)
		(**visdef).visit(data, legend);
}

void MultiVisdef::visit(Data& data, HistoVisitor& visitor)
{
	vector<Visdef*>& visdefs = ( data.dimension() == 1 ) ? one_d_ : two_d_;

	if(visdefs.size() > 0)
		visdefs.at(0)->visit(data,visitor);	

}

void MultiVisdef::visit(MetaDataVisitor& visitor)
{

}

void MultiVisdef::visit(TopAxisVisitor& top)
{

}

void MultiVisdef::visit(Transformation&, Data&)
{
	ASSERT(false);
}

void MultiVisdef::visit(Layer& layer)
{

}

void MultiVisdef::beanInfo(IntervalMap<Colour>& bean)
{

}
