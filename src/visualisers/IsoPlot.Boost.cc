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

/*! \file IsoPlot.cc  
 \brief Implementation of the Template class IsoPlot.
 
 Magics Team - ECMWF 2004 
 
 Started: Wed 3-Mar-2004
  
 Changes:
 
 
 CONREC
 ======

 http://astronomy.swin.edu.au/~pbourke/projection/conrec/

 Copyright (c) 1996-1997 Nicholas Yue

 This software is copyrighted by Nicholas Yue. This code is base on the work of
 Paul D. Bourke CONREC.F routine

 The authors hereby grant permission to use, copy, and distribute this
 software and its documentation for any purpose, provided that existing
 copyright notices are retained in all copies and that this notice is included
 verbatim in any distributions. Additionally, the authors grant permission to
 modify this software and its documentation for any purpose, provided that
 such modifications are not distributed without the explicit consent of the
 authors and that existing copyright notices are retained in all copies. Some
 of the algorithms implemented by this software are patented, observe all
 applicable patent law.

 IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR
 DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY DERIVATIVES THEREOF,
 EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING,
 BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE IS PROVIDED ON AN
 "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO OBLIGATION TO PROVIDE
 MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 
 */
#include "IsoPlot.h"
#include "Factory.h"
#include "MatrixHandler.h"
#include "Timer.h"
#include "UserPoint.h"
#include "IsoHighlight.h"
#include "Colour.h"
#include "AutoLock.h"
#include "ThreadControler.h"
#include "LegendVisitor.h"

static  MutexCond producerMutex_;
template<class P> IsoPlot<P>::IsoPlot() {
	setTag("isoline");
}

template<class P> IsoPlot<P>::~IsoPlot() {
}
 
/*!
 Class information are given to the output-stream.
 */
template<class P> void IsoPlot<P>::print(ostream& out) const {
	out << "IsoPlot<P>[";
	IsoPlotAttributes<P>::print(out);
	out << "]";
}





namespace magics {

template <class P> 
class CellBox : public VectorOfPointers<vector<CellBox<P>* > >
{
public:
	CellBox(const CellArray<P>* parent, int row1, int row2, int column1, int column2) :
		parent_(parent), row1_(row1), row2_(row2), column1_(column1),
				column2_(column2) {
			shape_ = new Polyline();
	}

	CellBox(const CellArray<P>* parent) :
		parent_(parent), row1_(0), row2_(parent->rows_-1), column1_(0),
				column2_(parent->columns_-1) {
		shape_ = new Polyline();
	}
	CellBox() :
			parent_(0), row1_(0), row2_(0), column1_(0),
					column2_(0) {
			shape_ = new Polyline();
		}
	virtual ~CellBox() { }
   
	virtual double value () {
		return (*parent_)(row1_, column1_)->value(0);
		
	}
	
	virtual RangeType range() {
		
		
		// First try to finfd iif the cell is outOfRange...
		bool out = true;
		for (int row = row1_; row <= row2_; row++) {
			for (int column = column1_; column <= column2_; column++) {
				if 	((*parent_)(row, column)->range() != outOfRange) {
					out = false;
					break;
				}
			}
		}
		if ( out )
			return outOfRange;
		int min = 10000000;
		int max = -min;
		for (int row = row1_; row <= row2_; row++) {
					for (int column = column1_; column <= column2_; column++) {
						Cell<P>* cell = (*parent_)(row, column);
						RangeType range   = cell->range();
						if ( range == outOfRange)
							continue;
						if (range == multipleRange)
							return multipleRange;
						min = ( min < cell->min_ ) ? min : cell->min_;
						max = ( max > cell->max_ ) ? max : cell->max_;
						if (max-min > 0 ) 
							return multipleRange; 
					}
		}
		return  ( max-min == 0 ) ?  singleRange : multipleRange; 
		
		
	}


	void reshape(Polyline* cell) {
		Colour colour = cell->cellColour();
		if ( colour.name() == "none" ) return;
		map<Colour, list<Polyline*> >::iterator entry = this->helper_.find(colour);
		if (entry == helper_.end() ) {
			helper_.insert(make_pair(colour, list<Polyline*>()));
			entry = helper_.find(colour);
			entry->second.push_back(cell);
			return;
		}

		 
		list<Polyline*>::iterator last = entry->second.end();
		
		for (list<Polyline*>::iterator shape = entry->second.begin(); shape != last; ++shape) {
			if ( (*shape)->concatenate(*cell) )
				return;
		}
		
		entry->second.push_back(cell);
			
	}
	
	

	virtual void reshape(CellBox<P>* parent) {
		if ( parent == this) return;
		for (map<Colour, list<Polyline*> >::iterator entry = helper_.begin(); entry != helper_.end(); ++entry) {			        
					for (list<Polyline*>::iterator shape = entry->second.begin(); shape != entry->second.end(); ++shape) {					
							parent->reshape(*shape);					
				}
		}
		helper_.clear();
	}

	
	virtual void split();
	

	virtual Polyline* shape(const Colour& colour) {
		if (shape_->empty()) {
			// bottom 
			
			
			Cell<P>* cell;
    
			
			
			for (int column = column1_; column <= column2_; column++) {
				cell = (*parent_)(row1_, column);
			    shape_->push_back(cell->column(0), cell->row(0));
			}

			// right
			for (int row = row1_; row <= row2_; row++) {
				cell = (*parent_)(row, column2_);
				shape_->push_back(cell->column(1), cell->row(1));
			}

			for (int column = column2_; column >= column1_; column--) {
				cell = (*parent_)(row2_, column);
				shape_->push_back(cell->column(2), cell->row(2));
			}

			for (int row = row2_; row >= row1_; row--) {
				cell = (*parent_)(row, column1_);	
				shape_->push_back(cell->column(3), cell->row(3));
			}
            
            
		}
		
		
	
		shape_->cellColour(colour);

		return shape_;

	}
    
	
	void shade(const IsoPlot<P>& owner);
	
	void shade(const IsoPlot<P>& owner, CellBox* parent) {
		RangeType def = this->range();
		switch (def) {
		    	case outOfRange : 
		    		break;
		    	case singleRange: 
		    		this->shape_->cellValue(this->value());
		    		reshape(this->shape(owner.colour(this->value())));		
		    		reshape(parent);
		    		break;
		    	default:
		    	
		
				
				split();		
				if (this->empty()) { 
				    assert( row1_ == row2_);
				    assert( column1_ == column2_);
				 
				   
					owner.isoline(*(*parent_)(row1_, column1_), this);
				}
				else {
					
					for (typename CellBox<P>::iterator cell = this->begin(); cell != this->end(); ++cell) 
						(*cell)->shade(owner, this);
				}
				reshape(parent);
		}
		
	}
	
	void contour(const IsoPlot<P>& owner) {
		   for (int row = row1_; row <= row2_; row++) {
						for (int column = column1_; column <= column2_; column++) {													
									owner.isoline(*(*parent_)(row, column));
						}
			}			
	}
	
	

	void feed(IsoPlot<P>& owner, BasicGraphicsObjectContainer& out) {
		
		
		
		for ( map<Colour, list<Polyline*> >::iterator colour = helper_.begin(); colour!= helper_.end(); ++colour) {
			
			//reduce(colour->second);
			
			for (list<Polyline*>::const_iterator cells = colour->second.begin(); cells != colour->second.end(); ++cells) {
					(*cells)->setFilled(true); 
	    			(*cells)->setStroke(false);
	    			(*cells)->setColour((*cells)->cellColour());
	    			(*cells)->setFillColour((*cells)->cellColour());
	    			(*cells)->setColour(Colour("black"));
    				(*cells)->setStroke(true);
    				(*cells)->setThickness(2);
	    			out.push_back(*cells);
	   			
	    			if ( !(*cells)->empty() ) {
	    				owner.getShading()((*cells));
	    				(*cells)->setColour(Colour("black"));
	    				(*cells)->setStroke(true);
	    				(*cells)->setThickness(2);	    				  
	    			}
	    	}
		}
	}
	
	Polyline* shape_;
	const CellArray<P>* parent_;
	int row1_;
	int row2_;
	int column1_;
	int column2_;
	map<Colour, list<Polyline* > > helper_;

};



}



template <class P>
void CellBox<P>::shade(const IsoPlot<P>& owner) {
	    shade(owner, this);			    
}
template <class P>
void CellBox<P>::split() {
		
		if ( row1_ == row2_ && column1_ ==  column2_ ) 
			return; 
		int row = (row2_+ row1_) /2;
		int column = (column2_+ column1_)/2;
		if (row2_- row1_ > 1&& column2_- column1_ > 1) {
			//try first 2 split in columns ...
			CellBox<P>* cell = new CellBox(parent_, row1_, row2_, column1_, column);
			RangeType def = cell->range();
			
			if ( def != multipleRange ) {
				push_back(cell);
			}

			else {
				delete cell;
				push_back(new CellBox(parent_, row1_, row, column1_, column));
				push_back(new CellBox(parent_, row+1, row2_, column1_, column));
			}
			cell = new CellBox(parent_, row1_, row2_, column+1, column2_);
			def = cell->range();
						
						if ( def != multipleRange ) {
							push_back(cell);
						}
						else {
				delete cell;
				push_back(new CellBox(parent_, row1_, row, column+1, column2_));
				push_back(new CellBox(parent_, row+1, row2_, column+1, column2_));
			}
			return;
		}
		if (row2_- row1_ > 0) {
			push_back(new CellBox(parent_, row1_, row, column1_, column2_));
			push_back(new CellBox(parent_, row+1, row2_, column1_, column2_));
			return;
		}
		if (column2_- column1_ > 0) {
			push_back(new CellBox(parent_, row1_, row2_, column1_, column));
			push_back(new CellBox(parent_, row1_, row2_, column+1, column2_));
			return;
		}
		
	    
		
		
		

	}




template <class P>
struct IsoProducerData {
public:
		IsoProducerData(bool shading, IsoPlot<P>& parent, CellBox<P>& cell):
			shading_(shading), parent_(parent), cell_(cell) { more_ = true; }  
		bool shading_;
		IsoPlot<P>& parent_;
		CellBox<P>& cell_;
		
	    bool more_;
	    MutexCond cond_;
};

template <class P>
class IsoProducer: public Thread {

public:	
	IsoProducer(int n, IsoProducerData<P>& data) : n_(n), objects_(data) {}
	void run()
	{  Timer timer("cell", "shading");
		( objects_.shading_ ) ? objects_.cell_.shade(objects_.parent_) : objects_.cell_.contour(objects_.parent_);
	}
	
    virtual ~IsoProducer() {}
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const {}
    
	int      n_;
	 IsoProducerData<P>&  objects_;



private:
    //! Copy constructor - No copy allowed
	IsoProducer(const IsoProducer&);
    //! Overloaded << operator to copy - No copy allowed
	IsoProducer& operator=(const IsoProducer&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const IsoProducer& p)
		{ p.print(s); return s; }

};


template<class P> 
void IsoPlot<P>::isoline(Cell<P>& cell, CellBox<P>* box) const
{
	static int castab[3][3][3] = { { { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 } }, {
			{ 9, 10, 11 }, { 12, 13, 14 }, { 15, 16, 17 } }, { { 18, 19, 20 },
			{ 21, 22, 23 }, { 24, 25, 0 } } };
	int m1, m2, m3, case_value;
	double x1=0, x2=0, y1=0, y2=0;

	RangeType def = cell.range();
	
	if (def == outOfRange) 
		return;
	if ( def == singleRange )
	{
		if ( !box  ) // NO Shading== Nothing to do! 
			return;
		else {
			double contour=levels_[cell.min_];
			Polyline* x = new Polyline();;
			for (int i = 0; i < 4; i++) {
					x->push_back(cell.column(i), cell.row(i));			
			}
			x->cellValue ((cell.value(0) > contour) ? this->shading_->rightRange(contour) : this->shading_->leftRange(contour));
			x->cellColour((cell.value(0) > contour) ? this->shading_->rightColour(contour) : this->shading_->leftColour(contour));
			box->reshape(x);
			return;
		}
	}







//=================================================================
	//
	// Note: at this stage the relative heights of the corners and the
	// centre are in the h array, and the corresponding coordinates are
	// in the xh and yh arrays. The centre of the box is indexed by 0
	// and the 4 corners by 1 to 4 as shown below.
	// Each triangle is then indexed by the parameter m, and the 3
	// vertices of each triangle are indexed by parameters m1,m2,and m3.
	// It is assumed that the centre of the box is always vertex 2
	// though this is important only when all 3 vertices lie exactly on
	// the same contour level, in which case only the side of the box
	// is drawn.
	//
	//      vertex 4 +-------------------+ vertex 3
	//               | \                 |
	//               |   \               |
	//               |     \         m=1 |
	//               |       \           |
	//               |         \         |      
	//               |           \       |
	//               |             \     |
	//               |    m=0        \   |
	//               |                 \ |
	//      vertex 1 +-------------------+ vertex 2
	//
	//               Scan each triangle in the box
	//=================================================================
	for (int m=0; m<2; m++)
	{
		m1 = m;
		m2 = m1+1;
		m3 = 3;
		
		
		//int count = 0;
		if (cell.missing(m1)) continue;
		if (cell.missing(m2)) continue;
		if (cell.missing(m3)) continue;
		
		
		
		
	
		
		vector<pair<Polyline*, Polyline*> > shapes;
	
		
		
		// Fisrt, build the list of isolines in this triangle...
		vector<int> levels;		
	
		
		
		for (int l = cell.min_; l != cell.max_; ++l) { 
		
			double contour=levels_[l];
			int out  = castab[cell.coef(m1, contour)][cell.coef(m2, contour)][cell.coef(m3, contour)];
			if ( out != 0 ) {
				levels.push_back(l);
			}		
		}
		
		if ( levels.empty() && box ) {
			Polyline* shape = new Polyline();
	
			shape->cellInfo(this->shading_->colour(cell.value(m1)), cell.value(m1));
			shape->push_back(cell.column(m1), cell.row(m1));
			shape->push_back(cell.column(m2), cell.row(m2));
			shape->push_back(cell.column(m3), cell.row(m3));
			
			box->reshape(shape);
		}
		bool complex = ( levels.size() > 1);
		for (vector<int>::const_iterator l = levels.begin(); l != levels.end(); ++l) {			   
				int level = *l;				
				// First make a quich check to see if there is at l
				
				double contour=levels_[level];
				Colour leftcolour= ( box ) ? this->shading_->leftColour(contour) : Colour("none"); 				
				Colour rightcolour  = ( box ) ? this->shading_->rightColour(contour) : Colour("none"); 
				

		case_value = castab[cell.coef(m1, contour)][cell.coef(m2, contour)][cell.coef(m3, contour)];
		
		int add = 2;
		
		Polyline* leftcell = new Polyline();
		leftcell->cellInfo(leftcolour, this->shading_->leftRange(contour));
		Polyline* rightcell= new Polyline();
		rightcell->cellInfo(rightcolour,  this->shading_->rightRange(contour));
		
	
		
		

		switch (case_value) {
		//===========================================================
		//     Case 0 -point out!
		//===========================================================

		case 0:
			add = 0;
			break;
			if ( !box ) break;
			
			if (cell.height(m1, contour) < 0) {
				leftcell->push_back(cell.column(m1), cell.row(m1));
				leftcell->push_back(cell.column(m2), cell.row(m2));
				leftcell->push_back(cell.column(m3), cell.row(m3));
			}
			else {
				rightcell->push_back(cell.column(m1), cell.row(m1));
				rightcell->push_back(cell.column(m2), cell.row(m2));
				rightcell->push_back(cell.column(m3), cell.row(m3));
			}
		
			break;
			//===========================================================
			//     Case 1 -Single point 3
			//===========================================================

		case 1:
			
			x1=cell.column(m3);
			y1=cell.row(m3);
			add = 1;
			if ( !box) break;
			if (cell.height(m1, contour) < 0) {
				leftcell->push_back(cell.column(m1), cell.row(m1));
				leftcell->push_back(cell.column(m2), cell.row(m2));
				leftcell->push_back(cell.column(m3), cell.row(m3));
			}
			else {
				rightcell->push_back(cell.column(m1), cell.row(m1));
				rightcell->push_back(cell.column(m2), cell.row(m2));
				rightcell->push_back(cell.column(m3), cell.row(m3));
			}
			
			break;
			//===========================================================
			//     Case 2 - Line between sides 2-3 and 3-1
			//===========================================================
		case 2:
			

			cell.xysect(m2,m3, contour, x1, y1);
			cell.xysect(m3,m1, contour, x2, y2); 
		
			if ( !box ) break;
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m1), cell.row(m1));
			leftcell->push_back(cell.column(m2), cell.row(m2));
			
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m3), cell.row(m3));
			rightcell->push_back(x2, y2);

			
			break;
			//===========================================================
			//     Case 3 -Single point 2
			//===========================================================
		case 3:
			
			x1=cell.column(m2);
			y1=cell.row(m2);
			add = 1;
			if ( !box) break;
			
			if (cell.height(m1, contour) < 0) {
							leftcell->push_back(cell.column(m1), cell.row(m1));
							leftcell->push_back(cell.column(m2), cell.row(m2));
							leftcell->push_back(cell.column(m3), cell.row(m3));
						}
						else {
							rightcell->push_back(cell.column(m1), cell.row(m1));
							rightcell->push_back(cell.column(m2), cell.row(m2));
							rightcell->push_back(cell.column(m3), cell.row(m3));
						}
					
			break;
			//===========================================================
			//     Case 4 - Line between vertex 2 and vertex 3
			//===========================================================
		case 4:
			
			x1=cell.column(m2);
			y1=cell.row(m2);
			x2=cell.column(m3);
			y2=cell.row(m3);
			if ( !box ) break;
			
							leftcell->push_back(cell.column(m1), cell.row(m1));
							leftcell->push_back(cell.column(m2), cell.row(m2));
							leftcell->push_back(cell.column(m3), cell.row(m3));
				
		

			break;
			//===========================================================
			//     Case 5 - Line between vertex 2 and side 3-1
			//===========================================================
		case 5:
			
			x1=cell.column(m2);
			y1=cell.row(m2);
			
			cell.xysect(m3,m1, contour,x2, y2);
			if ( !box ) break;
			leftcell->push_back(cell.column(m1), cell.row(m1));
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m3), cell.row(m3));
			rightcell->push_back(x2, y2);

			
			break;
			//===========================================================
			//     Case 6 - Line between side 1-2  and side 2.3
			//===========================================================
		case 6:
			
			cell.xysect(m1,m2, contour, x1, y1);
			cell.xysect(m2,m3, contour, x2, y2);
	
			if ( !box ) break;
			leftcell->push_back(cell.column(m1), cell.row(m1));
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m3), cell.row(m3));
			
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(x2, y2);
			
			break;
			//===========================================================
			//     Case 7 - Line between sides 1-2 aanve vertex 3
			//===========================================================
		case 7:
			
			cell.xysect(m1,m2, contour, x1, y1);
			x2=cell.column(m3);
			y2=cell.row(m3);
			
			if ( !box ) break;
			leftcell->push_back(cell.column(m1), cell.row(m1));
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);

			
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(x2, y2);
			
			break;
			//===========================================================
			//     Case 8 - Line between sides 1-2 and 3-1
			//===========================================================
		case 8:
			
			cell.xysect(m1,m2, contour, x1, y1);
			cell.xysect(m3,m1, contour, x2, y2);
			
			if ( !box ) break;
			leftcell->push_back(cell.column(m1), cell.row(m1));
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);

		
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(cell.column(m3), cell.row(m3));
			rightcell->push_back(x2, y2);
			
			break;
			//===========================================================
			//     Case 9 -single point 1
			//===========================================================
		case 9:
			
			x1=cell.column(m1);
			y1=cell.row(m1);
			add = 1;
			if ( !box) break;
			if (cell.height(m2, contour) < 0) {
							leftcell->push_back(cell.column(m1), cell.row(m1));
							leftcell->push_back(cell.column(m2), cell.row(m2));
							leftcell->push_back(cell.column(m3), cell.row(m3));
						}
						else {
							rightcell->push_back(cell.column(m1), cell.row(m1));
							rightcell->push_back(cell.column(m2), cell.row(m2));
							rightcell->push_back(cell.column(m3), cell.row(m3));
						}
			break;
			//===========================================================
			//     Case 10 - Line between vertex 3 and vertex 1
			//===========================================================
		case 10:
			
			x1=cell.column(m3);
			y1=cell.row(m3);
			x2=cell.column(m1);
			y2=cell.row(m1);
			if ( !box) break;

			
										leftcell->push_back(cell.column(m1), cell.row(m1));
										leftcell->push_back(cell.column(m2), cell.row(m2));
										leftcell->push_back(cell.column(m3), cell.row(m3));
									
		
			break;
			//===========================================================
			//     Case 11 - Line between side 3-2 and vertex 1
			//===========================================================
		case 11:
			
		
			cell.xysect(m3,m2, contour, x1, y1);
			x2=cell.column(m1);
			y2=cell.row(m1);
			if ( !box ) break;
			leftcell->push_back(cell.column(m1), cell.row(m1));
			leftcell->push_back(cell.column(m2), cell.row(m2));
			leftcell->push_back(x1, y1);
			
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m3), cell.row(m3));
			rightcell->push_back(x2, y2);
		
			break;
			//===========================================================
			//     Case 12 - Line between vertex 1 and vertex 2
			//===========================================================
		case 12:
			
			x1=cell.column(m1);
			y1=cell.row(m1);
			x2=cell.column(m2);
			y2=cell.row(m2);
			if ( !box ) break;
		
										leftcell->push_back(cell.column(m1), cell.row(m1));
										leftcell->push_back(cell.column(m2), cell.row(m2));
										leftcell->push_back(cell.column(m3), cell.row(m3));
							
		
			break;
			//===========================================================
			//     Case 13 - Flat Area all vertex have the isoline value!
			//===========================================================
		case 13:
			add = 0;
			if ( !box ) break;
			if (cell.height(m1, contour) < 0) {
										leftcell->push_back(cell.column(m1), cell.row(m1));
										leftcell->push_back(cell.column(m2), cell.row(m2));
										leftcell->push_back(cell.column(m3), cell.row(m3));
									}
									else {
										rightcell->push_back(cell.column(m1), cell.row(m1));
										rightcell->push_back(cell.column(m2), cell.row(m2));
										rightcell->push_back(cell.column(m3), cell.row(m3));
									}
		
			break;
			//===========================================================
			//     Case 14 - Line between vertex 2 and vertex 1
			//===========================================================
		case 14:
		
			x1=cell.column(m2);
			y1=cell.row(m2);
			x2=cell.column(m1);
			y2=cell.row(m1);
			if ( !box ) break;
			
										rightcell->push_back(cell.column(m1), cell.row(m1));
										rightcell->push_back(cell.column(m2), cell.row(m2));
										rightcell->push_back(cell.column(m3), cell.row(m3));
					
			
			break;
			//===========================================================
			//     Case 15 - Line between vertex 1 and side3-2
			//===========================================================
		case 15:
		
			x1=cell.column(m1);
			y1=cell.row(m1);
			
			cell.xysect(m3,m2, contour, x2, y2);
			
			
			if ( !box ) break;
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m3), cell.row(m3));
			
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(x2, y2);
			
			break;
			//===========================================================
			//     Case 16 - Line between vertex 1 and vertex 3
			//===========================================================
		case 16:
			
			x1=cell.column(m1);
			y1=cell.row(m1);
			x2=cell.column(m3);
			y2=cell.row(m3);
			if ( !box ) break;
			rightcell->push_back(x1, y1);
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(x2, y2);
			
			
		
			break;
			//===========================================================
			//     Case 17 - single point 1
			//===========================================================
		case 17:
			
			x1=cell.column(m1);
			y1=cell.row(m1);
			add = 1;
			if ( !box ) break;
			if (cell.height(m2, contour) < 0) {
										leftcell->push_back(cell.column(m1), cell.row(m1));
										leftcell->push_back(cell.column(m2), cell.row(m2));
										leftcell->push_back(cell.column(m3), cell.row(m3));
									}
									else {
										rightcell->push_back(cell.column(m1), cell.row(m1));
										rightcell->push_back(cell.column(m2), cell.row(m2));
										rightcell->push_back(cell.column(m3), cell.row(m3));
									}
			
			break;
			//===========================================================
			//     Case 18 - Line between side3-1 and side1-2
			//===========================================================
		case 18:
		
			cell.xysect(m3,m1, contour, x1, y1);
		
			cell.xysect(m1,m2, contour, x2, y2);
			
			if ( !box ) break;
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m2), cell.row(m2));
			leftcell->push_back(cell.column(m3), cell.row(m3));
		
			rightcell->push_back(x1, y1);

			rightcell->push_back(cell.column(m1), cell.row(m1));
			rightcell->push_back(x2, y2);
		
			break;
			//===========================================================
			//     Case 19 - Line between vertex 3 and side1-2
			//===========================================================
		case 19:
			
			x1=cell.column(m3);
			y1=cell.row(m3);
		
			cell.xysect(m1,m2, contour, x2, y2);
			
			if ( !box ) break;
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m2), cell.row(m2));
		
			rightcell->push_back(x1, y1);

			rightcell->push_back(cell.column(m1), cell.row(m1));
			rightcell->push_back(x2, y2);
			
			break;

			//===========================================================
			//     Case 20 - Line between side3-2 and side1-2
			//===========================================================
		case 20:
		
			cell.xysect(m3,m2, contour, x1, y1);
		
			cell.xysect(m1,m2, contour, x2, y2);
			
			if ( !box ) break;
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m2), cell.row(m2));
		
			rightcell->push_back(x1, y1);

			rightcell->push_back(cell.column(m3), cell.row(m3));
			rightcell->push_back(cell.column(m1), cell.row(m1));
			rightcell->push_back(x2, y2);
			
			break;
			//===========================================================
			//     Case 21 - Line between side 3-1 and vertex 2 
			//===========================================================
		case 21:
			
			
			cell.xysect(m3,m1, contour, x1, y1);
			
			x2=cell.column(m2);
			y2=cell.row(m2);
			if ( !box ) break;
			
				leftcell->push_back(x1, y1);
				leftcell->push_back(x2, y2);
				leftcell->push_back(cell.column(m3), cell.row(m3));
				
				rightcell->push_back(x1, y1);
				rightcell->push_back(cell.column(m1), cell.row(m1));
				rightcell->push_back(cell.column(m2), cell.row(m2));
				
			
			
			break;
			//===========================================================
			//     Case 22 - Line between vertex 3 and vertex 2
			//===========================================================
		case 22:
			
			x1=cell.column(m3);
			y1=cell.row(m3);
			x2=cell.column(m2);
			y2=cell.row(m2);
			if ( !box) break;
			rightcell->push_back(cell.column(m1), cell.row(m1));
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(cell.column(m3), cell.row(m3));
			
			
			break;
			//===========================================================
			//     Case 23 - single point 2
			//===========================================================
		case 23:
			
			x1=cell.column(m2);
			y1=cell.row(m2);
			add = 1;
			if ( !box) break;
			if (cell.height(m1, contour) < 0) {
										leftcell->push_back(cell.column(m1), cell.row(m1));
										leftcell->push_back(cell.column(m2), cell.row(m2));
										leftcell->push_back(cell.column(m3), cell.row(m3));
									}
									else {
										rightcell->push_back(cell.column(m1), cell.row(m1));
										rightcell->push_back(cell.column(m2), cell.row(m2));
										rightcell->push_back(cell.column(m3), cell.row(m3));
									}
			break;
			//===========================================================
			//     Case 24 - Line between side1-3  and side3-2
			//===========================================================
		case 24:
			
			cell.xysect(m1,m3, contour, x1, y1);
			cell.xysect(m3,m2, contour, x2, y2);
			if ( !box) break;
			leftcell->push_back(x1, y1);
			leftcell->push_back(x2, y2);
			leftcell->push_back(cell.column(m3), cell.row(m3));
			
			rightcell->push_back(x1, y1);

			rightcell->push_back(cell.column(m1), cell.row(m1));
			rightcell->push_back(cell.column(m2), cell.row(m2));
			rightcell->push_back(x2, y2);
			
			break;
			//===========================================================
			//     Case 25 - single point C
			//===========================================================
		case 25:
			
			x1=cell.column(m3);
			y1=cell.row(m3);
			add = 1;
			if ( !box) break;
			
			 
			if (cell.height(m1, contour) < 0) {
											leftcell->push_back(cell.column(m1), cell.row(m1));
											leftcell->push_back(cell.column(m2), cell.row(m2));
											leftcell->push_back(cell.column(m3), cell.row(m3));
										}
										else {
											rightcell->push_back(cell.column(m1), cell.row(m1));
											rightcell->push_back(cell.column(m2), cell.row(m2));
											rightcell->push_back(cell.column(m3), cell.row(m3));
										}
			
			
			break;
			
		default:
			break;
		}

		
		if (add == 2 && needIsolines() ) {
		
			// here we have 2 Points to add! 
							// We send it to a thread! 
					int t = (*l) % threads_;
						{     
							AutoLock<MutexCond> lockproducer(producerMutex_);
							{
								
								AutoLock<MutexCond> lock(segments_[t]->cond_);
								segments_[t]->segments_.push_back(make_pair(levels_[*l], make_pair(make_pair(
										x1, y1), make_pair(x2, y2))));
								//cout << t <<  "=" << levels_[*l] << "-->[" << x1 << ", " << y1 << "]->" <<  "[" << x2 << ", " << y2 << "]" << endl;
								if ( segments_[t]->segments_.size() >= 2000 ) 
									segments_[t]->cond_.signal();
							}
							producerMutex_.signal();
							}
						
			
			}
			
		    if ( !box) continue;
		    
		    if ( !complex ) {
		    			box->reshape(leftcell);
		    			box->reshape(rightcell);
		    		}
		    else {
		    		
		    		shapes.push_back(make_pair(leftcell, rightcell)); 
		    	
		    	
		    }
				    
		} // end of levels...
		
		//
		if ( shapes.empty() ) continue;
		
		// Now we reduce the shape! 
		vector<pair<Polyline*, Polyline*> >::iterator current = shapes.begin();	
		vector<pair<Polyline*, Polyline*> >::iterator next = shapes.begin();
		
		box->reshape(current->first);
		next++;
		
		while ( next != shapes.end() ) {
			(*current->second).intersection(*next->second);
			box->reshape(current->second);
			current++;
			next++ ;
		}

		box->reshape(current->second);
		
		
	} // next triangle
			
}

template<class P> 
void IsoPlot<P>::isoline(MatrixHandler<P>& data, BasicGraphicsObjectContainer& parent)  {

	const Transformation& transformation = parent.transformation();
		levels_.clear();
		// Find the used levels!
		
		const vector<double>::const_iterator end = (*this->levelSelection_).end();
		vector<double>::const_iterator previous = (*this->levelSelection_).end();
		vector<double>::const_iterator last = (*this->levelSelection_).end();
		
		if ( (*this->levelSelection_).front()  > data.min() )
			levels_.push_back(data.min());
		
		for (vector<double>::const_iterator l = (*this->levelSelection_).begin(); l != end; ++l) {
			if ( (*l <= data.min()) ) {
				previous = l;
				continue;
			}
			if ( (*l >= data.max()) ) {
				       last = l;
						continue;
						
			}
			if ( previous != (*this->levelSelection_).end() ) {
				 levels_.push_back(*previous);
				 previous = (*this->levelSelection_).end();
			}
			levels_.push_back(*l);
		}
		
		if ( last != (*this->levelSelection_).end())
					 levels_.push_back(*last);
		else 
			 levels_.push_back(data.max());
					
		
		
		missing_ = data.missing();
 
	//===========================================================================
	// Note that castab is arranged differently from the FORTRAN code because
	// Fortran and C/C++ arrays are transposed of each other, in this case
	// it is more tricky as castab is in 3 dimension
	//===========================================================================


	IntervalMap<int> range;
	int r= 1;
	for (vector<double>::const_iterator level = levels_.begin(); level
			!= levels_.end(); ++level) {

		
		if (level+1!= levels_.end() )
			range.insert(make_pair(Interval(*level, *(level+1)), r++));
	}
	
	CellArray<P> array(data, range, transformation, parent.widthResolution(), parent.heightResolution());

	CellBox<P> view(&array);
	
	
	threads_ =  ( needIsolines() )  ? 4: 0;

		vector<IsoHelper*> consumers_;
		vector<IsoProducer<P>* >  producers_;
	
		 
		{
		VectorOfPointers<vector<ThreadControler *>  > consumers;
		VectorOfPointers<vector<ThreadControler *>  > producers;
		segments_ .clear();
			
			colourShapes_.clear(); 
	        
			lines_.clear();
	    
	    
		for (int c = 0; c < threads_; c++) {
			vector<Polyline* >* lines = new vector<Polyline*>();
			lines_.push_back(lines);	
			segments_.push_back(new IsoData()); 
			consumers_.push_back(new IsoHelper(c, *lines,*(segments_.back())));
			consumers.push_back(new ThreadControler(consumers_.back(), false));
			consumers.back()->start();
	 
		}
		
		view.split();
		
		
		// let's start 4 producers...
				int c = 0;
				for (typename CellBox<P>::iterator cell = view.begin(); cell != view.end(); ++cell) 
				{
					     
					       IsoProducerData<P>* data = new IsoProducerData<P>(this->shading_->shadingMode(), *this, **cell);
					       producers_.push_back(new IsoProducer<P>(c, *data));
					       producers.push_back(new ThreadControler(producers_.back(), false));
					      producers.back()->start();
					
					       c++;
					    
				}
	    
	   	
				for (vector<ThreadControler *>::iterator producer = producers.begin(); 
				 			producer != producers.end(); ++producer) {
				 		(*producer)->wait();
				 	}

	 // No more
	 	{
	         
	         for (int i = 0; i < threads_; i++) {
              
	 		    AutoLock<MutexCond> lock(segments_[i]->cond_);
	 		    segments_[i]->more_ = false;
	 		    segments_[i]->cond_.signal();
	         }
	         
	 	}

	 	for (vector<ThreadControler *>::iterator consumer = consumers.begin(); 
	 			consumer != consumers.end(); ++consumer) {
	 		(*consumer)->wait();
	 	}
	 
		}
     

		for (typename CellBox<P>::iterator cell = view.begin(); cell != view.end(); ++cell) 					
					   (*cell)->feed(*this,parent);		
	//for (typename map<double, vector<Polyline* > >::iterator lines = helper_.begin(); lines != helper_.end(); ++lines)
		//for (typename vector<Polyline* >::iterator line = lines->second.begin(); line != lines->second.end(); ++line)
			//this->push_back(*line);
   
}

template<class P> bool IsoPlot<P>::prepare(MatrixHandler<P>& data) {

	(*this->levelSelection_).clear();
	(*this->levelSelection_).calculate(data.min() , data.max() , this->shading_->shadingMode());
	(*this->label_).prepare(*this->levelSelection_, (*this->colour_).name());
	return (*this->shading_)(*this->levelSelection_);
}

template<class P> void IsoPlot<P>::operator()(MatrixHandler<P>& data, BasicGraphicsObjectContainer& parent)
{
	// Get the triangles list ...
	// Create the isolines...    
	
	
	prepare(data);
	if ( this->legend_only_ ) return;
	
	{
		Timer timer("contouring", "Time spent in contouring");
		isoline(data, parent);
	}
	
	vector<Colour> colours;
	
	colours.push_back(Colour("red"));
	colours.push_back(Colour("green"));
	colours.push_back(Colour("blue"));
	colours.push_back(Colour("orange"));
	vector<Colour>::iterator colour = colours.begin();

	(*this->shading_)(data, parent);
	(*this->highlight_).prepare(*this->levelSelection_);

	// Now we feed the task...   
	for (typename vector<vector<Polyline* >* >::const_iterator lines = this->lines_.begin(); lines != this->lines_.end(); ++lines)
	{
	
	for (typename vector<Polyline* >::const_iterator poly = (*lines)->begin(); poly != (*lines)->end(); ++poly)
	{
		if ( (*poly)->empty() ) continue;
		(*poly)->setLineStyle(this->style_);
		(*poly)->setThickness(this->thickness_);
		(*poly)->setColour(*this->colour_);
		//(*poly)->setColour(*colour);
		colour++;
		if ( colour == colours.end()) 
			colour = colours.begin();
		(*this->highlight_)(*(*poly));
		(*this->label_)(**poly, (*poly)->front().value());

		parent.push_back(*poly);
		
	
	}
	
	}
	


	this->lines_.clear();
}

template<class P> void NoIsoPlot<P>::operator()(MatrixHandler<P>& data, BasicGraphicsObjectContainer& parent)
{
	// Create the isolines...        
	if ( !prepare(data) ) {	
		if ( this->legend_only_ ) return;
		(*this->shading_)(data, parent);
		// do not send the isolines...  
		return;
	}
	if ( this->legend_only_ ) return;
	// The shading needs the isolines..
	// WE will calculate them but will not send them to the printer     
	{
		Timer timer("contouring", "Time spent in contouring");
		isoline(data, parent);
	}

	(*this->shading_)(data, parent);


}

template<class P> void IsoPlot<P>::visit(LegendVisitor& legend) {
	(*this->shading_).visit(legend);
	if (this->shading_->shadingMode() ) return;
	
	
	Polyline* line1 = new Polyline();
	Polyline* line2 = 0;
	line1->setColour(*this->colour_);   
    line1->setLineStyle(this->style_);
	line1->setThickness(this->thickness_);
	
	this->highlight_->visit(line2);
	legend.add(new DoubleLineEntry(this->legend_text_, line1, line2));
	if ( legend.size() < 3) {
		   legend.add(new EmptyEntry());
		   legend.add(new EmptyEntry());
		   legend.add(new EmptyEntry());
	}
}
template<class P> void NoIsoPlot<P>::visit(LegendVisitor& legend) {
	(*this->shading_).visit(legend);
	
}
