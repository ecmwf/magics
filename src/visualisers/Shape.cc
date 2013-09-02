/*! \file Shape.cc
    \brief Implementation of the Template class Shape.
    
    Magics Team - ECMWF 2010
    
    Started: Fri 12-Mar-2010
    
    Changes:
    
*/



#include "Shape.h"

using namespace magics;

Shape::Shape()  : colour_("none"), 
	minx_(numeric_limits<double>::max()), 
	maxx_(-numeric_limits<double>::max()), 
	miny_(numeric_limits<double>::max()), 
	maxy_(-numeric_limits<double>::max()) 
{}

Shape::Shape(const Colour& colour, double val) : 
				value_(val),
				colour_(colour), 
				minx_(numeric_limits<double>::max()), 
				maxx_(-numeric_limits<double>::max()), 
				miny_(numeric_limits<double>::max()), 
				maxy_(-numeric_limits<double>::max())
{
}

void Shape::push_back(double x, double y) 
{
	    if ( empty() ) {
	    	list<pair<double, double> >::push_back(make_pair(x, y));
	    	minx_ = x;
	    	miny_ = y;
	    	maxx_ = x;
	    	maxy_ = y;
	    	return;
	    }	    
		pair<double, double>& last = back();				
		if ( last.first == x && last.second ==  y	) 
				return; // we do not need to add the point!
		
		list<pair<double, double> >::push_back(make_pair(x, y));		
			
		if ( x < minx_ ) minx_ = x;
		if ( x > maxx_ ) maxx_ = x;
		if ( y < miny_ ) miny_ = y;
		if ( y > maxy_ ) maxy_ = y;
}

void Shape::reduce() {
		 list<pair<double, double>  >points;			    			   
		stack<pair<double, double> > helper;
		list<pair<double, double>  >::const_iterator point = begin();
				    			points.push_back(*point);
				    			while ( point != end() ) {
				    					 if (  *point != points.back()  ) {
				    							points.push_back(*point);
				    					 }
				    					 ++point;
				               }
				    			pair<double, double> top;
				    		   for (point = points.begin();  point != points.end(); ++point) {
				    						if ( helper.empty() ) {
				    							     helper.push(*point);
				    							     continue;
				    						}
				    							                           
				    						top =  helper.top();
				    						helper.pop();
				    						if ( helper.empty() ) {	
				    							        helper.push(top);
				    							        if ( top != *point ) { 				                        		   
				    							                  helper.push(*point);
				    							         }
				    					    }
				    						else if ( !( *point == helper.top()) ) {
				    							        helper.push(top);
				    							        helper.push(*point);
				    					    }
				    		      }

				    			 		
				    			 	   clean();
				    			 		// rebuild!
				    			 	   //cout << "reduced----------------->" << endl;
				    			 		while ( !helper.empty() ) {
				    			 			//cout << "[" << helper.top().first << ", " <<  helper.top().second << "]" << endl;
				    			 			push_back(helper.top().first, helper.top().second);
				    			 			helper.pop();
				    			 		}
				    			 		//cout << "<-----------------" << endl;
		
}


void Shape::simplify(iterator from)
{
	  iterator last = from;
	  iterator a1,b1, a2,b2;
	  a1 = from;
	  b1 = next(a1);
	  a2 = next(b1);
	  b2 = next(a2);

		    while( a1 != last ){
		        if ( *a1 == *a2 && *b1 == *b2) {
		                last = previous(a1);
		        		erase(a1);
		                erase(a2);
		                erase(b1);
		                erase(b2);
		                simplify(last);
		                return;
		        }
		        a1 = next(a1);
		        b1 = next(a1);
		      	a2 = next(b1);
		      	b2 = next(a2);
		    }
	
}
void Shape::intersection(Shape& other) {
		

		std::reverse(other.begin(),other.end());
		list<pair<double, double>  >::iterator p1= begin();
		
	

		while (p1!= end() ) {
				list<pair<double, double>  >::iterator p2= other.begin();
		        while ( p2!= other.end()) {
		        	if ( *p1 == *p2) {
			    			 std::rotate(other.begin(), p2, other.end());
			    			 p1++;
			    			 
			    		     insert(p1, other.begin(), other.end());
			    			 
			    			//now we need to take off the duplicates! 
			    		  		   
			    		     stack<pair<double, double> > helper;
			    		     list<pair<double, double>  >::const_iterator point = begin();
			    		     	   
			    		     pair<double, double> top;
			    		     				    		   for (point = begin();  point != end(); ++point) {
			    		     				    						if ( helper.empty() ) {
			    		     				    							     helper.push(*point);
			    		     				    							     continue;
			    		     				    						}
			    		     				    							                           
			    		     				    						top =  helper.top();
			    		     				    						helper.pop();
			    		     				    						
			    		     				    							        if ( top != *point ) {
			    		     				    							        		  helper.push(top);
			    		     				    							                  helper.push(*point);
			    		     				    							         }
			    		     				    					
			    		     				    		      }

			    		     
			    		     
			    		    

			    		                                        clean();//cout << "new shape ----------------->" << endl;
			    		                                         // rebuild!
			    		                                      
			    		                                        				    			 	  
			    		                                        				    			 		while ( !helper.empty() ) {
			    		                                        				    			 			//cout << "[" << setprecision(4) << helper.top().first << ", " <<  helper.top().second << "]" << endl;
			    		                                        				    			 			push_back(helper.top().first, helper.top().second);
			    		                                        				    			 			helper.pop();
			    		                                        				    			 		}
			    		                                        				    			 		//cout << "<-----------------" << endl;
			    							  			     			                       
			    		                                         
			    			 		
			    			 return;
		        	}	
		        	p2++;
		        }
				p1++;				
		}

		
	}
Shape::iterator Shape::previous(Shape::iterator x)
{
    iterator p = x;
    if ( p == begin()) {
        p = end();
        --p;
    }
    else --p;
    return p;

}

Shape::iterator Shape::next(Shape::iterator x)
{
    iterator n = x;
    if ( n == end()) {
        n = begin();
    }
    else {
        n++;
        if ( n == end() )
             n = begin();
    }
    return n;

}


// Here if we find to equal adjacent point we get rid of one of them...
 void  Shape::reduce(iterator from)
{    	
	  iterator last = from;
	  iterator n = next( from);
	  iterator nn, cc;
	  iterator c = from;

	    while( n != last ){
	        if ( *c == *n ) {

	            cc = previous(c);
	            nn = next(n);
	            if ( *cc == *nn ) {
	                erase(c);
	                erase(n);
	                c = cc;
	            }
	            else {
	                erase(n);

	                }
	            reduce( c);
	            return;

	        }
	        c = next(c);
	        n = next( c);
	    }

}
 
 
/*
 Class information are given to the output-stream.
*/
void Shape::print(ostream& out)  const
{
	out << "Shape[";
	out << "]";
}



 
 bool Shape::concatenate(Shape& other) 
 {
 	    if( !intersect(other) ) 
 	    	return false;
 	    	    
 	    iterator where;
 	    iterator from;
 	   iterator next1;
 	  iterator next2;
 	    for ( iterator line1= begin(); line1 != end(); ++line1) {
 	    	 for ( iterator line2= other.begin(); line2 != other.end(); ++line2) {
 	    		 next1 = next(line1);
 	    		 next2 = next(line2);
 	    		 if ( *line2 == *line1 && *next2 == *next1) {
 	    			 where = line1;
 	    			 from = line2;
 	    			 ++from;
 	    			 std::rotate(other.begin(), from, other.end());
 	    			 
 					  splice(where, other);
 					 
 					    // Remove common lines!
 					 simplify(where);
 					  
 					  if ( minx_ > other.minx_ ) minx_ = other.minx_;
 					  if ( maxx_ < other.maxx_ ) maxx_ = other.maxx_;
 					  if ( miny_ > other.miny_ ) miny_ = other.miny_;
 					  if ( maxy_ < other.maxy_ ) maxy_ = other.maxy_;
 					 
 					  return true;
 	    		 }
 	    	 }
 		}
 	    
 	    return false;
 	  
 	}