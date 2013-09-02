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
 \author <a href="http://ootips.org/yonat/4dev/">Yonat Sharon</a>

 * VectorOfPointers - auto-cleaning container of pointers
 *
 * Example usage:
 * {
 *     VectorOfPointers< std::vector<int*> > v;
 *     // v can be manipulated like any std::vector<int*>.
 *
 *     v.push_back(new int(42));
 *     v.push_back(new int(17));
 *     // v now owns the allocated int-s
 *
 *     v.erase(v.begin());
 *     // frees the memory allocated for the int 42, and then removes the
 *     // first element of v.
 * }
 * // v's destructor is called, and it frees the memory allocated for
 * // the int 17.
 *
 * Notes:
 * 1. Assumes all elements are unique (you don't have two elements
 *    pointing to the same object, otherwise you might delete it twice).
 * 2. Not usable with pair associative containers (map and multimap).
 * 3. For ANSI-challenged compilers, you may want to #define
 *    NO_MEMBER_TEMPLATES.
 *
 * Written 10-Jan-1999 by Yonat Sharon <yonat@ootips.org>
 * Last updated 07-Feb-1999
 */

#ifndef VectorOfPointers_H
#define VectorOfPointers_H

template <typename TContainer>
class VectorOfPointers : public TContainer
{
public:
	typedef VectorOfPointers<TContainer> its_type;

	VectorOfPointers(){}
	VectorOfPointers(const TContainer& c):TContainer(c) {}
	its_type& operator=(const TContainer& c)
		{TContainer::operator=(c); return *this;}
	~VectorOfPointers()
		{clean_all();}
        void simple_clear(){ TContainer::clear();} //Do nor delete the pointers!
	void clear()
		{clean_all(); TContainer::clear();}
	typename TContainer::iterator erase(typename TContainer::iterator i) 
		{clean(i); return TContainer::erase(i);}
	typename TContainer::iterator erase(typename TContainer::iterator f, typename TContainer::iterator l)
		{clean(f,l); return TContainer::erase(f,l);}

	//! for associative containers: erase() a value
	typename TContainer::size_type erase(const typename TContainer::value_type& v)
	{
		typename TContainer::iterator i = find(v);
		typename TContainer::size_type found(i != this->end()); // can't have more than 1
		if (found) erase(i);
		return found;
	}

	//! for sequence containers: pop_front(), pop_back(), resize() and assign()
	void pop_front() {clean(this->begin()); TContainer::pop_front();}
	void pop_back()  {typename TContainer::iterator i(this->end()); clean(--i); TContainer::pop_back();}

	void resize(typename TContainer::size_type s, typename TContainer::value_type c = typename TContainer::value_type())
	{
		if (s < this->size()) clean(this->begin()+s, this->end());
	typename TContainer::resize(s, c);
	}
	
	template <class InIter> void assign(InIter f, InIter l)
	{
		clean_all();
		TContainer::assign(f,l);
	}
	
	template <class Size, class T> void assign(Size n, const T& t = T())
	{
		clean_all();
		TContainer::assign(n,t);
	}
    
	//! for std::list: remove() and remove_if()
	void remove(const typename TContainer::value_type& v)
	{
		clean( std::find(this->begin(), this->end(), v) );
		TContainer::remove(v);
	}

	template <class Pred>
	void remove_if(Pred pr)
	{
		for (typename TContainer::iterator i = this->begin(); i != this->end(); ++i)
		if (pr(*i)) clean(i);
		TContainer::remove_if(pr);
	}

private:
	void clean(typename TContainer::iterator i)
		{delete *i;}
	void clean(typename TContainer::iterator f, typename TContainer::iterator l)
		{while (f != l) clean(f++);}
	void clean_all()
		{clean( this->begin(), this->end() );}

	//! we can't have two VectorOfPointerss own the same objects:
	VectorOfPointers(const its_type&) {}
	its_type& operator=(const its_type&) {}
};

#endif // VectorOfPointers_H
