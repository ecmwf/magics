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

/*! \file Factory.cc
    \brief Definition of Parameter base class.
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/

using namespace magics;
using namespace std;   // to run on AIX

template<class B>
map<string, SimpleFactory<B>* >* SimpleFactory<B>::map_ = 0;





template<class B>
SimpleFactory<B>::SimpleFactory(const string& name): name_(lowerCase(name))
{
	if (!map_) map_ = new map<string, SimpleFactory<B>* >();
	(*map_)[name_] = this; 
}


template<class B>
SimpleFactory<B>::~SimpleFactory()
{
	if (map_) {
		delete map_;
		map_ = 0;
	}
}

template<class B>
B* SimpleFactory<B>::create(const string& name)
{
	SimpleFactory<B>* maker = get(name);
	if (maker) {
		B* object =(*maker).make();
		return object;
	}
#ifdef MAGICS_EXCEPTION
	throw NoFactoryException(name);
#else 
    cerr << "SimpleFactory: Failed to create an object named " << name << endl;
	return 0;
#endif	
} 



	
template<class B>
SimpleFactory<B>* SimpleFactory<B>::get(const string& name)
{	 
	assert(map_);
	typename map<string, SimpleFactory<B>*>::iterator maker = (*map_).find(lowerCase(name));
	if (maker != (*map_).end()) return  (*maker).second;
#ifdef MAGICS_EXCEPTION
	throw NoFactoryException(name);
#else 
    cerr << "SimpleFactory: Failed to get an object named " << name << endl;
	return 0;
#endif
}

	

