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

/*! \file PlotManager.cc
    \brief Implementation of the Template class PlotManager.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Aug-2004
    
    Changes:
    
*/

#include "PlotManager.h"
#include "Page.h"


using namespace magics;

PlotManager::PlotManager() 
{
}


PlotManager::~PlotManager() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void PlotManager::print(ostream& out)  const
{
	out << "PlotManager[";
	out << "]";
}

void PlotManager::superpage(MagicsManager& magics)
{
   while (!empty() ) pop(); 
   magics.execute();
   magics.clear();
}

void PlotManager::page(MagicsManager&)
{
   pop();
   page_ = true;
}


void PlotManager::check(MagicsManager& magics)
{
    if (empty()) {
       addRoot(magics);
    }
    if (page_) addpage(magics);
   
}
void PlotManager::addpage(MagicsManager& magics)
{
    if (empty()) addRoot(magics);
    PageNode* node = new PageNode();
    
    top()->addChild(node);
    node->setFromFortran();
    push(node);   
    page_ = false;
}

void PlotManager::addRoot(MagicsManager& magics)
{
   while (!empty()) {
       pop();
   }
   
   magics.execute();
   magics.clear();
       
   
   
   BaseSceneObject* node = magics.root()->newFortranNode();

   push(node);     
   page_ = true;
}

void PlotManager::subpage(MagicsManager&)
{
	pop();
	page_ = true;
}
void PlotManager::addNode(MagicsManager& manager, BaseSceneObject* object)
{ 
	if ( empty()) addRoot(manager);
 	top()->addChild(object);
 	push(object); 
}
void PlotManager::add(BaseSceneObject* object)
{ 
 	top()->addChild(object);
}


static SimpleObjectMaker<PlotManager> plot_manager("positional");
