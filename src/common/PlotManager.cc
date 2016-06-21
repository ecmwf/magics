/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
