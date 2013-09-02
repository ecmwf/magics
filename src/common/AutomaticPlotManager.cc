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

/*! \file AutomaticPlotManager.cc
    \brief Implementation of the Template class AutomaticPlotManager.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Aug-2004
    
    Changes:
    
*/



#include "AutomaticPlotManager.h"

using namespace magics;



AutomaticPlotManager::AutomaticPlotManager() : 
    x_(-1), y_(-1), pageX_(-1), pageY_(-1),
    pwidth_(0), pheight_(0),
    swidth_(0), sheight_(0)
{
}


AutomaticPlotManager::~AutomaticPlotManager() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AutomaticPlotManager::print(ostream& out)  const
{
	out << "AutomaticPlotManager[";
	out << "]";
}

AutomaticPlotManager::PlotDirection AutomaticPlotManager::direction() 
{
        string val = lowerCase(plot_direction_);
		// enum PlotDirection { VERTICAL, HORIZONTAL };
        if (val  == "vertical") return VERTICAL;
		if (val  == "horizontal") return HORIZONTAL;

        return VERTICAL;
}

AutomaticPlotManager::PlotStart AutomaticPlotManager::start() 
{
        string val = lowerCase(plot_start_);
		// enum PlotStart { BOTTOM, TOP };
        if (val  == "bottom") return BOTTOM;
		if (val  == "top") return TOP;

        return BOTTOM;
}

void AutomaticPlotManager::page(MagicsManager& magics)
{
    // Does it fit in the superpge?
    MagLog::dev() << "Width = " << magics.root()->getWidth() << "\n";
    MagLog::dev() << "Height = " << magics.root()->getHeight() << "\n";
    if ( !empty() ) pop();
    page_ = true;
}
 
void AutomaticPlotManager::addpage(MagicsManager& magics)
{
    if (empty()) addRoot(magics);
    PageNode* page = new PageNode();
    page->setParent(top());
    page->setFromFortran();
    MagLog::dev() << " try to fit page[" << page->width() << ", " <<  page->height() << "]" << "\n";
    // calculate position 
    pageX_ = x_; 
    pageY_ = y_;
    if ( x_ == -1 && y_ == -1) {
        pwidth_ =  page->width();
        pheight_ = page->height();
        swidth_ =  magics.root()->getWidth();
        sheight_ = magics.root()->getHeight();
        x_ = 0;
        y_ = (start() == BOTTOM) ? 0 : sheight_ - pheight_;
    } 
    else {
        // Calculate new coordinates :
        if (direction() == VERTICAL) {
            y_ += (start() == BOTTOM) ? pheight_ : -pheight_;
            if (y_ <= 0 || y_ + pheight_ > sheight_) {// Go to next column... 
                y_ = (start() == BOTTOM) ? 0 : sheight_ - pheight_;
                x_ += pwidth_;
                if (x_ + pwidth_ > swidth_) { // go to next superpage...
                    addRoot(magics);
                    x_ = 0;
                    y_ = (start() == BOTTOM) ? 0 : sheight_ - pheight_;
                }
            }
        }
        else {
            x_ += pwidth_;
            if (x_ + pwidth_>= swidth_) { // go to next line ..
                x_ = 0;
                y_ += (start() == BOTTOM) ? pheight_ : -pheight_;
                if (y_ <= 0 || y_ + pheight_ > sheight_) {// go to next superpage...
                    addRoot(magics);
                    x_ = 0;
                    y_ = (start() == BOTTOM) ? 0 : sheight_ - pheight_;
                }
            }
        }
    }
    
   
    page->x(x_);
    page->y(y_);
   
    
    
    MagLog::dev() << " fit it at [" << x_ << ", " << y_ << "]" << "\n";
    
    top()->addChild(page);
    push(page);
    page_ = false;
    
}



void AutomaticPlotManager::subpage(MagicsManager&)
{
    if ( !empty() ) pop();
    x_ = pageX_;
    y_ = pageY_;
  
    page_ = true;
}


static SimpleObjectMaker<AutomaticPlotManager, PlotManager> automatic_plot_manager("automatic");

