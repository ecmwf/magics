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

/*! \file Title.cc
    \brief Implementation of the Template class Title.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 12-Feb-2004
    
    Changes:
    
*/

#include "Title.h"
#include "PaperPoint.h"
#include "Text.h"
#include "Layer.h"
#include "Layout.h"


using namespace magics;

Title::Title() 
{
}


Title::~Title() 
{
}

MagFont Title::font() const
{
	MagFont font(font_);
    font.style(style_);
    font.size(height_);
    font.colour(*colour_);
    return font;
    
}

void TitleBase::update(const string& family, const string& definition, const string& value)
{
	map<string, DefList >::iterator fam = definitions_.find(family);
	if ( fam == definitions_.end()) {
		definitions_.insert(make_pair(family, DefList()) );
		fam = definitions_.find(family);
	}
	DefList::iterator def = fam->second.find(definition);
	
	if ( def == fam->second.end() ) {
		fam->second.insert(make_pair(definition, std::set<string>()));
		def = fam->second.find(definition);
	}
	
	def->second.insert(value);
	
	
	 
}


string TitleBase::get(const string& family, const string& definition)
{
	map<string, DefList >::iterator fam = definitions_.find(family);
	
	if ( fam == definitions_.end() ) return "";
	
	DefList::iterator def = fam->second.find(definition);
	
	if ( def == fam->second.end() ) return "";
	
	string sep = " ";
	string value;
	for ( std::set<string>::const_iterator v = def->second.begin(); v != def->second.end(); ++v) {
		if ((*v).empty()) continue;
		value = sep + *v; 
		sep ="/";
	}
		
	return value;
	 
}
map<string, TitleBase::DefList > TitleBase::definitions_;

string TitleEntry::entry()
{
    // delete duplicated space characters! 
    ostringstream entry;

	bool space = true;
	for (string::iterator c = entry_.begin(); c != entry_.end(); ++c) {
		bool newspace = (*c == ' '); 
		if ( space && !newspace )  entry << *c;
		if (!space) entry << *c;
		space = (*c == ' '); 
	}
	
    return entry.str();
	

    
}

//void TitleTask::setTask(Node& task) 
//{ 
//    task.title(this); 
//}

#ifdef later

void TitleTask::prepareGraphics()
{
	
    if ( title_.empty() ) {
    	current_ = begin();
    	return;
    }
    
    Layout& layout = getLayout();
    
    
    Box& box = layout.box("title");
       MagFont font = title_.font();
    
    double height = layout.height()*(box.getHeight()/100);    
    double ch = font.size()/height*100; // working in percent 
    
    double step = (ch*1.25);
    double start = (ch*0.6)+(title_.size()-1)*step;    
    
    for (vector<TitleEntry*>::const_iterator entry = title_.begin(); entry != title_.end(); ++entry) {
        Text* text = new Text(new Layer(), "title");
        (*text).setColour(Colour(title_.colour()));
        (*text).setText((*entry)->entry());
		(*text).font(font);
       
          // Calculate the position depending on the jsutification.
        double x;
        if (title_.justification() == MLEFT) x = 0.1; // 2% 
		else if (title_.justification() == MRIGHT) x = 98; //98 %
		else x = 50; 
		(*text).setJustification(title_.justification());
        (*text).push_back(PaperPoint(x, start)); // approximate position to be improved 

        
       
        start -= step;
        push_back(text);
    }
    
    //layout["title"]->frame(*this);
   
	current_ = begin();
}
 
#endif
/*!
 Class information are given to thfe output-stream.
*/		
void Title::print(ostream& out)  const
{
	out << "Title[";
	TitleAttributes::print(out);
	out << "]";
}



