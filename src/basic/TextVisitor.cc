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

/*! \file TextVisitor.cc
    \brief Implementation of the Template class TextVisitor.

    Magics Team - ECMWF 2007

    Started: Tue 6-Mar-2007

    Changes:

 */



#include "TextVisitor.h"
#include "Text.h"
#include "Dimension.h"
#include "XmlReader.h"
#include "AnimationRules.h"
#include "Layer.h"
#include "MagicsFormat.h"
#include "OutputHandler.h"
#include "MetaData.h"

using namespace magics;

/*!
 \brief Method to relate C++ special characters with XML ones

 \todo write one code to do so 

 \sa PostScriptDriver.cc
 */

map<string, string> TextVisitor::tags_;







TextVisitor::TextVisitor() :positional_(true)
{

	static int i = 0;
	ostringstream n;
	n << "TextVisitor" << i;
	name(n.str());

	layout_ = new Layout();
	layout_->name(n.str());
	i++;
	current_ = layout_;

	if (tags_.empty()) {
		tags_["base_date"] = "grib_info  key='start-date' format='%Y-%m-%d %H:%M:00'";
		tags_["valid_date"] = "grib_info  key='end-date' format='%A %d %B %Y at %H UTC'";
	}
}


TextVisitor::~TextVisitor() 
{
}


void TextVisitor::addToTags(const string& line, const string& entry) 
{
	add(line, new TextEntry(entry)); 
}
/*!
 Class information are given to the output-stream.
 */
void TextVisitor::print(ostream& out)  const
{
	out << "TextVisitor[";
	LayoutVisitor::print(out);
	out << "]";
}

void TextVisitor::getReady()
{	
}


void TextVisitor::visit(MetaDataVisitor& meta){
	meta.add("magics.title", "\"" + label_ + "\"");

}


void  TextVisitor::visit() {

	decode();
	clear();

	vector<double>::iterator ratio = height_ratios_.begin();
	currentTexts_.clear();

	for ( vector<string>::iterator line = lines_.begin(); line != lines_.end(); ++line) {
		double font_size = font_size_;
		if ( ratio !=  height_ratios_.end() ) {
			font_size *= *ratio;
			++ratio;
		}
		MagFont font(font_);
		font.style(font_style_);
		font.size(font_size);
		font.colour(font_colour_->name());

		TagConverter converter(*this);
		converter.font(font);
		if ( converter.staticTag(*line) ) {
			Text* text = new Text();

			converter.decode(*line, text);	
			currentTexts_[*line].push_back(text);
		}
	}


}

void TextVisitor::visit(BasicSceneObject& parent)
{

	// Setup the information the automatic title if necessary!
	decode();


	TagHandler::reset();
	bool automatic = false;
	clear();
	parent.visit(*this);

	vector<string>::iterator line = lines_.begin();
	map<string, vector<string> > lines;
	vector<string> lineslist; ;

	while ( line != lines_.end() )
	{
		lines.insert(make_pair(*line, vector<string>()));

		if ( *line != "<magics_title/>" ) {
			lines[*line].push_back(*line);
			(*this)[*line].push_back(new TextEntry(*line));
		}
		else {
			automatic = true;
			map<string, vector<TextEntry* >  >::iterator key = find(*line);
			if ( key != end() ) {
				for (vector<TextEntry* >::iterator entry = key->second.begin(); entry !=  key->second.end(); ++entry)
					lines[*line].push_back((*entry)->entry_);
			}

		}
		++line;
	}
	if ( !automatic && !texts_.empty() ) {
		// Nothing to do the text has already been created!
		return;
	}




	vector<string>::reverse_iterator lend = lines_.rend();
	vector<double>::reverse_iterator ratios = height_ratios_.rbegin();
	currentTexts_.clear();

	for (vector<string>::reverse_iterator line = lines_.rbegin(); line != lend; ++line)
	{
		double ratio =  ( ratios ==  height_ratios_.rend() ) ?  1 : *ratios;

		if ( ratios != height_ratios_.rend() ) 	++ratios;
		MagFont font(font_);
		font.style(font_style_);
		font.size(font_size_*ratio);
		font.colour(font_colour_->name());
		TagConverter converter(*this);
		converter.font(font);
		if ( converter.staticTag(*line) )
			continue;
		if ( !hasInfos() )
			continue;
		currentTexts_[*line] = vector<Text*>();
		map<string,vector<TextEntry* >  >::iterator key = find(*line);
		if ( key != end() ) {

			for (vector<TextEntry*  >::reverse_iterator entry = key->second.rbegin(); entry !=  key->second.rend(); ++entry) {
				Text* text = new Text();
				converter.decode((*entry)->entry_, text);
				if (text->noText()) {
					delete text;
					continue;
				}
				if ( currentTexts_.find(*line) == currentTexts_.end() )
					currentTexts_[*line] = vector<Text*>();
				currentTexts_[*line].push_back(text);
			}
		}


	}

}
void TextVisitor::start()
{

	texts_.clear();
}
void TextVisitor::finish(BasicGraphicsObjectContainer& parent) 
{
	// Here we adjustt the position of the text...
	newLayout();
	parent.push_back(current_);

	current_->blankIt();

	double absheight = current_->absoluteHeight();
	double abswidth = current_->absoluteWidth();
	double height = (font_size_ /absheight)*100;

	double last = orientation_ == "horizontal" ? (0.10/absheight)*100 : (0.10/abswidth)*100;//in %
	double ratio = OutputHandler::patchLineSpacing();
	double gap = height*ratio;

	double angle = 0;





	for (vector<Text*>::iterator text = texts_.begin(); text != texts_.end(); ++text)
	{

		// Calculate the position depending on the jsutification.
		double x;

		if (justification_ == MLEFT) x = .1; // 0.1% 
		else if (justification_ == MRIGHT) x = 98.; //98 %
		else x = 50.; 

		(*text)->setJustification(justification_);
		(*text)->setVerticalAlign(MBOTTOM);



		if ( orientation_ == "horizontal" ) {
			angle = 0;
			gap = (*text)->noText() ?  (font_size_ /absheight)*100 *ratio : (((*text)->getFontMax()/absheight)*100)*ratio;
			(*text)->push_back(PaperPoint(x, last)); // approximate position to be improved
		}
		if ( magCompare(orientation_, "bottom_top" ) ) {

			angle = 3*3.14/2;;
			(*text)->setVerticalAlign(MTOP);
			gap = (*text)->noText() ?  (font_size_ /abswidth)*100 *ratio : (((*text)->getFontMax()/abswidth)*100)*ratio;
			(*text)->push_back(PaperPoint(last, x)); // approximate position to be improved
		}

		if ( magCompare(orientation_, "top_bottom" ) ) {
			if (justification_ == MLEFT) x = 98.; // 0.1%
			else if (justification_ == MRIGHT) x = .1; //98 %
			else x = 50.;
			angle = 3.14/2;
			gap = (*text)->noText() ?  (font_size_ /abswidth) *ratio : (((*text)->getFontMax()/abswidth)*100)*ratio;
			(*text)->push_back(PaperPoint(last, x)); // approximate position to be improved
		}

		(*text)->setJustification(justification_);



		(*text)->setAngle(angle);
		last += gap;

		current_->push_back(*text);

	}
	current_->frameIt();

}

void TextVisitor::update(vector<Text*>& texts) 
{



	for (vector<Text*>::iterator text = texts.begin(); text != texts.end(); ++text)
	{
		texts_.push_back(*text);
	}

}


struct TextHelper : public XmlNodeVisitor, public vector<string>
{
	TextHelper() {}
	~TextHelper() {}

	vector<string>::iterator begin()
	{
		push_back(current_.str());
		return vector<string>::begin();
	}

	bool  empty()
	{
		return 	current_.str().empty();	
	}

	void visit(const XmlNode& node)
	{
		if ( magCompare(node.name(), "font") )
			current_ << node << endl;
		if ( magCompare(node.name(), "b") )
			current_ << node << endl;
		if ( magCompare(node.name(), "magics_title") )
			current_ << node << endl;
		if ( magCompare(node.name(), "grib_info") )
			current_ << node << endl;
		if ( magCompare(node.name(), "data") )
			current_ << node << endl;

		if (magCompare(node.name(), "br")) {
			push_back(current_.str());
			current_.seekp(ios_base::beg);
			for (unsigned int n = 0; n < back().size(); n++)
				current_ << " ";
			current_.seekp(ios_base::beg);
		}
	}
	ostringstream current_; 
};

void XmlTextVisitor::set(const XmlNode& node) 
{ 
	XmlNode view = node;
	view.name("view");
	XmlBasicNodeAttributes::set(view); 
	TextVisitor::set(node);
	TextHelper helper;
	node.visit(helper);

	if ( helper.empty() ) {
		vector<string> lines;

		lines.push_back(line1_);
		lines.push_back(line2_);
		lines.push_back(line3_);
		lines.push_back(line4_);
		lines.push_back(line5_);
		lines.push_back(line6_);
		lines.push_back(line7_);
		lines.push_back(line8_);
		lines.push_back(line9_);
		lines.push_back(line10_);
		for (int i = 0; i <count_; i++)
			lines_.push_back(lines[i]);
	}


}

void XmlTextVisitor::getReady()
{
	ASSERT (BasicSceneObject::parent_);

	Dimension bottom(bottom_, BasicSceneObject::parent_->absoluteWidth(), 0);
	Dimension left(left_, BasicSceneObject::parent_->absoluteHeight(), 0);
	Dimension width(XmlBasicNodeAttributes::width_, BasicSceneObject::parent_->absoluteWidth(), 100);
	Dimension height(XmlBasicNodeAttributes::height_, BasicSceneObject::parent_->absoluteHeight(), 100);

	Dimension mb(margin_bottom_, height.absolute(), 0);	
	Dimension ml(margin_left_, width.absolute(), 5);
	Dimension mr(margin_right_, width.absolute(), 5);
	Dimension mt(margin_top_, height.absolute(), 0);



	this->x(left.percent()+ml.percent());
	this->y(bottom.percent() + mb.percent());
	this->width (width.percent() - ml.percent() - mr.percent());
	this->height(height.percent() - mt.percent() - mb.percent());

	// adjust the font size!...	
	Dimension text(font_dimension_,height.absolute(), 10);
	font_size_ = text.absolute();

	layout_->display(ABSOLUTE);
	layout_->frame(TextVisitorAttributes::blanking_, TextVisitorAttributes::border_, *TextVisitorAttributes::border_colour_, M_SOLID, 1);

}

FortranTextVisitor::FortranTextVisitor()
{
}

void XmlTextVisitor::decode()
{
}


void FortranTextVisitor::decode()
{
	if (lines_.empty()) {

		stringarray lines;
		vector<double> ratios;

		interpret(line1_, lines);
		ratios.push_back(height_ratio_1_);
		interpret(line2_, lines);
		ratios.push_back(height_ratio_2_);
		interpret(line3_, lines);
		ratios.push_back(height_ratio_3_);
		interpret(line4_, lines);
		ratios.push_back(height_ratio_4_);
		interpret(line5_, lines);
		ratios.push_back(height_ratio_5_);
		interpret(line6_, lines);
		ratios.push_back(height_ratio_6_);
		interpret(line7_, lines);
		ratios.push_back(height_ratio_7_);
		interpret(line8_, lines);
		ratios.push_back(height_ratio_8_);
		interpret(line9_, lines);
		ratios.push_back(height_ratio_9_);
		interpret(line10_, lines);
		ratios.push_back(height_ratio_10_);

		if (first_ > 10) {
			MagLog::warning() << "Invalid value for text_first_line[" << first_ << "] : reset to 1 " << "\n";
			first_ = 1;
		}

		if ( count_ > 10) {
			MagLog::warning() << "Invalid value for text_count_line[" << first_ << "]  reset to  " << count_ << "\n";
			count_ = 10 - first_ + 1;
		}

		int last = first_ + count_;
		if ( last > 10) {
			count_ = 10 - first_ + 1;
			MagLog::warning() << "Invalid value (" << last << ") for text_first_line[" << first_ << "] and  text_count_line[" << count_ << "] reset\n";
			last  = 10;
		}

		for (int i = 0; i < count_; i++) {
			if (lines[first_+i-1].empty())
				lines_.push_back("<magics_title/>"); // Ask for an automatic title...
			else
				lines_.push_back(lines[first_+i-1]);
			height_ratios_.push_back(ratios[first_+i-1]);
		}
	}

}


void FortranTextVisitor::interpret(string& line, stringarray& lines)
{
	map<string, string> getString;
	map<string, int> getInt;
	map<string, double> getFloat;

	if ( getString.empty() ) {
		getString["text_character_1"] = character1_;
		getString["text_character_2"] = character2_;
		getString["text_character_3"] = character3_;
		getString["text_character_4"] = character4_;
		getString["text_character_5"] = character5_;
		getString["text_character_6"] = character6_;
		getString["text_character_7"] = character7_;
		getString["text_character_8"] = character8_;
		getString["text_character_9"] = character9_;
		getString["text_character_10"] = character10_;
	}
	if ( getFloat.empty() ) {
		getFloat["text_real_1"] = real1_;
		getFloat["text_real_2"] = real2_;
		getFloat["text_real_3"] = real3_;
		getFloat["text_real_4"] = real4_;
		getFloat["text_real_5"] = real5_;
		getFloat["text_real_6"] = real6_;
		getFloat["text_real_7"] = real7_;
		getFloat["text_real_8"] = real8_;
		getFloat["text_real_9"] = real9_;
		getFloat["text_real_10"] = real10_;
	}
	if ( getInt.empty() ) {
		getInt["text_integer_1"] = integer1_;
		getInt["text_integer_2"] = integer2_;
		getInt["text_integer_3"] = integer3_;
		getInt["text_integer_4"] = integer4_;
		getInt["text_integer_5"] = integer5_;
		getInt["text_integer_6"] = integer6_;
		getInt["text_integer_7"] = integer7_;
		getInt["text_integer_8"] = integer8_;
		getInt["text_integer_9"] = integer9_;
		getInt["text_integer_10"] = integer10_;
	}

	string key, format;
	int where;
	int howmany;

	vector<KeyInfo> keys;

	extract(line, keys);

	for (vector<KeyInfo>::iterator ikey = keys.begin(); ikey != keys.end(); ++ikey ) {
		string key = ikey->key_;
		string format = ikey->format_;

		int where = line.find(ikey->what_);

		int howmany = ikey->what_.size();


		// Try to find the key in the string keys
		map<string, string>::const_iterator text = getString.find(key);
		if ( text != getString.end() ) {
			line.replace(where, howmany, text->second);
		}


		//  Try to find the key in the integer keys and apply the Format!
		map<string, int>::const_iterator integer = getInt.find(key);
		if ( integer != getInt.end() ) {
			// Apply format
			ostringstream value;
			value << MagicsFormat(format, integer->second);
			line.replace(where, howmany, value.str());
		}

		//  Try to find the key in the real keys and apply the Format!
		//  Try to find the key in the integer keys and apply the Format!
		map<string, double>::const_iterator real = getFloat.find(key);
		if ( real != getFloat.end() ) {
			// Apply format
			ostringstream value;
			value << MagicsFormat(format, real->second);
			line.replace(where, howmany, value.str());
		}
	}

	if (html_) {
		for (map<string, string>::iterator tag = tags_.begin(); tag != tags_.end(); ++tag) {
			size_t pos = line.find(tag->first);
			if ( pos != string::npos ) 
				line.replace(pos, tag->first.length(), tag->second);
		}
	}
	lines.push_back( line );
}


FortranAutomaticTextVisitor::FortranAutomaticTextVisitor()
{
	positional_ = false;
}


void FortranAutomaticTextVisitor::getReady()
{
	decode();
	MagLog::dev() << "FortranAutomaticTextVisitor::getReady()" << endl;

	Dimension text(font_dimension_, 1, 10);
	font_size_ = text.absolute();
	layout_->frame(blanking_, border_, *border_colour_, border_line_style_, border_thickness_);
}


FortranPositionalTextVisitor::FortranPositionalTextVisitor()
{
}

void FortranPositionalTextVisitor::getReady()
{
	decode();
	positional_ = true;

	layout_->x(adjustDimension(TextVisitorAttributes::x_, 7.5, BasicPositionalObject::absoluteWidth()));
	layout_->y( adjustDimension(TextVisitorAttributes::y_, 5.,BasicPositionalObject::absoluteHeight()));
	layout_->width(adjustDimension(TextVisitorAttributes::width_, 50., BasicPositionalObject::absoluteWidth()));
	layout_->height(adjustDimension(TextVisitorAttributes::height_, 20., BasicPositionalObject::absoluteHeight()));
	Dimension text(font_dimension_, absoluteHeight(), 10);
	font_size_ = text.absolute();
	layout_->frame(TextVisitorAttributes::blanking_, TextVisitorAttributes::border_, *border_colour_, border_line_style_, border_thickness_);
}


XmlTextVisitor::XmlTextVisitor() 
{
	//displayType_ = BLOCK;
	//border_ = false;
	//blanking_= false;
	//lines_.push_back("<magics_title/>");
}

void TextVisitor::titles(vector<string>& titles)
{	
	//prepare();	
	for ( vector<string>::const_iterator line = lines_.begin(); line != lines_.end(); ++line)
		titles.push_back(*line);
}


void TextVisitor::add(const string& line, TextEntry* entry)
{
	map<string, vector<TextEntry* >  >::iterator key = find(line);
	if ( key == end() ) {
		map<string, vector<TextEntry* >  >::insert(make_pair(line, vector<TextEntry* >()) );
		key = find(line);
	}
	key->second.push_back(entry);
}


void TextVisitor::extract(const string& line, vector<KeyInfo>& keys)
{
	int count = 0;

	char escape = *(parameter_escape_.begin());
	bool special = false;

	KeyInfo key;
	key.format_= "(automatic)";


	string::const_iterator x = line.begin();
	while ( x != line.end() ) {
		if (*x == escape)  {
			if ( special ) { // end of the instruuction

				string::iterator k = key.what_.begin();
				// Try to extract the format!
				while ( k != key.what_.end() ) {

					if ( *k == '(' )  {
						key.format_.clear();
						while ( k != key.what_.end() ) {
							key.format_.push_back(*k);
							if ( *k == ')' )
								break;
							++k;
						}
					}
					else
						key.key_.push_back(*k);
					++k;
				}

				key.key_= lowerCase(key.key_);
				key.what_= escape + key.what_ + escape;
				keys.push_back(key);
				key = KeyInfo();
				key.format_= "(automatic)";
			}
			special = !special;
		}
		else {
			if ( special)
				key.what_.push_back(*x);
		}
		++x;
	}
}

void TextVisitor::addAutomaticTitle(const string& text)
{
	update("grib", "magics", text);
	add("<magics_title/>", new TextEntry(text));
}
