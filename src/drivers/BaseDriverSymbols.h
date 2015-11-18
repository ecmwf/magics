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

/*! \file BaseDriverSymbols.h
    \brief Implementation of Symbol methods of driver base class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2005

*/

using namespace magics;

#include <Symbol.h>
#include <expat.h>


/////////////////////////////////////////////////////
//
//  READING XML
//
/////////////////////////////////////////////////////

/*!
 callback for expat
*/
static void XMLCALL startElement(void *userData, const char *name, const char **atts)
{
	vSymbols *s = (vSymbols*) userData;
	
	if(string(name)=="g") {svgSymbol sym;sym.id=*(atts+1); s->push_back(sym);return;}
	if(s->empty()) return;

	svgSymbol& sym = s->back();
	xmlElement ele;
	ele.name = name;
	while (*atts)
	{
		ele.attributes[*(atts)] = *(atts+1);
            	atts+=2;
	}
	sym.elements.push_back(ele);
}

/*!
 callback for expat
*/
static void XMLCALL endElement(void *, const char *){}


MAGICS_NO_EXPORT bool BaseDriver::checkDistanceMoreThan(const PaperPoint *pp, double distance) const
{
  for( vector<const PaperPoint *>::const_iterator iter = vecPoints_.begin(); iter != vecPoints_.end(); ++iter )
  {
    const double d = sqrt((pp->x()-(*iter)->x())*(pp->x()-(*iter)->x()) + (pp->y()-(*iter)->y())*(pp->y()-(*iter)->y()));
    if(d < distance)
        return false;
  }
  return true;
}

MAGICS_NO_EXPORT void BaseDriver::loadSymbols() const
{	
	if(sym_.empty())
	{
		string filename = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "symbols.svg";
		XML_Parser parser = XML_ParserCreate(NULL);
		XML_SetUserData(parser, &sym_);
		XML_SetElementHandler(parser, startElement, endElement);

		FILE* in  = fopen(filename.c_str(), "r");
		if(!in)
		{
			MagLog::error() << "BaseDriver::loadSymbols() -> Symbol file " << filename
			             << "could NOT been found! NO symbols will be plotted!" << endl;
			return;
		}
	
		int done = 0;
		char buf[BUFSIZ];
		do
		{
			size_t len = fread(buf, 1, sizeof(buf), in);
			done = len < sizeof(buf);
			if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR)
			{
				MagLog::error() << "BaseDriver::loadSymbols()-> XML ERROR: "
				             << XML_ErrorString(XML_GetErrorCode(parser))
				             << " at line  " << XML_GetCurrentLineNumber(parser) << endl;
			}
		} while (!done);
		XML_ParserFree(parser);
		fclose(in);

	}// endif of reading symbols.svg
}

MAGICS_NO_EXPORT void BaseDriver::renderTextSymbols(const TextSymbol& symbol) const
{
	const int nPos  = symbol.size();
	const int nText = symbol.text().size();

	ASSERT(nText >= nPos);
	if(nText < nPos) MagLog::debug() << " BaseDriverSymbols > Not enough texts ("<<nText<<") for "<<nPos<<" symbols!";

	//Loop for all positions
	for(int i=0;i< nPos ;i++)
	{
		Text text;
		text.setJustification(MCENTRE);
		text.setVerticalAlign(MHALF);
		text.setBlanking(symbol.blanking());

		MFloat shiftX = 0.;
		MFloat shiftY = 0.;

		// default is M_NONE
		if( symbol.position() == Symbol::M_NONE )
		{
//			std::cout<<"M_NONE"<< std::endl;
		}
		else if( symbol.position() == Symbol::M_RIGHT )
		{
			shiftX = convertCM(symbol.getHeight()*.5)/coordRatioX_;
			text.setJustification(MLEFT);
		}
		else if( symbol.position() == Symbol::M_LEFT )
		{
			shiftX = -convertCM(symbol.getHeight()*.5)/coordRatioX_;
			text.setJustification(MRIGHT);
		}
		else if( symbol.position() == Symbol::M_BELOW )
		{
			shiftY = setY(-convertCM(symbol.getHeight()*.6)/coordRatioY_);
			text.setVerticalAlign(MTOP);
		}
		else if( symbol.position() == Symbol::M_ABOVE )
		{
			shiftY = setY(convertCM(symbol.getHeight()*0.8)/coordRatioY_);
			text.setVerticalAlign(MBOTTOM);
		}

// MagLog::dev()<<"x: " << symbol[i].x()<<"  y: "<<symbol[i].y()<<"+"<< shiftY<< endl;
		text.push_back(PaperPoint(symbol[i].x()+shiftX, symbol[i].y()+setFlagY(shiftY) ));
		text.setAngle(0.);
		text.addText(symbol.text().at(i), symbol.font()); //75 dpi
		renderText(text);
	}
	renderSymbols(symbol);
}


MAGICS_NO_EXPORT void BaseDriver::renderComplexSymbols(const ComplexSymbol& symbol) const
{
	obs_distance_ = convertCM(symbol.getDistanceApart()) / coordRatioX_;

	if((obs_distance_>0.) && !checkDistanceMoreThan(&symbol[0],obs_distance_))
	{
		return;
	}

	vecPoints_.push_back(&symbol[0]);
	for (vector<GraphicsItem*>::const_iterator item = symbol.itemBegin(); item != symbol.itemEnd(); ++item)
		(*item)->redisplay(symbol, *this);
}

MAGICS_NO_EXPORT void BaseDriver::renderTextItem(const TextItem& textItem, const ComplexSymbol& owner) const
{
	Text text;
	const MFloat scaling = convertCM(owner.getHeight()*1.5);
	const MFloat pX = 1. / coordRatioX_;
	const MFloat pY = 1. / coordRatioY_;

//cout <<">>>>>>>>>> "<<(*text.textBegin()).text()<<" -> "<<owner[0].x() <<" "<<owner[0].y()<< endl;
	const MFloat posX = owner[0].x() + (           textItem.x()  * scaling *pX);
	const MFloat posY = owner[0].y() + (setSymbolY(textItem.y()) * scaling *pY);
	text.push_back(PaperPoint(posX, posY));
	text.setJustification(MCENTRE);
//
//! \todo We need to decide if we need to align text in WMO symbols
//
//	if(owner[0].x() > 0) text.setJustification(MRIGHT);
//	else if(owner[0].x() < 0) text.setJustification(MLEFT);

	text.setVerticalAlign(MHALF);
	text.setAngle(0.);
	text.addText(textItem.text(), textItem.font());
	renderText(text);
}

MAGICS_NO_EXPORT void BaseDriver::renderFlagItem(const FlagItem& flagItem, const ComplexSymbol& owner) const
{
	Flag flag;
	flag.setLength(flagItem.length());
	flag.setOriginMarker(flagItem.getOriginMarker());
	flag.setOriginHeight(flagItem.getOriginHeight());           // NOT flagItem.getOriginHeight() !!!
	flag.setColour(flagItem.getColour());

	flag.setHemisphere(flagItem.getHemisphere());

	const MFloat scaling = convertCM(owner.getHeight());
	const MFloat pX = 1. / coordRatioX_;
	const MFloat pY = 1. / coordRatioY_;

	const MFloat posX = owner[0].x() + (           flagItem.x()  * scaling *pX);
	const MFloat posY = owner[0].y() + (setSymbolY(flagItem.y()) * scaling *pY);
	ArrowPoint a(PaperPoint(posX, posY));
	a.set(flagItem.speed(),flagItem.direction());
	flag.push_back(a);

	renderWindFlag(flag);
}

MAGICS_NO_EXPORT void BaseDriver::renderSymbolItem(const SymbolItem& symbol, const ComplexSymbol& owner) const
{
	setNewColour(symbol.colour());
	setLineParameters(M_SOLID, 1.); // reset line
	loadSymbols();
	if(sym_.empty()) {MagLog::error() << "BaseDriver::renderSymbols("<<symbol.symbol()<<")-> NO symbols available!"<< std::endl; return;}

	const int noSymbols = sym_.size();
	svgSymbol sym;
	int ii=0;
	for(;ii<noSymbols;ii++)
	{
		sym = sym_[ii];
		if(sym.id==symbol.symbol()) break;
	}
	if(ii==noSymbols)
	{
	  sym = sym_[0];
	  MagLog::warning() << "BaseDriver::renderSymbols("<<symbol.symbol()<<")-> symbol NOT available! use question mark."<< std::endl;
	}

	const unsigned int si = sym.elements.size();
	const MFloat pX = 1. / coordRatioX_;
	const MFloat pY = 1. / coordRatioY_;
	
	MFloat scaling = convertCM(owner.getHeight()*1.5);  // convertCM scales them up!

	const MFloat posX = owner[0].x() + (           symbol.x()  * scaling * pX);
	const MFloat posY = owner[0].y() + (setSymbolY(symbol.y()) * scaling * pY);

	for(unsigned int i=0;i<si;i++)  // for all elements in the symbol description
	{
		if(sym.elements[i].name == "circle")
		{
			const MFloat r  = atof(sym.elements[i].attributes["r"].c_str())  * scaling;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			const int s = atoi(sym.elements[i].attributes["fill"].c_str());
			circle(posX+cx,posY+cy,r,s);
		}
		else if(sym.elements[i].name == "snowflake")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str())   * scaling * pX;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			snowflake(posX+cx,posY+cy,r);
		}
		else if(sym.elements[i].name == "drizzle")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str())   * scaling * pX;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			drizzle(posX+cx,posY+cy,r);
		}
		else if(sym.elements[i].name == "triangle")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str()) * scaling * pX;
			const int s = atoi(sym.elements[i].attributes["fill"].c_str());
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			const int li = atoi(sym.elements[i].attributes["line"].c_str());
			triangle(posX+cx,posY+cy,r,s,li);
		}
		else if(sym.elements[i].name == "lightning")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str()) * scaling * pX;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			lightning(posX+cx,posY+cy,r);
		}
		if(sym.elements[i].name == "polyline")
		{
			vector<PaperPoint> vPP;
			parsePoints(vPP, sym.elements[i].attributes["points"]);
			const int size = vPP.size();

			if(size > 0)
			{
			  const MFloat sX = posX;
			  const MFloat sY = posY;
			  for(int s=0;s<size;s++)
			  {
				vPP[s].x( (vPP[s].x() * scaling * pX) + sX);
				vPP[s].y( setSymbolY(setY((vPP[s].y()) * scaling * pY)) + sY);
			  }
			}			
				
		        if(!magCompare("none",sym.elements[i].attributes["fill"]) )
				renderSimplePolygon(vPP);
			else
			  renderPolyline(vPP);
		}
	}// endfor 
}


/*!
  \brief Symbol render method for ALL drivers.
  
  This method should be used by all Magics++ drivers to render symbols.
  
*/
MAGICS_NO_EXPORT void BaseDriver::renderSymbols(const Symbol& symbol) const
{
	debugOutput("Start Symbols");
	loadSymbols();

	// if still empty
	if(sym_.empty()) {MagLog::error() << "BaseDriver::renderSymbols("<<symbol.getSymbol()<<")-> NO symbols available!"<< std::endl; return;}

	const int noSymbols = sym_.size();
	svgSymbol sym;
	int ii=0;
	for(;ii<noSymbols;ii++)
	{
		sym = sym_[ii];
		if(sym.id==symbol.getSymbol()) break;
	}
	if(ii==noSymbols) sym = sym_[0];

	const unsigned int si = sym.elements.size();
	setNewColour(symbol.getColour());
	setLineParameters(M_SOLID, 1.); // reset line

	const MFloat pX = 1. / coordRatioX_;
	const MFloat pY = 1. / coordRatioY_;
	const MFloat scaling = convertCM(symbol.getHeight()*.5);

	for(unsigned int i=0;i<si;i++)  // for all elements in the symbol description
	{
		const long n = symbol.size();

		if(sym.elements[i].name == "circle")
		{
			const MFloat r  = atof(sym.elements[i].attributes["r"].c_str())  * scaling;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(setY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY));
			const int s = atoi(sym.elements[i].attributes["fill"].c_str());
			for(long l=0;l<n;l++)
			{
				circle(symbol[l].x()+cx,symbol[l].y()+cy,r,s);
				if(s==8 && symbol.outline())
				{
					setNewColour(symbol.outlineColour());
					setLineParameters(symbol.outlineLineStyle(),symbol.outlineThickness());
					circle(symbol[l].x()+cx,symbol[l].y()+cy,r,0);
					//We need to go back to the original values
					setNewColour(symbol.getColour());
					setLineParameters(M_SOLID, 1.);
				}
			}	
		}
		else if(sym.elements[i].name == "snowflake")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str()) * scaling * pX;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			for(long l=0;l<n;l++) snowflake(symbol[l].x()+cx,symbol[l].y()+cy,r);
		}
		else if(sym.elements[i].name == "drizzle")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str()) * scaling * pX;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			for(long l=0;l<n;l++) drizzle(symbol[l].x()+cx,symbol[l].y()+cy,r);
		}
		else if(sym.elements[i].name == "triangle")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str()) * scaling * pX;
			const int s = atoi(sym.elements[i].attributes["fill"].c_str());
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			const int li = atoi(sym.elements[i].attributes["line"].c_str());

			for(long l=0;l<n;l++)
			{
				triangle(symbol[l].x()+cx,symbol[l].y()+cy,r,s,li);
				if(s == 1 && symbol.outline())
				{
					setNewColour(symbol.outlineColour());
					setLineParameters(symbol.outlineLineStyle(),symbol.outlineThickness());
					triangle(symbol[l].x()+cx,symbol[l].y()+cy,r,0,li);
					//We need to go back to the original values
					setNewColour(symbol.getColour());
					setLineParameters(M_SOLID, 1.);
				}	
			}
		}	
		else if(sym.elements[i].name == "lightning")
		{
			const MFloat r = atof(sym.elements[i].attributes["r"].c_str()) * scaling * pX;
			const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling * pX;
			const MFloat cy = setSymbolY(atof(sym.elements[i].attributes["cy"].c_str()) * scaling * pY);
			for(long l=0;l<n;l++) lightning(symbol[l].x()+cx,symbol[l].y()+cy,r);
		}
		if(sym.elements[i].name == "polyline")
		{
			for(long l=0;l<n;l++)
			{
				vector<PaperPoint> vPP;
				parsePoints(vPP, sym.elements[i].attributes["points"]);
				const int size = vPP.size();

				if(size > 0)
				{
					const MFloat sX = symbol[l].x();
					const MFloat sY = symbol[l].y();
					for(int s=0;s<size;s++)
					{
						vPP[s].x( (vPP[s].x() * scaling * pX) + sX);
						vPP[s].y( setSymbolY(setY((vPP[s].y()) * scaling * pY)) + sY);
					}
				
					if(!magCompare("none",sym.elements[i].attributes["fill"]) )
						renderSimplePolygon(vPP);
					else
						renderPolyline(vPP);
					if(!magCompare("none",sym.elements[i].attributes["fill"]) && symbol.outline())
					{
						setNewColour(symbol.outlineColour());
						setLineParameters(symbol.outlineLineStyle(),symbol.outlineThickness());
						renderPolyline(vPP);
						//We need to go back to the original values
						setNewColour(symbol.getColour());
						setLineParameters(M_SOLID, 1.);
					}
				}
			}
		}
	}// endfor 
}//end BaseDriver::renderSymbols()

/*!
  Plotting a snowflake
*/
MAGICS_NO_EXPORT void BaseDriver::snowflake(const MFloat x, const MFloat y, const MFloat size) const
{
	const MFloat s5 = size * 1.2;
	const MFloat s3 = size * 0.75;

	vector<PaperPoint> line;
	  line.push_back(PaperPoint(x-size,y));
	  line.push_back(PaperPoint(x+size,y));
	renderPolyline2(line);
	line.clear();
	  line.push_back(PaperPoint(x-s3,y+s5));
	  line.push_back(PaperPoint(x+s3,y-s5));
	renderPolyline2(line);
	line.clear();
	  line.push_back(PaperPoint(x-s3,y-s5));
	  line.push_back(PaperPoint(x+s3,y+s5));
	renderPolyline2(line);
}

/*!
  Plotting drizzle
*/
MAGICS_NO_EXPORT void BaseDriver::drizzle(const MFloat x, const MFloat y, const MFloat size) const
{
	const MFloat s5  = size*.6;
	const MFloat s25 = size*.3;

	vector<PaperPoint> line;
	  line.push_back(PaperPoint( x+s25, y-s5) );
	  line.push_back(PaperPoint( x-s25, y-s5) );
	  line.push_back(PaperPoint( x-s5,  y-s25));
	  line.push_back(PaperPoint( x-s5,  y+s25));
	  line.push_back(PaperPoint( x-s25, y+s5) );
	  line.push_back(PaperPoint( x+s25, y+s5) );
	  line.push_back(PaperPoint( x+s5,  y+s25));
	  line.push_back(PaperPoint( x+s5,  y-s25));
	  line.push_back(PaperPoint( x+(size*.58), y-(size*.85)));
	  line.push_back(PaperPoint( x+(size*.45), y-(size*1.1)));
	  line.push_back(PaperPoint( x+(size*.1), y-(size*1.4)));
	  line.push_back(PaperPoint( x+s25, y-(size*.7)));

    renderSimplePolygon(line);
/*	circle(x,y,s2 * coordRatioX_,8);

	vector<PaperPoint> line;
	  line.push_back(PaperPoint(x+ s2      ,y));
	  line.push_back(PaperPoint(x+ s2      ,y-(s2*1.75)));
	  line.push_back(PaperPoint(x          ,y-(s2*1.95)));
	  line.push_back(PaperPoint(x          ,y-(s2*1.77)));
	  line.push_back(PaperPoint(x+(s2*0.4) ,y-(s2*1.25)));
	  line.push_back(PaperPoint(x+(s2*0.4) ,y-s2));	  
	renderSimplePolygon(line);
*/
}

/*!
  Plotting lightning symbol
*/
MAGICS_NO_EXPORT void BaseDriver::lightning(const MFloat x, const MFloat y, const MFloat size) const
{
	vector<PaperPoint> line;
	  line.push_back(PaperPoint(x-(size*.3),y+(size)));
	  line.push_back(PaperPoint(x-(size*.3),y-(size)));
	renderPolyline(line);
	line.clear();
	  line.push_back(PaperPoint(x-(size*.3),y+(size)));
	  line.push_back(PaperPoint(x+(size*.3),y+(size)));
	  line.push_back(PaperPoint(x,y));
	  line.push_back(PaperPoint(x+(size*.3),y-(size)));
	renderPolyline(line);
	line.clear();
	  line.push_back(PaperPoint(x+(size*.35),y-(size*.3) ));
	  line.push_back(PaperPoint(x+(size*.3), y-(size) ));
	  line.push_back(PaperPoint(x, y-(size*.7)));
	renderPolyline(line);
}

/*!
  Plotting a triangle
*/
MAGICS_NO_EXPORT void BaseDriver::triangle(const MFloat x, const MFloat y, const MFloat size, const int fill, const int l) const
{
	const MFloat s = 0.5 * size;

	vector<PaperPoint> line;
	  line.push_back(PaperPoint( x+s, y-s) );
	  line.push_back(PaperPoint( x-s, y-s) );
	  line.push_back(PaperPoint(   x, y+size) );
	  line.push_back(PaperPoint( x+s, y-s) );
	if(fill < 1)
	{
		renderPolyline(line);
		if(l>0)
		{
			line.clear();
			line.push_back(PaperPoint( x+s*.6, y-(s*.5)) );
			line.push_back(PaperPoint( x-s*.6, y-(s*.5)) );
			renderPolyline(line);
		}
	}
	else renderSimplePolygon(line);
}
