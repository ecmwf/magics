/******************************** LICENSE ********************************


  Copyright 2009 European Centre for Medium-Range Weather Forecasts (ECMWF)

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

/*! \file BaseDriverBinaryReader.cc
    \brief Implementation of methods to read the binary input
    
    Magics Team - ECMWF 2010
    
    Started: June 2010
    
    Changes:

*/
#include "Timer.h"

using namespace magics;

/*!
 \brief Method to read and execute binary file
 
 This method is implemented for performance in Metview 4 and WREP
 
*/
void BaseDriver::setDimensionsFromBinary(string mbg_tmpl,MFloat &ratio,int &width) const
{
  ifstream in(mbg_tmpl.c_str());
  if(in.is_open()) 
  {
	char mag [6];
	in.read((char *)(&mag), 6);
	if(strncmp( "MAGICS", mag,6) !=0 )
	{
		MagLog::error() << "Magics number of TEMPLATE binary file " <<mbg_tmpl<<" is wrong!\n"
			<< "   Please check endiness and/or if you gave the right filename."<< endl;
		return;
	}

	int checksum;
	in.read((char *)(&checksum), sizeof(int));
	if(checksum!=10)
	{
		MagLog::error() << "Checksum of TEMPLATE binary file " <<mbg_tmpl<<" is wrong!\n"
		       << "   Please check endiness of your file."<< endl;
		return;
	}

	int version;
	in.read((char *)(&version), sizeof(int));
	if(version!=BINARY_VERSION) MagLog::error() << " MGB VERSION MISMATCH " <<version<<" != "<<BINARY_VERSION << endl;
	int lengthHeader;
	in.read((char *)(&lengthHeader), sizeof(int));

	MFloat lx;
	MFloat ly;
	in.read((char *)(&lx), sizeof(MFloat));
	in.read((char *)(&ly), sizeof(MFloat));
	in.close();

	ratio = ly / lx;
	width = maground(lx);
  }
  else
  {
  	MagLog::error() << "TEMPLATE binary file " <<mbg_tmpl<<" could NOT be opened!"<< endl;
  }
}


/*!
 \brief Method to read and execute binary file
 
 This method is implemented for performance in Metview 4 and WREP
 
*/
void BaseDriver::redisplay(const BinaryObject& binary) const 
{
  Timer timer("binary", " read");
  const string mbg_tmpl = binary.getPath();
  ifstream in(mbg_tmpl.c_str());
  if(in.is_open()) 
  {
    const float alpha = binary.getTransparency();
    char mag [6];
    in.read((char *)(&mag), 6);
    if(strncmp( "MAGICS", mag,6) !=0 )
    {
	  MagLog::error() << "Magics number of binary file " <<mbg_tmpl.c_str()<<" is wrong!\n"
			<< "   Please check endiness and if you gave the right filename."<< endl;
	  return;
    }

    int checksum;
  in.read((char *)(&checksum), sizeof(int));
  if(checksum!=10)
  {
    MagLog::error() << "Checksum of binary file " <<mbg_tmpl.c_str()<<" is wrong!\n"
                    << "   Please check endiness of your file."<< endl;
    return;
  }

  string old_currentLayer_ = currentLayer_;
  currentLayer_ = mbg_tmpl;

  Layer lay;
  lay.name(mbg_tmpl);
  newLayer(lay);

  int version;
  in.read((char *)(&version), sizeof(int));

  int lengthHeader;
  in.read((char *)(&lengthHeader), sizeof(int));
  
  
  MFloat lx;
  MFloat ly;
  in.read((char *)(&lx), sizeof(MFloat));
  in.read((char *)(&ly), sizeof(MFloat));
  
//  if(lx!=0 || ly != 0) MagLog::warning()<<"Reading Binary: dimension mismatch!!!"<<endl;
  Layout la;
   la.x(binary.getMgb_x());
   la.y(binary.getMgb_y());
   la.width(binary.getMgb_width());
   la.height(binary.getMgb_height());
   la.minX(0);
   la.minY(0);
   la.maxX(100);
   la.maxY(100);
  project(la);

  char c;
  while( in.get(c) )
  {
	int n;
	switch(c) {
	case 'T':
		MFloat x;
		MFloat y;
		MFloat s;
		int len, noNT;
		int size;
		{ 
			in.read((char *)(&size), sizeof(int));
			MFloat r,g,b;
			in.read((char *)(&r), sizeof(MFloat));
			in.read((char *)(&g), sizeof(MFloat));
			in.read((char *)(&b), sizeof(MFloat));

			MFloat angle;
			in.read((char *)(&angle), sizeof(MFloat));
			bool bl;
			in.read((char *)(&bl), sizeof(bool));
			enum Justification horizontal;
			enum VerticalAlign vertical;
			in.read((char *)(&horizontal), sizeof(enum Justification));
			in.read((char *)(&vertical), sizeof(enum VerticalAlign));

			in.read((char *)(&noNT), sizeof(int));

			Text text;
			text.setBlanking(bl);
			text.setJustification(horizontal);
			text.setVerticalAlign(vertical);
			text.setAngle(angle);

			for(int ntc=0;ntc<noNT;ntc++)
			{
			  in.read((char *)(&r), sizeof(MFloat));
			  in.read((char *)(&g), sizeof(MFloat));
			  in.read((char *)(&b), sizeof(MFloat));
			  in.read((char *)(&s), sizeof(MFloat));
			  in.read((char *)(&len), sizeof(int));
			  char *tex = new char[len+1];
			  in.read(tex, sizeof(char)*len);
			  tex[len]='\0';
			  string str(tex);
			  text.addText(str,Colour(r,g,b,alpha),s);
			  delete [] tex;
			}

			for(int n=0;n<size;n++)
			{
				in.read((char *)(&x), sizeof(MFloat));
				in.read((char *)(&y), sizeof(MFloat));
				PaperPoint pp(x,y);
				text.push_back(pp);
			}
			renderText(text);
		}
        	break;

	case 'A':
        	in.read((char *)(&n), sizeof(int));
        	{
			Arrow arrow;
			double sc;
			in.read((char *)(&sc), sizeof(double));
			arrow.setScale(sc);
			int index;
			in.read((char *)(&index), sizeof(int));
			arrow.setHeadIndex(index);
			LineStyle ls;
			in.read((char *)(&ls), sizeof(LineStyle));
			arrow.setStyle(ls);
			ArrowPosition ap;
			in.read((char *)(&ap), sizeof(ArrowPosition));
			arrow.setArrowPosition(ap);
			int hi;
			in.read((char *)(&hi), sizeof(int));
			arrow.setHeadIndex(hi);
			double hr;
			in.read((char *)(&hr), sizeof(double));
			arrow.setHeadRatio(hr);
			MFloat r,g,b;
			in.read((char *)(&r), sizeof(MFloat));
			in.read((char *)(&g), sizeof(MFloat));
			in.read((char *)(&b), sizeof(MFloat));
			setNewColour(Colour(r,g,b,alpha));
			arrow.setColour(Colour(r,g,b,alpha));

			for(int pts=0;pts<n;pts++)
			{
			  double x,y;
			  in.read((char *)(&x), sizeof(double));
			  in.read((char *)(&y), sizeof(double));
			  PaperPoint p;
			  in.read((char *)(&p), sizeof(PaperPoint));
			  ArrowPoint ap(x, y, p);
			  arrow.push_back(ap);
			} 
			renderWindArrow(arrow);
		}
		break;
	case 'F':
        	in.read((char *)(&n), sizeof(int));
        	{
			Flag flag;
			double ll;
			in.read((char *)(&ll), sizeof(double));
			flag.setLength(ll);
			LineStyle ls;
			in.read((char *)(&ls), sizeof(LineStyle));
			flag.setStyle(ls);
			FlagConvention fc;
			in.read((char *)(&fc), sizeof(FlagConvention));
			flag.setConvention(fc);
			Hemisphere he;
			in.read((char *)(&he), sizeof(Hemisphere));
			flag.setHemisphere(he);
			double hr;
			in.read((char *)(&hr), sizeof(double));
			flag.setThickness(hr);
			double hi;
			in.read((char *)(&hi), sizeof(double));
			flag.setOriginHeight(hi);

			MFloat r,g,b;
			in.read((char *)(&r), sizeof(MFloat));
			in.read((char *)(&g), sizeof(MFloat));
			in.read((char *)(&b), sizeof(MFloat));
			setNewColour(Colour(r,g,b,alpha));
			flag.setColour(Colour(r,g,b,alpha));

			int len;
			in.read((char *)(&len), sizeof(int));
			char *tex = new char[len+1];
			in.read(tex, sizeof(char)*len);
			tex[len]='\0';
			string str(tex);
			flag.setOriginMarker(str);

			for(int pts=0;pts<n;pts++)
			{
			  double x,y;
			  in.read((char *)(&x), sizeof(double));
			  in.read((char *)(&y), sizeof(double));
			  PaperPoint p;
			  in.read((char *)(&p), sizeof(PaperPoint));
			  ArrowPoint ap(x, y, p);
			  flag.push_back(ap);
			} 
			renderWindFlag(flag);
		}
		break;
	case 'H':
		in.read((char *)(&n), sizeof(int));
		{
			MFloat *x = new MFloat[n];
			MFloat *y = new MFloat[n];
			in.read((char *)(x), sizeof(MFloat)*n);
			in.read((char *)(y), sizeof(MFloat)*n);
			renderPolyline( n, x, y);
			delete [] x;
			delete [] y;
		}
		break;

	case 'I':
		{
			MFloat x0=0.;
			MFloat x1=0.;
			MFloat y0=0.;
			MFloat y1 = 0.;
			int he=0;
			int wi=0;
			int si=0;
			double red=0.;double green=0.;double blue=0.;double alpha=0.;

			in.read((char *)(&wi), sizeof(int));
			in.read((char *)(&he), sizeof(int));
			in.read((char *)(&x0), sizeof(MFloat));
			in.read((char *)(&y0), sizeof(MFloat));
			in.read((char *)(&x1), sizeof(MFloat));
			in.read((char *)(&y1), sizeof(MFloat));
			const int d=wi*he;

			in.read((char *)(&si), sizeof(int));
			ColourTable table;

//cout << "Image>  pixel: "<<wi<<"x"<<he<<"="<<d<<" orig: "<<x0<<"/"<<y0<<"    dim: "<<x1<<"/"<<y1<<"     noColours: "<<si<< endl;

			for(int v=0;v<si;v++)
			{
			  in.read((char *)(&red  ), sizeof(double));
			  in.read((char *)(&green), sizeof(double));
			  in.read((char *)(&blue ), sizeof(double));
			  in.read((char *)(&alpha), sizeof(double));
			  table.push_back(ColourTableEntry(Colour(red,green,blue,alpha)));
			}

			short cc[d];
			in.read((char *)(cc), sizeof(short)*d);

			Image object;
			PaperPoint pp(x0,y0,0.);
			object.setOrigin(pp);
			object.setWidth(x1);
			object.setHeight(y1);
			object.set(he,wi);
			for(int i=0;i<d;i++) object.push_back(cc[i]);
			object.setColourTable(table);
			renderCellArray(object);
		}
		break;

	case 'B':
		in.read((char *)(&n), sizeof(int));
		{
			MFloat *x = new MFloat[n];
			MFloat *y = new MFloat[n];
			in.read((char *)(x), sizeof(MFloat)*n);
			in.read((char *)(y), sizeof(MFloat)*n);
			renderPolyline2( n, x, y);
			delete [] x;
			delete [] y;
		}
		break;

	case 'R':
		MFloat cx,cy,cr;
		int cs;
		in.read((char *)(&cx), sizeof(MFloat));
		in.read((char *)(&cy), sizeof(MFloat));
		in.read((char *)(&cr), sizeof(MFloat));
		in.read((char *)(&cs), sizeof(int));
		circle( cx, cy, cr, cs);
		break;

	case 'X':
		in.read((char *)(&n), sizeof(int));
		{
			MFloat *x = new MFloat[n];
			MFloat *y = new MFloat[n];
			in.read((char *)(x), sizeof(MFloat)*n);
			in.read((char *)(y), sizeof(MFloat)*n);
			Polyline line;
			for(int i=0;i<n;i++)
			{
				line.push_back(PaperPoint(x[i],y[i]));
			}
			unsigned int nh = 0;
			in.read((char *)(&nh), sizeof(unsigned int));
			if(nh!=0)
			{
			 for (unsigned int h=0; h < nh; ++h)
			 {
			  unsigned int nhx = 0;
			  in.read((char *)(&nhx), sizeof(unsigned int));
			  MFloat *xx = new MFloat[nhx];
			  MFloat *yy = new MFloat[nhx];
			  in.read((char *)(xx), sizeof(MFloat)*nhx);
			  in.read((char *)(yy), sizeof(MFloat)*nhx);
			  line.newHole();
			  for (unsigned int u=0; u < nhx; ++u)
			  {
			    line.push_back_hole( PaperPoint(xx[u],yy[u]) );
			  }
			 }
			}
			line.setFillColour(Colour("green"));
			FillShadingProperties* shading = new FillShadingProperties();    
			line.setShading(shading);
			MFloat r,g,b,a;
			in.read((char *)(&r), sizeof(MFloat));
			in.read((char *)(&g), sizeof(MFloat));
			in.read((char *)(&b), sizeof(MFloat));
			in.read((char *)(&a), sizeof(MFloat));
			line.setFillColour(Colour(r,g,b,a*alpha));
			renderSimplePolygon(line);
			delete [] x;
			delete [] y;
		}
	break;
	case 'S':
		in.read((char *)(&n), sizeof(int));
        	{
			MFloat *x = new MFloat[n];
			MFloat *y = new MFloat[n];
			in.read((char *)(x), sizeof(MFloat)*n);
			in.read((char *)(y), sizeof(MFloat)*n);
			renderSimplePolygon( n, x, y);
			delete [] x;
			delete [] y;
		}
		break;

	case 'W':
		MFloat width;
		in.read((char *)(&width), sizeof(MFloat));
		setNewLineWidth(width);
		break;

	case 'L':
		MFloat w;
		LineStyle linestyle;
		in.read((char *)(&linestyle), sizeof(LineStyle));
		in.read((char *)(&w), sizeof(MFloat));
		setLineParameters(linestyle, w);
		break;

	case 'C':
		MFloat r,g,b,a;
		in.read((char *)(&r), sizeof(MFloat));
		in.read((char *)(&g), sizeof(MFloat));
		in.read((char *)(&b), sizeof(MFloat));
		in.read((char *)(&a), sizeof(MFloat));
        	setNewColour(Colour(r,g,b,a*alpha));
        	break;

	case 'N':
        	startPage();
        	break;

	case 'E':
        	endPage();
        	break;

	case 'P':
		{
			Layout l;
			double x,y,w,h,minx,miny,maxx,maxy;
			in.read((char *)(&x), sizeof(double));
			in.read((char *)(&y), sizeof(double));
			in.read((char *)(&w), sizeof(double));
			in.read((char *)(&h), sizeof(double));
			in.read((char *)(&minx), sizeof(double));
			in.read((char *)(&miny), sizeof(double));
			in.read((char *)(&maxx), sizeof(double));
			in.read((char *)(&maxy), sizeof(double));
			l.x(x);
			l.y(y);
			l.width(w);
			l.height(h);
			l.minX(minx);
			l.minY(miny);
			l.maxX(maxx);
			l.maxY(maxy);
			project(l);
		}
        	break;
	case 'U':
        	unproject();
        	break;
	}
   }
   unproject();
   in.close();
   closeLayer(lay);
   currentLayer_ = old_currentLayer_;
  }
  else
  {
  	MagLog::error() << "TEMPLATE binary file " <<mbg_tmpl<<" could NOT be opened!"<< endl;
  }
}
