/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
#include <TeColorUtils.h>
#include <TeDatabase.h>
#include <TeAsciiFile.h>

bool getColors(std::vector<std::string>& ramps, int nc, std::vector<TeColor> &colors)
{
	if (ramps.empty())
		ramps.push_back("RED");

	TeColor	RGB;
	map<string, TeColor> mapcor;

	RGB.name_ = "RED";
	RGB.red_ = 240;
	RGB.green_ = 0;
	RGB.blue_ = 0;
	mapcor["RED"] = RGB;

	RGB.name_ = "GREEN";
	RGB.red_ = 0;
	RGB.green_ = 240;
	RGB.blue_ = 0;
	mapcor["GREEN"] = RGB;

	RGB.name_ = "BLUE";
	RGB.red_ = 0;
	RGB.green_ = 0;
	RGB.blue_ = 240;
	mapcor["BLUE"] = RGB;

	RGB.name_ = "YELLOW";
	RGB.red_ = 255;
	RGB.green_ = 255;
	RGB.blue_ = 100;
	mapcor["YELLOW"] = RGB;

	RGB.name_ = "CYAN";
	RGB.red_ = 100;
	RGB.green_ = 255;
	RGB.blue_ = 255;
	mapcor["CYAN"] = RGB;

	RGB.name_ = "MAGENTA";
	RGB.red_ = 255;
	RGB.green_ = 100;
	RGB.blue_ = 255;
	mapcor["MAGENTA"] = RGB;

	RGB.name_ = "ORANGE";
	RGB.red_ = 255;
	RGB.green_ = 140;
	RGB.blue_ = 0;
	mapcor["ORANGE"] = RGB;

	RGB.name_ = "GRAY";
	RGB.red_ = 240;
	RGB.green_ = 240;
	RGB.blue_ = 240;
	mapcor["GRAY"] = RGB;

	RGB.name_ = "BLACK";
	RGB.red_ = 0;
	RGB.green_ = 0;
	RGB.blue_ = 0;
	mapcor["BLACK"] = RGB;

	int	ii, jj;
	int n = 200;
	
	TeColor cfrom = mapcor[ramps[0]];
	TeColor	cto, cor;

	vector<TeColor> allColors;
	if (ramps.size() == 1)
	{
		cto.red_ = cfrom.red_ / 5;
		cto.green_ = cfrom.green_ / 5;
		cto.blue_ = cfrom.blue_ / 5;
		double dr = (double)(cto.red_ - cfrom.red_) / (double)n;
		double dg = (double)(cto.green_ - cfrom.green_) / (double)n;
		double db = (double)(cto.blue_ - cfrom.blue_) / (double)n;

		for(jj=0; jj<n; jj++)
		{
			cor.red_ = cfrom.red_ + (int)(dr * (double)jj);
			cor.green_ = cfrom.green_ + (int)(dg * (double)jj);
			cor.blue_ = cfrom.blue_ + (int)(db * (double)jj);
			allColors.push_back(cor);
		}
	}
	else
	{
		ii = 1;
		while(ii < (int)ramps.size())
		{
			cto = mapcor[ramps[ii]];

			double dr = (double)(cto.red_ - cfrom.red_) / (double)n;
			double dg = (double)(cto.green_ - cfrom.green_) / (double)n;
			double db = (double)(cto.blue_ - cfrom.blue_) / (double)n;

			for(jj=0; jj<n; jj++)
			{
				cor.red_ = cfrom.red_ + (int)(dr * (double)jj);
				cor.green_ = cfrom.green_ + (int)(dg * (double)jj);
				cor.blue_ = cfrom.blue_ + (int)(db * (double)jj);
				allColors.push_back(cor);
			}
			cfrom = cto;
			ii++;
		}
	}
	double step = 0.;
	if (nc > 1)
		step = (double)(allColors.size()) / (double)(nc-1);
	for (int i=0; i<nc; i++)
	{
		int ind = (int)(step * (double)i + .5);
		if (ind > (int)(allColors.size() - 1))
			ind = allColors.size() - 1;
		colors.push_back(allColors[ind]);
	}
	allColors.clear();
	return true;
}


void generateColorBarMap(vector<ColorBar>& inputColorVec, int ncores, map<int, vector<TeColor> >& colorMap)
{
	if(inputColorVec.empty())
	{
		TeColor c(255, 0, 0);
		ColorBar b;
		b.color(c);
//		b.distance_ = 1.;
		b.distance_ = 0.;
		inputColorVec.push_back(b);

		c.init(60, 0, 0);
		b.color(c);
		b.distance_ = 1.;
		inputColorVec.push_back(b);
	}

	int i, nc;
	if(inputColorVec.empty())
		return;

	double totalDistance = inputColorVec[inputColorVec.size()-1].distance_;
	double dd = (double)ncores / totalDistance;

	colorMap.clear();
	for(i=0; i<(int)inputColorVec.size()-1; ++i)
	{
		nc = TeRound(dd * (fabs(inputColorVec[i+1].distance_) - fabs(inputColorVec[i].distance_)));
		TeColor corFrom, corTo;
		hsv2Rgb(corFrom,inputColorVec[i].h_, inputColorVec[i].s_, inputColorVec[i].v_);
		hsv2Rgb(corTo,inputColorVec[i+1].h_, inputColorVec[i+1].s_, inputColorVec[i+1].v_);
//		TeColor corFrom = inputColorVec[i].cor_;
//		TeColor corTo = inputColorVec[i+1].cor_;
		colorMap[i] = getColors(corFrom, corTo, nc);
	}
	vector<TeColor> vv;
	colorMap[i] = vv;
}

vector<TeColor> getColors(TeColor cfrom, TeColor cto, int nc)
{
	int n = 255, i, j;
	vector<TeColor> colorVec;
	TeColor cor;

	vector<TeColor> allColors;
	double dr = (double)(cto.red_ - cfrom.red_) / (double)n;
	double dg = (double)(cto.green_ - cfrom.green_) / (double)n;
	double db = (double)(cto.blue_ - cfrom.blue_) / (double)n;

	for(j=0; j<n; j++)
	{
		cor.red_ = cfrom.red_ + (int)(dr * (double)j);
		cor.green_ = cfrom.green_ + (int)(dg * (double)j);
		cor.blue_ = cfrom.blue_ + (int)(db * (double)j);
		allColors.push_back(cor);
	}
	double step = 0.;
	if (nc > 1)
		step = (double)(allColors.size()) / (double)(nc-1);
	for (i=0; i<nc; ++i)
	{
		int ind = (int)(step * (double)i + .5);
		if (ind > (int)(allColors.size() - 1))
			ind = allColors.size() - 1;
		colorVec.push_back(allColors[ind]);
	}
	allColors.clear();
	return colorVec;
}

vector<ColorBar>  getColorBarVector(string& groupingColors, const bool& first)
{
	vector<ColorBar> cbVec;
	char	buf[1000];
	string ss;
	TeColor	cor(255, 255, 255);

	if(groupingColors.empty())
		groupingColors = "R-";
	if(groupingColors.size() == 1)
		groupingColors += "-";

	size_t t = groupingColors.find(";");
	string s = groupingColors.c_str();

	if(t == string::npos)
	{
		size_t a = s.find("-");

		double dist = -1.;
		while(a != string::npos)
		{
			memset(buf, '\0', 1000);
			s.copy(buf, a);
			ss = buf;

			a++;
			memset(buf, '\0', 1000);
			s.copy(buf, s.size()-a, a);
			s = buf;
			a = s.find("-");

			if("R" == ss)
				cor.init(240, 0, 0);
			else if("G" == ss)
				cor.init(0, 240, 0);
			else if("B" == ss)
				cor.init(0, 0, 240);
			else if("Cy" == ss)
				cor.init(100, 255, 255);
			else if("Or" == ss)
				cor.init(255, 140, 0);
			else if("Mg" == ss)
				cor.init(255, 100, 255);
			else if("Y" == ss)
				cor.init(255, 255, 100);
			else
				cor.init(240, 240, 240);

			ColorBar cb;
			cb.color(cor);
			dist += 1.;
			cb.distance_ = dist;

//			cbVec.push_back(cb);
//
//			if(a == string::npos)
//			{
//				ss = s;
//				if("R" == ss)
//					cor.init(240, 0, 0);
//				else if("G" == ss)
//					cor.init(0, 240, 0);
//				else if("B" == ss)
//					cor.init(0, 0, 240);
//				else if("Cy" == ss)
//					cor.init(100, 255, 255);
//				else if("Or" == ss)
//					cor.init(255, 140, 0);
//				else if("Mg" == ss)
//					cor.init(255, 100, 255);
//				else if("Y" == ss)
//					cor.init(255, 255, 100);
//				else if("W" == ss)
//					cor.init(240, 240, 240);
//				else
//					cor.init(0, 0, 0);
//			}
//			else
//				cor.init(cor.red_/5, cor.green_/5, cor.blue_/5);
//
//			cb.color(cor);
//			dist += 1.;
//			cb.distance_ = dist;
//
			cbVec.push_back(cb);
		}
		if(cbVec.size() == 1)
		{
			cor.init(cor.red_/5, cor.green_/5, cor.blue_/5);
			ColorBar cb;
			cb.color(cor);
			dist += 1.;
			cb.distance_ = dist;
			cbVec.push_back(cb);
		}
	}
	else
	{
		t = groupingColors.find("|");
		if(first)
		{
			if(t != string::npos)
			{
				memset(buf, '\0', 1000);
				s.copy(buf, t, 0);
				s = buf;
			}
		}
		else
		{
			if(t != string::npos)
			{
				memset(buf, '\0', 1000);
				s.copy(buf, s.size()-t-1, t+1);
				s = buf;
			}
			else // error
			{
				ColorBar cb;
				cbVec.clear();
				TeColor c;
				hsv2Rgb(c, 0, 255, 255);
				cb.color(c);
				cb.distance_ = 0.;
				cbVec.push_back(cb);
				hsv2Rgb(c, 240, 255, 255);
				cb.color(c);
				cb.distance_ = 1.;
				cbVec.push_back(cb);
				return cbVec;
			}
		}

		size_t a = s.find("-");

		while(a != string::npos)
		{
			if(a == 0) // hue = -1
				a = s.find("-", 1);
			int cc = 0;
			memset(buf, '\0', 1000);
			s.copy(buf, a);
			ss = buf;

			a++;
			memset(buf, '\0', 1000);
			s.copy(buf, s.size()-a, a);
			s = buf;
			a = s.find("-");
			if(a == 0) // hue = -1
				a = s.find("-", 1);
			
			string sss;
			vector<double> vVec;
			size_t aa = ss.find(";");
			while(aa != string::npos)
			{
				cc++;
				memset(buf, '\0', 1000);
				ss.copy(buf, aa);
				sss = buf;
				double v = atof(sss.c_str());
				vVec.push_back(v);

				aa++;
				memset(buf, '\0', 1000);
				ss.copy(buf, ss.size()-aa, aa);
				ss = buf;
				aa = ss.find(";");
			}
			ColorBar cb;
			double brilho, dist = atof(ss.c_str());
			cb.distance_ = dist;

			if(vVec.empty()) // error in group color
			{
				cbVec.clear();
				TeColor c;
				hsv2Rgb(c, 0, 255, 255);
				cb.color(c);
				cb.distance_ = 0.;
				cbVec.push_back(cb);
				hsv2Rgb(c, 240, 255, 255);
				cb.color(c);
				cb.distance_ = 1.;
				cbVec.push_back(cb);
				return cbVec;
			}

			if(vVec.size() >= 4)
			{
				cor.init((int)vVec[0], (int)vVec[1], (int)vVec[2]);
				brilho = vVec[3];
				cb.color(cor);
			}
			else
			{
				int h = (int)vVec[0];
				int	s = (int)vVec[1];
				int v = (int)vVec[2];
				TeColor c;
				hsv2Rgb(c, h, s, v);
				cb.color(c);
			}

			cbVec.push_back(cb);

			if(a == string::npos)
			{
				cc = 0;
				vVec.clear();
				ss = s;
				size_t aa = ss.find(";");
				while(aa != string::npos)
				{
					cc++;
					memset(buf, '\0', 1000);
					ss.copy(buf, aa);
					sss = buf;
					double v = atof(sss.c_str());
					vVec.push_back(v);

					aa++;
					memset(buf, '\0', 1000);
					ss.copy(buf, ss.size()-aa, aa);
					ss = buf;
					aa = ss.find(";");
				}
				ColorBar cb;
				double brilho, dist = atof(ss.c_str());
				cb.distance_ = dist;

				if(vVec.size() >= 4)
				{
					cor.init((int)vVec[0], (int)vVec[1], (int)vVec[2]);
					brilho = vVec[3];
					cb.color(cor);
				}
				else
				{
					int h = (int)vVec[0];
					int	s = (int)vVec[1];
					int v = (int)vVec[2];
					TeColor c;
					hsv2Rgb(c, h, s, v);
					cb.color(c);
				}

				cbVec.push_back(cb);
			}
		}
	}
	return cbVec;
}


vector<TeColor> getColors(vector<ColorBar>& iVec, int ncores)
{
	vector<TeColor> cVec;
	vector<TeColor> outVec;
	map<int, vector<TeColor> > colorMap;

	generateColorBarMap(iVec, ncores * 255, colorMap);

	map<int, vector<TeColor> > :: iterator it = colorMap.begin();

	while(it != colorMap.end())
	{
		vector<TeColor>& cores = it->second;
		vector<TeColor> :: iterator i = cores.begin();

		while(i != cores.end())
		{
			cVec.push_back(*i);
			i++;
		}
		it++;
	}

	int s = (int)cVec.size();
	double d, dd = (double)s /(double)(ncores-1);
	int	i = 0, j;

	while(i < ncores)
	{
		d = dd * (double)i;
		j = TeRound(d);
		if(j >= s)
			j = s - 1;
		if((int)cVec.size() > j)
			outVec.push_back(cVec[j]);
		i++;
	}
	return outVec;
}

string getColors(vector<ColorBar>& aVec, vector<ColorBar>& bVec, int groupingMode)
{
	string s, ss;
	vector<ColorBar> colorBarVec = aVec;
	vector<ColorBar> :: iterator it;

	int r, g, b;
	double d;
//	double br, d;

	it = colorBarVec.begin();
	while(it != colorBarVec.end())
	{
		ColorBar cb = (*it);
		TeColor cor;
		hsv2Rgb(cor, cb.h_, cb.s_, cb.v_);
		r = cor.red_;
		g = cor.green_;
		b = cor.blue_;
		d = cb.distance_;

		s += Te2String(cb.h_) + ";" + Te2String(cb.s_) + ";" + Te2String(cb.v_) + ";" + Te2String(d, 2);
		if(s.size() > 255)
		{
			s = ss;
			break;
		}

		ss = s;		
		it++;
		if(it != colorBarVec.end())
			s += "-";
	}

	if(groupingMode == TeStdDeviation)
	{
		s += "|";

		colorBarVec = bVec;
		it = colorBarVec.begin();
		while(it != colorBarVec.end())
		{
			ColorBar cb = (*it);
			TeColor cor;
			hsv2Rgb(cor, cb.h_, cb.s_, cb.v_);
			r = cor.red_;
			g = cor.green_;
			b = cor.blue_;
			d = cb.distance_;

			s += Te2String(cb.h_) + ";" + Te2String(cb.s_) + ";" + Te2String(cb.v_) + ";" + Te2String(d, 2);
			if(s.size() > 255)
			{
				s = ss;
				break;
			}

			ss = s;		
			it++;
			if(it != colorBarVec.end())
				s += "-";
		}
	}

	return s;
}

void rgb2Hsv(const TeColor& c, int& h, int& s, int& v)
{
	double r = (double)(c.red_ / 255.);
	double g = (double)(c.green_ / 255.);
	double b = (double)(c.blue_ / 255.);

	double hh, ss, vv;
	RGBtoHSV(r, g, b, hh, ss, vv);

	h = TeRound(hh);
	s = TeRound(255. * ss);
	v = TeRound(255. * vv);
}

// r,g,b values are from 0 to 1
// h = [0,360], s = [0,1], v = [0,1]
//		if s == 0, then h = -1 (undefined)

void RGBtoHSV(const double& r, const double& g, const double& b, double& h, double& s, double& v )
{
	if(r == g && g == b) // achromatic (grey)
	{
		h = - 1;
		s = 0;
		v = r;
		return;
	}

	double min, max, delta;

	min = MIN(r, g);
	min = MIN(min, b);
	max = MAX(r, g);
	max = MAX(max, b);
	v = max;				// v

	delta = max - min;

	if( max != 0 )
		s = delta / max;		// s
	else {
		// r = g = b = 0		// s = 0, v is undefined
		s = 0;
		h = -1;
		return;
	}

	if( r == max )
		h = ( g - b ) / delta;		// between yellow & magenta
	else if( g == max )
		h = 2 + ( b - r ) / delta;	// between cyan & yellow
	else
		h = 4 + ( r - g ) / delta;	// between magenta & cyan

	h *= 60;				// degrees
	if( h < 0 )
		h += 360;

}

void hsv2Rgb(TeColor& c, const int& h, const int& s, const int& v)
{
	double	r, g, b;
	double hh = (double)h;
	double ss = (double)s / 255.;
	double vv = (double)v / 255.;

	HSVtoRGB(r, g, b, hh, ss, vv);

	c.red_ = TeRound(r * 255.);
	c.green_ = TeRound(g * 255.);
	c.blue_ = TeRound(b * 255.);
}

void HSVtoRGB( double& r, double& g, double& b, const double& h, const double& s, const double& v )
{
	int i;
	double f, p, q, t, hh = h;

	if( s == 0 || h == -1) {
		// achromatic (grey)
		r = g = b = v;
		return;
	}

	hh /= 60;			// sector 0 to 5
	i = TeRound(floor(hh));
	f = hh - i;			// factorial part of h
	p = v * ( 1 - s );
	q = v * ( 1 - s * f );
	t = v * ( 1 - s * ( 1 - f ) );

	switch( i ) {
		case 0:
			r = v;
			g = t;
			b = p;
			break;
		case 1:
			r = q;
			g = v;
			b = p;
			break;
		case 2:
			r = p;
			g = v;
			b = t;
			break;
		case 3:
			r = p;
			g = q;
			b = v;
			break;
		case 4:
			r = t;
			g = p;
			b = v;
			break;
		default:		// case 5:
			r = v;
			g = p;
			b = q;
			break;
	}
}


unsigned int  TeReadColorRampTextFile(const string& fileName, map<string,string>& colorRamps)
{
	string name;
	string rgb;
	vector<string> aux;
	double r, g, b;
	double h, s, v;
	unsigned int ncolors = 0;
	unsigned int n;
	try
	{
		TeAsciiFile file(fileName);
		do
		{
			string ramp;
			try 
			{
				name = file.readLine();
				ncolors = file.readInt();
				file.findNewLine();
				for (n=0; n<ncolors-1; ++n)
				{
					rgb = file.readLine();
					aux.clear();
					TeSplitString(rgb, ",", aux);
					TeTrim(aux[0]);
					TeTrim(aux[1]);
					TeTrim(aux[2]);
					r = atof(aux[0].c_str());	
					g = atof(aux[1].c_str());
					b = atof(aux[2].c_str());
					RGBtoHSV(r, g, b, h, s, v);
	                
					ramp += Te2String(int(h)) + ";";
					ramp += Te2String(int(s*255.)) + ";";
					ramp += Te2String(int(v)) + ";";
					ramp += Te2String(n) + "-";
				}
				rgb = file.readLine();
				aux.clear();
				TeSplitString(rgb, ",", aux);
				TeTrim(aux[0]);
				TeTrim(aux[1]);
				TeTrim(aux[2]);
				r = atof(aux[0].c_str());	
				g = atof(aux[1].c_str());
				b = atof(aux[2].c_str());
				RGBtoHSV(r, g, b, h, s, v);
	            
				ramp += Te2String(int(h)) + ";";
				ramp += Te2String(int(s*255.)) + ";";
				ramp += Te2String(int(v))+ ";";
				ramp += Te2String(n);
				++ncolors;
				TeTrim(name);
				colorRamps[name]=ramp;
			}
			catch(...)
			{
				break;
			}
		}while(true);
	}
	catch(...)
	{
		return 0;
	}
	return colorRamps.size();
}
