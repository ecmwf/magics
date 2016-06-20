/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <iostream>

#include <XmlMagics.h>
#include <ParameterManager.h>

#include <XmlReader.h>

using namespace std;
using namespace magics;


class MagicsService {
public:
	MagicsService() : 
		manager_()
	{
	}
	
	void execute(const string& xml)
	{
		try {
			manager_.execute(xml);
		}
		catch(MagicsException& e)
		{
			MagLog::error() << e << endl;    
		}
	}

private:
	XmlMagics     manager_;
};


static 
void substitute(string& xml, const string& var, const string& val)
{
	string name = "$" + var;
	string::size_type index = xml.find(name);
	int last = 0;
	
	while ( index != string::npos)
	{
		xml.replace(index, name.length(), val);
		last = index + val.length();

		index = xml.find(name, last);
        }
}

char nospace(char c)
{
	if (c == ' ' ) return '_'; 
	return c;  
}

class TempFile
{
  public:
    TempFile() : filename(tmpnam(0)), ofs(filename)
    {
        if (!ofs) return;
    }

    ~TempFile()
    {
        ofs.close();
        remove(filename);
    }

    ofstream& operator()() { return ofs; }
    string name() { return filename; }

  private:
    const char* filename;
    ofstream ofs;
};

class Station: public XmlNodeVisitor
{
public:
    Station(const XmlNode& node)
    {
        date_ = node.getAttribute("date");
        time_ = node.getAttribute("time");
        template_ = node.getAttribute("template");
        if ( template_.empty() ) 
            template_ = string(getenv("MAGPLUS_HOME")) + "/share/templates/10_days_epsgram.xml";
        database_ = node.getAttribute("database");
        format_ = node.getAttribute("format");
         if ( format_.empty() ) format_ = "a4";
        deterministic_ = node.getAttribute("deterministic");
        if ( deterministic_.empty() ) deterministic_ = "on";
        whisker_ = node.getAttribute("whisker");
        if ( whisker_.empty() ) whisker_ = "on";
        parameters_ = node.attributes();
    }
    
    ~Station() {}
    void visit(const XmlNode& node )
    {
	if ( node.name() == "station")
	{
		ostringstream station;
		for ( XmlNode::DataIterator data = node.firstData(); data != node.lastData(); ++data)
		station << *data;

		MagLog::dev() << "title-->" << station.str() << endl;    
		MagLog::dev() << "Plot Station-->" << node.getAttribute("name") << endl;
		MagLog::dev() << "Template-->" << template_ << endl;

		try{
			ifstream test(template_.c_str());
			
			if ( !test.good() )
			{		
				// Try to look in ethe default template directory :
				template_ = string(getenv("MAGPLUS_HOME")) + "/share/templates/" + template_;
				MagLog::dev() << "try to find template in system directory-> " << template_ << endl;
				ifstream defaut(template_.c_str());
				if ( !defaut.good() ) {
					MagLog::error() << "Can not open template file " << template_ << endl;
					return;
				}
				defaut.close();
			}
			test.close();
			
			ifstream in(template_.c_str());
			
			TempFile file;
			ofstream& out = file();
			static string s; 
			s = "";

			char c;
			while(in.get(c))
			{
				s+=c;
			}

			ostringstream def;
			def << INT_MAX;

			string meta = node.getAttribute("metafile");
#ifdef MAGICS_AIX_XLC
			meta ="";
#endif
			meta = (meta.empty()) ? "nometada" : "meta path=\'" + meta + "\'";

			MagLog::dev() << "Meta-->" << meta;
			substitute(s, "meta", meta);

			string height = (node.getAttribute("height") == "" )? def.str() : node.getAttribute("height");
			substitute(s, "height", height);
			substitute(s, "latitude", node.getAttribute("latitude"));
			substitute(s, "longitude", node.getAttribute("longitude"));
			substitute(s, "station", node.getAttribute("name"));
			substitute(s, "title", station.str());
			substitute(s, "date", date_);
			substitute(s, "time", time_);
			
			substitute(s, "format", format_);
			substitute(s, "deterministic", deterministic_);
			substitute(s, "whisker", whisker_);

            for ( map<string, string>::iterator param = parameters_.begin(); param != parameters_.end(); ++param)
                     substitute(s, param->first, param->second);       
			string ps = node.getAttribute("psfile");
			ps = ps.empty() ? "nops" : "ps fullname=\'" + ps + "\'";
			substitute(s, "ps", ps);
            
			string pdf = node.getAttribute("pdffile");
			pdf = pdf.empty() ? "nopdf" : "pdf fullname=\'" + pdf + "\'";
			substitute(s, "pdf", pdf);
            
			string gif = node.getAttribute("giffile");
			gif = gif.empty() ? "nogif" : "gif fullname=\'" + gif + "\'";
			substitute(s, "gif", gif);

			string png = node.getAttribute("pngfile");
			png = png.empty() ? "nopng" : "png fullname=\'" + png + "\'";
			substitute(s, "png", png);

			string svg = node.getAttribute("svgfile");
			svg = svg.empty() ? "nosvg" : "svg fullname=\'" + svg + "\'";
			substitute(s, "svg", svg);

			out << s;

			in.close();
			out.flush();
 
			magics_.execute(file.name());
		}
		catch (exception e) {}
	}
    }
protected:
    string date_;
    string time_;
    string  template_;
    string  directory_;
    string  database_;
    string  format_;
    string  deterministic_;
    string  whisker_;
    MagicsService magics_;
    map<string, string> parameters_;
};


class Eps: public XmlNodeVisitor
{
public:
    Eps()  {}
    ~Eps() {}
    void visit(const XmlNode& node)
    {
        MagLog::dev() << "node-->" << node.name() << endl;
        if ( node.name() == "eps")
	{
            Station station(node);
            node.visit(station);
        }       
    }
};

int main(int argc, char **argv)
{
    if ( argc < 2 )
    {
        cout << "Usage: " << argv[0] << " <file.epsml>\n"; 
        exit(1);
    }

    string xml = argv[1];
    XmlReader reader;
    XmlTree tree;
    
    try {
        reader.interpret(xml, &tree);
    }
    catch (...)
    {
        cerr << "metgram : error interpreting file " << xml << "\n";
    }
    Eps metgram;

    tree.visit(metgram);
}
