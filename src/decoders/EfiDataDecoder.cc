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

/*! \file EfiDataDecoder.cc
    \brief Implementation of the Template class EfiDataDecoder.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 18-May-2006
    
    Changes:
    
*/



#include "EfiDataDecoder.h"
#include "Title.h"
#include "Legend.h"

using namespace magics;

namespace magics {

static string extract(const string& from)
{	
	string to;
	int first = from.find_first_of("\"");
    int last = from.find_last_of("\"");	        	
    return from.substr(first+1, last-first-1);
}
	
class Xmgr
{
public:
    Xmgr() {} 
    virtual string parse(ifstream&);        
    virtual ~Xmgr() {}
    virtual void operator()(PointsList<PaperPoint>& list);
    const string& legend() { return legend_; }

protected:
    vector<double> x_;
    vector<double> y_;
    string legend_;
    virtual void print(std::ostream&) const;
    friend ostream& operator<<(ostream& s,const Xmgr& p)
		{ p.print(s); return s; }
	
};

class XmgrXYZ : public Xmgr
{
public:
     XmgrXYZ() {}
     virtual string parse(ifstream&);     
     virtual ~XmgrXYZ() {}
     virtual void print(std::ostream&) const;
     virtual void operator()(PointsList<PaperPoint>& list);
protected:
     vector<double> z_;
};

class XmgrBasic : public vector<Xmgr*>
{
public:
    XmgrBasic(const string& name) : file_(name) {}
    void parse();
    void save(const string& name) {}
    const vector<string>& title() { return title_; }    
    void operator()(int index, PointsList<PaperPoint>& list) { (*(*this)[index])(list); }
    
protected:
    string file_;
    void print(ostream&) const;
    vector<string> title_;
    friend ostream& operator<<(ostream& s,const XmgrBasic& p)
		{ p.print(s); return s; }
};

} // ens namespace

string Xmgr::parse(ifstream& in)
{
     double x, y;
     char buf[1024];
     
    
     
     while (in.good()) {
        in.getline(buf, 1024);
        string parse(buf);
        if ( parse.find("legend string") != string::npos) {
        	legend_ = extract(parse);
        	continue;
        }
        
        if ( parse.find("@TYPE") != string::npos) 
        	return parse;
        	
        
        
        std::istringstream line(buf);
        line >> x >> y;
        x_.push_back(x);
        y_.push_back(y);
      
       
    }
    return "";
}

void Xmgr::operator()(PointsList<PaperPoint>& list)
{
	vector<double>::const_iterator x = x_.begin();
	vector<double>::const_iterator y = y_.begin();
	while ( x != x_.end() && y != y_.end() ) {
		list.push_back(PaperPoint(*x, *y));
		x++;
		y++;
	}
	
}

void XmgrXYZ::operator()(PointsList<PaperPoint>& list)
{
	vector<double>::const_iterator x = x_.begin();
	vector<double>::const_iterator y = y_.begin();
	vector<double>::const_iterator z = z_.begin();
	while ( x != x_.end() && y != y_.end() && z != z_.end()) {
		list.push_back(PaperPoint(*x, *y, *z));
		x++;
		y++;
		z++;
	}
}


void Xmgr::print(std::ostream& out) const
{
    out << "Xmgr[" << x_.size() << ", " << y_.size() << "]\n"; 
}

void XmgrXYZ::print(std::ostream& out) const
{
    out << "Xmgr[" << x_.size() << ", " << y_.size() << ", " << z_.size() << "]\n"; 
}

string XmgrXYZ::parse(ifstream& in)
{
     double x, y, z;
     char buf[1024];
     
     while (in.good()) {
        in.getline(buf, 1024);
        string parse(buf);
        if ( parse.find("legend string") != string::npos) {
        	legend_ = extract(parse);
        	continue;
        }
         if ( parse.find("@TYPE") != string::npos) 
        	return parse;
       
        std::istringstream line(buf);
        line >> x >> y >> z;
        x_.push_back(x);
        y_.push_back(y);
        z_.push_back(z);
        
      
    }
    return "";
}



void XmgrBasic::print(ostream& out) const
{
    for ( vector<Xmgr*>::const_iterator item = begin(); item!=end(); ++item)
        out << **item;
}


void XmgrBasic::parse()
{
    ifstream in(file_.c_str());
	char buf[1024];
	
	

	
	while (in.good()) {
	    in.getline(buf, 1024);
	   
        string line(buf);
        if ( line.find("title") != string::npos) {
        	title_.push_back(extract(line));
        	continue;
        }
        if ( line.find("subtitle") != string::npos) {
        	title_.push_back(extract(line));
        	continue;
        }
       
        while (line != "") {
        	if ( line.find("@TYPE xyz") != string::npos) {
            	MagLog::dev()<< line << endl;
            	Xmgr* object = new XmgrXYZ();
            	line = object->parse(in);
            	push_back(object); 
            	continue;
        	}
        	if ( line.find("@TYPE xy") != string::npos ) {
            	MagLog::dev()<< line << endl;
            	Xmgr* object = new Xmgr();
            	line = object->parse(in);
            	push_back(object);         
                continue;
        	}
        }
	}
	in.close();
    
    
    
	
	
}






EfiDataDecoder::EfiDataDecoder() : data_(0)
{
}


EfiDataDecoder::~EfiDataDecoder() 
{
	if (data_ ) delete data_;

}

/*!
 Class information are given to the output-stream.
*/		
void EfiDataDecoder::print(ostream& out)  const
{
	out << "EfiDataDecoder[";
	out << "]";
}

PointsHandler<PaperPoint>&  EfiDataDecoder::points()
{
	decode();	
	pointsHandlers_.push_back(new PointsHandler<PaperPoint>(*this));
	return *(pointsHandlers_.back());
}

void  EfiDataDecoder::decode()
{
    data_ = new XmgrBasic(path_);
    
    data_->parse();
    
    (*data_)(record_, *this);
    legend_ = (*data_)[record_]->legend();
    MagLog::debug() << " LEGEND-->" << legend_ << endl;
	
}

void EfiDataDecoder::visit(TitleBase& title)
{
   
    if ( record_ == 0  ) 
    {
    	for (vector<string>::const_iterator entry = data_->title().begin(); entry != data_->title().end(); ++entry)
    		title.push_back(new TitleEntry(*entry));
    }
     
}
void EfiDataDecoder::visit(LegendBase&)
{
    
    	
}
     
