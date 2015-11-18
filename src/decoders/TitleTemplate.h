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

/*! \file TitleTemplate.h
    \brief Definition of the Template class TitleTemplate.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/

#ifndef TitleTemplate_H
#define TitleTemplate_H

#include <magics.h>
#include <TitleTemplateAttributes.h>
#include <TitleField.h>
#include <Data.h>
#include <Factory.h>
#include <VectorOfPointers.h>

#include <stack>

namespace magics {
class GribDecoder;


class SimpleStringFormat
{
public:
	SimpleStringFormat(const string& value, const string& format) : format_(format), value_(value) {}
protected:
	string format_;
	string value_;
	
	void operator()(ostream& out) const
	{
	  string dup = format_;
	  string::size_type pos = dup.find ("%s",0);

	  if (pos != string::npos)
	  {
	    dup.replace(pos,2,value_);
	  }
	  out << dup;
	}

	friend ostream& operator<<(ostream& out,const SimpleStringFormat& format)
	{
	      format(out);
	      return out;
	}
};


class TitleFieldHandler 
{
public :
    TitleFieldHandler() {}
    virtual ~TitleFieldHandler() {};
    virtual void operator()(TitleField& field, vector<string>& lines, const GribDecoder&) { ASSERT (false); }

};


class TitleTemplate : public VectorOfPointers<vector<TitleTemplate*> >, 
	public TitleTemplateAttributes, 
	public std::stack<TitleTemplate*>
{

public:
	TitleTemplate(map<string, string> criteria) : criteria_(criteria) {}
	TitleTemplate();
	virtual ~TitleTemplate();

	void add(TitleField* field) { template_.push_back(field); }

	template <class D>
	static void title(vector<string>& out,  const D& data)
	{
		if ( !singleton_) new TitleTemplate();
		(*singleton_)(out, data);
	}

	static void release() 
	{
		if (singleton_) {
		     delete singleton_;
		     singleton_ = 0;
		}
	}

	template <class D>
	void operator()(vector<string>& out, const D& data) const
	{

		vector<string> lines;
		lines.push_back("");
		for (const_iterator child = begin(); child != end(); ++child)
		{
			if ((*child)->verify(data))
			{
				(*(*child))(lines, data);
				for (vector<string>::iterator line = lines.begin(); line != lines.end(); ++line)
					if ( !line->empty() ) out.push_back(*line);
				return; 
			}
		}

		for (vector<TitleField*>::const_iterator entry = template_.begin(); entry != template_.end(); ++entry)
		{
#ifdef MAGICS_EXCEPTION
			try
			{
				auto_ptr<TitleFieldHandler > object(SimpleObjectMaker<TitleFieldHandler >::create((*entry)->name()));
				(*object)(*(*entry), lines, data);
			}
			catch (NoFactoryException& e)
			{
				// The data do not know how to verify the criter ....
				MagLog::debug() << "Can Not Create the TitleFieldHandler for " << (*entry)->name() << "\n";
				(*(*entry))(lines);
			}
#else
			TitleFieldHandler* object = SimpleObjectMaker<TitleFieldHandler >::create((*entry)->name());
			if (object)
			{
				(*object)(*(*entry), lines, data);
			}
			else
			{
				// The data do not know how to verify the criter ....
				MagLog::debug() << "Can Not Create the TitleFieldHandler for " << (*entry)->name() << "\n";
				(*(*entry))(lines);
			}
#endif
		}
		
		for (vector<string>::iterator line = lines.begin(); line != lines.end(); ++line)
			if ( !line->empty()) out.push_back(*line);
	}

/*!
 \todo code needed if MAGICS_EXCEPTION are disabled!
*/

	bool verify(const GribDecoder& data) const;

	map<string, string>& criteria() { return criteria_; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	map<string, string> criteria_;
	VectorOfPointers<vector<TitleField*> > template_;
	static TitleTemplate* singleton_;
	void decode();

private:
	//! Copy constructor - No copy allowed
	TitleTemplate(const TitleTemplate&);
	//! Overloaded << operator to copy - No copy allowed
	TitleTemplate& operator=(const TitleTemplate&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TitleTemplate& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
