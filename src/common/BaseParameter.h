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

/*! \file BaseParameter.h
    \brief Definition of Parameter base class.
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/

#ifndef BaseParameter_H
#define BaseParameter_H


#include "magics.h"
#include "MagException.h"
#include <PaperPoint.h>
#include "Matrix.h"

#ifdef MAGICS_CAIRO
 #include "cairo.h"
 typedef cairo_t* CairoPtr;
#endif

#ifdef MAGICS_QT
 #include "Qt/qwidget.h"
 #include "Qt/qgraphicsscene.h"	
 typedef QWidget* QWidgetPtr;
 typedef QGraphicsScene* QGraphicsScenePtr;
#endif

#ifdef LATER
#include "grib_api.h"
typedef grib_handle* GribHandlePtr ;
#endif

#ifndef MAGICS_Exception
#include <MagLog.h>
#endif

namespace magics {

class MistmatchType : public MagicsException
{
public:
	MistmatchType(const string& name, const string& type, const string& wait) : 
		MagicsException("Parameter " + name + ": type mismatch -> type received \"" + type + "\", expected type \"" + wait + "\" - setting ignored!") {}
};



class BaseParameter {

public:
	BaseParameter(const string& name);
	virtual ~BaseParameter();
	virtual void reset() = 0;
	virtual void setLocal(const BaseParameter*) = 0;
	virtual void resetLocal() = 0;
	virtual BaseParameter* clone() = 0;
	const string& name() const { return name_; }

#ifdef MAGICS_EXCEPTION
	virtual void set(const double&) { throw MistmatchType(name_, "real", type()); }
	virtual void setLocal(const double&) { throw MistmatchType(name_, "real", type()); }
	virtual void get(double&) const { throw MistmatchType(name_, "real", type()); }
	
	virtual void set(bool) { throw MistmatchType(name_, "bool", type()); }
	virtual void setLocal(bool) { throw MistmatchType(name_, "bool", type()); }
	virtual void get(bool&) const { throw MistmatchType(name_, "bool", type()); }

	virtual void set(const magvector<double>&) { throw MistmatchType(name_, "array of reals", type()); }
	virtual void setLocal(const magvector<double>&) { throw MistmatchType(name_, "array of reals", type()); }
	virtual void get(magvector<double>&) const { throw MistmatchType(name_, "array of reals", type()); }

	virtual void set(const int&) { throw MistmatchType(name_, "integer", type()); }
	virtual void setLocal(const int&) { throw MistmatchType(name_, "integer", type()); }
	virtual void get(int&) const { throw MistmatchType(name_, "integer", type()); }

	virtual void set(const magvector<int>&) { throw MistmatchType(name_, "integer", type()); }
	virtual void setLocal(const magvector<int>&) { throw MistmatchType(name_, "integer", type()); }
	virtual void get(magvector<int>&) const { throw MistmatchType(name_, "integer", type()); }

	virtual void set(const string&) { throw MistmatchType(name_, "string", type()); }
	virtual void setLocal(const string&) { throw MistmatchType(name_, "string", type()); }
	virtual void get(string&) const { throw MistmatchType(name_, "string", type()); }

	virtual void set(const magvector<string>&) { throw MistmatchType(name_, "stringarray", type()); }
	virtual void setLocal(const magvector<string>&) { throw MistmatchType(name_, "stringarray", type()); }
	virtual void get(magvector<string>&) const { throw MistmatchType(name_, "stringarray", type()); }

	virtual void set(LineStyle) { throw MistmatchType(name_, "LineStyle", type()); }
	virtual void setLocal(LineStyle) { throw MistmatchType(name_, "LineStyle", type()); }
	virtual void get(LineStyle&) const { throw MistmatchType(name_, "LineStyle", type()); }
	
	virtual void set(DisplayType) { throw MistmatchType(name_, "LineStyle", type()); }
	virtual void setLocal(DisplayType) { throw MistmatchType(name_, "LineStyle", type()); }
	virtual void get(DisplayType&) const { throw MistmatchType(name_, "LineStyle", type()); }

	virtual void set(Justification) { throw MistmatchType(name_, "Justification", type()); }
	virtual void setLocal(Justification) { throw MistmatchType(name_, "LineStyle", type()); }
	virtual void get(Justification&) const { throw MistmatchType(name_, "Justification", type()); }
	
	virtual void set(ListPolicy) { throw MistmatchType(name_, "ListPolicy", type()); }
	virtual void setLocal(ListPolicy) { throw MistmatchType(name_, "LineStyle", type()); }
	virtual void get(ListPolicy&) const { throw MistmatchType(name_, "ListPolicy", type()); }
	
	virtual void set(AxisAutomaticSetting) { throw MistmatchType(name_, "AxisAutomaticSetting", type()); }
	virtual void setLocal(AxisAutomaticSetting) { throw MistmatchType(name_, "AxisAutomaticSetting", type()); }
	virtual void get(AxisAutomaticSetting&) const { throw MistmatchType(name_, "AxisAutomaticSetting", type()); }
	
	virtual void set(ArrowPosition) { throw MistmatchType(name_, "ArrowPosition", type()); }
	virtual void setLocal(ArrowPosition) { throw MistmatchType(name_, "ArrowPosition", type()); }
	virtual void get(ArrowPosition&) const { throw MistmatchType(name_, "ArrowPosition", type()); }
	
	virtual void set(const Matrix&) { throw MistmatchType(name_, "Matrix", type()); }
	virtual void setLocal(const Matrix&) { throw MistmatchType(name_, "Matrix", type()); }
	virtual void get(Matrix&) const { throw MistmatchType(name_, "Matrix", type()); }
#else	
	virtual void set(const double&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }
	virtual void setLocal(const double&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }
	virtual void get(double&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }

	virtual void set(bool) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "bool", type()) << "\n";}
	virtual void setLocal(bool) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "bool", type()) << "\n"; }
	virtual void get(bool&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "bool", type()) << "\n"; }

	virtual void set(const magvector<double>&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }
	virtual void setLocal(const magvector<double>&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }
	virtual void get(magvector<double>&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }

	virtual void set(const int&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "integer", type()) << "\n"; }
	virtual void setLocal(const int&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }
	virtual void get(int&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "integer", type()) << "\n"; }

	virtual void set(const magvector<int>&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "real", type()) << "\n"; }
	virtual void setLocal(const magvector<int>&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "integer", type()) << "\n"; }
	virtual void get(magvector<int>&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "integer", type()) << "\n"; }

	virtual void set(const string&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "string", type()) << "\n"; }
	virtual void setLocal(const string&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "string", type()) << "\n"; }
	virtual void get(string&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "string", type()) << "\n"; }

	virtual void set(const magvector<string>&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "string", type()) << "\n"; }
	virtual void setLocal(const magvector<string>&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "string", type()) << "\n"; }
	virtual void get(magvector<string>&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "string", type()) << "\n"; }

	virtual void set(LineStyle) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "LineStyle", type()) << "\n"; }
	virtual void setLocal(LineStyle) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "LineStyle", type()) << "\n"; }
	virtual void get(LineStyle&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "LineStyle", type()) << "\n"; }

	virtual void set(Justification) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "Justification", type()) << "\n"; }
	virtual void setLocal(Justification) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "Justification", type()) << "\n"; }
	virtual void get(Justification&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "Justification", type()) << "\n"; }
	
	virtual void set(ListPolicy) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "ListPolicy", type()) << "\n"; }
	virtual void setLocal(ListPolicy) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "Justification", type()) << "\n"; }
	virtual void get(ListPolicy&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "ListPolicy", type()) << "\n"; }
	
	virtual void set(AxisAutomaticSetting) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "AxisAutomaticSetting", type()) << "\n"; }
	virtual void setLocal(AxisAutomaticSetting) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "AxisAutomaticSetting", type()) << "\n"; }
	virtual void get(AxisAutomaticSetting&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "AxisAutomaticSetting", type()) << "\n"; }
			

	virtual void set(ArrowPosition) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "ArrowPosition", type()) << "\n"; }
	virtual void setLocal(ArrowPosition) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "ArrowPosition", type()) << "\n"; }	
	virtual void get(ArrowPosition&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "ArrowPosition", type()) << "\n"; }
	
	virtual void set(const Matrix&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "Matrix", type()) << "\n"; }
	virtual void setLocal(Matrix&) { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "ArrowPosition", type()) << "\n"; }	
	virtual void get(Matrix&) const { MagLog::dev()<< "Magics-warning:" << MistmatchType(name_, "Matrix", type()) << "\n"; }
#endif
	virtual string type() const = 0;
	


#ifdef MAGICS_QT
	virtual void set(const QWidgetPtr&) { throw MistmatchType(name_, "qt_widget", type()); }
	virtual void get(QWidgetPtr&) const { throw MistmatchType(name_, "qt_widget", type()); }
	string getType(QWidgetPtr) const { return "qt_widget"; }

	virtual void set(const QGraphicsScenePtr&) { throw MistmatchType(name_, "qt_scene", type()); }
	virtual void get(QGraphicsScenePtr&) const { throw MistmatchType(name_, "qt_scene", type()); }
	string getType(QGraphicsScenePtr) const { return "qt_scene"; }
#endif

#ifdef MAGICS_CAIRO
	virtual void set(const CairoPtr&) { throw MistmatchType(name_, "cairo_context", type()); }
	virtual void get(CairoPtr&) const { throw MistmatchType(name_, "cairo_context", type()); }
	string getType(CairoPtr) const { return "cairo_context"; }
#endif

#ifdef LATER
	virtual void set(const GribHandlePtr&) { throw MistmatchType(name_, "grib_handle", type()); }
	virtual void get(GribHandlePtr&) const { throw MistmatchType(name_, "grib_handle", type()); }
	string getType(GribHandlePtr&) const { return "grib_handle"; }
#endif

	string getType(const string&) const { return "string"; }
	string getType(const int&) const { return "integer"; }
	string getType(const double&) const { return "real"; }
	string getType(const magvector<string>&) const { return "array of string"; }
	string getType(const magvector<int>&) const { return "array of integer"; }
	string getType(const magvector<double>&) const { return "array of real"; }
	string getType(LineStyle) const { return "LineStyle"; }
	string getType(AxisAutomaticSetting) const { return "AxisAutomaticSetting"; }
	string getType(Justification) const { return "Justification"; }
	string getType(ArrowPosition) const { return "ArrowPosition"; }
	string getType(Matrix) const { return "2DMatrix"; }

protected:
	virtual void print(ostream&) const;
	string name_;
     
private:
	// No copy allowed
	BaseParameter(const BaseParameter&);
	BaseParameter& operator=(const BaseParameter&);

// -- Friends
	friend ostream& operator<<(ostream& s,const BaseParameter& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
