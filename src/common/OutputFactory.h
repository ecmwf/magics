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

/*! \file OutputFactory.h
    \brief Definition of the Template class OutputFactory.
    \author Meteorological Visualisation Section, ECMWF

    Started: Wed July-2006
*/

#ifndef OutputFactory_H
#define OutputFactory_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"


namespace magics {

class DriverManager;
class MagicsManager;
class GDDriver;
class PostScriptDriver;

class OutputFactory {

public:
	OutputFactory();
	virtual ~OutputFactory();

	virtual void set(const XmlNode&)
	{
		MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
	}
	virtual void set(const map<string, string>&)
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
	}
	virtual OutputFactory* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new OutputFactory();
	}
	virtual void set(DriverManager&, const XmlNode&); 
	virtual void set(MagicsManager&) {} 
	virtual void reset() {};

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	OutputFactory(const OutputFactory&);
	//! Overloaded << operator to copy - No copy allowed
	OutputFactory& operator=(const OutputFactory&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const OutputFactory& p)
		{ p.print(s); return s; }
};

class BinaryOutputFactory : public OutputFactory
{
public:
	BinaryOutputFactory() {}
	virtual ~BinaryOutputFactory() {}
	
	virtual OutputFactory* clone() const {return new BinaryOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};

class PS_PsOutputFactory : public OutputFactory
{
public:
	PS_PsOutputFactory() {}
	virtual ~PS_PsOutputFactory() { reset(); }
	
	virtual OutputFactory* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new PS_PsOutputFactory();
	}
	virtual void set(DriverManager&, const XmlNode&); 
	virtual void reset();
protected:
	static PostScriptDriver* driver_;
};

class PS_EpsOutputFactory : public PS_PsOutputFactory
{
public:
	PS_EpsOutputFactory() {}
	virtual ~PS_EpsOutputFactory() {}	
	virtual OutputFactory* clone() const  { return new PS_EpsOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
	
};

class PS_PdfOutputFactory : public PS_PsOutputFactory
{
public:
	PS_PdfOutputFactory() {}
	virtual ~PS_PdfOutputFactory() {}	
	virtual OutputFactory* clone() const  { return new PS_PdfOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
};


#ifdef MAGICS_RASTER
class GdOutputFactory : public OutputFactory
{
public:
	GdOutputFactory() {}
	virtual ~GdOutputFactory() { reset(); }
	
	virtual OutputFactory* clone() const {return new GdOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 	
	virtual void reset();
protected:
	static GDDriver* driver_;
};

class GD_GifOutputFactory : public GdOutputFactory
{
public:
	GD_GifOutputFactory() {}
	virtual ~GD_GifOutputFactory() {}

	virtual OutputFactory* clone() const { return new GD_GifOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};

class GD_PngOutputFactory : public GdOutputFactory
{
public:
	GD_PngOutputFactory() {}
	virtual ~GD_PngOutputFactory() {}

	virtual OutputFactory* clone() const  { return new GD_PngOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&);
};

class GD_JpegOutputFactory : public GdOutputFactory
{
public:
	GD_JpegOutputFactory() {}
	virtual ~GD_JpegOutputFactory() { reset(); }
	
	virtual OutputFactory* clone() const  { return new GD_JpegOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&); 
};

class GD_GifAnimOutputFactory : public GdOutputFactory
{
public:
	GD_GifAnimOutputFactory() {}
	virtual ~GD_GifAnimOutputFactory() { reset(); }	
	virtual OutputFactory* clone() const { return new GD_GifAnimOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
};
#endif

class SVG_SvgOutputFactory : public OutputFactory
{
public:
	SVG_SvgOutputFactory() {}
	virtual ~SVG_SvgOutputFactory() {}
	
	virtual OutputFactory* clone() const {return new SVG_SvgOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};

/*
#ifdef MAGICS_QT
class QT_OutputFactory : public OutputFactory
{
public:
	QT_OutputFactory() {}
	virtual ~QT_OutputFactory() {}	

	virtual OutputFactory* clone() const { return new QT_OutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};
#endif
*/

#ifdef HAVE_CAIRO
class CAIRO_PngOutputFactory : public OutputFactory
{
public:
	CAIRO_PngOutputFactory() {}
	virtual ~CAIRO_PngOutputFactory() {}

	virtual OutputFactory* clone() const { return new CAIRO_PngOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&);
};

class CAIRO_CairoOutputFactory : public OutputFactory
{
public:
	CAIRO_CairoOutputFactory() {}
	virtual ~CAIRO_CairoOutputFactory() {}	

	virtual OutputFactory* clone() const { return new CAIRO_CairoOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};

class CAIRO_PdfOutputFactory : public OutputFactory
{
public:
	CAIRO_PdfOutputFactory() {}
	virtual ~CAIRO_PdfOutputFactory() {}

	virtual OutputFactory* clone() const  { return new CAIRO_PdfOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&);
};

class CAIRO_PsOutputFactory : public OutputFactory
{
public:
	CAIRO_PsOutputFactory() {}
	virtual ~CAIRO_PsOutputFactory() {}

	virtual OutputFactory* clone() const  { return new CAIRO_PsOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&);
};

class CAIRO_EpsOutputFactory : public OutputFactory
{
public:
	CAIRO_EpsOutputFactory() {}
	virtual ~CAIRO_EpsOutputFactory() {}	
	virtual OutputFactory* clone() const  { return new CAIRO_EpsOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
};

class CAIRO_SvgOutputFactory : public OutputFactory
{
public:
	CAIRO_SvgOutputFactory() {}
	virtual ~CAIRO_SvgOutputFactory() {}	
	virtual OutputFactory* clone() const  { return new CAIRO_SvgOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
};

class CAIRO_XOutputFactory : public OutputFactory
{
public:
	CAIRO_XOutputFactory() {}
	virtual ~CAIRO_XOutputFactory() {}
	
	virtual OutputFactory* clone() const  { return new CAIRO_XOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
};

class CAIRO_GeoTiffOutputFactory : public OutputFactory
{
public:
	CAIRO_GeoTiffOutputFactory() {}
	virtual ~CAIRO_GeoTiffOutputFactory() {}
	
	virtual OutputFactory* clone() const  { return new CAIRO_GeoTiffOutputFactory(); }    
	virtual void set(DriverManager&, const XmlNode&);
};
#endif

class KML_KmlOutputFactory : public OutputFactory
{
public:
	KML_KmlOutputFactory() {}
	virtual ~KML_KmlOutputFactory() {}
	
	virtual OutputFactory* clone() const {return new KML_KmlOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};

class GEOJSON_GeoJsonOutputFactory : public OutputFactory
{
public:
	GEOJSON_GeoJsonOutputFactory() {}
	virtual ~GEOJSON_GeoJsonOutputFactory() {}
	
	virtual OutputFactory* clone() const {return new GEOJSON_GeoJsonOutputFactory();}
	virtual void set(DriverManager&, const XmlNode&); 
};


template <>
class MagTranslator<string, OutputFactory> { 
public:
	OutputFactory* operator()(const string& val )
	{
		return SimpleObjectMaker<OutputFactory>::create(val);
	}     

	OutputFactory* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
