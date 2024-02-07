/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file OutputFactory.h
    \brief Definition of the Template class OutputFactory.
    \author Meteorological Visualisation Section, ECMWF

    Started: Wed July-2006
*/

#ifndef OutputFactory_H
#define OutputFactory_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class DriverManager;
class MagicsManager;
class GDDriver;
class PostScriptDriver;

class OutputFactory {
public:
    OutputFactory();
    virtual ~OutputFactory();

    virtual void set(const XmlNode&) { MagLog::dev() << "(const XmlNode&)---> to be checked!...\n"; }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual OutputFactory* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new OutputFactory();
    }
    virtual void set(DriverManager&, const XmlNode&);
    virtual void set(MagicsManager&) {}
    virtual void reset() {}

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
    friend ostream& operator<<(ostream& s, const OutputFactory& p) {
        p.print(s);
        return s;
    }
};

class BinaryOutputFactory : public OutputFactory {
public:
    BinaryOutputFactory() {}
    virtual ~BinaryOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new BinaryOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class PS_PsOutputFactory : public OutputFactory {
public:
    PS_PsOutputFactory() {}
    virtual ~PS_PsOutputFactory() override { reset(); }

    virtual OutputFactory* clone() const override {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new PS_PsOutputFactory();
    }
    virtual void set(DriverManager&, const XmlNode&) override;
    virtual void reset() override;

protected:
    static PostScriptDriver* driver_;
};

class PS_EpsOutputFactory : public PS_PsOutputFactory {
public:
    PS_EpsOutputFactory() {}
    virtual ~PS_EpsOutputFactory() override {}
    virtual OutputFactory* clone() const override { return new PS_EpsOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class PS_PdfOutputFactory : public PS_PsOutputFactory {
public:
    PS_PdfOutputFactory() {}
    virtual ~PS_PdfOutputFactory() override {}
    virtual OutputFactory* clone() const override { return new PS_PdfOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};


#ifdef MAGICS_RASTER
class GdOutputFactory : public OutputFactory {
public:
    GdOutputFactory() {}
    virtual ~GdOutputFactory() override { reset(); }

    virtual OutputFactory* clone() const override { return new GdOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
    virtual void reset() override;

protected:
    static GDDriver* driver_;
};

class GD_GifOutputFactory : public GdOutputFactory {
public:
    GD_GifOutputFactory() {}
    virtual ~GD_GifOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new GD_GifOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class GD_PngOutputFactory : public GdOutputFactory {
public:
    GD_PngOutputFactory() {}
    virtual ~GD_PngOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new GD_PngOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class GD_JpegOutputFactory : public GdOutputFactory {
public:
    GD_JpegOutputFactory() {}
    virtual ~GD_JpegOutputFactory() override { reset(); }

    virtual OutputFactory* clone() const override { return new GD_JpegOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class GD_GifAnimOutputFactory : public GdOutputFactory {
public:
    GD_GifAnimOutputFactory() {}
    virtual ~GD_GifAnimOutputFactory() override { reset(); }
    virtual OutputFactory* clone() const override { return new GD_GifAnimOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};
#endif

class SVG_SvgOutputFactory : public OutputFactory {
public:
    SVG_SvgOutputFactory() {}
    virtual ~SVG_SvgOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new SVG_SvgOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

/*
#ifdef MAGICS_QT
class QT_OutputFactory : public OutputFactory
{
public:
    QT_OutputFactory() {}
    virtual ~QT_OutputFactory () override {}

    virtual OutputFactory* clone() const override { return new QT_OutputFactory() override;}
    virtual void set(DriverManager&, const XmlNode&) override;
};
#endif
*/

#ifdef HAVE_CAIRO
class CAIRO_PngOutputFactory : public OutputFactory {
public:
    CAIRO_PngOutputFactory() {}
    virtual ~CAIRO_PngOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new CAIRO_PngOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class CAIRO_CairoOutputFactory : public OutputFactory {
public:
    CAIRO_CairoOutputFactory() {}
    virtual ~CAIRO_CairoOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new CAIRO_CairoOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class CAIRO_PdfOutputFactory : public OutputFactory {
public:
    CAIRO_PdfOutputFactory() {}
    virtual ~CAIRO_PdfOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new CAIRO_PdfOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class CAIRO_PsOutputFactory : public OutputFactory {
public:
    CAIRO_PsOutputFactory() {}
    virtual ~CAIRO_PsOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new CAIRO_PsOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class CAIRO_EpsOutputFactory : public OutputFactory {
public:
    CAIRO_EpsOutputFactory() {}
    virtual ~CAIRO_EpsOutputFactory() override {}
    virtual OutputFactory* clone() const override { return new CAIRO_EpsOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class CAIRO_SvgOutputFactory : public OutputFactory {
public:
    CAIRO_SvgOutputFactory() {}
    virtual ~CAIRO_SvgOutputFactory() override {}
    virtual OutputFactory* clone() const override { return new CAIRO_SvgOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};
/*
class CAIRO_XOutputFactory : public OutputFactory
{
public:
    CAIRO_XOutputFactory() {}
    virtual ~CAIRO_XOutputFactory () override {}

    virtual OutputFactory* clone() const override { return new CAIRO_XOutputFactory() ; }
    virtual void set(DriverManager&, const XmlNode&) override;
};
*/

class CAIRO_GeoTiffOutputFactory : public OutputFactory {
public:
    CAIRO_GeoTiffOutputFactory() {}
    virtual ~CAIRO_GeoTiffOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new CAIRO_GeoTiffOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class CAIRO_WebpOutputFactory : public OutputFactory {
public:
    CAIRO_WebpOutputFactory() {}
    virtual ~CAIRO_WebpOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new CAIRO_WebpOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};
#endif

class KML_KmlOutputFactory : public OutputFactory {
public:
    KML_KmlOutputFactory() {}
    virtual ~KML_KmlOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new KML_KmlOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};

class GEOJSON_GeoJsonOutputFactory : public OutputFactory {
public:
    GEOJSON_GeoJsonOutputFactory() {}
    virtual ~GEOJSON_GeoJsonOutputFactory() override {}

    virtual OutputFactory* clone() const override { return new GEOJSON_GeoJsonOutputFactory(); }
    virtual void set(DriverManager&, const XmlNode&) override;
};


template <>
class MagTranslator<string, OutputFactory> {
public:
    OutputFactory* operator()(const string& val) { return SimpleObjectMaker<OutputFactory>::create(val); }

    OutputFactory* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
