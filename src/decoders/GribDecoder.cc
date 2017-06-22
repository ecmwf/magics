/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribDecoder.cc
    \brief Implementation of the class GribDecoder.

    Magics Team - ECMWF 2004

    Started: Tue 16-Mar-2004

    Changes:

 */

#include "GribDecoder.h"
#include "Factory.h"
#include <limits>
#include <unistd.h>
#include "TitleTemplate.h"
#include "LocalTable.h"
#include "DateTime.h"
#include "GribTables.h"
#include "GribInterpretor.h"
#include "XmlReader.h"
#include "TextVisitor.h"
#include "Timer.h"
#include "VisualAction.h"
#include "AnimationRules.h"
#include "Transformation.h"
#include "MetaData.h"
#include "MagJSon.h"

using namespace magics;

int  GribDecoder::count_ = 0;

GribDecoder::GribDecoder() :  matrix_(0),  xComponent_(0), yComponent_(0),
        colourComponent_(0), handle_(0), nearest_(0), interpretor_(0),
        field_(0), component1_(0), component2_(0), colour_(0), xValues_(0), yValues_(0)
{
    count_++;
    title_ = "grib_" + tostring(count_);
    version();

}

void GribDecoder::version()
{
    static bool done = false;
    if ( done ) return;
    done = true;
    MagLog::info() << "GribAPI Version :" << grib_get_api_version() << endl;
}

GribDecoder::~GribDecoder()
{
    if ( matrix_) delete matrix_;
    if ( xComponent_ ) delete xComponent_;
    if ( yComponent_ ) delete yComponent_;
    if ( handle_ ) {
        grib_handle_delete (handle_);
    }

    if (nearest_)
        grib_nearest_delete(nearest_);

    for (PointsList::iterator point = points_.begin(); point != points_.end(); ++point) {
        delete *point;
        *point = 0;
    }
    if ( interpretor_ ) delete interpretor_;

    //context is a reference to a global context and should not be deleted
}

void GribDecoder::set(const GribLoop& loop, int index)
{
    // Copy the information about scaling...
    scaling_         = loop.scaling_;
    derived_scaling_ = loop.derived_scaling_;
    scaling_offset_  = loop.scaling_offset_;
    scaling_factor_  = loop.scaling_factor_;
    index_           = loop.uniqueId_;
    interpolation_method_ = loop.interpolation_method_;
    missing_fill_count_ = loop.missing_fill_count_;
    wind_mode_       = auto_ptr<WindMode>(loop.wind_mode_->clone());
    internalIndex_ = index;
}

long computeStep( const GribDecoder& grib,const string&  key)
{
    static map<string, double> stepUnits;
    if ( stepUnits.empty() ) {
        stepUnits["h"] = 3600;
        stepUnits["s"] = 1;
        stepUnits["m"] = 60;
        stepUnits["3h"] = stepUnits["h"] * 3;
        stepUnits["6h"] = stepUnits["h"] * 6;
        stepUnits["12h"] = stepUnits["h"] * 6;
        stepUnits["D"] = stepUnits["h"] * 24;
        stepUnits["M"] = stepUnits["D"] * 30;
        stepUnits["Y"] = stepUnits["M"] * 12;
        stepUnits["10Y"] = stepUnits["M"] * 10;
        stepUnits["30Y"] = stepUnits["M"] * 30;
        stepUnits["C"] = stepUnits["M"] * 100;
    }
    string units = grib.getString("stepUnits");
    long step = grib.getLong(key);

    map<string, double>::iterator stepunit = stepUnits.find(units);
    double factor = 1;
    if ( stepunit != stepUnits.end() )
        factor = stepunit->second;
    return step * factor;
}

long GribDecoder::getLong(const string& key, bool warnIfKeyAbsent) const
{
    long val;
    map<string, long>::const_iterator lk = lKeys_.find(key);
    if ( lk != lKeys_.end() ) {
        return lk->second;
    }
    int err = grib_get_long(handle_, key.c_str(), &val);
    if ( err )
    {
        if (warnIfKeyAbsent)
        {
            MagLog::warning() << "Grib API: can not find key [" << key << "]  - "<< grib_get_error_message(err) <<"\n";
        }
        return 0;
    }
    lKeys_.insert(make_pair(key, val));
    return val;
}

string GribDecoder::getstring(const string& key, bool warnIfKeyAbsent, bool cache) const
{
    if ( cache ) {
        map<string, string>::const_iterator sk = sKeys_.find(key);
        if ( sk != sKeys_.end() ) {
            return sk->second;
        }
    }
    char val[1024];
    size_t length = 1024;

    int err = grib_get_string(handle_, key.c_str(), val, &length);

    if ( err )
    {
        if (warnIfKeyAbsent)
        {
            MagLog::warning() << "Grib API: can not find key [" << key << "]  - "<< grib_get_error_message(err) <<"\n";
        }
        return "";
    }
    if ( cache )
        sKeys_.insert(make_pair(key, val));
    return string(val);
}

string GribDecoder::getString(const string& key, bool warnIfKeyAbsent) const
{
    if ( Data::dimension_ == 1 )
        return getstring(key, warnIfKeyAbsent, false);

    string value;
    grib_handle* handle = handle_;
    // otherwise we build a name...

    GribDecoder* grib = const_cast<GribDecoder*>(this);
    grib->openFirstComponent();
    string name1 = getstring(key, warnIfKeyAbsent, false);
    grib->openSecondComponent();
    string name2 = getstring(key, warnIfKeyAbsent, false);
    grib->handle(handle);
    value = (name1==name2) ? name1 : name1 + "/" + name2;

    if ( Data::dimension_ == 3 ) {
        grib->openThirdComponent();
        string name3 = getstring(key, warnIfKeyAbsent, false);
        grib->handle(handle);
        if ( value != name3 )
            value = value + "/" + name3;
    }
    return value;
}


double GribDecoder::getDouble(const string& key, bool warnIfKeyAbsent) const
{
    map<string, double>::const_iterator dk = dKeys_.find(key);
    if ( dk != dKeys_.end() ) {
        return dk->second;
    }
    double val;
    int err = grib_get_double(handle_, key.c_str(), &val);
    if ( err )
    {
        if (warnIfKeyAbsent)
        {
            MagLog::warning() << "Grib API: can not find key [" << key << "]  - " << grib_get_error_message(err) <<"\n";
        }
        return 0;
    }
    dKeys_.insert(make_pair(key, val));
    return val;
}


void   GribDecoder::setDouble(const string& key, double val) const
{
    int err = grib_set_double(handle_, key.c_str(), val);
    if ( component1_ )
    	err = grib_set_double(component1_, key.c_str(), val);
    if ( component2_ )
       	err = grib_set_double(component2_, key.c_str(), val);
    if ( err )
    {
        MagLog::warning() << "Grib API: can not find key [" << key << "]  - "<< grib_get_error_message(err) <<"\n";
    }
}

void GribDecoder::scale(const string& metadata, double& scaling, double& offset)
{
	scaling = 1;
	offset = 0;
	if ( metadata.empty() ) {

		return;
	}
	ParamJSon data = ParamJSon(metadata);

	long table = 128; // hard-coded in case of GRIB 2
	long centre = 98;  // hard-coded in case of GRIB 2

	ParamJSon::iterator pid = data.find("paramId");

	long id = ( pid != data.end() )  ? tonumber(pid->second) : 0;

	try {
		const ParamDef& paramdef = LocalTable::localInfo(id, table,
				centre);
		scaling = paramdef.scaling();
		offset = paramdef.offset();

	}
	catch (...) {
		MagLog::warning() << " Can not find information for the parameter [" << id << "." << table << "]\n";
	}
}

void GribDecoder::read(Matrix **matrix, Matrix** matrix2)
{

    if ( !handle_ ) {
        *matrix = 0;
        return;
    }
    long repres;
    grib_get_long(handle_,"dataRepresentationType",&repres);
    const string representation = getString("typeOfGrid");

    try {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }
        interpretor_->interpretAsMatrix(*this, matrix, matrix2);
        if ( *matrix == 0 ) {
            valid_ = false;
            ostringstream msg;
            msg << "Grib Decoder: Representation [" << representation << "] not yet fully implemented";
            MagLog::error() <<  msg.str() << std::endl;
            throw MagicsException(msg.str());
        }
        interpretor_->scaling(*this, matrix);
    }
    catch (NoFactoryException&)
    {
        ostringstream msg;
        msg << "Grib Decoder - read: Representation [" << representation << "] not supported";
        MagLog::error() << msg.str() << endl;
        valid_ = false;
        throw MagicsException(msg.str());
    }
}


void GribDecoder::read(Matrix **matrix, const Transformation&  transformation)
{
    if ( !handle_  ) {
        *matrix = 0;
        return;
    }
    long repres;
    grib_get_long(handle_,"dataRepresentationType",&repres);
    const string representation = getString("typeOfGrid");

    try {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }
        interpretor_->interpretAsMatrix(*this, matrix, transformation);
        interpretor_->scaling(*this, matrix);
    }
    catch (NoFactoryException&)
    {
        MagLog::error() << "Grib Decoder - read: Representation [" << representation << "] not supported.\n"<< std::endl;;
        valid_ = false;
        throw MagicsException("Grib Decoder: Representation [] not supported.");
    }
}


/*!
 Class information are given to the output-stream.
 */
void GribDecoder::print(ostream& out)  const
{
    out << "GribDecoder[";
    GribDecoderAttributes::print(out);
    out << "] ";
}

void GribDecoder::release()
{
    if ( matrix_ ) {
        delete matrix_;
        matrix_ = 0;
    }
    if ( xComponent_ ) {
        delete xComponent_;
        xComponent_ = 0;
    }
    if ( yComponent_ ) {
        delete yComponent_;
        yComponent_ = 0;
    }
    if ( colourComponent_ ) {
        delete colourComponent_;
        colourComponent_ = 0;
    }

    for (PointsList::iterator point = points_.begin(); point != points_.end(); ++point) {
        delete *point;
        *point = 0;
    }
    points_.clear();
}

void GribDecoder::visit(Transformation& transformation)
{
    if(transformation.coordinateType() == Transformation::GeoType )
        return;
    decode();
    // Here are in a dump ode .. the coordinates are pixels.
    if ( transformation.getAutomaticX() ) {
        transformation.setMinMaxX(1, matrix_->columns());
    }
    if ( transformation.getAutomaticY() ) {
        transformation.setMinMaxY(1, matrix_->rows());
    }
}

void GribDecoder::decode2D()
{
    if (xComponent_) return;
    Matrix     *w1 = 0;
    Matrix     *w2 = 0;
    colourComponent_ = 0;
    const string representation = getString("typeOfGrid");
    try {

        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }
        interpretor_->keepOriginal(true);

    }
    catch (NoFactoryException&)
    {
        MagLog::warning() << "Grib Decoder: Vector cobination of representations [" << representation << "] not supported.\n"<< std::endl;;
        valid_ = false;
    }
    readColourComponent();
    openFirstComponent();
    openSecondComponent();
    read(&w1, &w2);


    Data::dimension_ = ( colourComponent_ ) ? 3 : 2;

    wind_mode_->x(&xComponent_, &yComponent_, w1, w2);
    interpretor_->keepOriginal(false);
}


void GribDecoder::customisedPoints(const AutomaticThinningMethod& thinning, const Transformation& transformation, const std::set<string>& request, CustomisedPointsList& points)
{
    openFirstComponent();
    long repres;
    grib_get_long(handle_,"dataRepresentationType",&repres);
    const string representation = getString("typeOfGrid");
    try {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }

        // Compute the thinning factor...

        double x1 = 0;
        double y1 = 60;
        double x2 = 0;
        double y2 = 60 + interpretor_->XResolution(*this);

        UserPoint p1(x1, y1);
        UserPoint p2(x2, y2);

        double res = transformation.distance(p1, p2);

        double xstep = ( transformation.getMaxPCX() - transformation.getMinPCX())/ (thinning.x()-1);
        double ystep = ( transformation.getMaxPCY() - transformation.getMinPCY())/ (thinning.y()-1);

        int nb = (xstep/res);
        xstep = (nb > 1 ) ? nb * res : 0;
        nb = (ystep/res);
        ystep = (nb > 1 ) ? nb * res : 0;

        customisedPoints(transformation, points, xstep, ystep, 0);
    }
    catch (NoFactoryException&)
    {
        MagLog::error() << "Grib Decoder - customisedPoints: Representation [" << representation << "] not supported.\n"<< std::endl;;
        throw MagicsException("Grib Decoder: Representation [] not supported.");
    }
}


grib_handle*  GribDecoder::uHandle(string& name)
{
    openFirstComponent();
    name = "x_component";
    return handle_;
}

grib_handle*  GribDecoder::vHandle(string& name)
{
    openSecondComponent();
    name = "y_component";
    return handle_;
}

grib_handle*  GribDecoder::cHandle(string&)
{
    return 0;
}



bool compare(const pair<double, double>& pt1, const pair<double, double>& pt2)
{
    if ( pt1.second != pt2.second)
        return false;
    return pt1.second < pt2.second;
}





void GribDecoder::newPoint(const Transformation& transformation, double lat, double lon, double uc, double vc, double cc, vector<CustomisedPoint*>& points, double grid)
{
    std::stack<UserPoint>   duplicates;
    UserPoint geo(lon, lat);

    transformation.wraparound(geo, duplicates);
    while (duplicates.empty() == false) {
        UserPoint pt = duplicates.top();
        // we try to see if no to near neighborg ...

        map<double, std::set<double> >::iterator row =  positions_.find(pt.y());
        if ( row == positions_.end() )
            positions_.insert(make_pair(pt.y(), std::set<double>()));
        row =  positions_.find(pt.y());
        std::set<double>::iterator low =  row->second.lower_bound(pt.x());
        bool add = true;
        if ( low != row->second.end() ) {
            double diff = fabs(*low-pt.x());
            if ( diff <= grid )
                add = false;
            ++low;
            if ( low != row->second.end() ) {
                double diff = fabs(*low -pt.x());
                if ( diff <= grid )
                    add = false;
            }
        }
        if ( !add ) {
            duplicates.pop();
            continue;
        }

        row->second.insert(pt.x());

        CustomisedPoint *point = new CustomisedPoint(pt.x(), pt.y(), "");

        point->insert(make_pair("x_component", uc));
        point->insert(make_pair("y_component", vc));
        points.push_back(point);
        if ( cc != -9999 ) {
            point->insert(make_pair("colour_component", cc));
        }
        duplicates.pop();
    }
}



struct Compare
{
    template< typename T1, typename T2 >
    bool operator()( T1 const& t1, T2 const& t2 ) const
    {
        return t1.first < t2;
    }
};

void GribDecoder::customisedPoints(const Transformation& transformation, CustomisedPointsList& out, double thinx, double thiny, double gap)
{
    
    
    decode2D();
    

    double minlon = 0.;
    double maxlon = 360.;

    double missing = getDouble("missingValue");


    if ( thiny ) {
        vector<pair<double, double> > positions;

        PaperPoint xy = interpretor_->reference(*this, transformation);
        transformation.thin(thinx, xy, positions);

        vector<pair<double, double> >::iterator pos = positions.begin();
        out.reserve(positions.size());

        int i = 0;
       
        std::set<int> cache;
        cache.insert(-1);
        for ( pos = positions.begin(); pos != positions.end(); ++pos) {
            double offset = 0.;
            // First make sure tthat the lon is between the minlon and maxlon.
            double lon = pos->first;
            double lat = pos->second;
            double nlat, nlon;

            int w = xComponent_->nearest_index(lat, lon, nlat, nlon);

           
            i++;
            bool cached = ( cache.find(w) != cache.end() );
            if ( !cached ) {
                    cache.insert(w);
                    CustomisedPoint *add = new CustomisedPoint(nlon, nlat, "");
                    double u = xComponent_->data_[w];
                    double v = yComponent_->data_[w];
                    interpretor_->interpret2D(nlat, nlon, u, v);
                    pair<double, double> value = (*wind_mode_)(u, v);

                    add->insert(make_pair("x_component", value.first));
                    add->insert(make_pair("y_component", value.second));
                    if (colourComponent_)
                         add->insert(make_pair("colour_component", (*colourComponent_)[w]));
                  
                    out.push_back(add);

                    string debug = getEnvVariable("WIND_DEBUG");
                    if ( debug != "" ) {
                    	add = new CustomisedPoint(lon, lat, "");

                    	add->insert(make_pair("x_component", 0.01));
                    	add->insert(make_pair("y_component", 0.01));
                    	out.push_back(add);
                    }

                }
                
            }
            
    }

    else { // no thinning !
        // get all the points of the index!
    	
        for ( map<double, int>::iterator y = xComponent_->yIndex_.begin(); y != xComponent_->yIndex_.end(); ++y) {
        	InfoIndex x = xComponent_->xIndex_[y->second];
        	double lat = y->first;
        	int index = x.offset_;
            for ( int i = 0; i < x.nbPoints_; i++) {
            	double lon = x.first_  + (i*x.step_);
                double u = xComponent_->data_[index];
                double v = yComponent_->data_[index];
                index++;
                if ( u != missing && v != missing) {
                    interpretor_->interpret2D(lat, lon, u, v);
                    pair<double, double> value = (*wind_mode_)(u, v);
                    vector<UserPoint> pos;
                    transformation.populate(lon, lat, 0, pos);
                    for ( vector<UserPoint>::iterator p = pos.begin(); p != pos.end(); ++p) {
                        CustomisedPoint *add = new CustomisedPoint(p->x(), p->y(), "");
                        add->insert(make_pair("x_component", value.first));
                        add->insert(make_pair("y_component", value.second));
                        if (colourComponent_)
                          add->insert(make_pair("colour_component", (*colourComponent_)[index]));
                        out.push_back(add);
                    }
                }
            }
        }
    }
}


void GribDecoder::customisedPoints(const BasicThinningMethod& thinning, const Transformation& transformation, const std::set<string>& needs, CustomisedPointsList& points)
{
    openFirstComponent();
    long repres;
    grib_get_long(handle_,"dataRepresentationType",&repres);
    const string representation = getString("typeOfGrid");
    try {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }

        // Compute the thinning factor...
        double gap = 0;
        // find the middle point :
        UserPoint ll = transformation.reference();
        PaperPoint xy1 = transformation(ll);
        ll.y_ = ll.y_ + interpretor_->XResolution(*this);
        PaperPoint xy2 = transformation(ll);

        gap = xy1.distance(xy2);
        if ( thinning.factor() == 1 ) {
            gap = 0;
        }
        thinning_debug_ = ( needs.find("debug") != needs.end() );
        customisedPoints(transformation, points,
                gap * thinning.factor(),
                gap * thinning.factor(), gap);
    }
    catch (NoFactoryException&)
    {
        MagLog::error() << "Grib Decoder- customisedPoints: Representation [" << representation << "] not supported.\n"<< std::endl;;
        throw MagicsException("Grib Decoder: Representation [] not supported.");
    }
}


void GribDecoder::decode2D(const Transformation&)
{
    Data::dimension_ = 2;
    if (xComponent_) return;
    Matrix     *w1 = 0;
    Matrix     *w2 = 0;

    const string representation = getString("typeOfGrid");

    try {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }
        readColourComponent();
        openFirstComponent();
        read(&w1);
        openSecondComponent();
        read(&w2);
    }
    catch (NoFactoryException&)
    {
        MagLog::warning() << "Grib Decoder: Vector representation [" << representation << "] not supported.\n"<< std::endl;;
    }
    wind_mode_->x(&xComponent_, &yComponent_, w1, w2);
}


void GribDecoder::openFirstComponent()
{
    grib_field_position_ =  position_1_;
    component1_ = open(component1_);
}

grib_handle* GribEntryDecoder::open(grib_handle* handle, bool) {
    if ( !handle_ ) {
        return handle_;
    }
    return handle;
}

void GribDecoder::openSecondComponent()
{
    grib_field_position_ =  position_2_;
    component2_ = open(component2_);
}

void GribDecoder::openThirdComponent()
{
    grib_field_position_ =  colour_position_;
    colour_ = open(colour_, false);
}

void GribDecoder::readColourComponent()
{
    grib_field_position_ =  colour_position_;
    try {
        colour_ = open(colour_, false);
        read(&colourComponent_);
    }
    catch (...) {
        colourComponent_ = 0;
        valid_ = true;
    }
}

grib_handle*  GribDecoder::open(grib_handle* grib, bool sendmsg)
{
    if ( grib ) {
        handle(grib);
        return grib;
    }


    FILE* file = fopen(file_name_.c_str(),"r");
    handle_ = 0;
    if (!file)
    {
        MagLog::error() << "file can not be opened [" << file_name_ << "]" << std::endl;
        MagLog::broadcast();
        valid_ = false;
        return 0;
    }

    handle_ = (*address_mode_)(0, file, grib_field_position_);

    if ( !handle_ )
    {
        if (sendmsg) {
            MagLog::error() << "can not access position [" << grib_field_position_<<" in " << file_name_ << "]" << std::endl;
            MagLog::broadcast();
            valid_ = false;
            return 0;
        }
    }
    fclose(file);

    return handle_;
}


grib_nearest* GribDecoder::nearest_point_handle(bool keep)
{
    int err;

    if (!keep)
    {
        return grib_nearest_new(handle_, &err); // create a new one
    }
    else
    {   // we want to retain this and only create a new one if needed
        if (!nearest_)
        {
            nearest_ = grib_nearest_new(handle_, &err); // only used in Metview's Cursor Data facility
            if (err)
                return 0;
        }
        return nearest_;
    }
}


bool GribDecoder::id(const string& id, const string& where) const
{
    if ( id_.empty() && id.empty() ){

        return ( verify(where) );
    }
    return magCompare(id_, id);
}


bool GribDecoder::verify(const string& val) const
{
    // we except a string with the following format "key1=val,key2=val2,...,keyn=valn"
    Tokenizer tokenizer(",= ");
    vector<string> tokens;
    map<string, string> where;
    tokenizer(val, tokens);
    vector<string>::iterator token = tokens.begin();

    string key, value;
    while (true) {
        if (token == tokens.end() )
            break;
        key = *token;
        ++token;
        if (token == tokens.end() )
            break;
        value = *token;
        where.insert(make_pair(key, value));

        ++token;
    }
    for (  map<string, string>::const_iterator w = where.begin(); w != where.end(); ++w) {
        string val = getString(w->first);
        if ( magCompare(val, w->second) == false )
            return false;
    }
    return true;
}

void GribDecoder::decodePoints()
{
    if ( !points_.empty() ) return;
    unsigned long flags=0;
    int error;

    if ( Data::dimension_ == 1 ) {
        double scaling;
        double offset;
        field_ = open(field_);
        const string representation = getString("typeOfGrid");
        double missing = getDouble("missingValue");
        try {
            if ( !interpretor_ ) {
                interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
            }
            interpretor_->scaling(*this, scaling, offset);
        }
        catch (NoFactoryException&)
        {
            MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not supported.\n"<< std::endl;;
            scaling =1 ; offset =0;
        }

        grib_iterator* iter = grib_iterator_new(handle_, flags, &error);

        if (!iter)
        {
            MagLog::error() << "Grib Iterator not yet supported on this kind of GRIB\n";
            MagLog::broadcast();
            throw MagicsException("Grib Iterator not yet supported.");
        }

        double lat;
        double lon;
        double val;
        while (grib_iterator_next(iter,&lat,&lon,&val))
        {
            if ( val != missing)
                points_.push_back(new UserPoint(lon, lat, (val*scaling) + offset));
        }
        return;
    }

    openFirstComponent();
    const string representation = getString("typeOfGrid");
    double missing = getDouble("missingValue");

    grib_iterator* iter1 = grib_iterator_new(handle_, flags, &error);
    openSecondComponent();
    grib_iterator* iter2 = grib_iterator_new(handle_, flags, &error);
    if (!iter1 || !iter2)
    {
        MagLog::error() << "Grib Iterator not yet supported on this kind of GRIB\n";
        throw MagicsException("Grib Iterator not yet supported.");
    }

    double lat1, lat2;
    double lon1, lon2;
    double val1, val2, norm;
    while ( grib_iterator_next(iter1,&lat1,&lon1,&val1) && grib_iterator_next(iter2,&lat2,&lon2,&val2) )
    {
        if ( lat1 == lat2 && lon1 == lon2 )
            if ( val1 != missing && val2 != missing) {
                norm=wind_mode_->norm(val1,val2);
                points_.push_back(new UserPoint(lon1, lat1, norm));
            }
    }
}



GribLoop::~GribLoop()
{
    for (vector<GribDecoder*>::const_iterator g = gribs_.begin(); g != gribs_.end(); ++g)
    {
        delete *g;
    }

    if (file_ != 0)
    {
        fclose(file_);
        file_ = 0;
    }
}


Data* GribLoop::current()
{
    return currentgrib_;
}


map<string, string> GribLoop::ids_;
int GribLoop::index_ = 0;
void GribLoop::next() {}


GribLoop::GribLoop():  currentgrib_(0), file_(0)
{
    currentDim_ = dimension_.begin();
    currentPos_ = dim_.begin();
    gribs_.clear();
    uniqueId_ = Data::getUniqueOwnerId();
    counter_ = 0;
}


void GribLoop::setToFirst()
{
    currentDim_ = dimension_.begin();
    currentPos_ = dim_.begin();
}


bool  GribLoop::hasMore()
{
    if (file_ == 0 ) {
        file_ = fopen(path_.c_str(),"r");
        if (!file_) {
            MagLog::error() << "file can not be opened [" << path_ <<  "]" << std::endl;
            throw GribFileMagException(path_, 0);
        }
    }

    // Now we have to find the right Entry!!!

    if ( currentDim_ == dimension_.end() )
        return false;

    if ( *currentDim_ == 1 ) {
        if (  dim_.empty() ) {
            // case 1 dimension= 1 and loop on all the fields!
            int error;
            grib_handle* handle = grib_handle_new_from_file(0, file_, &error) ;
            if ( !handle )
                return false;
            currentgrib_ = new GribEntryDecoder(handle);
            currentgrib_->set(*this, counter_++);
            gribs_.push_back(currentgrib_);
        }
        else {
            if ( currentPos_ == dim_.end() )
                return false;
            // Case 3 Dimension = 1 but we only used  a subset of fields

            grib_handle* handle =  (*address_mode_)(0, file_, *currentPos_);
            currentPos_++;
            if (!handle)
                return false;
            currentgrib_ = new GribEntryDecoder(handle);
            currentgrib_->set(*this, counter_++);
            gribs_.push_back(currentgrib_);
        }
    }


    if ( *currentDim_  == 2)
    {
        if ( dim_.empty()  )
        {
            // case 2 Dimension = 2 and loop on all the field!
            int error;
            grib_handle* handle1 = grib_handle_new_from_file(0, file_, &error) ;
            if (!handle1)  return false;
            grib_handle* handle2 = grib_handle_new_from_file(0, file_, &error) ;
            if (!handle2)  return false;
            currentgrib_ = new GribEntryDecoder(handle1, handle2);
            currentgrib_->set(*this, counter_++);
            gribs_.push_back(currentgrib_);
        }
        else {
            // Case 4 Dimesnion = 2 and we only used a subset of fields!
            vector<long int>::iterator dim1 =  currentPos_;
            if ( currentPos_ ==  dim_.end() )
                return false;
            currentPos_++;
            vector<long int>::iterator dim2 =  currentPos_;
            if ( currentPos_ ==  dim_.end() )
                return false;
            currentPos_++;


            grib_handle* handle1 =  (*address_mode_)(0, file_, *dim1);
            grib_handle* handle2 =  (*address_mode_)(0, file_, *dim2);
            if ( !handle1 )
                return false;
            if ( !handle2 )
                return false;
            currentgrib_ = new GribEntryDecoder(handle1, handle2);
            currentgrib_->set(*this, counter_++);

            gribs_.push_back(currentgrib_);

        }
    }

    if ( *currentDim_  == 3)
    {
        if ( dim_.empty()  )
        {
            // case 2 Dimension = 2 and loop on all the field!
            int error;
            grib_handle* handle1 = grib_handle_new_from_file(0, file_, &error) ;
            if ( !handle1 )  return false;
            grib_handle* handle2 = grib_handle_new_from_file(0, file_, &error) ;
            if ( !handle2 )  return false;
            grib_handle* handle3 = grib_handle_new_from_file(0, file_, &error) ;
            if ( !handle3)  return false;
            currentgrib_ = new GribEntryDecoder(handle1, handle2, handle3);
            currentgrib_->set(*this, counter_++);
            gribs_.push_back(currentgrib_);
        }
        else {
            // Case 4 Dimesnion = 2 and we only used a subset of fields!
            vector<long int>::iterator dim1 =  currentPos_;
            if ( currentPos_ ==  dim_.end() )
                return false;
            currentPos_++;
            vector<long int>::iterator dim2 =  currentPos_;
            if ( currentPos_ ==  dim_.end() )
                return false;
            currentPos_++;
            vector<long int>::iterator dim3 =  currentPos_;
            if ( currentPos_ ==  dim_.end() )
                return false;
            currentPos_++;

            grib_handle* handle1 =  (*address_mode_)(0, file_, *dim1);
            grib_handle* handle2 =  (*address_mode_)(0, file_, *dim2);
            grib_handle* handle3 =  (*address_mode_)(0, file_, *dim3);

            if ( !handle1 )
                return false;
            if ( !handle2 )
                return false;
            if ( !handle3 )
                return false;
            currentgrib_ = new GribEntryDecoder(handle1, handle2, handle3);
            currentgrib_->set(*this, counter_++);

            gribs_.push_back(currentgrib_);

        }
    }
    currentDim_++;
    currentgrib_->setPath(path_);
    if ( iconName_.empty() )
    {
        map<string, string>::iterator id = ids_.find(path_);
        if ( id == ids_.end() )
        {
            iconName_ = "Grib" + tostring(index_);
            index_++;
            ids_.insert(make_pair(path_, iconName_));
        }
        else
            iconName_ = id->second;
    }
    currentgrib_->icon(*this);
    return true;
}

void GribLoop::print(ostream&) const {}

void GribDecoder::handle(grib_handle* handle)
{
    handle_ = handle;
    lKeys_.clear();
    sKeys_.clear();
    dKeys_.clear();
}

class GribTag: public XmlNodeVisitor
{
public:
    GribTag(GribDecoder& grib, TagHandler& title) : grib_(grib), title_(title) {
    }

    ~GribTag() {}
    string baseDate(const XmlNode& node)
    {
        string format= node.getAttribute("format");
        if ( format.empty() )
            format =  "%A %d %B %Y %H%M UTC";
        const long day  = grib_.getLong("date");
        const long hour = grib_.getLong("hour");
        const long mn   = grib_.getLong("minute");
        MagDate part1 = MagDate(day);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);

        const long type  = grib_.getLong("significanceOfReferenceTime", false);
        if ( type == 2 ) { //     Verifying time of forecast
            const long step =  computeStep(grib_, "stepRange");
            full = full + (step * -1);
        }
        return full.tostring(format);
    }

    string startDate(const XmlNode& node)
    {
        string format= node.getAttribute("format");
        if ( format.empty() )
            format =  "%A %d %B %Y %H%M UTC";
        const long day  = grib_.getLong("date");
        const long hour = grib_.getLong("hour");
        const long mn   = grib_.getLong("minute");
        const long step = computeStep(grib_, "startStep");

        MagDate part1 = MagDate(day);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);
        full = full + step;

        return full.tostring(format);
    }

    string validDate(const XmlNode& node)
    {
        string format= node.getAttribute("format");
        if ( format.empty() )
            format =  "%A %d %B %Y %H%M UTC";
        const long day =  grib_.getLong("date");
        const long hour = grib_.getLong("hour");
        const long mn =  grib_.getLong("minute");
        const long step =  computeStep(grib_, "stepRange");  // default is in hours. Set 'stepUnits' to change.



        MagDate part1 = MagDate(day);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);
        const long type  = grib_.getLong("significanceOfReferenceTime", false);
        if ( type != 2 ) { //     Verifying time of forecast
            full = full + step;
        }

        return full.tostring(format);

    }

    string endDate(const XmlNode& node)
    {
        string format=  node.getAttribute("format");
        if ( format.empty() )
            format =  "%A %d %B %Y %H%M UTC";
        const long day =  grib_.getLong("date");
        const long hour = grib_.getLong("hour");
        const long mn =  grib_.getLong("minute");
        const long step = computeStep(grib_, "endStep");

        MagDate part1 = MagDate(day);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);
        full = full + step;

        return full.tostring(format);
    }

    void visit(const XmlNode& node)
    {
        if ( magCompare(node.name(), "grib_info") )
        {
            string grib = node.getAttribute("id");
            string where = node.getAttribute("where");
            if ( !grib_.id(grib, where)) {

                return;
            }
            string def = node.getAttribute("key");
            if (def.empty()) {
                def = node.getAttribute("definition");
                // for backward compatibility with the first version!
            }
            if ( def== "valid-date") {
                title_.update("grib"+grib, def, validDate(node));
                return;
            }

            if ( def == "base-date") {
                title_.update("grib"+grib, def, baseDate(node));
                return;
            }
            if ( def == "MV_Format" ) {
                title_.update("grib"+grib, def, "grib");
                return;
            }
            if ( def == "MV_Index"  || def == "MV_Frame" ||  def == "MV_Value" ) {
                // this is handled by Metview, so we just ignore it here
                return;
            }
            if ( def == "start-date" ) {
                title_.update("grib"+grib, def, startDate(node));
                return;
            }
            if ( def == "end-date" ) {
                title_.update("grib"+grib, def, endDate(node));
                return;
            }
            string val;
            string readAsLong = node.getAttribute("readAsLong");
            if(readAsLong != "yes")
            {
                val =  grib_.getString(def);
                string format = node.getAttribute("format");
                if ( !format.empty() ) {
                    char tmp[256];
                    sprintf(tmp, format.c_str(), val.c_str());
                    val = tmp;

                }
            }
            else
            {
                long longVal = grib_.getLong(def);
                std::stringstream sst ;
                sst << longVal;
                val=sst.str();
            }

            if ( val.empty() )
                val =  node.getAttribute("default");
            title_.update("grib"+grib, def, val);
        }

        if ( magCompare(node.name(), "magics_title") )
        {
            string grib = node.getAttribute("id");
            string where = node.getAttribute("where");
            if ( !grib_.id(grib, where)) return;

            vector<string> lines;
            TitleTemplate::title(lines, grib_);
            for (unsigned int i = 0; i < lines.size(); i++)
            {
                string id = grib_.title() + "_" + tostring(i);
                title_.update("grib"+grib, id, lines[i]);
                string entry = "<grib_info definition=\'" + id + "\'/>";
                title_.addToTags("<magics_title/>", entry);
            }
            //title_.addLines(lines.size());
        }
        node.visit(*this);
    }

    void decode(const string& line)
    {
        XmlReader parser;
        XmlTree tree;

        ostringstream xml;
        xml << "<?xml version='1.0' ?> \n";
        xml << "<xml> \n";
        xml << line;
        xml << "\n</xml>";

        try {
            parser.decode(xml.str(), &tree);
            tree.visit(*this);
        }
        catch (MagicsException& e) {
            MagLog::debug() << e.what() << endl;
        }
    }
    string str() const { return out.str(); }
protected :
    GribDecoder& grib_;
    TagHandler& title_;
    ostringstream out;
};

void GribDecoder::visit(AnimationRules& )
{
}



// GribDecoder::nearestGridpoints
// For a list of input gridpoint locations, returns the locations and values for the closest actual data
// point to each.
// Note that we cannot yet apply the same method to all grid types because there is a bug in GRIB_API versions
// prior to 1.15.0 which means that grib_nearest_find() does not work with Lambert grids when we keep the
// same grib_nearest object.
void GribDecoder::nearestGridpoints(double *inlats, double *inlons, double *outlats, double *outlons, double *values, double *distances, int nb, string &representation)
{
    bool retainGribNearestHandle = false;
    grib_nearest *nearHandle = NULL;
    double outlats4[4];   // grib_nearest_find returns 4 results
    double outlons4[4];   // grib_nearest_find returns 4 results
    double outvals4[4];   // grib_nearest_find returns 4 results
    double outdist4[4];   // grib_nearest_find returns 4 results
    int    outindexes[4]; // grib_nearest_find returns 4 results
    size_t len;

    if (representation == "regular_ll" ||
        representation == "reduced_ll" ||
        representation == "regular_gg" ||
        representation == "reduced_gg")
    {
        retainGribNearestHandle = true;  // more efficient
    }

    nearHandle = nearest_point_handle(retainGribNearestHandle);

    for (int i=0; i < nb; i++)
    {
        if (nearHandle)
        {
            grib_nearest_find(nearHandle, handle_, inlats[i], inlons[i], GRIB_NEAREST_SAME_GRID, outlats4, outlons4,
                outvals4, outdist4, outindexes, &len);
            vector<double> vdistances(outdist4, outdist4+4);
            int closestIndex = distance(vdistances.begin(), min_element(vdistances.begin(), vdistances.end()));
            outlats[i]   = outlats4[closestIndex];
            outlons[i]   = outlons4[closestIndex];
            values[i]    = outvals4[closestIndex];
            distances[i] = outdist4[closestIndex];
        }
        else
        {
            outlats[i]   = 0;
            outlons[i]   = 0;
            values[i]    = 0;
            distances[i] = 0;
        }
//          grib_nearest_find_multiple(handle_, 0, inlats, inlons, nb, outlats, outlons, values, distances, indexes);
//          int closestIndex = 0;
    }


    if (!retainGribNearestHandle && nearHandle) // was this a temporary handle?
        grib_nearest_delete(nearHandle);
}


void GribDecoder::visit(ValuesCollector& points)
{
    field_ = open(field_);

    points.setCollected(true);

    int nb = points.size();
    double inlats[nb];
    double inlons[nb];
    double outlats[nb];
    double outlons[nb];
    double values[nb];
    double x[nb];
    double y[nb];
    double distances[nb];

    double scaling, offset;
    string oriUnits, derivedUnits;
    string representation = getString("typeOfGrid");

    //Scaling works only for scalar data!!!

    try
    {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }
        interpretor_->scaling(*this, scaling, offset,oriUnits,derivedUnits);
    }
    catch (NoFactoryException&)
    {
        MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not supported.\n"<< std::endl;;
        scaling =1 ; offset =0;
    }

    for (int i =0; i < nb; i++)
    {

        inlats[i] = points[i].y();
        inlons[i] = std::fmod(points[i].x(),360.);
        if(inlons[i] < 0.) inlons[i]+=360.;
        i++;
    }

    double missing = getDouble("missingValue");

    if ( Data::dimension_ == 1 ) {
        bool scaled=(scaling==1 && offset == 0)?false:true;
        points.setScaled(scaled);
        points.setUnits(oriUnits);
        points.setScaledUnits(derivedUnits);

        field_ = open(field_);

        nearestGridpoints(inlats, inlons, outlats, outlons, values, distances, nb, representation);

        for (int i=0; i < nb; i++)
        {
            points[i].push_back(new ValuesCollectorData(outlons[i],outlats[i],values[i],distances[i]));
            if(scaled)
                points[i].back()->setScaledValue(scaling*values[i] + offset);
            if(values[i] == missing)
                points[i].back()->setMissing(true);
        }
    }
    else { //if ( Data::dimension_ == 2  ) {

        oriUnits=getString("units",false);
        if(oriUnits.find("/") == string::npos)
        {
            oriUnits=oriUnits + "/" + oriUnits;
        }
        points.setUnits(oriUnits);
        points.setScaledUnits("/");


        openFirstComponent();
        nearestGridpoints(inlats, inlons, outlats, outlons, x, distances, nb, representation);
        openSecondComponent();
        nearestGridpoints(inlats, inlons, outlats, outlons, y, distances, nb, representation);
        for (int i =0; i < nb; i++)
        {
            points[i].push_back(wind_mode_->values(outlons[i],outlats[i],x[i],y[i], distances[i]));
            if(x[i] == missing || y[i] == missing)
                points[i].back()->setMissing(true);
        }
    }
}

void GribDecoder::visit(MagnifierCollector& magnifier)
{
    const Transformation& transformation = magnifier.transformation();
    PointsHandler& points = this->points(transformation);
    points.setToFirst();

    while  ( points.more() )
    {
        magnifier.push_back(transformation(points.current()));
        points.advance();
    }
}

const DateDescription& GribDecoder::timeStamp()
{
    vector<string> need;
    need.push_back("<grib_info key='valid-date' format='%Y-%m-%d %H:%M:00'/>");
    need.push_back("<grib_info key='level'/>");
    need.push_back("<grib_info key='typeOfLevel'/>");

    TagHandler helper;
    GribTag tag1(*this, helper);
    for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
        tag1.decode(*t);

    timeStamp_ = DateDescription(helper.get("grib", "valid-date"), index_, internalIndex_);
    dataLevel_ = LevelDescription::level(helper.get("grib", "typeOfLevel"), tonumber(helper.get("grib", "level")), index_, internalIndex_);

    return timeStamp_;
}

const LevelDescription& GribDecoder::level()
{
    timeStamp();
    return dataLevel_;
}

void GribDecoder::visit(MetaDataVisitor& meta)
{
    if ( !handle_ )
        return;
    vector<string> need;

    need.push_back("<grib_info key='shortName'/>");
    need.push_back("<grib_info key='name'/>");
    need.push_back("<grib_info key='level'/>");
    need.push_back("<grib_info key='base-date' format='%Y-%m-%d %H:%M:00'/>");
    need.push_back("<grib_info key='valid-date' format='%Y-%m-%d %H:%M:00'/>");

    TagHandler helper;
    GribTag tag1(*this, helper);
    for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
    {
        tag1.decode(*t);
    }

    ostringstream grib;
    grib << "{\"level\":\""<<  helper.get("grib", "level") << "\",";
    grib << "\"name\":\""<<  helper.get("grib", "name") << "\",";
    grib << "\"base-date\":\""<<  helper.get("grib", "base-date") << "\",";
    grib << "\"valid-date\":\""<<  helper.get("grib", "valid-date") << "\"}";

    meta.add("grib", grib.str());
}

void GribDecoder::visit(MetaDataCollector& step)
{
    // Here we gather information for the label!
    const Transformation& transformation = step.transformation();

    field_ = open(field_); // just to be sure the file is opened!

    initInfo();

    //Collect infos
    if(step.empty())
    {
        MetviewIcon::visit(step);
        return;
    }

    try {
        bool members=false;
        vector<string> need;
        if(name_.empty())
        {
            members=true;
            need.push_back("<grib_info key='shortName'/>");
            need.push_back("<grib_info key='level'/>");
            need.push_back("<grib_info key='start-date' format='%Y-%m-%d %H:%M:00'/>");
            need.push_back("<grib_info key='end-date' format='%Y-%m-%d %H:%M:00'/>");
        }

        for(map<string, string>::iterator key = step.begin(); key != step.end(); ++key )
        {
            //If key is not found in information we use gribapi
            if(information_.find(key->first) == information_.end())
            {
                //Compute stats
                if (step.attribute(key->first).group() == MetaDataAttribute::StatsGroup)
                {
                    stats_.clear();


                    PointsHandler& points = this->points(transformation, false);

                    points.setToFirst();

                    while( points.more() )
                    {
                        stats_["value"].push_back(points.current().value());
                        points.advance();
                    }

                    computeStats();


                }
                //We use gribapi
                else if(step.attribute(key->first).source() == MetaDataAttribute::AnySource ||
                        step.attribute(key->first).source() == MetaDataAttribute::GribApiSource)
                {
                    if(step.attribute(key->first).type() != MetaDataAttribute::NumberType)
                    {
                        need.push_back("<grib_info key='"+key->first+ "'/>");
                    }
                    else
                    {
                        need.push_back("<grib_info key='"+key->first+ "' readAsLong='yes'/>");
                    }

                }
                else if(key->first == "scaling_formula" ||key->first == "scaled_units" )
                {
                    double scaling, offset;
                    string oriUnits, derivedUnits;
                    string representation = getString("typeOfGrid");
                    try
                    {
                        auto_ptr<GribInterpretor> interpretor_(SimpleObjectMaker<GribInterpretor>::create(representation));
                        interpretor_->scaling(*this, scaling, offset,oriUnits,derivedUnits);
                        if(scaling==1 && offset == 0)
                        {
                            information_["scaling_formula"]="";
                            information_["scaled_units"]="";
                        }
                        else
                        {
                            string offsetSign=(offset >=0)?"+":"-";
                            information_["scaling_formula"]="(value * " + tostring(scaling) + ") " + offsetSign + " " + tostring(fabs(offset));
                            information_["scaled_units"]= derivedUnits;
                        }
                    }
                    catch (NoFactoryException&)
                    {
                        MagLog::warning() << "Grib Decoder: Representation [" << representation << "] not supported.\n"<< std::endl;;
                        information_[key->first]="N/A";
                    }

                }

                else if(key->first == "isOctahedral")
                {
                    // Octahedral reduced Gaussian grids:
                    // - only reduced Gaussian grids can be octahedral
                    // - the isOctahedral key only works properly with global fields
                    string representation = getString("typeOfGrid");
                    if (representation == "reduced_gg")
                    {
                        if (getLong("global") == 1)
                        {
                            information_["isOctahedral"] = (getLong("isOctahedral") == 1) ? "yes" : "no";
                        }
                    }
                }
/*
                // alternative code, for if we want to display in one line the Gaussian
                // number and the octahedral flag.
                else if(key->first == "GaussianNumber")
                {
                    // e.g. N200 ('classic' reduced Gaussian) or O1280 ('octahedral' reduced Gaussian)
                    // - but only use prefix of N or O on global fields, because, as explained below,
                    //   we don't know whether a sub-areas field is classic or octahedral
                    // Octahedral reduced Gaussian grids:
                    // - only reduced Gaussian grids can be octahedral
                    // - the isOctahedral key only works properly with global fields
                    string representation = getString("typeOfGrid");
                    if (representation == "reduced_gg")
                    {
                        long N = getLong("N");
                        string prefix("");
                        if (getLong("global") == 1)
                        {
                            prefix = (getLong("isOctahedral") == 1) ? "O" : "N";
                        }
                        information_["GaussianNumber"] = prefix + tostring(N);
                    }
                }*/
            }
            
            //If key is found in information_ we copy it
            if(information_.find(key->first) != information_.end())
            {
                key->second=information_[key->first];
            }
        }


        if(!need.empty())
        {
            TagHandler helper;
            GribTag tag1(*this, helper);
            for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
            {
                tag1.decode(*t);
            }

            if(members)
            {
                name_ = helper.get("grib", "shortName") +  " " +  helper.get("grib", "level");

                from_ = DateTime(helper.get("grib", "start-date"));
                to_ =  DateTime(helper.get("grib", "end-date"));
            }

            for(map<string, string>::iterator key = step.begin(); key != step.end(); ++key )
            {
                if(information_.find(key->first) == information_.end())
                {
                    if(step.attribute(key->first).source() == MetaDataAttribute::AnySource ||
                            step.attribute(key->first).source() == MetaDataAttribute::GribApiSource)
                    {
                        key->second = helper.get("grib", key->first);
                        setInfo(key->first,key->second);
                    }
                }

            }

        }
    }

    catch (...) {}

}
MatrixHandler& GribDecoder::direction() {

    decode2D();
    // Now X et X components are ready ..
    // We compute the direction in matrix_  and send it back.

    matrix_ = xComponent_;

    vector<double>::const_iterator x = xComponent_->begin();
    vector<double>::const_iterator y = yComponent_->begin();
    vector<double> directions;
    //  MagLog::dev()<< "missing1-->" << in1->missing() << endl;
    //  MagLog::dev()<< "missing2-->" << in2->missing() << endl;
    while ( x != xComponent_->end() &&  x != yComponent_->end() ) {
        if ( *x == xComponent_->missing() || *y == yComponent_->missing() )
            directions.push_back(xComponent_->missing());
        else
            directions.push_back(atan2((*x), (*y)) );
        ++x;
        ++y;
    }
    matrix_->clear();
    xComponent_ = 0;



    for (vector<double>::iterator d = directions.begin(); d != directions.end(); ++d) {
        matrix_->push_back(*d);
    }

    matrixHandlers_.push_back(new MatrixHandler(*matrix_));
    return *(matrixHandlers_.back());

}
void GribDecoder::decode(const Transformation& transformation)
{
    if (matrix_) return;

    field_ = open(field_);

    read(&matrix_, transformation);
    if (!matrix_) return;

    // here we build information for the layers!
    TagHandler helper;
    vector<string> need;
    need.push_back("<grib_info id=\'" + id_ +"\' key='shortName'/>");
    need.push_back("<grib_info id=\'" + id_ +"\' key='level'/>");
    need.push_back("<grib_info id=\'" + id_ +"\'  key='start-date' format='%Y-%m-%d %H:%M:00'/>");
    need.push_back("<grib_info id=\'" + id_ +"\' key='end-date' format='%Y-%m-%d %H:%M:00'/>");
    GribTag tag1(*this, helper);

    for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
    {
        tag1.decode(*t);
    }

    name_ = helper.get("grib"+id_, "shortName") +  "-" +  helper.get("grib"+id_, "level");
    name_ = iconName_;
    layerId_ = name_ + file_name_;
    from_ = DateTime(helper.get("grib"+id_, "start-date"));
    to_ =  DateTime(helper.get("grib"+id_, "end-date"));
}

void GribDecoder::decode()
{

    if ( dimension_ == 1) {
        if (matrix_) return;
        field_ = open(field_);
        read(&matrix_);
        if (!matrix_) return;
    }
    else {
        decode2D();
    }


    // here we build information for the layers!
    TagHandler helper;
    vector<string> need;
    need.push_back("<grib_info id=\'" + id_ +"\' key='shortName'/>");
    need.push_back("<grib_info id=\'" + id_ +"\' key='level'/>");
    need.push_back("<grib_info id=\'" + id_ +"\'  key='start-date' format='%Y-%m-%d %H:%M:00'/>");
    need.push_back("<grib_info id=\'" + id_ +"\' key='end-date' format='%Y-%m-%d %H:%M:00'/>");
    GribTag tag1(*this, helper);

    for ( vector<string>::const_iterator t = need.begin(); t != need.end(); ++t )
    {
        tag1.decode(*t);
    }

    name_ = helper.get("grib"+id_, "shortName") +  "-" +  helper.get("grib"+id_, "level");
    name_ = iconName_;
    layerId_ = name_ + file_name_;
    from_ = DateTime(helper.get("grib"+id_, "start-date"));
    to_ =  DateTime(helper.get("grib"+id_, "end-date"));
}



void GribDecoder::visit(TextVisitor& title)
{
    try {
        field_ = open(field_);
    }
    catch ( ... )
    {
        return;
    }

    vector<string> titles;

    title.titles(titles);
    GribTag tag(*this, title);

    for ( vector<string>::const_iterator t = titles.begin(); t != titles.end(); ++t ) {
        tag.decode(*t);
    }
}



void GribDecoder::decodeRaster(const Transformation& transformation)
{
    field_ = open(field_);

    string representation = getString("typeOfGrid");

    try {
        if ( !interpretor_ ) {
            interpretor_ = SimpleObjectMaker<GribInterpretor>::create(representation);
        }
        interpretor_->interpretAsRaster(*this, raster_, transformation);
    }

    catch (NoFactoryException&)
    {
        MagLog::error() << "Grib Decoder: Raster representation [" << representation << "] not supported.\n";
        throw MagicsException("Grib Decoder: Raster representation [] not supported.");
    }
}


void GribDecoder::initInfo()
{
    if(information_.find("_datatype") == information_.end())
    {
        setInfo("_datatype","GRIB");

        char buf[1024];
        int count = readlink(file_name_.c_str(), buf, sizeof(buf));
        if (count > 0)
        {
            buf[count] = '\0';
            setInfo("path", string(buf));
        }
        else
        {
            setInfo("path", file_name_);
        }
        setInfo("MV_Format","GRIB");
    }
}



namespace magics {



class GribInfo
{
public:
    GribInfo() {}
    virtual ~GribInfo() {}
    virtual void operator()(ostream&, const GribDecoder&) = 0;
};

class GribParameter : public GribInfo
{
public:
    GribParameter() {}
    ~GribParameter() {}
    void operator()(ostream& out, const GribDecoder& grib)
    {
        string val = grib.getString("name");
        out << val;
    }
};

class GribParamCriter : public MatchCriteria
{
public:
    GribParamCriter() {}
    ~GribParamCriter() {}
    bool verify(const GribDecoder& grib, const string&, const string& val)
    {
        long param = grib.getLong("paramId");
        return (tostring(param) == val);
    }
};

class GribLocalCriter : public MatchCriteria
{
public:
    GribLocalCriter() {}
    ~GribLocalCriter() {}
    bool verify(const GribDecoder& grib, const string& param, const string& val)
    {
        string key = param;
        string criter = grib.getString(key, false); // 'false' to avoid warnings if key is absent
        MagLog::debug() << "I am verifing " << param << " for a GribDecoder : " << criter << " ==  " << val << "???" << "\n";
        return (criter == val);
    }
};

class GribObsDiagCriter : public MatchCriteria
{
public:
    GribObsDiagCriter() {}
    ~GribObsDiagCriter() {}
    bool verify(const GribDecoder& grib, const string&, const string& )
    {
        string param = grib.getString("observationDiagnostic", false); // do not warn if the key is absent
        return (param != "");
    }
};


class GribLocalDefHandler : public TitleFieldHandler
{
public:
    GribLocalDefHandler() {}
    ~GribLocalDefHandler() {}

    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {

        ostringstream out;
        string local = grib.getString("localDefinitionNumber");
        out << "local definition =" << local << " ";
        title.back() += out.str();
    }
};

class GribObsDiagHandler : public TitleFieldHandler
{
public:
    GribObsDiagHandler() {}
    ~GribObsDiagHandler() {}

    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {

        ostringstream out;
        string local = grib.getString("observationDiagnostic");
        out << "diagnostic =" << local << " ";
        title.back() += out.str();
    }
};

class GribObstatHandler : public TitleFieldHandler
{
public:
    GribObstatHandler() {}
    ~GribObstatHandler() {}

    void operator()(TitleField&, vector<string>& /*title*/, const GribDecoder& grib)
    {


    }
};

class GribLocalHandler : public TitleFieldHandler
{
public:
    GribLocalHandler(const string& local) : local_(local) {}
    ~GribLocalHandler() {}

    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {

        ostringstream out;
        string code = grib.getString(local_);
        out << local_ << "=" << code  << " ";
        title.back()+= out.str();
    }
protected :
    string local_;
};

class GribStreamHandler : public GribLocalHandler
{
public:
    GribStreamHandler() : GribLocalHandler("marsStream") {}
    ~GribStreamHandler() {}
};

class GribClassHandler : public GribLocalHandler
{
public:
    GribClassHandler() : GribLocalHandler("marsClass") {}
    ~GribClassHandler() {}
};

class GribTypeHandler : public GribLocalHandler
{
public:
    GribTypeHandler() : GribLocalHandler("marsType") {}
    ~GribTypeHandler() {}
};


class GribParamHandler : public TitleFieldHandler
{
public:
    GribParamHandler() {}
    ~GribParamHandler() {}
    virtual void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {

        string param = grib.getString("name");
        title.back()+=param;
    }
};


class GribKeyHandler : public TitleFieldHandler
{
public:
    GribKeyHandler() {}
    ~GribKeyHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {


        char x[256];

        string key = field.attribute("key", "");
        string value  = grib.getString(key);
        string format = field.attribute("format", "%s");
        sprintf(x, format.c_str(), value.c_str());

        title.back() += string(x);

    }
};



class GribBaseDateHandler : public TitleFieldHandler
{
public:
    GribBaseDateHandler() {}
    ~GribBaseDateHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {

        ostringstream out;

        long date = grib.getLong("dataDate");
        long hour = grib.getLong("hour");
        long mn =  grib.getLong("minute");
        string x = grib.getString("dataDate");
        cout << x << endl;

        MagDate part1 = MagDate(date);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);
        const long type  = grib.getLong("significanceOfReferenceTime", false);
        if ( type == 2 ) { //     Verifying time of forecast
            long step = grib.getLong("step"); // needs steps in second!   // default is in hours. Set 'stepUnits' to change.
            full = full + (step*-3600);
        }

        string format = field.attribute("format", "%A %d %B %Y at %H%M UTC");

        title.back() += full.tostring(format);
    }
};


class GribValidDateHandler : public TitleFieldHandler
{
public:
    GribValidDateHandler() {}
    ~GribValidDateHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {


        ostringstream out;
        long date = grib.getLong("dataDate");
        long hour = grib.getLong("hour");
        long mn   = grib.getLong("minute");


        long step = computeStep(grib, "step");

        MagDate part1 = MagDate(date);
        MagTime part2 = MagTime(hour, mn, 0);
        DateTime full(part1, part2);
        const long type  = grib.getLong("significanceOfReferenceTime", false);
        if ( type != 2 ) { //     Verifying time of forecast
            full = full + step;
        }


        string format = field.attribute("format", "%A %d %B %Y %H%M UTC");

        title.back() += full.tostring(format);


    }
};

class GribStepHandler : public TitleFieldHandler
{
public:
    GribStepHandler() {}
    ~GribStepHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {

        ostringstream out;
        long istep = grib.getLong("startStep");


        ostringstream step;
        step << istep;
        string format = field.attribute("format", "t+%s");
        out << SimpleStringFormat(step.str(), format);
        title.back() += out.str();
    }
};


class GribLevelHandler : public TitleFieldHandler
{
public:
    GribLevelHandler() {
        if (map_.empty()) {
            map_["Surface"] = &GribLevelHandler::surface;
            map_["Unknown"] = &GribLevelHandler::surface;
            map_["isobaricInhPa"] = &GribLevelHandler::isobaricInhPa;
            map_["heightAboveGround"] = &GribLevelHandler::heightAboveGround;
            map_["heightAboveGround"] = &GribLevelHandler::heightAboveGround;
            map_["hybrid"] = &GribLevelHandler::hybrid;
        }
    }

    ~GribLevelHandler() {}
    typedef string (GribLevelHandler::*Builder)(const string& def, const GribDecoder& grib) const;

    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {
        ostringstream out;

        string level = grib.getString("typeOfLevel");


        map<string,  GribLevelHandler::Builder>::iterator help = map_.find(level);
        if ( help != map_.end() ) out << (this->*help->second)(level, grib) << " ";
        else out << level << " ";

        title.back() +=  out.str();

    }

protected:
    static map<string, GribLevelHandler::Builder> map_;

    string surface(const string&, const GribDecoder& ) const
    {
        return "";
    }
    string unknown(const string& level, const GribDecoder& ) const
    {
        ostringstream out;
        out << "Unknown type of level[" << level << "]";
        return out.str();
    }
    string isobaricInhPa(const string& , const GribDecoder& grib) const
    {
        ostringstream out;
        long level = grib.getLong("level");
        out  << level  <<  " " << "hPa";
        return out.str();
    }
    string pv(const string& , const GribDecoder& grib) const
    {
        ostringstream out;
        long level = grib.getLong("level");
        out  << level  <<  " mPVU";
        return out.str();
    }
    string heightAboveGround(const string& , const GribDecoder& grib) const
    {
        ostringstream out;
        long level = grib.getLong("level");
        out  << level  <<  " m";
        return out.str();
    }
    string hybrid(const string& , const GribDecoder& grib) const
    {
        ostringstream out;
        long level = grib.getLong("level");
        out  << "Model level " << level;
        return out.str();
    }
};
map<string, GribLevelHandler::Builder> GribLevelHandler::map_;


class GribTimeHandler : public TitleFieldHandler
{
public:
    GribTimeHandler() {}
    ~GribTimeHandler() {}
    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {
        //        if (!grib.getText()) return;
        //        ostringstream out;
        //        grib_int_t idate;
        //        grib_get(grib.id(),(grib_string_t*)"time","I",&idate);
        //
        //        out << "Time:" << idate;
        //        title.add(out.str());

        title.back() +=  "Time? ";
    }
};

class GribCentreHandler : public TitleFieldHandler
{
public:
    GribCentreHandler() {}
    ~GribCentreHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {
        string format = field.attribute("format", "%s");
        string style = field.attribute("style", "short");
        string centre = grib.getString("centre");

        title.back() += centre;
    }
};

class GribProductHandler : public TitleFieldHandler
{
public:
    GribProductHandler() {}
    ~GribProductHandler() {}
    void operator()(TitleField&, vector<string>& title, const GribDecoder& grib)
    {

        long type = grib.getLong("type");

        GeneralDef def = TypeTable::definition(type);
        title.back() += def.longTitle();
    }
};


class GribPlotTypeHandler : public TitleFieldHandler
{
public:
    GribPlotTypeHandler() {}
    ~GribPlotTypeHandler() {}
    void operator()(TitleField&, vector<string>&,const GribDecoder&)
    {
        //MagLog::warning() << "Plot Type: not implemented--> wait for the specification." << "\n";
    }
};


class NewLineHandler : public TitleFieldHandler
{
public:
    NewLineHandler() {}
    ~NewLineHandler() {}
    void operator()(TitleField&, vector<string>& title,const GribDecoder&)
    {
        title.push_back("");
    }
};


class SatelliteHandler : public TitleFieldHandler
{
public:
    SatelliteHandler()  {}
    ~SatelliteHandler() {}
    void operator()(TitleField&, vector<string>& title,const GribDecoder& grib)
    {
        static map<long, string> names;
        if ( names.empty() ){
            names[54] = "METEOSAT-7";
            names[55] = "METEOSAT-8";
            names[57] = "METEOSAT-10";
            names[172] = "MTSAT-2";
            names[257] = "GOES-13";
            names[259] = "GOES-15";
        }

        long ident =  grib.getLong("ident");
        map<long, string>::iterator sat = names.find(ident);

        if ( sat != names.end() )
            title.back() += sat->second;
        else
            title.back() += "satellite identifier " + tostring(ident);
    }
};

class ChannelHandler : public TitleFieldHandler
{
public:
    ChannelHandler()  {}
    ~ChannelHandler() {}
    void operator()(TitleField&, vector<string>& title,const GribDecoder& grib)
    {
        map<long, map<long, string> > channels;
        if ( channels.empty() ) {
            map<long, string>  l54;
            l54[1] = "WV 6-4";
            l54[2] = "IR 11-5";
            l54[3] = "VIS 00-7";
            channels[54] = l54;
            map<long, string>  l55;
            l55[1] = "VIS 0-6";
            l55[4] = "IR 3-9";
            l55[5] = "WV 6-2";
            l55[6] = "WV 7-3";
            l55[8] = "IR 9-7";
            l55[9] = "IR 10-8";
            l55[10] = "IR 12-0";
            channels[55] = l55;
            map<long, string>  l57;
            l57[1] = "VIS 0-6";
            l57[4] = "IR 3-9";
            l57[5] = "WV 6-2";
            l57[6] = "WV 7-3";
            l57[8] = "IR 9-7";
            l57[9] = "IR 10-8";
            l57[10] = "IR 12-0";
            channels[57] = l57;
            map<long, string>  l172;
            l172[2] = "IR 10-8";
            l172[4] = "WV 6-8";
            l172[9] = "IR 10-8";
            channels[172] = l172;
            map<long, string>  l257;
            l257[1] = "VIS 00-7";
            l257[3] = "WV 6-6";
            l257[4] = "IR 10-7";
            channels[257] = l257;
            map<long, string>  l259;
            l259[1] = "VIS 00-7";
            l259[3] = "WV 6-6";
            l259[4] = "IR 10-7";
            channels[259] = l259;
        }
        ostringstream out;
        long ident = grib.getLong("ident");
        long band = grib.getLong("channel");

        map<long, map<long, string> >::iterator sat = channels.find(ident);

        if ( sat == channels.end() )  {
            title.back() += "channel " + tostring(band);
            return;
        }
        map<long, string>::iterator channel = sat->second.find(band);
        if ( channel == sat->second.end() )  {
            title.back() += "channel " + tostring(band);
            return;
        }

        title.back() += channel->second;
    }
};


class GribExpverHandler : public TitleFieldHandler
{
public:
    GribExpverHandler() {}
    ~GribExpverHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {
        if ( !grib.getExpver() ) return;
        ostringstream out;
        string expver = grib.getString("mars.experimentVersionNumber");
        string format = field.attribute("format", "Expver=%s");
        out << SimpleStringFormat(expver, format);
        title.back() += out.str();
    }
};


class GribEpsNumberInfoHandler : public TitleFieldHandler
{
public:
    GribEpsNumberInfoHandler() {}
    ~GribEpsNumberInfoHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {

        //       if (!grib.getText()) return;
        //        ostringstream out;
        //       grib_int_t local;
        //       grib_get(grib.id(),(grib_string_t*)"localDefinition","I","localDefinitionNumber",&local);
        //       if (local != 1) return;
        //
        //       char number[1024];
        //       grib_get(grib.id(),(grib_string_t*)"localDefinition","s","total",number);
        //       string format = field.attribute("format", "(%s members)");
        //
        //      out << SimpleStringFormat(number, format);
        //        title.add(out.str());

        title.back() +=  "epsnumber?";
    }
};


class GribUnitHandler : public TitleFieldHandler
{
public:
    GribUnitHandler() {}
    ~GribUnitHandler() {}
    void operator()(TitleField& field, vector<string>& title, const GribDecoder& grib)
    {
        /*
        if (!grib.getText()) return;
        if ( !grib.getUnits() ) return;
        ostringstream out;

        double id   = grib.getDouble("paramId");
        long centre  = grib.getLong("centre");

        long param = (long) id;
        long table   = (id - param )*100;


        const ParamDef& parameter = LocalTable::localInfo(param, table, centre);

        string format = field.attribute("format", "Units:%s");
        string unit = (grib.getScaling()) ? parameter.derivedUnit() :  parameter.originalUnit();
        out << SimpleStringFormat(unit, format);


        title.back() += out.str();
         */
    }
};

double GribDecoder::uComponent(int index)
{
    return xValues_[index];
}

double GribDecoder::vComponent(int index)
{
    return yValues_[index];
}

void GribDecoder::uComponent()
{
    if ( xValues_ )
        return;
    size_t nb;
    string name;
    grib_handle* handle = uHandle(name);

    grib_get_size(handle, "values", &nb);
    xValues_ = new double[nb];
    grib_get_double_array(handle, "values", xValues_, &nb);
}

void GribDecoder::vComponent()
{
    if ( yValues_ )
        return;

    string name;
    grib_handle* handle = vHandle(name);
    size_t nb;
    grib_get_size(handle, "values", &nb);

    yValues_ = new double[nb];
    grib_get_double_array(handle, "values", yValues_, &nb);
}
}// end namespace magics



static SimpleObjectMaker<GribParamCriter, MatchCriteria > gribparamcriter("parameter");
static SimpleObjectMaker<GribParamHandler, TitleFieldHandler > gribparamhandler("parameter");
static SimpleObjectMaker<GribBaseDateHandler, TitleFieldHandler > gribbasedatehandler("base_date");
static SimpleObjectMaker<GribValidDateHandler, TitleFieldHandler > gribvaliddatehandler("valid_date");
static SimpleObjectMaker<GribStepHandler, TitleFieldHandler > gribstephandler("step");
static SimpleObjectMaker<GribEpsNumberInfoHandler, TitleFieldHandler > gribepsnumberhandler("eps_number_info");

static SimpleObjectMaker<GribTimeHandler, TitleFieldHandler > gribTimehandler("time");
static SimpleObjectMaker<GribLevelHandler, TitleFieldHandler > gribLevelhandler("level");
static SimpleObjectMaker<NewLineHandler, TitleFieldHandler > newlinehandler("newline");
static SimpleObjectMaker<GribKeyHandler, TitleFieldHandler > gribkeyhandler("gribapi");

static SimpleObjectMaker<GribStreamHandler, TitleFieldHandler > gribstreamhandler("stream");
static SimpleObjectMaker<GribClassHandler, TitleFieldHandler > gribclasshandler("class");
static SimpleObjectMaker<GribTypeHandler, TitleFieldHandler > gribtypehandler("type");
static SimpleObjectMaker<GribLocalDefHandler, TitleFieldHandler > griblocaldefhandler("localdef");
static SimpleObjectMaker<GribCentreHandler, TitleFieldHandler > gribcentrehandler("centre");
static SimpleObjectMaker<GribProductHandler, TitleFieldHandler > gribproducthandler("product");
static SimpleObjectMaker<GribUnitHandler, TitleFieldHandler > gribunithandler("units");
static SimpleObjectMaker<GribExpverHandler, TitleFieldHandler > gribexpverhandler("expver");
static SimpleObjectMaker<GribPlotTypeHandler, TitleFieldHandler > gribplottypehandler("plot_type");
static SimpleObjectMaker<SatelliteHandler, TitleFieldHandler > satellitehandler("satellite");
static SimpleObjectMaker<ChannelHandler, TitleFieldHandler > channelhandler("channel");
static SimpleObjectMaker<GribBaseDateHandler, TitleFieldHandler > datehandler("date");


static SimpleObjectMaker<GribObsDiagCriter, MatchCriteria > gribobsdiagriter("observationDiagnostic");
static SimpleObjectMaker<GribObsDiagHandler, TitleFieldHandler > gribobsdiaghandler("observationDiagnostic");
static SimpleObjectMaker<GribObstatHandler, TitleFieldHandler > gribobstathandler("obstat");


static SimpleObjectMaker<GribLocalCriter, MatchCriteria > gribstreamcriter("stream");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > gribtypecriter("type");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > gribclasscriter("class");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > typeOfgeneratingProcess("typeOfGeneratingProcess");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > timeRangeIndicator("timeRangeIndicator");
static SimpleObjectMaker<GribLocalCriter, MatchCriteria > dataType("dataType");

#include "GribRegularInterpretor.h"
static SimpleObjectMaker<GribRegularInterpretor, GribInterpretor> regular_ll("regular_ll");
static SimpleObjectMaker<GribReducedLatLonInterpretor, GribInterpretor> reduced_ll("reduced_ll");
static SimpleObjectMaker<GribRegularGaussianInterpretor, GribInterpretor> regular_gg("regular_gg");
static SimpleObjectMaker<GribReducedGaussianInterpretor, GribInterpretor> reduced_gg("reduced_gg");
static SimpleObjectMaker<GribRotatedInterpretor, GribInterpretor> rotated_ll("rotated_ll");
static SimpleObjectMaker<GribLambertAzimutalInterpretor, GribInterpretor> lambert_azimuthal_equal_area("lambert_azimuthal_equal_area");
static SimpleObjectMaker<GribLambertInterpretor, GribInterpretor> lambert("lambert");
static SimpleObjectMaker<GribPolarStereoInterpretor, GribInterpretor> polar("polar_stereographic");


#include "GribSatelliteInterpretor.h"
static SimpleObjectMaker<GribSatelliteInterpretor, GribInterpretor> satellite("space_view");

