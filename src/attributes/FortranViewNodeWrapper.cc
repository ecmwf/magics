
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file FortranViewNodeAttributes.h
    \\brief Definition of FortranViewNode Attributes class.

    This file is automatically generated.
    Do Not Edit!

*/

#include "MagRequest.h"
#include "FortranViewNodeWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "MagicsGlobal.h"

using namespace magics;



FortranViewNodeWrapper::FortranViewNodeWrapper(): fortranviewnode_(new FortranViewNode())


{


	

}
FortranViewNodeWrapper::FortranViewNodeWrapper(FortranViewNode* fortranviewnode): fortranviewnode_(fortranviewnode)
{

	
}

FortranViewNodeWrapper::~FortranViewNodeWrapper()
{

}

void FortranViewNodeWrapper::set(const MagRequest& request)
{

	

	if  (request.countValues("SUBPAGE_X_POSITION") ) {
		double left_value = request("SUBPAGE_X_POSITION");
		fortranviewnode_->left_ = left_value;
		}
	if  (request.countValues("SUBPAGE_Y_POSITION") ) {
		double bottom_value = request("SUBPAGE_Y_POSITION");
		fortranviewnode_->bottom_ = bottom_value;
		}
	if  (request.countValues("SUBPAGE_X_LENGTH") ) {
		double width_value = request("SUBPAGE_X_LENGTH");
		fortranviewnode_->width_ = width_value;
		}
	if  (request.countValues("SUBPAGE_Y_LENGTH") ) {
		double height_value = request("SUBPAGE_Y_LENGTH");
		fortranviewnode_->height_ = height_value;
		}
	if  (request.countValues("SUBPAGE_MAP_LIBRARY_AREA") ) {
		string predefined_value = request("SUBPAGE_MAP_LIBRARY_AREA");
		
		fortranviewnode_->predefined_ = MagTranslator<string, bool>()(predefined_value);
		
		}
	if  (request.countValues("SUBPAGE_MAP_AREA_NAME") ) {
		string predefined_name_value = request("SUBPAGE_MAP_AREA_NAME");
		fortranviewnode_->predefined_name_ = predefined_name_value;
		}
	if  (request.countValues("SUBPAGE_Y_POSITION_INTERNAL") ) {
		double bottom_internal_value = request("SUBPAGE_Y_POSITION_INTERNAL");
		fortranviewnode_->bottom_internal_ = bottom_internal_value;
		}
	if  (request.countValues("SUBPAGE_X_POSITION_INTERNAL") ) {
		double left_internal_value = request("SUBPAGE_X_POSITION_INTERNAL");
		fortranviewnode_->left_internal_ = left_internal_value;
		}
	if  (request.countValues("SUBPAGE_RIGHT_POSITION") ) {
		double right_value = request("SUBPAGE_RIGHT_POSITION");
		fortranviewnode_->right_ = right_value;
		}
	if  (request.countValues("SUBPAGE_Y_LENGTH_INTERNAL") ) {
		double height_internal_value = request("SUBPAGE_Y_LENGTH_INTERNAL");
		fortranviewnode_->height_internal_ = height_internal_value;
		}
	if  (request.countValues("SUBPAGE_X_LENGTH_INTERNAL") ) {
		double width_internal_value = request("SUBPAGE_X_LENGTH_INTERNAL");
		fortranviewnode_->width_internal_ = width_internal_value;
		}
	if  (request.countValues("SUBPAGE_TOP_POSITION") ) {
		double top_value = request("SUBPAGE_TOP_POSITION");
		fortranviewnode_->top_ = top_value;
		}
	if  (request.countValues("SUBPAGE_CLIPPING") ) {
		string clipping_value = request("SUBPAGE_CLIPPING");
		
		fortranviewnode_->clipping_ = MagTranslator<string, bool>()(clipping_value);
		
		}
	if  (request.countValues("SUBPAGE_FRAME") ) {
		string frame_value = request("SUBPAGE_FRAME");
		
		fortranviewnode_->frame_ = MagTranslator<string, bool>()(frame_value);
		
		}
	if  (request.countValues("SUBPAGE_FRAME_THICKNESS") ) {
		int frame_thickness_value = request("SUBPAGE_FRAME_THICKNESS");
		fortranviewnode_->frame_thickness_ = frame_thickness_value;
		}
	if  (request.countValues("SUBPAGE_VERTICAL_AXIS_WIDTH") ) {
		double vertical_axis_with_value = request("SUBPAGE_VERTICAL_AXIS_WIDTH");
		fortranviewnode_->vertical_axis_with_ = vertical_axis_with_value;
		}
	if  (request.countValues("SUBPAGE_HORIZONTAL_AXIS_HEIGHT") ) {
		double horizontal_axis_height_value = request("SUBPAGE_HORIZONTAL_AXIS_HEIGHT");
		fortranviewnode_->horizontal_axis_height_ = horizontal_axis_height_value;
		}
	if  (request.countValues("SUBPAGE_MAP_OVERLAY_CONTROL") ) {
		string overlay_value = request("SUBPAGE_MAP_OVERLAY_CONTROL");
		fortranviewnode_->overlay_ = overlay_value;
		}
	if  (request.countValues("SUBPAGE_ALIGN_HORIZONTAL") ) {
		string horizontal_value = request("SUBPAGE_ALIGN_HORIZONTAL");
		fortranviewnode_->horizontal_ = horizontal_value;
		}
	if  (request.countValues("SUBPAGE_ALIGN_VERTICAL") ) {
		string vertical_value = request("SUBPAGE_ALIGN_VERTICAL");
		fortranviewnode_->vertical_ = vertical_value;
		}
	if  (request.countValues("SUBPAGE_MAP_JSON_DEFINITION") ) {
		string json_value = request("SUBPAGE_MAP_JSON_DEFINITION");
		fortranviewnode_->json_ = json_value;
		}
	if  (request.countValues("AUTOMATIC_TITLE") ) {
		string title_value = request("AUTOMATIC_TITLE");
		
		fortranviewnode_->title_ = MagTranslator<string, bool>()(title_value);
		
		}
	if  (request.countValues("SUBPAGE_EXPAND_MODE") ) {
		string expand_value = request("SUBPAGE_EXPAND_MODE");
		
		fortranviewnode_->expand_ = MagTranslator<string, bool>()(expand_value);
		
		}
	
	
		string transformation_value = request.countValues("SUBPAGE_MAP_PROJECTION") ?  (string) request("SUBPAGE_MAP_PROJECTION") : "cylindrical";
	MagLog::debug() << " SUBPAGE_MAP_PROJECTION set to " << transformation_value << endl;
	TransformationWrapper* transformation_wrapper = 0;
	try
	{
		transformation_wrapper = SimpleFactory<TransformationWrapper>::create(transformation_value);
	}
	 catch (NoFactoryException&) {
		if (MagicsGlobal::strict()) {
            throw;
        }
		MagLog::warning() << "[" << transformation_value << "] is not a valid value for transformation: reset to default -> [cylindrical]" << endl;
		transformation_wrapper = SimpleFactory<TransformationWrapper>::create("cylindrical");
	}
	transformation_wrapper->set(request);
	fortranviewnode_->transformation_ =  unique_ptr<Transformation>(transformation_wrapper->object());
	delete transformation_wrapper;
	if  (request.countValues("SUBPAGE_BACKGROUND_COLOUR") ) {
		string background_value = request("SUBPAGE_BACKGROUND_COLOUR");
		fortranviewnode_->background_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(background_value));
	}
		
	if  (request.countValues("SUBPAGE_FRAME_COLOUR") ) {
		string frame_colour_value = request("SUBPAGE_FRAME_COLOUR");
		fortranviewnode_->frame_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(frame_colour_value));
	}
		
	
	if  (request.countValues("SUBPAGE_FRAME_LINE_STYLE") ) {
		string frame_line_style_value = request("SUBPAGE_FRAME_LINE_STYLE");
		fortranviewnode_->frame_line_style_ = MagTranslator<string, LineStyle>()(frame_line_style_value);
	}
		
	
		string preview_value = request.countValues("SUBPAGE_MAP_PREVIEW") ?  (string) request("SUBPAGE_MAP_PREVIEW") : "off";
	MagLog::debug() << " SUBPAGE_MAP_PREVIEW set to " << preview_value << endl;
	NoPreviewVisitorWrapper* preview_wrapper = 0;
	try
	{
		preview_wrapper = SimpleFactory<NoPreviewVisitorWrapper>::create(preview_value);
	}
	 catch (NoFactoryException&) {
		if (MagicsGlobal::strict()) {
            throw;
        }
		MagLog::warning() << "[" << preview_value << "] is not a valid value for preview: reset to default -> [off]" << endl;
		preview_wrapper = SimpleFactory<NoPreviewVisitorWrapper>::create("off");
	}
	preview_wrapper->set(request);
	fortranviewnode_->preview_ =  unique_ptr<NoPreviewVisitor>(preview_wrapper->object());
	delete preview_wrapper;
	
		string magnify_value = request.countValues("SUBPAGE_MAP_MAGNIFIER") ?  (string) request("SUBPAGE_MAP_MAGNIFIER") : "off";
	MagLog::debug() << " SUBPAGE_MAP_MAGNIFIER set to " << magnify_value << endl;
	NoMagnifierVisitorWrapper* magnify_wrapper = 0;
	try
	{
		magnify_wrapper = SimpleFactory<NoMagnifierVisitorWrapper>::create(magnify_value);
	}
	 catch (NoFactoryException&) {
		if (MagicsGlobal::strict()) {
            throw;
        }
		MagLog::warning() << "[" << magnify_value << "] is not a valid value for magnify: reset to default -> [off]" << endl;
		magnify_wrapper = SimpleFactory<NoMagnifierVisitorWrapper>::create("off");
	}
	magnify_wrapper->set(request);
	fortranviewnode_->magnify_ =  unique_ptr<NoMagnifierVisitor>(magnify_wrapper->object());
	delete magnify_wrapper;
	
}

void FortranViewNodeWrapper::print(ostream& out)  const
{
	out << "FortranViewNodeWrapper[]";
}


#include "GeoRectangularProjectionWrapper.h"
static SimpleObjectMaker<GeoRectangularProjection, Transformation> FortranViewNode_subpage_map_projection_cylindrical ("cylindrical");
static SimpleObjectMaker<GeoRectangularProjectionWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_cylindrical_wrapper ("cylindrical");


#include "PolarStereographicProjectionWrapper.h"
static SimpleObjectMaker<PolarStereographicProjection, Transformation> FortranViewNode_subpage_map_projection_polar_stereographic ("polar_stereographic");
static SimpleObjectMaker<PolarStereographicProjectionWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_polar_stereographic_wrapper ("polar_stereographic");


#include "Proj4PolarNorthWrapper.h"
static SimpleObjectMaker<Proj4PolarNorth, Transformation> FortranViewNode_subpage_map_projection_polar_north ("polar_north");
static SimpleObjectMaker<Proj4PolarNorthWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_polar_north_wrapper ("polar_north");


#include "Proj4PolarSouthWrapper.h"
static SimpleObjectMaker<Proj4PolarSouth, Transformation> FortranViewNode_subpage_map_projection_polar_south ("polar_south");
static SimpleObjectMaker<Proj4PolarSouthWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_polar_south_wrapper ("polar_south");


#include "Proj4GeosWrapper.h"
static SimpleObjectMaker<Proj4Geos, Transformation> FortranViewNode_subpage_map_projection_geos ("geos");
static SimpleObjectMaker<Proj4GeosWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_geos_wrapper ("geos");


#include "Proj4Meteosat0Wrapper.h"
static SimpleObjectMaker<Proj4Meteosat0, Transformation> FortranViewNode_subpage_map_projection_meteosat ("meteosat");
static SimpleObjectMaker<Proj4Meteosat0Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_meteosat_wrapper ("meteosat");


#include "Proj4Meteosat57Wrapper.h"
static SimpleObjectMaker<Proj4Meteosat57, Transformation> FortranViewNode_subpage_map_projection_meteosat_57E ("meteosat_57E");
static SimpleObjectMaker<Proj4Meteosat57Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_meteosat_57E_wrapper ("meteosat_57E");


#include "Proj4GeoseWrapper.h"
static SimpleObjectMaker<Proj4Geose, Transformation> FortranViewNode_subpage_map_projection_goes_east ("goes_east");
static SimpleObjectMaker<Proj4GeoseWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_goes_east_wrapper ("goes_east");


#include "Proj4LambertWrapper.h"
static SimpleObjectMaker<Proj4Lambert, Transformation> FortranViewNode_subpage_map_projection_lambert ("lambert");
static SimpleObjectMaker<Proj4LambertWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_lambert_wrapper ("lambert");


#include "Proj4EPSG3857Wrapper.h"
static SimpleObjectMaker<Proj4EPSG3857, Transformation> FortranViewNode_subpage_map_projection_EPSG3857 ("EPSG:3857");
static SimpleObjectMaker<Proj4EPSG3857Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_EPSG3857_wrapper ("EPSG:3857");


#include "Proj4EPSG900913Wrapper.h"
static SimpleObjectMaker<Proj4EPSG900913, Transformation> FortranViewNode_subpage_map_projection_EPSG900913 ("EPSG:900913");
static SimpleObjectMaker<Proj4EPSG900913Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_EPSG900913_wrapper ("EPSG:900913");


#include "Proj4EPSG32661Wrapper.h"
static SimpleObjectMaker<Proj4EPSG32661, Transformation> FortranViewNode_subpage_map_projection_EPSG32661 ("EPSG:32661");
static SimpleObjectMaker<Proj4EPSG32661Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_EPSG32661_wrapper ("EPSG:32661");


#include "Proj4EPSG32761Wrapper.h"
static SimpleObjectMaker<Proj4EPSG32761, Transformation> FortranViewNode_subpage_map_projection_EPSG32761 ("EPSG:32761");
static SimpleObjectMaker<Proj4EPSG32761Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_EPSG32761_wrapper ("EPSG:32761");


#include "Proj4EPSG4326Wrapper.h"
static SimpleObjectMaker<Proj4EPSG4326, Transformation> FortranViewNode_subpage_map_projection_EPSG4326 ("EPSG:4326");
static SimpleObjectMaker<Proj4EPSG4326Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_EPSG4326_wrapper ("EPSG:4326");


#include "Proj4EPSG3035Wrapper.h"
static SimpleObjectMaker<Proj4EPSG3035, Transformation> FortranViewNode_subpage_map_projection_EPSG3035 ("EPSG:3035");
static SimpleObjectMaker<Proj4EPSG3035Wrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_EPSG3035_wrapper ("EPSG:3035");


#include "Proj4GoodeWrapper.h"
static SimpleObjectMaker<Proj4Goode, Transformation> FortranViewNode_subpage_map_projection_goode ("goode");
static SimpleObjectMaker<Proj4GoodeWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_goode_wrapper ("goode");


#include "Proj4CollignonWrapper.h"
static SimpleObjectMaker<Proj4Collignon, Transformation> FortranViewNode_subpage_map_projection_collignon ("collignon");
static SimpleObjectMaker<Proj4CollignonWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_collignon_wrapper ("collignon");


#include "Proj4MollweideWrapper.h"
static SimpleObjectMaker<Proj4Mollweide, Transformation> FortranViewNode_subpage_map_projection_mollweide ("mollweide");
static SimpleObjectMaker<Proj4MollweideWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_mollweide_wrapper ("mollweide");


#include "Proj4RobinsonWrapper.h"
static SimpleObjectMaker<Proj4Robinson, Transformation> FortranViewNode_subpage_map_projection_robinson ("robinson");
static SimpleObjectMaker<Proj4RobinsonWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_robinson_wrapper ("robinson");


#include "Proj4BonneWrapper.h"
static SimpleObjectMaker<Proj4Bonne, Transformation> FortranViewNode_subpage_map_projection_bonne ("bonne");
static SimpleObjectMaker<Proj4BonneWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_bonne_wrapper ("bonne");


#include "Proj4GoogleWrapper.h"
static SimpleObjectMaker<Proj4Google, Transformation> FortranViewNode_subpage_map_projection_google ("google");
static SimpleObjectMaker<Proj4GoogleWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_google_wrapper ("google");


#include "Proj4EfasWrapper.h"
static SimpleObjectMaker<Proj4Efas, Transformation> FortranViewNode_subpage_map_projection_efas ("efas");
static SimpleObjectMaker<Proj4EfasWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_efas_wrapper ("efas");


#include "Proj4TPersWrapper.h"
static SimpleObjectMaker<Proj4TPers, Transformation> FortranViewNode_subpage_map_projection_tpers ("tpers");
static SimpleObjectMaker<Proj4TPersWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_tpers_wrapper ("tpers");


#include "Proj4AutomaticWrapper.h"
static SimpleObjectMaker<Proj4Automatic, Transformation> FortranViewNode_subpage_map_projection_automatic ("automatic");
static SimpleObjectMaker<Proj4AutomaticWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_automatic_wrapper ("automatic");


#include "Proj4MercatorWrapper.h"
static SimpleObjectMaker<Proj4Mercator, Transformation> FortranViewNode_subpage_map_projection_mercator ("mercator");
static SimpleObjectMaker<Proj4MercatorWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_mercator_wrapper ("mercator");


#include "CartesianTransformationWrapper.h"
static SimpleObjectMaker<CartesianTransformation, Transformation> FortranViewNode_subpage_map_projection_cartesian ("cartesian");
static SimpleObjectMaker<CartesianTransformationWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_cartesian_wrapper ("cartesian");


#include "TaylorProjectionWrapper.h"
static SimpleObjectMaker<TaylorProjection, Transformation> FortranViewNode_subpage_map_projection_taylor ("taylor");
static SimpleObjectMaker<TaylorProjectionWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_taylor_wrapper ("taylor");


#include "TephigramWrapper.h"
static SimpleObjectMaker<Tephigram, Transformation> FortranViewNode_subpage_map_projection_tephigram ("tephigram");
static SimpleObjectMaker<TephigramWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_tephigram_wrapper ("tephigram");


#include "SkewtWrapper.h"
static SimpleObjectMaker<Skewt, Transformation> FortranViewNode_subpage_map_projection_skewt ("skewt");
static SimpleObjectMaker<SkewtWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_skewt_wrapper ("skewt");


#include "EmagramWrapper.h"
static SimpleObjectMaker<Emagram, Transformation> FortranViewNode_subpage_map_projection_emagram ("emagram");
static SimpleObjectMaker<EmagramWrapper, TransformationWrapper> FortranViewNode_subpage_map_projection_emagram_wrapper ("emagram");






#include "NoPreviewVisitorWrapper.h"

static SimpleObjectMaker<NoPreviewVisitorWrapper> FortranViewNode_subpage_map_preview_off_Wrapper("off");


#include "PreviewVisitorWrapper.h"
static SimpleObjectMaker<PreviewVisitor, NoPreviewVisitor> FortranViewNode_subpage_map_preview_on ("on");
static SimpleObjectMaker<PreviewVisitorWrapper, NoPreviewVisitorWrapper> FortranViewNode_subpage_map_preview_on_wrapper ("on");



#include "NoMagnifierVisitorWrapper.h"

static SimpleObjectMaker<NoMagnifierVisitorWrapper> FortranViewNode_subpage_map_magnifier_off_Wrapper("off");


#include "MagnifierVisitorWrapper.h"
static SimpleObjectMaker<MagnifierVisitor, NoMagnifierVisitor> FortranViewNode_subpage_map_magnifier_on ("on");
static SimpleObjectMaker<MagnifierVisitorWrapper, NoMagnifierVisitorWrapper> FortranViewNode_subpage_map_magnifier_on_wrapper ("on");



