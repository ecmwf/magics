/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImportObjectHandler.cc
    \brief Implementation of the Template class ImportObjectHandler.

    Magics Team - ECMWF 2010

    Started: Wed 10-Feb-2010

    Changes:

*/

#include "ImportObjectHandler.h"
#include "Dimension.h"
#include "ImportObject.h"
#include "Layer.h"

using namespace magics;

ImportObjectHandler::ImportObjectHandler() {
    BasicSceneObject::name_ = "import";
}

ImportObjectHandler::~ImportObjectHandler() {}

/*!
 Class information are given to the output-stream.
*/
void ImportObjectHandler::print(ostream& out) const {
    out << "ImportObjectHandler[";
    ImportObjectHandlerAttributes::print(out);
    out << "]";
}

void ImportObjectHandler::getReady() {
    layout_ = new Layout();
}

void ImportObjectHandler::visit(SceneLayer& tree, vector<LayoutVisitor*>&) {
    StaticLayer* import = new StaticLayer(this);
    import->name(path_);
    tree.add(import);

    layout_ = new Layout();
    import->add(layout_);

    layout_->x(adjustDimension(import_x_, 0., BasicPositionalObject::absoluteWidth()));
    layout_->y(adjustDimension(import_y_, 0., BasicPositionalObject::absoluteHeight()));
    layout_->width(adjustDimension(import_width_, 100., BasicPositionalObject::absoluteWidth()));
    layout_->height(adjustDimension(import_height_, 100., BasicPositionalObject::absoluteHeight()));

    // layout_->frame(true, true, Colour("blue"), M_DOT, 3);
    // layout_->frameIt();

    ImportObject* object = new ImportObject();
    object->setPath(path_);
    object->setOrigin(PaperPoint(0, 0));
    object->setWidth(import_width_ == -1 ? import_width_ : 100);
    object->setHeight(import_height_ == -1 ? import_height_ : 100);
    object->setFormat(format_);
    object->setOriginReference(ImportObject::bottom_left);
    layout_->push_back(object);
}

void ImportObjectHandler::visit(MetaDataCollector&) {
    /*// jsut look for MV_Format
    MetaDataCollector::iterator format = collector.find("MV_Format");
    if ( format != collector.end() ) {
        format->second = service_;
    }

    (*interpretor_).visit(collector);
    */
}
