/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file QtDriver.cc
    \brief Implementation of QtDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Jan  4 20:28:15 2010

*/

#include <AnimationRules.h>
#include <Flag.h>
#include <Image.h>
#include <ImportObject.h>
#include <Layer.h>
#include <Polyline.h>
#include <QtDriver.h>
#include <Symbol.h>
#include <Text.h>
#include "HistoVisitor.h"

#include <QApplication>
#include <QDebug>
#include <QDesktopWidget>
#include <QGraphicsItem>
#include <QPainter>

#ifdef MAGICS_QT5
#include <QGuiApplication>
#include <QScreen>
#elif defined(Q_WS_X11)
#include <QX11Info>
#endif

#include "MgQPlotScene.h"

#include "MgQDriverObject.h"
#include "MgQHistoItem.h"
#include "MgQLayerItem.h"
#include "MgQLayoutItem.h"
#include "MgQPathItem.h"
#include "MgQPattern.h"
#include "MgQPixmapItem.h"
#include "MgQPolylineSetItem.h"
#include "MgQRootItem.h"
#include "MgQSceneItem.h"
#include "MgQStepItem.h"
#include "MgQSymbol.h"
#include "MgQTextItem.h"

using namespace magics;

/*!
  \brief Constructor
*/
QtDriver::QtDriver() {
    scene_                       = 0;
    initialized_                 = false;
    symbolManager_               = 0;
    patternManager_              = 0;
    magnifierIsBeingRedisplayed_ = false;
    magnifierZoomFactor_         = 1.;

    currentPolylineSetItem_ = 0;

    penStyle_[M_DASH]       = Qt::DashLine;
    penStyle_[M_DOT]        = Qt::DotLine;
    penStyle_[M_CHAIN_DASH] = Qt::DashDotLine;
    penStyle_[M_CHAIN_DOT]  = Qt::DashDotDotLine;
    penStyle_[M_SOLID]      = Qt::SolidLine;

    // lineWidthFactor_=0.67;
    // lineWidthFactor_=0.6;
    lineWidthFactor_ = 0.55;
    fontSizeFactor_  = 1.1;
}

/*!
  \brief Destructor
*/
QtDriver::~QtDriver() {}

/*!
  \brief Opening the driver
*/
void QtDriver::open() {
    MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);

    scene->clearBeforeNewRequest();

    currentItem_ = scene->plotRootItem();

    // Delete layout item stack
    while (!layoutItemStack_.empty()) {
        layoutItemStack_.pop();
    }

    // Initialise layout item stack
    layoutItemStack_.push(0);

    // Delete layer item stack
    while (!layerItemStack_.empty()) {
        layerItemStack_.pop();
    }

    // Initialise layer item stack
    layerItemStack_.push(0);

    // Clear the display
    // glClear(GL_COLOR_BUFFER_BIT);

    currentPolylineSetItem_ = 0;

    // We find out the screen dpy from Qt. It can be different to the value
    // returned by scene->dpiResolution(), which is defined externally by
    // a reliable command (e.g. by 'xdpyinfo') and avialiable through
    // the env var METVIEW_SCREEN_RESOLUTION on the metview side.

    // So we need to compute their ratio to correctly set font size for rendering!
#ifdef MAGICS_QT5
    QList<QScreen*> scList    = QGuiApplication::screens();
    const int qtDpiResolution = (!scList.isEmpty()) ? scList.at(0)->logicalDotsPerInchY() : 72;
#elif defined(Q_WS_X11)  // Do we work with a X11 display?
    const int qtDpiResolution = QX11Info::appDpiY(0);
#else                    // for MacOS X with Qt4
    const int qtDpiResolution = 95;
#endif

    // By default the ratio between the physical pixel size according to the external definition and Qt is 1
    dpiResolutionRatio_ = 1.;

    // If we have the physical pixel size from an external definition
    if (scene->dpiResolution() != -1) {
        // We compute the correct ratio
        dpiResolutionRatio_ = static_cast<float>(qtDpiResolution) / static_cast<float>(scene->dpiResolution());
    }
    // Otherwise we suppose it is the same as given by Qt (thus their ratio will be 1)
    else {
        scene->setDpiResolution(qtDpiResolution);
    }


    setCMscale(static_cast<float>(scene->dpiResolution()) / 2.54);  // cm -> pixel

    dimensionX_ = floorf(convertCM(getXDeviceLength()));
    dimensionY_ = floorf(convertCM(getYDeviceLength()));

    scene->setSceneRect(0, 0, dimensionX_, dimensionY_);
    scene->setOriSceneRect(scene->sceneRect());
    scene->plotRootItem()->setScale(1);

    // currentItem_=0;
    // currentPolylineSetItem_=0;

    // currentItem_=scene->plotRootItem();

    // Initialise layout item stack
    // layoutItemStack_.push(0);

    // Scaling factors have to be stored for
    // symbol plotting and for rotations in an unscaled co-ordinate system
    coordRatioX_ = 1.;
    coordRatioY_ = 1;

    // The driver will regard pngs as transparent images
    alphaEnabled_ = true;

    // Set initialization flag
    // initialized_=true;
}

/*!
  \brief Closing the driver
*/
void QtDriver::close() {
    MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);

    // scene->drawIt();

    // endPage();
    currentPage_ = 0;

    MagLog::debug() << "QtDriver::close -->item num:" << scene->items().count() << endl;
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.
*/
MAGICS_NO_EXPORT void QtDriver::startPage() const {
    if (currentPage_ > 0)
        endPage();

    MagLog::debug() << "QtDriver::startPage needs implementing." << endl;

    currentPage_++;
    newPage_ = true;
}

/*!
  \brief ending a page

  This method has to take care that for formats with multiple output
  files are closed.
*/
MAGICS_NO_EXPORT void QtDriver::endPage() const {
    MagLog::debug() << "QtDriver::endPage needs implementing." << endl;
}
/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/

MAGICS_NO_EXPORT void QtDriver::project(const magics::Layout& layout) const {
    MagLog::progress("project");

    // Create a new Layout Item
    string name         = layout.name();
    MgQLayoutItem* item = new MgQLayoutItem(layout);

    QGraphicsItem* parentItem = currentItem_;
    item->setParentItem(parentItem);
    item->setParentItemInMainScene(parentItem);

    project(item);
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new PreviewLayout given.

  \sa Layout
*/

MAGICS_NO_EXPORT void QtDriver::project(const magics::PreviewLayout& layout) const {
    MagLog::progress("project");

    // Create a new PreviewLayoutItem
    string name                = layout.name();
    MgQPreviewLayoutItem* item = new MgQPreviewLayoutItem(layout);
    item->hide();

    MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);
    if (scene->currentSceneItem()) {
        scene->currentSceneItem()->addPreviewLayoutItem(item);
    }

    QGraphicsItem* parentItem = currentItem_;
    item->setParentItem(parentItem);
    item->setParentItemInMainScene(parentItem);

    project(item);
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new MagnifierLayout given.

  \sa Layout
*/

MAGICS_NO_EXPORT void QtDriver::project(const magics::MagnifierLayout& layout) const {
    // Create a new MagnifierLayoutItem
    string name                  = layout.name();
    MgQMagnifierLayoutItem* item = new MgQMagnifierLayoutItem(layout);
    // item->hide();

    QGraphicsItem* parentItem = currentItem_;
    item->setParentItem(parentItem);
    item->setParentItemInMainScene(parentItem);

    project(item);
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new MagnifierLayout given.

  \sa Layout
*/

MAGICS_NO_EXPORT void QtDriver::project(const magics::HistoLayout& layout) const {
    // Create a new HistrogramItem
    string name         = layout.name();
    MgQLayoutItem* item = new MgQLayoutItem(layout);
    // item->hide();

    QGraphicsItem* parentItem = currentItem_;
    item->setParentItem(parentItem);
    item->setParentItemInMainScene(parentItem);

    project(item);
}


MAGICS_NO_EXPORT void QtDriver::project(const magics::SceneLayout& layout) const {
    // QCoreApplication::processEvents();

    string name = layout.name();
    MgQLayoutItem* item;

    // QString::fromStdString(name).contains("page", Qt::CaseInsensitive))

    if (currentItem_->scene() == scene_ && QString::fromStdString(name).contains("page", Qt::CaseInsensitive)) {
        MgQSceneItem* sceneItem = new MgQSceneItem(layout);

        MgQDriverObject* drv = new MgQDriverObject(*this);
        sceneItem->setDriverObject(drv);

        MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);
        scene->addSceneItem(sceneItem);
        scene->setCurrentSceneItem(sceneItem);

        item = sceneItem;
    }
    else {
        item = new MgQLayoutItem(layout);
    }

    QGraphicsItem* parentItem = currentItem_;
    item->setParentItem(parentItem);
    item->setParentItemInMainScene(parentItem);
    project(item);
}


/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void QtDriver::project(MgQLayoutItem* item) const {
    const Layout& layout = item->layout();

    if (layout.isZoomable() && magnifierIsBeingRedisplayed_ == false) {
        item->setData(MgQ::ItemIsZoomableKey, true);

        MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);
        scene->currentSceneItem()->addProjectorItem(item);
    }


    MagLog::debug() << "---> (Qt) PROJECT " << layout.name() << endl;

    // Get layout geometry
    const MFloat width  = layout.width() * 0.01 * dimensionX_;
    const MFloat height = layout.height() * 0.01 * dimensionY_;
    const MFloat x      = layout.x() * 0.01 * dimensionX_;
    const MFloat y      = layout.y() * 0.01 * dimensionY_;

    // Get layout coordinate range
    const MFloat minX   = layout.minX();
    const MFloat maxX   = layout.maxX();
    const MFloat minY   = layout.minY();
    const MFloat maxY   = layout.maxY();
    const MFloat rangeX = maxX - minX;
    const MFloat rangeY = maxY - minY;

    // Set dimensions
    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    dimensionX_ = width;
    dimensionY_ = height;
    item->dimensionX(dimensionX_);
    item->dimensionY(dimensionY_);

    //--------------------------------------------
    // Define the coordinate system translation
    // (in parent coordinates!)
    //--------------------------------------------

    QTransform tr;

    // Here we have to compensate the translation by project(-XMin) etc. in the
    // parent layout item!
    MgQLayoutItem* parentLayoutItem = layoutItemStack_.top();
    if (parentLayoutItem && parentLayoutItem->scene() == scene_) {
        tr.translate(parentLayoutItem->projectedMinX(), parentLayoutItem->projectedMinY());

        MagLog::dev() << "Translate redo: " << parentLayoutItem->projectedMinX() << " "
                      << parentLayoutItem->projectedMinY() << endl;
    }


    // Translate the origo
    tr.translate(x, y);
    MagLog::dev() << "Translate: " << x << " " << y << endl;

    // Set the scaling factor
    // projectX and, projectY depend on coordRatio[X,Y]_ !!!
    const MFloat xx = width / rangeX;
    const MFloat yy = height / rangeY;

    scalesX_.push(coordRatioX_);
    scalesY_.push(coordRatioY_);
    coordRatioX_ = xx;
    coordRatioY_ = yy;

    item->coordRatioX(coordRatioX_);
    item->coordRatioY(coordRatioY_);

    // Translate the origo
    tr.translate(projectX(-minX), projectY(-minY));

    // Save the projected minX, minY
    item->projectedMinX(projectX(minX));
    item->projectedMinY(projectY(minY));
    item->projectedMaxX(projectX(maxX));
    item->projectedMaxY(projectY(maxY));

    MagLog::dev() << "Translate: " << projectX(-minX) << " " << projectY(-minY) << endl;

    // Set thr transformation for the layout item
    item->setTransform(tr);

#if 0
	if(layout.name() == "drawing" || layout.name() == "left" || layout.name() == "top")
	{
		QGraphicsRectItem *r=new QGraphicsRectItem(projectX(minX),projectY(minY),projectX(rangeX),projectY(rangeY));
		MagLog::debug() << projectX(minX) << "  " << projectY(minY) << " " << projectX(rangeX) << " " << projectY(rangeY) << endl;

		if(layout.name() != "drawing")
			r->setBrush(Qt::cyan);
		else
			r->setBrush(Qt::red);	
		//r->setTransform(tr);
		//scene_->addItem(r);
		//group->addToGroup(r);
		r->setParentItem(layoutItem);
	}
#endif

    // Update item history
    currentItem_ = item;
    layoutItemStack_.push(item);
}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

*/
MAGICS_NO_EXPORT void QtDriver::unproject() const {
    MgQLayoutItem* qln = layoutItemStack_.top();

    if (qln)
        MagLog::debug() << "---> (Qt) UNPROJECT " << qln->layout().name() << endl;
    else
        MagLog::debug() << "---> (Qt) UNPROJECT 0x000000" << endl;


    // If layout item is zero returns
    if (!qln)
        return;

    // Set current item to the layouts parent
    if (qln->parentItem() != 0)
        currentItem_ = static_cast<QGraphicsItem*>(qln->parentItem());
    else
        currentItem_ = 0;

    // Pop layout from the stack
    layoutItemStack_.pop();

    // Verify dimension stack
    if (dimensionStack_.empty()) {
        MagLog::error() << "--->UNPROJECT ("
                        << ") Dimension stack error!" << endl;
        assert(dimensionStack_.empty() == false);
    }

    coordRatioX_ = scalesX_.top();
    coordRatioY_ = scalesY_.top();
    scalesX_.pop();
    scalesY_.pop();

    // Verify dimension stack
    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();
}

/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa PhysicalLayer
*/
MAGICS_NO_EXPORT void QtDriver::newLayer(Layer& layer) const {
    MagLog::debug() << "(Qt) NEW Layer ---> " << layer.id() << endl;
}


/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa PhysicalLayer
*/
MAGICS_NO_EXPORT void QtDriver::newLayer(StaticLayer& layer) const {
    MagLog::debug() << "QtDriver::newLayer (StaticLayer) ---> " << layer.id() << endl;

    QString name(layer.name().c_str());
    MgQLayoutItem* parentLayout = layoutItemStack_.top();
    MgQLayerItem* qln           = new MgQLayerItem(layer, parentLayout, 0);

    newLayer(qln);
}

/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa PhysicalLayer
*/
MAGICS_NO_EXPORT void QtDriver::newLayer(StepLayer& layer) const {
    MagLog::debug() << "(Qt) NEW StepLayer ---> " << layer.id() << endl;

    QString name(layer.name().c_str());
    MgQLayoutItem* parentLayout = layoutItemStack_.top();
    MgQLayerItem* qln           = new MgQLayerItem(layer, parentLayout, layer.parent()->numberOfSteps());

    newLayer(qln);
}

MAGICS_NO_EXPORT void QtDriver::newLayer(MgQLayerItem* qln) const {
    MagLog::debug() << "QtDriver::newLayer (MgQLayerItem) ---> " << qln->layer().id() << endl;

    MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);

    qln->setParentItem(currentItem_);

    scene->currentSceneItem()->addLayerItem(qln);

    layerItemStack_.push(qln);
    currentItem_ = qln->rootItem();
}


/*!
  \brief close the current layer

  This method will close an existing layer. This includes resets of existing boxes.

  \sa UnPhysicalLayer PhysicalLayer
*/
MAGICS_NO_EXPORT void QtDriver::closeLayer(Layer& layer) const {
    MagLog::debug() << "---> (Qt) CLOSE LAYER" << layer.name() << endl;
}


MAGICS_NO_EXPORT void QtDriver::closeLayer(StaticLayer& layer) const {
    MagLog::debug() << "---> (Qt) CLOSE LAYER" << layer.name() << endl;
    MgQLayerItem* qln = layerItemStack_.top();
    closeLayer(qln);
}

MAGICS_NO_EXPORT void QtDriver::closeLayer(StepLayer& layer) const {
    MagLog::debug() << "---> (Qt) CLOSE LAYER" << layer.name() << endl;
    MgQLayerItem* qln = layerItemStack_.top();
    closeLayer(qln);
}


/*!
  \brief close the current layer

  This method will close an existing layer. This includes resets of existing boxes.

  \sa UnPhysicalLayer PhysicalLayer
*/
MAGICS_NO_EXPORT void QtDriver::closeLayer(MgQLayerItem* qln) const {
    // Get current layer item
    // MgQLayerItem *qln=layerItemStack_.top();

    if (qln)
        MagLog::debug() << "---> (Qt) CLOSE LAYER" << qln->layer().name() << endl;
    else
        MagLog::debug() << "---> (Qt) CLOSE LAYER 0x000000" << endl;

    // If layer item is zero returns
    if (!qln)
        return;

    // Get current layer item
    // assert(qln->layer().id() == layer.id());

    // Pop layer item from the stack
    layerItemStack_.pop();

    // Set current item to the layer parent
    if (qln->parentItem() != 0)
        currentItem_ = static_cast<QGraphicsItem*>(qln->parentItem());
    else
        currentItem_ = 0;
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void QtDriver::setNewColour(const Colour& colour) const {
    if (currentColour_ == colour)
        return;
    currentColour_ = colour;
    // MagLog::debug() << "QtDriver::setNewColour needs checking." <<endl;
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void QtDriver::setNewLineWidth(const MFloat width) const {
    currentLineWidth_ = width * lineWidthFactor_;
    // MagLog::debug() << "QtDriver::setNewColour needs checking." <<endl;
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT int QtDriver::setLineParameters(const LineStyle linestyle, const MFloat w) const {
    setNewLineWidth(w);

    currentLineStyle_ = linestyle;

    if (penStyle_.contains(linestyle)) {
        currentPenStyle_ = penStyle_[linestyle];
    }
    else {
        currentPenStyle_ = Qt::SolidLine;
    }

    return 0;
}

/*!
  \brief renders polylines

  This method renders a polyline given as two MFloat arrays. The two
  arrays given as X and Y values have to be at least the length of
  <i>n</i>. All values beyond <i>n</i> will be ignored. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void QtDriver::renderPolyline(const int n, MFloat* x, MFloat* y) const {
    if (currentColour_ == Colour("none"))
        return;

    QVector<QPointF> pp;
    for (int i = 0; i < n; i++) {
        pp << QPointF(projectX(x[i]), projectY(y[i]));
    }


    if (currentPolylineSetItem_ != 0) {
        QBrush brush(Qt::NoBrush);
        QPen pen(QPen(getQtColour(currentColour_)));

        pen.setStyle(currentPenStyle_);
        pen.setWidthF(currentLineWidth_);
        pen.setCosmetic(true);

        currentPolylineSetItem_->addPolyline(pp, brush, pen, false);
    }
    else {
        QPainterPath path;
        path.addPolygon(pp);
        QGraphicsPathItem* item = new QGraphicsPathItem(path);
        item->setParentItem(currentItem_);
        QPen pen(QPen(getQtColour(currentColour_)));
        pen.setStyle(currentPenStyle_);
        pen.setWidthF(currentLineWidth_);
        pen.setCosmetic(true);

        item->setPen(pen);
    }
}

/*!
  \brief renders a single line

  This method renders a polyline with two points.The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void QtDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const {
    if (currentColour_ == Colour("none"))
        return;

    if (n != 2)
        return;

    if (currentPolylineSetItem_ != 0) {
        QVector<QPointF> pp;
        for (int i = 0; i < n; i++) {
            pp << QPointF(x[i], y[i]);
        }

        QBrush brush(Qt::NoBrush);
        QPen pen(QPen(getQtColour(currentColour_)));
        pen.setWidthF(currentLineWidth_);
        pen.setStyle(currentPenStyle_);
        pen.setCosmetic(true);

        currentPolylineSetItem_->addPolyline(pp, brush, pen, false);
    }
    else {
        QLineF pp(x[0], y[0], x[1], y[1]);

        QGraphicsLineItem* item = new QGraphicsLineItem(pp);
        item->setParentItem(currentItem_);
        QPen pen(QPen(getQtColour(currentColour_)));

        pen.setWidthF(currentLineWidth_);
        pen.setStyle(currentPenStyle_);
        pen.setCosmetic(true);
        item->setPen(pen);
    }
}

/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void QtDriver::renderSimplePolygon(const int n, MFloat* x, MFloat* y) const {
    if (currentColour_ == Colour("none"))
        return;

    QBrush brush(Qt::SolidPattern);
    QPen pen;

    if (currentShading_ == M_SH_DOT) {
        const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
        const int density               = (int)sqrt(pro->density_);
        if (density <= 0)
            return;
        const MFloat square_size = convertCM(1.) / density;
        int dotSize              = static_cast<int>(pro->size_ * convertCM(1.));
        if (dotSize < 2)
            dotSize = 1;

        int s = static_cast<int>(ceil(square_size));

        bool doSolidShading = false;
        if (s < 2) {
            doSolidShading = true;
        }
        else if (s - 2 < dotSize) {
            dotSize = s - 2;
            if (dotSize < 1) {
                doSolidShading = true;
            }
        }

        if (doSolidShading) {
            brush.setColor(getQtColour(currentColour_));

            pen.setColor(getQtColour(currentColour_));
        }
        else {
            if (!patternManager_) {
                patternManager_ = new MgQPatternManager;
            }

            MgQPatternProperties dot(MgQPatternProperties::DotShading);
            dot.size_     = QSize(s, s);
            dot.itemSize_ = QSize(dotSize, dotSize);
            dot.colour_   = getQtColour(currentColour_);

            MgQPattern* pix = patternManager_->addPattern(dot);

            brush.setTexture(pix->pixmap());
            pen = QPen(Qt::NoPen);
            // pen.setColor(QColor(255,255,255,0));
        }
    }
    else if (currentShading_ == M_SH_HATCH) {
        const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
        indexHatch_                       = pro->index_;
        if (indexHatch_ < 1 || indexHatch_ > 6) {
            MagLog::warning() << "QtDriver::renderSimplePolygon > Hatch index " << indexHatch_
                              << " is wrong. No hatch sahding possible!" << endl;
            return;
        }
        float square_size = convertCM(1.) / pro->density_;
        int s;

        // If the square is too small we go for solid shading
        if (square_size < 1.5) {
            brush.setColor(getQtColour(currentColour_));

            pen.setColor(getQtColour(currentColour_));
        }
        else {
            s = static_cast<int>(ceil(square_size));

            if (!patternManager_) {
                patternManager_ = new MgQPatternManager;
            }

            MgQPatternProperties hatch(MgQPatternProperties::HatchShading);
            hatch.id_        = QString::number(indexHatch_);
            hatch.size_      = QSize(s, s);
            hatch.lineWidth_ = pro->thickness_;
            hatch.colour_    = getQtColour(currentColour_);

            MgQPattern* pix = patternManager_->addPattern(hatch);

            brush.setTexture(pix->pixmap());
            pen.setColor(QColor(255, 255, 255, 0));
        }
    }
    else {
        brush.setColor(getQtColour(currentColour_));

        pen.setColor(getQtColour(currentColour_));
    }

    if (currentPolylineSetItem_ != 0) {
        QVector<QPointF> pp;
        for (int i = 0; i < n; i++) {
            pp << QPointF(projectX(x[i]), projectY(y[i]));
        }
        currentPolylineSetItem_->addPolyline(pp, brush, pen, true);
    }
    else {
        QVector<QPointF> pp;
        for (int i = 0; i < n; i++) {
            pp << QPointF(projectX(x[i]), projectY(y[i]));
        }

        QGraphicsPolygonItem* item = new QGraphicsPolygonItem(pp);
        item->setParentItem(currentItem_);

        item->setPen(pen);
        item->setBrush(brush);
    }
}

void QtDriver::renderSimplePolygon(const Polyline& line) const {
    // MagLog::progress("poly");

    // closeGroup();
    // debugOutput("renderSimplePolygon");
    setNewColour(line.getFillColour());
    const unsigned int n = line.size();
    if (n < 3 || (currentColour_ == Colour("none")))
        return;

    line.getShading()->draw(*this);

    QBrush brush(Qt::SolidPattern);
    QPen pen;
    if (currentShading_ == M_SH_DOT) {
        const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
        const int density               = (int)sqrt(pro->density_);
        if (density <= 0)
            return;
        const MFloat square_size = convertCM(1.) / density;
        int dotSize              = static_cast<int>(pro->size_ * convertCM(1.));
        if (dotSize < 2)
            dotSize = 1;

        int s = static_cast<int>(ceil(square_size));

        bool doSolidShading = false;
        if (s < 2) {
            doSolidShading = true;
        }
        else if (s - 2 < dotSize) {
            dotSize = s - 2;
            if (dotSize < 1) {
                doSolidShading = true;
            }
        }

        if (doSolidShading) {
            brush.setColor(getQtColour(currentColour_));

            pen.setColor(getQtColour(currentColour_));
        }
        else {
            if (!patternManager_) {
                patternManager_ = new MgQPatternManager;
            }

            MgQPatternProperties dot(MgQPatternProperties::DotShading);
            dot.size_     = QSize(s, s);
            dot.itemSize_ = QSize(dotSize, dotSize);
            dot.colour_   = getQtColour(currentColour_);

            MgQPattern* pix = patternManager_->addPattern(dot);

            brush.setTexture(pix->pixmap());
            pen = QPen(Qt::NoPen);
            // pen.setColor(QColor(255,255,255,0));
        }
    }
    else if (currentShading_ == M_SH_HATCH) {
        const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
        indexHatch_                       = pro->index_;
        if (indexHatch_ < 1 || indexHatch_ > 6) {
            MagLog::warning() << "QtDriver::renderSimplePolygon > Hatch index " << indexHatch_
                              << " is wrong. No hatch sahding possible!" << endl;
            return;
        }
        float square_size = convertCM(1.) / pro->density_;
        int s;

        // If the square is too small we go for solid shading
        if (square_size < 1.5) {
            brush.setColor(getQtColour(currentColour_));

            pen.setColor(getQtColour(currentColour_));
        }
        else {
            s = static_cast<int>(ceil(square_size));

            if (!patternManager_) {
                patternManager_ = new MgQPatternManager;
            }

            MgQPatternProperties hatch(MgQPatternProperties::HatchShading);
            hatch.id_        = QString::number(indexHatch_);
            hatch.size_      = QSize(s, s);
            hatch.lineWidth_ = pro->thickness_;
            hatch.colour_    = getQtColour(currentColour_);

            MgQPattern* pix = patternManager_->addPattern(hatch);

            brush.setTexture(pix->pixmap());
            pen = QPen(Qt::NoPen);
            // pen.setColor(QColor(255,255,255,0));
        }
    }
    else {
        brush.setColor(getQtColour(currentColour_));

        if (line.isStroked()) {
            pen.setColor(getQtColour(currentColour_));
        }
        else {
            pen = QPen(Qt::NoPen);
        }
    }

    QPainterPath path;
    QVector<QPointF> pp;
    for (unsigned int i = 0; i < n; i++) {
        pp << QPointF(projectX(line.get(i).x()), projectY(line.get(i).y()));
    }


    if (line.beginHoles() != line.endHoles()) {
        path.addPolygon(pp);
        path.closeSubpath();

        for (Polyline::Holes::const_iterator h = line.beginHoles(); h != line.endHoles(); h++) {
            vector<double> x;
            vector<double> y;
            line.hole(h, x, y);

            QVector<QPointF> ppHole;
            for (unsigned int i = 0; i < x.size(); i++) {
                ppHole << QPointF(projectX(x[i]), projectY(y[i]));
            }

            path.addPolygon(ppHole);
            path.closeSubpath();
        }
    }


    if (currentPolylineSetItem_ != 0) {
        if (path.isEmpty())
            currentPolylineSetItem_->addPolyline(pp, brush, pen, true);
        else
            currentPolylineSetItem_->addPath(path, brush, pen);
    }
    else {
        if (path.isEmpty()) {
            QGraphicsPolygonItem* item = new QGraphicsPolygonItem(pp);
            item->setParentItem(currentItem_);
            item->setPen(pen);
            item->setBrush(brush);
        }
        else {
            QGraphicsPathItem* item = new QGraphicsPathItem(path);
            item->setParentItem(currentItem_);
            item->setPen(pen);
            item->setBrush(brush);
        }
    }
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \note As of version 2.0 there are two forms of describing text in Text.

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void QtDriver::renderText(const Text& text) const {
    if (text.empty())
        return;

    // Check if nicetext available
    const vector<NiceText>& niceT = text.getNiceText();
    if (niceT.empty())
        return;

    const enum Justification horizontal = text.getJustification();
    const enum VerticalAlign vertical   = text.getVerticalAlign();

    string textString;
    MFloat pheight;

    // Check if all the text items has the same font, size, colour  and style
    assert(text.textBegin() != text.textEnd());
    const MagFont& magfontFirst  = (text.textBegin())->font();
    bool sameFontForItems = true;
    for (vector<NiceText>::const_iterator niceText = text.textBegin(); niceText != text.textEnd(); niceText++) {
        const MagFont& magfont = niceText->font();
        if (magfont.size() != magfontFirst.size() || magfont.name() != magfontFirst.name() ||
            !(magfont.colour() == magfontFirst.colour()) || magfont.styles() != magfontFirst.styles()) {
            sameFontForItems = false;
            break;
        }
    }

    // Loop for all string CO-ORDINATES
    unsigned int noTexts = text.size();
    for (unsigned int nT = 0; nT < noTexts; nT++) {
        const MFloat x0 = projectX(text[nT].x());
        const MFloat y0 = projectY(text[nT].y());
        const MFloat an = text.getAngle() * 57.29577951;

        //----------------------------------------------
        // If all the text items have the same font ....
        //----------------------------------------------

        if (sameFontForItems) {
            const MagFont& magfont          = magfontFirst;
            const std::set<string>& styles = magfont.styles();

            pheight = 72. * magfont.size() / 2.54;  // height in points
            pheight /= dpiResolutionRatio_;
            pheight *= fontSizeFactor_;

            if (pheight < 1.0) 
                pheight = 1.0;
            else if (pheight > 200.0) {
                pheight = 200.0;
            }    
            
            QFont font(QString::fromStdString(magfont.name()), pheight);
            font.setPointSizeF(pheight);

            if (styles.find("underlined") != styles.end())
                font.setUnderline(true);
            if (styles.find("bold") != styles.end())
                font.setBold(true);
            if (styles.find("italic") != styles.end())
                font.setItalic(true);
            if (styles.find("bolditalic") != styles.end()) {
                font.setItalic(true);
                font.setBold(true);
            }

            QString allText;
            for (vector<NiceText>::const_iterator niceText = text.textBegin(); niceText != text.textEnd(); niceText++) {
                QString str;
                textToUnicode((*niceText).text(), str);
                allText += str;
            }

            QFontMetrics fm(font);
            int width  = fm.width(allText);
            int height = fm.height();

            MFloat x = 0;
            if (horizontal == MCENTRE)
                x = width * .5;
            else if (horizontal == MRIGHT)
                x = width;

            MFloat y = 0.;
            if (vertical == MBASE) {
                y = height;
            }
            else if (vertical == MTOP) {
                y = 0.;
            }
            else if (vertical == MHALF) {
                y = height * .5;
            }
            else if (vertical == MBOTTOM) {
                y = height;
            }

            // Create the item
            MgQTextItem* item = new MgQTextItem(allText);
            item->setParentItem(currentItem_);
            item->setFont(font);
            // item->setPen(QPen());
            item->setTextBlanking(text.getBlanking());
            item->setBrush(getQtColour(magfont.colour()));

            if (magnifierIsBeingRedisplayed_ == true) {
                item->setFlag(QGraphicsItem::ItemIgnoresTransformations);
            }

            // item->setPos(x0,y0);
            QTransform tr;
            tr.scale(1., -1.);
            tr.translate(-x, -y);
            item->setTransform(tr);

            if (an != 0 && an != 360) {
#ifdef MAGICS_QT5
                item->setTransform(QTransform::fromTranslate(x, y), true);
                item->setTransform(QTransform().rotate(an), true);
                item->setTransform(QTransform::fromTranslate(-x, -y), true);
#else
                item->translate(x, y);
                item->rotate(an);
                item->translate(-x, -y);
#endif
            }
            item->setPos(x0, y0);
        }

        //--------------------------------------
        // If the text item fonts differ ...
        //--------------------------------------

        else {
            // Find out text width
            int totalWidth = 0;
            for (vector<NiceText>::const_iterator niceText = text.textBegin(); niceText != text.textEnd(); niceText++) {
                const MagFont& magfont          = niceText->font();
                const std::set<string>& styles = magfont.styles();

                pheight = 72. * magfont.size() / 2.54;  // height in points
                pheight /= dpiResolutionRatio_;
                pheight *= fontSizeFactor_;

                if (pheight < 1.0) 
                    pheight = 1.0;
                else if (pheight > 200.0) {
                    pheight = 200.0;
                }    
                
                QFont font(QString::fromStdString(magfont.name()), pheight);
                font.setPointSizeF(pheight);
                if (styles.find("underlined") != styles.end())
                    font.setUnderline(true);
                if (styles.find("bold") != styles.end())
                    font.setBold(true);
                if (styles.find("italic") != styles.end())
                    font.setItalic(true);

                QString str;
                textToUnicode((*niceText).text(), str);

                QFontMetrics fm(font);
                totalWidth += fm.width(str);
            }

            // Find out text start position
            int xPos = x0;
            ;
            if (horizontal == MCENTRE)
                xPos -= totalWidth * .5;
            else if (horizontal == MRIGHT)
                xPos -= totalWidth;

            // Loop for the indidual text items
            for (vector<NiceText>::const_iterator niceText = text.textBegin(); niceText != text.textEnd(); niceText++) {
                const MagFont& magfont          = niceText->font();
                const std::set<string>& styles = magfont.styles();

                pheight = 72. * magfont.size() / 2.54;  // height in points
                pheight /= dpiResolutionRatio_;
                pheight *= fontSizeFactor_;

                QFont font(QString::fromStdString(magfont.name()), pheight);
                font.setPointSizeF(pheight);
                if (styles.find("underlined") != styles.end())
                    font.setUnderline(true);
                if (styles.find("bold") != styles.end())
                    font.setBold(true);
                if (styles.find("italic") != styles.end())
                    font.setItalic(true);

                QString str;
                textToUnicode((*niceText).text(), str);

                QFontMetrics fm(font);
                int width  = fm.width(str);
                int height = fm.height();

                MFloat y = 0.;
                if (vertical == MBASE) {
                    y = height;
                }
                else if (vertical == MTOP) {
                    y = 0.;
                }
                else if (vertical == MHALF) {
                    y = height * .5;
                }
                else if (vertical == MBOTTOM) {
                    y = height;
                }

                // Create the item
                MgQTextItem* item = new MgQTextItem(str);
                item->setParentItem(currentItem_);
                item->setFont(font);
                // item->setPen(QPen());
                item->setTextBlanking(text.getBlanking());
                item->setBrush(getQtColour(magfont.colour()));

                if (magnifierIsBeingRedisplayed_ == true) {
                    item->setFlag(QGraphicsItem::ItemIgnoresTransformations);
                }

                item->setPos(x0, y0);

                if (an != 0 && an != 360)
                    item->setRotation(an);

                QTransform tr;
                tr.scale(1., -1.);
                tr.translate(xPos - x0, -y);
                item->setTransform(tr);

                xPos += width;
            }
        }
    }
}

// \brief Convert std::string to unicode QString
void QtDriver::textToUnicode(const string& str, QString& ustr) const {
    ustr = QString::fromUtf8(str.c_str());

    // Replace HTML 4 entities &#...;  to unicode char
    QRegExp rx("&#(\\d+);");

    QStringList lstHtml;
    QList<QChar> lstUni;

    int pos = 0;
    while ((pos = rx.indexIn(ustr, pos)) != -1) {
        if (!rx.cap(1).isEmpty()) {
            lstHtml << rx.cap(0);
            lstUni << QChar(rx.cap(1).toInt());
        }
        pos += rx.matchedLength();
    }

    for (int i = 0; i < lstHtml.count(); i++) {
        ustr.replace(lstHtml[i], lstUni[i]);
    }

    // qDebug() << "textToUnicode" << str.c_str()  << ustr;

    return;
}

/*!
  \brief drawing a circle

  This method renders given text strings.

  The meaning of the last parameter <i>s</i> is as follows:
     - 0-8 determines how many quarters of the circle are filled. Starting from the top clock-wise.
     - 9 fills the whole circle but leaves a vertical bar empty in the middle of the circle.

  \param x X Position
  \param y Y Position
  \param r Radius of circle
  \param s Style which determines how the circle is shaded
*/

MAGICS_NO_EXPORT void QtDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const {
    int fill = s;

    MFloat cx = projectX(x);
    MFloat cy = projectY(y);

    // Fill wedges
    if (fill > 0 && fill < 8) {
        QPainterPath path;
        path.moveTo(QPointF(cx, cy));
        path.arcTo(cx - r, cy - r, 2. * r, 2. * r, 45. * (fill - 2.), -45. * fill);

        QBrush brush(getQtColour(currentColour_));
        QPen pen(Qt::NoPen);

        QGraphicsPathItem* item = new QGraphicsPathItem(path);

        item->setParentItem(currentItem_);
        item->setPen(pen);
        item->setBrush(brush);
    }

    // Full filled circle
    else if (fill == 8) {
        QPainterPath path;

        path.addEllipse(QPointF(cx, cy), r, r);

        QBrush brush(getQtColour(currentColour_));
        QPen pen(Qt::NoPen);

        QGraphicsPathItem* item = new QGraphicsPathItem(path);

        item->setParentItem(currentItem_);
        item->setPen(pen);
        item->setBrush(brush);
    }

    // Full filled circle whit a white vertical bar in the middle
    else if (fill == 9) {
        QPainterPath path;

        path.arcMoveTo(cx - r, cy - r, 2. * r, 2. * r, 110);
        path.arcTo(cx - r, cy - r, 2. * r, 2. * r, 110, 140);
        path.closeSubpath();

        path.arcMoveTo(cx - r, cy - r, 2. * r, 2. * r, 290);
        path.arcTo(cx - r, cy - r, 2. * r, 2. * r, 290, 140);
        path.closeSubpath();

        QBrush brush(getQtColour(currentColour_));
        QPen pen(Qt::NoPen);

        QGraphicsPathItem* item = new QGraphicsPathItem(path);

        item->setParentItem(currentItem_);
        item->setPen(pen);
        item->setBrush(brush);
    }

    // Outline
    if (fill < 8) {
        QPainterPath path;
        path.addEllipse(QPointF(cx, cy), r, r);

        QBrush brush(Qt::NoBrush);
        QPen pen(getQtColour(currentColour_));

        QGraphicsPathItem* item = new QGraphicsPathItem(path);

        item->setParentItem(currentItem_);
        item->setPen(pen);
        item->setBrush(brush);
    }
}

MAGICS_NO_EXPORT void QtDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s,
                                       MgQSymbolItem* qSym) const {
    int fill = s;
    // if(s==9) fill=8

    // Fill white
    /*if(fill != 8 && fill != 0)
    {
        MgQPainterPath whiteCircle(true,true);
        whiteCircle.addEllipse(QPointF(x,y),r,r);
        qSym->addPath(whiteCircle);
    }*/

    // Fill wedges
    if (fill > 0 && fill < 8) {
        MgQPainterPath path(true);
        path.moveTo(QPointF(x, y));
        path.arcTo(x - r, y - r, 2. * r, 2. * r, 45. * (fill - 2.), -45. * fill);
        qSym->addPath(path);
    }

    // Full filled circle
    else if (fill == 8) {
        MgQPainterPath path(true);
        path.addEllipse(QPointF(x, y), r, r);
        qSym->addPath(path);
    }

    // Full filled circle whit a white vertical bar in the middle
    else if (fill == 9) {
        MgQPainterPath path(true);
        // path.arcTo(x-r,y-r,2.*r,2.*r, 70, -140);
        path.arcMoveTo(x - r, y - r, 2. * r, 2. * r, 110);
        path.arcTo(x - r, y - r, 2. * r, 2. * r, 110, 140);
        path.closeSubpath();
        qSym->addPath(path);

        MgQPainterPath path2(true);
        // path2.arcTo(x-r,y-r,2.*r,2.*r, 250, 140);
        path2.arcMoveTo(x - r, y - r, 2. * r, 2. * r, 290);
        path2.arcTo(x - r, y - r, 2. * r, 2. * r, 290, 140);
        path2.closeSubpath();
        qSym->addPath(path2);
    }

    // Outline
    // Full filled circle
    if (fill == 8) {
        MgQPainterPath pathCircle(false);
        pathCircle.addEllipse(QPointF(x, y), r, r);
        pathCircle.setRenderOnlyForOutline(true);
        qSym->addPath(pathCircle);
    }
    else {
        MgQPainterPath pathCircle(false);
        pathCircle.addEllipse(QPointF(x, y), r, r);
        qSym->addPath(pathCircle);
    }
}


/*!
  \brief Image render method for ALL drivers.

  This method should be used by all Magics++ drivers to render image objects.
*/

MAGICS_NO_EXPORT void QtDriver::renderImage(const ImportObject& obj) const {
    std::string f         = obj.getFormat();
    GraphicsFormat format = PNG;
    if (magCompare(f, "ps"))
        return;  // format = PS;
    else if (magCompare(f, "eps"))
        return;  // format = EPS;
    else if (magCompare(f, "gif"))
        format = GIF;
    else if (magCompare(f, "jpeg") || magCompare(f, "jpg"))
        format = JPG;
    else if (magCompare(f, "png"))
        format = PNG;
    else if (magCompare(f, "svg"))
        return;  // format = SVG;
    else
        return;

    MFloat width  = obj.getWidth();   // cm
    MFloat height = obj.getHeight();  // cm

    if (width == 0. || height == 0.)
        return;

    QImage img(obj.getPath().c_str());
    if (img.isNull()) {
        return;
    }

    if (width < 0) {
        width = img.width();  // pixels
        // We need the width in cm
        MFloat cm0 = projectX(obj.getOrigin().x());
        MFloat cm1 = projectX(obj.getOrigin().x() + 1.);
        MFloat cm  = fabs(cm1 - cm0);
        if (cm > 0.)
            width /= cm;
        else
            return;
    }

    if (height < 0) {
        height = img.height();  // pixels
        // We need the height in cm
        MFloat cm0 = projectY(obj.getOrigin().y());
        MFloat cm1 = projectY(obj.getOrigin().y() + 1.);
        MFloat cm  = fabs(cm1 - cm0);
        if (cm > 0.)
            height /= cm;
        else
            return;
    }

    MgQPixmapItem* item = new MgQPixmapItem(QPixmap::fromImage(img.mirrored(false, true)));

    MFloat x0 = projectX(obj.getOrigin().x());
    MFloat y0 = projectY(obj.getOrigin().y());
    MFloat x1 = projectX(obj.getOrigin().x() + width);
    MFloat y1 = projectY(obj.getOrigin().y() + height);

    item->setParentItem(currentItem_);
    item->setTargetRect(QRectF(x0, y0, x1 - x0, y1 - y0));
    item->setPos(x0, y0);

    // Set clipping
    MgQLayoutItem* layoutParent = layoutItemStack_.top();

    QRectF bb = layoutParent->boundingRect();
    bb.translate(-QPointF(x0, y0));
    item->setClipRect(bb);
}


/*!
  \brief render pixmaps

  This method renders pixmaps. These are used for cell shading and raster input (GIFs and PNGs).

  \sa renderCellArray()

  \param x0 x of lower corner
  \param y0 y of lower corner
  \param x1 x of higher corner
  \param y1 y of higher corner
  \param w width of pixmap
  \param h height of pixmap
  \param pixmap contents
  \param landscape says if contents is landscape
  \param hasAlpha has the array transparency?

*/
MAGICS_NO_EXPORT bool QtDriver::renderPixmap(MFloat x0, MFloat y0, MFloat x1, MFloat y1, int w, int h,
                                             unsigned char* pixmap, int landscape, bool hasAlpha) const {
    MagLog::debug() << "renderPixmap: " << x0 << " " << y0 << " " << x1 << " " << y1 << endl;


    QImage img = QImage(w, h, QImage::Format_ARGB32);

    // uchar *data = new uchar[w*h*4];
    int srcPos;
    int pixel;

    if (hasAlpha) {
        for (int j = 0; j < h; j++) {
            for (int i = 0; i < w; i++) {
                srcPos = ((h - j - 1) * w + i) * 4;
                // pixel=qRgba(pixmap[srcPos+1],pixmap[srcPos+2],pixmap[srcPos+3],pixmap[srcPos]);
                pixel = qRgba(pixmap[srcPos], pixmap[srcPos + 1], pixmap[srcPos + 2], pixmap[srcPos + 3]);
                img.setPixel(i, j, pixel);
            }
        }
    }
    else {
        for (int j = 0; j < h; j++) {
            for (int i = 0; i < w; i++) {
                srcPos = ((h - j - 1) * w + i) * 3;
                pixel  = qRgba(pixmap[srcPos], pixmap[srcPos + 1], pixmap[srcPos + 2], 0xff);
                img.setPixel(i, j, pixel);
            }
        }

        /*int srcPos=(h-1)*w*3;
        int targetPos=0;
        for(int j=0; j< h; j++)
        {
            for(int i=0; i < w; i++)
            {
                data[targetPos]=pixmap[srcPos+2];
                data[targetPos+1]=pixmap[srcPos+1];
                data[targetPos+2]=pixmap[srcPos];
                data[targetPos+3]=0xff;

                srcPos+=3;
                targetPos+=4;
            }
            srcPos-=w*3;
        }
        img=QImage(data,w,h,QImage::Format_ARGB32);*/
    }


    MgQPixmapItem* item = new MgQPixmapItem(QPixmap::fromImage(img));

    item->setParentItem(currentItem_);
    item->setTargetRect(QRectF(x0, y0, x1 - x0, y1 - y0));
    item->setPos(x0, y0);

    // MgQLayoutItem layoutParent=layoutItemStack_.top();
    // QRectF r=layoutparent->sceneBoundingRect();


    // delete data;

    return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool QtDriver::renderCellArray(const Image& image) const {
    ColourTable& lt  = image.getColourTable();
    const int width  = image.getNumberOfColumns();
    const int height = image.getNumberOfRows();
    const MFloat x0  = projectX(image.getOrigin().x());
    const MFloat y0  = projectY(image.getOrigin().y());
    const MFloat x1  = projectX(image.getOrigin().x() + image.getWidth());
    const MFloat y1  = projectY(image.getOrigin().y() + image.getHeight());

    // Converts colour table into qRGb
    QList<QRgb> cols;
    for (unsigned int i = 0; i < lt.size(); i++) {
        Colour c(lt[i].red(), lt[i].green(), lt[i].blue());
        cols << getQtColour(c).rgb();
    }

    // Create an image
    QImage img(width, height, QImage::Format_ARGB32);
    img.fill(0x00ffffff);

    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            const int in  = width * i + j;
            const short c = image[in];

            if (!(lt[c] == "none")) {
                img.setPixel(j, height - i - 1, cols[c]);
            }
        }
    }
    //
    MgQPixmapItem* item = new MgQPixmapItem(QPixmap::fromImage(img));
    item->setParentItem(currentItem_);
    item->setTargetRect(QRectF(x0, y0 - (y1 - y0), x1 - x0, y1 - y0));
    item->setPos(x0, y0 - (y1 - y0));

    return true;
}


void QtDriver::generateSymbolPath(MgQSymbolItem* qSym, svgSymbol sym) const {
    // setCoordRatio(1.,1.);

    // Symbol size in pixels!!!
    MFloat symbolSize = qSym->size();

    const unsigned int si = sym.elements.size();

    // setNewLineWidth(1.);
    // const MFloat pX = 1. / coordRatioX_;
    // const MFloat pY = 1. / coordRatioY_;

    const MFloat scaling = symbolSize * 0.5;  /// convertCM(1.);

    for (unsigned int i = 0; i < si; i++)  // for all elements in the symbol description
    {
        if (sym.elements[i].name == "circle") {
            const MFloat r  = atof(sym.elements[i].attributes["r"].c_str()) * scaling;
            const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling;
            const MFloat cy = atof(sym.elements[i].attributes["cy"].c_str()) * scaling;
            const int s     = atoi(sym.elements[i].attributes["fill"].c_str());
            circle(cx, cy, r, s, qSym);
        }
        else if (sym.elements[i].name == "snowflake") {
            const MFloat r  = atof(sym.elements[i].attributes["r"].c_str()) * scaling;
            const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling;
            const MFloat cy = atof(sym.elements[i].attributes["cy"].c_str()) * scaling;
            snowflake(cx, cy, r, qSym);
        }
        else if (sym.elements[i].name == "drizzle") {
            const MFloat r  = atof(sym.elements[i].attributes["r"].c_str()) * scaling;
            const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling;
            const MFloat cy = atof(sym.elements[i].attributes["cy"].c_str()) * scaling;
            drizzle(cx, cy, r, qSym);
        }
        else if (sym.elements[i].name == "triangle") {
            const MFloat r  = atof(sym.elements[i].attributes["r"].c_str()) * scaling;
            const int s     = atoi(sym.elements[i].attributes["fill"].c_str());
            const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling;
            const MFloat cy = atof(sym.elements[i].attributes["cy"].c_str()) * scaling;
            const int li    = atoi(sym.elements[i].attributes["line"].c_str());
            triangle(cx, cy, r, s, li, qSym);
        }
        else if (sym.elements[i].name == "lightning") {
            const MFloat r  = atof(sym.elements[i].attributes["r"].c_str()) * scaling;
            const MFloat cx = atof(sym.elements[i].attributes["cx"].c_str()) * scaling;
            const MFloat cy = atof(sym.elements[i].attributes["cy"].c_str()) * scaling;
            lightning(cx, cy, r, qSym);
        }

        else if (sym.elements[i].name == "polyline") {
            vector<PaperPoint> vPP;
            parsePoints(vPP, sym.elements[i].attributes["points"]);

            for (unsigned int s = 0; s < vPP.size(); s++) {
                vPP[s].x(vPP[s].x() * scaling);
                vPP[s].y(vPP[s].y() * scaling);
            }

            QVector<QPointF> pp;
            for (unsigned int j = 0; j < vPP.size(); j++) {
                // pp << QPointF(projectX(vPP[j].x()),projectY(vPP[j].y()));
                pp << QPointF(vPP[j].x(), vPP[j].y());
            }

            bool fill = false;
            if (!magCompare("none", sym.elements[i].attributes["fill"])) {
                fill = true;
            }

            MgQPainterPath path(fill);
            path.addPolygon(pp);

            qSym->addPath(path);
        }
    }  // endfor

    // qSym->setPath(path);

    // resetCoordRatio();
}


MAGICS_NO_EXPORT void QtDriver::renderSymbols(const Symbol& symbol) const {
    // setLineParameters(M_SOLID, 1.); // reset line

    // if still empty
    loadSymbols();

    if (sym_.size() == 0) {
        MagLog::error() << "QtDriver::renderSymbols() --> NO symbols available!" << endl;
        return;
    }

    MagLog::dev() << "Number of symbols: " << symbol.size() << endl;

    const int noSymbols = sym_.size();
    svgSymbol sym;

    int ii = 0;
    for (; ii < noSymbols; ii++) {
        sym = sym_[ii];
        if (sym.id == symbol.getSymbol())
            break;
    }

    if (ii == noSymbols)
        sym = sym_[0];

    // Find out symbol size in pixels!!!!!!!
    MFloat symbolSize = convertCM(symbol.getHeight());

    // Create symbolmanager if needed
    if (!symbolManager_) {
        symbolManager_ = new MgQSymbolManager;
    }

    // If needed compile a display list for the symbol
    // The size of this symbol is "1 cm"
    MgQSymbolItem* actSymbol;
    QString id(QString::fromStdString(sym.id));
    if ((actSymbol = symbolManager_->getSymbol(id, symbolSize)) == 0) {
        actSymbol = symbolManager_->addSymbol(id, symbolSize);
        generateSymbolPath(actSymbol, sym);
    }

    // Set colour
    setNewColour(symbol.getColour());

    MgQLayoutItem* layout = layoutItemStack_.top();
    QRectF brect          = layout->boundingRect();

    MgQSymbolSetItem* item = new MgQSymbolSetItem(actSymbol, brect);
    item->setParentItem(currentItem_);
    item->setColor(getQtColour(currentColour_));

    if (actSymbol->hasFilledPart() && symbol.outline()) {
        const Colour& outCol = symbol.outlineColour();
        item->setOutlineColor(getQtColour(outCol));
    }

    if (symbol.connectLine()) {
        item->setConnectLine(true);
        const Colour& lineCol = symbol.connectLineColour();
        QPen pen(getQtColour(lineCol));

        magics::LineStyle lineStyle = symbol.connectLineStyle();
        if (penStyle_.contains(lineStyle)) {
            pen.setStyle(penStyle_[lineStyle]);
        }
        else {
            pen.setStyle(Qt::SolidLine);
        }

        pen.setWidthF(symbol.connectLineThickness() * lineWidthFactor_);
        pen.setCosmetic(true);
        item->setConnectLinePen(pen);
    }


    if (magnifierIsBeingRedisplayed_ == true) {
        // item->setFlag(QGraphicsItem::ItemIgnoresTransformations);
        item->setKeepSizeWhenScaling(true);
    }

    for (unsigned int i = 0; i < symbol.size(); i++) {
        item->addPoint(projectX(symbol[i].x()), projectY(symbol[i].y()));
    }


    /*for(unsigned int i=0;i<symbol.size();i++)
//	for(unsigned int i=0;i<1000;i++)
    {
        for(int j=0; j < actSymbol->paths().count(); j++)
        {
            MgQPathItem *item=new MgQPathItem(actSymbol->paths().at(j));
            item->setParentItem(currentItem_);
            item->setPen(QColor::fromRgbF(currentColour_.red(),
                        currentColour_.green(),
                        currentColour_.blue()));

            if(actSymbol->paths().at(j).isFilled())
            {
                if(actSymbol->paths().at(j).isDefaultFill())
                {
                    item->setBrush(Qt::white);
                }
                else
                {
                    item->setBrush(QColor::fromRgbF(currentColour_.red(),
                            currentColour_.green(),
                            currentColour_.blue()));
                }
            }

            item->setPos(projectX(symbol[i].x()),projectY(symbol[i].y()));

            item->setBoundingRectSize(symbolSize);

            if(magnifierIsBeingRedisplayed_ == true)
            {
                item->setFlag(QGraphicsItem::ItemIgnoresTransformations);
            }
        }
    }*/
}

/*!
  Plotting a snowflake
*/

MAGICS_NO_EXPORT void QtDriver::snowflake(const MFloat x, const MFloat y, const MFloat size,
                                          MgQSymbolItem* qSym) const {
    const MFloat s5 = size;
    const MFloat s3 = size * 0.78;

    MgQPainterPath path(false);
    QVector<QPointF> line;

    line << QPointF(x - s5, y);
    line << QPointF(x + s5, y);
    path.addPolygon(line);

    line.clear();
    line << QPointF(x - s3, y + s3);
    line << QPointF(x + s3, y - s3);
    path.addPolygon(line);

    line.clear();
    line << QPointF(x - s3, y - s3);
    line << QPointF(x + s3, y + s3);
    path.addPolygon(line);

    qSym->addPath(path);
}

/*!
  Plotting drizzle
*/
MAGICS_NO_EXPORT void QtDriver::drizzle(const MFloat x, const MFloat y, const MFloat size, MgQSymbolItem* qSym) const {
    const MFloat s2 = size * .5;

    circle(x, y, s2, 8, qSym);

    MgQPainterPath path(false);
    QVector<QPointF> line;
    line << QPointF(x + (s2 * 0.8), y);
    line << QPointF(x, y - size);
    path.addPolygon(line);

    qSym->addPath(path);
}

/*!
  Plotting lightning symbol
*/
MAGICS_NO_EXPORT void QtDriver::lightning(const MFloat x, const MFloat y, const MFloat size,
                                          MgQSymbolItem* qSym) const {
    MgQPainterPath path(false);
    QVector<QPointF> pp;

    pp << QPointF(x - (size * .9), y - (size * .9));
    pp << QPointF(x - (size * .9), y + (size * .9));
    path.addPolygon(pp);

    pp.clear();
    pp << QPointF(x - (size * .9), y + (size * .9));
    pp << QPointF(x + (size * .9), y + (size * .9));
    pp << QPointF(x + (size * .1), y);
    pp << QPointF(x + (size * .9), y - (size * .9));
    path.addPolygon(pp);

    pp.clear();
    pp << QPointF(x + (size * .8), y - (size * .4));
    pp << QPointF(x + (size * .9), y - (size * .9));
    pp << QPointF(x + (size * .45), y - (size * .8));
    path.addPolygon(pp);

    qSym->addPath(path);
}

/*!
  Plotting a triangle
*/
MAGICS_NO_EXPORT void QtDriver::triangle(const MFloat x, const MFloat y, const MFloat size, const int fill, const int l,
                                         MgQSymbolItem* qSym) const {
    const MFloat s = 0.5 * size;

    bool fillIt = (fill >= 1) ? true : false;

    MgQPainterPath path(fillIt);
    QVector<QPointF> pp;

    pp << QPointF(x + s, y - s);
    pp << QPointF(x - s, y - s);
    pp << QPointF(x, y + s);
    pp << QPointF(x + s, y - s);
    path.addPolygon(pp);

    qSym->addPath(path);

    if (fill < 1 && l > 0) {
        pp.clear();
        MgQPainterPath lp(false);
        pp << QPointF(x + s * .6, y - (s * .5));
        pp << QPointF(x - s * .6, y - (s * .5));
        lp.addPolygon(pp);
        qSym->addPath(lp);
    }
}

MAGICS_NO_EXPORT void QtDriver::executeStep(int step) const {
    /*MgQPlotScene *scene = static_cast<MgQPlotScene*>(scene_);

    const SceneLayer &sl=scene->sceneLayerItem()->layer();
    for(vector<Layer*>::const_iterator itL = sl.beginLayer();itL != sl.endLayer(0); ++itL)
    {
        executeStep(step,scene->layerItem(**itL);
    }*/
}

void QtDriver::executeStep(int step, MgQLayerItem* layerItem) const {
    QGraphicsItem* prevItem = currentItem_;

    MgQLayoutItem* parentLayout = layerItem->parentLayoutItem();
    layoutItemStack_.push(parentLayout);

    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    dimensionX_ = parentLayout->dimensionX();
    dimensionY_ = parentLayout->dimensionY();

    MgQStepItem* item = new MgQStepItem(0);
    item->setParentItem(layerItem->rootItem());
    item->setCached(true);
    layerItem->setStep(step, item);

    currentItem_ = item;

    MgQPlotScene* scene = static_cast<MgQPlotScene*>(scene_);

    // Notify scene item about the new step
    // scene->sceneLayerItem()->sceneLayer().reset();

    scene->currentSceneItem()->sceneLayerItem()->sceneLayer().execute(&layerItem->layer(), step, *this);

    // layerItem->layer().execute(step,*this);

    currentItem_ = prevItem;
    layoutItemStack_.pop();

    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();
}

void QtDriver::executeStep(int step, MgQLayerItem* layerItem, const SceneLayer& sceneLayer) const {
    QGraphicsItem* prevItem = currentItem_;

    MgQLayoutItem* parentLayout = layerItem->parentLayoutItem();
    layoutItemStack_.push(parentLayout);

    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    dimensionX_ = parentLayout->dimensionX();
    dimensionY_ = parentLayout->dimensionY();

    MgQStepItem* item = new MgQStepItem(0);
    item->setParentItem(layerItem->rootItem());
    item->setCached(true);
    layerItem->setStep(step, item);

    currentItem_ = item;

    sceneLayer.execute(&layerItem->layer(), step, *this);

    currentItem_ = prevItem;
    layoutItemStack_.pop();

    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();
}


MAGICS_NO_EXPORT void QtDriver::executeMagnifier(Layer* layer, MgQMagnifierLayoutItem* layoutItem) const {
    QGraphicsItem* prevItem = currentItem_;
    layoutItemStack_.push(layoutItem);

    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    dimensionX_ = layoutItem->dimensionX();
    dimensionY_ = layoutItem->dimensionY();

    scalesX_.push(coordRatioX_);
    scalesY_.push(coordRatioY_);
    coordRatioX_ = layoutItem->coordRatioX();
    coordRatioY_ = layoutItem->coordRatioY();

    magnifierZoomFactor_         = layoutItem->zoomFactor();
    magnifierIsBeingRedisplayed_ = true;

    // QGraphicsItem *rootItem=new QGraphicsItem("MagnifierRoot");
    // item->setRootItem(rootItem);
    // rootItem->setParentItem(item);

    currentItem_ = layoutItem;

    layer->magnify(*this, layoutItem->resolutionX(), layoutItem->resolutionY());

    // item->layout().redisplay(*this,
    //	      pp,item->resolutionX(),item->resolutionY());

    magnifierZoomFactor_         = 1.;
    magnifierIsBeingRedisplayed_ = false;

    currentItem_ = prevItem;
    layoutItemStack_.pop();

    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();

    coordRatioX_ = scalesX_.top();
    coordRatioY_ = scalesY_.top();
    scalesX_.pop();
    scalesY_.pop();
}

MAGICS_NO_EXPORT void QtDriver::executeHisto(Layer* layer, MgQHistoItem* layoutItem, QString visdefName,
                                             QString visdefClass) const {
    QGraphicsItem* prevItem = currentItem_;
    // layoutItemStack_.push(layoutItem);

    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    // dimensionX_=layoutItem->dimensionX();
    // dimensionY_=layoutItem->dimensionY();
    dimensionX_ = 150;
    dimensionY_ = 100;

    scalesX_.push(coordRatioX_);
    scalesY_.push(coordRatioY_);
    // coordRatioX_ = layoutItem->coordRatioX();
    // coordRatioY_ = layoutItem->coordRatioY();
    coordRatioX_ = 1;
    coordRatioY_ = 1;

    // magnifierIsBeingRedisplayed_=true;

    currentItem_ = layoutItem;

    layer->histogram(*this, visdefName.toStdString(), visdefClass.toStdString());

    // magnifierIsBeingRedisplayed_=false;

    currentItem_ = prevItem;
    // layoutItemStack_.pop();

    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();

    coordRatioX_ = scalesX_.top();
    coordRatioY_ = scalesY_.top();
    scalesX_.pop();
    scalesY_.pop();
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const Layer& layer) const {
    //	newLayer(layer);
    //	layer.visit(*this);
    //	closeLayer(layer);
}


MAGICS_NO_EXPORT void QtDriver::redisplay(const PreviewLayout& layout) const {
    project(layout);
    layout.visit(*this);
    unproject();
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const MagnifierLayout& layout) const {
    project(layout);
    layout.visit(*this);
    unproject();
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const HistoLayout& layout) const {
    project(layout);
    layout.visit(*this);
    unproject();
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const SceneLayout& layout) const {
    project(layout);
    layout.visit(*this);
    unproject();
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const SceneLayer& layer) const {
    MagLog::dev() << "QtDriver::redisplay(const SceneLayer& layer)" << endl;

    MgQPlotScene* scene   = static_cast<MgQPlotScene*>(scene_);
    MgQSceneLayerItem* ln = new MgQSceneLayerItem(layer);
    MgQSceneItem* item    = scene->currentSceneItem();

    if (item) {
        item->setSceneLayerItem(ln);
        item->setStepNum(layer.numberOfSteps());
    }

    // currentItem_=item;

    for (vector<Layer*>::iterator itL = layer.beginLayer(); itL != layer.endLayer(); ++itL) {
        (*itL)->newLayer(*this);
        (*itL)->execute(*this);
        (*itL)->closeLayer(*this);
    }

    scene->restoreLayerState();

    // currentSceneLayoutItem_=0;

    /*AnimationStep* step = layer.rules()->at(0);

        for (list<Layer*>::const_iterator itL = layer.begin(); itL != layer.end(); ++itL)
        {
               map<Layer*, int>::iterator itS = step->find(*itL);
            if ( itS == step->end() )
            {
                (*itL)->execute(*this);
            }
                else
            {
                    (*itL)->execute(itS->second,*this);
            }
        }*/


    /*for (int i = 0; i < layer.numberOfSteps(); i++) {
        layer.execute(i, *this);
    }*/
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const StaticLayer& layer) const {
    MagLog::dev() << "QtDriver::redisplay(const StaticLayer& layer)" << endl;
    layer.visit(*this);
}

MAGICS_NO_EXPORT void QtDriver::redisplay(const StepLayer&) const {
    MagLog::dev() << "QtDriver::redisplay(const StepLayer& layer)" << endl;
}


void QtDriver::redisplay(const Arrow& arrow) const {
    MgQLayoutItem* layout = layoutItemStack_.top();
    QRectF brect          = layout->boundingRect();

    MgQPolylineSetItem* item = new MgQPolylineSetItem(brect);
    item->setParentItem(currentItem_);

    currentPolylineSetItem_ = item;

    BaseDriver::renderWindArrow(arrow);

    currentPolylineSetItem_ = 0;
}

void QtDriver::redisplay(const Flag& flag) const {
    MgQLayoutItem* layout = layoutItemStack_.top();
    QRectF brect          = layout->boundingRect();

    MgQPolylineSetItem* item = new MgQPolylineSetItem(brect);
    item->setParentItem(currentItem_);

    currentPolylineSetItem_ = item;

    BaseDriver::renderWindFlag(flag);

    currentPolylineSetItem_ = 0;
}
/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void QtDriver::debugOutput(const std::string& s) const {
    MagLog::debug() << s << endl;
}

/*!
  \brief class information are given to the output-stream
*/
void QtDriver::print(ostream& out) const {
    out << "QtDriver[";
    out << "]";
}

QColor QtDriver::getQtColour(const Colour& col) const {
    qreal r     = col.red();
    qreal g     = col.green();
    qreal b     = col.blue();
    qreal alpha = col.alpha();

    if (r < 0.)
        r = 0.;
    else if (r > 1.)
        r = 1.;

    if (g < 0.)
        g = 0.;
    else if (g > 1.)
        g = 1.;

    if (b < 0.)
        b = 0.;
    else if (b > 1.)
        b = 1.;

    if (alpha < 0.)
        alpha = 0.;
    else if (alpha > 1.)
        alpha = 1.;

    return QColor::fromRgbF(r, g, b, alpha);
}

static SimpleObjectMaker<QtDriver, BaseDriver> Qt_driver("Qt");
