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

/*!
    \file MgQPlotScene.h
    \brief Definition of MgQPlotScene.
    \author Graphics Section, ECMWF

    Started: March 2010
*/

#ifndef MgPlotScene_H
#define MgQPlotScene_H

#include <QGraphicsScene>
#include <QMap>

#include "QtDriver.h"
#include "MgQScene.h"

// for AIX compiler which fails when we have two definition of a macro
// (gcc also complains, but does not fail)
// in any case, we do not use the macro, so removing it should be safe
#undef ABS

class QGraphicsRectItem;
class QProgressDialog;

class MgQDriverObject;
class MgQLayerItem;
class MgQLayerState;
class MgQLayoutItem;
class MgQPreviewLayoutItem;
class MgQRootItem;
class MgQSceneCacheItem;
class MgQSceneItem;
class MgQSceneLayerItem;
class MgQStepMetaData;

using magics::Layer;
using magics::SceneLayer;

class MgQPlotScene : public MgQScene
{

  Q_OBJECT
  
public:
	MgQPlotScene(QObject *parent=0);
	~MgQPlotScene();
	
	enum PlotScaleMode {RelativeToCurrentSize,RelativeToOriSize};
	
	void clearBeforeNewRequest();
	void saveStateBeforeNewRequest();
	void restoreLayerState();
	
	void addSceneItem(MgQSceneItem*);
	MgQSceneItem* currentSceneItem();
	void setCurrentSceneItem(MgQSceneItem*);	
	QList<MgQSceneItem*> sceneItems() {return sceneItems_;}
	MgQSceneItem* findSceneItem(QPointF);

	QRectF oriSceneRect() {return oriSceneRect_;}
	void setOriSceneRect(QRectF r) {oriSceneRect_=r;}

	MgQLayoutItem* firstProjectorItem();
	MgQLayoutItem* findProjectorItem(QPointF);
	MgQLayoutItem* projectorItem(QPointF p) {return findProjectorItem(p);}
	bool identifyPos(QPointF,MgQSceneItem**,MgQLayoutItem**);

	void setEnableAntialias(bool);
	void updateAfterNewRequest();	
	void renderForMagnifier(QPainter *, const QRectF &, const QRectF &);
    
        void sceneItemChanged();
	void highlightSceneItem(MgQSceneItem*, bool );	
	void highlightSceneItemForBrief(MgQSceneItem*, bool );	
	void setPlotScale(float, PlotScaleMode);
	float plotScale();
	
	void setDpiResolution(int r) {dpiResolution_=r;}
	int dpiResolution() {return dpiResolution_;}

public slots:	
	void slotSceneRectChanged(const QRectF&);

protected:	
	void updateCache();
	void drawBackground ( QPainter * painter, const QRectF & rect ); 
	
	bool ignoreSceneItemChange_;
	
	QList<int> prevSceneItemCurrentStep_;
	int stepNum_;

	QRectF magnifierSceneRect_;
	float  magnifierFactor_;

	MgQPreviewLayoutItem* previewLayoutItem_;
	QList<MgQSceneItem*> sceneItems_;
	int currentSceneItemIndex_;
	
	QGraphicsRectItem* highlightItem_;
	MgQSceneItem* highlightedSceneItem_;
	
	QGraphicsRectItem* highlightItemForBrief_;
		
	MgQSceneLayerItem *sceneLayerItem_;
	QList<MgQLayerItem*> layerItems_;
	QList<QList<MgQLayerState*> > previousSceneState_;
	
	MgQDriverObject *driverObject_;

	QPixmap *cacheDevice_;
	QPainter* cachePainter_;
	MgQSceneCacheItem*  cacheItem_;

	bool antialias_;
	QRectF oriSceneRect_;
	
	int dpiResolution_;
};



#endif
