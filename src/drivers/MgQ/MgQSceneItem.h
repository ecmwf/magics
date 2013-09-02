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
    \file MgQSceneLayout.h
    \brief Definition of MgQSceneLayout.
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#ifndef MgQSceneItem_H
#define MgQSceneItem_H

#include "MgQ.h"
#include "MgQLayoutItem.h"

#include <QMap>
#include "Layer.h"
#include "QtDriver.h"
#include "MgQScene.h"

class QProgressDialog;

class MgQDriverObject;
class MgQLayerItem;
class MgQLayerState;
class MgQPreviewLayoutItem;
class MgQSceneLayerItem;
class MgQStepMetaData;                        

using magics::SceneLayer;

class MgQIcon
{
public:
    
        MgQIcon(QString name, QString cs, QString id) : 
  		name_(name), class_(cs), id_(id) {}
  		
	QString name_;
	QString class_;
	QString id_;
};	

typedef QList<MgQIcon> MgQIconList;


class MgQSceneItem : public MgQLayoutItem
{
  
public:
	enum {Type = MgQ::SceneItemType}; 
	
	MgQSceneItem(const Layout &);							
	~MgQSceneItem();

	int type() const {return Type;}	
	
	void clearBeforeNewRequest();
	void saveStateBeforeNewRequest();
	void restoreLayerState();

	MgQLayoutItem* findPreviewLayout();
	MgQLayoutItem* findMagnifierLayout();

	//MgQAnimation* animation();
	//void addAnimation(MgQLayoutItem*,MgQAnimation*);
	void selectCurrentStepForAnimation();
	
	void setStepNum(int n) {stepNum_=n;} 
	int stepNum();
	int currentStep();
	bool stepCached(int);
	void setCurrentStep(int,bool update=true);
	void setPrevCurrentStep(int i) {prevCurrentStep_=i;};
	void setStepVisible(int,bool);
	QStringList stepLabel(int) {return QStringList();}
	void stepMetaData(MgQStepMetaData*);

	MgQMagnifierLayoutItem* updateMagnifier(float);
	void clearMagnifier();

	void setSceneLayerItem(MgQSceneLayerItem *item) {sceneLayerItem_=item;}
	MgQSceneLayerItem* sceneLayerItem() {return sceneLayerItem_;}
	
	const QList<MgQLayerItem*>& layerItems() {return layerItems_;}
	void addLayerItem(MgQLayerItem*);
	MgQLayerItem* layerItem(const Layer&);

	//void collectLayerData(QList<QPointF> &,QMap<int,QList<QStringList> > &,bool addLayerName=true);
	//void collectLayerDataForCurrentStep(QList<QPointF> &,QList<QStringList> &,bool addLayerName=true);
	//void collectLayerData(QList<QPointF> &,QList<QStringList> &,int,bool addLayerName=true);

	void collectLayerData(QList<QPointF> &,QMap<int,QList<ValuesCollector> > &,double,double);
	void collectLayerDataForCurrentStep(QList<QPointF> &,QList<ValuesCollector> &,double,double);
	void collectLayerData(QList<QPointF> &,QList<ValuesCollector> &,int,double,double);
	void collectLayerDataForCurrentStep(MgQLayerItem *,ValuesCollector& data);
	void collectLayerData(MgQLayerItem *,ValuesCollector& ,int );

	void layerMetaDataForCurrentStep(MgQLayerItem *,MetaDataCollector&);
	void layerMetaData(MgQLayerItem *,MetaDataCollector&,int);
	void layerDataIndexForCurrentStep(MgQLayerItem *,DataIndexCollector&);
	void layerDataIndex(MgQLayerItem *,DataIndexCollector&,int);
	QPixmap layerInfoImageForCurrentStep(MgQLayerItem *,QHash<QString,QString>);
	QPixmap layerInfoImage(MgQLayerItem *,int,QHash<QString,QString>);
	void layerIconsForCurrentStep(MgQLayerItem *,MgQIconList&);
	void layerIcons(MgQLayerItem *,MgQIconList&,int);
	void  renderLayerPreview();
   
	void addPreviewLayoutItem(MgQPreviewLayoutItem* item) {previewLayoutItem_=item;}
	MgQPreviewLayoutItem* previewLayoutItem() {return previewLayoutItem_;}

	//QList<MgQLayerState*> previousLayerState() {return previousLayerState_;}

	void setDriverObject(MgQDriverObject* drv) {driverObject_=drv;}

	void renderForMagnifier(QPainter *, const QRectF &, const QRectF &);
	void renderForPrinter(QPainter *);
	void renderForVideo(QPainter *,QProgressDialog *,QString,QStringList &);

	void updateAnimation();
	void updateLayers();
	void setEnableAntialias(bool);
	
	void addProjectorItem(MgQLayoutItem *n) {projectorItems_ << n;}
	QList<MgQLayoutItem*> projectorItems() {return projectorItems_;}
	MgQLayoutItem* findProjectorItem(QPointF);
	MgQLayoutItem* firstProjectorItem();

protected:
	void scanLayers();
	void updateCache();
	void drawBackground( QPainter * painter, const QRectF & rect ); 

	int prevCurrentStep_;
	int currentStep_;
	int stepNum_;

	//QRectF magnifierSceneRect_;
	//float  magnifierFactor_;

	MgQSceneLayerItem *sceneLayerItem_;
	
	MgQPreviewLayoutItem* previewLayoutItem_;

	QList<MgQLayerItem*> layerItems_;
	QList<MgQLayerState*> previousLayerState_;
	QList<MgQLayoutItem*> projectorItems_;

	MgQDriverObject *driverObject_;
	
	bool antialias_;
};


#endif
