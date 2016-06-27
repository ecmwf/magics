/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
