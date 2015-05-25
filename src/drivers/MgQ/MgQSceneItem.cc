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
    \file MgQSceneItem.cc
    \brief Definition of MgQSceneItem
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#include "MgQSceneItem.h"

#include <QDebug>
#include <QGraphicsItem>
#include <QImage>
#include <QPixmap>
#include <QPainter>
#include <QProgressDialog>
#include <QStyleOptionGraphicsItem>

#include "MgQDriverObject.h"
#include "MgQHistoItem.h"
#include "MgQLayerItem.h"
#include "MgQLayoutItem.h"
#include "MgQStepMetaData.h"

#include "MgQPlotScene.h"

MgQSceneItem::MgQSceneItem(const Layout& layout) : MgQLayoutItem(layout)							
{
	prevCurrentStep_=0;
	driverObject_=0;

	sceneLayerItem_=0;
	previewLayoutItem_=0;
	
	antialias_=false;
	
	stepNum_=0;
}
 
MgQSceneItem::~MgQSceneItem()
{
	if(driverObject_)
		delete driverObject_;

	foreach(MgQLayerState *st,previousLayerState_)
	{
		delete st;
	}	
}

void MgQSceneItem::clearBeforeNewRequest()
{
/*	if(rootItem_)
	{
		removeItem(rootItem_);
		delete rootItem_;
		rootItem_=0;
	}
*/
	//Animation
	prevCurrentStep_=currentStep_;


	layerItems_.clear();
	sceneLayerItem_=0;
	previewLayoutItem_=0;
	stepNum_=0;
}

void MgQSceneItem::saveStateBeforeNewRequest()
{
	foreach(MgQLayerState *st,previousLayerState_)
	{
		delete st;
	}	
	previousLayerState_.clear();

	foreach(MgQLayerItem *item,layerItems_)
	{
		MgQLayerState *st=new MgQLayerState;
		item->saveLayerState(st);
		previousLayerState_.push_back(st);		
	}
}

MgQLayoutItem* MgQSceneItem::findProjectorItem(QPointF scenePos)
{
	foreach(MgQLayoutItem* item, projectorItems_)
	{			
	  	//qDebug() << "projector" << item->name() << scenePos << item->mapFromScene(scenePos);
	  	if(item->contains(item->mapFromScene(scenePos)))
		{			  
			return item;
		}
	}
	return 0;	
}

MgQLayoutItem* MgQSceneItem::firstProjectorItem()
{		
	if(projectorItems_.count() > 0)
	{
		 return projectorItems_.at(0);	

	}
	
	return 0;
}


MgQLayoutItem* MgQSceneItem::findPreviewLayout()
{
	return 0;

	/*foreach(QGraphicsItem *item, items())
	{
		if(item->data(MgQ::ItemType).toInt() == MgQ::PreviewLayoutItem)
		{
			
			MgQLayoutItem *layout=static_cast<MgQLayoutItem*>(item);
			return layout;
		}
	}

	return 0;*/		
}

MgQLayoutItem* MgQSceneItem::findMagnifierLayout()
{
/*	foreach(QGraphicsItem *item, items())
	{
		if(checkItemType(item,MgQ::MagnifierLayoutItem))
		{			
			MgQLayoutItem *layout=static_cast<MgQLayoutItem*>(item);
			return layout;
		}
	}
*/
	return 0;		
}


//-------------------------------------
// Animation
//-------------------------------------

void MgQSceneItem::updateAnimation()
{
  	MgQPlotScene *sc=static_cast<MgQPlotScene*>(scene());
 
	sc->sceneItemChanged();
	
	//Regenarete the cache
	//updateCache();
	
	//Repaint everything
	//update();
}

void MgQSceneItem::selectCurrentStepForAnimation()
{
	setCurrentStep(prevCurrentStep_);
}

int MgQSceneItem::currentStep()
{
	return currentStep_;
}

int MgQSceneItem::stepNum()
{
	return stepNum_;
}

void MgQSceneItem::setCurrentStep(int step,bool update)
{
	currentStep_=step;

	if(currentStep_< 0 || currentStep_ >= stepNum_)
		return;

	for(int i=0; i < stepNum_; i++)
	{
		if(i!= currentStep_)
		{
			setStepVisible(i,false);
		}
		else
		{
			setStepVisible(i,true);	
			//updateCache(); //Why we need to call it here????
		}
	}
	
	if(update)
        	updateAnimation();
}

void MgQSceneItem::setStepVisible(int step,bool visible)
{
	if(visible == true && stepCached(step) ==false)
	{
		//Notify scene item about the new step
		//sceneLayer_.reset();
		sceneLayerItem_->sceneLayer().getReady(step);
		foreach(MgQLayerItem *item,layerItems_)
		{
			if(item->stepNum() > 0 && item->stepCached(step) == false)
			{
				//driverObject_->driver().executeStep(step,item);
				driverObject_->driver().executeStep(step,item,sceneLayerItem_->sceneLayer());
				//item->setStepVisible(step,visible);
			}
		}
		
	}

	foreach(MgQLayerItem *item,layerItems_)
	{
		if(item->stepNum() > 0)
		{
			item->setStepVisible(step,visible);
		}
	}
}

bool MgQSceneItem::stepCached(int step)
{
	foreach(MgQLayerItem *item,layerItems_)
	{
		if(item->stepNum() > 0 && item->stepCached(step) == false)
		{
			return false;
		}
		
	}

	return true;
}

void MgQSceneItem::stepMetaData(MgQStepMetaData* metaData)
{
	if(!sceneLayerItem_)
		return;

	const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer(); 
	
	MetaDataCollector stepData;
	MetaDataAttribute attr;
	attr.setSource(MetaDataAttribute::AnySource); //to enable any metadata query method

	for(int step=0; step < stepNum_; step++)
	{	
		metaData->addStep("");

		for(vector<Layer*>::iterator it = snl.beginLayer(step); it != snl.endLayer(step); ++it) 
		{
			//Layer& l=*it;

			foreach(QString str,metaData->keys())
			{
				stepData[str.toStdString()]="";
				stepData.setAttribute(str.toStdString(),attr);
			}

			(*it)->collect(stepData);
			
			for(map<string,string>::iterator itK=stepData.begin(); itK!= stepData.end(); itK++)
			{
				QString keyStr=QString::fromStdString(itK->first);
				QString valueStr=QString::fromStdString(itK->second);
				if(valueStr != "")
				{
					valueStr+="\n";

					/*if(keyStr == "MV_Format")
					{
						valueStr="GRIB";
						valueStr+="\n";
					}*/

					metaData->appendToStepData(keyStr,step,valueStr);
				}
			}
		}
	}

	/*

	foreach(MgQLayerItem *item,layerItems_)
	{
		if(item->stepNum() > 0)
		{
			int step=0;
			Layer& l=item->layer();
			StepLayer *layer=static_cast<StepLayer*>(&l);
			for(vector<SingleLayer*>::iterator it=layer->firstStep(); it != layer->endStep(); it++)
			{
				foreach(QString str,metaData->keys())
				{
					stepData[str.toStdString()]="";
				}				
				
				(*it)->metadata(stepData);
				
				if(firstLayer || metaData->stepNum() <= step)
				{
					metaData->addStep("");
				}

				for(map<string,string>::iterator itK=stepData.begin(); itK!= stepData.end(); itK++)
				{
					QString keyStr=QString::fromStdString(itK->first);
					QString valueStr=QString::fromStdString(itK->second);
					valueStr+="\n";


					if(keyStr == "MV_Format")
					{
						valueStr="GRIB";
						valueStr+="\n";
					}


					metaData->appendToStepData(keyStr,step,valueStr);
				}

				step++;
			}
			
			firstLayer=false;
		
		}
	}	*/
}


//-------------------------------------
// Layers
//-------------------------------------

void MgQSceneItem::updateLayers()
{
	MgQPlotScene *sc=static_cast<MgQPlotScene*>(scene());
 	
	sc->sceneItemChanged();

	//Regenarete the cache
//	updateCache();
	
	//Repaint everything
//	update();
}
 
void MgQSceneItem::addLayerItem(MgQLayerItem* item) 
{
	layerItems_.push_back(item);
	//item->setStackLevel(layerItems_.count()-1);
	ASSERT(item->layer().zindex() >= 0);
	item->setStackLevel(item->layer().zindex());
}


MgQLayerItem* MgQSceneItem::layerItem(const Layer& layer)
{
	foreach(MgQLayerItem *item,layerItems_)
	{
		if(&item->layer() == &layer)
		{
			return item;
		}
	}
	
	return 0;
}

void MgQSceneItem::restoreLayerState()
{
	if(previousLayerState_.count() == 0)
		return;

	return;
	
	QList<MgQLayerItem*> newItems;
	QMap<int,MgQLayerItem*> stStackLevelToItem;	
	QSet<MgQLayerState*> stToItem;
	
	foreach(MgQLayerItem *item,layerItems_)
	{
		bool found=false;
		foreach(MgQLayerState *st,previousLayerState_)
		{
			if(stToItem.contains(st) ==  false &&
			   st->id_ == QString::fromStdString(item->layer().id()))
			{
				stToItem.insert(st);
				stStackLevelToItem[st->stackLevel_]=item;
				item->setLayerAlpha(st->alpha_);
				item->setLayerVisibility(st->visible_);
				found=true;
				break;
			}
		}
		if(!found)
		{
			newItems.push_back(item);
		}
	}	

	//Set stack level for the "old" items
	int currenStackLevel=0;
	foreach(int level,stStackLevelToItem.keys())
	{
		//qDebug() << "restore" << QString::fromStdString(stStackLevelToItem[level]->layer().id()) << level << currenStackLevel;
		stStackLevelToItem[level]->setStackLevel(currenStackLevel);
		currenStackLevel++;
	}
	
	//Set stack level for the "new" items 
	foreach(MgQLayerItem *item,newItems)
	{
		item->setStackLevel(currenStackLevel);
		currenStackLevel++;
	}
}

void MgQSceneItem::collectLayerData(QList<QPointF> &pos,QMap<int,QList<ValuesCollector> > &val,
				    double searchRadiusX, double searchRadiusY)
{
	for(int step=0; step < stepNum_; step++)
	{	
		collectLayerData(pos,val[step],step,searchRadiusX,searchRadiusY);
	}
}

void MgQSceneItem::collectLayerDataForCurrentStep(QList<QPointF> &pos,QList<ValuesCollector> &val,
						  double searchRadiusX, double searchRadiusY)
{
	collectLayerData(pos,val,currentStep_,searchRadiusX,searchRadiusY);
}

void MgQSceneItem::collectLayerData(QList<QPointF> &pos,QList<ValuesCollector> &val,int step,
				    double searchRadiusX,  double searchRadiusY)
{
	if(!sceneLayerItem_)
		return;

	const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer(); 	
	QString str; 

	QList<Layer*> layerLst;
		
	foreach(MgQLayerItem* item,layerItems_)	 
	{	  
		if(item->stepNum() == 0)
		{
			layerLst << item->layer().get();
		}
	}	
	
	for(vector<Layer*>::iterator it = snl.beginLayer(step); it != snl.endLayer(step); ++it) 
	{		
		if(layerLst.indexOf(*it) == -1)
		{  
			layerLst << *it;
		}	
	}  
	  
	foreach(Layer *layer,layerLst)  
	{  
	  	if(!layer)
		  	continue;
		
		if(layer->visibility() == true)
		{
			ValuesCollector layerData(layer->name());
			layerData.setSearchRadius(searchRadiusX,searchRadiusY);
			
			foreach(QPointF pp,pos)
			{		
				layerData.push_back(ValuesCollectorPoint(pp.x(),pp.y()));
			}		
	
			layer->collect(layerData);
	
			val << layerData;
		}	
	}

}

void MgQSceneItem::collectLayerDataForCurrentStep(MgQLayerItem *item,ValuesCollector& data)
{
	return collectLayerData(item,data,currentStep_);
}

void MgQSceneItem::collectLayerData(MgQLayerItem *item,ValuesCollector& data,int step)
{	
	if(!item || !sceneLayerItem_ || 
	   !layerItems_.contains(item))
		return;

	if(item->stepNum() == 0)
	{
		item->layer().collect(data);
	}
	else
	{	
		const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer();
		Layer *stepLayer=snl.findLayer(&item->layer(),step);
		if(stepLayer)
		{
		  	 stepLayer->collect(data);
		}	
	}
}


void MgQSceneItem::layerMetaDataForCurrentStep(MgQLayerItem *item,MetaDataCollector& data)
{
	return layerMetaData(item,data,currentStep_);
}

void MgQSceneItem::layerMetaData(MgQLayerItem *item,MetaDataCollector& data,int step)
{	
	if(!item || !sceneLayerItem_ || 
	   !layerItems_.contains(item))
		return;

	if(item->stepNum() == 0)
	{
		item->layer().collect(data);
	}
	else
	{	
		const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer();
		Layer *stepLayer=snl.findLayer(&item->layer(),step);
		if(stepLayer)
		{
		  	 stepLayer->collect(data);
		}	
	}
}

void MgQSceneItem::layerDataIndexForCurrentStep(MgQLayerItem *item,DataIndexCollector& data)
{
	return layerDataIndex(item,data,currentStep_);
}

void MgQSceneItem::layerDataIndex(MgQLayerItem *item,DataIndexCollector& data,int step)
{	
	if(!item || !sceneLayerItem_ || 
	   !layerItems_.contains(item))
		return;

	if(item->stepNum() == 0)
	{
		item->layer().collect(data);
	}
	else
	{	
		const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer();
		Layer *stepLayer=snl.findLayer(&item->layer(),step);
		if(stepLayer)
		{
		  	 stepLayer->collect(data);
		}	
	}
}


QPixmap MgQSceneItem::layerInfoImageForCurrentStep(MgQLayerItem *item,QHash<QString,QString> imageId)
{
	return layerInfoImage(item,currentStep_,imageId);
}

QPixmap MgQSceneItem::layerInfoImage(MgQLayerItem *item,int step,QHash<QString,QString> imageId)
{
	if(!item || !sceneLayerItem_ || 
	   !layerItems_.contains(item))
		return QPixmap();
		
	MgQHistoItem *histoItem=item->histoItem(step);
	if(!histoItem)	
		return QPixmap();
		
	if(!histoItem->cached() || histoItem->pixmapId() != imageId)
	{
		if(histoItem->cached())
		{  
			histoItem=item->resetHistoItem(step);
		}
		
		histoItem->setPixmapId(imageId);
		
		Layer *stepLayer;

		if(item->stepNum() == 0)
		{
			stepLayer=&item->layer();
		}
		else
		{			
			const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer(); 	
			stepLayer=snl.findLayer(&item->layer(),step);
		}

		if(!stepLayer)
			return QPixmap();
			
		QString visdefName=imageId.value("visdefName");
		QString visdefClass=imageId.value("visdefClass");
		driverObject_->driver().executeHisto(stepLayer,histoItem,visdefName,visdefClass);
	}	

	return item->histoPixmap(step,QSize(300,200));
}

void MgQSceneItem::layerIconsForCurrentStep(MgQLayerItem *item,MgQIconList& icons)
{
	return layerIcons(item,icons,currentStep_);
}

void MgQSceneItem::layerIcons(MgQLayerItem *item,MgQIconList& icons,int step)
{	
	if(!item || !sceneLayerItem_ || 
	   !layerItems_.contains(item))
		return;

		
	for(vector<MetviewIcon>::const_iterator it=item->layer().iconsBegin(); it != item->layer().iconsEnd(); it++)
	{
		icons << MgQIcon(QString::fromStdString((*it).iconName()),
				 QString::fromStdString((*it).iconClass()),
				 QString::fromStdString((*it).iconId()));
	}
		
	/*if(item->stepNum() == 0)
	{
		for(vector<pair<string, string>  >::const_iterator it=item->layer().iconsBegin(); it != item->layer().iconsEnd(); it++)
		{
			icons << QPair<QString,QString>(QString::fromStdString(it->first),QString::fromStdString(it->second));
		}	
	}
	else
	{	
		const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer();
		Layer *stepLayer=snl.findLayer(&item->layer(),step);
		if(stepLayer)
		{
		  	for(vector<pair<string, string>  >::const_iterator it=stepLayer->iconsBegin(); it != stepLayer->iconsEnd(); it++)
			{
				icons << QPair<QString,QString>(QString::fromStdString(it->first),QString::fromStdString(it->second));	
			}
		}	
	}*/
}

void MgQSceneItem::renderLayerPreview()
{
	foreach(MgQLayerItem *item,layerItems_)
	{
		item->renderPreview();
	}
	
	/*preview_=QImage();
	
	foreach(MgQLayerItem *item,layerItems_)
	{
		QImage img=item->preview();
		preview_
	}*/
	
}

//----------------------------------------
// Magnifier
//---------------------------------------

MgQMagnifierLayoutItem* MgQSceneItem::updateMagnifier(float zoomFactor)
{
	//magnifierZoom_=zoom;
#if 0	
	MgQLayerItem *magLayerItem=0;	

	foreach(MgQLayerItem *item,layerItems_)
	{
		if(item->stepNum() > 0) //&& item->stepCached(currentStep_) == false)
		{
			magLayerItem=item;
			break;
			//driverObject_->driver().executeMagnifier(currentStep_,item);
				//item->setStepVisible(step,visible);
		}
	}

	if(!magLayerItem) 
		return 0;

	const magics::SceneLayer& snl=sceneLayerItem_->sceneLayer(); 	
	QString str;
	
	Layer *magLayer=snl.findLayer(&magLayerItem->layer(),currentStep_);

	MgQMagnifierLayoutItem *magLayoutItem=magLayerItem->magnifierLayoutItem();

	if(!magLayoutItem)
	{
		return 0;
	}		

	magLayoutItem->clearPlotContents(); 

	/*if(zoomFactor  != item->zoomFactor())
	{
		item->clearPlotContents();
	}
	else
	{ 
		return;
	}*/	

	magLayoutItem->setZoomFactor(zoomFactor);

	/*QVector<QPointF> pp;

	pp.push_back(QPointF(item->layout().minX(),item->layout().minY()));
	pp.push_back(QPointF(item->layout().maxX(),item->layout().minY()));
	pp.push_back(QPointF(item->layout().maxX(),item->layout().maxY()));
	pp.push_back(QPointF(item->layout().minX(),item->layout().maxY()));

	item->setArea(pp);*/

	float textWidth = 70.+20;
	float textHeight = 40.;
		
	magLayoutItem->setResolutionX(textWidth/zoomFactor);
	magLayoutItem->setResolutionY(textHeight/zoomFactor);

	driverObject_->driver().executeMagnifier(magLayer,magLayoutItem);

	return magLayoutItem;


	/*if(!animation_)
		return;

	item->setStep(animation_->currentStepObject());
		
	//MagLog::dev() << "GridValArea>  dx: " << fabsf(ldx) << " dy: " << fabsf(ldy) << endl;  
	for(int i=0; i <  pp.count() ; i++)
	{
		MagLog::dev() << " " << pp[i].x() << " " << pp[i].y() << endl;
	}	

	animation_->driver().redisplayMagnifier(item);*/
	
#endif	
}

void MgQSceneItem::clearMagnifier()
{
	/*MgQLayoutItem *litem=findMagnifierLayout();
	if(!litem) return;

	MgQMagnifierLayoutItem *item=static_cast<MgQMagnifierLayoutItem*>(litem);

	item->clearPlotContents();*/
}

/*void MgQSceneItem::drawItems(QPainter *painter, int numItems,
                             QGraphicsItem *items[],
                             const QStyleOptionGraphicsItem options[],
                             QWidget *widget)
 {
     for (int i = 0; i < numItems; ++i) {
          // Draw the item
          painter->save();
          painter->setMatrix(items[i]->sceneMatrix(), true);
          items[i]->paint(painter, &options[i], widget);
          painter->restore();
      }
 }
*/

void MgQSceneItem::updateCache()
{	
#if 0  
  
  	if ( !rootItem_ )
		return;
	
	
	
	QPainter *painter=cachePainter_;
	QStyleOptionGraphicsItem options;

	QRectF sourceRect = sceneRect();
        QRectF targetRect(0, 0, painter->device()->width(), painter->device()->height());

	cacheDevice_->fill(qRgba(255,255,255,255));

	painter->save();

	QTransform painterTransform;
    	painterTransform *= QTransform()
                        .translate(targetRect.left(), targetRect.top())
                        //.scale(xratio, yratio)
                        .translate(-sourceRect.left(), -sourceRect.top());
    	painter->setWorldTransform(painterTransform, true);

	rootItem_->setVisible(false);

	renderItemRecursively(rootItem_,painter,&options);

	rootItem_->setVisible(false);

	/*foreach(QGraphicsItem *item, items(Qt::AscendingOrder))
	{
		if(item->zValue() < 1.2 && item->isVisible())
		{
			painter->save();
          		painter->setTransform(item->sceneTransform(), true);
          		item->paint(painter, &options, 0);
          		painter->restore();			
		}
	}*/

	painter->restore();

	//cacheDevice_->save("/var/tmp/cgr/test.png");

	//rootItem_->setFlag(QGraphicsItem::ItemHasNoContents,true);
	rootItem_->setVisible(false); 


	//setBackgroundBrush(QPixmap::fromImage(*cacheDevice_));
#endif

}

void MgQSceneItem::drawBackground ( QPainter * painter, const QRectF & rect ) 
{	
	/*qDebug() << "bg" << rect << sceneRect();

	QRectF targetRect(0, 0, sceneRect().width(), sceneRect().height());
	//painter->drawPixmap(sceneRect(),*cacheDevice_,targetRect);

	painter->drawPixmap(rect,*cacheDevice_,rect);*/
}

void MgQSceneItem::renderForMagnifier(QPainter *painter, const QRectF &targetRect, const QRectF &sourceRect)
{
	/*QStyleOptionGraphicsItem option;
	painter->setRenderHint(QPainter::Antialiasing,antialias_);
	renderContents(painter,&option,targetRect,sourceRect,true);*/
}

void MgQSceneItem::renderForPrinter(QPainter *painter)
{
	/*QStyleOptionGraphicsItem option;
	
	QRectF sourceRect = sceneRect();
        QRectF targetRect(0, 0, painter->device()->width(), painter->device()->height());

	painter->setRenderHint(QPainter::Antialiasing,antialias_);
	renderContents(painter,&option,targetRect,sourceRect);*/
}

void MgQSceneItem::renderForVideo(QPainter *painter,QProgressDialog *progress,QString path,QStringList &files)
{	
	/*QPixmap *pix=static_cast<QPixmap*>(painter->device());
	QStyleOptionGraphicsItem option;
	
	QRectF sourceRect = sceneRect();
        QRectF targetRect(0, 0, painter->device()->width(), painter->device()->height());
		
	int oriCurrentStep=currentStep_;

	for(int i=0; i < stepNum_ && i < files.count() ; i++)
	{		
		progress->setValue(i);

		pix->fill();
		
		bool oriCached=stepCached(i);

		setStepVisible(i,true);	
		for(int j=0; j < stepNum_; j++)
		{
			if(j != i)
				setStepVisible(j,false);
		}

		renderContents(painter,&option,targetRect,sourceRect);
		
		pix->save(path + files[i]);
		
		if(oriCached == false)
		{
			foreach(MgQLayerItem *item,layerItems_)
			{
				item->clearStep(i);
			}
		}
	}
	
	currentStep_=-1;
	setCurrentStep(oriCurrentStep);*/
}

void MgQSceneItem::setEnableAntialias(bool status)
{
	/*if(antialias_!=status)
	{
		antialias_=status;
		cachePainter_->setRenderHint(QPainter::Antialiasing,antialias_);

		updateAnimation();
	}*/
}


