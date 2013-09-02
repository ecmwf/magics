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
    \file MgQStepMetaData.cc
    \brief Definition of MgQStepMetaData.
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#include "MgQStepMetaData.h"

#include <QDebug>


MgQStepMetaData::MgQStepMetaData(QStringList keys) : keys_(keys)
{
	for(int i=0; i < keys_.count(); i++)
	{
		stepData_[i]=QStringList();
	}
}

void MgQStepMetaData::clear()
{
	keys_.clear();
	stepData_.clear();
}

int MgQStepMetaData::stepNum()
{
	if(keys_.count() > 0)
		return stepData_[0].count();
	else
		return 0;
}

QStringList MgQStepMetaData::stepData(int index)
{
	return stepData_[index];
}

void MgQStepMetaData::addStep(QString value)
{
	foreach(int index,stepData_.keys())
	{
		stepData_[index].append(value);
	}
}

void MgQStepMetaData::appendToStepData(QString key, int step, QString value)
{	
	int index=keys_.indexOf(key,0);
	while(index != -1)
	{
		QStringList& lst=stepData_[index];
		//qDebug() << key << index << step << lst;
		lst[step].append(value);

		index=keys_.indexOf(key,index+1);
	}
} 