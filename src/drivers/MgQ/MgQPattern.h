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
    \file MgQPattern.h
    \brief Definition of MgQPattern.
    \author Graphics Section, ECMWF

    Started: Feb 2011
*/

#ifndef _MgQPattern_H
#define _MgQPattern_H

#include <QList>
#include <QPixmap>
#include <QString>


class MgQPatternProperties
{
public:
	enum Type {DotShading,HatchShading,Symbol};

	MgQPatternProperties(Type);
	bool operator==(const MgQPatternProperties&) const;

	Type type_;
	QString id_;
	QSize size_;
	QSize itemSize_;
	float lineWidth_;
	QColor colour_;
	QColor bg_;	
};

class MgQPattern : public QPixmap
{
public:
	MgQPattern(const MgQPatternProperties&);
	//bool equal(MgQPatternProperties);
	const QPixmap& pixmap() {return *this;}
	const MgQPatternProperties& properties() {return prop_;}

protected:
	MgQPatternProperties prop_;
};

class MgQPatternManager
{
public:
	MgQPatternManager() {};
	~MgQPatternManager();
 
	MgQPattern* addPattern(MgQPatternProperties&);
	MgQPattern* getPattern(MgQPatternProperties&);	
	void deletePattern(MgQPatternProperties&);		

	//int    symbolNum() {return symbols_.count();};	

protected:
	QList<MgQPattern*> patterns_;
};	


#endif 
