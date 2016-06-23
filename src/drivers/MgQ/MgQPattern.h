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
