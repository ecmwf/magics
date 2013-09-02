/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TePieBar.h
    \brief This file contains a class that represents a char (pie or bar)  
*/
#ifndef TEPIEBAR_H
#define TEPIEBAR_H

#include "TeDefines.h"

//! A class that represents a chart (pie or bar)
class TL_DLL TePieBar
{
public:
	enum			TePieBarInput {ALL, TABSEL, QUERYSEL, LEGEND, NOTTABSEL, NOTQUERYSEL, NOTLEGEND};
	string			type_; // PIECHART or BARCHART
	int				themeId_;
	double			maxDiameter_;	
	double			minDiameter_;	
	double			diameter_;
	string			dimensionAttr_;
	vector<string>	attrVector_;
	vector<TeColor> colorVector_;
	double			maxHeight_;
	double			minHeight_;
	double			width_;
	double			maxValue_;
	double			minValue_;
	bool			keepDimension_;
	int				inputType_;
	TeDatabase		*db_;
	string			keyMove_;

	TePieBar() {}

	TePieBar(int themeId, TeDatabase* db)
	{
		themeId_ = themeId;
		db_ = db;
	}

	TePieBar (const TePieBar& pb )
	{
		type_ = pb.type_;
		themeId_ = pb.themeId_;
		maxDiameter_ = pb.maxDiameter_;	
		minDiameter_ = pb.minDiameter_;	
		diameter_ = pb.diameter_;
		dimensionAttr_ = pb.dimensionAttr_;
		maxHeight_ = pb.maxHeight_;
		minHeight_ = pb.minHeight_;
		width_ = pb.width_;
		maxValue_ = pb.maxValue_;
		minValue_ = pb.minValue_;
		keepDimension_ = pb.keepDimension_;
		inputType_ = pb.inputType_;
		db_ = pb.db_;
		keyMove_ = pb.keyMove_;

		attrVector_.clear();
		colorVector_.clear();
		int size = pb.attrVector_.size();
		int i = 0;
		while(i < size)
		{	attrVector_.push_back(pb.attrVector_[i]);
			colorVector_.push_back(pb.colorVector_[i]);
			i++;
		}
	}

	TePieBar& operator=(const TePieBar& pb)
	{
		type_ = pb.type_;
		themeId_ = pb.themeId_;
		maxDiameter_ = pb.maxDiameter_;	
		minDiameter_ = pb.minDiameter_;	
		diameter_ = pb.diameter_;
		dimensionAttr_ = pb.dimensionAttr_;
		maxHeight_ = pb.maxHeight_;
		minHeight_ = pb.minHeight_;
		width_ = pb.width_;
		maxValue_ = pb.maxValue_;
		minValue_ = pb.minValue_;
		keepDimension_ = pb.keepDimension_;
		inputType_ = pb.inputType_;
		db_ = pb.db_;
		keyMove_ = pb.keyMove_;

		attrVector_.clear();
		colorVector_.clear();
		int size = pb.attrVector_.size();
		int i = 0;
		while(i < size)
		{	attrVector_.push_back(pb.attrVector_[i]);
			colorVector_.push_back(pb.colorVector_[i]);
			i++;
		}
		return *this;
	}

	~TePieBar() {}

	void init(int themeId, TeDatabase* db)
	{
		themeId_ = themeId;
		db_ = db;
	}

	bool locate(TeCoord2D pt, double delta)
	{
		keyMove_.clear();
		string	TS = "AuxTheme" + Te2String(themeId_);
		string xmin = Te2String(pt.x()-delta);
		string xmax = Te2String(pt.x()+delta);
		string ymin = Te2String(pt.y()-delta);
		string ymax = Te2String(pt.y()+delta);

		TeDatabasePortal* portal = db_->getPortal();
		string sel = "SELECT SELKEY FROM " + TS + " WHERE ";
		sel += "PIEBARX > " + xmin + " AND ";
		sel += "PIEBARX < " + xmax + " AND ";
		sel += "PIEBARY > " + ymin + " AND ";
		sel += "PIEBARY < " + ymax;
		if(portal->query(sel) == false)
		{
			delete portal;
			return false;
		}

		bool b = portal->fetchRow(0);
		if(b == true)
			keyMove_ = portal->getData(0);
		delete portal;

		if(keyMove_.empty() == false)
			return true;
		return false;
	}

	void move(TeCoord2D pt)
	{
		if(keyMove_.empty() == true)
			return;

		string	x = Te2String(pt.x());
		string	y = Te2String(pt.y());
		string	TS = "AuxTheme" + Te2String(themeId_);
		string mover = "UPDATE " + TS + " SET PIEBARX = " + x + ", PIEBARY = " + y;
		mover += " WHERE SELKEY = '" + keyMove_ + "'";
		db_->execute(mover);
	}

	bool load()
	{
		TeDatabasePortal* portal = db_->getPortal();
		string sel = "SELECT * FROM piebars WHERE themeId = " + Te2String(themeId_);
		if(portal->query(sel))
		{
			bool b = portal->fetchRow(0);
			if(b == false)
			{
				delete portal;
				return false;
			}
			type_ = portal->getData("type");
			maxDiameter_ = portal->getDouble("maxDiameter");
			minDiameter_ = portal->getDouble("minDiameter");
			diameter_ = portal->getDouble("diameter"));
			string	attr = portal->getData("dimensionAttr");
			dimensionAttr_ = attr;
			maxHeight_ = portal->getDouble("maxHeight");
			minHeight_ = portal->getDouble("minHeight");
			width_ = portal->getDouble("width"));
			maxValue_ = portal->getDouble("maxValue");
			minValue_ = atof(portal->getData("minValue");
			keepDimension_ = portal->getDouble("keepDimension");
			inputType_ = portal->getDouble("inputType");
			int i;
			for(i=0; i<10; i++)
			{
				string attr = "attr" + Te2String(i+1);
				string vattr = portal->getData(attr);
				if(vattr.empty() == true)
					break;
				attrVector_.push_back(vattr);

				string red = "red" + Te2String(i+1);
				string green = "green" + Te2String(i+1);
				string blue = "blue" + Te2String(i+1);
				TeColor cor;
				cor.red_ = atoi(portal->getData(red));
				cor.green_ = atoi(portal->getData(green));
				cor.blue_ = atoi(portal->getData(blue));
				colorVector_.push_back(cor);
			}
			delete portal;
			return true;
		}
		delete portal;
		return false;
	}
	
	void save()
	{
		int	i;
		TeDatabasePortal* portal = db_->getPortal();
		bool b = portal->query("SELECT * FROM piebars");
		delete portal;
		if(b == false)
		{
			string criar = "CREATE TABLE piebars (themeId INTEGER,";
			criar += "type VARCHAR(16) DEFAULT NULL,";
			criar += "maxDiameter DOUBLE DEFAULT NULL,";
			criar += "minDiameter DOUBLE DEFAULT NULL,";
			criar += "diameter DOUBLE DEFAULT NULL,";
			criar += "dimensionAttr  VARCHAR(64) DEFAULT NULL,";
			criar += "maxHeight DOUBLE DEFAULT NULL,";
			criar += "minHeight DOUBLE DEFAULT NULL,";
			criar += "width DOUBLE DEFAULT NULL,";
			criar += "maxValue DOUBLE DEFAULT NULL,";
			criar += "minValue DOUBLE DEFAULT NULL,";
			criar += "keepDimension INTEGER DEFAULT 0,";
			criar += "inputType INTEGER DEFAULT 0,";
			for (i=0; i<10; i++)
			{
				criar += "attr" + Te2String(i+1) + " VARCHAR(64) DEFAULT NULL,";
				criar += "red" + Te2String(i+1) + " INTEGER DEFAULT NULL,";
				criar += "green" + Te2String(i+1) + " INTEGER DEFAULT NULL,";
				criar += "blue" + Te2String(i+1) + " INTEGER DEFAULT NULL,";
			}
			criar.erase(criar.size()-1, 1);
			criar += ")";
			db_->execute(criar);
		}

		string del = "DELETE FROM piebars WHERE themeId = " + Te2String(themeId_);
		db_->execute(del);

		if(attrVector_.size() == 0)
			return;
		string up = "INSERT INTO piebars (themeId, type, maxDiameter, ";
		up += "minDiameter, diameter, dimensionAttr, maxHeight, minHeight, ";
		up += "width, maxValue, minValue, keepDimension, inputType, ";
		for (i=0; i<attrVector_.size() && i<10; i++)
		{
			up += "attr" + Te2String(i+1) + ", ";
			up += "red" + Te2String(i+1) + ", ";
			up += "green" + Te2String(i+1) + ", ";
			up += "blue" + Te2String(i+1) + ", ";
		}
		up.erase(up.size()-2, 2);
		up += ")";

		up += " VALUES (" + Te2String(themeId_) + ", ";
		up += "'" + type_ + "', ";
		up += Te2String(maxDiameter_) + ", ";
		up += Te2String(minDiameter_) + ", ";
		up += Te2String(diameter_) + ", ";
		up += "'" + dimensionAttr_ + "', ";
		up += Te2String(maxHeight_) + ", ";
		up += Te2String(minHeight_) + ", ";
		up += Te2String(width_) + ", ";
		up += Te2String(maxValue_) + ", ";
		up += Te2String(minValue_) + ", ";
		up += Te2String(keepDimension_) + ", ";
		up += Te2String(inputType_) + ", ";
		for (i=0; i<attrVector_.size() && i<10; i++)
		{
			up += "'" + attrVector_[i] + "', ";
			up += Te2String(colorVector_[i].red_) + ", ";
			up += Te2String(colorVector_[i].green_) + ", ";
			up += Te2String(colorVector_[i].blue_) + ", ";
		}
		up.erase(up.size()-2, 2);
		up += ")";
		db_->execute(up);
	}
	
	void remove()
	{
		TeDatabasePortal* portal = db_->getPortal();
		bool b = portal->query("SELECT * FROM piebars");
		delete portal;
		if(b == false)
			return;

		string del = "DELETE FROM piebars WHERE themeId = " + Te2String(themeId_);
		db_->execute(del);
	}
};

#endif





