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
/*! \file TeLegendEntry.h
    \brief This file deals with legends in TerraLib
*/
#ifndef  __TERRALIB_INTERNAL_LEGENDENTRY_H
#define  __TERRALIB_INTERNAL_LEGENDENTRY_H

#include <string>
#include <vector>
#include <map>

using namespace std;

#include "TeUtils.h"
#include "TeDataTypes.h"
#include "TeSlice.h"
#include "TeVisual.h"

class TeTheme;
struct TeColor;

//! A map from representation types to a presentation visual characteristics
typedef map<TeGeomRep,TeVisual*> TeGeomRepVisualMap;


//! A Legend Entry represents a group of objects of a theme
/*!
	A legend entry is associated to a  group of a theme. It has a label and a TeSlice
	that defines a property of of the objects that fall into this legend.
	A legend entry has an associated label and the visual presentation characteristics 
	of the objects that fall into this legend.
*/
class TL_DLL TeLegendEntry
{
public:

	//! Empty constructor
	TeLegendEntry () : 
		label_(""),
		id_(-1),
		theme_(0),
		group_(0)
		{}

	//! Constructor from a slice
	TeLegendEntry (const TeSlice& slice);
	
	//! Copy constructor
	TeLegendEntry (const TeLegendEntry& leg);

	//! Operator =
	TeLegendEntry& operator = (const TeLegendEntry& leg);

	//! Destructor
	virtual ~TeLegendEntry()
	{	clear(); }

	//! Clear the legend
	void clear();

	//! Returns the legend id
	int	id () 
	{ return id_; }
	//! Sets the legend id
	void id (int i) { id_ = i; }

	//! Returns the theme identifier to which the legend belongs
	int	theme () { return theme_; }
	//! Sets the theme id to which this legend belongs to
	void theme (int i) { theme_ = i; }

	//! Returns the group identifier associated to the legend
	int	group () { return group_; }
	//! Sets the group identification to which this legend refers to
	void group (int i) { group_ = i; }

	//! Returns the visual presentation characteristics associated to a given representation
	TeVisual* visual (TeGeomRep rep, const string& visualType="tevisual");

	//! Sets the visual presentation characteristics associated to a given representation
	void setVisual (TeVisual* vis, TeGeomRep rep); 

	//! Sets the visual presentation characteristics associated to a given representation
	void setVisual (TeVisual& vis, TeGeomRep rep); 
	
	//! Return the visual presentation characteristics map
	TeGeomRepVisualMap& getVisualMap()
	{	return visualMap_; }

	//! Returns the slice associated to this legend
	TeSlice& slice()
	{  return slice_; }
	// Sets the slice associated to this legend
	void setSlice(const TeSlice& sl)
	{	slice_ = sl; }
	//! Returns the number of objects of the legend
	int count() 
	{ return slice_.count_; }
	//! Sets the number of objects with the legend
	void count(int n) 
	{ slice_.count_ = n; }
	//! Increase the number of objects with the legend
	void incCount(int n=1)
	{	slice_.count_ += n; }
	
	//! Returns the lower value associated to the legend
	string from()
	{	return slice_.from_; }
	//! Sets the lower value associated to the legend
	void from(string& s)
	{ slice_.from_ = s; }

	//! Returns the upper value associated to the legend
	string to()
	{	return slice_.to_; }
	//! Sets the upper value associated to the legend
	void to(string& s)
	{	slice_.to_ = s; }

	//! Returns the label associated to the legend 
	string label();
	
	//! Sets the label associated to the legend
	void label(string& s)
	{	label_ = s; }

	//! Sets the color associated to the legend
	void color(TeColor& color);

private:

	TeGeomRepVisualMap	visualMap_;
	TeSlice				slice_;
	string				label_;
	int					id_;
	int					theme_;
	int					group_;
};

//! A vector of legendy entries
typedef vector<TeLegendEntry> TeLegendEntryVector;

//! A map of legend identifiers to legend entrie pointers
typedef map<int, TeLegendEntry*> TeLegendEntryMap;

#endif

