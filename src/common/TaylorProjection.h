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
    \file TaylorProjection.h
    \brief Definition of TaylorProjection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jun 12 16:01:47 2008
*/

#ifndef _TaylorProjection_H
#define _TaylorProjection_H

#include <Transformation.h>
#include <TaylorProjectionAttributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \class TaylorProjection
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class TaylorProjection: public Transformation, public TaylorProjectionAttributes
{

public:
	TaylorProjection();
	~TaylorProjection();

	/*!
	  \brief sets  from an XML node
	*/
	void set(const XmlNode& node)
	{
		Transformation::set(node);
		TaylorProjectionAttributes::set(node);
	}
	/*!
	  \brief sets  from a map
	*/
	void set(const map<string, string>& map)
	{
		Transformation::set(map);
		TaylorProjectionAttributes::set(map);
	}
    
	virtual Transformation* clone() const {
		TaylorProjection* transformation = new TaylorProjection();
		transformation->copy(*this);
		return transformation;
	}
	
	/*!
	\\brief Initialise the projection
	*/
	virtual void init() ;
    virtual void cleanPCEnveloppe();
	/*!
	\\brief 
	*/
	virtual PaperPoint operator()(const UserPoint&) const;
	/*!
	\\brief 
	*/

	virtual PaperPoint operator()(const PaperPoint&) const;
	/*!
	\\brief 
	*/
	virtual void revert(const PaperPoint&, UserPoint&) const;


	/*!
	\\brief Does the projection needs the coastalines to be shifted!
	*/
	virtual bool needShiftedCoastlines() const;
	/*!
	\\brief set the aspect ratio!
	*/
	virtual void aspectRatio(double&, double&) ;
	/*!
	\\brief set the bounding box!
	*/
	virtual void boundingBox(double&, double&, double&, double&) const;

	/*!
	\\brief return the xmin in user coordinates!
	*/
	virtual double getMinX() const;
	/*!
	\\brief return the ymin in user coordinates!
	*/
	virtual double getMinY() const;
	/*!
	\\brief return the xmax in user coordinates!
	*/
	virtual double getMaxX() const;
	/*!
	\\brief return the ymax in user coordinates!
	*/
	virtual double getMaxY() const;
	/*!
	\\brief set the xmin in user coordinates!
	*/
	virtual void setMinX(double) ;
	/*!
	\\brief return the ymin in user coordinates!
	*/
	virtual void setMinY(double) ;
	/*!
	\\brief return the xmax in user coordinates!
	*/
	virtual void setMaxX(double) ;
	/*!
	\\brief return the ymax in user coordinates!
	*/
	virtual void setMaxY(double) ;
	/*!
	\\brief return the xmin in projection coordinates!
	*/
	virtual double getMinPCX() const;
	/*!
	\\brief return the ymin in projection coordinates!
	*/
	virtual double getMinPCY() const;
	/*!
	\\brief return the xmax in projection coordinates!
	*/
	virtual double getMaxPCX() const;
	/*!
	\\brief return the ymax in projection coordinates!
	*/
	virtual double getMaxPCY() const;

	virtual Polyline& getPCBoundingBox() const;
	virtual Polyline& getUserBoundingBox() const;

	virtual void setDefinition(const string&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	TaylorProjection(const TaylorProjection&);
    //! Overloaded << operator to copy - No copy allowed
	TaylorProjection& operator=(const TaylorProjection&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TaylorProjection& p)
		{ p.print(s); return s; }

};
    

} // namespace magics
#endif
