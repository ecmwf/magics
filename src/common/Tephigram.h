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
    \file Tephigram.h
    \brief Definition of Tephigram.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jun 12 16:01:47 2008
*/

#ifndef _Tephigram
#define _Tephigram

#include <Transformation.h>
#include <XmlNode.h>
#include <Coordinate.h>
#include <TephigramAttributes.h>

namespace magics
{

/*! \class Tephigram
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class Tephigram: public Transformation, public TephigramAttributes
{

public:
	Tephigram();
	~Tephigram();

	/*!
	  \brief sets  from an XML node
	*/
	void set(const XmlNode& node)
	{
		Transformation::set(node);
		TephigramAttributes::set(node);

	}
	/*!
	  \brief sets  from a map
	*/
	void set(const map<string, string>& map)
	{
		Transformation::set(map);
		TephigramAttributes::set(map);
	}
    
	virtual Transformation* clone() const {
		Tephigram* transformation = new Tephigram();
		return transformation;
	}
	
	/*!
	\\brief Initialise the projection
	*/
	virtual void init() ;
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

	void revert(const vector<pair<double, double> >& , vector<pair<double, double> >&) const;



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
	virtual double getMaxTestPCX() const;
	/*!
	\\brief return the ymax in projection coordinates!
	*/
	virtual double getMaxPCY() const;

	virtual Polyline& getPCBoundingBox() const;
	virtual Polyline& getUserBoundingBox() const;

	virtual void setDefinition(const string&);
	void getNewDefinition(const UserPoint&, const UserPoint&, string&) const;
	bool in(const PaperPoint& point) const;
	void operator()(const Polyline& poly,  BasicGraphicsObjectContainer& out) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

	 double minPCX_;
	 double maxPCX_;
	 double minPCY_;
	 double maxPCY_;
private:
    //! Copy constructor - No copy allowed
	Tephigram(const Tephigram&);
    //! Overloaded << operator to copy - No copy allowed
	Tephigram& operator=(const Tephigram&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Tephigram& p)
		{ p.print(s); return s; }

};

class TephiInfo: public Tephigram
{
public:
	TephiInfo();
	~TephiInfo();

	/*!
	  \brief sets  from an XML node
	*/
	void set(const XmlNode& node)
	{

		Tephigram::set(node);

	}
	/*!
	  \brief sets  from a map
	*/
	void set(const map<string, string>& map)
	{

		Tephigram::set(map);
	}

	virtual Transformation* clone() const {
		Tephigram* transformation = new Tephigram();
		return transformation;
	}

	/*!
		\\brief Initialise the projection
		*/
		virtual void init() ;
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
	void getNewDefinition(const UserPoint&, const UserPoint&, string&) const;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;
	 double reference_;

private:
    //! Copy constructor - No copy allowed
	TephiInfo(const TephiInfo&);
    //! Overloaded << operator to copy - No copy allowed
	TephiInfo& operator=(const TephiInfo&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TephiInfo& p)
		{ p.print(s); return s; }

};
    

} // namespace magics
#endif
