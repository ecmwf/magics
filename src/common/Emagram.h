/*
 * (C) Copyright 1996-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef EMAGRAM_H
#define EMAGRAM_H

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

class Emagram: public Transformation, public TephigramAttributes
{

public:
    Emagram();
    ~Emagram();

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
        Emagram* transformation = new Emagram();
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
    virtual bool getAutomaticX() const { return x_automatic_; }
    virtual bool getAutomaticY() const { return y_automatic_; }
    virtual void setMinMaxX(double, double);
    virtual void setMinMaxY(double, double);
    virtual PaperPoint operator()(const PaperPoint&) const;
    /*!
    \\brief
    */
    virtual void revert(const PaperPoint&, UserPoint&) const;

    void revert(const vector< std::pair<double, double> >& , vector< std::pair<double, double> >&) const;



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
    Emagram(const Emagram&);
    //! Overloaded << operator to copy - No copy allowed
    Emagram& operator=(const Emagram&);

// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const Emagram& p)
        { p.print(s); return s; }

};

class EmagramInfo: public Emagram
{
public:
    EmagramInfo();
    ~EmagramInfo();

    /*!
      \brief sets  from an XML node
    */
    void set(const XmlNode& node)
    {

        Emagram::set(node);

    }
    /*!
      \brief sets  from a map
    */
    void set(const map<string, string>& map)
    {

        Emagram::set(map);
    }

    virtual Transformation* clone() const {
        Emagram* transformation = new Emagram();
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
    EmagramInfo(const EmagramInfo&);
    //! Overloaded << operator to copy - No copy allowed
    EmagramInfo& operator=(const EmagramInfo&);

// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const EmagramInfo& p)
        { p.print(s); return s; }

};


} // namespace magics

#endif // EMAGRAM_H
