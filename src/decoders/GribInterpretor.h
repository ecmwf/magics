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

/*! \file GribInterpretor.h
    \brief Definition of the Template class GribInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 18-Apr-2005
    
    Changes:
    
*/

#ifndef GribInterpretor_H
#define GribInterpretor_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "UserPoint.h"


namespace magics {

class GribDecoder;
class Matrix;
class Transformation;


class RasterData;


class GribInterpretor  {

public:
	GribInterpretor() {}
	virtual ~GribInterpretor() {}
	virtual double XResolution(const GribDecoder&) const { return 0; }
	virtual void raw(const GribDecoder&, const Transformation&, const string&, map<double, map<double, CustomisedPoint*> >&) const;
	virtual void raw(GribDecoder&, const Transformation&, vector<pair<double, vector<pair<double, CustomisedPoint*> > > >&, double&, double&) const;

	virtual void interpretAsMatrix(const GribDecoder&, Matrix** matrix) const { *matrix = 0; }
	virtual void interpretAsMatrix(const GribDecoder& grib, Matrix** matrix, const Transformation&) const
		{ interpretAsMatrix(grib, matrix); }


    virtual void interpret2D(double&, double&, double&, double&) const {}
    virtual void keepOriginal(bool) {}
    virtual PaperPoint reference(const GribDecoder&, const Transformation&);
    virtual void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const {}
    virtual void scaling(const GribDecoder&, Matrix**) const;
    virtual void scaling(const GribDecoder& grib, double& scaling, double& offset) const;
    virtual void scaling(const GribDecoder& grib, double& scaling, double& offset, string& originalUnits, string& derivedUnits) const;
    void longitudesSanityCheck(double&, double&) const;

    void interpolate(const GribDecoder& grib, Matrix& matrix) const;

    virtual void  index(const GribDecoder& grib);
    virtual int nearest(double, double, double&, double&);
    double west_;
    double east_;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out <<  "GribInterpretor" << endl; }
	 map<double, map<double, int> > index_;


private:

    //! Copy constructor - No copy allowed
	GribInterpretor(const GribInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	GribInterpretor& operator=(const GribInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GribInterpretor& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, GribInterpretor> { 
public:
	MAGICS_NO_EXPORT GribInterpretor* operator()(const string& val )
	{
		 return SimpleObjectMaker<GribInterpretor>::create(val);
	}     
	MAGICS_NO_EXPORT GribInterpretor* magics(const string& param)
	{
		GribInterpretor* object;
		ParameterManager::update(param, object);
		return object;
	}
};


} // namespace magics
#endif
