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

/*! \file GribRegularInterpretor.h
    \brief Definition of the Template class GribRegularInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 18-Apr-2005
    
    Changes:
    
*/

#ifndef GribRegularInterpretor_H
#define GribRegularInterpretor_H

#include "magics.h"

#include "GribInterpretor.h"



namespace magics {

class GribRegularInterpretor: public GribInterpretor {

public:
	GribRegularInterpretor();
	virtual ~GribRegularInterpretor();
	void interpretAsMatrix(const GribDecoder&, Matrix**, grib_handle*) const;
	
	virtual double XResolution(const GribDecoder& grib) const { return  longitudeIncrement(grib); }
	void interpretAsMatrix(const GribDecoder&, Matrix**, const Transformation&) const; 
	virtual void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const;
    virtual void latitudes(const GribDecoder&, vector<double>&) const;
    virtual double longitudeIncrement(const GribDecoder&) const;
    void  index(const GribDecoder& grib);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 


private:
    //! Copy constructor - No copy allowed
	GribRegularInterpretor(const GribRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	GribRegularInterpretor& operator=(const GribRegularInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GribRegularInterpretor& p)
		{ p.print(s); return s; }

};

class GribReducedGaussianInterpretor: public GribInterpretor {

public:
	GribReducedGaussianInterpretor() {}
	virtual ~GribReducedGaussianInterpretor() {}
	
	virtual double XResolution(const GribDecoder& grib) const;
	void interpretAsMatrix(const GribDecoder&, Matrix**,  grib_handle*) const;
	void interpretAsMatrix(const GribDecoder&, Matrix**, const Transformation&) const; 
	void interpretAsRaster(const GribDecoder&, RasterData&, const Transformation&) const;
	void  index(const GribDecoder& grib);
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	GribReducedGaussianInterpretor(const GribRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	GribReducedGaussianInterpretor& operator=(const GribRegularInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GribReducedGaussianInterpretor& p)
		{ p.print(s); return s; }

};

class GribReducedLatLonInterpretor: public GribInterpretor {

public:
	GribReducedLatLonInterpretor() {}
	virtual ~GribReducedLatLonInterpretor() {}

	void interpretAsMatrix(const GribDecoder&, Matrix**,  grib_handle*) const;
	virtual double XResolution(const GribDecoder& grib) const;
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	GribReducedLatLonInterpretor(const GribRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	GribReducedLatLonInterpretor& operator=(const GribRegularInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GribReducedLatLonInterpretor& p)
		{ p.print(s); return s; }

};

class GribRotatedInterpretor: public GribRegularInterpretor {

public:
	GribRotatedInterpretor(): original_(false) {}
	virtual ~GribRotatedInterpretor() {}
	
	std::pair<double, double> unrotate(double lat, double lon) const;
	std::pair<double, double> rotate(double lat, double lon) const;
	PaperPoint reference(const GribDecoder&, const Transformation&);
	void raw(const GribDecoder&, const Transformation&, const string&, map<double, map<double, CustomisedPoint*> >& ) const;
	void keepOriginal(bool original) { original_ = original; }
	void interpretAsMatrix(const GribDecoder&, Matrix**,  grib_handle*) const;
	void interpret2D(double&, double&, double&, double&) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 mutable double southPoleLat_;
	 mutable double southPoleLon_;
	 mutable double angle_;
	 mutable bool uvRelativeToGrid_;

	 bool original_;
};

class GribLambertAzimutalInterpretor: public GribRegularInterpretor {

public:
	GribLambertAzimutalInterpretor() {}
	virtual ~GribLambertAzimutalInterpretor() {}
	
	UserPoint unrotate(double lat, double lon) const;
	void interpretAsMatrix(const GribDecoder&, Matrix**, grib_handle*) const;
	PaperPoint reference(const GribDecoder&, const Transformation&);
	double XResolution(const GribDecoder& grib) const;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 mutable double standardParallel_;
	 mutable double centralLongitude_;

   
};

class GribLambertInterpretor: public GribLambertAzimutalInterpretor {

public:
	GribLambertInterpretor() {}
	virtual ~GribLambertInterpretor() {}


	void interpretAsMatrix(const GribDecoder&, Matrix**, grib_handle*) const;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;



};


class GribPolarStereoInterpretor: public GribRegularInterpretor {

public:
	GribPolarStereoInterpretor() {}
	virtual ~GribPolarStereoInterpretor() {}


	void interpretAsMatrix(const GribDecoder&, Matrix**,  grib_handle*) const;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;



};


class GribRegularGaussianInterpretor: public GribRegularInterpretor 
{

public:
	GribRegularGaussianInterpretor() {}
	virtual ~GribRegularGaussianInterpretor() {}
	
	void latitudes(const GribDecoder&, vector<double>&) const;

protected:
     

};


} // namespace magics
#endif
