/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AutomaticContourMethod.h
    \brief Definition of the Template class AutomaticContourMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 4-Oct-2004
    
    Changes:
    
*/

#ifndef AutomaticContourMethod_H
#define AutomaticContourMethod_H

#include "magics.h"

#include "ContourMethod.h"
#include "Akima760Method.h"
#include "MatrixHandler.h"
#include "BasicSceneObject.h"



namespace magics {





class AutomaticContourMethod: public ContourMethod {

public:
	         AutomaticContourMethod() {}
	virtual ~AutomaticContourMethod() {}
	ContourMethod* clone() { return new AutomaticContourMethod(); }
	virtual bool accept(const string& node) { return magCompare(node, "automatic"); }

    virtual MatrixHandler* handler(const AbstractMatrix& matrix, const BasicGraphicsObjectContainer& owner)
    {
        // this is the ideal number of points per cm on the paper
        const double fDesiredPointsPerCm = 5.0;  

        // do not allow the resolution to go beyond this
        const double fMinSensibleContourResolution = 0.01;   

        // if the computed resolution is within this much of the native resolution,
        // then just use the native resolution
        const double fAutoContourRoundupProportion = 0.05;  

        // we use this to reduce the aggressiveness of the subsampling calculation;
        // the higher the number, the less subsampling will be done.
        const double fSampleAdjustment = 1.7;

        MatrixHandler data(matrix);
        MatrixHandler* pMatrixHandler;
        if ( matrix.akimaEnable() == false ) {
                    if  ( matrix.delegate() ) {
                        return new DelegateMatrixHandler(matrix);
                    }
        	        ContourMethod * pContourMethod =new  ContourMethod();

                    pMatrixHandler = pContourMethod->handler(matrix, owner);

                    MagLog::debug() << "Linear contouring, "    << "\n";
                    return pMatrixHandler;

                   
        }

        double fGeoAreaWidth;
        double fGeoAreaHeight;
        double fPaperAreaWidth;
        double fPaperAreaHeight;
        double fContourResolutionX;
        double fContourResolutionY;
        double fDataResolutionX;
        double fDataResolutionY;
        double fMinX, fMaxX;
        double fMinY, fMaxY;
        int    nSampleX = 1, nSampleY = 1;
        

        // find the dimensions of the paper on which we will plot
        fPaperAreaWidth  = owner.absoluteWidth();
        fPaperAreaHeight = owner.absoluteHeight();

        // retrieve the data's resolution
        fDataResolutionX = fabs (data.XResolution());
        fDataResolutionY = fabs (data.YResolution());

        // retrieve the geographical area being used
        fMinX = data.minX();
        fMaxX = data.maxX();
        fMinY = data.minY();
        fMaxY = data.maxY();


        // if the points given are extreme values, then it means there are no
        // grid points - in this case, for the purposes of the algorithm, we will
        // pretend that there are 4 points
        
        if (fMinX == INT_MAX)    // yes, INT_MAX!
        {
            fMinX = 0.0;
            fMaxX = fMinX + fDataResolutionX;
            fMinY = 0.0;
            fMaxY = fMinY + fDataResolutionY;
        }
        else
        {
            // if only one grid point is in the area, then we may get min and max the same,
            // which causes problems because their difference is then 0 and it forces the
            // maximum possible Akima interpolation.

            if (fMinX == fMaxX) fMaxX = fMinX + fDataResolutionX;  // pretend that we have 2 points!
            if (fMinY == fMaxY) fMaxY = fMinY + fDataResolutionY;  // pretend that we have 2 points!
        }


        fGeoAreaWidth  = fMaxX - fMinX;
        fGeoAreaHeight = fMaxY - fMinY;

        // calculate the resolutions we need in order to fulfil our 
        // 'desired points per cm' criteria
        fContourResolutionX = fGeoAreaWidth  / (fDesiredPointsPerCm * fPaperAreaWidth);
        fContourResolutionY = fGeoAreaHeight / (fDesiredPointsPerCm * fPaperAreaHeight);

        // clip it to sensible limits so that we don't go overboard on the Akima interpolation
        // XXX we may need to revise this, given that we're not always in geo projection

        if (fContourResolutionX < fMinSensibleContourResolution)
        {
            fContourResolutionX = fMinSensibleContourResolution;
        }

        // otherwise, if we don't need every data point, then we maybe need to subsample.
        // for example, if our desired contouring resolution (fContourResolutionX) is to
        // have one point every 1.0 degree, but our data (fDataResolutionX) is one point
        // every 0.5 degrees, then we only need to take every second point.
        // But on top of that, we want to be a little bit cautious about removing data points,
        // so we make an adjustment to the computation so that we don't subsample too aggressively.

        else if (fContourResolutionX > fDataResolutionX - (fDataResolutionX * fAutoContourRoundupProportion))
        {
            nSampleX = static_cast<int>(fContourResolutionX / (fDataResolutionX * fSampleAdjustment));
            if (nSampleX < 1) nSampleX = 1;
            fContourResolutionX = fDataResolutionX; // only needed if we end up with linear contouring
        }



        // do all the same again for the Y direction

        if (fContourResolutionY < fMinSensibleContourResolution)
        {
            fContourResolutionY = fMinSensibleContourResolution;
        }

        else if (fContourResolutionY > fDataResolutionY - (fDataResolutionY * fAutoContourRoundupProportion))
        {
            nSampleY = static_cast<int>(fContourResolutionY / (fDataResolutionY * fSampleAdjustment));
            if (nSampleY < 1) nSampleY = 1;
            fContourResolutionY = fDataResolutionY; // only needed if we end up with linear contouring
        }



        // compute some values for debug output, including some safety checking
        
        int nNumColumns = matrix.columns();
        int nNumRows    = matrix.rows();
        double fCol0    = (nNumColumns == 0) ? 0.0 : matrix.column (0,0);
        double fColN    = (nNumColumns == 0) ? 0.0 : matrix.column (0,matrix.columns() - 1);
        double fRow0    = (nNumRows    == 0) ? 0.0 : matrix.row (0,0);
        double fRowN    = (nNumRows    == 0) ? 0.0 : matrix.row (matrix.rows() - 1,0);

        MagLog::debug() << "\n*************************************************************\n"
                     << "Automatic contour method\n"
                     << "Points per cm (desired): " << fDesiredPointsPerCm << "\n"
                     << "Your data X: " << fMaxX << " to "  << fMinX << "\n"
                     << "Your data Y: " << fMaxY << " to "  << fMinY << "\n"
                     << "Resolution:[ " << fDataResolutionX << ", "  << fDataResolutionY << "]\n"
                     << "Cols: "  << nNumColumns  << "  Rows: " << nNumRows << "\n"
                     << "Col 0: " << fCol0 << " Col n: " << fColN << "\n"
                     << "Row 0: " << fRow0 << " Row n: " << fRowN << "\n"
                     << "Paper dimension: [" << fPaperAreaWidth << ", " << fPaperAreaHeight << "]\n";



       

        // Check for the linear case (contour resolution == data resolution).
        // Also need to use linear contouring if we have missing values, because Akima will
        // incorrectly interpolate them. Missing data is also handled correctly if we have high-res
        // data, because the sub-sampling method (above) uses no interpolation.
        if ((nSampleX > 1) && (nSampleY > 1))   {
            ContourMethod* cm = MagTranslator<string, ContourMethod >()("linear");
            auto_ptr<ContourMethod > pContourMethod(cm);

            pMatrixHandler = pContourMethod->handler(matrix, owner);

            MagLog::debug() << "Linear contouring, Res: " << fContourResolutionX << "x" << fContourResolutionY    << "\n";
        }
        else if (((fContourResolutionX == fDataResolutionX) && (fContourResolutionY == fDataResolutionY)) ||
                 data.hasMissingValues())
        {
            ContourMethod* cm = MagTranslator<string, ContourMethod >()("linear");
            auto_ptr<ContourMethod > pContourMethod(cm);

            pMatrixHandler = pContourMethod->handler(matrix, owner);

            MagLog::debug() << "Linear contouring, Res: " << fContourResolutionX << "x" << fContourResolutionY    << "\n";
        }

        // otherwise we use the Akima760 method
        else
        {
            Akima760Method *am = static_cast < Akima760Method *> (MagTranslator<string, ContourMethod >()("akima760"));
			auto_ptr<Akima760Method > pAkima760Method(am);

            pAkima760Method->resolutionX_ = (fContourResolutionX);
            pAkima760Method->resolutionY_ =  (fContourResolutionY);

            pMatrixHandler = pAkima760Method->handler(matrix, owner);

            MagLog::debug() << "Akima 760, Res: " << fContourResolutionX << "x" << fContourResolutionY << "\n";
        }

        MagLog::debug()  << "\n*************************************************************\n";

        return pMatrixHandler;
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out <<  "AutomaticContourMethod" << "\n"; }

private:
    //! Copy constructor - No copy allowed
	AutomaticContourMethod(const AutomaticContourMethod&);
    //! Overloaded << operator to copy - No copy allowed
	AutomaticContourMethod& operator=(const AutomaticContourMethod&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AutomaticContourMethod& p)
		{ p.print(s); return s; }
};


} // namespace magics

#endif
