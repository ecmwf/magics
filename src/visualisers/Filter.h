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

/*! \file Filter.h
    \brief Definition of the Template class Filter.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 14-Jun-2004
    
    Changes:
    
*/

#ifndef Filter_H
#define Filter_H

#include "magics.h"
#include "UserPoint.h"
#include "MatrixHandler.h"
namespace magics {


class Filter: public vector<UserPoint>
{
public : 

    Filter(MatrixHandler& matrix, int nrows, int ncols);
    virtual ~Filter(); 

    //!  Get number of columns of the Mask
    int DimensionX () const { return ncols_; }

    //!  Get number of lines of the Mask
    int DimensionY () const { return nrows_; }

    //!  Start the processing of the filters
    virtual bool Process () {}

protected :

    /*!
      \brief Load the first lines in buffer.
      Return:
         true  if the operantion was successfull
         false otherwise
    */
    bool InitBuffer();

    /*!
      \brief Scrolls buffer by saving the oldest line and loading the next line
      Return:
         true  if the operantion was successfull
         false otherwise
    */
    bool ScrollBuffer();

    /*!
      \brief Allocate memory for the scroll buffer
      Return:
           true  if the operantion was successfull
           false otherwise
    */
    bool AllocImageBuffers();

    /*!
       \brief Disallocate scroll buffer memory
       Return:
           true  if the operantion was successfull
           false otherwise
    */
    void FreeImageBuffers();

//TEST, REMOVE LATER
void test_build_data();

     MatrixHandler& matrix_;
     int               nrows_;    //!< number of Mask rows
     int               ncols_;    //!< number of Mask columns
     int               flr_;      //!< current row
     double            **fbuf_;	  //!< Rotating buffer
};


class MinMaxFilter: public Filter {
public:
//	MinMaxFilter (const BaseMatrixHandler& matrix, int nrows, int ncols);
	MinMaxFilter (MatrixHandler& matrix, int nrows, int ncols, int flag=3);

	virtual ~MinMaxFilter();
    
        /*!
          Convolutes the filter over the input matrix
          Return:
             true  if the operantion was successfull
             false otherwise
        */
        virtual bool Process ();

        //! Set min/max flag
	void SetMinMaxFlag (int flag) { mflag_ = flag; }
        //! Get min/max flag
        int  GetMinMaxFlag () { return mflag_; }

//TEST, REMOVE LATER
void test_build_data();

private:
    //! Copy constructor - No copy allowed
    MinMaxFilter(const MinMaxFilter&);
    //! Overloaded << operator to copy - No copy allowed
    MinMaxFilter& operator=(const MinMaxFilter&);
    
// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const MinMaxFilter& p)
        { return s; }

    /*!
      1 : compute low
      2 : compute high
      3 : compute high & low
    */
    int mflag_;
};

} // namespace magics



#endif
