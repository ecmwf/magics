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

/*! \file InputData.h
    \brief Definition of the Template class InputData.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 6-May-2004
    
    Changes:
    
*/

#ifndef InputData_H
#define InputData_H

#include "magics.h"
#include "MagException.h"

#include "Data.h"
#include "InputDataAttributes.h"
#include "UserPoint.h"
#include "Matrix.h"

namespace magics {


class InputData: public Data,
              public InputDataAttributes, 
              public PointsList
{
public:
	
	InputData(): matrix_(0){}
	virtual ~InputData() {}
    
    void prepare();
   
    void set(const map<string, string>& map ) { InputDataAttributes::set(map); }
	void set(const XmlNode& node ) { InputDataAttributes::set(node); }
	void visit(Transformation& transformation);


    
    MatrixHandler& matrix();
    virtual PointsHandler& points(const Transformation&, bool);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
   void getReady(const Transformation&);
   void visit(ValuesCollector&);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void  dateSetting(vector<string>&, vector<double>& , DateTime&, bool);
	 void  numberSetting(vector<double>& , vector<double>&);
	
     vector<double> x_values_;
     vector<double> y_values_;
     vector<double> x2_values_;
     vector<double> y2_values_;

     DateTime baseDateX_;
     DateTime baseDateY_;

     Matrix *matrix_;

private:
    //! Copy constructor - No copy allowed
	InputData(const InputData&);
    //! Overloaded << operator to copy - No copy allowed
	InputData& operator=(const InputData&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const InputData& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
