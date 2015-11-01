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

/*! \file NetcdfGeopointsInterpretor.h
    \brief Definition of the Template class NetcdfGeopointsInterpretor.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#ifndef NetcdfGeopointsInterpretor_H
#define NetcdfGeopointsInterpretor_H

#include "magics.h"

#include "NetcdfGeopointsInterpretorAttributes.h"
#include "NetcdfXYpointsInterpretorAttributes.h"
#include "NetcdfInterpretor.h"
#include "Matrix.h"
#include "XmlNode.h"
namespace magics {

class NetcdfGeopointsInterpretor: public NetcdfInterpretor, public NetcdfGeopointsInterpretorAttributes {

public:
    NetcdfGeopointsInterpretor();
    virtual ~NetcdfGeopointsInterpretor();

    void set(const map<string, string>& params) {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params);
        NetcdfGeopointsInterpretorAttributes::set(params);
    }
    void set(const XmlNode& node) {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)" << "\n";
        XmlNode netcdf = node;
        NetcdfGeopointsInterpretorAttributes::set(node);
        netcdf.name("netcdf");
        NetcdfInterpretorAttributes::set(netcdf);

    }
    virtual NetcdfInterpretor* clone() const {
        NetcdfGeopointsInterpretor* object = new NetcdfGeopointsInterpretor();
        object->clone(*this);
        return object;
    }
    void clone(const NetcdfGeopointsInterpretor& other) {
        NetcdfInterpretorAttributes::copy(other);
        NetcdfGeopointsInterpretorAttributes::copy(other);
    }
    bool interpretAsPoints(PointsList&);
    bool interpretAsPoints(PointsList&, const Transformation&);
    bool interpretAsMatrix(Matrix**) { throw MagicsException("Not Yet"); }
    virtual void visit(MetaDataCollector&);
    virtual void visit(ValuesCollector&,PointsList&);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
     virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    NetcdfGeopointsInterpretor(const NetcdfGeopointsInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfGeopointsInterpretor& operator=(const NetcdfGeopointsInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const NetcdfGeopointsInterpretor& p)
        { p.print(s); return s; }

};
class NetcdfXYpointsInterpretor: public NetcdfInterpretor, public NetcdfXYpointsInterpretorAttributes {

public:
    NetcdfXYpointsInterpretor();
    virtual ~NetcdfXYpointsInterpretor();

    void set(const map<string, string>& params) {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params);
        NetcdfXYpointsInterpretorAttributes::set(params);
    }
    void set(const XmlNode& node) {
        MagLog::debug() << "NetcdfGeopointsInterpretor::set(params)" << "\n";
        XmlNode netcdf = node;
        NetcdfXYpointsInterpretorAttributes::set(node);
        netcdf.name("netcdf");
        NetcdfInterpretorAttributes::set(netcdf);

    }
    virtual NetcdfInterpretor* clone() const {
        NetcdfXYpointsInterpretor* object = new NetcdfXYpointsInterpretor();
        object->clone(*this);
        return object;
    }
    void clone(const NetcdfXYpointsInterpretor& other) {
        NetcdfInterpretorAttributes::copy(other);
        NetcdfXYpointsInterpretorAttributes::copy(other);
    }
    bool interpretAsPoints(PointsList&, const std::set<string>&);
    bool interpretAsPoints(PointsList&);
    bool interpretAsPoints(PointsList&, const Transformation&);
    bool interpretAsMatrix(Matrix**) { throw MagicsException("Not Yet"); }
    virtual void visit(Transformation&);
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int s);
    virtual void visit(MetaDataCollector&);
    virtual void visit(ValuesCollector&,PointsList&);
    void visit(TextVisitor&);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
     virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    NetcdfXYpointsInterpretor(const NetcdfXYpointsInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    NetcdfXYpointsInterpretor& operator=(const NetcdfXYpointsInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const NetcdfXYpointsInterpretor& p)
        { p.print(s); return s; }

};

} // namespace magics
#endif
