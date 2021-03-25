/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Visualiser.h
    \brief Definition of the Template class Visualiser.

    Magics Team - ECMWF 2004

    Started: Mon 19-Jan-2004

    Changes:

*/

#ifndef Visualiser_H
#define Visualiser_H

#include "magics.h"


#include "VisualTask.h"


namespace magics {

template <class P>
class Data;
template <class P>
class MatrixHandler;

template <class P>
class VisualComponent {
public:
    VisualComponent(){};
    virtual ~VisualComponent(){};


    virtual void preparePlot(Data<P>&, Task&) {
        MagLog::dev() << "VisualComponent::preparePlot(Data<P>&, Task&)--->Not yet implemented\n";
    }
    virtual void preparePlot(MatrixHandler<P>&, Task&) {
        MagLog::dev() << "VisualComponent::preparePlot(MatrixHandler<P>&, Task&)--->Not yet implemented\n";
    }
    virtual void preparePlot(Task&) { MagLog::dev() << "VisualComponent::preparePlot(Task&)--->Not yet implemented\n"; }
};


template <class P>
class Visualiser : public BaseSceneObject, public VisualComponent<P> {
public:
    Visualiser(){};
    virtual ~Visualiser(){};

    virtual void specialise(VisualTask<P>& task) { task.set(this); };

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "Base class Visualiser"; }


private:
    //! Copy constructor - No copy allowed
    Visualiser(const Visualiser&);
    //! Overloaded << operator to copy - No copy allowed
    Visualiser& operator=(const Visualiser&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Visualiser<P>& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
