/*! \file MagClipper.h
    \brief Definition of the Template class MagClipper.

    Magics Team - ECMWF 2018

    Started: Mon 13-Aug-2018

    Changes:

*/

#ifndef MagClipper_H
#define MagClipper_H

#include "clipper.hpp"
#include "magics.h"

namespace magics {

class Polyline;
class PaperPoint;

class MagClipper {
public:
    MagClipper();
    ~MagClipper();

    static void clipOpened(const Polyline& subject, const Polyline& clip, vector<Polyline*>& result);
    static void clipClosed(const Polyline& subject, const Polyline& clip, vector<Polyline*>& result);
    static void clip(const Polyline& subject, const Polyline& clip, vector<Polyline*>& result);
    static void clip(const Polyline& subject, const PaperPoint& lowerleft, const PaperPoint& upperright,
                     vector<Polyline*>& result);
    static void add(const Polyline& subject, const Polyline& clip, vector<Polyline*>& result);
    static bool in(const Polyline& poly, const PaperPoint& point);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    MagClipper(const MagClipper&);
    //! Overloaded << operator to copy - No copy allowed
    MagClipper& operator=(const MagClipper&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MagClipper& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
