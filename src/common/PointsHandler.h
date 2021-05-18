/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PointsHandler.h
    \brief Definition of the Template class PointsHandler.

    Magics Team - ECMWF 2004

    Started: Tue 20-Jan-2004

    Changes:

*/

#ifndef PointsHandler_H
#define PointsHandler_H

#include "magics.h"

#include "BasePointsHandler.h"
#include "Transformation.h"

namespace magics {


class PointsHandler : public AbstractPoints {
public:
    PointsHandler(AbstractPoints& handler) : handler_(handler) {}
    virtual ~PointsHandler() override {}
    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst() override { handler_.setToFirst(); }
    //! Method to test the end of collection.
    virtual bool more() override { return handler_.more(); }
    //! Method to return the current value
    virtual const UserPoint& current() override { return handler_.current(); }
    //! Method to advance
    virtual void advance() override { handler_.advance(); }
    virtual bool empty() {
        handler_.setToFirst();
        return handler_.more() == false;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "PointsHandler on " << handler_; }
    AbstractPoints& handler_;

private:
    //! Copy constructor - No copy allowed
    PointsHandler(const PointsHandler&);
    //! Overloaded << operator to copy - No copy allowed
    PointsHandler& operator=(const PointsHandler&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const PointsHandler& p) {
        p.print(s);
        return s;
    }
};

class BatchPointsHandler : public PointsHandler {
public:
    BatchPointsHandler(AbstractPoints& handler, int count) :
        PointsHandler(handler), count_(count), more_(0), last_(false) {}
    virtual ~BatchPointsHandler() override {}
    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst() override {}
    //! Method to test the end of collection.
    virtual bool more() override {
        if (last_) {
            last_ = false;
            return false;
        }
        // MagLog::debug() << "batch " << more_ << "\n";
        if (more_++ < count_)
            return this->handler_.more();

        last_ = true;
        return true;
    }
    //! Method to return the current value
    virtual const UserPoint& current() override { return this->handler_.current(); }
    //! Method to advance
    virtual void advance() override { this->handler_.advance(); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override {
        out << "BatchPointsHandler(" << count_ << "items) on " << this->handler_;
    }

    int count_;
    mutable int more_;
    mutable bool last_;

private:
    //! Copy constructor - No copy allowed
    BatchPointsHandler(const BatchPointsHandler&);
    //! Overloaded << operator to copy - No copy allowed
    BatchPointsHandler& operator=(const BatchPointsHandler&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BatchPointsHandler& p) {
        p.print(s);
        return s;
    }
};


class BoxPointsHandler : public PointsHandler {
public:
    BoxPointsHandler(AbstractPoints& handler, const Transformation& transformation, bool filter) :
        PointsHandler(handler), transformation_(transformation), filter_(filter) {}
    virtual ~BoxPointsHandler() override {}

    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst() override;

    //! Method to test the end of collection.
    virtual bool more() override { return more_; }

    //! Method to return the current value
    virtual UserPoint& current() override { return current_; }

    //! Method to advance
    virtual void advance() override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "BocPointsHandler() on " << this->handler_; }

    const Transformation& transformation_;
    mutable std::stack<UserPoint> duplicates_;

    mutable UserPoint current_;
    mutable bool more_;
    bool filter_;  // Do no send the point if they are outside the transformation view!
};

class ThinningPointsHandler : public PointsHandler {
public:
    ThinningPointsHandler(AbstractPoints& handler, int xfreq, int yfreq) :
        PointsHandler(handler), xfreq_(xfreq), yfreq_(yfreq) {}
    virtual ~ThinningPointsHandler() override {}

    //! Method to set the current position to the first point.(abstract)
    virtual void setToFirst() override;

    //! Method to test the end of collection.
    virtual bool more() override { return more_; }

    //! Method to return the current value
    virtual const UserPoint& current() override { return current_; }

    //! Method to advance
    virtual void advance() override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "BocPointsHandler() on " << this->handler_; }

    mutable int xfreq_;
    mutable int yfreq_;
    mutable map<double, map<double, UserPoint> > data_;
    map<double, map<double, UserPoint> >::iterator y_;
    map<double, UserPoint>::iterator x_;
    mutable UserPoint current_;
    mutable bool more_;
};

}  // namespace magics

#endif
