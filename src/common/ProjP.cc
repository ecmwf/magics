/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

//! \file ProjP.cc
/*!
 Sylvie Lamy-Thepaut - ECMWF Apr 02

 Changes:

   Apr 06: update for GCC 4.0 (Stephan)
*/
#include <proj.h>
#include <ProjP.h>

using namespace magics;

PJ_CONTEXT* ProjP::context_ = 0;

ProjP::ProjP() : converter_(0) {}
ProjP::ProjP(const string& from, const string& to) : from_(from), to_(to), converter_(0) {
    if (!context_)
        context_ = proj_context_create();
    PJ* p = proj_create_crs_to_crs(context_, from_.c_str(), to_.c_str(), NULL);
    assert(p);
    converter_ = proj_normalize_for_visualization(context_, p);

    assert(converter_);
    
}

ProjP::~ProjP() {
    if (converter_)
        proj_destroy(converter_);

    converter_ = 0;
}


string ProjP::error(int error) {
    return proj_errno_string(error);
}


int LatLonProjP::convert(double& x, double& y) const {
    PJ_COORD in, out;
    in.lpzt.lam = x;
    in.lpzt.phi = y;
    in.lpzt.z   = 0.0;
    in.lpzt.t   = HUGE_VAL;

    out = proj_trans(converter_, PJ_FWD, in);
    if (proj_errno(converter_)) {
        proj_errno_reset(converter_);
        return 1;
    }
    x = out.xy.x;
    y = out.xy.y;
    return 0;
}
int LatLonProjP::revert(double& x, double& y) const {
    PJ_COORD in, out;
    in.xy.x = x;
    in.xy.y = y;

    out = proj_trans(converter_, PJ_INV, in);
    if (proj_errno(converter_)) {
        proj_errno_reset(converter_);
        return 1;
    }

    x = out.lpzt.lam;
    y = out.lpzt.phi;
    return 0;
}

int ProjP::convert(double& x, double& y) const {
    PJ_COORD in, out;

    in.xy.x = x;
    in.xy.y = y;
    out     = proj_trans(converter_, PJ_FWD, in);
    if (proj_errno(converter_)) {
        proj_errno_reset(converter_);
        return 1;
    }
    x = out.xy.x;
    y = out.xy.y;

    return 0;
}
int ProjP::revert(double& x, double& y) const {
    PJ_COORD in, out;
    in.xy.x = x;
    in.xy.y = y;

    out = proj_trans(converter_, PJ_INV, in);
    if (proj_errno(converter_)) {
        proj_errno_reset(converter_);
        return 1;
    }
    x = out.xy.x;
    y = out.xy.y;

    

    return 0;
}

void ProjP::print(ostream&) const {}
