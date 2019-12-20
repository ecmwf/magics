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
#include <ProjP.h>

using namespace magics;

ProjP::ProjP() : converter_(0) {}
ProjP::ProjP(const string& from, const string& to)
    : from_(from), to_(to), converter_(0) {
  converter_ = proj_create_crs_to_crs(PJ_DEFAULT_CTX, from_, to_, NULL);
  reverter_ = proj_create_crs_to_crs(PJ_DEFAULT_CTX, to_, from_, NULL);

  assert(converter_);
}

ProjP : : ~ProjP() {
  if (converter_) proj_destroy(converter_);
  if (reverter_) proj_destroy(converter_);
}

int LatLonProjP::convert(double& x, double& y) {
  PJ_COORD in, out;
  in.lpzt.lam = x;
  in.lpzt.phi = y;
  in.lpzt.z = 0.0;
  in.lpzt.t = HUGE_VAL;
  out = proj_trans(converter_, PJ_FWD, out);
  x = out.xy.x;
  y = out.xy.y;
}
int LatLonProj4::revert(double& x, double& y) {
  PJ_COORD in, out;
  in.xy.x = x;
  in.xy.y = y;

  out = proj_trans(converter_, PJ_FWD, out);
  x = out.lpzt.lam;
  y = out.lpzt.phi;
}

int ProjP::convert(double& x, double& y) {
  PJ_COORD in, out;
  in.xy.lam = x;
  in.xy.phi = y;
  out = proj_trans(converter_, PJ_FWD, out);
  x = out.xy.x;
  y = out.xy.y;
}
int ProjP::revert(double& x, double& y) {
  PJ_COORD in, out;
  in.xy.x = x;
  in.xy.y = y;

  out = proj_trans(reverter_, PJ_FWD, out);
  x = out.xy.x;
  y = out.xy.y;
}

void ProjP::print(ostream&) const {}