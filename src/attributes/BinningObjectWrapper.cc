
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file BinningObjectAttributes.h
    \\brief Definition of BinningObject Attributes class.

    This file is automatically generated.
    Do Not Edit!

*/

#include "MagRequest.h"
#include "BinningObjectWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "MagicsGlobal.h"

using namespace magics;



BinningObjectWrapper::BinningObjectWrapper(): binningobject_(new BinningObject())


{


	

}
BinningObjectWrapper::BinningObjectWrapper(BinningObject* binningobject): binningobject_(binningobject)
{

	
}

BinningObjectWrapper::~BinningObjectWrapper()
{

}

void BinningObjectWrapper::set(const MagRequest& request)
{

	

	if  (request.countValues("BINNING_X_METHOD") ) {
		string x_value = request("BINNING_X_METHOD");
		binningobject_->x_ = x_value;
		}
	if  (request.countValues("BINNING_X_MIN_VALUE") ) {
		double x_min_value = request("BINNING_X_MIN_VALUE");
		binningobject_->x_min_ = x_min_value;
		}
	if  (request.countValues("BINNING_X_MAX_VALUE") ) {
		double x_max_value = request("BINNING_X_MAX_VALUE");
		binningobject_->x_max_ = x_max_value;
		}
	if  (request.countValues("BINNING_X_COUNT") ) {
		int x_count_value = request("BINNING_X_COUNT");
		binningobject_->x_count_ = x_count_value;
		}
	doublearray  x_list_value;
	for (int i = 0; i < request.countValues("BINNING_X_LIST"); i++)
		x_list_value.push_back((double)request("BINNING_X_LIST", i));
	if ( !x_list_value.empty() )
		binningobject_->x_list_ = x_list_value;
	if  (request.countValues("BINNING_X_INTERVAL") ) {
		double x_interval_value = request("BINNING_X_INTERVAL");
		binningobject_->x_interval_ = x_interval_value;
		}
	if  (request.countValues("BINNING_X_REFERENCE") ) {
		double x_reference_value = request("BINNING_X_REFERENCE");
		binningobject_->x_reference_ = x_reference_value;
		}
	if  (request.countValues("BINNING_Y_METHOD") ) {
		string y_value = request("BINNING_Y_METHOD");
		binningobject_->y_ = y_value;
		}
	if  (request.countValues("BINNING_Y_MIN_VALUE") ) {
		double y_min_value = request("BINNING_Y_MIN_VALUE");
		binningobject_->y_min_ = y_min_value;
		}
	if  (request.countValues("BINNING_Y_MAX_VALUE") ) {
		double y_max_value = request("BINNING_Y_MAX_VALUE");
		binningobject_->y_max_ = y_max_value;
		}
	if  (request.countValues("BINNING_Y_COUNT") ) {
		int y_count_value = request("BINNING_Y_COUNT");
		binningobject_->y_count_ = y_count_value;
		}
	doublearray  y_list_value;
	for (int i = 0; i < request.countValues("BINNING_Y_LIST"); i++)
		y_list_value.push_back((double)request("BINNING_Y_LIST", i));
	if ( !y_list_value.empty() )
		binningobject_->y_list_ = y_list_value;
	if  (request.countValues("BINNING_Y_INTERVAL") ) {
		double y_interval_value = request("BINNING_Y_INTERVAL");
		binningobject_->y_interval_ = y_interval_value;
		}
	if  (request.countValues("BINNING_Y_REFERENCE") ) {
		double y_reference_value = request("BINNING_Y_REFERENCE");
		binningobject_->y_reference_ = y_reference_value;
		}
	
	
}

void BinningObjectWrapper::print(ostream& out)  const
{
	out << "BinningObjectWrapper[]";
}


