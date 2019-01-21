/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file MgQStepMetaData.h
    \brief Definition of MgQStepMetaData.
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#ifndef MgQStepMetaData_H
#define MgQStepMetaData_H

#include <QMap>
#include <QStringList>

class MgQStepMetaData {
public:
    MgQStepMetaData(QStringList);

    void clear();
    QStringList keys() { return keys_; }
    int stepNum();
    QStringList stepData(int);
    void addStep(QString);
    void appendToStepData(QString, int, QString);

protected:
    QStringList keys_;
    QMap<int, QStringList> stepData_;
};

#endif