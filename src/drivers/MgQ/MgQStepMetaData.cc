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
    \file MgQStepMetaData.cc
    \brief Definition of MgQStepMetaData.
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#include "MgQStepMetaData.h"

#include <QDebug>


MgQStepMetaData::MgQStepMetaData(QStringList keys) : keys_(keys) {
    for (int i = 0; i < keys_.count(); i++) {
        stepData_[i] = QStringList();
    }
}

void MgQStepMetaData::clear() {
    keys_.clear();
    stepData_.clear();
}

int MgQStepMetaData::stepNum() {
    if (keys_.count() > 0)
        return stepData_[0].count();
    else
        return 0;
}

QStringList MgQStepMetaData::stepData(int index) {
    return stepData_[index];
}

void MgQStepMetaData::addStep(QString value) {
    foreach (int index, stepData_.keys()) { stepData_[index].append(value); }
}

void MgQStepMetaData::appendToStepData(QString key, int step, QString value) {
    int index = keys_.indexOf(key, 0);
    while (index != -1) {
        QStringList& lst = stepData_[index];
        // qDebug() << key << index << step << lst;
        lst[step].append(value);

        index = keys_.indexOf(key, index + 1);
    }
}