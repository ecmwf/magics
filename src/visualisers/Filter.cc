/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Filter.cc
    Implementation of Filter class.

    Magics Team - ECMWF 2004

    Created: Wed 14-Jun-2004

*/

#include "Filter.h"
#include "MagLog.h"
#include "Timer.h"

using namespace magics;


Filter::Filter(MatrixHandler& matrix, int nrows, int ncols) : matrix_(matrix), nrows_(nrows), ncols_(ncols) {
    flr_  = 0;
    fbuf_ = NULL;

    // Cache input data
    //	matrix.cache(matrix_);
}


Filter::~Filter() {
    FreeImageBuffers();
}


bool Filter::AllocImageBuffers() {
    int i;

    FreeImageBuffers();

    // Allocate space rotating buffer
    fbuf_ = new double*[nrows_];
    if (fbuf_ == NULL)
        return false;
    int nb_columns = matrix_.columns();
    for (i = 0; i < nrows_; i++) {
        fbuf_[i] = NULL;
        fbuf_[i] = new double[nb_columns];
        if (fbuf_[i] == NULL)
            return false;
    }

    return true;
}


void Filter::FreeImageBuffers() {
    if (fbuf_ == NULL)
        return;
    int i;
    for (i = 0; i < nrows_; i++) {
        if (fbuf_[i] != NULL) {
            delete[] fbuf_[i];
            fbuf_[i] = NULL;
        }
    }

    delete[] fbuf_;
    fbuf_ = NULL;

    return;
}


bool Filter::InitBuffer() {
    int lin, col;

    // Allocate space for output buffer
    if (AllocImageBuffers() == false)
        return false;
    int nb_columns = matrix_.columns();
    // Fill buffer with first nrows_ input data
    for (lin = 0; lin < nrows_; lin++)
        for (col = 0; col < nb_columns; col++)
            fbuf_[lin][col] = matrix_(lin, col);

    flr_ = nrows_ - 1;

    return true;
}


bool Filter::ScrollBuffer() {
    int ind, col;
    int nr = nrows_ - 1;
    double* aux;
    int nb_rows    = matrix_.rows();
    int nb_columns = matrix_.columns();

    // verify if the line is within of the image
    flr_++;
    if (flr_ < nb_rows) {
        aux = fbuf_[0];
        for (ind = 0; ind < nrows_ - 1; ind++)
            fbuf_[ind] = fbuf_[ind + 1];
        fbuf_[nr] = aux;
        for (col = 0; col < nb_columns; col++)
            fbuf_[nr][col] = matrix_(flr_, col);
    }
    else
        return false;

    return true;
}


MinMaxFilter::MinMaxFilter(MatrixHandler& matrix, int nrows, int ncols, int flag) :
    Filter(matrix, nrows, ncols),
    mflag_(flag) {}


MinMaxFilter::~MinMaxFilter() {}

/*!
  \brief Method to find extrema
*/

bool MinMaxFilter::Process() {
    if (mflag_ == 0)
        return false;
    double val;  // central point value
    bool found;
    int lin, col;
    int i, j;

    int fcr = this->nrows_ * .5;  // central line
    int fcc = this->ncols_ * .5;  // central column

    const bool doMin = (mflag_ == 1 || mflag_ == 3) ? true : false;
    const bool doMax = (mflag_ == 2 || mflag_ == 3) ? true : false;

    // Initialize input buffer
    if (this->InitBuffer() == false) {
        MagLog::debug() << " InitBuffer Error"
                        << "\n";
        return false;
    }

    // Processing
    long hi        = 0;
    long lo        = 0;
    double nb_rows = this->matrix_.rows() - this->nrows_;
    double nb_cols = this->matrix_.columns() - this->ncols_;

    for (lin = 0; lin <= nb_rows; lin++) {
        for (col = 0; col <= nb_cols; col++) {
            // check min value
            if (doMin) {
                val   = this->fbuf_[fcr][col + fcc];
                found = true;
                for (i = 0; i < this->nrows_; i++) {
                    for (j = 0; j < this->ncols_; j++) {
                        if (val >= this->fbuf_[i][col + j]) {
                            if (i != fcr || j != fcc) {
                                found = false;  // stop searching
                                break;
                            }
                        }
                    }
                    if (!found)
                        break;
                }

                if (found)  // minimum found
                {
                    //				MagLog::debug() << "Min " << lin+fcr+1 << " " << col+fcc+1 << " " << val << "\n";
                    lo = lo + 1;
                    push_back(UserPoint(this->matrix_.column(lin + fcr, col + fcc),
                                        this->matrix_.row(lin + fcr, col + fcc), val));
                    this->back().low(true);
                    this->back().high(false);
                }
            }

            // check max value
            if (doMax) {
                val   = this->fbuf_[fcr][col + fcc];
                found = true;
                for (i = 0; i < this->nrows_; i++) {
                    for (j = 0; j < this->ncols_; j++) {
                        if (val <= this->fbuf_[i][col + j]) {
                            if (i != fcr || j != fcc) {
                                found = false;  // stop searching
                                break;
                            }
                        }
                    }
                    if (!found)
                        break;
                }

                if (found)  // minimum found
                {
                    //				MagLog::debug() << "Max " << lin+fcr+1 << " " << col+fcc+1 << " " << val << "\n";
                    hi = hi + 1;
                    push_back(UserPoint(this->matrix_.column(lin + fcr, col + fcc),
                                        this->matrix_.row(lin + fcr, col + fcc), val));
                    this->back().high(true);
                    this->back().low(false);
                }
            }
        }  // end column

        if (this->ScrollBuffer() == false)
            break;
    }

    MagLog::debug() << "TOTAL POINTS LOW = " << lo << "\n"
                    << "TOTAL POINTS HI  = " << hi << "\n";
    return true;
}


// TEST, REMOVE LATER

void MinMaxFilter::test_build_data() {
    printf("\n\n TESTING   \n");
    double val;  // central point value
    bool found;
    int lin, col;
    int i, j;

    int fcr = this->nrows_ / 2;  // central line
    int fcc = this->ncols_ / 2;  // central column

    bool doMin = (mflag_ == 1 || mflag_ == 3) ? true : false;
    bool doMax = (mflag_ == 2 || mflag_ == 3) ? true : false;

    // Initialize input buffer
    if (this->InitBuffer() == false) {
        MagLog::debug() << " InitBuffer Error"
                        << "\n";
        return;
    }

    long hi = 0;
    long lo = 0;
    // Processing
    for (lin = 0; lin <= this->matrix_.rows() - this->nrows_; lin++) {
        printf("\n");
        for (i = 0; i < this->nrows_; i++) {
            for (j = 0; j < this->matrix_.columns(); j++)
                MagLog::debug() << this->fbuf_[i][j] << " ";
            printf("\n");
        }

        for (col = 0; col <= this->matrix_.columns() - this->ncols_; col++) {
            // check min value
            if (doMin) {
                val   = this->fbuf_[fcr][col + fcc];
                found = true;
                for (i = 0; i < this->nrows_; i++) {
                    for (j = 0; j < this->ncols_; j++) {
                        if (val >= this->fbuf_[i][col + j]) {
                            if (i != fcr || j != fcc) {
                                found = false;  // stop searching
                                break;
                            }
                        }
                    }
                    if (!found)
                        break;
                }

                if (found)  // minimum found
                {
                    MagLog::debug() << "\n"
                                    << "Min " << lin + fcr + 1 << " " << col + fcc + 1 << " " << val << "\n";
                    lo = lo + 1;
                }
            }

            // check max value
            if (doMax) {
                val   = this->fbuf_[fcr][col + fcc];
                found = true;
                for (i = 0; i < this->nrows_; i++) {
                    for (j = 0; j < this->ncols_; j++) {
                        if (val <= this->fbuf_[i][col + j]) {
                            if (i != fcr || j != fcc) {
                                found = false;  // stop searching
                                break;
                            }
                        }
                    }
                    if (!found)
                        break;
                }

                if (found)  // minimum found
                {
                    MagLog::debug() << "\n"
                                    << "Max " << lin + fcr + 1 << " " << col + fcc + 1 << " " << val << "\n";
                    hi = hi + 1;
                }
            }
        }  // end column

        if (this->ScrollBuffer() == false)
            break;
    }

    MagLog::debug() << "TOTAL POINTS LOW=" << lo << "\n";
    MagLog::debug() << "TOTAL POINTS HI =" << hi << "\n";

    printf("\n\nEND TEST\n");
}
