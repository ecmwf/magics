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
    \file MgQSymbol.h
    \brief Definition of MgQSymbol.
    \author Graphics Section, ECMWF

    Started: Feb 2010
*/

#ifndef _MgQSymbol_H
#define _MgQSymbol_H

#include <QBrush>
#include <QGraphicsItem>
#include <QList>
#include <QPainterPath>
#include <QPen>
#include <QString>

#include "MgQ.h"

class MgQPainterPath : public QPainterPath {
public:
    MgQPainterPath(bool filled, bool defaultFill = false) :
        filled_(filled),
        defaultFill_(defaultFill),
        renderOnlyForOutline_(false){};
    bool isFilled() const { return filled_; }
    bool isDefaultFill() const { return defaultFill_; }
    void setRenderOnlyForOutline(bool b) { renderOnlyForOutline_ = b; }
    bool renderOnlyForOutline() const { return renderOnlyForOutline_; }

protected:
    bool filled_;
    bool defaultFill_;
    bool renderOnlyForOutline_;
};

class MgQSymbolItem {
public:
    MgQSymbolItem(QString id, float s) : id_(id), size_(s){};

    const QString& id() const { return id_; }
    float size() { return size_; };
    bool equal(const QString&, const float);
    const QList<MgQPainterPath>& paths() const { return paths_; }
    void addPath(MgQPainterPath& p) { paths_.push_back(p); }
    bool hasFilledPart();

protected:
    QString id_;
    float size_;
    QList<MgQPainterPath> paths_;
};

class MgQSymbolManager : public QList<MgQSymbolItem*> {
public:
    MgQSymbolManager(){};
    ~MgQSymbolManager();

    MgQSymbolItem* addSymbol(const QString&, const float);
    MgQSymbolItem* getSymbol(const QString&, const float);
    void deleteSymbol(const QString&, const float);

    int symbolNum() { return symbols_.count(); };

protected:
    QList<MgQSymbolItem*> symbols_;
};

class MgQSymbolSetItem : public QGraphicsItem {
public:
    enum
    {
        Type = MgQ::SymbolSetItemType
    };

    MgQSymbolSetItem(MgQSymbolItem* symbol, QRectF& boundingRect, QGraphicsItem* parent = 0) :
        QGraphicsItem(parent),
        symbol_(symbol),
        boundingRect_(boundingRect),
        keepSizeWhenScaling_(false),
        outline_(false),
        connectLine_(false){};

    QRectF boundingRect() const;
    void paint(QPainter*, const QStyleOptionGraphicsItem*, QWidget* widget = 0);
    int type() const { return Type; }

    void setColor(QColor);
    void setOutlineColor(QColor);
    void addPoint(double, double);
    void setKeepSizeWhenScaling(bool b) { keepSizeWhenScaling_ = b; }
    bool connectLine() { return connectLine_; }
    void setConnectLine(bool b) { connectLine_ = b; }
    void setConnectLinePen(QPen);

protected:
    QList<double> xp_;
    QList<double> yp_;
    MgQSymbolItem* symbol_;
    QPen pen_;
    QList<QBrush> brushes_;
    QRectF boundingRect_;
    bool keepSizeWhenScaling_;
    bool outline_;
    bool connectLine_;
    QPen connectLinePen_;
};


#endif
