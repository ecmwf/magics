/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvPrepBufrPrep.h

#ifndef MvPrepBufrPrep_DEFINED
#define MvPrepBufrPrep_DEFINED

#include <iostream>
#include <iomanip>
#include <vector>
#include "MvBufrObs.h"

using namespace std;

class MvObsSet;

//_____________________________________________________

class MvPrepBufrPrep // : public MvService
{
 private:
    MvPrepBufrPrep();
    MvPrepBufrPrep( const MvPrepBufrPrep& anOther );
    void operator= ( const MvPrepBufrPrep& anOther );

 public:
    MvPrepBufrPrep( MvObsSet& os ) : obsSet_(os) {}
    ~MvPrepBufrPrep();

    bool  prepareAll();

 protected:
    bool  createPrepBufrTableDir();
    bool  createTablesDecodeLinks( MvObs& firstObs );
    bool  setNewBufrTablesDir();
    bool  extractPrepBufrTables();
    bool  createDataDecodeLinks();
    void  revertBufrTablesDir();

 private:
    MvObsSet& obsSet_;                       //-- use a reference to MvObsSet
    string    origBufrTableDir_;
    string    prepBufrTableDir_;
    string    prepBufrTable_B_;
    string    prepBufrTable_D_;
};

//_____________________________________________________

class MvTableExtract // : public MvService
{
 private:
    MvTableExtract();
    MvTableExtract( const MvTableExtract& anOther );
    void operator= ( const MvTableExtract& anOther );

 public:
    MvTableExtract( MvObsSet& os ) : obsSet_(os) {}
    ~MvTableExtract(){}

    bool  extract( string& prepBufrTable_B, string& prepBufrTable_D );
    bool  initTableFiles( string& prepBufrTable_B, string& prepBufrTable_D );

 private:
    MvObsSet& obsSet_;                       //-- use a reference to MvObsSet
    MvObs     tableMsg_;
    ofstream  table_B_;
    ofstream  table_D_;
};
//_____________________________________________________

class TableB_entry
{
 public:
    TableB_entry(){}
    ~TableB_entry(){}

    bool getEntry( MvObs& obs );
    void writeEntry( ofstream& fout );
    string descr() const { return descriptor_; }

 private:
    string descriptor_;
    string name_;
    string unit_;
    string scale_;
    string refval_;
    string width_;
};
//_____________________________________________________

class TableD_entry
{
 public:
    TableD_entry(){}
    ~TableD_entry(){}

    bool getEntry( MvObs& obs );
    void writeEntry( ofstream& fout );
    string descr() const { return descriptor_; }

 private:
    string descriptor_;
    int    cnt_;
    vector<string> descrlist_;
};
//_____________________________________________________

#endif
// MvPrepBufrPrep_DEFINED
