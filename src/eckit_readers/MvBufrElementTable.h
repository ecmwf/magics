/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#ifndef MVBUFRELEMENTTABLE_H
#define MVBUFRELEMENTTABLE_H

#include <map>
#include <string>
#include <vector>

class MvBufrEdition;

class MvBufrElementTable {
public:
    const std::string& keyName(int);  // const;
    bool buildElementTable();
    static MvBufrElementTable* find(MvBufrEdition*);

protected:
    MvBufrElementTable(MvBufrEdition* edition);
    ~MvBufrElementTable();

    // static MvBufrCodeTable* make(int element,MvEccBufrMessage* msg);
    // void load(const std::string& path);

    MvBufrEdition* edition_;
    std::map<int, std::string> melems_;  // descriptior -> key

    static std::vector<MvBufrElementTable*> tables_;
};

#endif  // MVBUFRELEMENTTABLE_H
