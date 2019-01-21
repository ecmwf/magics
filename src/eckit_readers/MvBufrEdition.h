/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#ifndef MVBUFREDITION_H
#define MVBUFREDITION_H

#include <string>
#include <vector>

class MvBufrEdition {
public:
    int masterTablesNumber() const { return masterNumber_; }
    int masterTablesVersionNumber() const { return masterVersion_; }
    int localTablesVersionNumber() const { return localVersion_; }
    int centre() const { return centre_; }
    int subCentre() const { return subCentre_; }

    const std::string& centreAsStr() const { return centreAsStr_; }
    void setCentreAsStr(const std::string& c) { centreAsStr_ = c; }

    static MvBufrEdition* find(int masterNumber, int masterVersion, int localVersionr, int centre, int subCentre);

protected:
    MvBufrEdition(int masterNumber, int masterVersion, int localVersionr, int centre, int subCentre);

    int masterNumber_;
    int masterVersion_;
    int localVersion_;
    int centre_;
    int subCentre_;
    std::string centreAsStr_;
    static std::vector<MvBufrEdition*> items_;
};

#endif  // MVBUFREDITION_H
