/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#include "MvBufrEdition.h"

std::vector<MvBufrEdition*> MvBufrEdition::items_;

MvBufrEdition::MvBufrEdition(int masterNumber,int masterVersion,int localVersion,
              int centre, int subCentre) :
    masterNumber_(masterNumber),
    masterVersion_(masterVersion),
    localVersion_(localVersion),
    centre_(centre),
    subCentre_(subCentre)
{
    items_.push_back(this);
}

MvBufrEdition* MvBufrEdition::find(int masterNumber,int masterVersion,int localVersion,
                                   int centre, int subCentre)
{
    for(std::vector<MvBufrEdition*>::const_iterator it=items_.begin(); it != items_.end(); ++it)
    {
        if((*it)->masterNumber_ == masterNumber && (*it)->masterVersion_ == masterVersion &&
            (*it)->localVersion_ == localVersion &&  (*it)->centre_ == centre &&
            (*it)->subCentre_ == subCentre)
        {
            return *it;
        }
    }

    MvBufrEdition* e=new MvBufrEdition(masterNumber,masterVersion,localVersion,
                                       centre, subCentre);
    return e;
}
