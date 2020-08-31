/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#include "MvBufrEdition.h"

#ifdef METVIEW_BUFR
namespace metview {
static std::vector<MvBufrEdition*> bufrEditionItems;
}
#else
static std::vector<MvBufrEdition*> bufrEditionItems;
#endif

MvBufrEdition::MvBufrEdition(int masterNumber, int masterVersion, int localVersion,
                             int centre, int subCentre) :
    masterNumber_(masterNumber),
    masterVersion_(masterVersion),
    localVersion_(localVersion),
    centre_(centre),
    subCentre_(subCentre)
{
#ifdef METVIEW_BUFR
    metview::bufrEditionItems.push_back(this);
#else
    bufrEditionItems.push_back(this);
#endif
}

MvBufrEdition* MvBufrEdition::find(int masterNumber, int masterVersion, int localVersion,
                                   int centre, int subCentre)
{
#ifdef METVIEW_BUFR
    for(auto item: metview::bufrEditionItems) {
#else
    for(auto item: bufrEditionItems) {
#endif
        if (item->masterNumber_ == masterNumber && item->masterVersion_ == masterVersion &&
            item->localVersion_ == localVersion && item->centre_ == centre &&
            item->subCentre_ == subCentre) {
                    return item;
        }
    }

    MvBufrEdition* e = new MvBufrEdition(masterNumber, masterVersion, localVersion,
                                         centre, subCentre);
    return e;
}
