#ifndef TeAddressLocator_H
#define TeAddressLocator_H

#include <TeDatabase.h>
#include <TeLayer.h>

#include <string>

using namespace std;

class TeAddressLocator {

protected:

	TeDatabase* db_;
	TeLayer	*layer_;
	string	street_;
	string	initialEven_;
	string	finalEven_;
	string	initialOdd_;
	string	finalOdd_;

public:

	TeAddressLocator() {db_ = 0;}
	void setDB (TeDatabase* db) { db_ = db; }
	int setModel ( TeLayer* layer,		//! layer to be searched
					string& street,		//! field that contains street name
					string& initialEven,//! field that contains initial even number
					string& finalEven,	//! fiela that contains final even number
					string& initialOdd,	//! field that contains initial odd number
					string& finalOdd	//! field that contains final odd number
					);

	int lookForLike (string& street, int number);
	bool getLike (TeCoord2D &pt, string& objectid);

	int findExact (string& street, int number, TeCoord2D &pt, string& objectid,	TeLineSet& ls);
	int findSimilar (string& street, int number, TeCoord2D &pt, string& objectid);

};

#endif