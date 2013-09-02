#include <TeAddressLocator.h>

int TeAddressLocator::setModel ( TeLayer* layer,
					string& street,
					string& initialEven,
					string& finalEven,
					string& initialOdd,
					string& finalOdd)
{
	layer_		= layer;
	street_		= street;
	initialEven_= initialEven;
	finalEven_	= finalEven;
	initialOdd_ = initialOdd;
	finalOdd_	= finalOdd;
	return 0;
}

int TeAddressLocator::findExact (string& street, int number, TeCoord2D &pt, string& objectid, TeLineSet& ls)
{

// Build the query to select street segment that comprises the given number
	string snumber = Te2String (number);
	string query = "SELECT * FROM " + layer_->attributeTable();
	query += " WHERE ";
	query += street_+" LIKE '%"+street+"%'";
//	query += street_+" = '"+street+"'";
	if (number != 0)
	{
		if ((number%2) == 0)
			query += " AND "+initialEven_+" <= "+snumber+" AND "+finalEven_+" >= "+snumber;
		else
			query += " AND "+initialOdd_+" <= "+snumber+" AND "+finalOdd_+" >= "+snumber;
	}

	TeDatabasePortal *atrib = db_->getPortal ();

// Check if there is a segment that satisfies the clause
	if (!atrib->query(query))
	{
		delete atrib;
		return 1;
	}

// Evaluate the relative position of "number" (relativePosition) between initial and final numbers
// If number is zero, point to begin of line

	int initialEven,initialOdd,finalEven,finalOdd;
	double relativePosition = 0.;
	bool found = false;

	if (atrib->fetchRow())
	{
		found = true;
		objectid = string (atrib->getData(layer_->indexName()));

		if	(number != 0 )
		{
			if	((number%2) == 0)
			{
				initialEven = atoi (atrib->getData(initialEven_));
				finalEven = atoi (atrib->getData(finalEven_));
				if (initialEven != finalEven)
				{
					relativePosition = double(number-initialEven) / double(finalEven-initialEven);
				}
			}
			else
			{
				initialOdd = atoi (atrib->getData(initialOdd_));
				finalOdd = atoi (atrib->getData(finalOdd_));
				if (initialOdd != finalOdd)
				{
					relativePosition = double(number-initialOdd) / double(finalOdd-initialOdd);
				}
			}
		}
	}
	atrib->freeResult ();
	delete atrib;

// No street satifies the clause
	if (!found)
		return 2;

// Retrieve street segment geometry
	TeDatabasePortal *load = db_->getPortal ();
	ls.clear ();
	load->loadLineSet (layer_->tableName(TeLINES),objectid,ls);

// Find the actual position of number on the street
	if (ls.size() >= 1)
	{
		TeLine2D line = ls [0];
		TeCoord2D pt1,pt2;
		pt1 = line[0];
		pt2 = line[1];

		if (line.size () > 2)
		{
// Find the distance from number to the begin of street based on the relative position
			double targetLen = line.length()*relativePosition;
			double acumLen = 0.;
			double segLen;

// Find in which segment number is comprised
			for (int i=1 ; i < line.size() ; i++)
			{
				pt2 = line[i];
				segLen = sqrt ( (pt2.x()-pt1.x())*(pt2.x()-pt1.x())+(pt2.y()-pt1.y())*(pt2.y()-pt1.y()));
				acumLen += segLen;
				if (acumLen >= targetLen)
				{
					double targetLenInSeg = targetLen - (acumLen - segLen);
					relativePosition = targetLenInSeg/segLen;
					break;
				}
				pt1 = pt2;
			}

		}
		double x,y;
		x = pt1.x() + (pt2.x()-pt1.x())*relativePosition;
		y = pt1.y() + (pt2.y()-pt1.y())*relativePosition;
		pt = TeCoord2D (x,y);
	}

	delete load;
	
	return 0;
}

int TeAddressLocator::lookForLike (string& street, int number)
{
	string query = "SELECT * FROM " + layer_->attributeTable();
	query += " WHERE ";
	query += street_+" LIKE '%"+street+"%'";

	return 1;
}

bool TeAddressLocator::getLike ( TeCoord2D &pt, string& objectid)
{

	return false;
}

int TeAddressLocator::findSimilar (string& street, int number, TeCoord2D &pt, string& objectid)
{
	return 0;
}
