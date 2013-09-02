#include "TeImportExport.h"
#include "TeTable.h"
#include "TeAsciiFile.h"
#include "TeAttribute.h"
#include "TeUtils.h"
#include "TeLayer.h"
#include "TeDatabase.h"
#include "shapefil.h"
#include "TeException.h"

void 
TeReadCSVAttributeList ( const string& fileName, TeAttributeList& attList, char& sep )
{
	TeAsciiFile CSVFile(fileName);
	sep = ';';
	string value;

	// read the separator character
	while (CSVFile.isNotAtEOF())
	{
		value = CSVFile.readString();
		if (TeStringCompare(value,"separator"))
		{
			sep = CSVFile.readChar();
			break;
		}
	}
	CSVFile.rewind();

	// read the number of columns
	int nattr = 0;
	while (CSVFile.isNotAtEOF())
	{
		value = CSVFile.readString();
		if (TeStringCompare(value,"columns"))
		{
			nattr = CSVFile.readInt();
			break;
		}
	}
	CSVFile.findNewLine();
	for (int i=0; i<nattr; i++)
	{
		TeAttribute attribute;
		attribute.rep_.name_  = CSVFile.readStringCSVNoSpace(sep);
		string attType = CSVFile.readStringCSVNoSpace(sep);

		if ( TeStringCompare(attType,"INTEGER"))
		{
			attribute.rep_.type_ = TeINT;
		}
		else if (TeStringCompare(attType,"REAL"))
		{
			attribute.rep_.type_ = TeREAL;
		}
		else if (TeStringCompare(attType,"TEXT"))
		{
			attribute.rep_.type_  = TeSTRING;
			attribute.rep_.numChar_  = CSVFile.readIntCSV(sep);
		}
		attList.push_back ( attribute );
		CSVFile.findNewLine();
	}
}


bool
TeImportCSVTable(TeLayer& layer, const string& fileName, TeTable& table, unsigned int chunckSize)
{
	bool status;
	char separator = table.separator();

	// retrieves the associated attribute list
	TeAttributeList attList = table.attributeList();

	// index attribute must be also a primary key
	TeAttributeList keyList;
	TeAttribute keyAttr;
	string keyname;
	if (table.attributeIndex(keyAttr))
	{
		keyList.push_back(keyAttr);
		keyname = keyAttr.rep_.name_ + "_primkey";
	}

	layer.createAttributeTable(table,keyList,keyname);

	TeAsciiFile CSVFile(fileName);
	string value;

	// read the separator character
	while (CSVFile.isNotAtEOF())
	{
		value = CSVFile.readString();
		if (TeStringCompare(value,"INFO_END"))
			break;
	}
	CSVFile.findNewLine();
	int nAtt = attList.size();
	while (CSVFile.isNotAtEOF())
	{
		value = CSVFile.readStringCSVNoQuote(separator);
		if (TeStringCompare(value,"END"))
			break;

		TeTableRow row;
		for (int n=0; n<nAtt; n++)
		{
			row.push_back ( value ); 
			value = CSVFile.readStringCSVNoQuote(separator);
		}

		table.add( row );
		if ( table.size() == chunckSize )
		{
			status = layer.saveAttributeTable( table );
			table.clear();
			if (!status)
				return false;
		}
		CSVFile.findNewLine();
	}
	// save the remaining rows
	status = layer.saveAttributeTable( table );
	table.clear();
	return status;
}   
