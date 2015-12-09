/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvPrepBufrPrep.cc

#include <stdlib.h>
#include <string.h>
#include <list>
#include <set>
#include <unistd.h>  // for AIX to know symlink()
#include <sys/stat.h>
#include "MvPrepBufrPrep.h"
#include "MvObsSet.h"

const string cNoBufrTablesDir("NO_TABLE_DIR");
const string cCommonNamePart("_prepbufr.txt");
const string cAUX_TABLE_B("prepbufr_table_B.txt");
const string cAUX_TABLE_D("prepbufr_table_D.txt");


bool compare_tab_B_elems( const TableB_entry* first, const TableB_entry* second )
{ return first->descr().compare( second->descr() ) < 0; }

bool compare_tab_D_elems( const TableD_entry* first, const TableD_entry* second )
{ return first->descr().compare( second->descr() ) < 0; }


//______________________________________________________________________
//
//   MvPrepBufrPrep
//______________________________________________________________________

MvPrepBufrPrep::~MvPrepBufrPrep()
{
  //revertBufrTablesDir(); //-- this would change nothing as BUFR_TABLES
                           //-- will not be read anymore

  //string remove = "rm -r " + prepBufrTableDir_ + "/*";
  //cout << "rm command is: " << remove << endl;
//  int s = system( remove.c_str() );
}

//______________________________________________________________________

bool
MvPrepBufrPrep::prepareAll()
{
  bool ok = createPrepBufrTableDir();       //-- new unique subdir to play with table links
  if( ! ok )
     return false;

  MvObs firstObs = obsSet_.firstObs();      //-- create links to our tables that are
  ok = createTablesDecodeLinks( firstObs ); //-- needed to decode prepBUFR tables
  if( ! ok )
     return false;

  ok = setNewBufrTablesDir();               //-- (re)assign BUFR_TABLES env.var
  if( ! ok )
     return false;

  ok = extractPrepBufrTables();             //-- extract PrepBUFR tables using fixed table names
  if( ! ok )
     return false;

  ok = createDataDecodeLinks();             //-- 'bufrdc' file names link to fixed name PrepBUFR tables
  if( ! ok )
     return false;

  return true;
}

//______________________________________________________________________

bool
MvPrepBufrPrep::createPrepBufrTableDir()
{
                                            //-- store current value (if any)
  const char* oldDir = getenv("BUFR_TABLES");
  origBufrTableDir_ = oldDir ? oldDir : cNoBufrTablesDir;
  cout << "MvPrepBufrPrep::prepareTempDir: origBufrTableDir_=" << origBufrTableDir_ << endl;

  //char dirTemplate[1024];                   //-- build template for unique subdir name
  //strcpy( dirTemplate, getenv("METVIEW_TMPDIR") );
  //strcat( dirTemplate, "/PrepBufrTablesXXXXXX" );

  // to create a unique temporary directory name without using mkdtemp (which does not
  // work on AIX), we create a temporary file, append '_dir' and create a directory
  // with that name. This should be safe in almost 100% of cases...

  char *p;
  p = tempnam(NULL,"PrepBUFR");
  string tmpfilename(p);
  free(p);
  tmpfilename += "_dir";
  int ok = mkdir(tmpfilename.c_str(), S_IRWXU);

  if (ok != 0)
  {
#ifdef METVIEW
     marslog( LOG_EROR, "MvPrepBufrPrep::createPrepBufrTableDir: Unable to create directory %s", tmpfilename.c_str() );
#endif
     cerr << "MvPrepBufrPrep::createPrepBufrTableDir: Unable to create directory "
          << tmpfilename
          << endl;
     return false;
  }

  prepBufrTableDir_ = tmpfilename;//mkdtemp(dirTemplate); //-- create a unique subdir for PrepBufr tables
  cout << "MvPrepBufrPrep::prepareTempDir: prepBufrTableDir_=" << prepBufrTableDir_ << endl;

  return true;
}

//______________________________________________________________________
//  Build links (using file names required by 'bufrdc') to custom table
//  files stored in 'etc', needed to extract PrepBUFR Tables from ObsSet.
//  This is because PrepBUFR table msgs may require non-existing tables.

bool MvPrepBufrPrep::createTablesDecodeLinks( MvObs& firstObs )
{
  ostringstream commonPart;                 //-- build part of filename common to both files
  int centre    = firstObs.originatingCentre();
  int subCentre = firstObs.originatingSubCentre();
  int masterTableVer = firstObs.masterTableVersion();
  int localTableVer  = firstObs.localTableVersion();

  commonPart << "00"                        //-- discipline: 0 = meteorology
             << setw(6) << setfill('0') << subCentre
             << setw(5) << setfill('0') << centre
             << setw(3) << setfill('0') << masterTableVer
             << setw(3) << setfill('0') << localTableVer
             << ".TXT"
             << ends;

  string table_B_name = prepBufrTableDir_ + "/B" + commonPart.str();
  string table_D_name = prepBufrTableDir_ + "/D" + commonPart.str();

                                            //-- links to the real table files in dir 'etc'
  string mvShareDir( getenv("METVIEW_DIR_SHARE") );
  string etc_auxTable_B = mvShareDir + "/etc/AuxPrepBufrTable_B.txt";
  string etc_auxTable_D = mvShareDir + "/etc/AuxPrepBufrTable_D.txt";

  string auxTable_B = prepBufrTableDir_ + "/" + cAUX_TABLE_B;
  string auxTable_D = prepBufrTableDir_ + "/" + cAUX_TABLE_D;

  //--  1) copy table file from etc/share and make writable (we need to append PrepBUFR entries)
  string copyTableB = "cp " + etc_auxTable_B + " " + auxTable_B + "; chmod u+w " + auxTable_B + ";";
  cout << copyTableB << endl;
  int ok = system( copyTableB.c_str() );

  //--  2) create 'bufrex' named link to this file
  ok = symlink( auxTable_B.c_str(), table_B_name.c_str() );
  if( ok != 0 )
  {
#ifdef METVIEW
     marslog( LOG_EROR, "Unable to copy auxiliary BUFR Table B" );
#endif
     cerr << "Unable to copy auxiliary BUFR Table B" << endl;
     return false;
  }

  //--  1) copy table file from etc/share and make writable (we need to append PrepBUFR entries)
  string copyTableD = "cp " + etc_auxTable_D + " " + auxTable_D + "; chmod u+w " + auxTable_D + ";";
  cout << copyTableD << endl;
  ok = system( copyTableD.c_str() );

  //--  2) create 'bufrex' named link to this file
  ok = symlink( auxTable_D.c_str(), table_D_name.c_str() );
  if( ok != 0 )
  {
#ifdef METVIEW
     marslog( LOG_EROR, "Unable to create a link into auxiliary BUFR Table D" );
#endif
     cerr << "Unable to create a link into auxiliary BUFR Table D" << endl;
     return false;
  }

  return true;
}
//______________________________________________________________________
//  Assigns BUFR_TABLES to point to the temporary directory which by now
//  should contain links to auxiliary BUFR tables needed to decode the
//  prepBUFR tables, and where the prepBUFR tables will be extracted to.

bool MvPrepBufrPrep::setNewBufrTablesDir()
{
  const int overWrite = 1;
  string newEnvVar = prepBufrTableDir_ + "/";

  int ok = setenv( "BUFR_TABLES", newEnvVar.c_str(), overWrite );
  if( ok != 0 )
  {
#ifdef METVIEW
     marslog( LOG_EROR, "Unable change to BUFR_TABLES env.variable" );
#endif
     cerr << "Unable change to BUFR_TABLES env.variable" << endl;
     return false;
  }

  cout << "MvPrepBufrPrep::setNewBufrTablesDir: BUFR_TABLES new values is "
       << newEnvVar << endl;

  return true;
}
//______________________________________________________________________
//  Build file names for temporary PrepBUFR tables and
//  extract Table B and Table D from the current ObsSet.

bool MvPrepBufrPrep::extractPrepBufrTables()
{
  prepBufrTable_B_ = prepBufrTableDir_ + "/" + cAUX_TABLE_B; //-- build table B filename
  prepBufrTable_D_ = prepBufrTableDir_ + "/" + cAUX_TABLE_D; //-- build table D filename

  MvTableExtract extract( obsSet_ );
  return extract.extract( prepBufrTable_B_, prepBufrTable_D_ );
}
//______________________________________________________________________
//  Create links (using 'bufrdc' table file names) to temporary PrepBUFR
//  table files. Make sure that there are links corresponding to all
//  'bufrdc' table file names that are needed to decode any BUFR data
//  message in the current ObsSet.
//  Note that PrepBUFR ObsSet may contain msgs with different table or
//  local table version, i.e. we may need several different link names.

bool MvPrepBufrPrep::createDataDecodeLinks()
{
  set<string> tables;                       //-- keep track of existing table links
  set<string>::iterator it;

  obsSet_.rewind();
  MvObsSetIterator iter( obsSet_ );
  MvObs obs;

  while( (obs = iter()) )                     //-- search all msgs for the required 'bufrdc' table names
  {
     if( obs.messageType() == 11 )          //-- skip if msg contains BUFR tables
        continue;
                                            //-- get values used in building table names
     int centre    = obs.originatingCentre();
     int subCentre = obs.originatingSubCentre();
     int masterTableVer = obs.masterTableVersion();
     int localTableVer  = obs.localTableVersion();

     ostringstream commonPart;              //-- build part of the filename common to both files

     if( localTableVer == 0 )               //-- bufrdc: if no local tables then uses 00 tables
        commonPart << "00"                  //-- discipline: 0 = meteorology
	     << setw(6) << setfill('0') << 0  // subCentre
	     << setw(5) << setfill('0') << 0  // centre
	     << setw(3) << setfill('0') << masterTableVer
	     << setw(3) << setfill('0') << 0  // localTableVer
	     << ".TXT"
	     << ends;
     else                                   //-- bufrdc: need site specific tables
        commonPart << "00"                  //-- discipline: 0 = meteorology
	     << setw(6) << setfill('0') << subCentre
	     << setw(5) << setfill('0') << centre
	     << setw(3) << setfill('0') << masterTableVer
	     << setw(3) << setfill('0') << localTableVer
	     << ".TXT"
	     << ends;
                                            //-- build 'bufrdc' style Table B name
     string table_B_name = prepBufrTableDir_ + "/B" + commonPart.str();

     it = tables.find(table_B_name);        //-- link already created?
     if( it == tables.end() )               //-- if not, then create it now
     {
        int ok = symlink( prepBufrTable_B_.c_str(), table_B_name.c_str() );
        if( ok != 0 )
        {
#ifdef METVIEW
           marslog( LOG_EROR, "Unable to create a link into PrepBUFR Table B, returned %d", ok );
#endif
           cerr << "Unable to create a link into PrepBUFR Table B" << endl;
           return false;
        }
                                            //-- build 'bufrdc' style Table D name
        string table_D_name = prepBufrTableDir_ + "/D" + commonPart.str();
        ok = symlink( prepBufrTable_D_.c_str(), table_D_name.c_str() );
        if( ok != 0 )
        {
#ifdef METVIEW
           marslog( LOG_EROR, "Unable to create a link into PrepBUFR Table D, returned %d", ok );
#endif
           cerr << "Unable to create a link into PrepBUFR Table D" << endl;
           return false;
        }

  cout << " created table link " << table_B_name << endl;

        tables.insert(table_B_name);        //-- signal that link now exists
     }
  }
  obsSet_.rewind();
  return true;
}

//______________________________________________________________________
//  BUFR_TABLES was previously set to point to the temporary table dir.
//  Now revert it to point back to the original table dir (or unset if
//  not set previously).
void
MvPrepBufrPrep::revertBufrTablesDir()
{
  const int overWrite = 1;
  if( origBufrTableDir_ == cNoBufrTablesDir )
     unsetenv( "BUFR_TABLES" );
  else
     setenv( "BUFR_TABLES", origBufrTableDir_.c_str(), overWrite );
}

//______________________________________________________________________
//
//   TableB_entry
//______________________________________________________________________

//______________________________________________________________________
//  Decode one PrepBUFR Table B descriptor and its definition.
//
//  'bufrdc' read cmd is:
//
//                    1  2  8  9   73 74  98 99 102 103 115 116 ->119
//      READ(YENTRY,'(1X,I6,1x,64x,1x,24x,1x,I3,1x,I12,1x,I3)')
//     1                                         NTABBTR(J),NTABBS (J),
//     1                                         NTABBRV(J),NTABBDW(J)


bool
TableB_entry::getEntry( MvObs& obs )
{
  obs.setNextDescriptor();                  //-- build descriptor F+XX+YYY
  descriptor_ = obs.stringValue().substr(0,1);
  obs.setNextDescriptor();
  descriptor_ += obs.stringValue().substr(0,2);
  obs.setNextDescriptor();
  descriptor_ += obs.stringValue().substr(0,3);

  obs.setNextDescriptor();                  //-- data name: line 1 + line 2
  name_ = obs.stringValue().substr(0,32);
  obs.setNextDescriptor();
  name_ += obs.stringValue().substr(0,32);

  obs.setNextDescriptor();                  //-- unit name
  unit_ = obs.stringValue().substr(0,24);

  obs.setNextDescriptor();                  //-- scale: sign + abs value
  scale_ = obs.stringValue().substr(0,1);
  obs.setNextDescriptor();
  scale_ += obs.stringValue().substr(0,3);

  obs.setNextDescriptor();                  //-- reference value: sign + abs value
  refval_ = obs.stringValue().substr(0,1);
  obs.setNextDescriptor();
  refval_ += obs.stringValue().substr(0,10);

  obs.setNextDescriptor();                  //-- bit field width
  width_ = obs.stringValue().substr(0,3);

  //-- aki: check  OK...                    //-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  return true;
}
//______________________________________________________________________

void
TableB_entry::writeEntry( ofstream& fout )
{
  fout << " " << descriptor_
       << " " << name_
       << " " << unit_
       << " " << scale_
       << " " << refval_
       << " " << width_
       << endl;
}
//______________________________________________________________________
//
//   TableD_entry
//______________________________________________________________________

//______________________________________________________________________
//  Decode one PrepBUFR Table D descriptor and its definition

bool
TableD_entry::getEntry( MvObs& obs )
{
  obs.setNextDescriptor();                  //-- build descriptor F+XX+YYY
  descriptor_ = obs.stringValue().substr(0,1);
  obs.setNextDescriptor();
  descriptor_ += obs.stringValue().substr(0,2);
  obs.setNextDescriptor();
  descriptor_ += obs.stringValue().substr(0,3);

  obs.setNextDescriptor();                  //-- maybe text...
  long descr = obs.MvObs::currentDescriptor();
  if( (descr/1000) == 205 )                 //-- 2'05'yyy: signify character?
     obs.setNextDescriptor();               //-- is extra text => skip

  cnt_ = obs.currentValue();                //-- nr of descriptors in the sequence

  if( cnt_ > 0 )
  {
    for(int d=0; d<cnt_; ++d)
    {
       obs.setNextDescriptor();
       descrlist_.push_back( obs.stringValue().substr(0,6) );
    }
  }
  else
    descrlist_.push_back( "777777" );

  //-- aki: check  OK...                    //-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  return true;
}
//______________________________________________________________________

void
TableD_entry::writeEntry( ofstream& fout )
{
  fout << " " << descriptor_
       << setw(3) << cnt_
       << " " << descrlist_[0]
       << endl;

  if( cnt_ > 0 )
  {
    for(int d=1; d<cnt_; ++d)
       fout << "           " << descrlist_[d] << endl;
  }

  descrlist_.clear();
}
//______________________________________________________________________
//
//   MvTableExtract
//______________________________________________________________________

bool
MvTableExtract::initTableFiles( string& prepBufrTable_B, string& prepBufrTable_D )
{
  //-- use previously copied aux table file and open it in append mode
  table_B_.open( prepBufrTable_B.c_str(), ios::app ); //-- append to aux table B
  if( ! table_B_ )
  {
#ifdef METVIEW
     marslog( LOG_EROR, "Unable to open file %s", prepBufrTable_B.c_str() );
#endif
     cerr << "Unable to open file " << prepBufrTable_B << endl;
     return false;
  }

  //-- use previously copied aux table file and open it in append mode
  table_D_.open( prepBufrTable_D.c_str(), ios::app ); //-- append to aux table D
  if( ! table_D_ )
  {
#ifdef METVIEW
     marslog( LOG_EROR, "Unable to open file %s", prepBufrTable_D.c_str() );
#endif
     cerr << "Unable to open file " << prepBufrTable_D << endl;
     return false;
  }

  return true;
}
//______________________________________________________________________
//  Extract PrepBUFR Table B and Table D entries into memory, sort the
//  entries ('bufrdc' requirement), and then finally write the sorted
//  entries to PrepBUFR table files B and D.

bool
MvTableExtract::extract( string& prepBufrTable_B, string& prepBufrTable_D )
{
                                            //-- open/create table B and D files
  if( ! initTableFiles( prepBufrTable_B, prepBufrTable_D ) )
  {
     return false;
  }

  list<TableB_entry*> tab_B_list;           //-- for collecting PrepBUFR table B entries
  list<TableD_entry*> tab_D_list;           //-- for collecting PrepBUFR table D entries

  MvObsSetIterator iter(obsSet_);           //-- ObsSet iterator to select only table msgs
  iter.setMessageType(11);                  //-- 11: "BUFR tables, complete replacement or update"
  int msgNum = 0;

  tableMsg_ = iter();                       //-- get first BUFR table msg

  while( tableMsg_ )                        //-- iterate though table msgs
  {
    ++msgNum;                               //-- table msg number (for error msgs)

    //-- Each table msg contains three delayed replications (0'31'001):
    //-- 1st is for Table A (we skip), 2nd for Table B, 3rd for Table D

                                            //-- extract possible table B stuff
    int count_B = (int)tableMsg_.valueByOccurrence( 2, 31001 );
    for(int b=0; b<count_B; ++b)
    {
      TableB_entry* entry_B = new TableB_entry;
      if( entry_B->getEntry(tableMsg_) )
         tab_B_list.push_back(entry_B);
#ifdef METVIEW
      else
         marslog( LOG_EROR, "Error in extracting PrepBUFR Table B entry %d in msg %d", b+1, msgNum );
#endif
    }
                                            //-- extract possible table D stuff
    int count_D = (int)tableMsg_.valueByOccurrence( 3, 31001 );
    for(int d=0; d<count_D; ++d)
    {
      TableD_entry* entry_D = new TableD_entry;
      if( entry_D->getEntry(tableMsg_) )
         tab_D_list.push_back(entry_D);
#ifdef METVIEW
      else
         marslog( LOG_EROR, "Error in extracting PrepBUFR Table D entry %d in msg %d", d+1, msgNum );
#endif
    }

    tableMsg_ = iter();                     //-- get next BUFR tables msg
  }

  tab_B_list.sort( compare_tab_B_elems );   //-- sort prepBUFR Table B entries

  list<TableB_entry*>::iterator bit;        //-- write prepBUFR Table B entries
  for( bit = tab_B_list.begin(); bit != tab_B_list.end(); bit++ )
    (*bit)->writeEntry( table_B_ );

  tab_D_list.sort( compare_tab_D_elems );   //-- sort prepBUFR Table D entries

  list<TableD_entry*>::iterator dit;        //-- write prepBUFR Table D entries
  for( dit = tab_D_list.begin(); dit != tab_D_list.end(); dit++ )
    (*dit)->writeEntry( table_D_ );

  return true; //-- <aki> add failure checks...
}
//______________________________________________________________________

#if 0
int main()
{
  MvObsSet       obsSet( "/var/tmp/vesa/Data/bufr/2011-01-17-CPTEC/gdas1.t00z.prepbufr.nr" );
  MvTableExtract extract( obsSet );
  extract.extract();
}
#endif
