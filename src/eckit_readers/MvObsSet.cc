/***************************** LICENSE START ***********************************
 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvObsSet.cc,   vk July94
//            rev vk 980306
//            rev fi 20170801  replace BUFRDC by eccodes

//--------------------------------------------------------------------
//  MvObsSet hides the data structures and parameter driven subroutines
//  needed to access BUFR files by wrapping them to a nicer C++ cover.
//--------------------------------------------------------------------
//  Routines and required data structures are described in
//  ECMWF Meteorological Bulletin:
//
//  "Accessing GRIB and BUFR data."
//       by J.D.Chambers
//
//  May 1994 Original version.
//--------------------------------------------------------------------

#include "MvObsSet.h"

#include <iostream>
#include <assert.h>
#include <cerrno>

#ifdef METVIEW_PREPBUFR
# include "MvPrepBufrPrep.h"
#endif

#ifdef MV_BUFRDC_TEST
#ifndef METVIEW
#include "grib_api.h"
long _readbufr(FILE *f, char *b, long *l)  // from mars/tools.c
{
   size_t len = *l;
   long e =  wmo_read_any_from_file(f,(unsigned char*)b,&len);
   *l = len;
   return e;
}
#endif
#endif

int MAX_MESSAGE_LENGTH = 32000;  //e maybe delete in the future

const int cMSG_TYPE_BUFR_TABLES = 11;

static std::string WRITE("w");  // I/O mode

//____________________________________________________________________
//==================================================================== MvObsSet
//____________________________________________________________________

MvObsSet::MvObsSet( const char *aName )
 : _unpacked(false)
 , _msgCount(0)
 , _ecH( NULL )
 , _minTime( 2247, 6, 20 )
 , _maxTime( 1799, 12, 31 ),
 useSkipExtraAttributes_(true),
 cacheCompressedData_(true)
{
   _IO_mode = "r";
   _init( aName );
}

//____________________________________________________________________
MvObsSet::MvObsSet( const char *aName, const char *aMode)
 : _unpacked(false)
 , _msgCount(0)
 , _ecH( NULL )
 , _minTime( 2247, 6, 20 )
 , _maxTime( 1799, 12, 31 ),
 useSkipExtraAttributes_(true),
 cacheCompressedData_(true)
{
   _IO_mode = aMode;
   _init( aName );
}

//____________________________________________________________________
#ifdef METVIEW
MvObsSet::MvObsSet( MvRequest &aRequest, const char *aMode )
 : _unpacked(false)
 , _msgCount(0)
 , _ecH( NULL )
 , _minTime( 2247, 6, 20 )
 , _maxTime( 1799, 12, 31 )
{
   _IO_mode = aMode;
   const char *aName = 0;
   aRequest.getValue( aName, "PATH" );
   _init( aName );
}
#endif

//____________________________________________________________________
MvObsSet::~MvObsSet()
{
   close();

#ifdef MV_BUFRDC_TEST
   delete [] _message;
   _message = 0;

//FAMI VERY IMPORTANT *****************************
// The code below was commented out because it was crashing
// Revise it later
//   if( _bufrOut )
//      delete _bufrOut;
#endif

#ifdef METVIEW_PREPBUFR
   if( _prepBufr  )
   {
      delete _prepBufr;
      _prepBufr = 0;
   }
#endif
}

//____________________________________________________________________
void
MvObsSet::_init( const char *aName )
{
#ifdef MV_BUFRDC_TEST
   _bufrFile = 0;
   _msgLen = 0;
#endif

   _ecFile = 0;
   _obsCount = -1;
   _msgNumber = -1;
   _minMaxDone = false;
   _IO_buffer_OK = false;

#ifdef METVIEW_PREPBUFR
   _isPrepBufrFile = false;
   _prepBufr = 0;
#endif

   // Open input bufr file
   Open( aName );

   // Initialise variables
   codes_count_in_file( NULL,_ecFile,&_msgCount );  // number of messages

   // Check if output file is BUFR and if PREPBUFR support is available
   if( _IO_mode == WRITE )  //should be two classes!!!!!!! (but this a working one...)
   {
#ifdef MV_BUFRDC_TEST
     _message = 0;
     _bufrOut = new MvBufrOut( MAX_MESSAGE_LENGTH, this );
#else
      _bufrOut = new MvBufrOut( this );
#endif
   }
   else
   {
#ifdef MV_BUFRDC_TEST
      _message = new char[ MAX_MESSAGE_LENGTH ];
#endif
      _bufrOut = 0;

/*  FAMI20171005 removed PrepBUFR code
      // Testing if PrepBUFR file
#ifdef METVIEW_PREPBUFR
      _firstObs = next();     //-- check if PrepBUFR file (contains BUFR tables)
      _isPrepBufrFile = ( _firstObs.messageType() == cMSG_TYPE_BUFR_TABLES );
      rewind();

      //  1) test if PrepBUFR tables have already been extracted: MV_PREPBUFR_TABLES_EXTRACTED
      //  2) extract only if not yet extracted
      if( _isPrepBufrFile && ! getenv("MV_PREPBUFR_TABLES_EXTRACTED") )
      {
         prepBufrFile();
         putenv((char*)("MV_PREPBUFR_TABLES_EXTRACTED=YES"));
      }
#else
      cout << "MvObsSet::_init - PrepBUFR support NOT available!!!!" << endl;
#endif
*/ //FAMI20171005
   }

   return;
}

//____________________________________________________________________ setSubsetMax
void
MvObsSet::setSubsetMax( int subsetMax )
{
//FAMI20171016 - I think ecCodes does not need this function (_maxNrSubsets).
//               If this is the case delete it later.
#ifdef MV_BUFRDC_TEST
   _bufrOut->setSubsetCount( subsetMax );
#endif
}

//____________________________________________________________________ Open
bool
MvObsSet::Open( const char* aFileName )
{
   _msgCount = 0;
   _msgNumber = 0;

#ifdef MV_BUFRDC_TEST
   if ( _IO_mode == WRITE )
   {
      string fn = aFileName + string("_bufrdc");
      _bufrFile = fopen( fn.c_str(), _IO_mode.c_str() );
   }
   else
      _bufrFile = fopen( aFileName, _IO_mode.c_str() );
#endif

   // Open bufr file
   _ecFile = fopen( aFileName, _IO_mode.c_str() );
   if( !_ecFile )
   {
      std::cerr << " >>> MvObsSet::Open - ERROR opening file \'" << aFileName << "\' - " << std::strerror(errno) << std::endl;
      return false;
   }
   return true;
}

//____________________________________________________________________ close
bool
MvObsSet::close()
{
   long myReturnValue = -1;
   if( _ecFile )
   {
#ifdef MV_BUFRDC_TEST
      if( _IO_mode == WRITE && _bufrOut->_outState == kBufrOut_dataInBuffers )
         _bufrOut->encode();

      fclose(_bufrFile);
      _bufrFile = 0;
#endif

      myReturnValue = fclose(_ecFile);
      _ecFile = 0;

      // Clean eccodes handler
      if ( _ecH )
      {
         codes_handle_delete(_ecH);
         _ecH = 0;
      }
   }

   _IO_buffer_OK = false;
   return !myReturnValue;
}

//____________________________________________________________________ rewind
void
MvObsSet::rewind()
{
   _msgNumber = 0;
   if( _ecFile )
   {

#ifdef MV_BUFRDC_TEST
::rewind( _bufrFile );
#endif

      // Clean previous handler
      if ( _ecH )
      {
         codes_handle_delete(_ecH);
         _ecH = 0;
      }
      ::rewind( _ecFile );  // execute C command 'rewind'!
   }

   return;
}

//msgCnt indexed from one!!!!
MvObs MvObsSet::gotoMessage(long offset, int msgCnt)
{
    if( !_ecFile )
       return MvObs( NULL );  // nothing if file not ok

    if( _IO_mode == WRITE )
       return MvObs( NULL );  // no next when writing !

    // Clean previous handler
    if ( _ecH )
    {
       codes_handle_delete(_ecH);
       _ecH = 0;
    }

    _msgNumber=msgCnt;

    fseek(_ecFile,offset,SEEK_SET);

    int err = 0;
    _ecH = codes_handle_new_from_file(NULL,_ecFile,PRODUCT_BUFR,&err);
    if (_ecH != NULL || err != CODES_SUCCESS)
    {
       if (_ecH == NULL)
       {
          std::cout << "Failed reading next BUFR msg: unable to create handle for message = " << _msgNumber << std::endl;
          codes_handle_delete(_ecH);
          _ecH = 0;
          _IO_buffer_OK = false;
          return MvObs( NULL );
       }

       // Expand all the descriptors i.e. unpack the data values
       _unpacked = false;   // it is a new message
       //if ( unpack )
       //  this->expand();

       _IO_buffer_OK = true;
    }

    return MvObs( &_ecH, 1, _unpacked, useSkipExtraAttributes_, cacheCompressedData_);  // subset number = 1
}

//__________________________________________________________________ next
// Reads next BUFR message and returns an MvObs constructed with it.
// An MvObs without a message is returned at EOF.
//------------------------------------------------
MvObs
MvObsSet::next( bool unpack )
{
   if( !_ecFile )
      return MvObs( NULL );  // nothing if file not ok

   if( _IO_mode == WRITE )
      return MvObs( NULL );  // no next when writing !

   _msgNumber++;

//e Analyse the PREPBufr code inside the loop.
#ifdef MV_BUFRDC_TEST
   const int EOF_STATUS = -1;
   bool readAnotherMessage = true;
   _msgLen = MAX_MESSAGE_LENGTH;
   long lastPos = ftell(_bufrFile);
   long myError = _readbufr( _bufrFile, _message, &_msgLen );

   while (readAnotherMessage)
   {
      readAnotherMessage = false;  // might be set to true in the PREPBufr part

      if ( myError == -3 )  // Bufr too small
      {
         // Go back to previous and allocate memory
         fseek(_bufrFile,lastPos,SEEK_SET);
         delete [] _message;
         MAX_MESSAGE_LENGTH = _msgLen + 8;
         _message = new char[ MAX_MESSAGE_LENGTH ];
         _msgLen = MAX_MESSAGE_LENGTH;

         if ( ! _message )
         {
            cerr << "MvObsSet::next: Cannot allocate memory for next BUFR message" << endl;
            _msgLen = MAX_MESSAGE_LENGTH  = 0;
            _IO_buffer_OK = false;
            return MvObs( NULL );
         }
         else
         {
            cout << "MvObsSet::next: Allocated more memory for BUFR msg" << endl;
            myError = _readbufr( _bufrFile, _message, &_msgLen );
         }
      }

      if( myError )
      {
         if( myError != EOF_STATUS )
         {
            cerr << "MvObsSet::next: Failed reading next BUFR msg, returned status=" << myError << endl;
         }
         _IO_buffer_OK = false;
         break;
      }

#ifdef METVIEW_PREPBUFR
      //--
      //-- NCEP PrepBUFR files may contain msgs with ZERO subsets!!!
      //-- 'bufrex' cannot handle such illegal msgs, if so we must skip it
      //--
      MvBufr tmpBufr( _message, _msgLen, _msgNumber  ); //-- make BUFR octets into an object
      if( tmpBufr.subsetCount() == 0 )
      {
         std::ostringstream os;
         os << "Original BUFR msg " << _msgNumber
            << " has ZERO subsets - ignoring (not counting) this illegal msg!"
            << ends;

         cout << os.str() << endl;

         //-- get next msg and cross your fingers it fits into current _message array
         _msgLen = MAX_MESSAGE_LENGTH;
         myError = _readbufr( _bufrFile, _message, &_msgLen );
         lastPos = ftell(_bufrFile);
         readAnotherMessage = true;

         //_msgNumber++;
      }
#endif
   }
#endif

//e analyse this code related to PrepBufr. It was inside the BUFRDC loop above
#if 0
#ifdef METVIEW_PREPBUFR
      //--
      //-- NCEP PrepBUFR files may contain msgs with ZERO subsets!!!
      //-- 'bufrex' cannot handle such illegal msgs, if so we must skip it
      //--
      MvBufr tmpBufr( _message, _msgLen, _msgNumber  ); //-- make BUFR octets into an object
      if( tmpBufr.subsetCount() == 0 )
      {
         std::ostringstream os;
         os << "Original BUFR msg " << _msgNumber
            << " has ZERO subsets - ignoring (not counting) this illegal msg!"
            << ends;

         cout << os.str() << endl;

         //-- get next msg and cross your fingers it fits into current _message array
         _msgLen = MAX_MESSAGE_LENGTH;
         myError = _readbufr( _bufrFile, _message, &_msgLen );
         lastPos = ftell(_bufrFile);
         readAnotherMessage = true;

         //_msgNumber++;
      }
#endif
#endif

   // Clean previous handler
   if ( _ecH )
   {
      codes_handle_delete(_ecH);
      _ecH = 0;
   }

   // Get next message
   int err = 0;
   _ecH = codes_handle_new_from_file(NULL,_ecFile,PRODUCT_BUFR,&err);
   if (_ecH != NULL || err != CODES_SUCCESS)
   {
      if (_ecH == NULL)
      {
         std::cout << "Failed reading next BUFR msg: unable to create handle for message = " << _msgNumber << std::endl;
         codes_handle_delete(_ecH);
         _ecH = 0;
         _IO_buffer_OK = false;
         return MvObs( NULL );
      }

      // Expand all the descriptors i.e. unpack the data values
      _unpacked = false;   // it is a new message
      if ( unpack )
         this->expand();

      _IO_buffer_OK = true;

#ifdef MV_BUFRDC_TEST
      return MvObs( new MvBufr( _message, _msgLen, _msgNumber ), 1, &_ecH );
#else
      return MvObs( &_ecH, 1, _unpacked, useSkipExtraAttributes_, cacheCompressedData_);  // subset number = 1
#endif

   }

   _IO_buffer_OK = false;
   return MvObs( NULL );
}


//____________________________________________________________________ add
void
MvObsSet::add( MvObs& anObs )
{
   if( _IO_mode != WRITE )
     return;  // no add when reading !

   _bufrOut->add( anObs );
   _msgCount++;
}

//____________________________________________________________________ write
#ifdef MV_BUFRDC_TEST
bool
MvObsSet::write( const char* aMsg, int aMsgLen )
{
   if( _IO_mode != WRITE )
      return false;

   fwrite( aMsg, sizeof( char ), aMsgLen, _bufrFile );

//FAMI20171017 remove this line temporarily, because the ecCodes write command
// below is also increasing the message count
//FAMI20171017   _msgNumber++;
   return true;
}
#endif

bool
MvObsSet::write( const void* aMsg, const size_t aMsgLen )
{
   if( _IO_mode != WRITE )
      return false;

   size_t size = aMsgLen;
   if (fwrite(aMsg, 1, size, _ecFile) != aMsgLen)
   {
       std::cout << "ERROR -  MvObsSet::write(const void*,int) -> could not write a field" << std::endl;
       return false;
   }

   _msgNumber++;
   return true;
}

bool
MvObsSet::write( MvObs& anObs )
{
#ifdef MV_BUFRDC_TEST
   if(_bufrOut)
      _bufrOut->write_bufrdc( anObs );
#endif

   // Clone the input handle
   codes_handle *clone_handle = codes_handle_clone(anObs.getHandle());
   if (clone_handle == NULL)
   {
       std::cout << "ERROR -  MvObsSet::write(MvObs&) -> could not clone field" << std::endl;
       return false;
   }

   // Get the coded message in a buffer
   const void *buffer = NULL;
   size_t size = 0;
   if (codes_get_message(clone_handle, &buffer, &size))
   {
      std::cout << "ERROR -  MvObsSet::write(MvObs&) -> could not create a buffer message" << std::endl;

      // Release the clone's handle
      codes_handle_delete(clone_handle);
      return false;
   }

   // Write the buffer to a file
   bool err = write(buffer,size);

   // Release the clone's handle
   codes_handle_delete(clone_handle);

   return err;
}

bool MvObsSet::writeCompressed(MvObs *obs)
{
#ifdef MV_BUFRDC_TEST
   if(_bufrOut)
      _bufrOut->write_bufrdc( obs );
#endif

    assert(obs);

    if(!obs->compressData())
        return false;

    // Clone the input handle
    codes_handle *cloneH = codes_handle_clone(obs->getHandle());
    if(cloneH == NULL)
    {
        std::cout << "ERROR -  MvObsSet::write(MvObs&) -> could not clone field" << std::endl;
        return false;
    }

   codes_set_long(cloneH,"unpack",1);
   codes_set_long(cloneH,"extractSubset", obs->subsetNumber());
   codes_set_long(cloneH,"doExtractSubsets",1);

   // Get the coded message in a buffer
   const void *buffer = NULL;
   size_t size = 0;
   if (codes_get_message(cloneH, &buffer, &size))
   {
      std::cout << "ERROR -  MvObsSet::write(MvObs&) -> could not create a buffer message" << std::endl;

      // Release the clone's handle
      codes_handle_delete(cloneH);
      return false;
   }

   // Write the buffer to a file
   bool err = write(buffer,size);

   // Release the clone's handle
   codes_handle_delete(cloneH);

   return err;
}

bool MvObsSet::writeCompressed(MvObs *obs,const std::vector<int>& subsetVec)
{
    assert(obs);

    if(!obs->compressData())
        return false;

    if(subsetVec.empty())
        return false;

    // Clone the input handle
    codes_handle *cloneH = codes_handle_clone(obs->getHandle());
    if(cloneH == NULL)
    {
        std::cout << "ERROR -  MvObsSet::write(MvObs&) -> could not clone field" << std::endl;
        return false;
    }

   size_t arrSize= subsetVec.size();
   long *subsetArr=new long[arrSize];
   for(size_t i=0; i < subsetVec.size(); i++)
        subsetArr[i]=subsetVec[i];

   codes_set_long(cloneH,"unpack",1);
   codes_set_long_array(cloneH,"extractSubsetList",subsetArr,arrSize);
   codes_set_long(cloneH,"doExtractSubsets",1);

   // Get the coded message in a buffer
   const void *buffer = NULL;
   size_t size = 0;
   if (codes_get_message(cloneH, &buffer, &size))
   {
      std::cout << "ERROR -  MvObsSet::write(MvObs&) -> could not create a buffer message" << std::endl;

      // Release the clone's handle
      codes_handle_delete(cloneH);
      delete [] subsetArr;
      return false;
   }

   // Write the buffer to a file
   bool err = write(buffer,size);

   // Release the clone's handle
   codes_handle_delete(cloneH);

   delete [] subsetArr;

   return err;
}


//____________________________________________________________________ messageCount
// Returns the number of BUFR messages found in the file.
// WARNING: a packed BUFR message may contain several observations
//          i.e. the nr of BUFR msgs found <= nr of observation msgs
//          i.e. messageCount() <= obsCount()
//------------------------------------------------------------------
int
MvObsSet::messageCount()
{
#if 0  //Fec
   // old code removed. Counts message using eccodes only.
   if( _msgCount < 1 )
   {
      if( _bufrFile )
      {
         long myOriginalFilePos = ftell( _bufrFile );
	 rewind();

         _msgCount = 0;
         while( next() )
	    _msgCount++;

printf("file bufrdc pos %ld\n",myOriginalFilePos);
	 fseek( _bufrFile, myOriginalFilePos, SEEK_SET );
       }
   }
//Fec   return _msgCount;
#endif

   return _msgCount;
}

//____________________________________________________________________ obsCount
int
MvObsSet::obsCount()
{
   // The number of total messages has been already computed
   if( _obsCount >= 1 )
      return _obsCount;

   // Save current position of the file
   long myOriginalFilePos = ftell(_ecFile);
   rewind();

   // Compute total number of messages, including sub-messages
   long numberOfSubsets;
   int err = 0;
   codes_handle* ecH = NULL;
   _obsCount = 0;
   while ((ecH = codes_handle_new_from_file(NULL,_ecFile,PRODUCT_BUFR,&err)) != NULL || err != CODES_SUCCESS)
   {
      if (ecH == NULL)
      {
         std::cout << "Error: unable to create handle for message" << std::endl;
         _obsCount = 0;
         break;
      }

      // Find out the number of subsets and update counter
      codes_get_long(ecH,"numberOfSubsets",&numberOfSubsets);
      _obsCount += numberOfSubsets;

      // Delete handle
      codes_handle_delete(ecH);
    }

   // Restore original position of the file
   fseek( _ecFile, myOriginalFilePos, SEEK_SET );

   return _obsCount;

//e code using BUFRDC
#if 0
   if( _obsCount < 1 )
   {
      if( _bufrFile )
      {
         long myOriginalFilePos = ftell( _bufrFile );
	 rewind();

         _obsCount = 0;
	 MvObs oneBufrMsg;
         while( ( oneBufrMsg = next() ) )
	    _obsCount += oneBufrMsg._bufrIn->subsetCount();

	 fseek( _bufrFile, myOriginalFilePos, SEEK_SET );
      }
   }
   return _obsCount;
#endif
}

//_________________________________________________________ searchMinMaxTime
void
MvObsSet::searchMinMaxTime()
{
   std::cout << "MvObsSet::searchMinMaxTime() -> not implemented yet" << std::endl;
   exit(0);

   // It has been computed
   if( _minMaxDone )
      return;

#ifdef MV_BUFRDC_TEST
   if( _bufrFile )
   {
      long myOriginalFilePos = ftell( _bufrFile );
      rewind();

      TDynamicTime msgTime;
      MvObs myObs;
      while( ( myObs = next() ) )
      {
         msgTime = myObs.msgTime();
         if( msgTime > _maxTime )
            _maxTime = msgTime;
         if( msgTime < _minTime )
            _minTime = msgTime;
      }

      fseek( _bufrFile, myOriginalFilePos, SEEK_SET );
   }
#endif

   _minMaxDone = true;
}

//_________________________________________________________ expand
void
MvObsSet::expand()
{
   if ( _unpacked )
      return;       // nothing to be done, it is already expanded

   if(useSkipExtraAttributes_)
   {
       codes_set_long(_ecH,"skipExtraKeyAttributes",1);
   }
   codes_set_long(_ecH,"unpack",1);
   _unpacked = true;
}

#ifdef METVIEW_PREPBUFR
//____________________________________________________________________ prepBufrFile
bool
MvObsSet::prepBufrFile()
{
   if( _isPrepBufrFile )
   {
      cout << "in MvObsSet::prepBufrFile()" << endl;
      cout << "BUFR file contains BUFR tables, processing..." << endl;

      //-- Create new PrepBUFR table files etc...
      _prepBufr = new MvPrepBufrPrep( *this );
      bool ok = _prepBufr->prepareAll();
      if( !ok )
      {
         cerr << "Errors: unable to process the PrepBUFR file" << endl;
         return false;
      }
   }
   return _isPrepBufrFile;
}
#endif

//____________________________________________________________________ minDate
#ifdef METVIEW
MvDate
MvObsSet :: minDate()
{
   searchMinMaxTime();
   double timfloat = 10000.0L*(double)_minTime.GetYear()
                     + 100.0L*(double)_minTime.GetMonth()
                     +        (double)_minTime.GetDay()
                     +        (double)_minTime.GetHour()/24.0L
                     +        ((double)_minTime.GetMin()+0.5L)/60.0L/24.0L; //rounded
   return MvDate( timfloat );
}
//____________________________________________________________________ maxDate
MvDate
MvObsSet :: maxDate()
{
   searchMinMaxTime();
   double timfloat = 10000.0L*(double)_maxTime.GetYear()
                     + 100.0L*(double)_maxTime.GetMonth()
                     +        (double)_maxTime.GetDay()
                     +        (double)_maxTime.GetHour()/24.0L
                     +        ((double)_maxTime.GetMin()+0.5L)/60.0L/24.0L; //rounded
   return MvDate( timfloat );
}
#endif

//___________________________________________________________________
//=================================================================== MvObsSetIterator
//___________________________________________________________________

MvObsSetIterator::MvObsSetIterator(MvObsSet& s) :
    _NoFiltersSet(true),
    useObsTime_(false),
    _SelectValueCount(0),
    _MsgTypeCount(0),
    _MsgSubtypeCount(0),
    _TimeFilterState(kTFS_notSet),
    _SelectState(SF_notSet),
    ObsSet(&s),
    observer_(0),
    filterProgressStep_(20)
{
}

//___________________________________________________________________
MvObsSetIterator::~MvObsSetIterator()
{
}

bool MvObsSetIterator::checkOptionSize(std::size_t num,const std::string& functionName)
{
    if(num >= MAX_FILTER_LIST_ARRAY_SIZE)
    {
        std::cerr << ">>> MvObsIterator::" + functionName + " array overflow!!!" << std::endl;
        return false;
    }
    return true;
}

int MvObsSetIterator::currentMessageNumber() const
{
    return (ObsSet?(static_cast<int>(ObsSet->_msgNumber)):-1);
}

MvObs MvObsSetIterator::operator() ( ENextReturn returnType )
{
   if( !current || ( returnType == NR_returnMsg ) || !current.Advance() )
    {
        if(observer_)
        {
            observer_->notifyObsIteratorProgress(currentMessageNumber());
        }
        next();
    }

    while(current && !AcceptedObs( current ) )
    {
        if( !current || ( returnType == NR_returnMsg ) || !current.Advance() )
        {
            //Notify the observer about the progress
            if(observer_)
            {
                observer_->notifyObsIteratorProgress(currentMessageNumber());
            }
            next();
        }
    }

    return current;
}

void MvObsSetIterator::setFilterProgressStep(int filterProgressStep)
{
    filterProgressStep_=filterProgressStep;
    assert(filterProgressStep_ >=0);
    if(filterProgressStep_ == 0)
       filterProgressStep_=1;
}


MvObs MvObsSetIterator::gotoMessage(long offset, int msgCnt)
{
    return ObsSet->gotoMessage(offset,msgCnt);
}

MvObs MvObsSetIterator::nextMessage()
{
    return ObsSet->next(false);
}

void MvObsSetIterator::next()
{
   current = ObsSet->next(false);

#ifdef METVIEW
//   static int change_me_with_debugger = 0;
//e  if( mars.debug && ( ( ObsSet->messageNumber() == 1 ) || change_me_with_debugger ) )
//e    current.printAllValues();
#endif
}

void MvObsSetIterator::setTime( const TDynamicTime& anObsTime )
{
   setTimeRange( anObsTime, anObsTime );
}

void MvObsSetIterator::setTimeRange( const TDynamicTime& anObsTime, short deltaInMinutes )
{
   fBeginTime = fEndTime = anObsTime;
   fBeginTime.ChangeByMinutes( -deltaInMinutes );
   fEndTime.ChangeByMinutes( deltaInMinutes );
   _TimeFilterState = kTFS_bothSet;
   _NoFiltersSet = false;
}

void MvObsSetIterator::setTimeRange( const TDynamicTime& aBeginTime, const TDynamicTime& anEndTime )
{
   fBeginTime = aBeginTime;
   fEndTime = anEndTime;
   _TimeFilterState = kTFS_bothSet;
   _NoFiltersSet = false;
}

//_____________________________________________________________ setTimeRangeWithoutDate
// add 941229/vk
//
// parameters: in format HHMM, i.e. 1200 == 12.00, 15 == 0.15, i.e. 100*hour+min!!!
//             values are normalized into range [0000..2400)
//_________________________________________
void
MvObsSetIterator::setTimeRangeWithoutDate( int aBegin, int anEnd )
{
   TDynamicTime aTime;  // date part is not used, so let's use today..
   int myBegin = aBegin;
   int myEnd   = anEnd;

   while( myBegin < 0 ) myBegin += 2400;
   aTime.SetTime( myBegin / 100, myBegin % 100, 0 );
   fBeginTime = aTime;

   while( myEnd >= 2400 ) myEnd -= 2400;
   aTime.SetTime( myEnd / 100, myEnd % 100, 0 );
   fEndTime = aTime;

   _TimeFilterState = kTFS_clockSet;
   _NoFiltersSet = false;
}

void MvObsSetIterator::setTimeRangeInSecWithoutDate( int aBegin, int anEnd )
{
   TDynamicTime aTime;  // date part is not used, so let's use today..
   int myBegin = aBegin;
   int myEnd   = anEnd;

   while( myBegin < 0 ) myBegin += 86400;
   int h=myBegin/3600;
   int m=(myBegin-h*3600)/60;
   int s=myBegin-h*3600-m*60;
   aTime.SetTime(h,m,s);
   fBeginTime = aTime;

   while( myEnd >= 86400 ) myEnd -= 86400;
   h=myEnd/3600;
   m=(myEnd-h*3600)/60;
   s=myEnd-h*3600-m*60;
   aTime.SetTime(h,m,s);
   fEndTime = aTime;

   _TimeFilterState = kTFS_clockSet;
   _NoFiltersSet = false;
}



//____________________________________________________________________
void MvObsSetIterator::setWmoBlock(int blockNumber)
{
    if(!checkOptionSize(wmoBlock_.size(),"setWmoBlock"))
        return;

    wmoBlock_.push_back(blockNumber);
    _NoFiltersSet = false;
}

void
MvObsSetIterator::setWmoStation(long wmoStation)
{
    if(!checkOptionSize(wmoStation_.size(),"setWmoStation"))
        return;

    wmoStation_.push_back(wmoStation);
    _NoFiltersSet = false;
}

void
MvObsSetIterator::select( const std::string& aDescriptor, double aValue )
{
   if( _SelectValueCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      std::cerr << ">>> MvObsIterator::select: array overflow!!!" << std::endl;
      return;
   }

   if( _SelectValueCount > 0 && aDescriptor != _SelectDescriptor )
   {
      std::cerr << ">>> MvObsIterator::select: changing the descriptor while building the list!!!" << std::endl;
   }

   _SelectDescriptor = aDescriptor;
   _SelectValue[ _SelectValueCount++ ] = aValue;
   _SelectState = SF_listSet;
   _NoFiltersSet = false;
}

void
MvObsSetIterator::selectRange( const std::string& aDescriptor, double firstValue, double secondValue )
{
   _SelectDescriptor = aDescriptor;
   _SelectValue[ 0 ] = firstValue < secondValue ? firstValue : secondValue;
   _SelectValue[ 1 ] = secondValue > firstValue ? secondValue : firstValue;
   _SelectValueCount = 2;
   _SelectState = SF_rangeSet;
   _NoFiltersSet = false;
}

void
MvObsSetIterator::excludeRange( const std::string& aDescriptor, double firstValue, double secondValue )
{
   _SelectDescriptor = aDescriptor;
   _SelectValue[ 0 ] = firstValue < secondValue ? firstValue : secondValue;
   _SelectValue[ 1 ] = secondValue > firstValue ? secondValue : firstValue;
   _SelectValueCount = 2;
   _SelectState = SF_excludeRangeSet;
   _NoFiltersSet = false;
}

void
MvObsSetIterator::setXSectionLine( const MvLocation& aStartPoint, const MvLocation& anEndPoint, float aDeltaInMeters )
{
   fXSectionLine.setLine( aStartPoint, anEndPoint );
   fXSectionLine.setMaxDelta( aDeltaInMeters );
   _NoFiltersSet = false;
}

void
MvObsSetIterator::setArea( const MvLocation& aCorner1, const MvLocation& aCorner2 )
{
   fArea.set( aCorner1, aCorner2 );
   _NoFiltersSet = false;
}

void
MvObsSetIterator::setMessageType( int aMsgType )
{
   if( _MsgTypeCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      std::cerr << ">>> MvObsIterator::setMessageType: array overflow!!!" << std::endl;
      return;
   }
   _MsgType[ _MsgTypeCount++ ] = aMsgType;
   _NoFiltersSet = false;
}

void MvObsSetIterator::setMessageSubtype( int aMsgSubtype )
{
   if( _MsgSubtypeCount >= MAX_FILTER_LIST_ARRAY_SIZE )
   {
      std::cerr << ">>> MvObsIterator::setMessageSubtype: array overflow!!!" << std::endl;
      return;
   }
   _MsgSubtype[ _MsgSubtypeCount++ ] = aMsgSubtype;
   _NoFiltersSet = false;
}

void MvObsSetIterator::setMessageNumber(int num)
{
    if(!checkOptionSize(messageNumber_.size(),"setMessageNumber"))
        return;

    messageNumber_.push_back(num);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setEditionNumber(int edition)
{
    if(!checkOptionSize(editionNumber_.size(),"setEditionNumber"))
        return;

    editionNumber_.push_back(edition);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setOriginatingCentre(int centre)
{
    if(!checkOptionSize(originatingCentre_.size(),"setOriginatingCentre"))
        return;

    originatingCentre_.push_back(centre);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setOriginatingCentreAsStr(const std::string& centre)
{
    if(!checkOptionSize(originatingCentreStr_.size(),"setOriginatingCentreAsStr"))
        return;

    originatingCentreStr_.push_back(centre);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setOriginatingSubCentre(int subCentre)
{
    if(!checkOptionSize(originatingSubCentre_.size(),"setOriginatingSubCentre"))
        return;

    originatingSubCentre_.push_back(subCentre);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setMasterTableVersion(int masterTable)
{
    if(!checkOptionSize(masterTableVersion_.size(),"setMasterTableVersion"))
        return;

    masterTableVersion_.push_back(masterTable);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setLocalTableVersion(int localTable)
{
    if(!checkOptionSize(localTableVersion_.size(),"setLocalTableVersion"))
        return;

    localTableVersion_.push_back(localTable);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setHeaderIdent(const std::string& headerIdent)
{
    if(!checkOptionSize(headerIdent_.size(),"setHeaderIdent"))
        return;

    headerIdent_.push_back(simplified(headerIdent));
    _NoFiltersSet = false;
}

void MvObsSetIterator::setIdentKey(const std::string& identKey)
{
    identKey_ = simplified(identKey);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setIdentValue(const std::string& identVal)
{
    if(!checkOptionSize(identValue_.size(),"setIdentValue"))
        return;

    identValue_.push_back(identVal);
    _NoFiltersSet = false;
}

void MvObsSetIterator::setMessageRdbtype(int rdbType)
{
    if(!checkOptionSize(rdbType_.size(),"setMessageRdbtype"))
        return;

    rdbType_.push_back(rdbType);
    _NoFiltersSet = false;
}

//____________________________________________________________________
float
MvObsSetIterator::distanceFromXSectionLine( const MvLocation& aPoint )
{
   if( fXSectionLine.startPoint().latitude() == MISSING_LOC_VALUE )
      return MISSING_LOC_VALUE;

   return fXSectionLine.deltaInMeters( aPoint );
}

//____________________________________________________________________
bool
MvObsSetIterator::TimeOk( MvObs *anObs ) const
{
   switch( _TimeFilterState )
   {
      case kTFS_notSet:
      break;

      case kTFS_clockSet:
      {
         long obsTime;
         if( useObsTime_ )
         {
             //We need time from the  data section so we need to expand the message
             anObs->expand();
             obsTime = anObs->obsTime().ClockInSeconds();  //-- need to decode => slow
         }
         else
         {
            obsTime = anObs->msgTime().ClockInSeconds();  //-- use metadata => fast
         }
         long time1 = fBeginTime.ClockInSeconds();
         long time2 = fEndTime.ClockInSeconds();

         if( time1 <= time2 )        // e.g.  12-18
         {
            if( obsTime < time1 || obsTime > time2 )
               return false;
         }
         else    // time1 > time2       e.g.  21-03
         {
            if( obsTime < time1 && obsTime > time2 )
               return false;
         }
      }
      break;

      case kTFS_bothSet:
      {
         TDynamicTime myTime;
         if( useObsTime_ )
         {
            //We need time from the  data section so we need to expand the message
            anObs->expand();
            myTime = anObs->obsTime();  //-- time from obs => decode first => slow
         }
         else
         {
             myTime = anObs->msgTime();  //-- time from sec1 => no decode => fast
         }

         if( myTime < fBeginTime || myTime > fEndTime )
            return false;
      }
      break;
   }

   return true;
}

//____________________________________________________________________
bool
MvObsSetIterator::WmoBlockOk(MvObs *anObs) const
{
    if(!wmoBlock_.empty())
    {
        //we need to expand the message
        anObs->expand();
        for(std::size_t i = 0; i < wmoBlock_.size(); i++ )
        {
            if(anObs->WmoBlockNumber() ==  wmoBlock_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::WmoStationOk(MvObs *anObs) const
{
    if(!wmoStation_.empty())
    {
        //we need to expand the message
        anObs->expand();
        for(std::size_t i = 0; i < wmoStation_.size(); i++ )
        {
            if(anObs->WmoIdentNumber() == wmoStation_[i])
                return true;
        }
        return false;
    }
    return true;
}

//____________________________________________________________________
bool
MvObsSetIterator::WithinXSectionLine( MvObs *anObs ) const
{
   if( fXSectionLine.maxDelta() < 0 )    // not set ?
      return true;

   //We need lat/lon from the  data section so we need to expand the message
   anObs->expand();

   if( fXSectionLine.withinDelta( anObs->location() ) )
      return true;
   else
      return false;
}

//____________________________________________________________________
bool
MvObsSetIterator::InsideArea( MvObs *anObs ) const
{
   if( fArea.lowerLeft().latitude() == MISSING_LOC_VALUE )
      return true;
   else
   {
      //We need lat/lon from the  data section so we need to expand the message
      anObs->expand();
      return fArea.inside( anObs->location() );
   }
}

//____________________________________________________________________
bool
MvObsSetIterator::msgTypeOk( MvObs *anObs ) const
{
   if( _MsgTypeCount < 1 )
      return true;

   for( int i = 0; i < _MsgTypeCount; i++ )
      if( anObs->messageType() == _MsgType[ i ] )
         return true;

  return false;
}

bool MvObsSetIterator::msgSubtypeOk( MvObs *anObs ) const
{
   if( _MsgSubtypeCount < 1 )
      return true;

   for( int i = 0; i < _MsgSubtypeCount; i++ )
      if( anObs->messageSubtype() == _MsgSubtype[ i ] )
         return true;

  return false;
}

bool MvObsSetIterator::messageNumberOk(MvObs *anObs) const
{
    if(!messageNumber_.empty())
    {
        for(std::size_t i = 0; i < messageNumber_.size(); i++ )
        {
            if(currentMessageNumber() == messageNumber_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::editionNumberOk(MvObs *anObs) const
{
    if(!editionNumber_.empty())
    {
        for(std::size_t i = 0; i < editionNumber_.size(); i++ )
        {
            if(anObs->editionNumber() ==  editionNumber_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::originatingCentreOk(MvObs *anObs) const
{
    if(!originatingCentre_.empty())
    {
        for(std::size_t i = 0; i < originatingCentre_.size(); i++ )
        {
            if(anObs->originatingCentre() ==  originatingCentre_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::originatingCentreAsStrOk(MvObs *anObs) const
{
    if(!originatingCentreStr_.empty())
    {
        for(std::size_t i = 0; i < originatingCentreStr_.size(); i++ )
        {
            if(anObs->originatingCentreAsStr() ==  originatingCentreStr_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::originatingSubCentreOk(MvObs *anObs) const
{
    if(!originatingSubCentre_.empty())
    {
        for(std::size_t i = 0; i < originatingSubCentre_.size(); i++ )
        {
            if(anObs->originatingSubCentre() ==  originatingSubCentre_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::masterTableVersionOk(MvObs *anObs) const
{
    if(!masterTableVersion_.empty())
    {
        for(std::size_t i = 0; i < masterTableVersion_.size(); i++ )
        {
            if(anObs->masterTableVersion() ==  masterTableVersion_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::localTableVersionOk(MvObs *anObs) const
{
    if(!localTableVersion_.empty())
    {
        for(std::size_t i = 0; i < localTableVersion_.size(); i++ )
        {
            if(anObs->localTableVersion() ==  localTableVersion_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::headerIdentOk(MvObs *anObs) const
{
    if(!headerIdent_.empty())
    {
        for(std::size_t i = 0; i < headerIdent_.size(); i++ )
        {
            if(simplified(anObs->headerIdent()) ==  headerIdent_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::identValueOk(MvObs *anObs) const
{
    if(!identValue_.empty())
    {
        //We need this key from the data section so we need to expand the message
        anObs->expand();
        for(std::size_t i = 0; i < identValue_.size(); i++ )
        {
            if(anObs->stringValue(identKey_,1) ==  identValue_[i])
                return true;
        }
        return false;
    }
    return true;
}

bool MvObsSetIterator::msgRdbtypeOk(MvObs *anObs) const
{
    if(!rdbType_.empty())
    {
        for(std::size_t i = 0; i < rdbType_.size(); i++ )
        {
            if(anObs->messageRdbtype() ==  rdbType_[i])
                return true;
        }
        return false;
    }
    return true;
}

//____________________________________________________________________
bool
MvObsSetIterator::selectOk( MvObs *anObs ) const
{
   if( _SelectState == SF_notSet )
      return true;

   //We can assume that this key is from the  data section so we need to expand the message
   anObs->expand();

   double myValue = anObs->valueC( _SelectDescriptor );
   if( myValue == kBufrMissingValue )
      return false;

   switch( _SelectState )
   {
      case SF_notSet:
      break;

      case SF_listSet:
      {
         for( int i = 0; i < _SelectValueCount; i++ )
            if( myValue == _SelectValue[ i ] )
               return true;
      }
      return false;

      case SF_rangeSet:
         if( myValue < _SelectValue[ 0 ] || myValue > _SelectValue[ 1 ] )
            return false;
      break;

      case SF_excludeRangeSet:
         if( myValue >= _SelectValue[ 0 ] && myValue <= _SelectValue[ 1 ] )
            return false;
      break;
   }

   return true;
}

//____________________________________________________________________
bool
MvObsSetIterator::AcceptedObs(MvObs& anObs,bool skipPreFilterCond) const
{
    if(!anObs)
        return false;

    if( _NoFiltersSet )
       return true;

    //Prefilter conditions are fully based on the BUFR header
    if(!skipPreFilterCond)
    {
        //Index of the message within the bufr file
        if(!messageNumberOk(&anObs))
            return false;

        //BUFR edition
        if(!editionNumberOk(&anObs))
            return false;

        //Edition parameters
        if(!originatingCentreOk(&anObs))
            return false;

        if(!originatingCentreAsStrOk(&anObs))
            return false;

        if(!originatingSubCentreOk(&anObs))
            return false;

        if(!masterTableVersionOk(&anObs))
            return false;

        if(!localTableVersionOk(&anObs))
            return false;

        //Type parameters - fully based on the BUFR header
        if(!msgTypeOk( &anObs))
            return false;

        if(!msgSubtypeOk( &anObs))
            return false;

        if(!msgRdbtypeOk( &anObs))
            return false;
    }

    //"ident" key defined in ecmwf (centre=98) header
    if(!headerIdentOk(&anObs))
        return false;

    //User defined identifier from the data section
    if(!identValueOk(&anObs))
        return false;

   if( ! TimeOk( &anObs ) )            // from Section 1: no decoding required
      return false;

   if( ! WmoBlockOk( &anObs ) )        // others require decoding of the msg
      return false;
   if( ! WmoStationOk( &anObs ) )
      return false;

   if( ! selectOk( &anObs ) )
      return false;

   if( ! WithinXSectionLine( &anObs ) )
      return false;

   if( ! InsideArea( &anObs ) )
      return false;

   return true;
}


//_____________________________________________________________ operator<<
std::ostream& operator<< (std::ostream& aStream, const MvObsSetIterator& aFilter )
{
   int i;

   aStream << "Observation Filter values set:\n";
   if( aFilter._NoFiltersSet )
   {
      aStream << "   No filter values set!";
      aStream << std::endl;
   }
   else
   {
      if( aFilter._TimeFilterState != kTFS_notSet )
      {
         aStream << "   Timerange: ";
         switch( aFilter._TimeFilterState )
         {
            case kTFS_notSet:
               aStream << "[not set!]";
            break;

            case kTFS_clockSet:
               aStream << aFilter.fBeginTime.GetHour() << ".";
               aStream.width(2); aStream.fill('0');
               aStream << aFilter.fBeginTime.GetMin();
               if( aFilter.fBeginTime != aFilter.fEndTime )
               {
                  aStream << " - " << aFilter.fEndTime.GetHour() << ".";
                  aStream.width(2); aStream.fill('0');
                  aStream << aFilter.fEndTime.GetMin();
               }
            break;

            case kTFS_bothSet:
               aStream << aFilter.fBeginTime;
               if( aFilter.fBeginTime != aFilter.fEndTime )
                  aStream << " - " << aFilter.fEndTime;
            break;
         }
         aStream << std::endl;
      }

      if( aFilter._MsgTypeCount > 0 )
      {
         aStream << "   Message types: ";
         for( i=0; i < aFilter._MsgTypeCount; i++ )
            aStream << " " << aFilter._MsgType[ i ];
         aStream << std::endl;
      }

      if( aFilter._MsgSubtypeCount > 0 )
      {
         aStream << "   Message subtypes: ";
         for( i=0; i < aFilter._MsgSubtypeCount; i++ )
            aStream << " " << aFilter._MsgSubtype[ i ];
         aStream << std::endl;
      }

      if(!aFilter.wmoBlock_.empty())
      {
         aStream << "   WMO Blocks:";
         for(std::size_t i=0; i < aFilter.wmoBlock_.size(); i++ )
            aStream << " " << aFilter.wmoBlock_[i];
         aStream << std::endl;
      }

      if(!aFilter.wmoStation_.empty())
      {
         aStream << "   WMO Stations:";
         for(std::size_t i=0; i < aFilter.wmoStation_.size(); i++ )
             aStream << " " << aFilter.wmoStation_[i];
         aStream << std::endl;
      }

      if( aFilter._SelectState != SF_notSet )
      {
         aStream << "   Select ";
         switch( aFilter._SelectState )
         {
            case SF_notSet:
               aStream << "by values/range: [not set!]";
            break;

            case SF_listSet:
               aStream << "by values: ";
            break;

            case SF_rangeSet:
               aStream << "by range: ";
            break;
            case SF_excludeRangeSet:
               aStream << "by excluding range: ";
            break;
         }

      for( i=0; i < aFilter._SelectValueCount; i++ )
         aStream << aFilter._SelectValue[ i ] << " ";
      aStream << "(descr. " << aFilter._SelectDescriptor.c_str() << ")";
      aStream << std::endl;
   }

   if( aFilter.fXSectionLine.startPoint().latitude() != MISSING_LOC_VALUE )
   {
      aStream << "   Cross Section Line: " << aFilter.fXSectionLine;
      aStream << std::endl;
   }

   if( aFilter.fArea.lowerLeft().latitude() != MISSING_LOC_VALUE )
   {
      aStream << "   Area: " << aFilter.fArea;
      aStream << std::endl;
    }
   // aStream << endl;  // calling method should add the final 'endl'!
   }

   return aStream;
}

//________________________________________________________ simplified
std::string simplified(const std::string& str)
{
    std::size_t pos1=str.find_first_not_of(" ");
    std::size_t pos2=str.find_last_not_of(" ");

    if(pos1 != std::string::npos && pos2 != std::string::npos && pos2 >= pos1)
    {
        return str.substr(pos1,pos2-pos1+1);
    }
    return std::string();
}
