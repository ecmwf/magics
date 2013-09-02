/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2004 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/

#include "TeDecoderSmartMem.h"

#include "TeException.h"
#include "TeAgnostic.h"


TeDecoderSmartMem::TeDecoderSmartMem ()
{ 
  initVars();
  
  params_.decoderIdentifier_ = "SMARTMEM";
};


TeDecoderSmartMem::TeDecoderSmartMem ( const TeRasterParams& par )
{
  initVars();
  
  params_ = par; 
  params_.decoderIdentifier_ = "SMARTMEM"; 
};



TeDecoderSmartMem::~TeDecoderSmartMem ()
{
  clear();
}

void TeDecoderSmartMem::init()
{
  clear();
  
  /* Creating the new data structures */

  if( ( params_.nBands() > 0 ) && 
      ( params_.nlines_ > 0 ) &&
      ( params_.ncols_ > 0 ) ) {
  
    std::vector< unsigned int > tiles_sizes;
    
    for( int band_index = 0 ; band_index < params_.nBands() ; 
      ++band_index ) {
      
      tiles_sizes.push_back( params_.elementSize( band_index ) * params_.ncols_ );
    }

    if( man_manager_.reset( params_.nBands(), params_.nlines_, 
      tiles_sizes, TeRasterMemManager::AutoMemPol ) ) {
      
      if (params_.mode_ == 'c' || params_.mode_ == 'w') // creating a new file
      {
        params_.status_ = TeRasterParams::TeReadyToWrite;
      } else if (params_.mode_ == 'r') {
        params_.status_ = TeRasterParams::TeReadyToRead;      
      }
      
      updateFuncPtrVectors();
      dummyFill();    
    } else {
      clear();
    }
  }
}

bool TeDecoderSmartMem::clear()
{
  man_manager_.clear();
  
  if( getelement_ptrs_vec_ != 0 ) {
    delete[] getelement_ptrs_vec_;
    getelement_ptrs_vec_ = 0;
  }
  
  if( setelement_ptrs_vec_ != 0 ) {
    delete[] setelement_ptrs_vec_;
    setelement_ptrs_vec_ = 0;
  }
  
  params_.status_= TeRasterParams::TeNotReady;
  
  return true;
}


void TeDecoderSmartMem::initVars()
{ 
  getelement_ptrs_vec_ = 0;
  setelement_ptrs_vec_ = 0;
};


void TeDecoderSmartMem::updateFuncPtrVectors()
{
  if( params_.nBands() > 0 ) {
    getelement_ptrs_vec_ = new GetEleFunctPtrT[ params_.nBands() ];
    if( getelement_ptrs_vec_ == 0 ) {
      throw TeException( UNKNOWN_ERROR_TYPE, 
        "Unable to allocate GET function pointers vector", false );
    }

    setelement_ptrs_vec_ = new SetEleFunctPtrT[ params_.nBands() ];
    if( setelement_ptrs_vec_ == 0 ) {
      throw TeException( UNKNOWN_ERROR_TYPE, 
        "Unable to allocate SET function pointers vector", false );
    }
    
    for( int band = 0 ; band < params_.nBands() ; ++band ) {
      switch ( params_.dataType_[ band ] ) {
        case (TeUNSIGNEDCHAR):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeUNSIGNEDCHAR;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeUNSIGNEDCHAR;
            break;
          }
        case (TeCHAR) :
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeCHAR;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeCHAR;          
            break;
          }
        case (TeUNSIGNEDSHORT):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeUNSIGNEDSHORT;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeUNSIGNEDSHORT;
            break;
          }
        case (TeSHORT):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeSHORT;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeSHORT;
            break;
          }
        case (TeINTEGER):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeINTEGER;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeINTEGER;
            break;
          }
        case (TeUNSIGNEDLONG):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeUNSIGNEDLONG;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeUNSIGNEDLONG;
            break;
          }
        case (TeLONG):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeLONG;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeLONG;
            break;
          }
        case (TeFLOAT):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeFLOAT;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeFLOAT;
            break;
          }
        case (TeDOUBLE):
          {
            getelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::getElement_TeDOUBLE;
            setelement_ptrs_vec_[ band ] = 
              &TeDecoderSmartMem::setElement_TeDOUBLE;
            break;
          }
        default:
          {
            TEAGN_LOG_AND_THROW( "Invalid raster data type" );
            break;
          }
      }
    }
  }
}


void TeDecoderSmartMem::dummyFill()
{
  TEAGN_DEBUG_CONDITION( 
    ( params_.status_ != TeRasterParams::TeNotReady),
    "Raster not Ready" );
  
  /* Checking if dummy fill is required */
  
  if( ! params_.useDummy_ ) {
    return;
  }
  
  /* Dummy fill */
      
  const unsigned int nlines = (unsigned int )params_.nlines_;
  const unsigned int ncols = (unsigned int )params_.ncols_;

  unsigned int line = 0;
  unsigned int col = 0;
  double double_dummy_value = 0;
  
  for( int band = 0 ; band < params_.nBands() ; ++band ) {
    if( params_.useDummy_ ) {
      double_dummy_value = params_.dummy_[ band ];
    } else {
      double_dummy_value = 0;
    }
  
    switch ( params_.dataType_[ band ] ) {
      case (TeUNSIGNEDCHAR):
        {
          unsigned char dummy_value = (unsigned char)double_dummy_value;
          unsigned char* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (unsigned char*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeCHAR) :
        {
          char dummy_value = (char)double_dummy_value;
          char* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (char*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeUNSIGNEDSHORT):
        {
          unsigned short dummy_value = (unsigned short)double_dummy_value;
          unsigned short* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (unsigned short*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeSHORT):
        {
          short dummy_value = (short)double_dummy_value;
          short* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (short*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeINTEGER):
        {
          int dummy_value = (int)double_dummy_value;
          int* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (int*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeUNSIGNEDLONG):
        {
          unsigned long dummy_value = (unsigned long)double_dummy_value;
          unsigned long* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (unsigned long*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeLONG):
        {
          long dummy_value = (long)double_dummy_value;
          long* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (long*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeFLOAT):
        {
          float dummy_value = (float)double_dummy_value;
          float* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (float*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = dummy_value;
            }
          }
          
          break;
        }
      case (TeDOUBLE):
        {
          double* line_ptr = 0;
          
          for( line = 0 ; line < nlines ; ++line ) {
            line_ptr = (double*) man_manager_.getTilePointer( band, 
              line );
          
            for( col = 0 ; col < ncols ; ++col ) {
              line_ptr[ col ] = double_dummy_value;
            }
          }
          
          break;
        }
      default:
        {
          TEAGN_LOG_AND_THROW( "Invalid raster data type" );
          break;
        }
    }
  }  
}


inline void TeDecoderSmartMem::setElement_TeUNSIGNEDCHAR( const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (unsigned char*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (unsigned char)val;
}


inline void TeDecoderSmartMem::setElement_TeCHAR(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (char*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (char)val;  
}


inline void TeDecoderSmartMem::setElement_TeUNSIGNEDSHORT(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (unsigned short*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (unsigned short)val;  
}


inline void TeDecoderSmartMem::setElement_TeSHORT(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (short*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (short)val;
}


inline void TeDecoderSmartMem::setElement_TeINTEGER(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (int*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (int)val;
}


inline void TeDecoderSmartMem::setElement_TeUNSIGNEDLONG(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (unsigned long*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (unsigned long)val;  
}


inline void TeDecoderSmartMem::setElement_TeLONG(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (long*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (long)val;  
}


inline void TeDecoderSmartMem::setElement_TeFLOAT(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (float*)man_manager_.getTilePointer( band, lin ) ) + col ) = 
    (float)val;    
}


inline void TeDecoderSmartMem::setElement_TeDOUBLE(const int& col, 
  const int& lin, const double& val, const int& band )
{
  *( ( (double*)man_manager_.getTilePointer( band, lin ) ) + col ) = val;    
}


inline void TeDecoderSmartMem::getElement_TeUNSIGNEDCHAR( const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (unsigned char*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeCHAR(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (char*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeUNSIGNEDSHORT(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (unsigned short*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeSHORT(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (short*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeINTEGER(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (int*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeUNSIGNEDLONG(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (unsigned long*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeLONG(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (long*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeFLOAT(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = (double)( *( ( (float*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}


inline void TeDecoderSmartMem::getElement_TeDOUBLE(const int& col, 
  const int& lin, double& val, const int& band )
{
  val = ( *( ( (double*)man_manager_.getTilePointer( band, 
    lin ) ) + col ) );
}

