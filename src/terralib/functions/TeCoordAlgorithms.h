//***********************************************************************
//      TerraLib is a GIS Classes and Functions Library that 
//      strongly explores Spatial Database Technologies 
//
//      Copyright © 2002 INPE and Tecgraf/PUC-Rio. 
//
//      This library is free software; you can redistribute it 
//      and/or modify it under the terms of the GNU Lesser General 
//      Public License as published by the Free Software Foundation
//      version 2.1.(http://www.opensource.org/licenses/lgpl-license.php)
//
//      
//
//      Send questions or suggestions about the TerraLib Project 
//      to terralib@dpi.inpe.br .
//**************************************************************************//
/*! \file TeCoordAlgorithms.h
    This file contains some algorithms to deal with geographical coordinates
*/
#ifndef TeCoordAlgs_H
#define TeCoordAlgs_H

#include <string>
using namespace std;
/*! 
   \fn bool TeLongDMS2DD(string lado, short& dg, short& mn, float& sc, double& grauDec) 
   \brief Transform a Longitude Coordinate in degrees, minutes and seconds to decimal degrees
   \param lado a character.
   \param dg dregrees value
   \param mn minutes value
   \param sc seconds value
   \return the decimal degree value
*/
bool TeLongDMS2DD(string lado, short& dg, short& mn, float& sc, double& grauDec);

/*! 
   \fn bool TeLongDMS2DD(string lado, short& dg, short& mn, float& sc, double& grauDec) 
   \brief Transform a Latitude Coordinate in degrees, minutes and seconds to decimal degrees
   \param lado a character.
   \param dg dregrees value
   \param mn minutes value
   \param sc seconds value
   \return the decimal degree value
*/
bool TeLatDMS2DD(string lado, short& dg, short& mn, float& sc, double& grauDec);

#endif
