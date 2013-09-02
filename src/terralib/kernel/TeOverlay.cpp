/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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

// TerraLib's include
#include "TeOverlay.h"
#include "TeOverlayUtils.h"
#include "TeIntersector.h"
#include "TeGeometryAlgorithms.h"
#include "TeFragmentation.h"
#include "TeRTree.h"

//---------------- Overlay operations ----------------//

// Operacao de uniao otimizada: consome memoria!
bool TeOVERLAY::TeUnion(TePolygonSet& polsIn, TePolygonSet& polsOut, const bool& makeCopy, const bool fixOrientation)
{
// limpa a lista de saida
	polsOut.clear();

// verifica se e necessario realizar uma copia dos polígonos
	TePolygonSet polygonSetIn;

	if(makeCopy)
	{
		if(!TeOVERLAY::TeClonePolygonSet(polsIn, polygonSetIn))
			return false;
        //polygonSetIn.copyElements(polsIn);
	}
	else
	{
		polygonSetIn = polsIn;
	}

	if(polygonSetIn.empty())
		return false;

	if(polygonSetIn.size() == 1)
	{
		polsOut.copyElements(polygonSetIn);

		return true;
	}

// verifica a orientacao de cada linha se necessario: aneis externos devem estar no sentido horario e os internos devem estar no sentido anti-horario
	if(fixOrientation)
		TeFixOrientation(polygonSetIn, TeCOUNTERCLOCKWISE, TeCLOCKWISE);

// cria uma arvore que indexara os segmentos de cada linha e
// chama uma rotina que calcula os pontos de intersecao e ja indexa os segmentos
	TeINTERSECTOR2::TeSegmentRTree segmentTree(polygonSetIn.box());

	TeINTERSECTOR2::TeVectorBoundaryIP report;

	TeINTERSECTOR2::TeIntersection(polygonSetIn, segmentTree, report);

// fragmenta as linhas
	TeLineSet fragments;
	TeLineSet boundaryFragments;
	vector<pair<unsigned int, unsigned int> > fragmentsIds;
	vector<pair<unsigned int, unsigned int> > boundaryFragmentsIds;

	TeFragmentBoundary(polygonSetIn, report, boundaryFragments, boundaryFragmentsIds, fragments, fragmentsIds);

	report.clear();

	short mask  = TeUNKNOWNPOSITION;

// os fragmentos de fronteira ja sao separados
	TeLineIndex boundaryFragmentsIndex;
	unsigned int i;
	if(boundaryFragments.size())
	{
		mask = TeBOUNDARY;

		unsigned int boundaryFragmentsSize = boundaryFragments.size();

		for(i = 0; i < boundaryFragmentsSize; ++i)
			boundaryFragmentsIndex.insert(TeLineIndex::value_type(boundaryFragments[i][0], pair<unsigned int, TeLine2D>(i, boundaryFragments[i])));
	}

// escolhe os fragmentos que estiverem localizados dentro de cada polígono
	short locationFragments  = TeOUTSIDE;	

	TeLineIndex fragmentsIndex;

	vector<TeLinearRing> rings;

	TeRtreeGetFragments(polygonSetIn, segmentTree, fragments, fragmentsIds, locationFragments, mask, fragmentsIndex, rings);

	// Tenta eliminar fragmentos de fronteira no mesmo sentido, deixando apenas um nesta direcao
	TeRemoveSameBoundaryFragments(boundaryFragmentsIndex);

	// Tenta eliminar fragmentos sobre fronteira opostos
	TeRemoveOpositeBoundaryFragments(boundaryFragmentsIndex);

	//--remover do boundaryfragments os fragmentos removidos do boundaryFragmentsIndex
	TeRtreeRemoveFragments(polygonSetIn, segmentTree, boundaryFragmentsIndex, boundaryFragmentsIds, TeINSIDE, mask, rings);
	segmentTree.clear();

// Faz um merge dos fragmentos de fronteira para o índice de fragmentos normal
	TeJoinFragments(fragmentsIndex, boundaryFragmentsIndex);

// monta os poligonos a partir dos fragmentos
	bool returnValue = TeMergeFragments(fragmentsIndex, rings, true);

	vector<TeLinearRing> ringsOut;

	bool resultSplit = true;

	if(TeOVERLAY::TeSplitRings(rings, ringsOut) == false)
	{
		ringsOut.clear();

		ringsOut = rings;

		resultSplit = false;
	}

// separate holes from islands
	vector<TeLinearRing> holes;

	bool result = TeClassifyRings(ringsOut, polsOut, holes);

// mount topology		
	if((polsOut.size() == 0) && (holes.size() == 0))
	{
		return false;	// Na uniao deve haver a formacao de poligonos	
	}
	
	bool mountResult = TeMountTopology(polsOut, holes);

	return (returnValue && result &&  mountResult && resultSplit);
}


// Operacao geral de conjunto: uniao, intersecao e diferenca
bool TeOverlay(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const short& operation, const bool& makeCopy, const bool fixOrientation)
{
// limpa a saida
	vecPolsOut.clear();
	resultVec.clear();

// verifica se a lista dos poligonos vermelhos esta vazia
	unsigned int numBluePols = bluePols.size();
	unsigned int i;

	if(redPols.empty())
	{
// se os poligonos vermelhos nao foram informados, no caso da uniao tenta copiar os azuis para o vetor de saida
		if(operation == TeUNION)
		{
			for(i = 0; i < numBluePols; ++i)
			{
				TePolygonSet bluePolsInAux;

				if(!TeOVERLAY::TeClonePolygonSet(bluePols[i], bluePolsInAux))
					return false;

				//bluePolsInAux.copyElements(bluePols[i]);

// se nao existir tambem o poligono azul, entao retirn falso
                vecPolsOut.push_back(bluePolsInAux);
				resultVec.push_back	(true);
			}

			if(vecPolsOut.empty())
				return false;

			return true;
		}
		
// TeDIFFERENCE ou TeINTERSECTION: resultado eh vazio, logo ja retorna true
		return true;
	}


// Defines location to choose fragments
	short locationRedFragments  = TeINSIDE;	
	short locationBlueFragments  = TeINSIDE;

	TeOVERLAY::TeChooseBoundaryLocation(operation, locationRedFragments, locationBlueFragments);

// verifica se e necessario realizar uma copia dos poligonos
	TePolygonSet redPolsIn;
	vector<TePolygonSet> bluePolsIn;

	if(makeCopy)
	{
		if(!TeOVERLAY::TeClonePolygonSet(redPols, redPolsIn))
			return false;

		//redPolsIn.copyElements(redPols);
		
		for(i = 0; i < numBluePols; ++i)
		{
			TePolygonSet bluePolsInAux;

			if(!TeOVERLAY::TeClonePolygonSet(bluePols[i], bluePolsInAux))
				return false;

			//bluePolsInAux.copyElements(bluePols[i]);

			bluePolsIn.push_back(bluePolsInAux);
		}
	}
	else
	{
		redPolsIn = redPols;
		
		bluePolsIn = bluePols;
	}


// verifica a orientacao de cada linha se necessario: aneis externos devem estar no sentido horario e os internos devem estar no sentido anti-horario
	if(fixOrientation)
	{	
		if(operation == TeINTERSECTION || operation == TeUNION)
		{
			TeOVERLAY::TeFixOrientation(redPolsIn, TeCOUNTERCLOCKWISE, TeCLOCKWISE);

			for(i = 0; i < numBluePols; ++i)
				TeOVERLAY::TeFixOrientation(bluePolsIn[i], TeCOUNTERCLOCKWISE, TeCLOCKWISE);
		}
		else
		{
			TeOVERLAY::TeFixOrientation(redPolsIn, TeCOUNTERCLOCKWISE, TeCLOCKWISE);

			for(i = 0; i < numBluePols; ++i)
				TeOVERLAY::TeFixOrientation(bluePolsIn[i], TeCLOCKWISE, TeCOUNTERCLOCKWISE);
		}
	}


// cria uma arvore que indexara os segmentos dos poligonos vermelhos
	TeINTERSECTOR2::TeSegmentRTree redSegmentTree(redPolsIn.box());
	TeINTERSECTOR2::TeIndexPolygonSet(redPolsIn, redSegmentTree);

	bool resultValue = true;

// para cada conjunto de poligonos azuis, faz a operacao dele com os vermelhos
	for(i = 0; i < numBluePols; ++i)
	{
// verifica se tem poligonos no polygonset azul
		if(bluePolsIn[i].empty())
		{
			if((operation == TeUNION) || (operation == TeDIFFERENCE))
			{
				vecPolsOut.push_back(redPolsIn);
				resultVec.push_back(true);	// mario - 2006-03
			}
			else
			{
                TePolygonSet emptySet;

                vecPolsOut.push_back(emptySet);
				resultVec.push_back(true);	// mario - 2006-03
			}

			continue;
		}

// Indexa os segmentos azuis
		TeINTERSECTOR2::TeSegmentRTree blueSegmentTree(bluePolsIn[i].box());
		TeINTERSECTOR2::TeIndexPolygonSet(bluePolsIn[i], blueSegmentTree);		

// calcula os potos de intersecao dos vermelhos com os azuis
		TeINTERSECTOR2::TeVectorBoundaryIP report;
		TeINTERSECTOR2::TeIntersection(redPolsIn, redSegmentTree, bluePolsIn[i], report);

// fragmenta as linhas vermelhas
		TeLineSet redFragments;
		TeLineSet redBoundaryFragments;
		vector<pair<unsigned int, unsigned int> > redFragmentsIds;
		vector<pair<unsigned int, unsigned int> > boundaryFragmentsIds;

		TeFragmentBoundary(redPolsIn, report, redBoundaryFragments, boundaryFragmentsIds, redFragments, redFragmentsIds);

		short mask  = TeUNKNOWNPOSITION;

// os fragmentos de fronteira ja sao separados
		TeOVERLAY::TeLineIndex boundaryFragmentsIndex;

		unsigned int boundaryId_ = 0;

		if(redBoundaryFragments.size())
		{
			mask = TeBOUNDARY;

			unsigned int boundaryFragmentsSize = redBoundaryFragments.size();

			for(unsigned int ti = 0; ti < boundaryFragmentsSize; ++ti)
			{
				boundaryFragmentsIndex.insert(TeOVERLAY::TeLineIndex::value_type(redBoundaryFragments[ti][0], pair<unsigned int, TeLine2D>(boundaryId_, redBoundaryFragments[ti])));
				++boundaryId_;
			}
		}

// escolhe os fragmentos vermelhos que estiverem localizados dentro/fora (dependendo da oparacao) dos poligonos azuis
		vector<TeLinearRing> rings;

		TeOVERLAY::TeLineIndex fragmentsIndex;

		TeOVERLAY::TeRtreeGetFragments(bluePolsIn[i], blueSegmentTree, redFragments, locationRedFragments, mask, fragmentsIndex, rings);

// inverte a ordem dos pontos de intersecao para fragmentar as linhas azuis
		TeINTERSECTOR2::TeVectorBoundaryIP::iterator it     = report.begin();
		TeINTERSECTOR2::TeVectorBoundaryIP::iterator it_end = report.end();
		
		while(it != it_end)
		{
			swap(it->bluePolNum_, it->redPolNum_);
			swap(it->bluePartNum_, it->redPartNum_);
			swap(it->blueSegNum_, it->redSegNum_);

			if(it->coords_.size() == 2)	//overlap
			{
				// Verificar se os pontos estao em ordem crescente
				if(it->coords_[0].x_ < it->coords_[1].x_)
				{
				}
				else if(it->coords_[0].x_ > it->coords_[1].x_)
				{
					swap(it->coords_[0], it->coords_[1]);
				}
				else if(it->coords_[0].y_ < it->coords_[1].y_)
				{

				}
				else
				{
					swap(it->coords_[0], it->coords_[1]);							
				}
			}

			++it;
		}

// fragmenta as linhas azuis
		TeLineSet blueFragments;
		TeLineSet blueBoundaryFragments;
		vector<pair<unsigned int, unsigned int> > blueFragmentsIds;
		boundaryFragmentsIds.clear();

		TeFragmentBoundary(bluePolsIn[i], report, blueBoundaryFragments, boundaryFragmentsIds, blueFragments, blueFragmentsIds);

		report.clear();

		mask  = TeUNKNOWNPOSITION;

// os fragmentos de fronteira ja sao separados
		if(blueBoundaryFragments.size())
		{
			mask = TeBOUNDARY;

			unsigned int boundaryFragmentsSize = blueBoundaryFragments.size();

			for(unsigned int ti = 0; ti < boundaryFragmentsSize; ++ti)
			{
				boundaryFragmentsIndex.insert(TeOVERLAY::TeLineIndex::value_type(blueBoundaryFragments[ti][0], pair<unsigned int, TeLine2D>(boundaryId_, blueBoundaryFragments[ti])));
				++boundaryId_;
			}
		}

// escolhe os fragmentos vermelhos que estiverem localizados fora dos poligonos azuis - fragmenta as linhas vermelhas
		TeRtreeGetFragments(redPolsIn, redSegmentTree, blueFragments, locationBlueFragments, mask, fragmentsIndex, rings);

// libera a memoria usada pelos indice azul
		blueSegmentTree.clear();

// tenta eliminar fragmentos de fronteira no mesmo sentido, deixando apenas um nesta direcao
		TeRemoveSameBoundaryFragments(boundaryFragmentsIndex);

// tenta eliminar fragmentos sobre fronteira opostos
        TeRemoveOpositeBoundaryFragments(boundaryFragmentsIndex);

// faz um merge dos fragmentos de fronteira para o indice de fragmentos normal
		TeJoinFragments(fragmentsIndex, boundaryFragmentsIndex);

// monta os poligonos a partir dos fragmentos
		bool returnValue = TeMergeFragments(fragmentsIndex, rings, true);

		// faz o split
		vector<TeLinearRing> ringsOut;

		bool resultSplit = true;

		if(TeOVERLAY::TeSplitRings(rings, ringsOut) == false)
		{
			ringsOut.clear();

			ringsOut = rings;

			resultSplit = false;
		}

// separate holes from islands
		vector<TeLinearRing> holes;
		TePolygonSet polsOut;

		bool result = TeOVERLAY::TeClassifyRings(ringsOut, polsOut, holes);

		returnValue = returnValue && result && resultSplit;

// mount topology		
		if((polsOut.size() == 0) && (holes.size() == 0))
		{
			if(operation == TeUNION)			
			{
				resultVec.push_back(false);	// mario - 2006-03
				resultValue = false;						// uniao nao pode dar vazio
			}
			else
			{
				resultVec.push_back(returnValue);	// mario - 2006-03
				resultValue = (resultValue && returnValue);	// Diferenca e intersecao podem retornar vazio
			}

			vecPolsOut.push_back(polsOut);	// only to mark result			
		}
		else
		{
			bool mountResult = TeOVERLAY::TeMountTopology(polsOut, holes);

			resultVec.push_back(returnValue && mountResult);	// mario - 2006-03

			resultValue = resultValue && returnValue && mountResult;	

			vecPolsOut.push_back(polsOut);
		}		
		
	}

	redSegmentTree.clear();

	return resultValue;
}

bool TeOVERLAY::TeIntersection(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const bool& makeCopy, const bool fixOrientation)
{
	return TeOverlay(redPols, bluePols, vecPolsOut, resultVec, TeINTERSECTION, makeCopy, fixOrientation);
}

bool TeOVERLAY::TeUnion(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const bool& makeCopy, const bool fixOrientation)
{
	return TeOverlay(redPols, bluePols, vecPolsOut, resultVec, TeUNION, makeCopy, fixOrientation);
}

bool TeOVERLAY::TeDifference(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const bool& makeCopy, const bool fixOrientation)
{
	return TeOverlay(redPols, bluePols, vecPolsOut, resultVec, TeDIFFERENCE, makeCopy, fixOrientation);
}

// deprecated functions
bool TeOVERLAY::TeOverlay(const TePolygonSet& redPols, const TePolygonSet& bluePols, TePolygonSet& polsOut, const short& operation)
{
	vector<TePolygonSet> bluePolsVec;
	bluePolsVec.push_back(bluePols);
	vector<TePolygonSet> resultPols;
	TePolygonSet redPolsAux = redPols;
	vector<bool> resultVec;

	bool result = TeOverlay(redPolsAux, bluePolsVec, resultPols, resultVec, operation, true, true);

	if(resultPols.size() > 0)
		polsOut = resultPols[0];

	return result;	
}

bool TeOVERLAY::TeUnion(TePolygonSet& redPols, TePolygonSet& bluePols, TePolygonSet& polsOut)
{
	return TeOVERLAY::TeOverlay(redPols, bluePols, polsOut, TeUNION);
}

bool TeOVERLAY::TeIntersection(TePolygonSet& redPols, TePolygonSet& bluePols, TePolygonSet& polsOut)
{
	return TeOVERLAY::TeOverlay(redPols, bluePols, polsOut, TeINTERSECTION);
}

bool TeOVERLAY::TeDifference(TePolygonSet& redPols, TePolygonSet& bluePols, TePolygonSet& polsOut)
{
	return TeOVERLAY::TeOverlay(redPols, bluePols, polsOut, TeDIFFERENCE);
}

TeMultiGeometry TeOverlay(const TeLineSet& redLinesIn, const TePolygonSet& bluePolsIn, TeINTERSECTOR2::TeSegmentRTree& blueTree, const short& operation)
{
//	short location = TeUNKNOWNPOSITION;
	short locationRedFragments = TeUNKNOWNPOSITION;
	short locationBlueFragments = TeUNKNOWNPOSITION;

	TeMultiGeometry mGeom;

	TeLineSet redLines;
	TePolygonSet bluePols;

// copia o conteudo das linhas de entrada
	redLines.copyElements(redLinesIn);

// copia o conteudo dos poligonos de entrada
	bluePols.copyElements(bluePolsIn);

// determina a localizacao dos fragmentos que serao utilizados
	TeOVERLAY::TeChooseBoundaryLocation(operation, locationRedFragments, locationBlueFragments);

// gets intersection list
	TeINTERSECTOR2::TeVectorBoundaryIP report;

	TeINTERSECTOR2::TeIntersection(redLines, bluePols, blueTree, report);

// fragment lines
	TeLineSet redFragments;
	TeLineSet redBoundaryFragments;
	
    TeFragmentBoundary(redLines, report, redBoundaryFragments, redFragments);

// choose red fragments
	vector<TeLinearRing> rings;

	TeOVERLAY::TeLineIndex fragmentsIndex;

	short mask = 0;

	TeRtreeGetFragments(bluePols, blueTree, redFragments, locationRedFragments, mask, fragmentsIndex, rings);

// report fragments from red lines
	TeOVERLAY::TeLineIndex::iterator it = fragmentsIndex.begin();

	while(it != fragmentsIndex.end())
	{
		mGeom.addGeometry(it->second.second);
		++it;
	}

	unsigned int nrings = rings.size();
	unsigned int i;
	for(i = 0; i < nrings; ++i)
	{
		mGeom.addGeometry(rings[i]);
	}

// if the operaton is union, so push bluepols

	if(operation == TeUNION)
	{							
		mGeom.setGeometry(bluePols);		
	}

	return mGeom;
}

void TeOverlay(vector<TeLineSet>& redLinesIn, const TePolygonSet& bluePolsIn, vector<TeMultiGeometry>& outPutGeoms, const short& operation)
{
	TeINTERSECTOR2::TeSegmentRTree blueTree(bluePolsIn.box());

	unsigned int nLines = redLinesIn.size();
	unsigned int i;
	for(i = 0; i < nLines; ++i)
	{
		TeMultiGeometry mg;

		mg = TeOverlay(redLinesIn[i], bluePolsIn, blueTree, operation);

		outPutGeoms.push_back(mg);        		
	}

	blueTree.clear();

	return;
}

// overlay for lines/polygons
TeMultiGeometry TeOVERLAY::TeOverlay(const TeLineSet& redLinesIn, const TePolygonSet& bluePolsIn, const short& operation)
{
	vector<TeMultiGeometry> mgeoms;
	vector<TeLineSet> redLines;
	redLines.push_back(redLinesIn);
	TeMultiGeometry outputGeom;

	::TeOverlay(redLines, bluePolsIn, mgeoms, operation);

	if(mgeoms.empty())
		return outputGeom;

	outputGeom = mgeoms[0];

	return outputGeom;
}

TeMultiGeometry TeOVERLAY::TeUnion(TeLineSet& redLines, TePolygonSet& bluePols)
{
	return TeOVERLAY::TeOverlay(redLines, bluePols, TeUNION);
}

TeMultiGeometry TeOVERLAY::TeIntersection(TeLineSet& redLines, TePolygonSet& bluePols)
{
	return TeOVERLAY::TeOverlay(redLines, bluePols, TeINTERSECTION);
}

TeMultiGeometry TeOVERLAY::TeDifference(TeLineSet& redLines, TePolygonSet& bluePols)
{
	return TeOVERLAY::TeOverlay(redLines, bluePols, TeDIFFERENCE);
}

bool TeOVERLAY::TePairUnion(TePolygonSet& psetIn, TePolygonSet& psetOut)
{
	psetOut.clear();

	vector<TePolygonSet> outPutSet;
	unsigned int i;
	for(i = 0; i < psetIn.size(); ++i)
	{
		TePolygonSet pset;
		pset.add(psetIn[i]);
		outPutSet.push_back(pset);
	}

	unsigned int outsize = outPutSet.size();

	bool resultUnion = true;

	while(outsize > 1)
	{
// une dois a dois
		unsigned int auxsize = 0;
		unsigned int i;
		for(i = 0; i < outsize; i += 2)
		{
			TePolygonSet psRed = outPutSet[i];
			if((i + 1) < outsize)
			{
				TePolygonSet psBlue = outPutSet[i+1];
				TePolygonSet psUnidoAgregado;
				resultUnion = resultUnion && TeOVERLAY::TeUnion(psRed, psBlue, psUnidoAgregado);
				if(psUnidoAgregado.size() > 0)
				{
					outPutSet[auxsize] = psUnidoAgregado;
					++auxsize;
				}
			}
			else
			{
				outPutSet[auxsize] = psRed;
				++auxsize;
			}			
		}

		outsize = auxsize;
	}

	for(unsigned int i = 0; i < outsize; ++i)
		for(unsigned int j = 0; j < outPutSet[i].size(); ++j)		
			psetOut.add(outPutSet[i][j]);

	return resultUnion;
}

bool TeOVERLAY::TeValidPolygonHoles(TePolygon& polygon, TePolygonSet& psValid)
{
	string objectId = polygon.objectId();
	int geomId = polygon.geomId();

	psValid.clear();

	if(polygon.size() == 0)
		return false;

	if(polygon.size() == 1)
	{	
		TePolygon paux;
		paux.copyElements(polygon);
		paux.geomId(geomId);
		paux.objectId(objectId);
		psValid.add(paux);
		return true;
	}

	TePolygon fixedPolygon;
	fixedPolygon.add(polygon[0]);
				
//adiciona todos os buracos em um TePolygonSet
	TePolygonSet psHoles;
	unsigned int i;
	for(i = 1; i < polygon.size(); i++)
	{
		TePolygon hole;
		hole.add(polygon[i]);

		psHoles.add(hole);
	}

//faz a uniao dos buracos
	TePolygonSet psOut;	
	if(!TeOVERLAY::TeUnion(psHoles, psOut))
		return false;

	psHoles = TePolygonSet();
	psHoles.copyElements(psOut);
	psOut = TePolygonSet();

//faz a diferenca entre o poligono original e a uniao dos seus buracos 
	TePolygonSet psTemp;
	psTemp.add(fixedPolygon);

	if(!TeOVERLAY::TeDifference(psTemp, psHoles, psOut))
		return false;
		
	for(i = 0; i < psOut.size(); i++)
	{
		psOut[i].geomId(geomId);
		psOut[i].objectId(objectId);
		psValid.add(psOut[i]);
	}

	return true;
}

bool TeOVERLAY::TeValidPolygonHoles(TePolygonSet& polygons, TePolygonSet& psValid)
{
	psValid.clear();
	unsigned int i, j;
    for(i = 0; i < polygons.size(); ++i)
	{
// do valid for each polygon
        TePolygon pol = polygons[i];

        TePolygonSet psAux;

        if(TeValidPolygonHoles(pol, psAux))
		{
			for(j = 0; j < psAux.size(); ++j)
			{
				psValid.add(psAux[j]);
			}
		}
		else
		{
			return false;
		}
	}

	return true;
}

