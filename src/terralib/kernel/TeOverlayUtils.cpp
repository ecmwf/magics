/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright 2001-2004 INPE and Tecgraf/PUC-Rio.

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

#include "TeOverlayUtils.h"
#include "TeGeometryAlgorithms.h"

//---------------- Auxiliary operations for overlay ----------------//

inline void TeOverlayMiddle(const TeCoord2D& c1, const TeCoord2D& c2, TeCoord2D& m)
{
	m.x_ = ((c1.x_ + c2.x_) / 2.0);
	m.y_ = ((c1.y_ + c2.y_) / 2.0);
}

// Verifies orientation for each line of polygon set, and reverse the orientation if need
void TeOVERLAY::TeFixOrientation(TePolygonSet& polSet, const short& outerOrientationToReverse, const short& innerOrientationToReverse)
{
	unsigned int polSetSize = polSet.size();

	for(unsigned int i = 0; i < polSetSize; ++i)
	{
		if(TeOrientation(polSet[i][0]) == outerOrientationToReverse)
			reverse(polSet[i][0].begin(), polSet[i][0].end());

		unsigned int polSize = polSet[i].size();

		for(unsigned int j = 1; j < polSize; ++j)
		{
			if(TeOrientation(polSet[i][j]) == innerOrientationToReverse)
				reverse(polSet[i][j].begin(), polSet[i][j].end());
		}
	}
}

// For each operation (union, intersection and difference) defines location for retrieval of fragments
void TeOVERLAY::TeChooseBoundaryLocation(const short& operation, short& locationRedFragments, short& locationBlueFragments)
{
	switch(operation)
	{							
// intersection: gets all fragments from red polygon wich are
// inside the blue polygon and the blue fragments that are inside
// the red polygon.
		case TeINTERSECTION:  locationRedFragments  = TeINSIDE;
			                  locationBlueFragments = TeINSIDE;
							  break;

// union: gets all fragments from red polygon wich are
// outside the blue polygon and the blue fragments that are outside
// the red polygon.
		case TeUNION:         locationRedFragments  = TeOUTSIDE;
			                  locationBlueFragments = TeOUTSIDE;
							  break;

// difference: gets all fragments from red polygon wich are
// outside the blue polygon and the blue fragments that are inside
// the red polygon.
		case TeDIFFERENCE:	  locationRedFragments  = TeOUTSIDE;
			                  locationBlueFragments = TeINSIDE;
		                      break;
	}
}

// Erases from fragmentsIndex boundary fragments that are in oposite direction: each fragment must have a unique identifier in the pair first field (pair<unsigned int, TeLine2D>)
void TeOVERLAY::TeRemoveOpositeBoundaryFragments(TeLineIndex& fragmentsIndex)
{
	TeLineIndex::iterator indexIterator = fragmentsIndex.begin();
	
	while(indexIterator != fragmentsIndex.end())
	{
		pair<TeLineIndex::iterator, TeLineIndex::iterator> its = fragmentsIndex.equal_range(indexIterator->second.second[(indexIterator->second.second.size() - 1u)]);

		bool eraseOwn = false;

        while(its.first != its.second)
		{
// Se nao achei o proprio fragmento, entao posso ver se e possivel apagar
			if(its.first->second.first != indexIterator->second.first)
			{
				if(TeEquals(its.first->second.second[its.first->second.second.size() - 1u], indexIterator->second.second[0u]))
				{
					TeLineIndex::iterator idxAux2 = its.first;
					++(its.first);
					fragmentsIndex.erase(idxAux2);

					eraseOwn = true;
				}
				else
				{
					++(its.first);
				}
			}
			else
			{
				++(its.first);
			}
		}

		if(eraseOwn)
		{
			fragmentsIndex.erase(indexIterator);
			indexIterator = fragmentsIndex.begin();
		}
		else
		{
			++indexIterator;
		}
	}
}

// Erases from fragmentsIndex boundary fragments that are equals to another boundary fragment
void TeOVERLAY::TeRemoveSameBoundaryFragments(TeLineIndex& fragmentsIndex)
{
	TeLineIndex::iterator indexIterator = fragmentsIndex.begin();
	
	while(indexIterator != fragmentsIndex.end())
	{
		pair<TeLineIndex::iterator, TeLineIndex::iterator> its = fragmentsIndex.equal_range(indexIterator->second.second[0u]);

        while(its.first != its.second)
		{
// Se nao achei o proprio fragmento, entao posso ver se e possivel apagar
			if(its.first->second.first != indexIterator->second.first)
			{
				if(TeEquals(its.first->second.second[its.first->second.second.size() - 1u], indexIterator->second.second[indexIterator->second.second.size() - 1u]))
				{
					TeLineIndex::iterator idxAux2 = its.first;
					++(its.first);
					fragmentsIndex.erase(idxAux2);
				}
				else
				{
					++(its.first);
				}
			}
			else
			{
				++(its.first);
			}
		}

		++indexIterator;
	}
}

// Merge fragments ito first index (fragmentsIndex)
void TeOVERLAY::TeJoinFragments(TeLineIndex& fragmentsIndex, TeLineIndex& boundaryFragmentsIndex)
{
	TeLineIndex::iterator indexIterator = boundaryFragmentsIndex.begin();

	while(indexIterator != boundaryFragmentsIndex.end())
	{
		fragmentsIndex.insert(*indexIterator);
		++indexIterator;
	}

	boundaryFragmentsIndex.clear();	
}

// Moves closed rings from fragmentsIndex to rins vector
void TeOVERLAY::TeFindAndMoveClosedRings(TeLineIndex& fragmentsIndex, vector<TeLinearRing>& rings)
{
	TeLineIndex::iterator indexIterator = fragmentsIndex.begin();

	while(indexIterator != fragmentsIndex.end())
	{
		if(indexIterator->second.second.isRing())
		{
			rings.push_back(indexIterator->second.second);

			TeLineIndex::iterator idxAux;
			idxAux = indexIterator;
			++indexIterator;
			fragmentsIndex.erase(idxAux);
		}
		else
			++indexIterator;
	}
}

// Gets a polygonset with outer rings and a vector with holes and try to find to what polygon the hole belongs to
bool TeOVERLAY::TeMountTopology(TePolygonSet& polysOut, vector<TeLinearRing>& holes)
{
	bool returnValue = true;

	if((polysOut.size() == 0) && (holes.size() > 0))
	{
// Formou buracos e nao formou os aneis externos
		return false;	
	}

	if(polysOut.size() == 1)
	{
		for(unsigned int i = 0; i < holes.size(); ++i)
			polysOut[0].add(holes[i]);		
	}
	else
	{
		for(unsigned int i = 0; i < holes.size(); ++i)
		{
			TeLinearRing ring = holes[i];

			vector<TePolygon> candidates;
			vector<unsigned int> candidatesPos;

			unsigned int j = 0;

			for(j = 0; j < polysOut.size(); ++j)
			{
				if(TeWithinOrCoveredByOrEquals(ring.box(), polysOut[j].box()))
				{
					candidates.push_back(polysOut[j]);
					candidatesPos.push_back(j);
				}
			}

			if(candidates.size() == 1)
			{
				candidates[0].add(ring);
				continue;
			}

			vector<TePolygon> newCandidates;

			for(j = 0; j < candidates.size(); ++j)
			{
				short rel = TeBOUNDARY;

				unsigned int nthVert = ring.size();
				unsigned int iVert = 0u;

				while(iVert < nthVert)
				{
					rel = TeRelation(ring[iVert], candidates[j][0]);

					if(rel & TeINSIDE)
					{				
						newCandidates.push_back(candidates[j]);
						break;
					}
					else if(rel & TeOUTSIDE)
					{
						break;
					}
				
					++iVert;
				}

				if(iVert == nthVert)	
				{
// Topologycal error: every point is on ring boundary...
					returnValue = false;

					TePolygon topTest;
					topTest.add(ring);

					short whatRel = TeRelation(topTest, candidates[j]);

// Se um buraco for igual ao exterior, existe um erro topologico
// No momento, eliminamos o anel externo
// e o interno... 
// Se o buraco for coberto pelo poligono externo, entao ele ira ficar dentro deste
// Caso contrario e erro sem possibilidades...
					if(whatRel & TeEQUALS)
					{						
						polysOut.erase(candidatesPos[j]);
						continue;
					}
				}				
			}

			if(newCandidates.size() <= 0)
			{
// Didn't find outer ring to this hole.
				returnValue = false;
				continue;
			}

			int idxMinArea = 0;
			
			double minArea = TeMAXFLOAT;

			for(j = 0; j < newCandidates.size(); ++j)
			{
				if(TeGeometryArea(newCandidates[j].box()) < minArea)
				{
					idxMinArea = j;
					minArea = TeGeometryArea(newCandidates[j].box());
				}
			}

			newCandidates[idxMinArea].add(ring);
		}
	}

	return returnValue;
}

// faz a tentativa ingenua!
bool TeMergeFragmentsFB(TeOVERLAY::TeLineIndex& fragmentsIndex, TeLine2D& line)
{
	TeCoord2D endLineCoordinate = line[line.size() - 1];

	TeOVERLAY::TeLineIndex::iterator indexIterator  = fragmentsIndex.begin();

	while(indexIterator != fragmentsIndex.end())
	{
		if(TeEquals(endLineCoordinate, indexIterator->second.second[0]))
		{
			for(unsigned int i = 1; i < indexIterator->second.second.size(); ++i)
				line.add(indexIterator->second.second[i]);

			line.setBox(::TeUnion(line.box(), indexIterator->second.second.box()));
			fragmentsIndex.erase(indexIterator);

			return true;
		}

		++indexIterator;
	}

	return false;
}

// Make polygons from fragments.
bool TeOVERLAY::TeMergeFragments(TeLineIndex& fragmentsIndex, vector<TeLinearRing>& rings, const bool& doExaustive)
{
	bool returnValue = true;

	TeLine2D  lAux;	
	TeCoord2D endLineCoordinate;

	while(!(fragmentsIndex.empty()))
	{
		if(lAux.size() == 0)
		{
			TeLineIndex::iterator indexIterator  = fragmentsIndex.begin();
			
			if(indexIterator != fragmentsIndex.end())
			{
				if(indexIterator->second.second.isRing())
				{
					rings.push_back(indexIterator->second.second);
					fragmentsIndex.erase(indexIterator);
					continue;
				}
				
				for(unsigned int i = 0; i < indexIterator->second.second.size(); ++i)
					lAux.add(indexIterator->second.second[i]);

				lAux.setBox(::TeUnion(lAux.box(), indexIterator->second.second.box()));

				fragmentsIndex.erase(indexIterator);
			}
			else
			{
                returnValue = false;	//Nao poderia vir aqui, deveria ter saido no teste do laco!!
			}
		}	
		else
		{
			endLineCoordinate = lAux[lAux.size() - 1];

			// Try to find the beginning of the next fragment that is part of the polygon in the same list
			TeLineIndex::iterator indexIterator = fragmentsIndex.find(endLineCoordinate);

			if(indexIterator != fragmentsIndex.end())
			{
				for(unsigned int i = 1; i < indexIterator->second.second.size(); ++i)
					lAux.add(indexIterator->second.second[i]);

				lAux.setBox(::TeUnion(lAux.box(), indexIterator->second.second.box()));
				fragmentsIndex.erase(indexIterator);
			}			
			else
			{
				if(doExaustive)
				{
					if(!TeMergeFragmentsFB(fragmentsIndex, lAux))
					{
	// Nao foi achada continuacao para a linha: nao fechou!
						returnValue = false;	// Erro na topologia dos poligonos

						// Clear auxiliary.
						TeLine2D emptyLine;
						lAux = emptyLine;
					}
				}
				else
				{
					// Nao foi achada continuacao para a linha: nao fechou!
					returnValue = false;	// Erro na topologia dos poligonos

					// Clear auxiliary.
					TeLine2D emptyLine;
					lAux = emptyLine;
				}
			}
			
		}

		if(lAux.isRing())
		{  
			// Add polygon
			rings.push_back(TeLinearRing(lAux));
			
			// Clear auxiliary.
			TeLine2D emptyLine;					
			lAux = emptyLine;
		}
	}

	if(lAux.size() > 0)
		returnValue = false;	// Erro, alguma linha nao fechou!!!

	return returnValue;
}

// For each linear ring, see it's orientation and classify in outer or inner ring
bool TeOVERLAY::TeClassifyRings(vector<TeLinearRing>& rings, TePolygonSet& polsOut, vector<TeLinearRing>& holes)
{
	bool returnValue = true;

	for(unsigned int z = 0; z < rings.size(); ++z)
	{
		short ori = TeOrientation(rings[z]);

		if(ori == TeCOUNTERCLOCKWISE)	// It is a hole
		{
			holes.push_back(rings[z]);			// add to holes list
		}
		else if(ori == TeCLOCKWISE)		// else if island
		{										// create a polygon
			TePolygon p;
			p.add(rings[z]);
			polsOut.add(p);
		}
		else	
		{
			returnValue = false;	// Objeto sem area? Isso e um erro!
		}
	}

	return returnValue;
}

// Seleciona os fragmentos quando existem dois conjuntos distintos: vermelho e azul
void TeOVERLAY::TeRtreeGetFragments(const TePolygonSet& bluePolygons, TeINTERSECTOR2::TeSegmentRTree& blueTree, TeLineSet& redFragments, const short& locationFragments, short& mask, TeLineIndex& redFragmentsIndex, vector<TeLinearRing>& rings)
{
	//unsigned int lastId = redFragmentsIndex.size();

	unsigned int redFragmentsSize = redFragments.size();	

	TeBox b = blueTree.getBox();

	for(unsigned int i = 0; i < redFragmentsSize; ++i)
	{
// Para cada fragmento vermelho escolhe um ponto para determinar a localizacao do fragmento
		TeCoord2D middle;

		unsigned int fragSize = redFragments[i].size();

		TeCoord2D& cfrom = redFragments[i][0];
		TeCoord2D& cto = redFragments[i][1];

		if(fragSize ==  2)	// If the fragment has two points I need to check the middle point of this fragment.
			TeOverlayMiddle(redFragments[i][0], redFragments[i][1], middle);
		else	// If the fragment has more than two points so I check one point between the end points.
			middle = redFragments[i][(unsigned int)((double(redFragments[i].size()) / 2.0 + 0.6)) - 1];

// Monta um raio horizontal que vai ate o extremo de todos os poligonos, partindo do ponto medio (middle)
		TeCoord2D c2 = middle;
		c2.x_ = b.x2();

		TeBox searchBox(middle, c2);

		vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;

        blueTree.search(searchBox, segs);

        unsigned int nSegsInter = segs.size();

// se nao achou nenhum segmento cruzando o raio, significa que o fragmento esta todo fora
		if(nSegsInter > 0)
		{
// ordena os segmentos azuis para permitir realizar o teste de ponto em poligono
			sort(segs.begin(), segs.end(), segOrder());

			bool inside_flag = false;

			unsigned int currentPolId = segs[0].polId_;

			double tx = middle.x();
		    double ty = middle.y();

			int yflag0, yflag1;

			for(unsigned int j = 0; j < nSegsInter; ++j)
			{
// Se passamos aos segmentos de outro poligono, temos que zerar o numero de cruzamentos
				if(currentPolId != segs[j].polId_)
				{
					if(inside_flag)
						break;

					currentPolId = segs[j].polId_;
				}

				const TeCoord2D& vtx0 = bluePolygons[segs[j].polId_][segs[j].lineId_][segs[j].segId_];
				const TeCoord2D& vtx1 = bluePolygons[segs[j].polId_][segs[j].lineId_][segs[j].segId_ + 1];

				if((fragSize == 2) && (TeIsOnSegment(middle, vtx0, vtx1)))
				{
					
					if((TeEquals(vtx0, cfrom) || TeEquals(vtx0, cto)) && (TeEquals(vtx1, cfrom) || TeEquals(vtx1, cto)))
					{
						if(locationFragments & TeINSIDE)
							inside_flag = false;
						else
							inside_flag = true;						

						break;
					}
				}

				yflag0 = (vtx0.y() >= ty);
				yflag1 = (vtx1.y() >= ty);

				if(yflag0 != yflag1)
				{
					if(((vtx1.y() - ty) * (vtx0.x() - vtx1.x()) >=
						(vtx1.x() - tx) * (vtx0.y() - vtx1.y())) == yflag1)
					{
						inside_flag = !inside_flag ;
					}
				}
			}

// ao sair do laco acima, se inside_flag for verdadeiro, entao o fragmento esta dentro, caso contrario, ele esta fora
			if(inside_flag && (locationFragments & TeINSIDE))
			{
				if(redFragments[i].isRing())
					rings.push_back(redFragments[i]);
				else
					redFragmentsIndex.insert(TeLineIndex::value_type(redFragments[i][0], pair<unsigned int, TeLine2D>(i, redFragments[i])));

				mask |= TeINSIDE;
			}
			else if(!inside_flag && (locationFragments & TeOUTSIDE))
			{
				if(redFragments[i].isRing())
					rings.push_back(redFragments[i]);
				else
					redFragmentsIndex.insert(TeLineIndex::value_type(redFragments[i][0], pair<unsigned int, TeLine2D>(i, redFragments[i])));

				mask |= TeOUTSIDE;
			}
		}
		else	// fragmento esta fora
		{
// Se a localizacao do fragmento for compativel, pegamos ele.
			if(locationFragments & TeOUTSIDE)
			{
				if(redFragments[i].isRing())
					rings.push_back(redFragments[i]);
				else
					redFragmentsIndex.insert(TeLineIndex::value_type(redFragments[i][0], pair<unsigned int, TeLine2D>(i, redFragments[i])));
			}

			mask |= TeOUTSIDE;
		}
	}
}

// Operacao especial que descobre a localizacao do fragmento dentro do proprio conjunto: usado na operacao de uniao otimizada
void TeOVERLAY::TeRtreeGetFragments(const TePolygonSet& polygons, TeINTERSECTOR2::TeSegmentRTree& tree, TeLineSet& fragments, vector<pair<unsigned int, unsigned int> >& fragmentsIds, const short& locationFragments, short& mask, TeLineIndex& fragmentsIndex, vector<TeLinearRing>& rings)
{
	unsigned int fragmentsSize = fragments.size();	

	TeBox b = tree.getBox();

	for(unsigned int i = 0; i < fragmentsSize; ++i)
	{
// Para cada fragmento escolhe um ponto para determinar a localizacao do fragmento
		TeCoord2D middle;

		unsigned int fragSize = fragments[i].size();

		TeCoord2D& cfrom = fragments[i][0];
		TeCoord2D& cto = fragments[i][1];

		if(fragSize ==  2)	// If the fragment has two points I need to check the middle point of this fragment.
			TeOverlayMiddle(fragments[i][0], fragments[i][1], middle);
		else	// If the fragment has more than two points so I check one point between the end points.
			middle = fragments[i][(unsigned int)((double(fragments[i].size()) / 2.0 + 0.6)) - 1];

// Monta um raio horizontal que vai ate o extremo de todos os poligonos, partindo do ponto medio (middle)
		TeCoord2D c2 = middle;
		c2.x_ = b.x2();

		TeBox searchBox(middle, c2);

		vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;

        tree.search(searchBox, segs);

        unsigned int nSegsInter = segs.size();

// se nao achou nenhum segmento cruzando o raio, significa que o fragmento esta todo fora
		if(nSegsInter > 0)
		{
// ordena os segmentos para permitir realizar o teste de ponto em poligono
			sort(segs.begin(), segs.end(), segOrder());

			bool inside_flag = false;

			unsigned int currentPolId = segs[0].polId_;

			double tx = middle.x();
		    double ty = middle.y();

			int yflag0, yflag1;

			for(unsigned int j = 0; j < nSegsInter; ++j)
			{
// Se passamos aos segmentos de outro poligono, temos que zerar o numero de cruzamentos
				if(currentPolId != segs[j].polId_)
				{
					if(inside_flag)
						break;

					currentPolId = segs[j].polId_;
				}

// Se os segmentos que estamos tratando sao do fratgmento, entao pulamos
				if(segs[j].polId_ == fragmentsIds[i].first)
					continue;

				const TeCoord2D& vtx0 = polygons[segs[j].polId_][segs[j].lineId_][segs[j].segId_];
				const TeCoord2D& vtx1 = polygons[segs[j].polId_][segs[j].lineId_][segs[j].segId_ + 1];

				if((fragSize == 2) && (TeIsOnSegment(middle, vtx0, vtx1)))
				{
					
					if((TeEquals(vtx0, cfrom) || TeEquals(vtx0, cto)) && (TeEquals(vtx1, cfrom) || TeEquals(vtx1, cto)))
					{
						if(locationFragments & TeINSIDE)
							inside_flag = false;
						else
							inside_flag = true;

						break;
					}
				}

				yflag0 = (vtx0.y() >= ty);
				yflag1 = (vtx1.y() >= ty);

				if(yflag0 != yflag1)
				{
					if(((vtx1.y() - ty) * (vtx0.x() - vtx1.x()) >=
						(vtx1.x() - tx) * (vtx0.y() - vtx1.y())) == yflag1)
					{
						inside_flag = !inside_flag ;
					}
				}
			}

// ao sair do laco acima, se inside_flag for verdadeiro, entao o fragmento esta dentro, caso contrario, ele esta fora
			if(inside_flag && (locationFragments & TeINSIDE))
			{
				if(fragments[i].isRing())
					rings.push_back(fragments[i]);
				else
					fragmentsIndex.insert(TeLineIndex::value_type(fragments[i][0], pair<unsigned int, TeLine2D>(i, fragments[i])));
			}
			else if(!inside_flag && (locationFragments & TeOUTSIDE))
			{
				if(fragments[i].isRing())
					rings.push_back(fragments[i]);
				else
					fragmentsIndex.insert(TeLineIndex::value_type(fragments[i][0], pair<unsigned int, TeLine2D>(i, fragments[i])));
			}

		}
		else	// fragmento esta fora
		{
// Se a localizacao do fragmento for compativel, pegamos ele.
			if(locationFragments & TeOUTSIDE)
			{
				if(fragments[i].isRing())
					rings.push_back(fragments[i]);
				else
					fragmentsIndex.insert(TeLineIndex::value_type(fragments[i][0], pair<unsigned int, TeLine2D>(i, fragments[i])));
			}

			mask |= TeOUTSIDE;
		}
	}
}

// Operacao especial que descobre a localizacao do fragmento dentro do proprio conjunto: usado na operacao de uniao otimizada
void TeOVERLAY::TeRtreeRemoveFragments(const TePolygonSet& polygons, TeINTERSECTOR2::TeSegmentRTree& tree,
		TeLineIndex &lineIndex, vector<pair<unsigned int, unsigned int> >& fragmentsIds,
		const short& locationFragments, short& mask, vector<TeLinearRing>& /* rings */)
{
	TeBox b = tree.getBox();

	TeLineIndex::iterator indexIterator = lineIndex.begin();
	
	while(indexIterator != lineIndex.end())
	{
		TeCoord2D middle;
		TeOverlayMiddle(indexIterator->second.second[0], indexIterator->second.second[1], middle);

		// Monta um raio horizontal que vai ate o extremo de todos os poligonos, partindo do ponto medio (middle)
		TeCoord2D c2 = middle;
		c2.x_ = b.x2();

		TeBox searchBox(middle, c2);

		vector<TeINTERSECTOR2::TeSegIdInPolygonSet> segs;

        tree.search(searchBox, segs);

        unsigned int nSegsInter = segs.size();

		// se nao achou nenhum segmento cruzando o raio, significa que o fragmento esta todo fora
		if(nSegsInter > 0)
		{
// ordena os segmentos para permitir realizar o teste de ponto em poligono
			sort(segs.begin(), segs.end(), segOrder());

			bool inside_flag = false;

			unsigned int currentPolId = segs[0].polId_;

			double tx = middle.x();
		    double ty = middle.y();

			int yflag0, yflag1;

			for(unsigned int j = 0; j < nSegsInter; ++j)
			{
// Se passamos aos segmentos de outro poligono, temos que zerar o numero de cruzamentos
				if(currentPolId != segs[j].polId_)
				{
					if(inside_flag)
						break;

					currentPolId = segs[j].polId_;
				}

// Se os segmentos que estamos tratando sao do fragmento, entao pulamos
				if(segs[j].polId_ == fragmentsIds[indexIterator->second.first].first)
					continue;

				const TeCoord2D& vtx0 = polygons[segs[j].polId_][segs[j].lineId_][segs[j].segId_];
				const TeCoord2D& vtx1 = polygons[segs[j].polId_][segs[j].lineId_][segs[j].segId_ + 1];

				// o ponto esta no segmeto de vertices vtx0 e vtx1?
				// se sim setar inside_flag como false e avancar o j ate mudar de poligino e pula pro inicio dessa secao (continue)
				if(TeIsOnSegment(middle, vtx0, vtx1))
				{
					inside_flag = false;

					while((j < nSegsInter) && currentPolId == segs[j].polId_)
						++j;

					if(j >= nSegsInter)
						break;

					--j;

					continue;
				}

				yflag0 = (vtx0.y() >= ty);
				yflag1 = (vtx1.y() >= ty);

				if(yflag0 != yflag1)
				{
					if(((vtx1.y() - ty) * (vtx0.x() - vtx1.x()) >=
						(vtx1.x() - tx) * (vtx0.y() - vtx1.y())) == yflag1)
					{
						inside_flag = !inside_flag ;
					}
				}
			}

// ao sair do laco acima, se inside_flag for verdadeiro, entao o fragmento esta dentro, caso contrario, ele esta fora
			if(inside_flag && (locationFragments & TeINSIDE))
			{
				TeLineIndex::iterator it_aux =	indexIterator;
				++(indexIterator);
				lineIndex.erase(it_aux);				
			}
			else if(!inside_flag && (locationFragments & TeOUTSIDE))
			{
				TeLineIndex::iterator it_aux =	indexIterator;
				++(indexIterator);
				lineIndex.erase(it_aux);
			}
			else
			{
				++(indexIterator);
			}

		}
		else	// fragmento esta fora
		{
// Se a localizacao do fragmento for compatìvel, pegamos ele.
			if(locationFragments & TeOUTSIDE)
			{
				TeLineIndex::iterator it_aux =	indexIterator;
				++(indexIterator);
				lineIndex.erase(it_aux);				
			}
			else
			{
				++(indexIterator);
			}

			mask |= TeOUTSIDE;
		}
	}
}

// estrutura auxiliar utilizada no codigo do split
struct TeSplitCoordSort
{
	bool operator()(pair<unsigned int, TeCoord2D> p1, pair<unsigned int, TeCoord2D> p2) const
	{
		if(p1.second.x() < p2.second.x())
			return true;	
		if(p1.second.x() > p2.second.x())
			return false;
		if(p1.second.y() < p2.second.y())
			return true;	
		if(p1.second.y() > p2.second.y())
			return false;
		if(p1.first < p2.first)
			return true;

		return false;
	}
};

// estrutura auxiliar utilizada no codigo do split
struct TeSIP
{
	vector<unsigned int>	indexes_;		
	TeCoord2D				coord_;
	bool					used_;

	TeSIP() 
	{
	}

	TeSIP(const TeCoord2D& coord)
	{
		indexes_.clear();
		coord_ = coord;		
		used_ = false;
	}

	bool exists(const unsigned int index)
	{
		for(unsigned int i = 0; i < indexes_.size(); i++)
		{
			if(index == indexes_[i])
			{
				return true;
			}
		}
		return false;
	}

	unsigned int getFirstIndex()
	{
		unsigned int first = indexes_[0];
		for(unsigned int i = 1; i < indexes_.size(); i++)
		{
			if(indexes_[i] < first)
			{
				first = indexes_[i];
			}
		}
		return first;
	}
	unsigned int getLastIndex()
	{
		unsigned int last = indexes_[0];
		for(unsigned int i = 1; i < indexes_.size(); i++)
		{
			if(indexes_[i] > last)
			{
				last = indexes_[i];
			}
		}
		return last;
	}
};

// funcao auxiliar
inline bool TeSEquals(const TeCoord2D& c1, const TeCoord2D& c2)
{
	return (c1.x() == c2.x()) && (c1.y() == c2.y());
}

// estrutura auxiliar do split
struct TeSFragment
{
	unsigned int	initialIndex_; //indice da coordenada inicial do fragmento
	TeLine2D		line_;

	TeSFragment()
	{
	}

	TeSFragment(const unsigned int &initialIndex, const TeCoord2D &coord)
	{
		initialIndex_ = initialIndex;
		line_.add(coord);
	}
};

// Make rings from split fragments
inline bool TeSMergeFragments(vector<TeSFragment> &fragments, vector<TeLinearRing>& rings)
{
	unsigned int fragmentsSize = fragments.size();

	TeOVERLAY::TeLineIndex fragmentsIndex;

	for(unsigned int i = 0; i < fragmentsSize; ++i)

		fragmentsIndex.insert(TeOVERLAY::TeLineIndex::value_type(fragments[i].line_[0], pair<unsigned int, TeLine2D>(0, fragments[i].line_)));
	return TeOVERLAY::TeMergeFragments(fragmentsIndex, rings);}


bool TeOVERLAY::TeSplitRing(TeLinearRing& ring, TeLineSet& ringsOut)
{
	ringsOut.clear();

	unsigned int ringSize = ring.size();

	if(ringSize == 0)
		return false;
	
//vector para armazenar os pontos da linha e sua respectiva posicao
	vector< pair<unsigned int, TeCoord2D> > vecCoords;

//adiciona os segmentos num vector de pontos, contendo o indice e a coordenada
	for(unsigned int i = 0; i < ringSize; i++)
	{
		vecCoords.push_back( pair<unsigned int, TeCoord2D>(i, ring[i]));
	}
	unsigned int vecCoordsSize = vecCoords.size();

	sort(vecCoords.begin(), vecCoords.end(), TeSplitCoordSort());

	vector<TeSIP> ips;

	bool usou = false;
	//varre todas as coordenadas procurando por pontos de interseccao
	for(unsigned int i = 1; i < vecCoordsSize; i++)
	{	
		//se o ponto corrente for igual ao anterior, entao existe interseccao
		if(TeSEquals(vecCoords[i-1].second, vecCoords[i].second))
		{
			//armazenamos o ponto de interseccao e todos os indices em que este ponto apareceu
			if(!usou)
			{
				//na primeira vez, criamos o ponto de interseccao
				TeSIP ip(vecCoords[i].second);
				ip.indexes_.push_back(vecCoords[i-1].first);
				ip.indexes_.push_back(vecCoords[i].first);

				ips.push_back(ip);

				usou = true;
			}
			else
			{
				//da segunda em diante, apenas anotamos o indice em que este ponto apareceu
				ips[ips.size() - 1].indexes_.push_back(vecCoords[i].first);				
			}
		}
		else
		{
			usou = false;
		}		
	}

	//so existe interseccao entre o inicio e o fim do pol
	if(ips.size() == 1)
	{
		ringsOut.add(ring);	
		return true;
	}

	//Gera os fragmentos
	vector< TeSFragment > fragments;
	TeSFragment currentFragment(0, ring[0]);

	//vamos remontar a linha
	for(unsigned int i = 1; i < ringSize; i++)
	{		
		unsigned int currentIPindex = 0;
		bool isIP = false;
		//verifica se o ponto corrente e um ponto de interseccao
		for(unsigned j = 0; j < ips.size(); j++)
		{
			if(ips[j].exists(i))
			{				
				currentIPindex = j;
				isIP = true;
				break;
			}
		}		

		///adiciono a coordenada corrente ao fragmento corrente
		currentFragment.line_.add(ring[i]);

		if(isIP)
		{
			//se fechou um anel, adiciona na lista de aneis
			// e remove o ponto de interseccao corrente
			if(currentFragment.line_.isRing())
			{
				ringsOut.add(currentFragment.line_);
			}
			else
			{
				//verifica se o IP ja passamos por este IP alguma vez
				//se ja passamos, vamos formar um poligono com os fragmentos entre eles
				if(ips[currentIPindex].used_)
				{					
					//se nao foi formado um anel, pegamos todos os fragmentos entre estes pontos de interseccao e
					//tentamos formar um anel
					unsigned int firstIndex = ips[currentIPindex].getFirstIndex();
					unsigned int lastIndex = ips[currentIPindex].getLastIndex();

					vector<unsigned int> remover;
					vector< TeSFragment > auxFragments;
					auxFragments.push_back(currentFragment);
					for(unsigned int j = 0; j<fragments.size(); j++)
					{			
						if(fragments[j].initialIndex_ >= firstIndex &&  fragments[j].initialIndex_ < lastIndex )
						{
							remover.push_back(j);
							auxFragments.push_back(fragments[j]);
						}
					}
					vector<TeLinearRing> r;
					if(auxFragments.size() >=2 && TeSMergeFragments(auxFragments, r))
					{
						for(unsigned int t = 0; t < r.size(); t++)
						{
							ringsOut.add(r[t]);
						}

						for(int j = remover.size()-1; j >= 0; j--)
						{
							fragments.erase(fragments.begin() + remover[j]);
						}
						ips[currentIPindex].used_ = false;
					}
					else
					{
						fragments.push_back(currentFragment);
					}
				}
				else
				{
					fragments.push_back(currentFragment);
					ips[currentIPindex].used_ = true;
				}				
			}

			currentFragment = TeSFragment (i, ring[i]);			
		}
	}
	
	if(fragments.size() > 1)
	{
		vector<TeLinearRing> r;
		if(TeSMergeFragments(fragments, r))
		{
			for(unsigned int t = 0; t < r.size(); t++)
			{
				ringsOut.add(r[t]);
			}
		}
		else
		{
			return false;
		}
	}
	else if(fragments.size() == 1)
	{
		return false;	
	}

	for(unsigned int i = 0; i < ringsOut.size(); i++)
	{
		ringsOut[i].objectId(ring.objectId());
		ringsOut[i].geomId(ring.geomId());
	}

	return true;
} 

bool TeOVERLAY::TeSplitRings(vector<TeLinearRing>& rings, vector<TeLinearRing>& ringsOut)
{
	ringsOut.clear();

	for(unsigned int i = 0; i < rings.size(); ++i)
	{
		TeLineSet ringsAux;
		
		if(TeOVERLAY::TeSplitRing(rings[i], ringsAux) == false)
			return false;

		for(unsigned int j = 0; j < ringsAux.size(); ++j)
			ringsOut.push_back(ringsAux[j]);
	}

	return true;
}

bool TeOVERLAY::TeCloneLine(const TeLine2D& lineIn, TeLine2D& lineOut, const unsigned int& minPts)
{
	lineOut.clear();

	unsigned int lineSize = lineIn.size();

	if(lineSize < 2)
		return false;

	lineOut.add(lineIn[0]);

	for(unsigned int i = 1; i < lineSize; ++i)
	{
		if(!TeEquals(lineIn[i - 1], lineIn[i]))
		{
			lineOut.add(lineIn[i]);
		}
	}

	if(lineOut.size() < minPts)
		return false;

	return true;
}


bool TeOVERLAY::TeClonePolygon(const TePolygon& polIn, TePolygon& polOut)
{
	polOut.clear();

	unsigned int polSize = polIn.size();

	if(polSize == 0)
		return false;

	for(unsigned int i = 0; i < polSize; ++i)
	{
		TeLine2D lineOut;

		if(!TeCloneLine(polIn[i], lineOut, 4))
			return false;

		TeLinearRing ringOut(lineOut);

		polOut.add(ringOut);
	}

	return true;
}

bool TeOVERLAY::TeClonePolygonSet(const TePolygonSet& polsIn, TePolygonSet& polsOut)
{
	polsOut.clear();

	unsigned int polsSize = polsIn.size();

	if(polsSize == 0)
		return false;

	for(unsigned int i = 0; i < polsSize; ++i)
	{
		TePolygon pol;

		if(!TeClonePolygon(polsIn[i], pol))
			return false;

		polsOut.add(pol);
	}

	return true;
}

