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

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeFragmentation.h"
#include "TeGeometryAlgorithms.h"
#include <algorithm>

using namespace std;

// estrutura auxiliar para implementar o funtor de ordenacao do algoritmo sort da STL:
// os pontos de intersecao de um segmento de uma dada linha eh agrupado usando esse funtor.
struct ipRedOrder	// XY order
{
	bool operator()(const TeINTERSECTOR2::TeBoundaryIP& ip1, const TeINTERSECTOR2::TeBoundaryIP& ip2) const
	{
		if(ip1.redPartNum_ < ip2.redPartNum_)
			return true;

		if(ip1.redPartNum_ > ip2.redPartNum_)
			return false;

		if(ip1.redSegNum_ < ip2.redSegNum_)
			return true;

		if(ip1.redSegNum_ > ip2.redSegNum_)
			return false;

		return false;
	}
};

// estrutura auxiliar para implementar o funtor de ordenacao do algoritmo sort da STL:
// os pontos de intersecao de um segmento de uma dada linha eh agrupado usando esse funtor.
struct ipRedOrder2	// XY order
{
	bool operator()(const TeINTERSECTOR2::TeBoundaryIP& ip1, const TeINTERSECTOR2::TeBoundaryIP& ip2) const
	{
		if(ip1.redPolNum_ < ip2.redPolNum_)
			return true;

		if(ip1.redPolNum_ > ip2.redPolNum_)
			return false;

		if(ip1.redPartNum_ < ip2.redPartNum_)
			return true;

		if(ip1.redPartNum_ > ip2.redPartNum_)
			return false;

		if(ip1.redSegNum_ < ip2.redSegNum_)
			return true;

		if(ip1.redSegNum_ > ip2.redSegNum_)
			return false;

		if(ip1.coords_[0].x_ < ip2.coords_[0].x_)
			return true;

		if(ip1.coords_[0].x_ > ip2.coords_[0].x_)
			return false;

		if(ip1.coords_[0].y_ < ip2.coords_[0].y_)
			return true;

		if(ip1.coords_[0].y_ > ip2.coords_[0].y_)
			return false;
		
		if(ip1.coords_.size() < ip2.coords_.size())
			return true;

		if(ip1.coords_.size() > ip2.coords_.size())
			return false;

		if((ip1.coords_.size() == 2) && (ip2.coords_.size() == 2))
		{
			if(ip1.coords_[1].x_ < ip2.coords_[1].x_)
				return true;

			if(ip1.coords_[1].x_ > ip2.coords_[1].x_)
				return false;

			if(ip1.coords_[1].y_ < ip2.coords_[1].y_)
				return true;

			if(ip1.coords_[1].y_ > ip2.coords_[1].y_)
				return false;
		}

		return false;
	}
};

// estrutura auxiliar para colocar os pontos de intersecao ao longo de um segment ordenados,
// de modo a facilitar a fragmentacao do segmento.
struct TeIPAux
{
	double x_;
	double y_;
	int type_;	//0 - Unico; 1 - Inicial de Overlap; 2 - Final de Overlap

	TeIPAux(const double& x, const double& y, const int& t)
		: x_(x), y_(y), type_(t)
	{
	}
};

// estrutura auxiliar para implementar o funtor de ordenacao do algoritmo sort da STL:
// os pontos de intersecao de um segmento de uma dada linha serao ordenados por esse funtor.
struct sortDist
{
	TeCoord2D c_;	// cordenada de referencia

	sortDist(const TeCoord2D& c)
		: c_(c)
	{
	}

	bool operator()(const TeIPAux& ip1, const TeIPAux& ip2) const
	{
		double dist1 = ((ip1.x_ - c_.x_) * (ip1.x_ - c_.x_)) + ((ip1.y_ - c_.y_) * (ip1.y_ - c_.y_));
		double dist2 = ((ip2.x_ - c_.x_) * (ip2.x_ - c_.x_)) + ((ip2.y_ - c_.y_) * (ip2.y_ - c_.y_));
		if(dist1 < dist2)
			return true;
		return false;
	}
};


// mario + gribeiro - 2006-03
// remove pontos de intersecao com a mesma localizacao que foram reportados por segmentos diferentes,
// deixandoo vetor "ips" na sequencia correta para uso da etapa de fragmentacao do segmento.
// Essa rotin pode lancar uma excessao em um caso muito especial: um ponto final de overlap veio ante do ponto inicial de overlap.
inline void cleanIntersections(TeINTERSECTOR2::TeVectorBoundaryIP& ips, const TeCoord2D& refCoord)
{
	vector<TeIPAux> vecips;
	for(unsigned int i = 0; i < ips.size(); ++i)
	{
		if(ips[i].coords_.size() == 2)
		{
			double dist1 = ((ips[i].coords_[0].x_ - refCoord.x_) * (ips[i].coords_[0].x_ - refCoord.x_)) + ((ips[i].coords_[0].y_ - refCoord.y_) * (ips[i].coords_[0].y_ - refCoord.y_));
			double dist2 = ((ips[i].coords_[1].x_ - refCoord.x_) * (ips[i].coords_[1].x_ - refCoord.x_)) + ((ips[i].coords_[1].y_ - refCoord.y_) * (ips[i].coords_[1].y_ - refCoord.y_)); 

			if(dist1 < dist2)
			{
				TeIPAux ip1(ips[i].coords_[0].x_, ips[i].coords_[0].y_, 1);
				TeIPAux ip2(ips[i].coords_[1].x_, ips[i].coords_[1].y_, 2);
				vecips.push_back(ip1);
				vecips.push_back(ip2);
			}
			else
			{
				TeIPAux ip1(ips[i].coords_[0].x_, ips[i].coords_[0].y_, 2);
				TeIPAux ip2(ips[i].coords_[1].x_, ips[i].coords_[1].y_, 1);
				vecips.push_back(ip1);
				vecips.push_back(ip2);
			}			
		}
		else
		{
			TeIPAux ip(ips[i].coords_[0].x_, ips[i].coords_[0].y_, 0);
			vecips.push_back(ip);
		}
	}

	sort(vecips.begin(), vecips.end(), sortDist(refCoord));

    TeINTERSECTOR2::TeVectorBoundaryIP newips;

	int cont = 0;

	TeLine2D l;
	TeLineSet lset;

	for(unsigned int i = 0; i < vecips.size(); ++i)
	{
		TeCoord2D c(vecips[i].x_, vecips[i].y_);

		if(vecips[i].type_ == 0)
		{
			if(l.size() > 0)
			{
				if(TeEquals(c, l[l.size() - 1]))
				{
					continue;
				}
				else if(cont == 0)
				{
					lset.add(l);
					l = TeLine2D();
					l.add(c);
				}
				else
				{
					l.add(c);
				}
			}
			else
			{				
				l.add(c);
			}
		}
		else if(vecips[i].type_ == 1)
		{
			if(l.size() > 0)
			{
				if(TeEquals(c, l[l.size() - 1]))
				{
					++cont;
					continue;
				}
				else if(cont == 0)
				{
					lset.add(l);
					l = TeLine2D();
					l.add(c);
				}
				else
				{
					l.add(c);
				}
			}
			else
			{
				l.add(c);
			}

			++cont;
		}
		else //else if(vecips[i].type == 2)
		{
			if(l.size() > 0)
			{
				if(TeEquals(c, l[l.size() - 1]))
				{
					--cont;
					continue;
				}
				else if(cont == 0)
				{
					throw string("An overlap end point started before an overlap begin!");		// nao podia ter chegado ate aqui
				}
				else
				{
					l.add(c);
				}
			}
			else
			{
				l.add(c);
			}

			--cont;
		}
	}

	if(l.size() > 0)
	{
		lset.add(l);
	}

	for(unsigned int i = 0; i < lset.size(); ++i)
	{
		if(lset[i].size() == 1)
		{
			TeINTERSECTOR2::TeBoundaryIP newIp;
			newIp.coords_.push_back(lset[i][0]);
			newips.push_back(newIp);
		}
		else
		{
			for(unsigned int j = 1; j < lset[i].size(); ++j)
			{
				TeINTERSECTOR2::TeBoundaryIP newIp;
				newIp.coords_.push_back(lset[i][j-1]);
				newIp.coords_.push_back(lset[i][j]);
				
				newips.push_back(newIp);
			}
		}
	}

	ips.clear();
	ips = newips;
}

// rotina que fragmenta uma aresta se necessario: aplica-se no caso de segmentos que nao fazem overlap
inline void TeFragmentSegmentByNonOverlapping(const TeCoord2D& s1, const TeCoord2D& /* s2 */,
									   TeINTERSECTOR2::TeVectorBoundaryIP& ips, 
									   TeLine2D& currentFragment,
									   TeLineSet& fragments)
{
// vamos deixar os pontos de intersecao numa sequencoa adequada para fragmentarmos este segmento
	cleanIntersections(ips, s1);


// eh so fragmentar o segmento ateh o ultimo ponto de interseccao dele
	for(unsigned int k = 0; k < ips.size(); ++k)
	{
		if(TeEquals(ips[k].coords_[0], currentFragment[currentFragment.size() - 1]))	// Se o ponto de interseccao esta sobre o vehrtice do segmento
		{
			// fragmenta a linha caso haja mais de um ponto nela
			if(currentFragment.size() > 1)								
			{
				fragments.add(currentFragment);

				currentFragment = TeLine2D();
				currentFragment.add(ips[k].coords_[0]);
			}
			else
			{
				continue;
			}
		}
		else
		{
			currentFragment.add(ips[k].coords_[0]);

			fragments.add(currentFragment);

			currentFragment = TeLine2D();
			currentFragment.add(ips[k].coords_[0]);
		}
	}
}

// fragmenta uma aresta que possui pontos de intersecao de overlap
inline void TeFragmentSegmentByOverlapping(const TeCoord2D& s1, const TeCoord2D& /* s2 */,
									   TeINTERSECTOR2::TeVectorBoundaryIP& ips,
									   TeLine2D& currentFragment,
									   TeLineSet& fragments, TeLineSet& boundaryFragments)
{
// vamos deixar os pontos de intersecao numa sequencoa adequada para fragmentarmos este segmento
	cleanIntersections(ips, s1);

	unsigned int i = 0;
	
// eh so fragmentar o segmento ateh o ultimo ponto de interseccao dele
	for(i = 0; i < ips.size(); ++i)
	{
		if(TeEquals(ips[i].coords_[0], currentFragment[currentFragment.size() - 1]))	// Se o ponto de interseccao esta sobre o vehrtice do segmento
		{
			// fragmenta a linha caso haja mais de um ponto nela
			if(currentFragment.size() > 1)
			{
				fragments.add(currentFragment);

				currentFragment = TeLine2D();

				if(ips[i].coords_.size() == 2)
				{
					TeLine2D l;
					l.add(ips[i].coords_[0]);
					l.add(ips[i].coords_[1]);
					
					boundaryFragments.add(l);

					currentFragment.add(ips[i].coords_[1]);
				}
				else
				{
					currentFragment.add(ips[i].coords_[0]);
				}
			}
			else
			{
				if(ips[i].coords_.size() == 2)
				{
					TeLine2D l;
					l.add(ips[i].coords_[0]);
					l.add(ips[i].coords_[1]);
					
					boundaryFragments.add(l);

					currentFragment.clear();
					currentFragment.add(ips[i].coords_[1]);
				}
			}
		}
		else
		{

			currentFragment.add(ips[i].coords_[0]);

			if(ips[i].coords_.size() == 2)
			{
				fragments.add(currentFragment);

				currentFragment = TeLine2D();
				
				currentFragment.add(ips[i].coords_[1]);

				TeLine2D l;
				l.add(ips[i].coords_[0]);
				l.add(ips[i].coords_[1]);
				
				boundaryFragments.add(l);
			}
			else
			{
				fragments.add(currentFragment);

				currentFragment = TeLine2D();
				
				currentFragment.add(ips[i].coords_[0]);
			}			
		}
	}
}

// Adiciona o ponto de intersecao ao fragmento corrente, decidindo entre um ponto de intersecao e o ponto original
void TeAddPoint(TeLine2D& lFrag, TeLine2D& currentLine, TeINTERSECTOR2::TeVectorBoundaryIP& ipsAux, const unsigned int& j)
{
	bool found = false;

	for(unsigned int i = 0; i < ipsAux.size(); ++i)
	{
		if(TeEquals(ipsAux[i].coords_[0], currentLine[j]))
		{
			lFrag.add(ipsAux[i].coords_[0]);
			found = true;
			break;
		}
		else if(ipsAux[i].coords_.size() == 2)
		{
			if(TeEquals(ipsAux[i].coords_[1], currentLine[j]))
			{
				lFrag.add(ipsAux[i].coords_[1]);
				found = true;
				break;
			}			
		}
	}

	if(!found)
		lFrag.add(currentLine[j]);
}

// rotina principal de fragmentacao de linhas
void TeFragmentBoundary(const TeLineSet& lines, TeINTERSECTOR2::TeVectorBoundaryIP& ips, TeLineSet& boundaryFragments, TeLineSet& fragments)
{
// informacoes sobre os pontos de intersecao
	unsigned int currentIP = 0;
	unsigned int nthIP = ips.size();

	// Informacoes sobre numero de linhas
	unsigned int nthLine = lines.size();

	// Ordena os pontos de intersecao
	sort(ips.begin(), ips.end(), ipRedOrder());

	for(unsigned int i = 0; i < nthLine; ++i)
	{
		if((currentIP < nthIP) && (ips[currentIP].redPartNum_ == i))	
		{
			// Se a linha corrente possui um ponto de intersecao entao ela devera ser fragmentada
			TeLine2D& currentLine = lines[i];
			
			TeLine2D lFrag;

			//unsigned int currentSegment = 0;
			unsigned int nthSegment = currentLine.size() - 1;

			for(unsigned int j = 0; j < nthSegment; ++j)
			{
				if((currentIP < nthIP) && (j == ips[currentIP].redSegNum_) && (ips[currentIP].redPartNum_ == i))	// Se ainda tenho pontos de interseccao e 
				{															// o corrente pertence ao segmento atual
					// Copias todos os pontos de interseccao do segmento para
					// um vetor auxiliar e ja avanca o currentIP
					TeINTERSECTOR2::TeVectorBoundaryIP ipsAux;

					bool hasOverlap = false;

					while((currentIP < nthIP) && (ips[currentIP].redSegNum_ == j) && (ips[currentIP].redPartNum_ == i))
					{
						if(ips[currentIP].coords_.size() == 2)
							hasOverlap = true;

						ipsAux.push_back(ips[currentIP]);

						++currentIP;
					}

					// Adiciona o primeiro ponto da linha na lista do fragmento atual: mas precisamos decidir entre o vertice original ou o de intersecao => preferimos o de intersecao para amarrar com o segmento que intercepta este segmento
					if((lFrag.size() == 0) || TeDisjoint(currentLine[j], lFrag[lFrag.size() - 1]))
							TeAddPoint(lFrag, currentLine, ipsAux, j);

					if(hasOverlap)	// Se houverem pontos de interseccao que fazem overlap
					{						
						TeFragmentSegmentByOverlapping(currentLine[j], currentLine[j + 1], ipsAux, lFrag, fragments, boundaryFragments);
					}
					else	// Se nao houver pontos de interseccao que fazem overlap
					{
						TeFragmentSegmentByNonOverlapping(currentLine[j], currentLine[j + 1], ipsAux, lFrag, fragments);
					}
				}
				else
				{
					// Caso nao haja ponto de interseccao sobre o segmento,
					// apenas insere o ponto no fragmento de linha
					lFrag.add(currentLine[j]);
				}
			}
			
			if(lFrag.size() > 0)
			{
				if(TeDisjoint(currentLine[nthSegment], lFrag[lFrag.size() - 1]))
				{
					lFrag.add(currentLine[nthSegment]);

					fragments.add(lFrag);
				}
				else if(lFrag.size() > 1)
				{
					fragments.add(lFrag);
				}
			}
		}
		else
		{
			// Caso contrario, se a linha nao possui ponto de intersecao
			// eh so colocar ela na lista de linhas fragmentadas
			fragments.add(lines[i]);
		}
	}
	
	return;
}

// rotina principal de fragmentacao de poligonos
void TeFragmentBoundary(const TePolygonSet& polygons, TeINTERSECTOR2::TeVectorBoundaryIP& ips, TeLineSet& boundaryFragments, vector<pair<unsigned int, unsigned int> >& boundaryFragmentsIds, TeLineSet& fragments, vector<pair<unsigned int, unsigned int> >& fragmentsIds)
{
	// Informacoes sobre os pontos de intersecao
	unsigned int currentIP = 0;
	unsigned int nthIP = ips.size();	

	// Ordena os pontos de intersecao
	sort(ips.begin(), ips.end(), ipRedOrder2());

	// Informacoes sobre numero de poligonos
	unsigned int nthPol = polygons.size();

	for(unsigned int polyIndex = 0; polyIndex < nthPol; ++polyIndex)
	{
		unsigned int nthLine = polygons[polyIndex].size();

		for(unsigned int ringIndex = 0; ringIndex < nthLine; ++ringIndex)
		{
// temos intersecao no anel?
            if((currentIP < nthIP) && (ips[currentIP].redPartNum_ == ringIndex) && (ips[currentIP].redPolNum_ == polyIndex))	
			{
				// Se a linha corrente possui um ponto de intersecao entao ela devera ser fragmentada
				TeLine2D& currentLine = polygons[polyIndex][ringIndex];
				
				TeLine2D lFrag;

				//unsigned int currentSegment = 0;
				unsigned int nthSegment = currentLine.size() - 1;

				TeCoord2D first(0,0);

				for(unsigned int j = 0; j < nthSegment; ++j)
				{
// temos intersecao sobre o segmento corrente?
					if((currentIP < nthIP) && (j == ips[currentIP].redSegNum_) && (ips[currentIP].redPartNum_ == ringIndex) && (ips[currentIP].redPolNum_ == polyIndex))	// Se ainda tenho pontos de interseccao e 
					{
						// Copias todos os pontos de interseccao do segmento para
						// um vetor auxiliar e ja avanca o currentIP
						TeINTERSECTOR2::TeVectorBoundaryIP ipsAux;

						bool hasOverlap = false;

						while((currentIP < nthIP) && (ips[currentIP].redSegNum_ == j) && (ips[currentIP].redPartNum_ == ringIndex) && (ips[currentIP].redPolNum_ == polyIndex))
						{
							if(ips[currentIP].coords_.size() == 2)
								hasOverlap = true;

							ipsAux.push_back(ips[currentIP]);

							++currentIP;
						}

						// Adiciona o primeiro ponto da linha na lista do fragmento atual: mas precisamos decidir entre o vertice original ou o de intersecao => preferimos o de intersecao para amarrar com o segmento que intercepta este segmento
						if((lFrag.size() == 0) || TeDisjoint(currentLine[j], lFrag[lFrag.size() - 1]))
							TeAddPoint(lFrag, currentLine, ipsAux, j);

						if(hasOverlap)	// Se houverem pontos de interseccao que fazem overlap
						{						
							TeFragmentSegmentByOverlapping(currentLine[j], currentLine[j + 1], ipsAux, lFrag, fragments, boundaryFragments);
						}
						else	// Se nao houver pontos de interseccao que fazem overlap
						{
							TeFragmentSegmentByNonOverlapping(currentLine[j], currentLine[j + 1], ipsAux, lFrag, fragments);
						}
					}
					else
					{
						// Caso nao haja ponto de interseccao sobre o segmento,
						// apenas insere o ponto no fragmento de linha
						lFrag.add(currentLine[j]);
					}
				}
				
				if(lFrag.size() > 0)
				{
					if(TeDisjoint(currentLine[0], lFrag[lFrag.size() - 1]))
					{
						lFrag.add(currentLine[0]);

						fragments.add(lFrag);
					}
					else if(lFrag.size() > 1)
					{
						fragments.add(lFrag);
					}
				}
			}
			else
			{
				// Caso contrario, se a linha nao possui ponto de intersecao
				// e so colocar ela na lista de linhas fragmentadas
				fragments.add(polygons[polyIndex][ringIndex]);
			}

			unsigned int numFragmentos = fragments.size();

			for(unsigned int fragIdxCount = fragmentsIds.size(); fragIdxCount < numFragmentos; ++fragIdxCount)
				fragmentsIds.push_back(pair<unsigned int, unsigned int>(polyIndex, ringIndex));


//mario - 2006-03
			unsigned int numFragmentosFronteira = boundaryFragments.size();

			for(unsigned int fragIdxCount = boundaryFragmentsIds.size(); fragIdxCount < numFragmentosFronteira; ++fragIdxCount)
			{
				boundaryFragmentsIds.push_back(pair<unsigned int, unsigned int>(polyIndex, ringIndex));
			}
		}
	}
	
	return;
}

