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

/*
   TODO: 1. tentar otimizar as rotinas auxilixares (elementares) de forma que o sentido do segmento nao seja importante
            e que possamos garantir a orientacao da linha que iremos tracar ao redor de um segmento de reta (acabar dom didswap!)
*/

#include "TeBufferRegion.h"
#include "TeOverlay.h"
#include "TeGeometry.h" 
#include "TeGeometryAlgorithms.h"

#include <map>
#include <algorithm>
#include <cmath>

using namespace std;

namespace TeBUFFERREGION
{

//---------------- Auxiliary Constants ----------------//
const double bufferPI       = 3.14159265358979323846;
const double doubleBufferPI = 6.28318530717958647692;
const double halfBufferPI   = 1.57079632679489661923;

//---------------- Auxiliary Functions ----------------//

/** Rotina auxiliar que define o angulo inicial utilizado para tracar o buffer ao redor de um segmento de reta (rotina de uso interno - nao deve ser utilizada diretamente).
		\param first	primeiro ponto que define o segmento de reta: a rotina pode inverter seu conteudo com o parametro "last"
		\param last		segundo ponto que define o segmento de reta: a rotina pode inverter seu conteudo com o parametro "first"
		\param ang		angulo que define uma linha perpendicular ao segmento passando pela coordenada first
		\param didswap	indica se as coordenadas first e last foram trocadas (se foi realizado o swap delas)
		\note As coordenadas first e last devem ser necessariamente diferentes.
  */
inline void TeFindInitialAng(TeCoord2D& first, TeCoord2D& last, double& ang, bool& didswap)
{
	double xLength  = 0.0;
	double val_ang  = 0.0;
	
	if(first.x_ > last.x_)
	{
	    swap(first, last);
		didswap = true;
	}
	else if(first.x_ == last.x_)
	{
		if(first.y_ < last.y_)
		{
			swap(first, last);
			didswap = true;
		}
	}

// cumprimeto do segmento ao longo do eixo "x"
	xLength = last.x_ - first.x_;

// cumprimento do segmento 
	double segmentLength = TeDistance(first, last);

// inclinacao do segmento
	val_ang = xLength / segmentLength;

	if(val_ang > 1.0)	// erros de arredondamento podem definir um valor um "pouco" maior que 1, e nesse caso arredondamos para 1.
	{
		val_ang = 1.0;
	}
	else if(val_ang < -1.0)	// erros de arredondamento podem definir um valor um "pouco" menor que -1, e nesse caso arredondamos para 1.
	{
		val_ang = -1.0;
	}
	else if(fabs(val_ang) <= 0.000000000000001)	// erros de arredondamento podem definir um valor um "pouco" maior que zero, e nesse caso arredondamos para 0.
	{
		ang = 0.0;
	}
	else
	{
		if(first.y_ == last.y_)		// se o segmento for horizontal, o angulo inicial sera: (pi / 2)
			ang = halfBufferPI;
		else
			if(first.y() > last.y())
				ang = asin(val_ang);
			else
				ang = acos(val_ang) + halfBufferPI;
	}

	return;
}

/** Rotina auxiliar que traca um buffer elementar ao redor de um segmento, arredondando apenas um dos cantos do segmento: a linha do buffer tera orientacao horaria.
		\param first	       primeiro ponto que define o segmento de reta.
		\param last		       segundo ponto que define o segmento de reta.
		\param bufferDistance  distancia utilizada para tracar o buffer.
		\param numPoints       numero de pontos utilizado na definicao da semi-circunferencia ao redor dos vertices.
		\param bufferLevels	   numero de niveis do buffer.
		\param bufferPols	   linhas no sentido horario que definem a linha de buffer de cada nivel.
  */
inline void TeElementarBuffer(TeCoord2D first, TeCoord2D last, const double& bufferDistance,
                              const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygon>& bufferPols)
{
	bufferPols.clear();

	double ang = 0.0;		// angulo que define uma linha perpendicular ao primeiro vertice do segmento
	bool didswap = false;	// indica se a rotina TeFindInitialAng fez o swap das coordenadas first e last

	TeFindInitialAng(first, last, ang, didswap);

	ang += bufferPI;	// como vamos tracar as linhas no sentido horario, vamos comecar 180 graus depois e entao vamos decrementando

	double deltaAng = bufferPI / (numPoints - 1);	// angulo de cada setor que compoem o buffer
	double c = 0.0; // variavel que contera o resultado do cosseno de um angulo
	double s = 0.0;	// variavel que contera o resultado do seno de um angulo
	double x = 0.0;	// variavel que contera a coordenada x transladada
	double y = 0.0; // variavel que contera a coordenada y transladada

// incializa a linha que delimitara cada nivel de buffer
	vector<TeLine2D> bufferLines;

	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TeLine2D newLine;
		bufferLines.push_back(newLine);
	}

// verifica se o segmento foi ou nao invertido: isso eh importante para podermos manter a orientacao da linha de buffer
// que estamos criando
	if(didswap)
	{
// para cada extremo, calcula uma semi-circunferencia
		for(unsigned int i = 1; i < numPoints; ++i)
		{
			c = cos(ang);
			s = sin(ang);

// repete o procedimento para cada nivel do buffer
			for(unsigned int j = 0; j < bufferLevels; ++j)
			{
				x = first.x() + bufferDistance * (j + 1) * c ;
				y = first.y() + bufferDistance * (j + 1) * s ;
				bufferLines[j].add(TeCoord2D(x, y));
			}
	
			ang -= deltaAng;
		}

// coloca o ultimo ponto do primeiro arco
		c = cos(ang);
		s = sin(ang);		

		for(unsigned int j = 0; j < bufferLevels; ++j)
		{
// coloca o ultimo ponto do primeiro arco
			x = first.x() + bufferDistance * (j + 1) * c; 
			y = first.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));

// coloca o primeiro ponto do segundo arco
			x = last.x() + bufferDistance * (j + 1) * c ; 
			y = last.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));
		}

// coloca o ultimo ponto do segundo arco
		ang -= bufferPI;

		c = cos(ang);
		s = sin(ang);

		for(unsigned int j = 0; j < bufferLevels; ++j)
		{		
			x = last.x() + bufferDistance * (j + 1) * c; 
			y = last.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));

// fecha a linha: que jah se encontra no sentido horario
			bufferLines[j].add(bufferLines[j][0]);

// cria o poligono correspondente e coloca no vetor de saida
			TePolygon pol;
			pol.add(TeLinearRing(bufferLines[j]));
			bufferPols.push_back(pol);
		}
	}
	else
	{
// coloca o primeiro ponto do primeiro arco
		c = cos(ang);
		s = sin(ang);

		for(unsigned int j = 0; j < bufferLevels; ++j)
		{		
			x = first.x() + bufferDistance * (j + 1) * c; 
			y = first.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));
		}

// coloca o ultimo ponto do primeiro arco
		ang -= bufferPI;

		c = cos(ang);
		s = sin(ang);
		
		for(unsigned int j = 0; j < bufferLevels; ++j)
		{		
// coloca o ultimo ponto do primeiro arco
			x = first.x() + bufferDistance * (j + 1) * c; 
			y = first.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));

// coloca o primeiro ponto do segundo arco
			x = last.x() + bufferDistance * (j + 1) * c ; 
			y = last.y() + bufferDistance * (j + 1) * s ;
			bufferLines[j].add(TeCoord2D(x, y));
		}

// coloca os pontos do ultimo arco
		for(unsigned int i = 1; i < numPoints; ++i)
		{
			ang -= deltaAng;

			c = cos(ang);
			s = sin(ang);

			for(unsigned int j = 0; j < bufferLevels; ++j)
			{		
				x = last.x() + bufferDistance * (j + 1) * c ; 
				y = last.y() + bufferDistance * (j + 1) * s ;
				bufferLines[j].add(TeCoord2D(x, y));
			}
		}

// fecha cada linha, que jah deve estar no sentido horario
		for(unsigned int j = 0; j < bufferLevels; ++j)
		{
			bufferLines[j].add(bufferLines[j][0]);

// cria o poligono correspondente e coloca no vetor de saida
			TePolygon pol;
			pol.add(TeLinearRing(bufferLines[j]));
			bufferPols.push_back(pol);
		}
	}

	return;
}

/** Rotina auxiliar que traca um buffer elementar ao redor de um segmento, arredondando os dois cantos do segmento: a linha do buffer tera orientacao horaria.
		\param first	       primeiro ponto que define o segmento de reta.
		\param last		       segundo ponto que define o segmento de reta.
		\param bufferDistance  distancia utilizada para tracar o buffer.
		\param numPoints       numero de pontos utilizado na definicao da semi-circunferencia ao redor dos vertices.
		\param bufferLevels	   numero de niveis do buffer.
		\param bufferPols	   linhas no sentido horario que definem a linha de buffer de cada nivel.
  */
inline void TeFullElementarBuffer(TeCoord2D first, TeCoord2D last, const double& bufferDistance,
							      const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygon>& bufferPols)
{
	bufferPols.clear();

	double ang = 0.0;		// angulo que define uma linha perpendicular ao primeiro vertice do segmento
	bool didswap = false;	// indica se a rotina TeFindInitialAng fez o swap das coordenadas first e last

	TeFindInitialAng(first, last, ang, didswap);

	ang += bufferPI;	// como vamos tracar as linhas no sentido horario, vamos comecar 180 graus depois e entao vamos decrementando

	double deltaAng = bufferPI / (numPoints - 1);	// angulo de cada setor que compoem o buffer
	double c = 0.0; // variavel que contera o resultado do cosseno de um angulo
	double s = 0.0;	// variavel que contera o resultado do seno de um angulo
	double x = 0.0;	// variavel que contera a coordenada x transladada
	double y = 0.0; // variavel que contera a coordenada y transladada

// incializa a linha que delimitara cada nivel de buffer
	vector<TeLine2D> bufferLines;

	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TeLine2D newLine;
		bufferLines.push_back(newLine);
	}

// para cada extremo, calcula uma semi-circunferencia
	for(unsigned int i = 1; i < numPoints; ++i)
	{
		c = cos(ang);
		s = sin(ang);

		for(unsigned int j = 0; j < bufferLevels; ++j)
		{		
			x = first.x() + bufferDistance * (j + 1) * c; 
			y = first.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));
		}

		ang -= deltaAng;
	}

	c = cos(ang);
	s = sin(ang);

	for(unsigned int j = 0; j < bufferLevels; ++j)
	{	
// coloca o ultimo ponto da primeira semi-circunferencia
		x = first.x() + bufferDistance * (j + 1) * c; 
		y = first.y() + bufferDistance * (j + 1) * s;
		bufferLines[j].add(TeCoord2D(x, y));

// coloca o primeiro ponto da segunda semi-circunferencia
		x = last.x() + bufferDistance * (j + 1) * c; 
		y = last.y() + bufferDistance * (j + 1) * s;
		bufferLines[j].add(TeCoord2D(x, y));
	}

// coloca os demais pontos da segunda semi-circunferencia
	for(unsigned int i = 1; i < numPoints; ++i)
	{
		ang -= deltaAng;

		c = cos(ang);
		s = sin(ang);

		for(unsigned int j = 0; j < bufferLevels; ++j)
		{		
			x = last.x() + bufferDistance * (j + 1) * c; 
			y = last.y() + bufferDistance * (j + 1) * s;
			bufferLines[j].add(TeCoord2D(x, y));
		}
	}

// fecha cada linha, que jah deve estar no sentido horario
	for(unsigned int j = 0; j < bufferLevels; ++j)
	{
		bufferLines[j].add(bufferLines[j][0]);

// cria o poligono correspondente e coloca no vetor de saida
		TePolygon pol;
		pol.add(TeLinearRing(bufferLines[j]));
		bufferPols.push_back(pol);
	}

	return;
}

/** Rotina auxiliar que traca um buffer ao redor de uma linha, tracando varios niveis independentemente um do outro: para cada nivel de buffer, o anel externo do poligono estara no sentido horario e os buracos estarao no sentido anti-horario.
		\param line	           linha para a qual os buffers serao tracados.
		\param bufferDistance  distancia utilizada para tracar o buffer.
		\param numPoints       numero de pontos utilizados para construcao dos semi-circulos de cada segmento.
		\param bufferLevels    numero de niveis do buffer.
		\param polVec          vetor de saida com os poligonos de buffer de cada nivel: nao aninhados, isto e, possivelmente estao sobrepostos, sendo o ratamento dado em outras rotinas de mais alto nivel (que utilizam esta como suporte).
  */
inline bool TeLineBuffer(const TeLine2D& line, const double& bufferDistance, const unsigned int& numPoints,
						 const unsigned int& bufferLevels, vector<TePolygon>& polVec)
{
	polVec.clear();

	unsigned int nStep = line.size();

	if(nStep < 2)
		return false;

// vetor auxiliar que contera em cada posicao um polygonset
// com todos os poligonos elementares de um determinado nivel de buffer
	vector<TePolygonSet> psAux;

	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TePolygonSet pset;
		psAux.push_back(pset);
	}

// para linhas fechadas poderemos tracar somente buffer elementares com metade dos arcos
// enquanto que para linhas abertas, o buffer do primeiro segmento sera feito com um buffer elementar completo (os dois semi-circulos serao tracados).
// a seguir, computaremos os buffers elementares de cada nivel.
	if(line.isRing())
	{
		for(unsigned int i = 1; i < nStep; ++i)
		{
// gera os poliginos elementares a partir dos segmentos definidos por cada par de vertice
			vector<TePolygon> pols;
			TeElementarBuffer(line[i - 1], line[i], bufferDistance, numPoints, bufferLevels, pols);

			for(unsigned int j = 0; j < bufferLevels; ++j)
				psAux[j].add(pols[j]);
		}
	}
	else
	{
// para linhas abertas, o primeiro segmento tem um tratamento diferenciado enquanto os demais nao terao
		vector<TePolygon> pols;
		TeFullElementarBuffer(line[0], line[1], bufferDistance, numPoints, bufferLevels, pols);

		for(unsigned int j = 0; j < bufferLevels; ++j)
			psAux[j].add(pols[j]);

		for(unsigned int i = 2; i < nStep; ++i)
		{
// gera os poliginos elementares a partir dos segmentos definidos por cada par de vertice
			TeElementarBuffer(line[i - 1], line[i], bufferDistance, numPoints, bufferLevels, pols);

			for(unsigned int j = 0; j < bufferLevels; ++j)
				psAux[j].add(pols[j]);
		}
	}

// agora, com os buffers elementares de cada nivel jah computados, vamos fazer
// a uniao dos buffers elementares de cada nivel individualmente
	for(unsigned int j = 0; j < bufferLevels; ++j)
	{
		TePolygonSet polSetOut;

// caso a uniao falhe, interrompe a execucao sinalizando o problema!
		if(TeOVERLAY::TePairUnion(psAux[j], polSetOut) == false)
			return false;

// teoricamente o buffer de cada nivel sera formado por um unico poligono com ou sem buracos.
		if(polSetOut.size() != 1)
			return false;

		polVec.push_back(polSetOut[0]);
	}

	return true;
}

/** Rotina auxiliar que traca um buffer interno (negativo) no interior de um poligono, tracando varios niveis independentemente um do outro: para cada nivel de buffer, o anel externo do poligono estara no sentido horario e os buracos estarao no sentido anti-horario.
		\param pol	           poligono para a qual os buffers serao tracados.
		\param bufferDistance  distancia utilizada para tracar o buffer.
		\param numPoints       numero de pontos utilizados para construcao dos semi-circulos de cada segmento.
		\param bufferLevels    numero de niveis do buffer.
		\param polsVec         vetor de saida com os poligonos de buffer de cada nivel: jah aninhados, isto e, os poligonos do nivel "n" jah consideram os do nivel "n-1".
  */
inline bool TeInsideBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polsVec)
{
	polsVec.clear();	

	unsigned int nRings = pol.size();

	//vetor de polygonset, onde cada poligonset está associado a um nivel
	//cada nivel contem os buffers internos do pai
	vector<TePolygonSet> internalBuffers;
	
	//vetor de polygonset, onde cada poligonset está associado a um nivel
	//cada nivel contem os buffers externos dos buracos
	vector<TePolygonSet> externalBuffers;

	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TePolygonSet pset;
		polsVec.push_back(pset);
		TePolygonSet pset2;
		internalBuffers.push_back(pset2);
		TePolygonSet pset3;
		externalBuffers.push_back(pset3);
	}

	// para cada anel vamos tracar o buffer e armazenar somente a parte de interesse: 
	// no caso de aneis externos vamos
	// ficar com o buffer interno e no caso de buracos vamos ficar com os buffer externos
	for(unsigned int i = 0; i < nRings; ++i)
	{
		vector<TePolygon> buffVec;
		
		if(TeLineBuffer(pol[i], bufferDistance, numPoints, bufferLevels, buffVec) == false)
			return false;

		if(i == 0)
		{
// para o anel externo ficamos com os aneis internos do buffer
			for(unsigned int j = 0; j < bufferLevels; ++j)
			{
				unsigned int rsize = buffVec[j].size();
				for(unsigned int k = 1; k < rsize; ++k)
				{
					TePolygon paux; paux.add(buffVec[j][k]); //comeca em 1 ate k
					internalBuffers[j].add(paux);
				}
			}
		}
		else
		{
// para um anel interno ficamos com os aneis externos do buffer
			for(unsigned int j = 0; j < bufferLevels; ++j)
			{
				TePolygon paux; paux.add(buffVec[j][0]); 
				externalBuffers[j].add(paux);
			}
		}
	}
	//para cada nivel, calcular o buraco do resultado que será
	//a diferenca entre os buffers internos e 
	//a uniao dos buffers externos 
	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TePolygonSet holes;

		if(externalBuffers[i].size()>0)
		{
			TePolygonSet externalBuffersUnion;
			//uniao dos buffers externos
			if(TeOVERLAY::TePairUnion(externalBuffers[i], externalBuffersUnion) == false)
				return false;

			// o buffer interno ao pai pode nao existir se a distancia for muito grande  
			if(internalBuffers[i].size()>0)
			{
				//diferenca
				if(TeOVERLAY::TeDifference(internalBuffers[i], externalBuffersUnion, holes) == false)
					return false;
			}

			//se o resultado da diferenca der vazio, significa que os buffers internos entao
			//contidos nos buffer externos
			//assim temos que adicionar como buracos, os buracos originais
			if(holes.size()==0)
			{
				TePolygon h;
				for(unsigned j=1; j<pol.size(); ++j) // buracos originais 
				{
					h.add(pol[j]); 
					holes.add(h);
				}
			}
		}
		else
		{
			//o unico buraco e o internal buffer do pai
			holes = internalBuffers[i];
		}
		
		//montar o buffer final
		TePolygon result;
		result.add(pol[0]); // pai do original
		for(unsigned j=0; j<holes.size(); ++j) // buracos calculados
            result.add(holes[j][0]);
		
		polsVec[i].add(result);
	}

	return true;
}

/** Rotina auxiliar que traca um buffer externo (positivo) ao redor de um poligono, tracando varios niveis independentemente um do outro: para cada nivel de buffer, o anel externo do poligono estara no sentido horario e os buracos estarao no sentido anti-horario.
		\param pol	           poligono para a qual os buffers serao tracados.
		\param bufferDistance  distancia utilizada para tracar o buffer.
		\param numPoints       numero de pontos utilizados para construcao dos semi-circulos de cada segmento.
		\param bufferLevels    numero de niveis do buffer.
		\param polsVec         vetor de saida com os poligonos de buffer de cada nivel: jah aninhados, isto e, os poligonos do nivel "n" jah consideram os do nivel "n-1".
  */
inline bool TeOutsideBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polsVec)
{
	polsVec.clear();	

	unsigned int nRings = pol.size();

// jah coloca um objeto polygonset correspondente a cada nivel
	vector<TePolygonSet> outPutPolsVec;

	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TePolygonSet pset;
		outPutPolsVec.push_back(pset);
		TePolygonSet pset2;
		polsVec.push_back(pset2);
	}

// para cada anel vamos tracar o buffer e armazenar somente a parte de interesse: no caso de aneis externos vamos
// ficar com o buffer externo e no caso de buracos vamos ficar com os buffer internos
	for(unsigned int i = 0; i < nRings; ++i)
	{
		vector<TePolygon> buffVec;
		
		if(TeLineBuffer(pol[i], bufferDistance, numPoints, bufferLevels, buffVec) == false)
			return false;

		if(i == 0)
		{
// para o anel externo ficamos com o anel externo do buffer
			for(unsigned int j = 0; j < bufferLevels; ++j)
			{
				TePolygon paux; paux.add(buffVec[j][0]);
				outPutPolsVec[j].add(paux);
			}
		}
		else
		{
// para um anel interno ficamos com os aneis internos do buffer
			for(unsigned int j = 0; j < bufferLevels; ++j)
			{
				unsigned int rsize = buffVec[j].size();

				for(unsigned int k = 1; k < rsize; ++k)
				{
					TePolygon paux; paux.add(buffVec[j][k]);
					outPutPolsVec[j].add(paux);
				}
			}
		}
	}

	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
// no caso de buffers positivos (para fora) pelo menos o buffer do anel externo devera aparecer em cada nivel
		if(outPutPolsVec[i].size() == 0)
			return false;

		TePolygonSet psetOut = outPutPolsVec[i];

// LOGICA PARA O ANEL EXTERNO
		if(i == 0)
		{
// o primeiro buffer pegara como buraco o anel externo original
			psetOut[0].add(pol[0]);	
			polsVec[i].add(psetOut[0]);
		}
		else
		{
			TeLine2D laux;
			laux.copyElements(outPutPolsVec[i - 1][0][0]);
			psetOut[0].add(TeLinearRing(laux));
			polsVec[i].add(psetOut[0]);
		}

// se nao temos buracos no corrente, entao nao precisamos nos preocupar com diferencas e etc...
		if(psetOut.size() == 1)
			continue;

		
// LOGICA PARA OS ANEIS INTERNOS
		if(i == 0)
		{			
// vamos montar um polygonset com os buracos do poligono original
			TePolygonSet psetOriginalInnerRings;

			for(unsigned int j = 1; j < nRings; ++j)
			{
				TePolygon paux; paux.add(pol[j]);
				psetOriginalInnerRings.add(paux);
			}

// agora vamos montar um polygonset com os buracos (que jah estao como poligonos) do primeiro nivel
			TePolygonSet currentPset;

			for(unsigned int j = 1; j < psetOut.size(); ++j)
			{
				currentPset.add(psetOut[j]);
			}

// agora fazemos a diferena dos originais com o do primeiro nivel
			TePolygonSet resultPset;

			if(TeOVERLAY::TeDifference(psetOriginalInnerRings, currentPset, resultPset) == false)
				return false;

			for(unsigned int j = 0; j < resultPset.size(); ++j)
			{
				polsVec[i].add(resultPset[j]);
			}
		}
		else
		{
// vamos montar um polygonset com os buracos do "n-1" e um com o corrente ("n")
			TePolygonSet previousPset;

			for(unsigned int j = 1; j < outPutPolsVec[i - 1].size(); ++j)
			{
				previousPset.add(outPutPolsVec[i - 1][j]);
			}

			TePolygonSet currentPset;

			for(unsigned int j = 1; j < psetOut.size(); ++j)
			{
				currentPset.add(psetOut[j]);
			}

// agora fazemos a diferena do nivel anterior com o do nivel corrente
			TePolygonSet resultPset;

			if(TeOVERLAY::TeDifference(previousPset, currentPset, resultPset) == false)
				return false;

			for(unsigned int j = 0; j < resultPset.size(); ++j)
			{
				polsVec[i].add(resultPset[j]);
			}
		}
	}

	return true;
}

inline bool TeInOutBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polsVec)
{
	polsVec.clear();	

	unsigned int nRings = pol.size();

	//vetor de polygonset, onde cada polygonset está associado a um nivel
	//cada nivel contem os buffers internos e externos  
	// de cada anel do poligono
	vector<TePolygonSet> polVecBuffers;
	
	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		TePolygonSet pset;
		polsVec.push_back(pset);
		TePolygonSet pset2;
		polVecBuffers.push_back(pset2);
	}

	// para cada anel vamos tracar o buffer e armazenar o poligono gerado (com aneis externo e internos)
	for(unsigned int i = 0; i < nRings; ++i)
	{
		vector<TePolygon> buffVec;
		
		if(TeLineBuffer(pol[i], bufferDistance, numPoints, bufferLevels, buffVec) == false)
			return false;

		//adicona o buffer gerado em cada polygonset
		for(unsigned int j=0; j<buffVec.size(); ++j)
			polVecBuffers[j].add(buffVec[j]);

	}


    //para cada nivel, fazer a uniao de todos os buffers gerados
	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		//uniao dos buffers 
		if(TeOVERLAY::TePairUnion(polVecBuffers[i], polsVec[i]) == false)
			return false;
	}

	return true;
}

}	// end namespace TeBUFFERREGION

//---------------- Buffer Algorithm ----------------//

bool TeBUFFERREGION::TeBufferRegion(const TeCoord2D& coord, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygon>& bufferPols)
{
// limpa o vetor de poligonos de saida
	bufferPols.clear();

// cria um vetor de linhas que contera a linha de buffer a cada nivel
	vector<TeLine2D> contourLines;

// cria a linha de cada nivel e jah acrescenta a coordenada inicial do canto esquerdo
	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
// linha de buffer do i-th nivel
		TeLine2D newLine;
		newLine.add(TeCoord2D((coord.x() - (bufferDistance * (i + 1))), (coord.y())));

		contourLines.push_back(newLine);
	}

	double initialAng = bufferPI;							// angulo inicial
	const double deltaAng = doubleBufferPI / numPoints;		// angulo de cada setor que compoem o buffer

	double x = 0.0;	// variavel que contera a coordenada x transladada
	double y = 0.0;	// variavel que contera a coordenada y transladada
	double c = 0.0;	// variavel que contera o resultado do cosseno de um angulo
	double s = 0.0;	// variavel que contera o resultado do seno de um angulo


// calcula as coordenadas que formarao a linha de buffer de cada nivel
	for(unsigned int i = 1; i < numPoints; ++i)
	{
		initialAng -= deltaAng;
		
		c = cos(initialAng);
		s = sin(initialAng);

// gera o ponto equivalente para cada nivel de buffer
		for(unsigned int k = 0; k < bufferLevels; ++k)
		{		
			x = coord.x_ + bufferDistance * (k + 1) * c; 
			y = coord.y_ + bufferDistance * (k + 1) * s;

			contourLines[k].add(TeCoord2D(x, y));
		}
	}

// fecha cada uma das linhas de buffer,
// repetindo o primeiro ponto e cria os poligonos de buffer de cada nivel formado pela respectiva
// linha de buffer
	for(unsigned int i = 0; i < bufferLevels; ++i)
	{
		contourLines[i].add(contourLines[i][0]);		

		TePolygon pol;
		pol.add(TeLinearRing(contourLines[i]));
		bufferPols.push_back(pol);
	}

// coloca o anel externo do nivel "n-1" como anel interno do nivel "n"
	for(unsigned int i = 1; i < bufferLevels; ++i)
	{
// clona a linha externa do nivel "n-1"
		TeLine2D laux;
		laux.copyElements(bufferPols[i - 1][0]);

// deixa no sentido anti-horario a linha que servira de buraco para o buffer do nivel "n"
		reverse(laux.begin(), laux.end());

// e, finalmente, coloca a linha do nivel "n-1" como buraco do nivel "n"
		bufferPols[i].add(TeLinearRing(laux));
	}

	return true;
}

bool TeBUFFERREGION::TeBufferRegion(const TeCoord2D& coord, const double& bufferDistance, const unsigned int& numPoints, TePolygon& pol)
{
	pol.clear();

// define um vetor temporario para chamar a rotina acima, que realmente implementa o buffer de um ponto
	vector<TePolygon> bufferPols;

// solicita o buffer de um unico nivel ao redor do ponto
	TeBufferRegion(coord, bufferDistance, numPoints, 1, bufferPols);

	if(bufferPols.empty())
		return false;

	pol = bufferPols[0];

	return true;
}

bool TeBUFFERREGION::TeBufferRegion(const TeLine2D& line, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polVec)
{
	polVec.clear();

	for(unsigned int level=0; level<bufferLevels; ++level)
	{
		TePolygonSet aux;
		polVec.push_back(aux);
	}

// para tracar o semi-circulo ao redor do canto do segmento precisamos de pelo menos tres pontos
	unsigned int cornerPts = numPoints;

	if(numPoints < 3)
		cornerPts = 3;

// traca os buffers de cada nivel ao redor da linha: cada posicao do vetor "outPutPolVec" contera o poligono de cada nivel
	vector<TePolygon> outPutPolVec;

	if(TeLineBuffer(line, bufferDistance, cornerPts, bufferLevels, outPutPolVec) == false)
		return false;

// se o numero de poligonos da saida for diferente do numero de niveis desejado, algum erro ocorreu
	if(outPutPolVec.size() != bufferLevels)
		return false;

// para cada nivel de buffer precisamos acomodar os aneis do nivel anterior:
// no caso do buffer de linhas, o buffer do nivel "n" eh exatamente a diferenca do resultado que temos ate aqui menos o resultado
// do buffer do nivel "n-1"
	polVec[0].add(outPutPolVec[0]);

	for(unsigned int i = 1; i < bufferLevels; ++i)
	{
		TePolygonSet currentPSet;  currentPSet.add(outPutPolVec[i]);
		TePolygonSet previousPSet; previousPSet.add(outPutPolVec[i - 1]);

		TePolygonSet polSetOut;

		if(TeOVERLAY::TeDifference(currentPSet, previousPSet, polSetOut) == false)
			return false;

// esperamos que o resultado da diferenca seja sempre um poligono (com zero ou mais buracos)
		//if(polSetOut.size() != 1)
		//	return false;
		//polVec.push_back(polSetOut[0]);

		for(unsigned int j=0; j<polSetOut.size(); ++j)
			polVec[i].add(polSetOut[j]);
	}

	return true;	
}

bool TeBUFFERREGION::TeBufferRegion(const TeLine2D& line, const double& bufferDistance, const unsigned int& numPoints, TePolygonSet& ps)
{
	ps.clear();

	vector<TePolygonSet> polVec;

	if(TeBufferRegion(line, bufferDistance, numPoints, 1, polVec) == false)
		return false;

	ps.copyElements(polVec[0]);
	
	return true;
}

bool TeBUFFERREGION::TeBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, const TeBufferType& buffType, vector<TePolygonSet>& polsVec)
{
	if(buffType == TeINSIDEBUFFER)
	{
		return TeInsideBufferRegion(pol, bufferDistance, numPoints, bufferLevels, polsVec);
	}
	else if(buffType == TeOUTSIDEBUFFER)
	{
		return TeOutsideBufferRegion(pol, bufferDistance, numPoints, bufferLevels, polsVec);
	}
	else
	{
		return TeInOutBufferRegion(pol, bufferDistance, numPoints, bufferLevels, polsVec);
	}	
}

bool TeBUFFERREGION::TeBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, TePolygonSet& ps)
{
	ps.clear();

	vector<TePolygonSet> pols;
	
	if(!TeBufferRegion(pol, bufferDistance, numPoints, 1, TeOUTSIDEBUFFER, pols))
		return false;

	ps = pols[0];

	return true;
}

bool TeBUFFERREGION::TeBufferRegion(const TePolygonSet& polSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, const TeBufferType& bufferType, vector<TePolygonSet>& polyVecOut)
{
	polyVecOut.clear();
	
	for(unsigned int j=0; j<polSetIn.size(); ++j)
	{
		vector<TePolygonSet> auxPolVec;
		if(!TeBUFFERREGION::TeBufferRegion(polSetIn[j], bufferDistance, numPoints, bufferLevels, bufferType, auxPolVec))
			return false;

		if(j==0)
		{
			for(unsigned int i=0; i<auxPolVec.size(); ++i)
				polyVecOut.push_back(auxPolVec[i]);
		}
		else
		{
			for(unsigned int i=0; i<auxPolVec.size(); ++i)
			{
				TePolygonSet polUnion;
				if(!TeOVERLAY::TeUnion(polyVecOut[i], auxPolVec[i], polUnion))
					return false;
				polyVecOut[i]=polUnion;
			}
		}
	}
	return true;
}


bool TeBUFFERREGION::TeBufferRegion(const TeLineSet& lineSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polyVecOut)
{
	polyVecOut.clear();

    for(unsigned int j=0; j<lineSetIn.size(); ++j)
	{
		vector<TePolygonSet> currentPolVec;
		if(!TeBUFFERREGION::TeBufferRegion(lineSetIn[j], bufferDistance, numPoints, bufferLevels, currentPolVec))
			return false;

		if(j==0)
		{
			for(unsigned int i=0; i<currentPolVec.size(); ++i)
				polyVecOut.push_back(currentPolVec[i]);
		}
		else
		{
			for(unsigned int i=0; i<currentPolVec.size(); ++i)
			{
				TePolygonSet polUnion;
				if(!TeOVERLAY::TeUnion(polyVecOut[i], currentPolVec[i], polUnion))
					return false;
				polyVecOut[i]=polUnion;
			}
		}
	}
	return true;
}


bool TeBUFFERREGION::TeBufferRegion(const TePointSet& pointSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polyVecOut)
{
	polyVecOut.clear();
	
	for(unsigned int j=0; j<pointSetIn.size(); ++j)
	{
		vector<TePolygon> currentPolVec;
		if(!TeBUFFERREGION::TeBufferRegion(pointSetIn[j].location(), bufferDistance, numPoints, bufferLevels, currentPolVec))
			return false;

		if(j==0)
		{
			for(unsigned int i=0; i<currentPolVec.size(); ++i)
			{
				TePolygonSet currentPolSet; currentPolSet.add(currentPolVec[i]);
				polyVecOut.push_back(currentPolSet);
			}
		}
		else
		{
			for(unsigned int i=0; i<currentPolVec.size(); ++i)
			{
				TePolygonSet polUnion, currentPolSet;
				currentPolSet.add(currentPolVec[i]);
				if(!TeOVERLAY::TeUnion(polyVecOut[i], currentPolSet, polUnion))
					return false;
				polyVecOut[i]=polUnion;
			}
		}
	}
	return true;
}

	
bool  TeBUFFERREGION::TeBufferRegion(const TeCellSet& cellSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, const TeBufferType& bufferType, vector<TePolygonSet>& polyVecOut)
{
	polyVecOut.clear();
	
	for(unsigned int j=0; j<cellSetIn.size(); ++j)
	{
		vector<TePolygonSet> auxPolVec;
		if(!TeBUFFERREGION::TeBufferRegion(TeMakePolygon(cellSetIn[j].box()), bufferDistance, numPoints, bufferLevels, bufferType, auxPolVec))
			return false;

		if(j==0)
		{
			for(unsigned int i=0; i<auxPolVec.size(); ++i)
				polyVecOut.push_back(auxPolVec[i]);
		}
		else
		{
			for(unsigned int i=0; i<auxPolVec.size(); ++i)
			{
				TePolygonSet polUnion;
				if(!TeOVERLAY::TeUnion(polyVecOut[i], auxPolVec[i], polUnion))
					return false;
				polyVecOut[i]=polUnion;
			}
		}
	}
	return true;
}

