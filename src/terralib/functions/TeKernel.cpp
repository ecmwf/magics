#include "TeKernel.h"
#include <TeDatabase.h>
#include <TeGeometryAlgorithms.h>
#include <math.h>

char* TeKAlgNames[5] = {"Quartico",
						 "Normal",
						 "Uniforme",
						 "Triangular",
						 "Exponencial Neg"};

/*
bool
ApplyKernel(TeDatabase* db, TeTheme* base, TeTheme* event, double radius, const string& destTable, const string& attrLink, const string& columnName)
{
	TePolygonSet pBase;
	TePointSet pEvents;
	
	TeLayer* lBase = base->layer();
	TeLayer* lEvent = event->layer();

	if (!lBase->getPolygons(pBase) || !lEvent->getPoints(pEvents))
		return false;
// O ideal seria termos metodos para recuperar geometrias de temas 
	// base->getPolygons(pBase);
	// event->getPoints(pEvents);

	TeAttributeRep newCol;
	newCol.name_ = columnName;
	newCol.type_ = TeREAL;

	if(db->addColumn (destTable, newCol ) == false)
		return false;

	for (int i=0; i< pBase.size(); i++)
	{
		TePolygon poly = pBase[i];
		TeCoord2D basePoint = TeFindCentroid( poly );
		double kernel = TeKernel (basePoint, radius, pEvents.begin(), pEvents.end());
		string update = "UPDATE "+destTable+" SET "+columnName+"="+Te2String(kernel);
		update += " WHERE "+attrLink+"='"+poly.objectId()+"'";
		db->execute (update);
	}

	return true;
}
*/

/**
 * Calcula a media geometrica de um conjunto de valores atraves da 
 * utilizacao de logaritmos
 * Implementado pelo Andre
 **/
double TeKernelGeometricMean(int nVal, double* val)
{
   double mantissa, MediaE, MediaM;
   int expoente, i, Cont;
   int MediaETmp = 0;
   double MediaMTmp = 0;

   double log_two = log(2.0);
   Cont = 0;

   for (i=0; i<nVal; i++)
         if (val[i])
         {
            mantissa = frexp(val[i],&expoente);
            MediaMTmp = MediaMTmp+log(mantissa);
            MediaETmp = MediaETmp+expoente;
            Cont++;
         }

   MediaE = MediaETmp;
   MediaM = (MediaMTmp+(MediaE*log_two))/Cont;
   MediaM = exp(MediaM);

   return MediaM;
}


bool
ApplyKernel(TeDatabase* db, TeTheme* base, TeTheme* event,
			 const vector<TeCovariableDescriptor>& covDesc,
			 bool adaptive, double radius, const string& destTable, const string& attrLink, 
			 const string& columnName,
			 string& errorMessage)
{
	TePolygonSet pBase;
	TeEventVector pEvents;

	//Vetor com valor de kernel -- devera ser trocado para colocar iterador
	double* kValues = NULL;

	TeLayer* lBase = base->layer();

	TeLayer* lEvent;
	lEvent = event->layer();

	if (!lBase->getPolygons(pBase)) {
		errorMessage = "Camada base nao contem poligonos";
		return false;
	}

	// O ideal seria termos metodos para recuperar geometrias de temas 
	// base->getPolygons(pBase);
	// event->getPoints(pEvents);

	if (!TeRetrieveEventVector (db, event, covDesc, pEvents, errorMessage )) {
		errorMessage = "Nao foi possivel recuperar eventos";
		return false;
	}

	TeAttributeRep newCol;
	newCol.name_ = columnName;
	newCol.type_ = TeREAL;

	if(!db->addColumn (destTable, newCol )) {
		errorMessage = db->errorMessage();
		return false;
	}

	if (adaptive) {
		kValues = new double[pBase.size()];
		//Esta considerando como area do layer. Tem que alterar
		double baseArea = lBase->box().width() * lBase->box().height();
		radius = 0.68*pow(pEvents.size(),-0.2)*sqrt(baseArea);
	}

	double area, kernel;
	string updateQuery;
	TePolygon basePoly;
	TeCoord2D basePoint;

	unsigned int i;
	for (i=0; i< pBase.size(); i++)
	{
		basePoly = pBase[i];
		area = TeGeometryArea(basePoly);
		basePoint = TeFindCentroid(basePoly );
		kernel = TeKernel (TeQuartic, basePoint, area, radius, pEvents.begin(), pEvents.end());
		if (adaptive) {
			kValues[i] = kernel;
		}
		else {
			updateQuery = "UPDATE "+destTable+" SET "+columnName+"="+Te2String(kernel);
			updateQuery += " WHERE "+attrLink+"='"+  basePoly.objectId()+"'";
			if (!db->execute (updateQuery)) {
				errorMessage = db->errorMessage();
				return false;
			}
		}
	}

	double newRadius, meanKernel;
	//Verifica se eh adaptativo, tendo que recalcular
	if (adaptive) {
		//Calcula a media geometrica
		meanKernel = TeKernelGeometricMean(pBase.size(), kValues);

		//Calcula o novo raio e chama o kernel
		unsigned int i;
		for (i=0; i< pBase.size(); i++)
		{
			basePoly = pBase[i];
			area = TeGeometryArea(basePoly);
			basePoint = TeFindCentroid( basePoly );
			if (kValues[i]) {
				newRadius = radius*pow((meanKernel/kValues[i]),0.5); 
				kernel = TeKernel (TeQuartic, basePoint, area, newRadius, pEvents.begin(), pEvents.end());
			}
			else {
				kernel = 0.0;
			}
			updateQuery = "UPDATE "+destTable+" SET "+columnName+"="+Te2String(kernel);
			updateQuery += " WHERE "+attrLink+"='"+basePoly.objectId()+"'";
			if (!db->execute (updateQuery)) {
				errorMessage = db->errorMessage();
				return false;
			}
		}
	}

	errorMessage = "";
	return true;
}


double TeKernelQuartic(double tau, double distance, double intensity, double weigth) 
{
    if (distance > tau)
		return 0.0;

	return intensity * weigth * (3.0 / (tau * tau * TePI)) *
		pow(1 - (distance * distance)/ (tau * tau),2.0);
}

double TeKernelNormal(double tau, double distance, double intensity, double weigth)
{
	return intensity * weigth * (1.0 / (tau * tau * 2 * TePI)) *
		exp(-1.0 * (distance * distance)/ (2 * tau * tau));
}

double TeKernelUniform(double tau, double distance, double intensity, double weigth)
{
    if (distance > tau)
		return 0.0;

	return intensity * weigth;
}

double TeKernelTriangular(double tau, double distance, double intensity, double weigth)
{
    if (distance > tau)
		return 0.0;

	return intensity * weigth * (1.0 - 1.0/tau) * distance;
}

double TeKernelNegExponential(double tau, double distance, double intensity, double weigth)
{
    if (distance > tau)
		return 0.0;

	return intensity * weigth * exp(-3.0 * distance);

}

