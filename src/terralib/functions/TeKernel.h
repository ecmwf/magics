#ifndef __TEKERNELH
#define __TEKERNELH

#include <TeGeometryAlgorithms.h>
#include <TeTheme.h>
#include <TeDatabase.h>
#include <TeEvent.h>

using namespace std;

//! Kernel Algorithm types
enum TeKernelAlgorithms 
 {TeQuartic, TeNormal, TeUniform, TeTriangular, TeNegExponential};

#define TeKAlgorithmsTotal 5

//! Labels para interface -- ver um jeito melhor de fazer, talvez na descricao do
// parametro em tabela
extern char* TeKAlgNames[5]; 

/*! Kernel algorithms */
double TeKernelQuartic(double tau, double distance, double intensity, double weigth);
double TeKernelNormal(double tau, double distance, double intensity, double weigth);
double TeKernelUniform(double tau, double distance, double intensity, double weigth);
double TeKernelTriangular(double tau, double distance, double intensity, double weigth);
double TeKernelNegExponential(double tau, double distance, double intensity, double weigth);


//! Kernel function for one point
/*! Receives as input the point where kernel function must be evaluated,
//  bandwith, iterator for events, type of function
*/
template<typename itin>
double TeKernel (TeKernelAlgorithms type, TeCoord2D& pt,
				 double area,
				 double tau, itin begin, itin end) {
	double k=0;
	double localK;
	while(begin != end)
	{
		TeCoord2D p = begin->location();
		double intensity = begin->intensity();
		double distance = TeDistance (pt,p);
		double weigth = 1.0;
		switch(type) {
		case TeQuartic:
			localK = TeKernelQuartic(tau,distance,intensity,weigth);
			break;
		case TeNormal:
			localK = TeKernelNormal(tau,distance,intensity,weigth);
			break;
		case TeTriangular:
			localK = TeKernelTriangular(tau,distance,intensity,weigth);
			break;
		case TeNegExponential:
			localK = TeKernelNegExponential(tau,distance,intensity,weigth);
			break;
		case TeUniform:
			localK = TeKernelUniform(tau,distance,intensity,weigth);
			break;
		}
		k += localK;
		begin++;
	}
	return k * area;
}




bool
ApplyKernel(TeDatabase* db, TeTheme* base, TeTheme* event, double radius, const string& destTable, const string& attrLink, const string& columnName);

//! Function to apply kernel. Retrieves event vector based on covariable description
bool
ApplyKernel(TeDatabase* db, TeTheme* base, TeTheme* event,
			 const vector<TeCovariableDescriptor>& covDesc, bool adaptive,
			 double radius, const string& destTable, const string& attrLink,
			 const string& columnName, string& errorMessage);
#endif

