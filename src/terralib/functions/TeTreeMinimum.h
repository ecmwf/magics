#ifndef TETREEMINIMUM_H
#define TETREEMINIMUM_H

#include <vector>
#include <map>
#include <string>

#include "TeSelectedObject.h"
#include "TeSpatialStatistics.h"
#include "TeLayer.h"

using namespace std;

class graph_link
{
public:
	string GeoId1_;
	string GeoId2_;
	double Dissimilarity_;

	~graph_link()
	{}


	graph_link() :
		GeoId1_(""),
		GeoId2_("")
	{}

	graph_link(const string& GeoId1, const string& GeoId2):
		GeoId1_(GeoId1),
		GeoId2_(GeoId2)
	{}

	graph_link(const string& GeoId1, const string& GeoId2, const double Dissimilarity)  :
		GeoId1_(GeoId1),
		GeoId2_(GeoId2),
		Dissimilarity_(Dissimilarity)
	{}

 
	void set_Dissimilatiry (const double Dissimilarity)
	{	Dissimilarity_ = Dissimilarity;	}

	bool compare_dissimilarity(const graph_link &graph_link1, const graph_link &graph_link2);
//	bool graph_link::operator<(const graph_link &graph_link1, const graph_link &graph_link2);
};



typedef vector<graph_link> graph;

typedef map<string,graph> matrix;

void TeCreateDissMatrix (TeSelectedObjectMap& Objects, TeProxMatrix& proxMatrix,
					   matrix& mat_diss, double& Diss_max);

void TeCreateMinimumTree ( TeSelectedObjectMap& Objects, matrix& DissMatrix, graph& Min_Tree, double Diss_max);

void TeCreateSubTrees( TeSelectedObjectMap& Objects, graph& Min_Tree,
					  TeSelectedObjectMap& result, int num_regioes);

#endif