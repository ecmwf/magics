#include <math.h>
#include <TeTreeMinimum.h>

bool 
compare_dissimilarity(const graph_link &graph_link1, const graph_link &graph_link2)
{
	if (graph_link1.Dissimilarity_ < graph_link2.Dissimilarity_) 
		return true;
	else
		return false;
}

void TeCreateDissMatrix (TeSelectedObjectMap& objects, TeProxMatrix& proxMatrix, 
					  matrix& mat_diss, double &Diss_max) 
{
	string GeoId1, GeoId2;
	TeSelectedObjectVector  neighbors;
	int num_neighbors;
	double val1, val2;
	int DISS_TYPE = 1;
	double Diss = 0.;	
	Diss_max = 0.;	
	graph graph_;
	int tamanho = 0;

	TeSelectedObjectMap::iterator it = objects.begin();
	int num_objects = objects.size();

	while ( it != objects.end() )
	{
		((*it).second).slice_ = 0.;
		GeoId1 = (*it).first;
		neighbors = proxMatrix [GeoId1];
		num_neighbors = neighbors.size();
		graph_.clear();
		tamanho = graph_.size();
		double difer = 0.;
		for (int i = 0; i< num_neighbors;   i++)
		{
			GeoId2 = neighbors[i].geoid_;
			TeSelectedObjectMap::iterator it2 = objects.find(GeoId2);
			//leitura dos valores
			for (int j = 0; j < (*it).second.properties_.size(); j++)
			{
				val1 = atof ( ((*it).second).properties_[j].value_.c_str() );
				val2 = atof ( ((*it2).second).properties_[j].value_.c_str() );		
				difer = val1 - val2;
				if (DISS_TYPE)
					Diss+= difer*difer;		// distancia euclidiana (y=1)
				else
					Diss+= double(fabs(difer));		// distancia quarteirao (y=0)
			}
			if (DISS_TYPE)
				Diss = sqrt(Diss);
			
			//	guarda maior valor
			if (Diss > Diss_max)
				Diss_max = Diss;

			//escrever na estrutura map de grafos
			if (GeoId1 != GeoId2)
				graph_.push_back(graph_link(GeoId1,GeoId2,Diss));
		}
		sort(graph_.begin(), graph_.end(), compare_dissimilarity); 
		mat_diss[GeoId1] = graph_;
		it++;
	}
}

void
TeCreateMinimumTree (TeSelectedObjectMap& objects, matrix& DissMatrix, 
					 graph& Min_Tree, double Diss_max)
{
	// GERAR ARVORE MINIMA
	map<int,string> nos_MimTree;  //container que armazenará nós da árvore minima
	// escolha do primeiro nó
	TeSelectedObjectMap::iterator it = objects.begin();

	string GeoId1 = (*it).first;	
	int num_nos = 0;	//número de nós na árvore mínima

	nos_MimTree[num_nos++] = GeoId1;
	string GeoId2 = DissMatrix[GeoId1][0].GeoId2_;  //A matriz possui as linhas ordenadas pela menor dissimilaridade
	nos_MimTree[num_nos++] = GeoId2;

	graph_link link1(GeoId1, GeoId2, DissMatrix[GeoId1][0].Dissimilarity_);
	Min_Tree.push_back(link1);

	double Diss_mim;
	graph::iterator it_link = DissMatrix[GeoId1].begin();
	DissMatrix[GeoId1].erase(it_link);	//apaga link já analisado

	int num_objects = objects.size();
	bool no_novo = false;

	while ((num_nos) < num_objects)
	{
		Diss_mim = Diss_max;
		no_novo = false;
		for (int i = 0; i < nos_MimTree.size(); i++)
		{
			string no_atual = nos_MimTree[i];
			if ((DissMatrix[no_atual].size() != 0) & (DissMatrix[no_atual][0].Dissimilarity_ < Diss_mim))
			{
				GeoId1 = DissMatrix[nos_MimTree[i]][0].GeoId1_;
				GeoId2 = DissMatrix[nos_MimTree[i]][0].GeoId2_;
				Diss_mim = DissMatrix[nos_MimTree[i]][0].Dissimilarity_;
				no_novo = true;
			}
		}

		// verifica se nó já pertence à árvore
		string no_atual;
		for (int j = 0; j < nos_MimTree.size(); j++)
		{
			no_atual = nos_MimTree[j];
			if(no_atual == GeoId2)
				no_novo = false;
		}

		// se nó é novo, inclui link na árvore mínima
		if (no_novo)
		{
			nos_MimTree[num_nos++] = GeoId2;
			graph_link link2(GeoId1, GeoId2, Diss_mim);
			Min_Tree.push_back(link2);
		}

		DissMatrix[GeoId1].erase(DissMatrix[GeoId1].begin());
	}
}


void
TeCreateSubTrees( TeSelectedObjectMap& objects,graph& Min_Tree, 
				 TeSelectedObjectMap& result, int num_regioes)
{ 
	double	SSA1 = 0., SSA2 = 0., SSTO = 0.;
	graph::iterator it1 = Min_Tree.begin();
	graph::iterator it2 = Min_Tree.begin();
	graph::iterator itMax = Min_Tree.begin();
	result.clear();
	TeSelectedObjectMap::iterator it = objects.begin();

	vector<string> subtree_vertexes_1, subtree1;
	vector<string> subtree_vertexes_2, subtree2;

	// há necessidade de zerar o slice!
	for (int regiao = 1; regiao < num_regioes; regiao++)
	{
		double link_cost = 0.;
		graph_link bigest_cost("","",0.); 
		int contagem = 0;
		it1 = Min_Tree.begin();
		while (it1 != Min_Tree.end())
		{
			graph MT_copy;
 
			string GeoId1 = it1->GeoId1_;
			string GeoId2 = it1->GeoId2_;
			subtree_vertexes_1.clear();			
			subtree_vertexes_2.clear();

			subtree_vertexes_1.push_back(GeoId1);
			subtree_vertexes_2.push_back(GeoId2);

			int actual_vert = 0;
			string new_vertixe;

			while (actual_vert < subtree_vertexes_1.size())
			{
				it2 = Min_Tree.begin();
				bool new_v = false;

				while (it2 != Min_Tree.end())
				{
					if ((subtree_vertexes_1[actual_vert] == it2->GeoId1_) && 
						(subtree_vertexes_2[0] != it2->GeoId2_ ))
					{
						new_vertixe = it2->GeoId2_; new_v = true;
					} else if ((subtree_vertexes_1[actual_vert] == it2->GeoId2_) && 
						(subtree_vertexes_2[0] != it2->GeoId1_ ))
					{
						new_vertixe = it2->GeoId1_; new_v = true;
					}
					if ((new_v) && (find(subtree_vertexes_1.begin(), subtree_vertexes_1.end(), new_vertixe) == subtree_vertexes_1.end()))
							subtree_vertexes_1.push_back(new_vertixe);
					it2++;
				}
				actual_vert++;
			}

			actual_vert = 0;
			while (actual_vert < subtree_vertexes_2.size())
			{
				it2 = Min_Tree.begin();
				bool new_v = false;

				while (it2 != Min_Tree.end())
				{
					if ((subtree_vertexes_2[actual_vert] == it2->GeoId1_) && 
						(subtree_vertexes_1[0] != it2->GeoId2_ ))
					{
						new_vertixe = it2->GeoId2_; new_v = true;
					} else if ((subtree_vertexes_2[actual_vert] == it2->GeoId2_) && 
						(subtree_vertexes_1[0] != it2->GeoId1_ ))
					{
						new_vertixe = it2->GeoId1_; new_v = true;
					}
					if ((new_v) && (find(subtree_vertexes_2.begin(), subtree_vertexes_2.end(), new_vertixe) == subtree_vertexes_2.end()))
							subtree_vertexes_2.push_back(new_vertixe);
					it2++;
				}
				actual_vert++;
			}				

			//Calcular o vetor média
			int tam1 = subtree_vertexes_1.size();
			int tam2 = subtree_vertexes_2.size();
			
			int attributes_number = ((*it).second).properties_.size();

			vector <double> sum1, sum2, total_sum;	
			vector<double> mean1, mean2, global_mean;
			for (int i = 0; i < attributes_number  ; i++)
			{
				sum1.push_back(0.);	sum2.push_back(0.);	total_sum.push_back(0.);
				mean1.push_back(0.); mean2.push_back(0.); global_mean.push_back(0.);
			}


			string GeoId;
			vector<string>::iterator it_s = subtree_vertexes_1.begin();

			double valor = 0.;
			while ( it_s != subtree_vertexes_1.end() )
			{
				GeoId = (*it_s).c_str();
				it = objects.find(GeoId);				
				for (int i = 0; i < attributes_number  ; i++)
				{
					valor = atof ( ((*it).second).properties_[i].value_.c_str() );
					sum1[i] += valor;
				}
				it_s++;
			}

			it_s = subtree_vertexes_2.begin();
			while ( it_s != subtree_vertexes_2.end() )
			{
				GeoId = (*it_s).c_str();
				it = objects.find(GeoId);				
				for (int i = 0; i < attributes_number ; i++)
				{
					valor = atof ( ((*it).second).properties_[i].value_.c_str() );
					sum2[i] += valor;
				}	
				it_s++;
			}

			for (int k = 0; k < attributes_number ; k++)
			{
				total_sum[k] = sum1[k] + sum2[k];
			}				


			for (int j = 0; j < attributes_number ; j++)
			{
				 mean1[j] = sum1[j]/tam1; 
				 mean2[j] =	sum2[j]/tam2;
				 global_mean[j] = total_sum[j]/(tam1+tam2);
			}			

			// Calculo do SSTO e SSA's		
			double SSA1 = 0.; SSA2 = 0.; SSTO = 0.; 
			double desvio1 = 0., desvio2 = 0., desvioGlobal = 0.;

			it_s = subtree_vertexes_1.begin();
			while ( it_s != subtree_vertexes_1.end() )
			{
				GeoId = (*it_s).c_str();
				it = objects.find(GeoId);				
				for (int i=0; i<attributes_number; i++)
				{
					valor = atof ( ((*it).second).properties_[i].value_.c_str() );
					desvio1 = valor - mean1[i];
					desvioGlobal = valor - global_mean[i];
					SSA1 += desvio1*desvio1;
					SSTO += desvioGlobal*desvioGlobal;
				}
				it_s++;
			}

			it_s = subtree_vertexes_2.begin();
			while ( it_s != subtree_vertexes_2.end() )
			{
				GeoId = (*it_s).c_str();
				it = objects.find(GeoId);
				for (int i=0; i<attributes_number; i++)
				{
					valor = atof ( ((*it).second).properties_[i].value_.c_str() );
					desvio2 = valor - mean2[i];
					desvioGlobal = valor - global_mean[i];
					SSA2 += desvio2*desvio2;
					SSTO += desvioGlobal*desvioGlobal;
				}
				it_s++;
			}
			
			// Calculo do custo de aresta
			double link_cost = SSTO - SSA1 - SSA2;

			if (link_cost > bigest_cost.Dissimilarity_)
			{
				bigest_cost.Dissimilarity_ = link_cost;
				bigest_cost.GeoId1_= GeoId1;
				bigest_cost.GeoId2_= GeoId2;
				subtree1 = subtree_vertexes_1;
				subtree2 = subtree_vertexes_2;
				itMax = it1; //armazena o iterator para o nó mais caro
			}

			it1++;
		}
		// excluir link
		string teste1 = itMax->GeoId1_;
		string teste2 = itMax->GeoId2_;
		Min_Tree.erase(itMax);

		// atualizar slice
		vector<string>::iterator it_s = subtree2.begin();
		int contador = 0;
		while (it_s != subtree2.end())
		{
			it = objects.find((*it_s).c_str());
			((*it).second).slice_ = regiao;						
			it_s++;
			contador++;
		}

	}

	// Transfer objects to map
//	TeSelectedObjectMap	*objectsMap = layer.objectMap();//->objectMap();
	it = objects.begin();
	while ( it != objects.end() )
	{
		string GeoId = (*it).first;
//		(*objectsMap)[GeoId] = (*it).second;
		TeSelectedObject obj = ( *it ).second;
		obj.properties_[0].attr_.semantic_ = "Cluster index";
		obj.properties_[0].value_ = Te2String (((*it).second).slice_ );
		result [ obj.geoid_] = obj;
		++it;
	}

}