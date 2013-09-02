#include "TeSpatialStatistics.h"
#include "TeDatabase.h"
#include "TeLayer.h"
#include "TeGeometryAlgorithms.h"

double 
TeMean ( TeSelectedObjectMap& bObjects )
{
	double mean = 0.;

	TeSelectedObjectMap::iterator it = bObjects.begin();

	while ( it != bObjects.end() )
	{
		mean += atof ( ( (*it).second ).properties_[0].value_.c_str() );
		++it;
	}

	mean /= bObjects.size();

	return mean;
}

double 
TeMedian ( TeSelectedObjectMap& bObjects )
{
	vector<double> values;

	TeSelectedObjectMap::iterator it = bObjects.begin();

	while ( it != bObjects.end() )
	{
		double d = atof ( ( (*it).second ).properties_[0].value_.c_str());
		values.push_back(d);
		++it;
	}
	sort ( values.begin(), values.end() );

	int index = values.size()/2;

	double median = values[index];
	return	median;

}

double
TeVariance ( TeSelectedObjectMap& bObjects, double mean )
{
	double var = 0.;

	TeSelectedObjectMap::iterator it = bObjects.begin();

	while ( it != bObjects.end() )
	{
		double val = atof ( ( (*it).second ).properties_[0].value_.c_str() );
		var += (  val - mean )* ( val - mean );
		++it;
	}
	if ( bObjects.size() > 1 ) 
		return var / ( bObjects.size() - 1 );
	else
		return var;
}

double 
TeStandardDeviation ( double variance )
{
	double sigma = sqrt(variance);
	return sigma;
}

void
TeNormalize ( TeSelectedObjectMap& /*objects*/, double /*mean*/, double /*variance*/ )
{

}

// Create proximity matrix
// iterate by all objects

void
TeCreateProxMatrix ( TeDatabase* db, TeSelectedObjectMap& objectMap, 
					 TeLayer& curLayer, TeProxMatrix& proxMatrix )
{
	// This function creates a proximity matrix based on adjacencty criterion
	// the values of the matrix are set to 1 or 0 

	// As a starting point, we must define the property we are measuring

	TeProperty prop;
	prop.attr_.semantic_ = "adjacency value"; // example
	prop.value_ = "1";

	// select table which contains polygons
	string polyTable = curLayer.tableName(TePOLYGONS);

	// for Oracle  
	if(!strcmp(db->dbmsName().c_str(),"OracleSpatial"))
	{
		map <string,bool> obj_looked;
		map <string,bool>::iterator viewobj;
		bool first = true;

		string sqlora = "select a.object_id, b.object_id from " + polyTable + " a, " + polyTable + " b where ";
		sqlora += "sdo_relate (a.spatial_data,b.spatial_data,'mask=TOUCH querytype=WINDOW') = 'TRUE' order by a.object_id";

		TeDatabasePortal *oraportal = db->getPortal();
		if(!oraportal->query(sqlora.c_str()))
			return;

		string cur_obj = "";
		TeSelectedObjectVector  Aneighbors;
		TeSelectedObjectVector  Bneighbors;
		map<string, TeSelectedObjectVector>::iterator  adjacents;

		bool selA = false;
		bool selB = false;

		while(oraportal->fetchRow())
		{
			// object A
			string objA = oraportal->getData(0);

			// object B
			string objB = oraportal->getData(1);
			viewobj = obj_looked.find(objB);
			if(viewobj != obj_looked.end())
				continue;

			// A neighbors
			if(strcmp(cur_obj.c_str(),objA.c_str()))
			{
				if(!first && selA)
					proxMatrix [cur_obj] = Aneighbors;

				cur_obj = objA;
				obj_looked [objA] = true;
				Aneighbors.clear();

				TeSelectedObjectMap::iterator Ait = objectMap.find(objA);
				if(Ait == objectMap.end())
					selA = false;
				else
				{
					selA = true;

					adjacents = proxMatrix.find(objA);
					if(adjacents != proxMatrix.end())
						Aneighbors = (*adjacents).second;
				}

				first = false;
			}

			if(!selA)
				continue;

			TeSelectedObjectMap::iterator Bit = objectMap.find(objB);
			if(Bit == objectMap.end())
				continue;

			TeSelectedObject Abo ( objB, prop );
			Aneighbors.push_back ( Abo );

			// B neighbors
			Bneighbors.clear();
			adjacents = proxMatrix.find(objB);
			if(adjacents != proxMatrix.end())
				Bneighbors = (*adjacents).second;

			TeSelectedObject Bbo ( objA, prop );
			Bneighbors.push_back ( Bbo );
			proxMatrix [objB] = Bneighbors;
		}
		if(!first && selA)
			proxMatrix [cur_obj] = Aneighbors;

		return;
	}

	// new implementation - relational model (Access, MySql)
	// portal for geometries
	string querygeom = "select " + polyTable + ".* from " + polyTable;
	querygeom += " order by object_id, geom_id, parent_id, num_holes DESC";

    TeDatabasePortal *geomportal = db->getPortal();

	// second portal - candidate adjacent geometries
	string poladj = "select t1.geom_id as geomid, t1.object_id as objid, t2.* from " + polyTable + " as t1, ";
	poladj += polyTable + " as t2 where not(t1.upper_x < t2.lower_x or t1.lower_x >  t2.upper_x or ";
	poladj += "t1.upper_y < t2.lower_y or t1.lower_y > t2.upper_y) ";
	poladj += "and t1.geom_id <> t2.geom_id and t1.object_id <> t2.object_id and t1.geom_id = t1.parent_id ";
	poladj += "order by t1.object_id, t2.parent_id, t2.num_holes DESC";

	TeDatabasePortal *adjportal = db->getPortal();

	if(!(adjportal->query(poladj.c_str()) && adjportal->fetchRow()))
		return;

	bool flagB = true;

	map <string,bool> obj_passed;
	map <string,bool>::iterator objview;

	if(geomportal->query(querygeom.c_str()) && geomportal->fetchRow())
	{
		bool flagA = true;
		bool isselA = true;
		do
		{
			TeSelectedObjectVector  neighborsA;

			string objIdA = geomportal->getData(1);

			map<string, TeSelectedObjectVector>::iterator  nbA = proxMatrix.find(objIdA);
			if(nbA != proxMatrix.end())
				neighborsA = (*nbA).second;

			TePolygon polyA;
			flagA = geomportal->fetchGeometry(polyA);

			TeSelectedObjectMap::iterator itA = objectMap.find(objIdA);
			if(itA == objectMap.end())
				isselA = false;

			// reading adjportal
			string objIdAA = adjportal->getData(1);
			bool isselB = true;

			while(!strcmp(objIdA.c_str(),objIdAA.c_str()))
			{
				string objIdB = adjportal->getData(3);

				TePolygon polyB;
				flagB = adjportal->fetchGeometry(polyB);

				objview = obj_passed.find(objIdB);
				if(objview == obj_passed.end())
				{
					TeSelectedObjectMap::iterator itB = objectMap.find(objIdB);
					if(itB == objectMap.end())
						isselB = false;

					if(isselA && isselB)
					{
						if ( TeTouch ( polyA, polyB, 0.001 ) )
						{
							TeSelectedObject bo ( objIdB, prop );
							neighborsA.push_back ( bo );
						

							TeSelectedObjectVector  neighborsB;
							map<string, TeSelectedObjectVector>::iterator  nbB = proxMatrix.find(objIdB);
							if(nbB == proxMatrix.end())
							{
								TeSelectedObject boB ( objIdA, prop );
								neighborsB.push_back ( boB );
								proxMatrix [ objIdB ] = neighborsB;
							}
							else
							{
								neighborsB = (*nbB).second;
								TeSelectedObject boC ( objIdA, prop );
								neighborsB.push_back ( boC );
								proxMatrix [ objIdB ] = neighborsB;

							}
						}
					}
				}
					// fetchGeometry reads next record
				if(!flagB)
					break;
				objIdAA = adjportal->getData(1);
			}

			proxMatrix [ objIdA ] = neighborsA;
			obj_passed [objIdA ] = true;

			if(!flagB)
				break;
		}
		while (flagA);
		delete adjportal;
		delete geomportal;
	}
	else
		delete adjportal;
}


double
TeMoranIndex ( TeSelectedObjectMap& objects,
			   TeProxMatrix& proxMatrix )
{
	double mean = TeMean ( objects );

	double var  = TeVariance ( objects, mean );

	double moran = 0;

	TeSelectedObjectMap::iterator it = objects.begin();

	while ( it != objects.end() )
	{
		TeSelectedObject obj = ( *it ).second;

		double normObjVal = atof ( obj. properties_[0].value_.c_str() ) - mean;

		TeSelectedObjectVector  neighbors = proxMatrix [ obj.geoid_ ];

		double li = 0.;
		double weightSum = 0.;

		unsigned int i;
		for ( i = 0; i < neighbors.size(); i++ )
		{
			TeSelectedObject neigh = neighbors [ i ] ;

			// retrieve the property value associated to this neighbor 
			double val = atof ( objects [ neigh.geoid_ ]. properties_[0].value_.c_str() );

			// normalize the property
			double normNeighVal =  val - mean;

			// find the weight associated with the neighbor
			double weight = atof ( neigh.properties_[0].value_.c_str() );

			li +=  weight * ( normNeighVal )  * ( normObjVal );

			weightSum += weight;
		}

		if (weightSum != 0.)
			li /= weightSum;
		
		moran += li;
		++it;
	}

	if ( objects.size() > 1 )
		return moran /  ( var * ( objects.size() - 1 ) );
	else 
		return moran / var ;
}

void
TeLocalMean ( TeSelectedObjectMap& objects,
			  TeProxMatrix& proxMatrix,
			  TeSelectedObjectMap& result )
{
	result.clear();
	TeSelectedObjectMap::iterator it = objects.begin();

	while ( it != objects.end() )
	{
		TeSelectedObject obj = ( *it ).second;

		TeSelectedObjectVector  neighbors = proxMatrix [ obj.geoid_ ];

		double lm = 0;
		double weightSum = 0.;

		if (neighbors.size() > 0)
		{
			unsigned int i;
			for ( i = 0; i < neighbors.size(); i++ )
			{
				TeSelectedObject neigh = neighbors [ i ] ;

				// retrieve the property value associated to this neighbor 
				TeSelectedObjectMap::iterator ito = objects.find(neigh.geoid_);
				if(ito != objects.end())
				{
					double val = atof ( objects [ neigh.geoid_ ]. properties_[0].value_.c_str() );

				// find the weight associated with the neighbor
					double weight = atof ( neigh. properties_[0].value_.c_str() );

					lm +=  weight * val;

					weightSum += weight;
				}
			}
			lm /= weightSum;
		}
		
		string semantic = obj.properties_[0].attr_.semantic_;
		obj.properties_[0].attr_.semantic_ = "Local Mean of " + semantic;
		obj.properties_[0].value_ = Te2String ( lm );
		result [ obj.geoid_] = obj;
		++it;
	}
}

void
TeGStatistics ( TeSelectedObjectMap& objects, TeProxMatrix& proxMatrix,
			  TeSelectedObjectMap& GResult, TeSelectedObjectMap& GStarResult )
{
	GResult.clear();
	GStarResult.clear();

	TeSelectedObjectMap::iterator it = objects.begin();
	double totalSum = 0.;
	double excludSum = 0.;

	while (it != objects.end() )
	{
		totalSum += atof( (*it).second.properties_[0].value_.c_str() );
		it++;
	}

	it = objects.begin();
	while ( it != objects.end() )
	{
		TeSelectedObject obj = ( *it ).second;

		TeSelectedObjectVector  neighbors = proxMatrix [ obj.geoid_ ];

		double G = 0;
		double GStar = 0;

		double weightSum = 0.;

		excludSum = totalSum - atof ( objects [ obj.geoid_ ]. properties_[0].value_.c_str() );;

		if (neighbors.size() > 0)
		{
			unsigned int i;
			for ( i = 0; i < neighbors.size(); i++ )
			{
				TeSelectedObject neigh = neighbors [ i ] ;

				// retrieve the property value associated to this neighbor 
				TeSelectedObjectMap::iterator ito = objects.find(neigh.geoid_);
				if(ito != objects.end())
				{
					double val = atof ( objects [ neigh.geoid_ ]. properties_[0].value_.c_str() );

				// find the weight associated with the neighbor
					double weight = atof ( neigh. properties_[0].value_.c_str() );

					if (neigh.geoid_ != obj.geoid_)
						G +=  weight * val;
					//else
						//excludSum = totalSum - val;

					GStar +=  weight * val;

					weightSum += weight;
				}
			}
			G = G / excludSum;
			GStar = GStar / totalSum;
		}
		
		obj.properties_[0].value_ = Te2String ( G );
		GResult [ obj.geoid_] = obj;
		obj.properties_[0].value_ = Te2String ( GStar );
		GStarResult [ obj.geoid_] = obj;
		++it;
	}
}

double
TeGlobalMoranSignificance ( TeSelectedObjectMap& objects, vector<double>& deviations,
						   TeProxMatrix& proxMatrix, unsigned int permutationsNumber, double moranIndex)
{
	vector<double> permutationsResults(permutationsNumber);
	double permutationIndex, significance, sum;
	unsigned int objectsNumber = objects.size();
	TeSelectedObjectMap changedObjects, result;
	
	double variance = TeSecondMoment (deviations.begin(), deviations.end(), 0);

	unsigned int i;
	for (i = 0; i < permutationsNumber; i++)
	{
		srand(i);
		vector<double> copy = deviations;	
		TeSelectedObjectMap::iterator it = objects.begin();
		vector<double>::iterator it_double = copy.begin();
		unsigned int j;
		for (j = 0; j < objectsNumber; j++)
		{
			TeSelectedObject obj = (*it).second;
			int randon = (int) ( (rand() * (objectsNumber-j) )/32768 );
			double value = copy[randon];
			copy.erase(it_double+randon);
			obj.properties_[0].value_ = Te2String ( value );
			changedObjects[obj.geoid_] = obj;
			it++;
		}

		TeLocalMean ( changedObjects, proxMatrix, result);
		vector<double> Z(objectsNumber), Wz(objectsNumber);
		it = changedObjects.begin();
		int index = 0;
		while ( it != changedObjects.end() )
		{
			string id = it->first;
			Z[index] = atof( (*it).second.properties_[0].value_.c_str() );
			Wz[index] = atof( result[id].properties_[0].value_.c_str() );
			index++;
			it++;
		}

		// calculate index of Moran of the permutation i
		sum = 0;
		unsigned int k;
		for (k = 0; k < objectsNumber; k++)
			sum += (Z[k] * Wz[k]);
		permutationIndex = sum / (variance*objectsNumber);
	
		permutationsResults[i] = permutationIndex;
	}

	// verify the significance
	int position = 0;
	significance = 0;
	unsigned int k;
	for (k = 0; k < permutationsNumber; k++)
	{
		if (moranIndex > permutationsResults[k])
			position++;
	}
	if ( moranIndex >= 0)
		significance = (double) position/( permutationsNumber + 1 );
	else
		significance = (double) (permutationsNumber-position)/(permutationsNumber+1);
		
	return significance;
}
