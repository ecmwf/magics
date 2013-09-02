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
/*! \file   TeKdTree.h
    \brief  This file contains an implementation of kdtree data structures in main memory for two dimensions.
	\note   These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone because
	        THIS IS FOR INTERNAL USE ONLY.
    \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>    
*/

#ifndef  __TERRALIB_INTERNAL_KDTREE_H
#define  __TERRALIB_INTERNAL_KDTREE_H

#include "TeGeometry.h"

/** 
 * WARNING: These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone because
 *          the support and interfaces can be changed in future. THIS IS FOR INTERNAL USE ONLY.
 *
 */

namespace TeSAM
{
/** @defgroup PartitionAlgorithms Partition Algorithms
 *  Partition Algorithms.
 *  @{
 */

//! This function partition the conteiner in two parts: k-1 elements to the left (elements less than or equals to k-th element) and the right part with all other elements (elements greater than or equal to k-th element)
/*
	\param A             Conteiner of elements to partition
	\param kthElement    Position of the k-th element, around the conteiner will be pertitionated
	\param firstElement  Position of the first element
	\param lastElement   Position of the las element
	\param compFunctor   Functor to compare elements: implements the function "less than"

*/
template<class CONTAINER, class COMPFUNCTOR> 
void TeHoareFind(CONTAINER& A, const unsigned int& kthElement, const unsigned int& firstElement, const unsigned int& lastElement,  const COMPFUNCTOR& compFunctor)
{
	unsigned int m = firstElement;
	unsigned int n = lastElement;

// Test if the median is in the bounds
	if((kthElement < firstElement) || (kthElement > lastElement))
		return;	

// Do conteiner partition
	while(m < n)
	{		
		unsigned int i = m;
		unsigned int j = n;

		typename CONTAINER::value_type r = A[kthElement];

		while(i <= j)
		{
			while(compFunctor(A[i], r))
				++i;  

			while(compFunctor(r, A[j]))
				--j;
			
			if(i <= j)
			{
				typename CONTAINER::value_type w = A[i];
				A[i] = A[j];
				A[j] = w;
				
			    ++i;
				--j;
			}
		}

		if(kthElement <= j)			// if the meet point was to the right, so all points above j are greater than the k-th element
			n = j;
		else if(i <= kthElement)	// otherwise, if the meeting point was to the left of k-th, so all elements before i are already in the correct location
			m = i;
		else
			break;
	}
}

//! Partition the conteiner like a bidimensional K-d Tree using Hoare algorithms
/*
	\param dataSet					Conteinet elements to be sorted like a K-dTree
	\param first					Position of the first element in conteiner, where the sort will begin
	\param last						Position of the last element, where the sort ends
	\param level					Indicates the axis to begin the sort ('x' or 'y') and is used during the recursion process
	\param lessThanCompFunctorByX	Functor to compare elements along the 'x' axis
	\param lessThanCompFunctorByY	Functor to compare elements along the 'y' axis

  	The expected complexity is O(N log N), where N is the number of elements in conteiner.
*/
template<class CONTAINER, class LESSTHANX, class LESSTHANY> 
void kdsort(CONTAINER& dataSet, const unsigned int& first, const unsigned int& last, const char& level, const LESSTHANX& lessThanCompFunctorByX, const LESSTHANY& lessThanCompFunctorByY)
{
	const unsigned int kth = (last - first + 1u) / 2u;

	if(level == 'x')
	{
// Move data around X axis
		TeHoareFind(dataSet, first + kth, first, last, lessThanCompFunctorByX);

// Recursive sort the left half and right half
		if((first + kth) > first)
			kdsort(dataSet, first, first + kth - 1u, 'y', lessThanCompFunctorByX, lessThanCompFunctorByY);

		if((first + kth) < last)
			kdsort(dataSet, first + kth + 1u, last, 'y', lessThanCompFunctorByX, lessThanCompFunctorByY);
	}
	else
	{
// Move data around Y axis
		TeHoareFind(dataSet, first + kth, first, last, lessThanCompFunctorByY);

// Recursive sort the left half and right half
		if((first + kth) > first)
			kdsort(dataSet, first, first + kth - 1u, 'x', lessThanCompFunctorByX, lessThanCompFunctorByY);

		if((first + kth) < last)
			kdsort(dataSet, first + kth + 1u, last, 'x', lessThanCompFunctorByX, lessThanCompFunctorByY);
	}
}
/** @} */

//! Kd-tree node type for nodes with single elements (used by template instantiation).
struct TL_DLL kd_node_data_single_tag {};

//! Kd-Tree node type for nodes with multuple elements (used by template instantiation).
struct TL_DLL kd_node_data_set_tag {};


//! Class that represents a node of a TeKdTree
/*!
	Each node contains a pointer to its left and right subtree (NULL if it is not set),
	one key used for insertion of the data into the tree.

	WARNING: <BR>

	1. The key must have methods called x() and y(). <BR>
	
	2. These kind of node stores the data in each node. <BR>

	3. The nodes may contains one single element (kd_node_data_single_tag) or a set of values (kd_node_data_set_tag). <BR>
	
	4. If the node type is kd_node_data_single_tag than NodeData and NodeDataItem are the same types. And if
	   one entry with the same key already exist, so they will be overwrite. <BR>
	   
	5. If the node type is kd_node_data_set_tag than NodeData mus have a method called push_back(NodeDataItem)
	   that permits to store elements with the same key in the node. <BR>
 */
template<class NodeKey, class NodeData, class NodeDataItem,
         class NodeDataTag = kd_node_data_single_tag>
class TeKdTreeNode
{
	protected:

		//! The key used to access this record
		NodeKey key_;

		//! The data stored in this record
		NodeData data_;

		//! Pointer to the left sub-tree.
		TeKdTreeNode* left_;

		//! Pointer to the right sub-tree.
		TeKdTreeNode* right_;

	public:

		//! Export key type.
		typedef NodeKey kdKey;

		//! Export data type.
		typedef NodeData kdData;

		//! Export data item type.
		typedef NodeDataItem kdDataItem;

		//! Export data type.
		typedef NodeDataTag  kdDataTag;

		//! Constructor
		TeKdTreeNode(const NodeKey& k)
			: key_(k), left_(0), right_(0)
		{
		}

		//! Sets the key to the node
		void setKey(const NodeKey& k)
		{
			key_ = k;
		}

		//! Returns a reference to node key
		const NodeKey& getKey(void) const
		{
			return key_;
		}

		//! Sets the data in the node
		void setData(const NodeData& data)
		{
			data_ = data;
		}

		//! Returns a reference to data node
		NodeData& getData(void)
		{
			return data_;
		}

		//! Sets the left child pointer
		void setLeft(TeKdTreeNode* node)
		{
			left_ = node;
		}

		//! Sets the right child pointer
		void setRight(TeKdTreeNode* node)
		{
			right_ = node;
		}

		//! Accessor for left child
		TeKdTreeNode* getLeft(void) const
		{
			return left_;
		}

		//! Accessor for right child
		TeKdTreeNode* getRight(void) const
		{
			return right_;
		}		

		//! Method to check if this has a left child
		bool hasLeft(void) const
		{
			return (left_ != 0);
		}
		
		//! Method to check if a this has a right child
		bool hasRight(void) const
		{
			return (right_ != 0);
		}

		//! Method to check if this has no children
		bool isLeaf(void) const
		{
			return !(hasLeft() || hasRight());
		}		

		//! Method to count the number of nodes below this
		unsigned int descendants(void) const
		{
			unsigned int totalLeft  = 0u;
			unsigned int totalRight = 0u;

			if(hasLeft())
				totalLeft = 1u + getLeft()->descendants();

			if(hasRight())
				totalRight = 1u + getRight()->descendants();

			return (totalLeft + totalRight);
		}

	private:

		//! No copy allowed
		TeKdTreeNode(const TeKdTreeNode& other);

		//! No copy allowed
		TeKdTreeNode& operator=(const TeKdTreeNode& other);

};	// end of class TeKdTreeNode

//! A base class for Kd-Tree structures
/*!
	This class implements only common kdtree methods and properties.
 */
template<class KdTreeNode>
class TeBasicKdTree
{
	protected:				

		//! Pointer to the root node
		KdTreeNode* root_;

		//! Bounding box of all nodes
		TeBox mbr_;

		//! The size of the K-d Tree (number of nodes)
		unsigned int size_;		

	public:

		//! Constructor
		TeBasicKdTree(const TeBox& mbr)
			: root_(0), mbr_(mbr), size_(0u)
		{
		}

		//! Destructor
		~TeBasicKdTree()
		{		
			clear();
		}

		//! Clear all tree nodes
		void clear(void)
		{
			if(root_)
			{
				erase(root_);
				root_ = 0;
				size_ = 0;
			}
		}

		//! The number of tree nodes
		const unsigned int& size(void) const
		{
			return size_;
		}
		
		//! Return true if the tree is empty
		bool isEmpty(void) const
		{
			return root_ == 0;
		}

		//! Sets the bounding box of all elements in the tree
		void setBox(const TeBox& mbr)
		{
			mbr_ = mbr;
		}

		//! Sets the bounding box of all elements in the tree
		const TeBox& getBox(void) const
		{
			return mbr_;
		}

	protected:

		//! Erases a node from the tree and all nodes below it.
		void erase(KdTreeNode* node)
		{
			if(node->hasLeft())
				erase(node->getLeft());

			if(node->hasRight())
				erase(node->getRight());

			delete node;

			return;
		}

	private:

		//! No copy allowed
		TeBasicKdTree(const TeBasicKdTree& other);

		//! No copy allowed
		TeBasicKdTree& operator=(const TeBasicKdTree& other);
};


//! A class that represents a two dimensional K-d Tree (2-d Tree)
/*!	
	WARNING:

	1. This type of tree stores the data into nodes (not only in the leafs node).
	
	2. This tree may be built by two ways: <BR>
	   2.1. Inserting each element in the tree. In this case the tree can becomes unbalanced, but in practical
	        cases this is not the expected, and is the best way to construct the tree (faster way). <BR>
	   2.2. Passing a container with pairs (key/data-item) and using the method buildOptimized after
	        calling kdsort. The tree built this way is almost balanced but will be 
			construct in time O(N log N).<BR>
			WARNING: In this case items with the same key will be stores in different nodes!<BR>
	
	3. This type of tree may be of special interest of BOX SEARCH QUERIES.	

	4. If the node type is kd_node_data_single_tag than NodeData and NodeDataItem are the same types. And if
	   one entry with the same key already exist, so they will be overwrite.
	   
	5. If the node type is kd_node_data_set_tag than NodeData mus have a method called push_back(NodeDataItem)
	   that permits to store elements with the same key in the node.
 */
template<class KdTreeNode>
class TeKdTree : public TeBasicKdTree<KdTreeNode>
{
        using TeBasicKdTree<KdTreeNode>::root_;
        using TeBasicKdTree<KdTreeNode>::size_;
        using TeBasicKdTree<KdTreeNode>::mbr_;
        
	public:

		//! Export key type.
		typedef typename KdTreeNode::kdKey kdKey;

		//! Export data type.
		typedef typename KdTreeNode::kdData kdData;

		//! Export data item type.
		typedef typename KdTreeNode::kdDataItem kdDataItem;		

		//! Export data type.
		typedef typename KdTreeNode::kdDataTag kdDataTag;

		//! Constructor
		TeKdTree(const TeBox& mbr)
			: TeBasicKdTree<KdTreeNode>(mbr)
		{
		}

		//! Inserts the data with a given key in tree
		inline void insert(const kdKey& key, const kdDataItem& item);

		//! Inserts the data in the tree and and keeps it balanced: the kdsort algorithm must be called before
		void buildOptimized(vector<pair<kdKey, kdDataItem> >& dataSet)
		{
			const unsigned int last = dataSet.size() - 1u;

			root_ = buildOptimized(dataSet, 0u, last);			
		}		

		//! Range search query.
		void search(const TeBox& rect, vector<KdTreeNode*>& report) const
		{
			if(root_)
				search(rect, root_, 'x', report);

			return;
		}					

	protected:

		//! Inserts data for single nodes, i.e., nodes that stores only one element
		void insertData(KdTreeNode*& node, const kdDataItem& data, const kd_node_data_single_tag&)
		{
			node->setData(data);
		}

		//! Inserts data for set nodes, i.e., nodes that may stores many element
		void insertData(KdTreeNode*& node, const kdDataItem& data, const kd_node_data_set_tag&)
		{
			node->getData().push_back(data);
		}

		//! Recursive range query
		inline void search(const TeBox& rect, KdTreeNode* node, const char& level, vector<KdTreeNode*>& report) const;

		//! Builds the tree recursively
		KdTreeNode* buildOptimized(vector<pair<kdKey, kdDataItem> >& dataSet, const unsigned int& first, const unsigned int& last)
		{
			const unsigned int kth = (last - first + 1u) / 2u;

			KdTreeNode* newNode = new KdTreeNode(dataSet[first + kth].first);

			newNode->setData(dataSet[first + kth].second);

			++size_;

			if((first + kth) > first)
				newNode->setLeft(buildOptimized(dataSet, first, first + kth - 1u));

			if((first + kth) < last)
				newNode->setRight(buildOptimized(dataSet, first + kth + 1u, last));

			return newNode;
		}				

	private:

		//! No copy allowed
		TeKdTree(const TeKdTree& other);

		//! No copy allowed
		TeKdTree& operator=(const TeKdTree& other);

};	// end class TeKdTree

template<class KdTreeNode>
void TeKdTree<KdTreeNode>::insert(const kdKey& key, const kdDataItem& item)
{
	if(root_ == 0)
	{
		root_ = new KdTreeNode(key);

		insertData(root_, item, kdDataTag());
	}
	else
	{					
		char level = 'x';

		bool left = false;

		KdTreeNode* x = root_;
		KdTreeNode* y = 0;

		while(x != 0)
		{
			y = x;

			if(level == 'x')
			{
				if(key.x() > x->getKey().x())		// if the key is greater than, inserts in the right subtree
				{
					x = x->getRight();
					left = false;
				}
				else if(key.x() < x->getKey().x())		// if the key is smaller than, inserts in the left subtree
				{
					x = x->getLeft();
					left = true;
				}
				else if(key.y() == x->getKey().y())		// if the key already exist, in the case of single node the data will be overwrite and in the case of set node they will push_back the item
                {
					insertData(x, item, kdDataTag());

					return;							
				}
				else					// found the same axis partition, so go left
				{
					x = x->getLeft();
					left = true;
				}						

				level = 'y';
			}
			else
			{
				if(key.y() > x->getKey().y())
				{
					x = x->getRight();
					left = false;
				}
				else if(key.y() < x->getKey().y())
				{
					x = x->getLeft();
					left = true;
				}
				else if(key.x() == x->getKey().x())
                                {
					insertData(x, item, kdDataTag());

					return;							
				}
				else
				{
					x = x->getLeft();
					left = true;
				}						

				level = 'x';						
			}
		}

		KdTreeNode* newNode = new KdTreeNode(key);

		insertData(newNode, item, kdDataTag());

		if(left)
			y->setLeft(newNode);
		else
			y->setRight(newNode);
	}

	++size_;

	return;
}

template<class KdTreeNode>
void TeKdTree<KdTreeNode>::search(const TeBox& rect, KdTreeNode* node, const char& level, vector<KdTreeNode*>& report) const
{
	if((node->getKey().x() >= rect.x1_) && (node->getKey().x() <= rect.x2_) &&
	   (node->getKey().y() >= rect.y1_) && (node->getKey().y() <= rect.y2_))
		report.push_back(node);

	if(level == 'x')
	{
		if(node->hasLeft())
			if(node->getKey().x() >= rect.x1_)
				search(rect, node->getLeft(), 'y', report);

		if(node->hasRight())
			if(node->getKey().x() <= rect.x2_)
				search(rect, node->getRight(), 'y', report);
	}
	else
	{
		if(node->hasLeft())
			if(node->getKey().y() >= rect.y1_)
				search(rect, node->getLeft(), 'x', report);

		if(node->hasRight())
			if(node->getKey().y() <= rect.y2_)
				search(rect, node->getRight(), 'x', report);
	}

	return;
}

//! Class that represents a node of a TeAdaptativeKdTree
/*!
	Each node contains a pointer to its left and right subtree (NULL if it is not set),
	a discriminator that indicates the axis of partition, the partition key and
	a set of data-items.

	WARNING:

	1. The key must have methods called x() and y().
	
	2. These kind of node stores the data only in the leafs.

	3. The leaf nodes contains a set of values that forms a bucket (the size is controlled by the tree methods tha use this class).
 */
template<class NodeKey, class NodeData, class NodeDataItem>
class TeAdaptativeKdTreeNode
{
protected:
		//! The key used to access this record
		double key_;

		//! The data stored in this record
		NodeData data_;

		//! The discriminator used in partition
		char discriminator_;

		//! Pointer to the left sub-tree.
		TeAdaptativeKdTreeNode* left_;

		//! Pointer to the right sub-tree.
		TeAdaptativeKdTreeNode* right_;

public:
		//! Export key type.
		typedef NodeKey kdKey;

		//! Export data type.
		typedef NodeData kdData;

		//! Export data item type.
		typedef NodeDataItem kdDataItem;

		//! Constructor
		TeAdaptativeKdTreeNode(const double& k)
			: key_(k), discriminator_('x'), left_(0), right_(0)
		{ }

		//! Sets the key to the node
		void setKey(const double& k)
		{
			key_ = k;
		}

		//! Returns a reference to node key
		const double& getKey(void) const
		{
			return key_;
		}

		//! Sets the data in the node
		void setData(const NodeData& data)
		{
			data_ = data;
		}

		//! Returns a reference to data node
		NodeData& getData(void)
		{
			return data_;
		}

		//! Sets the data in the node
		void setDiscriminator(const char& d)
		{
			discriminator_ = d;
		}

		//! Returns a reference to discriminator
		const char& getDiscriminator(void) const
		{
			return discriminator_;
		}

		//! Sets the left child pointer
		void setLeft(TeAdaptativeKdTreeNode* node)
		{
			left_ = node;
		}

		//! Sets the right child pointer
		void setRight(TeAdaptativeKdTreeNode* node)
		{
			right_ = node;
		}

		//! Accessor for left child
		TeAdaptativeKdTreeNode* getLeft(void) const
		{
			return left_;
		}

		//! Accessor for right child
		TeAdaptativeKdTreeNode* getRight(void) const
		{
			return right_;
		}		

		//! Method to check if this has a left child
		bool hasLeft(void) const
		{
			return (left_ != 0);
		}
		
		//! Method to check if a this has a right child
		bool hasRight(void) const
		{
			return (right_ != 0);
		}

		//! Method to check if this has no children
		bool isLeaf(void) const
		{
			return !(hasLeft() || hasRight());
		}

		//! Method to count the number of nodes below this
		unsigned int descendants(void) const
		{
			unsigned int totalLeft  = 0u;
			unsigned int totalRight = 0u;

			if(hasLeft())
				totalLeft = 1u + getLeft()->descendants();

			if(hasRight())
				totalRight = 1u + getRight()->descendants();

			return (totalLeft + totalRight);
		}		

	private:

		//! No copy allowed
		TeAdaptativeKdTreeNode(const TeAdaptativeKdTreeNode& other);

		//! No copy allowed
		TeAdaptativeKdTreeNode& operator=(const TeAdaptativeKdTreeNode& other);

};	// end of class TeAdaptativeKdTreeNode

//! A class that represents a two dimensional K-d Tree (2-d Tree) that store data-elements into the leafs
/*!	
	WARNING:

	1. This type of tree stores the data only in the leaf nodes.
	
	2. The process of construction expect that the tree is almost balanced
    	
	3. This type of tree may be of special interest of NEAREST NEIGHBOR SEARCH QUERIES.

	4. After a box search it will be necessary to do a refinement.
 */
template<class KdTreeNode>
class TeAdaptativeKdTree : public TeBasicKdTree<KdTreeNode>
{
	using TeBasicKdTree<KdTreeNode>::root_;
	using TeBasicKdTree<KdTreeNode>::size_;
	using TeBasicKdTree<KdTreeNode>::mbr_;
        
protected:				
		//! Bucket size (maximum number of elements in each node)
		unsigned int bucketSize_;
		
public:
		//! Export key type.
		typedef typename KdTreeNode::kdKey kdKey;		

		//! Export data type.
		typedef typename KdTreeNode::kdData kdData;		

		//! Export data item type.
		typedef typename KdTreeNode::kdDataItem kdDataItem;	

		//! Export node type.
		typedef KdTreeNode kdNode;		

		//! Constructor
		TeAdaptativeKdTree(const TeBox& box, const unsigned int& bucketSize = 12)
			: TeBasicKdTree<KdTreeNode>(box), bucketSize_(bucketSize)
		{
		}

		//! Sets bucket size for leaf nodes
		void setBucketSize(const unsigned int& size)
		{
			bucketSize_ = size;
		}

		//! Sets bucket size for leaf nodes
		const unsigned int& getBucketSize(void) const
		{
			return bucketSize_;
		}

		//! Inserts the data set into the tree
		void build(vector<pair<kdKey, kdDataItem> >& dataSet)
		{
			root_ = build(dataSet, 0.0, mbr_);
		}

		//! Search the nearest data in nodes: you must pass an array of kdDataItem of size "k" with coordinates values (X() and Y()) adjusted to TeMAXFLOAT (this dummy values will be replaced at processing time), and if not all neighbors are found so sqrDists will contains TeMAXFLOAT in array index
		void nearestNeighborSearch(const kdKey& key, vector<kdDataItem>& report, vector<double>& sqrDists, const unsigned int& k) const
		{
			if(root_)
			{
				sqrDists.clear();

				for(unsigned int i = 0; i < k; ++i)
					sqrDists.push_back(TeMAXFLOAT);
				
				TeBox rect(-TeMAXFLOAT, -TeMAXFLOAT, +TeMAXFLOAT, +TeMAXFLOAT);

				nearestNeighborSearch(root_, key, report, sqrDists, rect);
			}
		}		

		//! Range search query.
		void search(const TeBox& rect, vector<KdTreeNode*>& report) const
		{
			if(root_)
				search(rect, root_, report);

			return;
		}		

		//! Range search query: the refinement is already done
		inline void search(const TeBox& rect, vector<kdDataItem>& report) const;

protected:			
		//! Build the tree recursivily 
		inline KdTreeNode* build(vector<pair<kdKey, kdDataItem> >& dataSet, double averageValue, const TeBox& mbr);

		//! Recursive range query.
		inline void search(const TeBox& rect, KdTreeNode* node, vector<KdTreeNode*>& report) const;

		//! Recursive nearest neighbor search
		inline void nearestNeighborSearch(KdTreeNode* node, const kdKey& key, vector<kdDataItem>& report, vector<double>& sqrDists, TeBox& rect) const;

		//! Update neighbor list
		inline void update(KdTreeNode* node, const kdKey& key, vector<kdDataItem>& report, vector<double>& sqrDists, TeBox& rect) const;

		//! Returns the average value along the axis
		double average(vector<pair<kdKey, kdDataItem> >& dataSet, const char& discriminator) const
		{
			const unsigned int size = dataSet.size();

			double medianValue = 0.0;

			if(discriminator == 'x')
			{
				for(unsigned int i = 0; i < size; ++i)
					medianValue += dataSet[i].first.x();

				return medianValue / size;
			}
			else
			{
				for(unsigned int i = 0; i < size; ++i)
					medianValue += dataSet[i].first.y();

				return medianValue / size;
			}
		}			
		
private:
		//! No copy allowed
		TeAdaptativeKdTree(const TeAdaptativeKdTree& other);

		//! No copy allowed
		TeAdaptativeKdTree& operator=(const TeAdaptativeKdTree& other);

};	// end class TeAdaptativeKdTree

template<class KdTreeNode>
KdTreeNode* TeAdaptativeKdTree<KdTreeNode>::build(vector<pair<kdKey, kdDataItem> >& dataSet, double averageValue, const TeBox& mbr)
{
	++size_;

	if(dataSet.size() <= bucketSize_)
	{
		KdTreeNode* node = new KdTreeNode(averageValue);

		node->setDiscriminator('l');
		//node->setBox(mbr);

		unsigned int size = dataSet.size();

		for(unsigned int i = 0; i < size; ++i)
			node->getData().push_back(dataSet[i].second);

		return node;
	}

	TeBox newMbr1(mbr);
	TeBox newMbr2(mbr);	

	char discriminator = 'x';

	vector<pair<kdKey, kdDataItem> > leftDataSet;
	vector<pair<kdKey, kdDataItem> > rightDataSet;

// Finds the largest dimension
	if((mbr.x2_ - mbr.x1_) > (mbr.y2_ - mbr.y1_))
	{
// Finds the median along "x" axis
		averageValue = average(dataSet, 'x');

// Adjust box for left and right branchs
		newMbr1.x2_ = averageValue;
		newMbr2.x1_ = averageValue;

		unsigned int size = dataSet.size();

		for(unsigned int i = 0; i < size; ++ i)
		{
			if(dataSet[i].first.x() <= averageValue)
				leftDataSet.push_back(dataSet[i]);
			else
				rightDataSet.push_back(dataSet[i]);
		}
	}
	else
	{
		discriminator = 'y';

// Finds the median along "y" axis		
		averageValue = average(dataSet, 'y');

// Adjust box for left and right branchs
		newMbr1.y2_ = averageValue;
		newMbr2.y1_ = averageValue;

		unsigned int size = dataSet.size();

		for(unsigned int i = 0; i < size; ++ i)
		{
			if(dataSet[i].first.y() <= averageValue)
				leftDataSet.push_back(dataSet[i]);
			else
				rightDataSet.push_back(dataSet[i]);
		}
	}

	dataSet.clear();

	KdTreeNode* node = new KdTreeNode(averageValue);
	
	//node->setBox(mbr);
	
	if(rightDataSet.size() == 0u)		// If all coordinates have the same coordinate values, the right vector will be empty so we need stop division to
	{
		node->setDiscriminator('l');
		//node->setBox(newMbr1);

		unsigned int size = leftDataSet.size();

		for(unsigned int i = 0; i < size; ++i)
			node->getData().push_back(leftDataSet[i].second);

		//throw;
	}
	else if(leftDataSet.size() == 0u)		// If all coordinates have the same coordinate values, the left vector is empty, so we need to stop
	{
		node->setDiscriminator('l');
		//node->setBox(newMbr2);

		unsigned int size = rightDataSet.size();

		for(unsigned int i = 0; i < size; ++i)
			node->getData().push_back(rightDataSet[i].second);

		//throw;
	}
	else
	{
		node->setDiscriminator(discriminator);
		node->setLeft(build(leftDataSet, averageValue, newMbr1));
		node->setRight(build(rightDataSet, averageValue, newMbr2));
	}
	
	return node;
}

template<class KdTreeNode>
void TeAdaptativeKdTree<KdTreeNode>::search(const TeBox& rect, vector<kdDataItem>& report) const
{
	vector<KdTreeNode*> reportNodes;

	search(rect, reportNodes);

    unsigned int nNodes = reportNodes.size();

	for(unsigned int i = 0; i < nNodes; ++i)
	{
		unsigned int nElements = reportNodes[i]->getData().size();

		for(unsigned int j = 0; j < nElements; ++j)
		{
            if(TeIntersects((reportNodes[i])->getData()[j], rect))
			{
				report.push_back((reportNodes[i])->getData()[j]);
			}
		}
	}
}

template<class KdTreeNode>
void TeAdaptativeKdTree<KdTreeNode>::search(const TeBox& rect, KdTreeNode* node, vector<KdTreeNode*>& report) const
{
	if(node->getDiscriminator() == 'x')
	{
		if(node->hasLeft())
			if(rect.x1_ <= node->getKey())
				search(rect, node->getLeft(), report);		

		if(node->hasRight())
			if(rect.x2_ >= node->getKey())
				search(rect, node->getRight(), report);
	}
	else if(node->getDiscriminator() == 'y')
	{
		if(node->hasLeft())
			if(rect.y1_ <= node->getKey())
				search(rect, node->getLeft(), report);		

		if(node->hasRight())
			if(rect.y2_ >= node->getKey())
				search(rect, node->getRight(), report);
	}
	else
	{		
		report.push_back(node);
	}

	return;
}

template<class KdTreeNode>
void TeAdaptativeKdTree<KdTreeNode>::nearestNeighborSearch(KdTreeNode* node, const kdKey& key, vector<kdDataItem>& report, vector<double>& sqrDists, TeBox& rect) const
{
	if(node->getDiscriminator() == 'l')
	{
		update(node, key, report, sqrDists, rect);	// this is a leaf node -> update list of neighbours 
	}
	else if(node->getDiscriminator() == 'x')
	{
		if(key.x() <= node->getKey())
		{
			nearestNeighborSearch(node->getLeft(), key, report, sqrDists, rect);

			if((rect.x1_ < node->getKey()) && (node->getKey() < rect.x2_))
				nearestNeighborSearch(node->getRight(), key, report, sqrDists, rect);
		}
		else
		{
			nearestNeighborSearch(node->getRight(), key, report, sqrDists, rect);

			if((rect.x1_ < node->getKey()) &&(node->getKey() < rect.x2_))
				nearestNeighborSearch(node->getLeft(), key, report, sqrDists, rect);
		}		
	}
	else if(node->getDiscriminator() == 'y')
	{
		 if(key.y() <= node->getKey())
		{
			nearestNeighborSearch(node->getLeft(), key, report, sqrDists, rect);

			if((rect.y1_ < node->getKey()) &&(node->getKey() < rect.y2_ ))
				nearestNeighborSearch(node->getRight(), key, report, sqrDists, rect);
		}
		else
		{
			nearestNeighborSearch(node->getRight(), key, report, sqrDists, rect);

			if((rect.y1_ < node->getKey()) &&(node->getKey() < rect.y2_))
				nearestNeighborSearch(node->getLeft(), key, report, sqrDists, rect);
		}
	}	
	
}

template<class KdTreeNode>
void TeAdaptativeKdTree<KdTreeNode>::update(KdTreeNode* node, const kdKey& key, vector<kdDataItem>& report, vector<double>& sqrDists, TeBox& rect) const
{
// rect is the bounding box of neighbors

	const unsigned int size = node->getData().size();

	const unsigned int nNeighbors = report.size();

// for each element in the node, we need to search for distances less than of some one of sqrDists
	for(unsigned int i = 0u; i < size; ++i)
	{
		double dx = (key.x() - node->getData()[i].location().x());
		double dy = (key.y() - node->getData()[i].location().y());
		
		double dkp = (dx * dx) + (dy * dy);	// square distance from the key point to the node

// if the distance of "i-th" element is less than the maximum distance in the sqrDists
		if(dkp < sqrDists[nNeighbors - 1])
		{
// so the element must be reported

// and the srqDists vector must be rearranged
			for(unsigned int j = 0u; j < nNeighbors; ++j)
			{
				if(dkp < sqrDists[j])	// if the position is found
				{  
// move the elements to the right
					for(unsigned int k = nNeighbors - 1; k > j; --k)
					{
						report[k]   = report[k - 1];
						sqrDists[k] = sqrDists[k - 1];
					} 

// inserts the element in the report and update its distance
					report[j] = node->getData()[i];

					sqrDists[j] = dkp;

					break;
				}
			}
		}
	}

	double maxDist = sqrDists[nNeighbors - 1];

	if(maxDist != TeMAXFLOAT)
	{
		maxDist = sqrt(maxDist);
	}

	rect.x1_ = key.x() - maxDist;
	rect.y1_ = key.y() - maxDist;
	rect.x2_ = key.x() + maxDist;
	rect.y2_ = key.y() + maxDist;
}
/** @} */ 

};	// end namespace TeSAM


#endif	// __TERRALIB_INTERNAL_KDTREE_H




