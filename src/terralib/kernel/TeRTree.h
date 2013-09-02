/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file   TeRTree.h
    \brief  This file contains an implementation of rtree data structures in main memory for two dimensions.
	\note   These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone because
	        THIS IS FOR INTERNAL USE ONLY.
    \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
*/

#ifndef  __TERRALIB_INTERNAL_RTREE_H
#define  __TERRALIB_INTERNAL_RTREE_H

#include "TeGeometry.h"
#include "TeGeometryAlgorithms.h"

/*! \brief This namespace contain the definitions of various Spatial Data Structures,
           like K-d-Tree and R-Tree, ONLY FOR INTERNAL USE (must NOT be used by anyone).
*/
namespace TeSAM
{
//! Class that represents a node of a TeRTree
/*!
	If level is 0 so this is a leaf node otherwise this is a internal node.

	WARNING:

	1. A branch is a union, so be carefull with the types used here!
	
 */
template<class DATATYPE, int MAXNODES = 8, int MINNODES = MAXNODES / 2>
class TeRTreeNode
{
	public:

		//! Struct that represents a node-branch of a TeRTree
		/*!
			May be data or may be another subtree, if parents level is 0 so this is a data in a leaf node.

		*/
		struct TeRTreeBranch
		{
			TeBox rect_;	//!<	Bounding box for branch or object (if this is a leaf).

			union
			{
				TeRTreeNode* child_;		//!< Child node pointer.
				DATATYPE data_;				//!< Data Id or Ptr.
			};
		};

	public:

		int count_;			//!< Count.

		int level_;			//!< Leaf is zero, others positive.

		TeRTreeBranch branch_[MAXNODES];	//!< Branch.

		//! Constructor.


		TeRTreeNode()
			: count_(0), level_(-1)
		{
		}

		//! Returns true if this is a internal node.
        bool isInternalNode() const
		{
			return (level_ > 0);
		}

		//! Returns true if this is a leaf node.
        bool isLeaf() const
		{
			return (level_ == 0);
		}

		//! This method is used by split, when a node is re-filled.
		void init()
		{
			count_ = 0;

			level_ = -1;
		}

	private:

		//! No copy allowed
		TeRTreeNode(const TeRTreeNode& other);

		//! No copy allowed
		TeRTreeNode& operator=(const TeRTreeNode& other);

};	// end of class TeRTreeNode

//! A class that represents a two dimensional R-Tree
/*!	
	This implementation is based on:
	Antonin Guttman. R-Trees: A Dynamic Index Structure for Spatial Searching. ACM SIGMOD: International Conference on Management of Data, 1984, pp. 47-57.	

	and in his original source code. <BR>

	WARNING: <BR>

	1. Don't use this class! It is for TerraLib internal use. <BR>

	2. Only the basic methods for inserting and searching were implemented. <BR>

	3. In future we will implement: <BR>
		- the delete method. <BR>
		- the near query <BR>
		- point query <BR>
 */
template<class DATATYPE, int MAXNODES = 8, int MINNODES = MAXNODES / 2>
class TeRTree
{
	public:

		//! Export this typename.
		typedef TeRTreeNode<DATATYPE, MAXNODES, MINNODES> TeRTreeNodeType;

		//! Export this typename.
		typedef typename TeRTreeNodeType::TeRTreeBranch TeRTreeBranch;

	protected:

		//! Structure of auxiliary variables for finding a split partition.
		struct TePartitionVars
		{
			int partition_[MAXNODES + 1];	//!< Auxiliary partition vector.
			int taken_[MAXNODES + 1];		//!< Flag to indicate that entry is ok.
			int count_[2];					//!< Number of entries in each new page.
			TeBox cover_[2];				//!< Auxiliary box of each new page.
			double area_[2];				//!< Auxiliary area of each new page.

			TeRTreeBranch branchBuf_[MAXNODES + 1];	//!< Auxiliary branch buffer.
			TeBox coverSplit_;						//!< Auxiliary box covering branchBuf.

			//! Initializes partition vars.
			void init()
			{
				count_[0] = 0;
				count_[1] = 0;

                for(int i = 0; i <= MAXNODES; ++i)
                {
                    taken_[i] = 0;
                    partition_[i] = -1;
                }
			}
		};		
		
		TeRTreeNodeType* root_;		//!< Pointer to root node

		TeBox mbr_;					//!< Bounding box of the tree
        		
		mutable unsigned int size_;			//!< The size of R-Tree (number of nodes)		

	public:

		//! Construtor
		TeRTree(const TeBox& mbr)
			: root_(0), mbr_(mbr), size_(0)
		{
			++size_;
			root_ = new TeRTreeNodeType();
            root_->level_ = 0;
		}

		//! Destructor
		~TeRTree()
		{
			clear();

			if(root_)
				delete root_;
		}

		//! The number of elements of the tree
		const unsigned int& size(void) const
		{
			return size_;
		}
		
		//! Return true if the tree is empty
		bool isEmpty(void) const
		{
			return (root_->count_ == 0);
		}

		//! Clear all tree nodes
		void clear(void)
		{
			if(root_)
			{
				erase(root_);
				root_ = 0;
				
				size_ = 1;
				root_ = new TeRTreeNodeType();
				root_->level_ = 0;
			}
		}

		//! Inserts item into the tree
		void insert(const TeBox& rect, const DATATYPE& data)
		{
			insert(rect, data, &root_, 0);
		}

		//! Removes item from tree
		bool remove(const TeBox& rect, const DATATYPE& data)
		{
			return remove(rect, data, &root_);
		}

		//! Range search query
		int search(const TeBox& rect, vector<DATATYPE>& report) const
		{
			int foundObjs = 0;

			if(root_)
				search(rect, root_, report, foundObjs);

			return foundObjs;
		}

		//! Sets the bounding box of all elements in the tree
		void setBox(const TeBox& mbr)
		{
			mbr_ = mbr;
		}

		//! Gets the bounding box of all elements in the tree
		const TeBox& getBox(void) const
		{
			return mbr_;
		}

	protected:

		//! Insert a data rectangle into an index structure.
		/*!
			Insert provides for splitting the root. <BR>
			Returns true if root was split, false if it was not. <BR>
			The level argument specifies the number of steps up from the leaf
			level to insert; e.g. a data rectangle goes in at level = 0. <BR>
			chooseLeaf does the recursion.
		*/
		inline bool insert(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType** root, int level);

		//! Delete a data rectangle from an index structure.		   
		/*!
			Pass in a pointer to a Rect, the tid of the record, ptr to ptr to root node. <BR>
			Returns 1 if record not found, 0 if success. <BR>
			DeleteRect provides for eliminating the root.
		*/
		inline bool remove(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType** root);

		//! Delete a rectangle from non-root part of an index structure.		   
		/*!
			Called by DeleteRect. <BR>
			Descends tree recursively,
			merges branches on the way back up.
		*/
		inline bool remove2(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType* n, vector<TeRTreeNodeType*>& ee);

		//! Disconnect a dependent node.
		inline void disconBranch(TeRTreeNodeType* n, int i);

		//! Inserts a new data rectangle into the index structure.		   
		/*!
			Recursively descends tree, propagates splits back up.
			Returns false if node was not split.  Old node updated.
			If node was split, returns true and sets the pointer pointed to by
			new to point to the new node.  Old node updated to become one of two.
			The level argument specifies the number of steps up from the leaf
			level to insert; e.g. a data rectangle goes in at level = 0.
		*/
		inline bool chooseLeaf(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType* node, TeRTreeNodeType** newNode, int level);

		//! Recursive range query
		inline void search(const TeBox& rect, TeRTreeNodeType* node, vector<DATATYPE>& report, int& foundObjs) const;

		//! Find the smallest rectangle that includes all rectangles in branches of a node
		inline TeBox nodeCover(TeRTreeNodeType* n) const;

		//! Combine two rectangles into larger one containing both
		inline TeBox combineRect(const TeBox& rectA, const TeBox& rectB) const;

		//! Add a branch to a node.
		/*!
			Split the node if necessary. <BR>
			Returns false if node not split. <BR>
			Old node updated. <BR>
			Returns true if node split, sets *new to address of new node. <BR>
			Old node updated, becomes one of two.
		*/
		inline bool addBranch(TeRTreeBranch* branch, TeRTreeNodeType* node, TeRTreeNodeType** newNode) const;

		//! Pick a branch.
		/*!
			Pick the one that will need the smallest increase
			in area to accomodate the new rectangle.  This will result in the
			least total area for the covering rectangles in the current node.
			In case of a tie, pick the one which was smaller before, to get
			the best resolution when searching.
		*/
		inline int pickBranch(const TeBox& rect, TeRTreeNodeType* node) const;

		//! Split a node.
		/*!
			Divides the nodes branches and the extra one between two nodes. <BR>
			Old node is one of the new ones, and one really new one is created.
		*/
		inline void splitNode(TeRTreeNodeType* node, TeRTreeBranch* branch, TeRTreeNodeType** newNode) const;

		//! Load branch buffer with branches from full node plus the extra branch.
		inline void getBranches(TeRTreeNodeType* n, TeRTreeBranch* b, TePartitionVars& p) const;

		//! Method 0 for finding a partition.
		/*!
		   First find two seeds, one for each group, well separated.
           Then put other rects in whichever group will be smallest after addition.
	    */
		inline void methodZero(TePartitionVars& p) const;

		//! Pick two rects from set to be the first elements of the two groups.
		/*!	
		   Pick the two that are separated most along any dimension, or overlap least.
           Distance for separation or overlap is measured modulo the width of the
           space covered by the entire set along that dimension.
	     */
		inline void pickSeeds(TePartitionVars& p) const;

		//! Put a branch in one of the groups
		inline void classify(int i, int group, TePartitionVars& p) const;

		//! Put each rect that is not already in a group into a group.
		/*!	
           Process one rect at a time, using the following hierarchy of criteria.
           In case of a tie, go to the next test.<BR>
           1) If one group already has the max number of elements that will allow
              the minimum fill for the other group, put r in the other.<BR>
           2) Put r in the group whose cover will expand less.  This automatically
		      takes care of the case where one group cover contains r.<BR>
		   3) Put r in the group whose cover will be smaller.  This takes care of the
              case where r is contained in both covers.<BR>
           4) Put r in the group with fewer elements.<BR>
           5) Put in group 1 (arbitrary).<BR>

           Also update the covers for both groups.
	     */
		inline void pigeonhole(TePartitionVars& p) const;

		//! Copy branches from the buffer into two nodes according to the partition.
		inline void loadNodes(TeRTreeNodeType* n, TeRTreeNodeType* q, TePartitionVars& p) const;

		//! Calculate rect area
		double rectArea(const TeBox& b) const
		{
			return b.width() * b.height();
		}

		//! Erases a node from the tree and all nodes below it.
		void erase(TeRTreeNodeType* node)
		{
			if(node->isLeaf())
			{
				delete node;

				return;
			}

			for(int i = 0u; i < node->count_; ++i)
				erase(node->branch_[i].child_);
	
			delete node;

			return;
		}

		//! Only to determine if the two box intersects withou using an epsilon
		bool rtreeBoxIntersects(const TeBox& bx1, const TeBox& bx2) const
		{
			return ::TeIntersects(bx1, bx2);
		}		

	private:

		//! No copy allowed
		TeRTree(const TeRTree& other);

		//! No copy allowed
		TeRTree& operator=(const TeRTree& other);

};	// end of class TeRTree


//------------------- Implementation of the templates classes

template<class DATATYPE, int MAXNODES, int MINNODES>
bool TeRTree<DATATYPE, MAXNODES, MINNODES>::insert(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType** root, int level)
{
// this is the algorithm insert
    TeRTreeNodeType* newRoot;
	TeRTreeNodeType* newNode;
    TeRTreeBranch    branch;

    if(chooseLeaf(rect, data, *root, &newNode, level))  // I1
    {
// I4
// if root was split
// grow a new root, make tree taller
		++size_;
        newRoot = new TeRTreeNodeType();  

        newRoot->level_ = (*root)->level_ + 1;

// first half node
        branch.rect_  = nodeCover(*root);
        branch.child_ = *root;
        addBranch(&branch, newRoot, 0);

// second half node
        branch.rect_ = nodeCover(newNode);
        branch.child_ = newNode;        
		addBranch(&branch, newRoot, 0);

        *root = newRoot;

        return true;
    }

    return false;
}

template<class DATATYPE, int MAXNODES, int MINNODES>
bool TeRTree<DATATYPE, MAXNODES, MINNODES>::remove(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType** root)
{
	int i;
	TeRTreeNodeType *t;
	vector<TeRTreeNodeType*> reInsertList;
	
	if(remove2(rect, data, *root, reInsertList))
	{
// if we are here, so we have found and deleted a data item

// reinsert any branches from eliminated nodes
		while(!reInsertList.empty())
		{
			t = reInsertList[0];
			for(i = 0; i < t->count_; ++i)
				insert(t->branch_[i].rect_, t->branch_[i].data_, root, t->level_);
	
// erase node from list
			reInsertList.erase(reInsertList.begin());

// remove node card from memory
			delete t;
			--size_;
		}
		
		/* check for redundant root (not leaf, 1 child) and eliminate */
		if(((*root)->count_ == 1) && ((*root)->isInternalNode()))
		{
			t = (*root)->branch_[0].child_;

			delete (*root);
			--size_;

			*root = t;
		}
		
		return true;
	}
	
	return false;
}

template<class DATATYPE, int MAXNODES, int MINNODES>
bool TeRTree<DATATYPE, MAXNODES, MINNODES>::remove2(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType* n, vector<TeRTreeNodeType*>& ee)
{
	int i;	

	if(n->isInternalNode()) /* not a leaf node */
	{
		for(i = 0; i < n->count_; ++i)
		{
			if(rtreeBoxIntersects(rect, n->branch_[i].rect_))
			{
				if(remove2(rect, data, n->branch_[i].child_, ee))
				{
					if(n->branch_[i].child_->count_ >= MINNODES)
						n->branch_[i].rect_ = nodeCover(n->branch_[i].child_);
					else
					{
						/* not enough entries in child, eliminate child node */
						ee.push_back(n->branch_[i].child_);	//reInsert(n->branch_[i].child_, ee);
						disconBranch(n, i);
					}

					return true;	// found item
				}
			}
		}

		return false;	// din't find item
	}
	else  /* a leaf node */
	{
		for(i = 0; i < n->count_; ++i)
		{
			if(n->branch_[i].data_ == data)
			{
				disconBranch(n, i);
				return true;	// found item
			}
		}

		return false;	// didn't find item
	}
}

/*template<class DATATYPE, int MAXNODES, int MINNODES> void TeRTree<DATATYPE, MAXNODES, MINNODES>::reInsert(TeRTreeNodeType* n, vector<TeRTreeNodeType*>& ee)
{
	ee.push_back(n);
}*/

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::disconBranch(TeRTreeNodeType* n, int i)
{
// remove element copying the last element in array
	n->branch_[i] = n->branch_[n->count_ - 1];

	--(n->count_);
}

// CHOOSELEAF
template<class DATATYPE, int MAXNODES, int MINNODES>
bool TeRTree<DATATYPE, MAXNODES, MINNODES>::chooseLeaf(const TeBox& rect, const DATATYPE& data, TeRTreeNodeType* node, TeRTreeNodeType** newNode, int level)
{
	TeRTreeBranch    b;
	TeRTreeNodeType* n2;	
	
	if(node->level_ > level)
	{
// Still above level for insertion, go down tree recursively
		int i = pickBranch(rect, node);		// CL3

		if(!chooseLeaf(rect, data, node->branch_[i].child_, &n2, level))
		{
// child was not split
			node->branch_[i].rect_ = combineRect(rect, node->branch_[i].rect_);

			return false;
		}
		else		
		{
// child was split
			node->branch_[i].rect_ = nodeCover(node->branch_[i].child_);

			b.child_ = n2;
			b.rect_ = nodeCover(n2);

			return addBranch(&b, node, newNode);
		}
	}
	else if (node->level_ == level)
	{
// have reached level for insertion. Add rect, split if necessary

		b.rect_ = rect;
		//b.child_ = (TeRTreeNodeType*) data;
		b.data_ = data;

// child field of leaves contains tid of data record
		return addBranch(&b, node, newNode);
	}
	else
	{
// Not supposed to happen
		throw;
		return false;
	}
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::search(const TeBox& rect, TeRTreeNodeType* node, vector<DATATYPE>& report, int& foundObjs) const
{
	int i;
// S1
    if(node->isInternalNode()) // This is an internal node in the tree
	{
        for(i = 0; i < node->count_; ++i)
		{
            if(rtreeBoxIntersects(rect, node->branch_[i].rect_))
                search(rect, node->branch_[i].child_, report, foundObjs);
		}
	}
// S2
	else	// This is a leaf node
	{
        for(i = 0; i < node->count_; ++i)
		{
            if(rtreeBoxIntersects(rect, node->branch_[i].rect_))
			{
                DATATYPE& id = node->branch_[i].data_;

				report.push_back(id);

                ++foundObjs;
			}
		}
	}

	return;
}

template<class DATATYPE, int MAXNODES, int MINNODES>
TeBox TeRTree<DATATYPE, MAXNODES, MINNODES>::nodeCover(TeRTreeNodeType* n) const
{
	bool flag = true;

	TeBox r;
	
	for(int i = 0; i < n->count_; ++i)
	{
        if(flag)
		{
			r = n->branch_[i].rect_;
			flag = false;
		}
		else
			r = combineRect(r, n->branch_[i].rect_);
	}

	return r;
}

template<class DATATYPE, int MAXNODES, int MINNODES>
TeBox TeRTree<DATATYPE, MAXNODES, MINNODES>::combineRect(const TeBox& rectA, const TeBox& rectB) const
{
    TeBox newRect;

	newRect.x1_ = MIN(rectA.x1_, rectB.x1_);
	newRect.y1_ = MIN(rectA.y1_, rectB.y1_);
	newRect.x2_ = MAX(rectA.x2_, rectB.x2_);
	newRect.y2_ = MAX(rectA.y2_, rectB.y2_);

	return newRect;
}

template<class DATATYPE, int MAXNODES, int MINNODES>
bool TeRTree<DATATYPE, MAXNODES, MINNODES>::addBranch(TeRTreeBranch* branch, TeRTreeNodeType* node, TeRTreeNodeType** newNode) const
{
    if(node->count_ < MAXNODES)		/* split won't be necessary */
	{
		node->branch_[node->count_] = *branch;

		++(node->count_);

		return false;
	}
	else
	{
        splitNode(node, branch, newNode);

		return true;
	}
}

template<class DATATYPE, int MAXNODES, int MINNODES>
int TeRTree<DATATYPE, MAXNODES, MINNODES>::pickBranch(const TeBox& rect, TeRTreeNodeType* node) const
{
// CL3
	bool flag = true;

	double bestIncr = -1.0;

	double bestArea = 0.;

    int best = 0;
	
	for(int i = 0; i < node->count_; ++i)
	{
		TeBox rr = node->branch_[i].rect_;
		double area = rectArea(rr);

		rr = combineRect(rect, rr);
        
		double increase = rectArea(rr) - area;

        if((increase <  bestIncr) || flag)
		{
			best = i;

            bestArea = area;
            bestIncr = increase;

            flag = false;
		}
		else if((increase == bestIncr) && (area < bestArea))
		{
			best = i;
			bestArea = area;
			bestIncr = increase;
		}
	}

	return best;
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::splitNode(TeRTreeNodeType* node, TeRTreeBranch* branch, TeRTreeNodeType** newNode) const
{
    TePartitionVars partitions;

// load all the branches into a buffer, initialize old node
	int level = node->level_;

	getBranches(node, branch, partitions);

// find partition
	methodZero(partitions);

// put branches from buffer into 2 nodes according to chosen partition
	++size_;

	*newNode = new TeRTreeNodeType();
	(*newNode)->level_ = node->level_ = level;

	loadNodes(node, *newNode, partitions);
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::getBranches(TeRTreeNodeType* n, TeRTreeBranch* b, TePartitionVars& p) const
{
	int i;
// load the branch buffer
	for(i = 0; i < MAXNODES; ++i)
	{
		p.branchBuf_[i] = n->branch_[i];
	}

	p.branchBuf_[MAXNODES] = *b;

// calculate rect containing all in the set
	p.coverSplit_ = p.branchBuf_[0].rect_;

	for(i = 1; i <= MAXNODES; ++i)
		p.coverSplit_ = combineRect(p.coverSplit_, p.branchBuf_[i].rect_);

	n->init();
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::methodZero(TePartitionVars& p) const
{
	p.init();
	pickSeeds(p);
	pigeonhole(p);
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::pickSeeds(TePartitionVars& p) const
{
	double w;
	double separation;
	double bestSep;
	double width[2];
	int leastUpper[2];
	int greatestLower[2];
	int seed0;
	int seed1;
	int i;
// LPS1
// find the rectangles farthest out in each direction along dimens "x"
	greatestLower[0] = leastUpper[0] = 0;

	for(i = 1; i <= MAXNODES; ++i)
	{
		TeBox& r = p.branchBuf_[i].rect_;

		if(r.x1_ > p.branchBuf_[greatestLower[0]].rect_.x1_)
			greatestLower[0] = i;

		if(r.x2_ < p.branchBuf_[leastUpper[0]].rect_.x2_)
			leastUpper[0] = i;
	}

// LPS2
// LPS3
// find the width of the whole collection along this dimension
	width[0] = p.coverSplit_.x2_ - p.coverSplit_.x1_;

// find the rectangles farthest out in each direction along dimens "y"
	greatestLower[1] = leastUpper[1] = 0;

	for(i = 1; i <= MAXNODES; ++i)
	{
		TeBox& r = p.branchBuf_[i].rect_;

		if(r.y1_ > p.branchBuf_[greatestLower[1]].rect_.y1_)
			greatestLower[1] = i;

		if(r.y2_ < p.branchBuf_[leastUpper[1]].rect_.y2_)
			leastUpper[1] = i;
	}

// LPS2
// LPS3
// find the width of the whole collection along this dimension
	width[1] = p.coverSplit_.y2_ - p.coverSplit_.y1_;


// pick the best separation dimension and the two seed rects

// divisor for normalizing by width

// x
	if(width[0] == 0.0)
		w = 1.0;
	else
		w = width[0];

	TeBox rlow  = p.branchBuf_[leastUpper[0]].rect_;
    TeBox rhigh = p.branchBuf_[greatestLower[0]].rect_;

    seed0 = leastUpper[0];
	seed1 = greatestLower[0];
    
	bestSep = (rhigh.x1_ - rlow.x2_) / w;

// y
	if(width[1] == 0.0)
		w = 1.0;
	else
		w = width[1];

	rlow  = p.branchBuf_[leastUpper[1]].rect_;
    rhigh = p.branchBuf_[greatestLower[1]].rect_;

    separation = (rhigh.y1_ - rlow.y2_) / w;

// LPS3
	if(separation > bestSep)
	{
		seed0 = leastUpper[1];
		seed1 = greatestLower[1];

		bestSep = separation;
	}

	if(seed0 != seed1)
	{
		classify(seed0, 0, p);
		classify(seed1, 1, p);
	}
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::classify(int i, int group, TePartitionVars& p) const
{
    p.partition_[i] = group;
	p.taken_[i] = 1;

	if(p.count_[group] == 0)
		p.cover_[group] = p.branchBuf_[i].rect_;
	else
		p.cover_[group] = combineRect(p.branchBuf_[i].rect_, p.cover_[group]);

	p.area_[group] = rectArea(p.cover_[group]);

	++(p.count_[group]);
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::pigeonhole(TePartitionVars& p) const
{
	TeBox newCover[2];
	
	double newArea[2];
	double increase[2];

	for(int i = 0; i <= MAXNODES; ++i)
	{
		if(p.taken_[i] == 0)
		{
// if one group too full, put rect in the other regardless
			if(p.count_[0] >= (MAXNODES + 1 - MINNODES))
			{
				classify(i, 1, p);

				continue;
			}
			else if(p.count_[1] >= (MAXNODES + 1 - MINNODES))
			{
				classify(i, 0, p);

				continue;
			}

// find the areas of the two groups' old and new covers
			for(int group = 0; group < 2; ++group)
			{
				if(p.count_[group] > 0)
				{
					newCover[group] = combineRect(p.branchBuf_[i].rect_, p.cover_[group]);
				}
				else
				{
					newCover[group] = p.branchBuf_[i].rect_;
				}

				newArea[group] = rectArea(newCover[group]);
				increase[group] = newArea[group] - p.area_[group];
			}

// put rect in group whose cover will need to expand less
			if(increase[0] < increase[1])
				classify(i, 0, p);
			else if(increase[1] < increase[0])
				classify(i, 1, p);
// put rect in group that will have a smaller area cover
			else if(p.area_[0] < p.area_[1])
				classify(i, 0, p);
			else if(p.area_[1] < p.area_[0])
				classify(i, 1, p);
// put rect in group with fewer elements
			else if(p.count_[0] < p.count_[1])
				classify(i, 0, p);
			else
				classify(i, 1, p);
		}
	}
}

template<class DATATYPE, int MAXNODES, int MINNODES>
void TeRTree<DATATYPE, MAXNODES, MINNODES>::loadNodes(TeRTreeNodeType* n, TeRTreeNodeType* q, TePartitionVars& p) const
{
	for(int i = 0; i <= MAXNODES; ++i)
	{
		if(p.partition_[i] == 0)
			addBranch(&(p.branchBuf_[i]), n, 0);
		else if(p.partition_[i] == 1)
			addBranch(&(p.branchBuf_[i]), q, 0);
		//else
		//	throw;	// ERROR
	}
}
};	// end namespace TeSAM


#endif	// __TERRALIB_INTERNAL_RTREE_H




