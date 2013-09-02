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
/*! \file  TeRedBlackTree.h
    \brief This file contains structures and definitions for a balanced binary search tree.
    \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
*/

#ifndef  __TERRALIB_INTERNAL_RED_BLACK_TREE_H
#define  __TERRALIB_INTERNAL_RED_BLACK_TREE_H

//STL's include
#include <string>

//TerraLib's include
#include "TeUtils.h"

using namespace std;

#define PRINT

/** @brief Contains structures and definitions necessary to deal with a balanced Binary Search Tree
*/
namespace TeREDBLACK
{

	//! Possible values for a node color in a red-black tree
	enum TeRBNodeColor { TeREDNODE, TeBLACKNODE };

	//! Template class for nodes in a Red-Black tree.
	/*!
		This class contains the common attributes to a node
		used by a binary tree. Only used for commom binary trees,
		if you want a more sophisticated tree, you can continue
		using the Red-Black Tree framework defined below, it is possible
		taking care of some default attributes that a node must
		have to implement (left_, right_, parent_, data_).
	*/
	template<class T>
	class TeRedBlackNode
	{
		public:

			//! Export node's data type.
			typedef T TeRedBlackNodeData;

			//! Node's data.
			T data_;

			//! Pointer to the left sub-tree.
			TeRedBlackNode* left_;

			//! Pointer to the right sub-tree.
			TeRedBlackNode* right_;

			//! Pointeir to the father's node.
			TeRedBlackNode* parent_;

			//! Node's color (RED or BLACK).
			TeRBNodeColor color_;

			//! Constructor.
			TeRedBlackNode(const TeRBNodeColor& color = TeREDNODE)
				: left_(0), right_(0), parent_(0), color_(color)
			{
			}

			//! Constructor.
			TeRedBlackNode(T& d, const TeRBNodeColor& color = TeREDNODE)
				: data_(d), left_(0), right_(0), parent_(0), color_(color)
			{
			}


		private:

			//! No copy allowed
			TeRedBlackNode(const TeRedBlackNode& other);

			// No copy allowed
			TeRedBlackNode& operator=(const TeRedBlackNode& other);
	};

	//! Template class for Red-Black trees.
	/*!
		This class contains the common operations in a Red-Black Tree.
		It can be used like a framework. Extend the methods like
		"Insert" and "Search" to walk and to do some usefull things.
		You will use the methods for Rotation, Insert Propagation,
		Delete Propagation, Sucessor, Predecessor, IsEmpty, Clear.
		This tree is based on the algorithm of Cormen's book. The only difference is
		when a node being deleted has two children its successor node is relinked into its place,
		rather than copied, so that the only iterators invalidated are those referring to the deleted node.
		Every data used in nodes must implements a ToString() method.
	*/
	template<class NODE>
	class TeRBTree
	{
		private:

			//! No copy allowed.
			TeRBTree(const TeRBTree& other);

			//! No copy allowed.
			TeRBTree& operator=(const TeRBTree& other);

		protected:

			NODE* nil_;		//!< Reference node.			
			NODE* root_;	//!< Tree's root.			
			unsigned int nodeCount_;	//!< Count the number of nodes in the tree (excluding nil_ node).

		public:

			//! Export node type.
			typedef NODE* REDBLACKNODEPOINTER;

			//! Export node data type.
			typedef typename NODE::TeRedBlackNodeData TeRedBlackNodeData;

			//! Constructor.
			TeRBTree()
				: nodeCount_(0)
			{
				nil_ = new NODE();
				nil_->color_ = TeBLACKNODE;

				root_ = nil_;

			}

			//! Destructor.
			virtual ~TeRBTree()
			{
				Clear();

				if(nil_)
					delete nil_;
			}

			//! Returns true if the tree is empty otherwise returns false.
			bool IsEmpty(void) const
			{
				return root_ == nil_;
			}

			//! Returns the number of nodes in the tree, excluding the nil_ one.
			unsigned int Size(void) const
			{
				return nodeCount_;
			}

			//! Removes the node from the tree and do rebalancing.
			virtual void Delete(NODE* z)
			{
				if(z)
				{
					// z has at most one non-null child. y == z.
					// x might be null.
					// z has exactly one non-null child.  y == z.
					// x is not null.
					// z has two non-null children.  Set y to
					//   z's successor.  x might be null.
					NODE* y = ((z->left_ == nil_) || (z->right_ == nil_)) ? z : Successor(z);
					NODE* x = (y->left_  != nil_) ?  y->left_ : y->right_;
					NODE* x_parent = 0;

					if(y != z)	// relink y in place of z.  y is z's successor
					{
						z->left_->parent_ = y;
						y->left_ = z->left_;
						if(y != z->right_)
						{
							x_parent = y->parent_;

							if(x != nil_)
								x->parent_ = y->parent_;

							y->parent_->left_ = x;      // y must be a left child
							y->right_ = z->right_;
							z->right_->parent_ = y;
						}
						else
							x_parent = y;

						if(root_ == z)
							root_ = y;
						else
							if(z->parent_->left_ == z)
								z->parent_->left_ = y;
							else
								z->parent_->right_ = y;

						y->parent_ = z->parent_;

						TeRBNodeColor aux_color = y->color_;
						y->color_ = z->color_;
						z->color_ = aux_color;

						y = z;

						// y now points to node to be actually deleted
					}
					else
					{                        // y == z
						x_parent = y->parent_;
						if(x != nil_)
							x->parent_ = y->parent_;

						if(root_ == z)
							root_ = x;
						else
							if(z->parent_->left_ == z)
								z->parent_->left_ = x;
							else
								z->parent_->right_ = x;
					}

					if(y->color_ == TeBLACKNODE)
						DeleteFixUp(x, x_parent, root_);

					delete y;

					nodeCount_--;
				}

				return;
			}

			//! Returns the sucessor of a given node or zero if not.
			virtual NODE* Successor(NODE* x) const
			{
				if(x)
				{
					if(x->right_ != nil_)
					{
						x = x->right_;

						while(x->left_ != nil_)
							x = x->left_;

						return x;
					}

					NODE* y = x->parent_;

					while((y != nil_) && (x == y->right_))
					{
						x = y;
						y = y->parent_;
					}

					if(y == nil_)
						return 0;

					return y;
				}

				return 0;
			}

			//! Returns the predecessor of a given node or zero if not.
			virtual NODE* Predecessor(NODE* x) const
			{
				if(x)
				{
					if(x->left_ != nil_)
					{
						x = x->left_;

						while(x->right_ != nil_)
							x = x->right_;

						return x;
					}

					NODE* y = x->parent_;

					while((y != nil_) && (x == y->left_))
					{
						x = y;
						y = y->parent_;
					}

					if(y == nil_)
						return 0;

					return y;
				}

				return 0;
			}

			//! Returns the leftmost node in the tree, or zero if not.
			NODE* First(void) const
			{
				if(IsEmpty())
					return 0;

				NODE *x = root_;

				while(x->left_ != nil_)
					x = x->left_;

				return x;
			}

			//! Returns the rightmost node in the tree, or zero if not.
			NODE* Last(void) const
			{
				if(IsEmpty())
					return 0;

				NODE *x = root_;

				while(x->right_ != nil_)
					x = x->right_;

				return x;
			}

			//! Return the first element in the tree and removes it.
			virtual bool GetFirst(TeRedBlackNodeData& d)
			{
				NODE *x = First();

				if(x)
				{
						d = x->data_;

						Delete(x);

						return true;
				}

				return false;
			}

			//! Removes all nodes from the tree (excepty the nil node). Clear memory.
			void Clear(void)
			{
				if(!IsEmpty())
				{
					Erase(root_);

					root_ = nil_;
				}

				return;
			}

			//! Write the node's data to a file. The data must implement a methos called: void ToString(void).
			virtual void WriteToFile(const string& fileName) const
			{
				string strRepres = "";
		
				WriteToFile(root_, strRepres);

				TeWriteToFile(fileName, strRepres, "w");
				
				return;
			}

		protected:

			//! Left rotation.
			virtual void LeftRotate(NODE* x, NODE*& root)
			{
				NODE* y = x->right_;

				x->right_ = y->left_;
	
				if(y->left_ != nil_)
					y->left_->parent_ = x;

				y->parent_ = x->parent_;


				if(x == root)
					root = y;
				else
				{
					if(x == x->parent_->left_)
						x->parent_->left_ = y;
					else
						x->parent_->right_ = y;
				}

				y->left_ = x;
				x->parent_ = y;

				return;
			}

			//! Right rotation.
			virtual void RightRotate(NODE* y, NODE*& root)
			{
				NODE* x = y->left_;

				y->left_ = x->right_;

				if(x->right_ != nil_)
					x->right_->parent_ = y;

				x->parent_ = y->parent_;

				if(y == root)
					root = x;
				else
				{
					if(y == y->parent_->right_)
						y->parent_->right_ = x;
					else
						y->parent_->left_ = x;
				}

				x->right_ = y;
				y->parent_ = x;
			
				return;
			}

			//! Must be called after an insert, to fix-up the tree.
			void InsertFixUp(NODE*& n, NODE*& root)
			{
				n->color_ = TeREDNODE;

				NODE* y;
       
				while ((n != root) && (n->parent_->color_ == TeREDNODE))
				{
					if(n->parent_ == n->parent_->parent_->left_)	
					{											
						y = n->parent_->parent_->right_;			
						if(y->color_ == TeREDNODE)			
						{							
							n->parent_->color_ = TeBLACKNODE;
							y->color_ = TeBLACKNODE;
							n = n->parent_->parent_;
							n->color_ = TeREDNODE;
						}
						else
						{
							if(n == n->parent_->right_)
							{						
								n = n->parent_;		
								LeftRotate(n, root);
							}

							n->parent_->color_ = TeBLACKNODE;
							n->parent_->parent_->color_ = TeREDNODE;
							RightRotate(n->parent_->parent_, root);
						}
					}
					else
					{
						y = n->parent_->parent_->left_;
						if(y->color_ == TeREDNODE)
						{
							n->parent_->color_ = TeBLACKNODE;
							y->color_ = TeBLACKNODE;
							n = n->parent_->parent_;
							n->color_ = TeREDNODE;
						}
						else
						{
							if(n == n->parent_->left_)
							{
								n = n->parent_;
								RightRotate(n, root);
			                }

							n->parent_->color_ = TeBLACKNODE;
							n->parent_->parent_->color_ = TeREDNODE;
							LeftRotate(n->parent_->parent_, root);
						}
					}
				}

				root->color_ = TeBLACKNODE;

				return;
			}

			//! Must be called after a deletion, to fix-up the tree.
			void DeleteFixUp(NODE* x, NODE* x_parent, NODE*& root)
			{
				while((x != root) && (x == nil_ || x->color_ == TeBLACKNODE))
				{
					if(x == x_parent->left_)
					{
						NODE *w = x_parent->right_;

						if(w->color_ == TeREDNODE)
				        {
							w->color_ = TeBLACKNODE;
							x_parent->color_ = TeREDNODE;
							LeftRotate(x_parent, root);
							w = x_parent->right_;
				        }
		            
						if((w->left_ == nil_ || w->left_->color_ == TeBLACKNODE) && (w->right_ == nil_ || w->right_->color_ == TeBLACKNODE))
						{
							w->color_ = TeREDNODE;
							x = x_parent;
							x_parent = x_parent->parent_;
						}
						else
				        {
							if(w->right_ == nil_ || w->right_->color_ == TeBLACKNODE)
					        {
								if(w->left_ != nil_)
									w->left_->color_ = TeBLACKNODE;

								w->color_ = TeREDNODE;
								RightRotate(w, root);
								w = x_parent->right_;
						    }
						   
							w->color_ = x_parent->color_;
							x_parent->color_ = TeBLACKNODE;
							
							if(w->right_ != nil_)
								w->right_->color_ = TeBLACKNODE;

							LeftRotate(x_parent, root);

							break;
						}
					}
					else
				    {
						NODE* w = x_parent->left_;

						if(w->color_ == TeREDNODE)
				        {
							w->color_ = TeBLACKNODE;
							x_parent->color_ = TeREDNODE;
							RightRotate(x_parent, root);
							w = x_parent->left_;
						}
                
						if((w->left_ == nil_ || w->left_->color_ == TeBLACKNODE) && (w->right_ == nil_ || w->right_->color_ == TeBLACKNODE))
						{
							w->color_ = TeREDNODE;
							x = x_parent;
							x_parent = x_parent->parent_;
						}
						else
						{
							if(w->left_ == nil_ || w->left_->color_ == TeBLACKNODE)
				            {
								if(w->right_ != nil_)
									w->right_->color_ = TeBLACKNODE;

								w->color_ = TeREDNODE;
								LeftRotate(w, root);
								w = x_parent->left_;
							}
						
							w->color_ = x_parent->color_;
							x_parent->color_ = TeBLACKNODE;
							
							if(w->left_ != nil_)
								w->left_->color_ = TeBLACKNODE;

							RightRotate(x_parent, root);
							
							break;
						}
					}
				}

				if(x != nil_)
					x->color_ = TeBLACKNODE;
			}

			//! Only to be used by the public method, walking on the tree.
			virtual void WriteToFile(NODE* n, string& strRepres) const
			{
				if(n != nil_)
				{
					WriteToFile(n->left_, strRepres);

					strRepres += n->data_.ToString();
					strRepres += '\n';

					WriteToFile(n->right_, strRepres);
				}

				return;
			}	

			//! Removes the node and all node below it and doesn't do rebalancing. Used to free the memory.
			void Erase(NODE* n)
			{
				if(n != nil_)
				{
					Erase(n->left_);
					Erase(n->right_);

					delete n;

					nodeCount_--;
				}

				return;
			}

		public:

			//! Iterators class for the tree. It is used to abstract walking on the tree.
			template<class Node_i> class internal_iterator
			{
				protected:

					//! Pointer to the tree.
					TeRBTree<Node_i>* tree_;

					//! Pointer to the node.
					Node_i* node_;

				public:

					//! Makes iterator invalid
					void invalid(void)
					{
						node_ = 0;
						return;
					}

					//! Returns node pointer
					Node_i* getNode(void) const
					{
						return node_;
					}

				public:

					//! Constructor.
					internal_iterator()
						: node_(0), tree_(0)
					{						
					}
					
					//! Constructor
					internal_iterator(TeRBTree<Node_i>* tree, Node_i* n)
						: tree_(tree), node_(n)
					{
					}

					//! Destructor.
					virtual ~internal_iterator()
					{
					}

					//! Dereference operator.
					typename Node_i::TeRedBlackNodeData* operator->() const
					{
						return &node_->data_;
					}

					//! Dereference operator.
					typename Node_i::TeRedBlackNodeData& operator*() const
					{
						return node_->data_;
					}

					//! Operator ++ pre-fixed.
					internal_iterator& operator++()
					{
						node_ = tree_->Successor(node_);


						return *this;
					}

					//! Operator ++ pos-fixed
					internal_iterator operator++(int)	
					{
						internal_iterator temp = *this;
						++(*this);
						
						return temp;
					}

					//! Operator -- pre-fixed.
					internal_iterator& operator--()
					{
						node_ = tree_->Predecessor(node_);


						return *this;
					}

					//! Operator -- pos-fixed
					internal_iterator operator--(int)	
					{
						internal_iterator temp = *this;
						--(*this);
						return temp;
					}

					//! Operator ==.
					bool operator==(const internal_iterator& other) const
					{
						return (this->node_ == other.node_);
					}

					//! Operator !=.
					bool operator!=(const internal_iterator& other) const
					{
						return(this->node_ != other.node_);
					}
			};

			//! Exports iterator type.
			typedef internal_iterator<NODE> iterator;

			//! Returns an iterator to the leftmost node of the tree.
			iterator begin(void)
			{
				return iterator(this, First());
			}			

			//! Returns a reference iterator indicating the end of a tree.
			iterator end(void)
			{
				return iterator(this, 0);
			}

			//! Erases a node pointed by an interator.
			void erase(iterator& it)
			{
				Delete(it.getNode());

				it.invalid();

				return;
			}
	};

}		// end namespace TeREDBLACK
#endif	// __TERRALIB_INTERNAL_RED_BLACK_TREE_H
