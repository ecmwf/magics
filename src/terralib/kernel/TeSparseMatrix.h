// k9/a4/sparmat.h, templates for sparse matrices
#ifndef SPARSEMATRIX_H
#define SPARSEMATRIX_H SPARSEMATRIX_H

// selection of implementation
#ifdef STL_map            // defined in main()
#include<map>
#include<cassert>
#else
#include<hmap.h>

#include "TeDefines.h"

/* If at this point the HMap container is chosen, a function for
   calculating the hash table addresses is needed. As opposed to the
   hash functions described up to now, not only one value, but two are
   used for the calculation. Therefore, the function operator of the
   PairHashFun class takes a pair as argument. The address calculation
   itself is simple, but sufficient for our purposes. */

using namespace std;

template<class IndexType>  // int, long or unsigned
class PairHashFun
{
  public:
    PairHashFun(long prime=65537) // another prime number is possible
        // e.g. 2111 for smaller Matrizes
    : tabSize(prime)
    {}

    // Address calculation with two values
    long operator()(const pair<IndexType, IndexType>& p) const
    {
       return (p.first + p.second) % tabSize;
    }

    long tableSize() const
    { 
       return tabSize;
    }

  private:
    long tabSize;
};
#endif          //  STL_map

#ifdef _MSC_VER
#include <utility>
using namespace std;
#endif

template<class ValueType, class IndexType, class ContainerType>
class MatrixElement
{
  private:
    ContainerType& C;
    typename ContainerType::iterator I;
    IndexType row, column;

  public:
    typedef pair<IndexType, IndexType> IndexPair;
    typedef MatrixElement<ValueType, IndexType,
                          ContainerType>&  Reference;

    MatrixElement(ContainerType& Cont, IndexType r, IndexType c)
    : C(Cont), I(C.find(IndexPair(r,c))),
      row(r), column(c)
    {}

    /* The constructor initializes the private variables with all
       information that is needed. The container itself is located in
       the sparseMatrix class; here, the reference to it is entered.
       If the passed indices for row and column belong to an element
       not yet stored in the container, the iterator has the value
       C.end(). */

    ValueType asValue() const
    {
       if(I == C.end())
           return ValueType(0);
       else
           return (*I).second;
    }

    operator ValueType () const  // type conversion operator
    {
       return asValue();
    }

    /* According to the definition of the sparse matrix, 0 is returned
       if the element is not present in the container. Otherwise, the
       result is the second part of the object of type value_type
       stored in the container. */

    Reference operator=(const ValueType& x)
    {
       if(x != ValueType(0))        // not equal 0?
       {
         /* If the element does not yet exist, it is put, together
            with the indices, into an object of type value_type and
            inserted with insert(): */

          if(I == C.end())
          {
             assert(C.size() < C.max_size());
             I = (C.insert(
#ifndef _MSC_VER
			 typename
#endif
			 ContainerType::value_type(
                        IndexPair(row,column), x))
                 ).first;
          }
          else (*I).second = x;
       }

       /* insert() returns a pair whose first part is an iterator
          pointing to the inserted object. The second part is of type
          bool and indicates whether the insertion took place because
          no element with this key existed. This is, however, not
          evaluated here because, due to the precondition (I ==
          C.end()), the second part must always have the value true.
          If, instead, the element already exists, the value is
          entered into the second part of the value_type object. If
          the value is equal 0, in order to save space the element is
          deleted if it existed. */

       else                    // x = 0
          if(I != C.end())
          {
              C.erase(I);
              I = C.end();
          }
       return *this;
    }

    /* An assignment operator is required which in turn requires a
       reference to an object of type MatrixElement. When both the
       left- and right-hand side are identical, nothing has to happen.
       Otherwise, as above, it has to be checked whether the value of
       the right-hand element is 0 or not. The resulting behavior is
       described together with the above assignment operator, so that
       here it is simply called: */

    Reference operator=(const Reference rhs)
    {
       if(this != &rhs)      // not identical?
       {
           return operator=(rhs.asValue());  // see above
       }
       return *this;
    }

};  // class MatrixElement

template<class ValueType, class IndexType>
class TeSparseMatrix
{
   public:
     typedef pair<IndexType, IndexType> IndexPair;

     // The switch STL_map controls the compilation:

#ifdef STL_map
     typedef map<IndexPair, ValueType,
                  less<IndexPair> >       ContainerType;
#else
     typedef HMap<IndexPair, ValueType,
                  PairHashFun<IndexType> > ContainerType;
#endif

     typedef MatrixElement<ValueType, IndexType,
                           ContainerType> MatrixElement;

  public:
    typedef IndexType size_type;

    /* The constructor only initializes the row and column
       information. The container is created by its standard
       constructor, where in the case of hash implementation, the size
       of the container is given by the hash function object of type
       PairHashFun (see typedef above). */

  private:
    size_type rows, columns;
    ContainerType C;

  public:
    sparseMatrix(size_type r, size_type c)
    : rows(r), columns(c)
    {}

   size_type Rows()  const { return rows;}
   size_type Columns() const { return columns;}

   // usual container type definitions
   typedef typename ContainerType::iterator iterator;
   typedef typename ContainerType::const_iterator const_iterator;

   // usual container functions
   size_type size()       const { return C.size();}
   size_type max_size()   const { return C.max_size();}

   iterator begin()             { return C.begin();}
   iterator end()               { return C.end();}

   const_iterator begin() const { return C.begin();}
   const_iterator end()   const { return C.end();}

   void clear()
   {
       C.clear();

   }

   class Aux  // for index operator below
   {
     public:
       Aux(size_type r, size_type maxs, ContainerType& Cont)
       : Row(r), maxColumns(maxs), C(Cont)
       {}

       /* After checking the number of columns, the index operator of
          Aux returns a matrix element which is equipped with all
          necessary information to carry out a successful assignment.
        */

       MatrixElement operator[](size_type c)
       {
           assert(c >= 0 && c < maxColumns);
           return MatrixElement(C, Row, c);
       }

     private:
       size_type Row, maxColumns;
       ContainerType& C;
   };

   /* The index operator of the sparseMatrix class returns the
       auxiliary object, whose class is defined as nested inside
       sparseMatrix. */
   Aux operator[](size_type r)
   {
      assert(r >= 0 && r < rows);
      return Aux(r, columns, C);
   }

    /* Up to this point, from a functionality point of view, the
       sparseMatrix class is sufficiently equipped. In order, however,
       to avoid writing such horrible things as `(*I).first.first' for
       accessing the elements, some auxiliary functions follow which
       determine the indices and associated values of an iterator in a
       more readable way. */

   size_type Index1(iterator& I) const
   {
      return (*I).first.first;
   }

   size_type Index2(iterator& I) const
   {
      return (*I).first.second;
   }

   ValueType Value(iterator& I) const
   {
      return (*I).second;
   }
};       // class sparseMatrix

#endif   // file sparmat.h


