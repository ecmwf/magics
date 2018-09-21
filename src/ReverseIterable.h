/*
 * ReverseIterable.h
 *
 *  Created on: 20 Sep 2018
 *      Author: idris
 */

#ifndef SRC_REVERSEITERABLE_H_
#define SRC_REVERSEITERABLE_H_

#include	<iterator>
#include	<utility>

/**
 * Utility declarations to enable a reverse iteration of a container
 * using range-based for loops
 * Useful for AutoVector
 */

//Marker type
template <typename I>
struct ReverseIterable {I &container;};

//wrapper function to mark container for reverse iteration
template <typename I>
ReverseIterable<I> reverseIterable (I &&container) {return {std::forward<I>(container)};}

//redefine begin() & end() on marker type for reverse iteration
template <typename I>
auto begin (ReverseIterable<I> &revcon) {return std::rbegin(revcon.container);}
template <typename I>
auto end (ReverseIterable<I> &revcon) {return std::rend(revcon.container);}

#endif /* SRC_REVERSEITERABLE_H_ */
