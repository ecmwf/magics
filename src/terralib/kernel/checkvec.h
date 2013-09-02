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

// include/checkvec : vector class with checked limits
#ifndef CHECKVEC_H 
#define CHECKVEC_H
#include<cassert>
#include<vector>

namespace br_stl
{
	template<class T>
	class checkedVector : public std::vector<T>
	{
	public:
		// inherited types
		typedef typename checkedVector::size_type size_type;
		typedef typename checkedVector::iterator iterator;
		typedef typename checkedVector::difference_type difference_type;
		typedef typename checkedVector::reference reference;
		typedef typename checkedVector::const_reference const_reference;

		checkedVector() {
		}

		checkedVector(size_type n, const T& value = T())
		: std::vector<T>(n, value) {
		}

	//	  checkedVector(iterator i, iterator j)
	//      : std::vector<T>(i, j) {}

		reference operator[](difference_type index) {
			assert(index >=0 
				&& index < static_cast<difference_type>(this->size()));
			return std::vector<T>::operator[](index);
		}


			const_reference operator[](difference_type index) const {
			assert(index >=0 
				&& index < static_cast<difference_type>(this->size()));
			return std::vector<T>::operator[](index);
		}

	};
}


#endif

