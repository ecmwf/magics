/*
  CalcStreamlines is a function for calculating streamlines for wind field

  Copyright (C) 2010 Hungarian Meteorological Service

  Author: Mark Rajnai (rajnai.m@met.hu)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/


#ifndef CalcStreamlines_h
#define CalcStreamlines_h



// Shift the value between 'xstart' and 'xstart+period'
// -> x in [xstart,xstart+period]
float ShiftPeriod(float x, float xstart, float period);
// Shift the value between 'xstart' and 'xstart+period'
// -> x in [xstart,xstart+period)
float ShiftPeriod_(float x, float xstart, float period);

// Calculate difference of two longitude values considering periodicity
float CalcLonDist(float lon1, float lon2);


// Struct for the equidistant grid geometry
struct GSStruct
{
	// Number of gridpoints
	int nx;
	int ny;

	// Start coordinates (Longitude,Latitude)
	float startx;
	float starty;

	// Distance between the gridpoints
	float dx;
	float dy;

	// Is it periodic? (0/1)
	int period_x;
	//int period_y;
};


// Class to store coordinates of a line
class OneLineClass
{
	public:
		// Length of the line
		int Len;

		// Coordinates of breakpoints
		float *X;
		float *Y;

		// Constructor
		OneLineClass();

		// Destructor
		~OneLineClass();
		
		// Allocate memory for breakpoints
		void Alloc(int len);
};


// Streamline calculator function
int CalcStreamlines(int density, const float *dir, const GSStruct *gs, OneLineClass **&str_lines, int &linenum);


#endif
