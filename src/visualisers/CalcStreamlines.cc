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


#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <float.h>

#include "CalcStreamlines.h"


float ShiftPeriod(float x, float xstart, float period)
{
	float xend = xstart + period;
	// Shift the value between 'xstart' and 'xstart+period'
	while( x > xend )
		x -= period;
	while( x < xstart )
		x += period;

	return x;
}


float ShiftPeriod_(float x, float xstart, float period)
{
	float xend = xstart + period;
	// Shift the value between 'xstart' and 'xstart+period'
	while( x >= xend )
		x -= period;
	while( x < xstart )
		x += period;

	return x;
}


// Calculate difference of two longitude values considering periodicity
float CalcLonDist(float lon1, float lon2)
{
	float dlon = ShiftPeriod(lon2-lon1, 0, 360);
	if( dlon > 180 )
		return 360-dlon;
	return dlon;
}


OneLineClass::OneLineClass() :
	Len(0),
	X(0x0),
	Y(0x0)
{}


OneLineClass::~OneLineClass()
{
	delete [] X;
	delete [] Y;
}


void OneLineClass::Alloc(int len)
{
	if( len > 0 )
		{
			X = new float[len];
			Y = new float[len];
			Len = len;
		}
}


int CalcStreamlines(int density, const float *dir, const GSStruct *gs, OneLineClass **&str_lines, int &linenum)
{
	// Input arguments:
	// density: density of the streamlines 1: highest density 2,3,... lower density
	// dir: wind direction field (in radian)
	// gs: pointer of the grid geometry (lat-lon values in degree)

	// Output arguments:
	// str_lines: the result lines (lat-lon values in degree)
	// linenum: number of lines in the result (length of 'str_lines' array)

	// This algorithm works only with equidistant longitude-latitude grid

	// Check the pointers
	if( !dir || !gs )
		return 0;

	// Check density of the streamlines
	if( density < 1 )
		return 0;

	// Remove unnecessary streamlines
	if( str_lines )
		{
			for(int ii=0; ii<linenum; ii++)
				delete str_lines[ii]; 
			delete [] str_lines;
			str_lines = 0x0; 
		}
	linenum = 0;

	// Get gridshape
	float startx = gs->startx;
	float dx_orig = gs->dx;
	float starty = gs->starty;
	float dy_orig = gs->dy;
	int nx = gs->nx;
	int ny = gs->ny;
	int gridsize = nx*ny;
	int ys = (dy_orig > 0) ? 1 : -1;
	int xs = (dx_orig > 0) ? 1 : -1;
	float dy = fabsf(dy_orig);
	float dx = fabsf(dx_orig);
	float dx_2 = 0.5*dx;
	float dy_2 = 0.5*dy;
	// Use a little bigger dx and dy for comparing (in order to avoid floating
	// point errors) -> inrease them with an epsilon that is 0.1% of the original
	// values (perhaps it would be unnecessary if double would be used)
	float dx_2e = dx_2 * 1.001;
	float dy_2e = dy_2 * 1.001;
	int period_x = gs->period_x;
	int gs_geo =  gs->gs_geo;
	//int period_y = gs->period_y;

	float *dir_tmp = 0x0;
	int abut_x = 0;
	if( period_x )
		{
			float x_range = (nx-1)*dx;
			if( x_range > 359.999 && x_range < 360.001 ) // x_range == 360.
				abut_x = 1;
		}


	// x_min and x_per are used for ShiftPeriod function
	float x_min = 0.f, x_per = 360.f; // for grids on the Earth
	if( !gs_geo )
		{
			// for grids not on the Earth
			x_min = -100000000.f;
			x_per = FLT_MAX;
		}

	// A flag grid to store how many streamlines cross a grid cell
	int *cell_used = new int[ gridsize ];
	// We allow up to 4 lines to cross a cell (must be max_used+1)!!
	int *cell_used_by[4];
	int *cell_used_by_point[4];
	for(int i=0; i<gridsize; i++)
		cell_used[i] = 0;
	for(int j=0; j<4; j++)
		{
			cell_used_by[j] = new int[ gridsize ];
			cell_used_by_point[j] = new int[ gridsize ];
			for(int i=0; i<gridsize; i++)
				cell_used_by[j][i] = -1;
		}
	int line_start_array[100000];

	// The ordinal number of the current line we follow
	int act_line = 0;
	
	// Coordinates of a streamline
	float *x = new float[ gridsize ];
	float *y = new float[ gridsize ];
	int len = 0;
	int pos = gridsize/2;
	int line_start = pos;

	// Active cell and side in the cell
	int act_cell;
	int act_side;
	float side_shift_x[4] = { 0, 0.5*dx, 0, -0.5*dx};
	float side_shift_y[4] = { 0.5*dy, 0, -0.5*dy, 0};

	// Coordinate of center of the active cell
	float cell_x, cell_y;

	// One cell is a rectangle around the gridpoint
	//
	//            side 0
	//       +---------------+
	//       |       /       |
	//       |      /        |
	//       |     /    _    |
	//       |    /  |  /`   |
	//       |   /   |a/     |side 1   a = direction of the vector
	// side 3|  /    |/      |
	//       | /     X       |dy
	//       |/              |
	//       |               |
	//       |               |
	//       |               |
	//       |               |
	//       +---------------+
	//              dx
	//            side 2
	//

	// Double-float comparison problem
	// We use float numbers instead of doubles
	float M__PI_2 = M_PI_2;
	float M__PI = M_PI;
	float M3_PI_2 = 3*M_PI_2;
	float M_PI_180 = M_PI/180.0;

	// Minimum density in cell number
	int min_dist = density / 4;
	// Value of max_used can be 3!!!! See cell_used_by array!!!
	int max_used = 2 / density + 1;

	// Some constants for the density
	// Minimum density in degrees
	float max_dx = fabsf(dx_orig) / (float)max_used / 1.3f;
	float max_dy = fabsf(dy_orig) / (float)max_used / 1.3f;

	// density2 and start_index_[ij] are only for the more equable density of lines
	//int density2 = density * 4;
	//for(int start_index_j=0; start_index_j<=density*3; start_index_j+=density)
		//for(int start_index_i=0; start_index_i<=density*3; start_index_i+=density)
	for(int density2=density*64; density2>=density; density2/=2)
	for(int j=0; j<ny; j+=density2)
		for(int i=0; i<nx; i+=density2)
			{
				act_cell = j*nx + i;
				// If the cell is already in use, skip
				//if( cell_used[act_cell] > 0 )
				//	continue;

				// If the neighbouring cells are already in use, skip
				// Check the close used cells in a cross shape
				//
				//        x
				//        x
				//        x
				//    xxxxxxxxx
				//        x
				//        x
				//        x
				//
				int used = 0;
				for(int c=act_cell-min_dist; c<=act_cell+min_dist; c++)
					if( c > -1 && c < gridsize && cell_used[c] > 0 )
						{
							used = 1;
							break;
						}
				if( used )
					continue;
				for(int c=act_cell-min_dist*nx; c<=act_cell+min_dist*nx; c+=nx)
					if( c > -1 && c < gridsize && cell_used[c] > 0 )
						{
							used = 1;
							break;
						}
				if( used )
					continue;

				// Check NAN FillValue
				if( dir[act_cell] != dir[act_cell] )
					continue;

				// Get coordinate of the cell
				cell_x = ShiftPeriod(startx + dx_orig * i, x_min, x_per);
				// Quicker if this line is moved before 'for(i)'...
				cell_y = starty + dy_orig * j;

				// Check the direction in the first cell
				// Let's suppose that values are in interval [0,2PI)
				// 'direction' is -1 first than will be changed to 1!!
				float dir_limit;
			 	if( gs_geo )
					dir_limit = atan(fabs(dx*cos(cell_y*M_PI_180)/dy));
				else
					dir_limit = atan(fabs(dx/dy));
				float act_dir = ShiftPeriod_(dir[act_cell], 0, 2*M__PI);

				if( act_dir > 2*M__PI-dir_limit || act_dir < dir_limit )
					act_side = 2;
				else if( act_dir < M__PI - dir_limit )
					act_side = 3;
				else if( act_dir < M__PI + dir_limit )
					act_side = 0;
				else
					act_side = 1;

				// Let's start from the middle of the side
				len = 0;
				pos = gridsize/2;
				line_start = pos;
				x[pos] = ShiftPeriod(cell_x + side_shift_x[act_side], x_min, x_per);
				y[pos] = cell_y + side_shift_y[act_side];
				len++;
				pos--;

				// Set start cell for the forward following
				int forw_start_cell = -1;
			 	if( act_side == 0 )
					forw_start_cell = act_cell + ys*nx;
				else if( act_side == 2 )
					forw_start_cell = act_cell - ys*nx;
				else if( act_side == 1 )
					{
						if( abs( act_cell % nx - (act_cell+xs) % nx) > 1 )
							{
								if( period_x == 1 )
									forw_start_cell = act_cell + xs - xs*(nx - abut_x);
							}
						else
							forw_start_cell = act_cell + xs;
					}
				else if( act_side == 3 )
					{
						if( abs( act_cell % nx - (act_cell-xs) % nx) > 1 )
							{
								if( period_x == 1 )
									forw_start_cell = act_cell - xs + xs*(nx - abut_x);
							}
						else
							forw_start_cell = act_cell - xs;
					}

				int forw_start_side = (act_side + 2)%4;

				// Follow a streamline backward (-1) and then forward (1) too
				for(int direction = -1; direction <= 1; direction+=2)
					{

						if( direction == 1 )
							{
								if( forw_start_cell < 0 || forw_start_cell >= gridsize )
									break;
								act_cell = forw_start_cell;
								act_side = forw_start_side;
								pos = gridsize/2 + 1;
							}

						// Some help variables for the very variable wind direction (light wind)
						int cycle_cont = 0;
						float prev_dir, act_dir_orig;

						int stop_need = 0;

						do
							{
								// Check if stop needed (it can be also 2!!!)
								if( stop_need == 1 ) // hmmm.. now it can be only 2
									break;
								// Check if this line uses the same cell twice
								if( cell_used[act_cell] && cell_used_by[cell_used[act_cell]-1][act_cell] == act_line )
									break;
								// Check if too many lines are in the cell
								if( cell_used[act_cell] > max_used )
									break;
								// Check if any other line already crosses the cell
								if( density > 4 && cell_used[act_cell] > 0 )
									{
										// If previously it was also too close, stop it immediately
										if( stop_need == 2 )
											break;
										stop_need = 2; // Let's give an other chance!
									}
    
								// act_dir is in [0;2*M__PI)
								if( direction == 1 )
									act_dir = ShiftPeriod_(dir[act_cell]+M__PI, 0, 2*M__PI);
								else
									act_dir = ShiftPeriod_(dir[act_cell], 0, 2*M__PI);
								act_dir_orig = act_dir;

								// If wind direction is not valid break
								if( act_dir != act_dir )
									break;

								// Get coordinate of the cell
								// act_cell <=> j*nx + i
								cell_x = ShiftPeriod(startx + dx_orig * (act_cell%nx), x_min, x_per);
								cell_y = starty + dy_orig * (act_cell/nx);
								if( gs_geo )
									if( cell_y >= 90 || cell_y <= -90 )
										break;
    
								float cos_cell_y = 1.f;
								if( gs_geo )
									cos_cell_y = cos(cell_y*M_PI_180);
								int cell_shift = 0;
    
								// Check which side we are on
								if( act_side == 1 )
									{
										// Check if the line can be continue (headwind)
										if( act_dir > 0 && act_dir < M__PI )
											{
												// Compare the neighbouring wind directions
												if( cycle_cont &&
														( prev_dir > M__PI_2 && prev_dir < M3_PI_2 ) ==
														( act_dir > M__PI_2 && act_dir < M3_PI_2 ) )
													act_dir = ( act_dir > M__PI_2 ) ? M__PI : 0;
												else
													break;
											}
    
										if( act_dir == 0 )
											{
												// Northward
												x[pos] = x[pos-direction];
												y[pos] = cell_y + dy_2;
												
												cell_shift = ys*nx;
												act_side = 2;
											}
										else if( act_dir == M__PI )
											{
												// Southward
												x[pos] = x[pos-direction];
												y[pos] = cell_y - dy_2;

												cell_shift = - ys*nx;
												act_side = 0;
											}
										else
											{
												double tan_dir = tan(act_dir);
												float new_y = y[pos-direction] - dx * cos_cell_y / tan_dir;
												if( act_dir == M3_PI_2)
													new_y = y[pos-direction];
												if( new_y <= cell_y + dy_2e &&
														new_y >= cell_y - dy_2e )
													{
														// Westward
														x[pos] = ShiftPeriod(cell_x - dx_2, x_min, x_per);
														y[pos] = new_y;
    
														cell_shift = - xs;
														act_side = 1;
													}
												else
													{
														if( act_dir > M3_PI_2 )
															{
																// Northward
																x[pos] = ShiftPeriod(x[pos-direction] + (cell_y + dy_2 - y[pos-direction]) * tan_dir / cos_cell_y, x_min, x_per);
																y[pos] = cell_y + dy_2;
																
																cell_shift = ys*nx;
																act_side = 2;
															}
														else
															{
																// Southward
																x[pos] = ShiftPeriod(x[pos-direction] - (y[pos-direction] - cell_y + dy_2) * tan_dir / cos_cell_y, x_min, x_per);
																y[pos] = cell_y - dy_2;
																
																cell_shift = - ys*nx;
																act_side = 0;
															}
													}
											}
									}	// if( act_side == 1 )
								else if( act_side == 3 )
									{
										// Check if the line can continue (headwind)
										if( act_dir > M__PI && act_dir < 2*M__PI )
											{
												// Compare the neighbouring wind directions
												if( cycle_cont &&
														( prev_dir > M__PI_2 && prev_dir < M3_PI_2 ) ==
														( act_dir > M__PI_2 && act_dir < M3_PI_2 ) )
													act_dir = ( act_dir < M3_PI_2 ) ? M__PI : 0;
												else
													break;
											}
    
										if( act_dir == 0 )
											{
												// Northward
												x[pos] = x[pos-direction];
												y[pos] = cell_y + dy_2;
												
												cell_shift = ys*nx;
												act_side = 2;
											}
										else if( act_dir == M__PI )
											{
												// Southward
												x[pos] = x[pos-direction];
												y[pos] = cell_y - dy_2;
    
												cell_shift = - ys*nx;
												act_side = 0;
											}
										else
											{
												double tan_dir = tan(act_dir);
												float new_y = y[pos-direction] + dx * cos_cell_y / tan_dir;
												if( act_dir == M__PI_2)
													new_y = y[pos-direction];
												if( new_y <= cell_y + dy_2e &&
														new_y >= cell_y - dy_2e )
													{
														// Eastward
														x[pos] = ShiftPeriod(cell_x + dx_2, x_min, x_per);
														y[pos] = new_y;
    
														cell_shift = xs;
														act_side = 3;
													}
												else
													{
														if( act_dir < M__PI_2 )
															{
																// Northward
																x[pos] = ShiftPeriod(x[pos-direction] + (cell_y + dy_2 - y[pos-direction]) * tan_dir / cos_cell_y, x_min, x_per);
																y[pos] = cell_y + dy_2;
																
																cell_shift = ys*nx;
																act_side = 2;
															}
														else
															{
																// Southward
																x[pos] = ShiftPeriod(x[pos-direction] - (y[pos-direction] - cell_y + dy_2) * tan_dir / cos_cell_y, x_min, x_per);
																y[pos] = cell_y - dy_2;
																
																cell_shift = - ys*nx;
																act_side = 0;
															}
													}
											}
									}	// if( act_side == 3 )
								else if( act_side == 0 )
									{
										// Check if the line can continue (headwind)
										if( act_dir < M__PI_2 || act_dir > M3_PI_2 )
											{
												// Compare the neighbouring wind directions
												if( cycle_cont &&
														( prev_dir > 0 && prev_dir < M__PI ) ==
														( act_dir > 0 && act_dir < M__PI ) )
													act_dir = ( act_dir < M__PI ) ? M__PI_2 : M3_PI_2;
												else
													break;
											}
    
										if( act_dir == M__PI_2 )
											{
												// Eastward
												x[pos] = ShiftPeriod(cell_x + dx_2, x_min, x_per);
												y[pos] = y[pos-direction];
												
												cell_shift = xs;
												act_side = 3;
											}
										else if( act_dir == M3_PI_2 )
											{
												// Westward
												x[pos] = ShiftPeriod(cell_x - dx_2, x_min, x_per);
												y[pos] = y[pos-direction];
    
												cell_shift = - xs;
												act_side = 1;
											}
										else
											{
												double tan_dir = tan(act_dir);
												float new_x = x[pos-direction] - dy * tan_dir / cos_cell_y;
												if( gs_geo )
													new_x = ShiftPeriod(new_x, cell_x - dx_2e, 360);
												if( new_x <= cell_x + dx_2e &&
														new_x >= cell_x - dx_2e )
													{
														// Southward
														x[pos] = ShiftPeriod(new_x, x_min, x_per);
														y[pos] = cell_y - dy_2;
    
														cell_shift = - ys*nx;
														act_side = 0;
													}
												else
													{
														if( act_dir < M__PI )
															{
																// Eastward
																x[pos] = ShiftPeriod(cell_x + dx_2, x_min, x_per);
																if( gs_geo )
																	y[pos] = y[pos-direction] + CalcLonDist(cell_x + dx_2,x[pos-direction]) / tan_dir * cos_cell_y;
																else
																	y[pos] = y[pos-direction] + fabs(cell_x + dx_2 - x[pos-direction]) / tan_dir * cos_cell_y;
																
																cell_shift = xs;
																act_side = 3;
															}
														else
															{
																// Westward
																x[pos] = ShiftPeriod(cell_x - dx_2, x_min, x_per);
																if( gs_geo )
																	y[pos] = y[pos-direction] - CalcLonDist(x[pos-direction], cell_x - dx_2) / tan_dir * cos_cell_y;
																else
																	y[pos] = y[pos-direction] - fabs(x[pos-direction] - cell_x + dx_2) / tan_dir * cos_cell_y;
																
																cell_shift = - xs;
																act_side = 1;
															}
													}
											}
									}	// if( act_side == 0 )
								else if( act_side == 2 )
									{
										// Check if the line can continue (headwind)
										if( act_dir > M__PI_2 && act_dir < M3_PI_2 )
											{
												// Compare the neighbouring wind directions
												if( cycle_cont &&
														( prev_dir > 0 && prev_dir < M__PI ) ==
														( act_dir > 0 && act_dir < M__PI ) )
													act_dir = ( act_dir < M__PI ) ? M__PI_2 : M3_PI_2;
												else
													break;
											}
    
										if( act_dir == (float)M__PI_2 )
											{
												// Eastward
												x[pos] = ShiftPeriod(cell_x + dx_2, x_min, x_per);
												y[pos] = y[pos-direction];
												
												cell_shift = xs;
												act_side = 3;
											}
										else if( act_dir == M3_PI_2 )
											{
												// Westward
												x[pos] = ShiftPeriod(cell_x - dx_2, x_min, x_per);
												y[pos] = y[pos-direction];
    
												cell_shift = - xs;
												act_side = 1;
											}
										else
											{
												double tan_dir = tan(act_dir);
												float new_x = x[pos-direction] + dy * tan_dir / cos_cell_y;
												if( gs_geo )
													new_x = ShiftPeriod(new_x, cell_x - dx_2e, 360);
												if( ( new_x <= ( cell_x + dx_2e ) ) &&
														( new_x >= ( cell_x - dx_2e ) ) )
													{
														// Northward
														x[pos] = ShiftPeriod(new_x, x_min, x_per);
														y[pos] = cell_y + dy_2;
    
														cell_shift = ys*nx;
														act_side = 2;
													}
												else
													{
														if( act_dir < M__PI_2 )
															{
																// Eastward
																x[pos] = ShiftPeriod(cell_x + dx_2, x_min, x_per);
																if( gs_geo )
																	y[pos] = y[pos-direction] + CalcLonDist(cell_x + dx_2, x[pos-direction]) / tan_dir * cos_cell_y;
																else
																	y[pos] = y[pos-direction] + fabs(cell_x + dx_2 - x[pos-direction]) / tan_dir * cos_cell_y;
																
																cell_shift = xs;
																act_side = 3;
															}
														else
															{
																// Westward
																x[pos] = ShiftPeriod(cell_x - dx_2, x_min, x_per);
																if( gs_geo )
																	y[pos] = y[pos-direction] - CalcLonDist(x[pos-direction], cell_x - dx_2) / tan_dir * cos_cell_y;
																else
																	y[pos] = y[pos-direction] - fabs(x[pos-direction] - cell_x + dx_2) / tan_dir * cos_cell_y;
																
																cell_shift = - xs;
																act_side = 1;
															}
													}
											}
									}	// if( act_side == 2 )

								/*if( x[pos] != x[pos] )
									printf("x%d nan act_side = %d act_dir = %f\n", pos, act_side, act_dir);
								if( y[pos] != y[pos] )
									printf("y%d nan act_side = %d act_dir = %f\n", pos, act_side, act_dir);*/
								if( gs_geo )
									if( y[pos-direction] > 90 || y[pos-direction] < -90 )
										break;


								// Check if the break point (x,y) is already used by an other line
								// If the current (x,y) coordinate is already used finish the line
								//if( max_used > 1 ) Hey it should be always checked!
									{
										int stop_now = 0;
										for(int l=0; l<cell_used[act_cell]; l++)
											{
												// cell_used_by[l][act_cell] can not be greater than linenum
												const OneLineClass &line = *str_lines[ cell_used_by[l][act_cell] ];
												int lp = cell_used_by_point[l][act_cell] - line_start_array[cell_used_by[l][act_cell]];
												//if( line.X[lp] == x[pos] && line.Y[lp] == y[pos] )
												// This condition makes more equable line density
												// CalcSpereDist would be better (but slower and it changes with latitude)
												if( lp >= line.Len )
													printf("Error in CalcStreamlines %d > %d\n", lp, line.Len);
												if( line.X[lp] < x[pos]+max_dx && line.Y[lp] < y[pos]+max_dy &&
														line.X[lp] > x[pos]-max_dx && line.Y[lp] > y[pos]-max_dy )
													{
														// If previously this line was too close to an other one stop it
														// immediately
														if( stop_need == 2 )
															stop_now = 1;
														stop_need = 2; // instead of 1 (2 means we give an other chance)
														break;
													}

												// This was the slow version:
												//for(int lp=0; lp<line.Len; lp++)
												//	{
												//		if( line.X[lp] == x[pos] && line.Y[lp] == y[pos] )
												//			{
												//				stop_need = 1;
												//				break;
												//			}
												//	}
												//if( stop_need )
												//	break;
											}
										if( stop_now )
											break;

									}

								// Lehet, ide kene betenni a szurest ami lent volt!
								if( x[pos] == x[pos-direction] && y[pos] == y[pos-direction] )
									{
										pos-=direction;
										len--;
									}

								cell_used_by[cell_used[act_cell]][act_cell] = act_line;
								cell_used_by_point[cell_used[act_cell]][act_cell] = pos;
								cell_used[act_cell]++;

								pos+=direction;
								len++;

								// Handle periodic grid
								if( period_x )
									{
										if( cell_shift == -1 && act_cell % nx == 0 )
											act_cell += cell_shift + nx - abut_x;
										else if( cell_shift == 1 && act_cell % nx == nx-1 )
											act_cell += cell_shift - nx + abut_x;
										else
											act_cell += cell_shift;
									}
								else
									{
										if( ((cell_shift == -1) && (act_cell % nx == 0)) ||
											((cell_shift ==  1) && (act_cell % nx == nx-1)) )
											break;
										act_cell += cell_shift;
									}

								// Save the current direction
								prev_dir = act_dir_orig;
								cycle_cont = 1;
							} // do
						while( act_cell >=0 && act_cell < gridsize && pos > -1 && len < gridsize );

						if( direction == -1 )
							{
								line_start = pos + 1;
								line_start_array[act_line] = line_start;
							}

					} // for(direction = forward, backward)
				
				// Add a new line section with 'len' length
				if( len > 3 )
					{
						// Remove unnecessery duplicated points (needed for spline)
						int real_len = len;
						/*for(int k=1, l=line_start+1; k<len; k++, l++)
							{
								if( x[l] == x[l-1] && y[l] == y[l-1] )
									real_len--;
							}*/
						if( real_len > 3 )
							{
								// Increase ordinal number of the line
								act_line++;

								OneLineClass *line = new OneLineClass;
								line->Alloc(real_len);
								line->X[0] = x[line_start];
								line->Y[0] = y[line_start];
								for(int k=1, ll=1, l=line_start+1; ll<len; ll++, l++)
									{
										
											{
												line->X[k] = x[l];
												line->Y[k] = y[l];
												k++;
											}
									}

                              	linenum++;
								OneLineClass** new_str_lines = new OneLineClass*[linenum];
								for(int ii=0; ii<linenum-1; ii++)
									new_str_lines[ii] = str_lines[ii]; 
								new_str_lines[linenum-1] = line;
													    
								delete [] str_lines;
								str_lines = new_str_lines; 
							}
					}


			} // for(i,j)

	// Free temporary arrays
	delete [] cell_used;
	for(int j=0; j<4; j++)
		{
			delete [] cell_used_by[j];
			delete [] cell_used_by_point[j];
		}
	//delete [] cell_used_by;
	//delete [] cell_used_by_point;
	delete [] x;
	delete [] y;

	return 1;
}

