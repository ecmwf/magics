/* 
	Arne Jorgensen, ECMWF
	23 November, 2000
*/
#ifndef __mdiff_h_
#define __mdiff_h_

#include "inc_con.h"

#include <time.h>
#include <pwd.h>
#include <unistd.h>
namespace Diff {
	int check_ps(const string&, const int&, int&,  int&);	
	int Get_Real_Line(ifstream&, char*);
	int do_diff_ppm(const string&, const string&, const string&);	
	int do_diff_ppm(const string&, const string&);
	int do_check_ppm(const string&, int&,  int&,  int&);
	int diff_content_ppm(ifstream&, ifstream&, ofstream&,
				const int&, const int&);
	int diff_content_ppm(ifstream&, ifstream&);
	int check_header_ppm(ifstream&, int&,  int&,  int&);
	int do_check_header_ppm(const string&, int&,  int&,  int&);
}

 

int Diff::check_ps(const string& file, const int& v_opt, 
	int& boxx,  int& boxy)
{
	int r = 1;
	ifstream I(file.c_str());
	//ensure (I);
	if ( ! I )
		return r;
	int y;		
	char *cline;
	cline = new char[2048]; 
	int k = Get_Real_Line(I, cline);
	string s = cline;
	//int x = s.find("%!PS-Adobe-3.0");
	int x = s.find("%!PS");
	

	if ( x < 0 )
		return 1;  //no PostScript
	r = 0;
	boxx = boxy = 0;
	float xx,yy;
	xx = yy = 0.0;
	if ( ! r )
	{
		if ( v_opt )
			cout <<  "File " << file << endl;

		int max_lines = 10;
		int i;
		for ( i = 0; i < max_lines; i++)
		{
			k = Get_Real_Line(I, cline);
			s = cline;
			x = s.find("%%BoundingBox:");
			if ( x >= 0 )
			{	
				y = s.find("atend");
				if ( y >=0 ) 
				{
					xx = 595.0;
					yy = 841.0;
				}
				else
				{
				s.erase(0,x+15);
				istringstream ist(s.c_str());

				float dum1, dum2;
				ist >>dum1 >> dum2 >> xx >> yy;
				}
				if ( xx > 0 && yy > 0 && v_opt )
					cout << "\t%%BoundingBox: " <<
						xx << " " << yy << endl;

				
			}
			

			x = s.find("%%Title:");
			if ( x >=0 && v_opt )
				cout << "\t" << s << endl;
			
			x = s.find("%%CreationDate");
			if ( x >=0 && v_opt )
				cout << "\t" << s << endl;		

		}  
	}

	boxx = xx;  boxy = yy;
	if ( boxx == 0 || boxy == 0 )
	{
		boxx = 595;
		boxy = 841;
	
	}
	//	r = 1;

	I.close();
	delete [] cline;

	return r;

}

int Diff::Get_Real_Line(ifstream& I, char* cline)
{
	int k = 0;
	char ch;
	while ( I.get(ch) )
	{
		cline[k] = ch;
		if ( ch == '\n' || ch == '\0' || I.eof() )
			break;
		k++;	     	
	}
	cline[k] = '\0';
	return k;

}

int Diff::do_check_header_ppm(const string& file, int& col, int& row, 
	int& colours)
{
	ifstream I(file.c_str());
	if ( ! I )
		return 0;
	
	int r = Diff::check_header_ppm(I,col,row,colours);
	
	I.close();
	return 1 - r;

}

int Diff::check_header_ppm(ifstream& I, int& col, int& row, int& colours)
{
	
	char *cline;
	cline = new char[2048];

	int k = Get_Real_Line(I, cline);
	string s = cline;
	int r = 0;
	int x = s.find("P6");
	
	if ( x < 0 )
		r = 1;// not PPM P6 format
	
	if ( ! r )
	{
		cline[0] = '#';
		while ( cline[0] == '#')  // skip comments
			k = Get_Real_Line(I, cline);

		istringstream ist1(cline);     	
		ist1 >> col >> row;
		k = Get_Real_Line(I, cline);
		istringstream ist2(cline);
		ist2 >> colours;
	
		if ( col == 0 || row == 0 || colours == 0 )
			r =  1;
	}

	delete [] cline;
	return r;

}

int Diff::do_check_ppm(const string& file, int& col, int& row, int& colours)
{
	ifstream I(file.c_str());
	if ( ! I )
	{
		cout << file << " not found ...exit" << endl;
		exit(1);
	}
	
	int r = Diff::check_header_ppm(I,col,row,colours);
	
	if ( ! r )
	{
		int k = 0;
		char ch;
		while( I.get(ch) )
			k++;

		r = (k != 3*row*col);
		if ( r )
			cout << "mismatch - rows:" << row  << " cols: " << col <<
			" ( " << 3*row*col << " chars) - actual read:" << k << endl;
	}

	I.close();	
	return 1 - r;

}

int Diff:: do_diff_ppm(const string& f1, const string& f2, const string& f3)
{
	int	col1,row1,colours1,
		col2,row2,colours2;

	int r = 0;

	ifstream I1(f1.c_str());
	ifstream I2(f2.c_str());
	
	if ( ! I1 || ! I2 )
		return -2;
		
	if (	Diff::check_header_ppm(I1,col1,row1,colours1) ||
		Diff::check_header_ppm(I2,col2,row2,colours2) )
		r = -1;

	if ( col1 != col2 || row1 != row2 || colours1 != colours2 )	
		r = -1;
	
	if ( ! r )
	{
		ofstream O(f3.c_str());
       		if  ( O )
		{ 
			O << "P6\n# PPM raw image by mdiff\n";
			O << col1 << " " << row1 << "\n" << colours1 << endl;
			r = Diff::diff_content_ppm(I1,I2,O,col1,row1);
			O.close();
		}
		else 
			r = -2;
	}

	I1.close();
	I2.close();

	return r;
}

int Diff::do_diff_ppm(const string& f1, const string& f2)
{
	int	col1,row1,colours1,
		col2,row2,colours2;

	int r = 0;

	ifstream I1(f1.c_str());
	ifstream I2(f2.c_str());
	
	if ( ! I1 || ! I2 )
		return -2;

	if (	Diff::check_header_ppm(I1,col1,row1,colours1) ||
		Diff::check_header_ppm(I2,col2,row2,colours2) )
		r = -1;

	if ( col1 != col2 || row1 != row2 || colours1 != colours2 )
		r = -1;

	if ( ! r )
       		r = Diff::diff_content_ppm(I1,I2);
	
	I1.close();
	I2.close();

	return r;
}

int Diff::diff_content_ppm(ifstream& I1, ifstream& I2, ofstream& O,
	const int& col, const int& row)
{
	int r = 0; // means identical
	int csize = row*col;

	unsigned char cx1,cx2,cx3;
	unsigned char cy1,cy2,cy3;

	unsigned char *ch1;
	unsigned char *cr1;
	unsigned char *c1;
	c1 = new unsigned char [csize*3];
	unsigned char *c2;
	c2 = new unsigned char [csize*3];
	ch1 = c1;
	cr1 = c2;
	
	unsigned char ff = 0xff; //white when identical
	int i;
	

	// Iain Russell, 5th October 2005
	// These next two lines of code originally used getline() to read the data.
	// However, this function stops at a newline (ASCII 10) and so stopped
	// prematurely on some satellite images. We now use read().

	I1.read((char*)c1,csize*3);
	I2.read((char*)c2,csize*3);


	if ( ! I1.eof() || ! I1.eof() )
	{ 
		for ( i = 0; i < csize;i++)
		{
			cx1 = *c1++;
			cx2 = *c1++;
			cx3 = *c1++;
			cy1 = *c2++;
			cy2 = *c2++;
			cy3 = *c2++;
            
			if ( cx1 == cy1 && cx2 == cy2 && cx3 == cy3 )
				O << ff << ff << ff;	
			else
	       		{
				r++; // different - return count
				//O << cx1 << cx2 << cx3;
				// If first is white - write out second
				if ( cx1 == cx2 && cx1 == cx3 && cx1 == ff )
					O << cy1 << cy2 << cy3;
				else
					O << cx1 << cx2 << cx3;
			}
		}			
		
	}
	else
		r = -1;// ppm problem

	delete [] ch1;
	delete [] cr1;

	return r;
}
		
int Diff::diff_content_ppm(ifstream& I1, ifstream& I2)
{
	//unsigned char ch1,ch2;
	char ch1,ch2;

	while ( I1.get(ch1) && I2.get(ch2) )
	{
		if ( ch1 != ch2 )
			return 1;	
	}
	return 0;	
}
#endif /* __mdiff_h_ */
