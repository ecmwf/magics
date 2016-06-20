/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*		MAGICS

		Author:		Arne Jorgensen, ECMWF

		Date:		22 November, 2000
		Modified:	27 November, 2000
					24 October,  2002	
					05 October,  2005 (IR)	

		mdiff	compares  file1 with file2. 
			

		Call:
			mdiff [options  files .. ]

		options:
			-ps file1 file2	 	compare in ps format
			-ppm file1 file2	compare in ppm format
			-o file3		make diff into file3
			-check file		check for proper ppm (P6)
			-v			verbose
			-help			Print info





*/

#include <mdiff.hpp>

string file1 = "No Name";
string file2 = "No Name";
string file3 = "No Name";

int help_opt = 0;   // help

int v_opt = 0;
int k_opt = 1;
int ps_opt = 0;
int pps_opt = 0;
int o_opt = 0;
int check_opt = 0;
int check_ppm_opt = 0;
int ppm_opt = 0;
int xv_opt = 0;
int row,col,colours,width;
time_t when;

main(int argc, char *argv[])
{
	
	int varg = 1;
	while ( varg < argc ) {
		
		string entry = argv[varg++];
		
		if( entry == "-v" )
		 	v_opt = 1;

		if( entry == "-k" )
		 	k_opt = 0;

		if( entry == "-show" )
		{
		 	xv_opt = 1;
		}

		if( entry == "-o" )
		{
			o_opt = 1;
			if ( varg < argc ) 
				file3  = argv[varg];
			varg++;		
		}
				
		if ( entry == "-ps" )
		{
			ps_opt = 1;
			if ( varg < argc ) 
				file1  = argv[varg];	
			varg++;
			if ( varg < argc ) 
				file2  = argv[varg];	
			varg++;
		}

		if ( entry == "-ppm" )
		{
			ppm_opt = 1;
			if ( varg < argc ) 
				file1  = argv[varg];	
			varg++;
			if ( varg < argc ) 
				file2  = argv[varg];	
			varg++;
		}
				
		if ( entry == "-check" )
		{
			check_ppm_opt = 1;
			if ( varg < argc ) 
				file1  = argv[varg];	
			varg++;
		}

		if (entry == "-rm" )
		{
			if ( varg < argc )
			{			

				if ( remove(argv[varg]) )
					cout << argv[varg] << " not found" << endl;
				else
					cout << argv[varg] << " removed" << endl;
			}
			varg++;
		}
				
		if (entry == "-help" )
		       help_opt = 1;
		
	}	
	
	time(&when);

	if ( help_opt || (help_opt+ps_opt+ppm_opt+check_ppm_opt) == 0 )
	{
		if ( v_opt)
			cout << ctime(&when) << endl;

		cout << "Usage:\tmdiff [options files..]\n" << "options:\n" <<

		"\t-ppm file1 file2\tdiff in ppm formats\n" <<
		"\t-ps file1 file2\t\tdiff in ps formats\n"  <<
		"\t-o file3\t\tdiff into file3\n" <<
		"\t-show\t\t\tdisplay only if different\n" <<
		"\t-check file\t\tcheck for ppm raw (P6) formst\n" <<
		"\t-v\t\t\tverbose\n" <<
		"\t-k\t\t\tkeep ppm files\n" << 
		"\t-help\t\t\tPrint this info\n" << 
		"Version 1.3 - 05 October, 2005" << endl;
		 
	}

	if ( ps_opt+ppm_opt > 1 )
	{
		cout << " confusing format specifications...exit" << endl;
		exit(1);
	}
	if ( o_opt + xv_opt > 1 )
	{
		cout << " confusing output specifications (either -o OR -show)...exit" << endl;
		exit(1);
	}
	if ( ppm_opt )
	{
		if ( file1 ==  "No Name" || file2 ==  "No Name" )
		{
			cout << "too few files...exit" << endl;
			exit(1);
		}
		if ( file1 == file2 )
		{
			cout << " same files...exit"<< endl;
			exit(1);
		}
		if ( o_opt && file3 == "No Name" )
		{
			cout << "file3 missing...exit" << endl;
			exit(1);
		}
		
		int result = ( o_opt ) ?
			Diff::do_diff_ppm(file1,file2,file3) :
			Diff::do_diff_ppm(file1,file2);		
		
		if ( result == 0 )
		{
			cout << file1 << " and " << file2 <<
			" identical..." << endl;
			exit(0);
		}
		else if ( result == 1 )
			cout << file1 << " and " << file2 <<
			" differ... " << endl;
		else if ( result == -1 )
			cout << " files not compatible... " << endl;
		else  if ( result == -2 )
			cout << "output file not written..." << endl;
		else
			cout << "no ppm raw format(P6) file..." << endl;	
		exit(1);
	}

	if ( ps_opt )
	{
		 if ( v_opt)
			cout << ctime(&when) << endl;

		if ( file1 ==  "No Name" || file2 ==  "No Name" )
		{
			cout << "too few files ...exit" << endl;
			exit(1);
		}
		if ( file1 == file2 )
		{
			cout << " files identical...exit"<< endl;
			exit(1);
		}
		if ( o_opt && file3 == "No Name" )
		{
			cout << "file3 missing...exit" << endl;
			exit(1);
		}
		
		FILE*	fd3;
		char buf[1024];
		string cmd;
		int x1,y1,x2,y2;

		if (	Diff::check_ps(file1,v_opt,x1,y1) || 
			Diff::check_ps(file2,v_opt,x2,y2) )
		{
			cout << "open failed or not PostScript...exit" << endl;
			exit(1);
		}
		
		if ( x1 != x2 || y1 != y2 )
		{
			cout << "ps files not same size...exit" << endl;
			exit(1);
		}

		string file1_ppm = file1 + ".ppm";
		
		char boxx[5];
		char boxy[5];
		sprintf(boxx,"%d",x1);
		sprintf(boxy,"%d",y1);

		cmd = "gs -q -sDEVICE=ppmraw -sOutputFile=" + file1_ppm + 
			" -dBATCH " +  " -g" + boxx + "x" + boxy +
			 " " + file1 + " < /dev/null"; 
		if ( v_opt)
			cout << cmd << endl;

		if (( fd3 = popen(cmd.c_str(), "r")) )
			if ( v_opt)
				while ( fgets(buf,132,fd3) )
					cout << buf << endl;
		pclose(fd3);

		string file2_ppm = file2 + ".ppm";
		
		cmd = "gs -q -sDEVICE=ppmraw -sOutputFile=" + file2_ppm + 
			" -dBATCH " +  " -g" + boxx + "x" + boxy 
			+ " " + file2 + " < /dev/null";
		
		if ( v_opt)
			cout << cmd << endl;

		if (( fd3 = popen(cmd.c_str(), "r")) )
			if ( v_opt)
				while ( fgets(buf,132,fd3) )	
					cout << buf << endl;
		pclose(fd3);
		
		int result;
		if ( o_opt || xv_opt )
		{
			string file3_ppm;
			if ( xv_opt )
				file3_ppm = file2_ppm + ".ppm";
			else 
				file3_ppm = file3 + ".ppm";
			
			result = Diff::do_diff_ppm(file1_ppm,file2_ppm,
				file3_ppm);
				 
			if ( result >= 1 ) // only if different
			{
				if( o_opt )
				{
#if (defined(AIX) || defined(Linux) )

					cmd = "convert " + file3_ppm + " -page "
					+ boxx + "x" + boxy  + " -compress RLE PS:"  + file3;
#else

					cmd = "convert " + file3_ppm +  " -page "
					+ boxx + "x" + boxy  + " PS:"  + file3;
#endif
					//cout << cmd << endl;
				}
				else
				{
					cmd = "xv " +  file3_ppm;
				}
				if ( v_opt )
					cout << cmd << endl;

				if (( fd3 = popen(cmd.c_str(), "r")) )
					if ( v_opt )
						while ( fgets(buf,132,fd3) )
							cout << buf << endl;
				pclose(fd3);
			}
			if ( xv_opt || (k_opt && o_opt) )
					remove(file3_ppm.c_str());
								
		}
		else
			result = Diff::do_diff_ppm(file1_ppm,file2_ppm);

		if ( k_opt )
		{		
			remove(file1_ppm.c_str());
			remove(file2_ppm.c_str());
		}
		
		if ( result == 0  )
		{
			//if ( v_opt )
				cout << file1 << " and " << file2 <<
					" identical..." << endl;
			exit(0);
		}
		else if ( result == -1 )
			cout << " files not compatible... " << endl;
		else if ( result == -2 )
			cout << " files not found..." << endl;
		else
			cout << file1 << " and " << file2 <<
			" differ... (" << result << ")" << endl;

		exit(1);

	}

	if ( check_ppm_opt )
	{
		if ( file1 ==  "No Name" )
		{
			cout << "No file to check ... exit" << endl;
			exit(1);	
		}
		if ( Diff::do_check_ppm(file1,col,row,colours) )
		{
			cout << "ok:\n" <<
			"\tColumns: " << col <<
			"\tRows: " << row <<
			"\tColours: " << colours <<  endl;
			exit(0);
		}
		else
		{
			cout << "not ppm raw format(P6)...exit" << endl;
			exit(1);
		}
	}
}	
