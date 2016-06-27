C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program multidriver
c
     
      
      call popen
      call pset1c ('devices',['svg', 'ps', 'gd'], 3)
      
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','coast.ps')
      call psetc ('gd_file_name','coast.gif')
      call psetc ('svg_file_name','coast.svg')
      
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
    
      call psetc ('map_grid','on')   
      call pcoast
	  call pclose
      stop
      end
