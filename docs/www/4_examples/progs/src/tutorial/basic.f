C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program magics

c open magics      
      call popen 
c setting of parameters : 
c The output postscript filename will be basic.ps
      call psetc ('ps_device',    'ps_a4')
      call psetc ('ps_file_name', 'basic.ps')
c call of an action routine 
      call pcoast
c close magics 
      call pclose
      end
