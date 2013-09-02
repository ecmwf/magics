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
