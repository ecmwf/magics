Using the tools for the difference suite
----------------------------------------

1. Set up your environment to use the version of Magics++
   you wish to use as a reference (usually the older of the
   versions you wish to compare).

2. Run 'make_diff_testsuite -h' to see the options that
   should be used for running the test suite. Note that at
   this stage we are just running the test suite for one
   version, the one we set up the environment for in step 1.
   Run the test suite; an example command line would be:
   
     make_diff_testsuite  suse9  ++1.3.6  double cylindrical


3. Set up your environment to use the version newer of Magics++
   you wish to compare.

4. Run the difference test suite again as in step 2, this time
   using the same parameters except for the version label.

5. Now generate the set of differences and the web page to view them.
     cd diff
   Edit 'version_diff' and change the variables
     new_version_dirs
   and 
     reference_version_dir
   [yes, these should be command-line variables and will be soon]

    Run the script 'version_diff' to generate the differences.
    
6. View the differences in your web browser. The page will have been
   generated in the diff/html directory.



Iain Russell
12th December 2006
