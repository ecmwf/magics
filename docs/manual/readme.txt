Building the Magics++ Documentation
-----------------------------------

Note 1: all scripts in the root directory can be run with th
        '-h' option in order to see more options.

Note 2: set up your Magics++ environment beforehand.

Note 3: differences when building the tutorial are in brackets ().

Note 4: the PDF version comes in several separate chapters. This is not
        possible with the HTML version, so for that, all chapters
        are always processed together.


First time building the Magics++ documentation
----------------------------------------------


0. Get into the right directory:
     magics/docs/manual
   
0a. If you have had unsuccessful attempts at building the documentation:
     tools/clean

1. Compile and run the example programs:
     ./make_plots
       (make_plots -tutorial if building the tutorial)

2. Check that this has worked:
   Look at examples/manual/fortran/gif
     (or examples/tutorial/fortran/gif)

3. Generate the example source code in DocBook format:
     tools/generate_example_listings

4. Create the database of cross-references
   PDF version:
     ./make_all_docs -pdf -targets
   HTML version:
     ./make_all_docs -html -targets

5. Create all the documentation
   PDF version:
     ./make_all_docs -pdf
     ./make_docs -pdf -book=magics_tutorial
   HTML version:
     ./make_all_docs -html

5a. Alternatively, create a single chapter (PDF only), e.g.:
     ./make_docs -pdf -book=magics_tutorial
     ./make_docs -pdf -book=magics_contour
    The name of the chapter is from the files in the chapters directory.
     
6. Check the output:
     publish/pdf/
     publish/html/


After changing the text in a chapter
------------------------------------

Chapter text is in chapters/*.xml.

It is usually enough to perform step 5 or 5a.

If you have introduced a new 'target', ie a section or other
tag with an id which is cross-referenced by either the same or another
chapter, you must refresh the target database for the chapter with
the new target, e.g.:
     make_docs -pdf -book=magics_drivers -targets
     make_docs -html -targets


Updating an example program
---------------------------

a. The FORTRAN source to edit is in
     magics/docs/www/4_examples/progs/src/manual
     magics/docs/www/4_examples/progs/src/tutorial

   The source for a C program is in
     magics/docs/www/4_examples/progs/c/manual
     magics/docs/www/4_examples/progs/c/tutorial

   The source for a MagML program is in
     magics/docs/www/4_examples/progs/magml/manual
     magics/docs/www/4_examples/progs/magml/tutorial

b. Ensure that the name of the output PostScript matches the
   name of the program (ie, check the source code)

c. To run only the updated example:
     make_plots -noclean
     make_plots -noclean -tutorial  (if building the tutorial)

d. If a new example has been added, then run step 3.

e. Run step 5 (or 5a).



After XML parameter files have been updated
-------------------------------------------

Run step 5 or 5a.


------------------------
Iain Russell
Last Updated: 11/06/2007
