.. magics documentation master file, created by
   sphinx-quickstart on Tue Feb 16 16:17:14 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to magics's documentation!
==================================

.. toctree::
   :maxdepth: 4
   



Magics Parameters
=================
.. table:: Truth table for "not"
   :widths: auto

   


=====================
Contouring parameters
=====================

   =======  ===== =======
   Name     Doc   default
   =======  ===== =======
   Param1   doc1  def1
   =======  ===== =======
   Param2   doc2  def2
   =======  ===== =======



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

Documentation
==================
=============
Scene objects
=============
.. note::

   This is a note!

.. doxygenclass:: magics::BasicSceneObject
   :members:

=============
Data Decoders
=============


.. doxygenclass:: magics::Decoder
   :members:
   :undoc-members:


.. doxygenclass:: magics::GribDecoder
   :members:

.. doxygenclass:: magics::GribRegularInterpretor
   :members:

.. doxygenclass:: magics::NetcdfDecoder
   :members:



===========
Visualisers
===========

.. doxygenclass:: magics::Contour
   :members:

.. doxygenclass:: magics::Coastlines
   :members:
   

=================
Graphical objects
=================
.. doxygenclass:: magics::BasicGraphicsObject
   :members:

=======
Drivers
=======
.. doxygenclass:: magics::BaseDriver
   :members:
   :undoc-members:

.. doxygenclass:: magics::CairoDriver
   :members:
 