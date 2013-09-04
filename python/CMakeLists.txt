if ( WITH_PYTHON )

  set(PYTHON_SITE "lib/python${PYTHON_VERSION}/site-packages" )

  set(NUMPY_BUG_TOFIX "../../lib/python2.7/site-packages//numpy/core/include")

  configure_file ( Magics.i.in  Magics.i )

  include_directories ( ${PYTHON_INCLUDE_PATH}  ${PYTHON_INCLUDE_PATH}/${NUMPY_BUG_TOFIX} ${CMAKE_CURRENT_SOURCE_DIR} )
  set_source_files_properties ( Magics.i PROPERTIES GENERATED TRUE CXX ON )
  swig_add_module ( Magics python Magics_interface.cc Magics.i )
  swig_link_libraries ( Magics MagPlus ${PYTHON_LIBRARIES} )
  set(PYTHON_DEST "${PYTHON_SITE}/Magics" )
  
  # place in build tree so we can run python tests
  configure_file ( macro.py macro.py )
  configure_file ( metgram.py metgram.py )
  
  
  # install to final destination
  install ( TARGETS _Magics DESTINATION ${PYTHON_DEST} ) 
  install ( FILES ${CMAKE_CURRENT_BINARY_DIR}/Magics.py 
            macro.py  
            metgram.py 
            DESTINATION ${PYTHON_DEST} )
endif()

