if( HAVE_PYTHON )

  configure_file( setup.py.in setup.py )
  configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/../src/common/magics_api.h magics_api.h COPYONLY )
  configure_file( numpy.i numpy.i COPYONLY )
  configure_file( Magics_interface.cc Magics_interface.cc COPYONLY )
  configure_file( Magics.i.in Magics/Magics.i )

  # Copy python modules to build area
  # configure_file ensures the copy is triggered if the file changes
  configure_file( Magics/__init__.py Magics/__init__.py COPYONLY )
  configure_file( Magics/macro.py Magics/macro.py COPYONLY )
  configure_file( Magics/metgram.py Magics/metgram.py COPYONLY )

  set( _magics_swig "_Magics${CMAKE_SHARED_LIBRARY_SUFFIX}" )
  # Build the extension module for use in build tree with RPATH pointing to the build tree
  add_custom_command( OUTPUT ${_magics_swig}
    COMMAND ${PYTHON_EXECUTABLE} setup.py build_ext --inplace --rpath ${CMAKE_BINARY_DIR}/lib
    DEPENDS Magics_interface.cc Magics.i.in numpy.i setup.py.in MagPlus )
  add_custom_target(build_swig_wrapper ALL DEPENDS ${_magics_swig})

  # Build the extension module for use in install tree with RPATH pointing to install tree
  install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} setup.py build_ext --rpath ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")
  # Call distutils for installation
  install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} setup.py install --prefix \$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

endif()
