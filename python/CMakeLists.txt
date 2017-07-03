if( HAVE_PYTHON )

  configure_file( setup.py.in setup.py )
  configure_file( Magics.py.in Magics/Magics.py )

  # Copy python modules to build area
  # configure_file ensures the copy is triggered if the file changes
  configure_file( Magics/__init__.py Magics/__init__.py COPYONLY )
  configure_file( Magics/macro.py Magics/macro.py COPYONLY )
  configure_file( Magics/toolbox.py Magics/toolbox.py COPYONLY )
  configure_file( Magics/metgram.py Magics/metgram.py COPYONLY )

#  ${CMAKE_SHARED_LIBRARY_SUFFIX}   ${CMAKE_BINARY_DIR}/lib

  # Build the extension module for use in install tree with RPATH pointing to install tree
  install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} setup.py build_ext --rpath ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

  # Library installation directory override
  if( NOT INSTALL_LIB_DIR STREQUAL lib )
      execute_process( COMMAND ${PYTHON_EXECUTABLE} -c "from __future__ import print_function; import sys; print(sys.version[:3], end='')"
               OUTPUT_VARIABLE PYVER )
      set( __install_lib "--install-lib=${MAGICS_FULL_INSTALL_LIB_DIR}/python${PYVER}/site-packages" )
  endif()

  # Call distutils for installation
  install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} setup.py install ${__install_lib} --root \$ENV{DESTDIR}/ --prefix ${CMAKE_INSTALL_PREFIX} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

endif()
