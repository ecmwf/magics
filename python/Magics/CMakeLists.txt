if( ENABLE_PYTHON AND NUMPY_FOUND )

  ### python destination

  set( PYTHON_SITE "lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages" )
  set( PYTHON_DEST "${PYTHON_SITE}/Magics" )

  file( RELATIVE_PATH relative_rpath "${CMAKE_INSTALL_PREFIX}/${PYTHON_DEST}" "${${PNAME}_FULL_INSTALL_LIB_DIR}" )

  debug_var( relative_rpath )

  # See comment below, where the following lines are overriden
  ecbuild_append_to_rpath( ${relative_rpath} )
  ecbuild_append_to_rpath( "../../lib" ) # for the ctest to find magics libs

  ### place in build tree so we can run python tests

  configure_file( macro.py    macro.py    COPYONLY )
  configure_file( metgram.py  metgram.py  COPYONLY )

  ### build python bindings

  configure_file( Magics.i.in  Magics.i )

  include_directories( ${PYTHON_INCLUDE_DIRS} ${NUMPY_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR} )

  set_source_files_properties( ${CMAKE_CURRENT_BINARY_DIR}/Magics.i PROPERTIES GENERATED TRUE CXX ON )

  swig_add_module( Magics python Magics_interface.cc ${CMAKE_CURRENT_BINARY_DIR}/Magics.i )

  set_source_files_properties(partio.i PROPERTIES SWIG_FLAGS "-includeall")

  swig_link_libraries( Magics MagPlusShared ${PYTHON_LIBRARIES} )

  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR}/Magics.py ${CMAKE_CURRENT_BINARY_DIR}/__init__.py )

  # On the MAC, only one RPATH cat be set (Mac does not understand path:path:path)
  # Someone, please fix
  # set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
  set_target_properties(_Magics PROPERTIES INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

  ### make it available in the build dir

  file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/${PYTHON_SITE} )

  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_BINARY_DIR}/${PYTHON_DEST} )

  ### install to final destination

  install( TARGETS _Magics DESTINATION ${PYTHON_DEST} )
  install( FILES
               macro.py
               metgram.py
           DESTINATION ${PYTHON_DEST} )

  install( FILES  ${CMAKE_CURRENT_BINARY_DIR}/Magics.py
           RENAME __init__.py
           DESTINATION ${PYTHON_DEST} )

endif()

