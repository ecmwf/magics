if( ENABLE_PYTHON AND NUMPY_FOUND )

  ### python destination

  set( PYTHON_SITE "lib/python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages" )
  set( PYTHON_DEST "${PYTHON_SITE}/Magics" )

  file( RELATIVE_PATH relative_rpath "${CMAKE_INSTALL_PREFIX}/${PYTHON_DEST}" "${${PNAME}_FULL_INSTALL_LIB_DIR}" )

  debug_var( relative_rpath )

  ecbuild_append_to_rpath( ${relative_rpath} )

  ### place in build tree so we can run python tests

  configure_file( macro.py    macro.py    COPYONLY )
  configure_file( metgram.py  metgram.py  COPYONLY )

  ### build python bindings

  configure_file( Magics.i.in  Magics.i )

  include_directories( ${PYTHON_INCLUDE_DIRS} ${NUMPY_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR} )

  set_source_files_properties( Magics.i PROPERTIES GENERATED TRUE CXX ON )

  swig_add_module( Magics python Magics_interface.cc Magics.i )

  set_source_files_properties(partio.i PROPERTIES SWIG_FLAGS "-includeall")

  swig_link_libraries( Magics MagPlusShared ${PYTHON_LIBRARIES} )

  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR}/Magics.py ${CMAKE_CURRENT_BINARY_DIR}/__init__.py )
  
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

