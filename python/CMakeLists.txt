if( HAVE_PYTHON )

    if( PYTHONLIBS_FOUND )

        # preparing for generating setup.py -- this may not be needed as cmake can do the swig + shared libs without libtool

#        if( HAVE_LIBJASPER )
#            get_filename_component(JASPER_DIR ${JASPER_INCLUDE_DIR} PATH )
#        endif()

        configure_file( setup.py.in setup.py )
        configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/../src/common/magics_api.h magics_api.h COPYONLY )
        configure_file( numpy.i numpy.i COPYONLY )
        configure_file( Magics_interface.cc Magics_interface.cc COPYONLY )
        configure_file( Magics.i.in Magics.i )

        ####### Do not invoke swig. Use our own generated C wrapper file ######
        set( _magics_swig "_Magics${CMAKE_SHARED_LIBRARY_SUFFIX}" )
        # Build the extension module for use in build tree with RPATH pointing to the build tree
        add_custom_command( OUTPUT ${_magics_swig}
                            COMMAND ${PYTHON_EXECUTABLE} setup.py build_ext --inplace -R ${CMAKE_BINARY_DIR}/lib
                            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Magics_interface.cc Magics.i.in ${CMAKE_CURRENT_SOURCE_DIR}/numpy.i setup.py.in )
        add_custom_target(build_swig_wrapper ALL DEPENDS ${_magics_swig})

        # Copy python modules to build area
        file( COPY Magics DESTINATION . )

        # Call distutils for installation
        install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} setup.py install --prefix $ENV{DESTDIR}${CMAKE_INSTALL_PREFIX} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

    endif()

endif()
