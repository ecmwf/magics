
execute_process(
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMAND  git rev-parse --abbrev-ref HEAD
        OUTPUT_VARIABLE XX)

STRING(REGEX REPLACE "\n" ""  XX ${XX})
set( ${PROJECT_NAME}_VERSION_STR  "2.21.0-${XX}" )
set( BRANCH_NAME  "2.21.0-${XX}" )

