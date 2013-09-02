#!/bin/ksh
#set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed


# Defines the variables that are needed for any communication with ECF
export ECF_PORT=%ECF_PORT%    # The server port number
export ECF_NODE=%ECF_NODE%    # The name of ecf host that issued this task
export ECF_NAME=%ECF_NAME%    # The name of this current task
export ECF_PASS=%ECF_PASS%    # A unique password
export ECF_TRYNO=%ECF_TRYNO%  # Current try number of the task
export ECF_RID=$$

export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/usr/local/apps/ecflow/current/bin
#begin of modification CGJD 2012.03.30
#new - environment variable to control printing of default text in MAGICS (only for FORTRAN tests)
export MAGICS_TEST_AUTOTEXT=off
#end of modification CGJD 2012.03.30

#begin of modification CGJD 2012.05.04
#new - environment variable to control printing of default text in MAGICS (only for PYTHON tests)
export MAGICS_REGRESSION=None
#end of modification CGJD 2012.05.04

# Tell ecFlow we have started
ecflow_client --init=$$


# Defined a error hanlder
ERROR() {
  set +e                      # Clear -e flag, so we don't fail
  ecflow_client --abort=trap  # Notify ecFlow that something went wrong, using 'trap' as the reason
  trap 0                      # Remove the trap
  exit 0                      # End the script
}


# Trap any calls to exit and errors caught by the -e flag
trap ERROR 0


# Trap any signal that may cause the script to fail
trap '{ echo "Killed by a signal"; ERROR ; }' 1 2 3 4 5 6 7 8 10 12 13 15
