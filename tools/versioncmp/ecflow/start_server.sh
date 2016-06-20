#!/bin/ksh
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# Get the command line arguments

arg_count=$#
count=0
set -A argvec
for arg; do
  argvec[$count]=$arg
  shift
  (( count+=1 ))
done

force_restart=0
verbose=0
config_file=""

# Utility function
LOG () {
  if [[ $verbose -ne 0 ]]; then
    echo $1
  fi
}

RUN() {
  if [[ $verbose -ne 0 ]]; then
    $1 
  else
    $1 > /dev/null 2> /dev/null
  fi
}

USAGE() {

  echo "USAGE: start_server.sh [ options ]"
  echo "   -c,  --config-file        Required. Config file to use."
  echo "   -f,  --force-restart      Close down server first"
  echo "   -v,  --verbose            Verbose operation"
  echo "   -h,  --help               Prints this help"
}

# look for optional arguments
if [[ arg_count -gt 0 ]]; then
  count=0
  while [[ $count -lt $arg_count ]]; do
    case ${argvec[$count]} in
      "-help"|"--help"|"-h")
        USAGE;
        exit 0;;
      "-verbose"|"--verbose"|"-v")
        verbose=1;;
      "-force-restart"|"--force-restart"|"-f")
        force_restart=1;;
      "-config-file"|"--config-file"|"-c")
        config_file=${argvec[(($count+1))]};
        count=$count+1;;
      *)
      echo "Unknown option '${argvec[$count]}'";
      USAGE
      exit 1;;
    esac
    count=$count+1
  done
else
  USAGE
  exit 1
fi

LOG "config_file is $config_file"

cat $config_file | while read CMD; do
   key=`echo $CMD | awk -F= '{print $1}'`
   val=`echo $CMD | awk -F= '{print $2}'`
   if [[ $key = "ECFLOW_SERVER_HOSTNAME" ]]; then
      LOG "hostname $val"
      host=$val
   fi
   if [[ $key = "ECFLOW_SERVER_PORT" ]]; then
      LOG "port $val"
      port=$val
   fi
done

if [[ -z "$host" ]]; then
  echo "ERROR: Config file error: ECFLOW_SERVER_HOSTNAME is not set"
fi
if [[ -z "$port" ]]; then
  echo "ERROR: Config file error: ECFLOW_SERVER_PORT is not set"
fi

ssh_cmd=""

if [[ $HOST != $host && $host != "localhost" && $host != "lhost" ]]; then
  LOG "We are connecting to remote host $host"
  ssh_cmd="ssh $host use ecflow; "
else
  LOG "We are running on the local machine $host"
  RUN "use ecflow"
fi

if [[ $force_restart -eq 1 ]]; then
   LOG "Stopping ecflow ..."
   RUN "$ssh_cmd ecflow_stop.sh"
fi

$ssh_cmd ecflow_start.sh -p $port
LOG "Done."

