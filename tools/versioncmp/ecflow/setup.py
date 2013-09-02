#!/usr/bin/env python
'''
    Submits Magics testing to ecflow

    The following arguments may be specified:

    -c, --config-file=FILEPATH   Configuration file to use other than default.
    -f, --force                  Force overwrite of exising suite on server
    -n, --now                    Run now (ignore cron scheduling)
    -h, --help                   Shows this help.

'''

import os, errno
import ecflow
import shutil
import sys
import time
import getopt

def server_exists(client):
    try:
        cl.ping()
        return True
    except RuntimeError, e:
        print "Ping of server %s:%s failed: %s" % (server_hostname,
                                               str(server_port),
                                               str(e))
    return False

def mkdir_p(path):
    # mkdir -p functionality
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise


def mangle_name_for_ecf(name):
    return name.replace("+", "_plus")

def parse_config_file(filename):
    config_dict={}

    lines = open(filename, "r")
    for line in lines:
        line=line.strip()

        if len(line) == 0 or line[0]=="#":
            # line is comment or blank            
            continue

        tokens=line.split("=")
        if len(tokens) == 2:
            config_dict[tokens[0]]=tokens[1].strip()
        else:
            print "Unable to parse line %s" % line

    return config_dict

# Get the path to the executed file
run_path=sys.path[0]
# We use this for the working folder unless otherwise specified
working_folder=run_path

config_file="%s/../config/default.settings" % run_path

# Whether to ignore warnings and plough on
force_submission=False
run_now=False


try:
    opts, args = getopt.getopt(sys.argv[1:], "hfnc:", ["help", "force", "now", "config-file="])
except getopt.error, msg:
    print msg
    print "for help use --help"
    sys.exit(2)

for o, a in opts:
    if o in ("-h", "--help"):
        print __doc__
        sys.exit(0)
    elif o in ("-c", "--config-file"):
        config_file=a
    elif o in ("-f", "--force"):
        force_submission=True
    elif o in ("-n", "--now"):
        run_now=True
   

config_dict=parse_config_file(config_file)

# work out the suite name from the config file
# so if the config file is blahblah.settings then we 
# run with a suite name of "blahblah"
config_file_base=os.path.basename(config_file)
suite_name, _ =os.path.splitext(config_file_base) 

input_folder=""
output_folder=""


try:
    server_hostname=config_dict["ECFLOW_SERVER_HOSTNAME"]
    server_port=config_dict["ECFLOW_SERVER_PORT"]
    runtime=""
    if "ECFLOW_RUNTIME" in config_dict.keys() and not run_now:
        runtime=config_dict["ECFLOW_RUNTIME"]

    if "ECF_HOME" in config_dict.keys():
        working_folder=config_dict["ECF_HOME"]    
        mkdir_p(working_folder)
        for header_file in ["head.h", "tail.h"]:
            shutil.copyfile("./%s" % header_file, "%s/%s" % (working_folder, header_file))
 
    verbose_flag=""
    difference_threshold_factor=config_dict["DIFFERENCE_THRESHOLD_FACTOR"]
    if config_dict["VERBOSE"]==1:
        verbose_flag="-v"
    input_folder=config_dict["TEST_FOLDER_PATH"]
    output_folder=config_dict["OUTPUT_FOLDER_PATH"]
#begin of modification 06.08.2012 cgjd
    confluence_folder=config_dict["CONFLUENCE_FOLDER_PATH"]
    magpluslib=config_dict["MAGPLUSLIB"]
#end of modification 06.08.2012 cgjd
    comparisons=eval(config_dict["VERSIONS"])

except KeyError as error:
    print "Missing configuration key %s" % str(error)
    sys.exit(1)


# List the input folders in the input folder 
# As these will be the test names
tests = [ f for f in os.listdir(input_folder) if os.path.isdir("%s/%s" % (input_folder, f) ) and os.path.isfile("%s/%s/dorun.sh" % (input_folder, f))  ]

# TODO remove the folder and write the tests into an
# appropriate structure

suite_folder="%s/%s" % (working_folder, suite_name)

try:
    if (os.path.isdir(suite_folder)):
        shutil.rmtree(suite_folder)
except RuntimeError, e:
    print "Couldn't remove suite definition folder %s. Exiting." % suite_folder
    sys.exit(1)

os.mkdir(suite_folder)
for root_version_name in comparisons:
    
    # need to alter any "+" in the name with an appropriate character
    ecf_version_name=mangle_name_for_ecf(root_version_name) 

    target_folder="%s/%s" % (suite_folder, ecf_version_name)
    os.mkdir(target_folder)
    shutil.copyfile("./run.ecf", "%s/run.ecf" % target_folder)

shutil.copyfile("%s/summary.ecf" % run_path, "%s/summary.ecf" % suite_folder)
shutil.copyfile("%s/archive.ecf" % run_path, "%s/archive.ecf" % suite_folder)
shutil.copyfile("%s/init.ecf" % run_path, "%s/init.ecf" % suite_folder) 

defs=ecflow.Defs()
suite=defs.add_suite(suite_name)

# make the suite a real clock rather than hybrid
# so that date information follows the system clock
# rather than being fixed at the original submission date
hybrid=False
clock = ecflow.Clock(hybrid)
suite.add_clock(clock)

if runtime != "":
    cron = ecflow.Cron()
    h, m = runtime.split(":")
    time_series = ecflow.TimeSeries( ecflow.TimeSlot(int(h),int(m)) )
    cron.set_time_series( time_series )
    suite.add_cron(cron)
    #suite.add_time(runtime)
    #suite.add_date(0,0,0)

suite.add_variable("ECF_INCLUDE", working_folder)
suite.add_variable("ECF_HOME", working_folder)
suite.add_variable("TOOLS_HOME", "%s/.." % run_path)
suite.add_variable("OUTPUT_FOLDER", output_folder)

suite.add_variable("INPUT_FOLDER", input_folder)
suite.add_variable("VERBOSE_FLAG", verbose_flag)
suite.add_variable("DIFFERENCE_THRESHOLD_FACTOR", difference_threshold_factor)

#begin of modification 06.08.2012 cgjd
suite.add_variable("CONFLUENCE_FOLDER",confluence_folder)
suite.add_variable("MAGPLUSLIB",magpluslib)
#end of modification 06.08.2012 cgjd

#suite.add_limit("subtasks", 10)
#suite.add_inlimit("subtasks")

# add initialisation task here
suite.add_task("init")


for root_version_name in comparisons:

  ecf_version_name=mangle_name_for_ecf(root_version_name) 
  root_family=suite.add_family(ecf_version_name)
  
  # only start once initialised
  root_family.add_trigger("init == complete")
  root_family.add_variable("VERSION1", root_version_name)

  root_family.add_repeat( ecflow.RepeatString("VERSION2",
                                              comparisons[root_version_name] ) )

  task=root_family.add_task("run")
  task.add_repeat( ecflow.RepeatString("TASKNAME", tests ) )
  
  # here we make a copy of the input folder into each family and
  # override the appropriate variable
  local_input_folder="%s/%s/input" % (suite_folder, ecf_version_name)
  shutil.copytree(input_folder, local_input_folder)
  root_family.add_variable("INPUT_FOLDER", local_input_folder)


summary_task=suite.add_task("summary")
trigger_str = ""

for count, root_version_name in enumerate(comparisons):
    if count > 0:
        trigger_str +=  " and "
    trigger_str += "%s == complete" % mangle_name_for_ecf(root_version_name)

summary_task.add_trigger(trigger_str)

# add archive task
archive_task=suite.add_task("archive")
archive_trigger="summary == complete"
archive_task.add_trigger(archive_trigger)

defs.save_as_defs("%s/check.def" % run_path)
print "Written definition file to %s/check.def" % run_path


job_ctrl = ecflow.JobCreationCtrl()                    
defs.check_job_creation(job_ctrl)

# now use the client to get things running
# NB This needs sorting out
cl = ecflow.Client()
cl.set_host_port(server_hostname, str(server_port))

if server_exists(cl) == False:
    print "Server not running on %s:%s. Exiting." % (server_hostname,
                                                     str(server_port))
    sys.exit(1)

try:

    ######################################cl.delete_all()    
    # get a copy of the server defs on the client
    cl.sync_local()
    
    current_defs=cl.get_defs()
    if current_defs != None:
        current_suite=cl.get_defs().find_suite(suite_name)

        if force_submission == False and current_suite != None:
            print "Suite name %s already exists. Re-run using --force (-f) option.  " % suite_name
            sys.exit(1)


    cl.load(defs, force_submission)
    print "Loaded suite definition."
except RuntimeError, e:
    cl.load(defs)
    print "Load of suite definition failed: %s" % str(e)
    sys.exit(1)


try:
    cl.begin_suite(suite_name)
    print "Suite started successfully."
except RuntimeError, e:
    print "Suite did not start: %s" % str(e)
    sys.exit(1)

