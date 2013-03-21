import os
import subprocess
import sys

class bcolors:
    PASS = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

fail_count = 0
pass_count = 0
total_count = 0


command = "ls test_suite/"
pipe = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
files = pipe.stdout.read()
files = [file for file in files.split('\n') if(file)]
print "running "
print files

#gcs = ["mark", "semi"] # add generational.
gcs = ["mark"]

for i in files:
  for gc in gcs:
            command = "cd build; scala miniJS -gc " + gc + " -size 400 " + "../test_suite/" + i
            pipe = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
            output = pipe.stdout.read()
            parsed_output = "\n".join([line for line in output.split("\n") if "exception" in line or "assertion" in line or "OOM" in line])
            total_count += 1

            print "Test %s" % repr(i)
            print "Got: "
            if(len(parsed_output) > 1):
              fail_count += 1
            else:
              pass_count += 1
            print output

print "***** " + str(pass_count) + " tests PASSED *****"
print "***** " + str(fail_count) + " tests FAILED *****"

if (pass_count==total_count):
    print "YAYYYYYYYYY!!!!!! EVERYTHING PASSED!  =^.^="
