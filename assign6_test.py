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


#command = "ls test_suite/"
#pipe = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
#files = pipe.stdout.read()
#files = [file for file in files.split('\n') if(file)]
#print "running "
#print files


# Filenames (hardcoded)
files = { "assign.not" : (40, 21, 23), 
           "assign2.not" : (24, 13, 13), 
            "coords.not" : (144, 73, 79), 
            "cornerCase1.not" : (56, 29, 25), 
            "objRetClosure.not" : (156, 79, 99), 
            "object.not" : (116, 59, 81), 
            "object_nested.not" : (80, 41, 41), 
            "test-1.not" : (40, 21, 19), 
             "test-2.not" : (32, 17, 17), 
            "test-3.not" : (28, 17, 13), 
            "test-4.not" : (380, 191, 187) 
            }
#gcs = ["mark", "semi"] # add generational.
gcs = ["semi", "mark", "gen"] # order matters ^^^

for name, size in files.iteritems():
  for i, gc in enumerate(gcs):
            if(i == 2):
                continue # skip gen

            command = "cd build; scala miniJS -gc " + gc + " -size " + str(size[i]) + " ../test_suite/" + name
            print command
            pipe = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
            output = pipe.stdout.read()
            parsed_output = "\n".join([line for line in output.split("\n") if "exception" in line or "assertion" in line or "OOM" in line or "cs162.miniJS.domains.Domains$undefined$" in line])
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
