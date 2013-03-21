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


files = [ "fail%s" % str(i) for i in range(1, 31) ]
files += [ "pass0%s" % str(i) for i in range(1, 6) ]
#print files

for i in files:
	for j in range(0,1):


            command = "cd build; scala miniJS -gc mark -size 100 " + "../test_suite/" + i + ".not"
            pipe = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
            output = pipe.stdout.read()
            output = "\n".join([line for line in output.split("\n")
                                if line != "DEBUG: Retrying commit."])
            total_count += 1

            print "Test %s" % repr(i)
            print "Got: "
            if(output.find("NOT secure!") != -1 and i.find("fail") != -1 or output.find("secure!") != -1 and i.find("pass") != -1):
                print "Success!"
                pass_count += 1
            else:
                print "Failure!"
                fail_count += 1

            print output

print "***** " + str(pass_count) + " tests PASSED *****"
print "***** " + str(fail_count) + " tests FAILED *****"

if (pass_count==total_count):
    print "YAYYYYYYYYY!!!!!! EVERYTHING PASSED!  =^.^="
