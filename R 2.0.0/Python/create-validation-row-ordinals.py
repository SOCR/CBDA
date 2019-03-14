#!/usr/bin/env python3

# CBDA validation and training set creation.

# This script defines the set of ordinal line numbers to use for the validation
# set of a CBDA project.
#
# Inputs:
#     File name of original data file.
#         The following assumptions are made about the contents of the file:
#             It is a text file with standard Unix style end of line markers (\n).
#
#             It has a header line with column names.
#
#     Number of lines to be in the validation set.
#     This must be less than the number of non-header lines in the original
#     file.
#
# Outputs:
#
#    A Python Pickle file with a Python set object containing the lines for the
#    validation set.
#
#    The validation set lines themselves are not written.
#
#    The file of validation line ordinals is needed for creating training set
#    files, to avoid including validation lines in a training set. It is also
#    used to create the validation set itself.



import sys
import argparse
import os
import random
import subprocess
import pickle

def formatCmdResult(returnCode, out, err):

	"""
	Format the result from a call to subprocess functions, such as Popen,
	typically for use in error messages.
	"""
	
	msg = '\nReturn code:{0}\nout:\n{1}\nerr:\n{2}'
	msg = msg.format(returnCode, out, err)
	return msg


def defineArgs(args=None):

    parser = argparse.ArgumentParser()

    msg = 'The file name of the original data set'
    parser.add_argument('-i', '--original-file', dest='originalFileName', \
                        help=msg, type=str, default=None, required=True)

    msg = 'The number of lines in the validation set'
    parser.add_argument('--vss', '--validation-set-size',
                        dest='validationSetSize', help=msg, type=int,
                        required=True)

    msg = 'The name for the output file.'
    parser.add_argument('--o', '--output-file', dest='outputFileName',
                        help=msg, type=str, default=None, required=True)

    args = parser.parse_args()
    
    # Check the arguments for validity.
    
    argsOk = True

    if not os.path.isfile(args.originalFileName):
        msg = '\nOriginal data set file "{0}" does not exist.\n'
        msg = msg.format(args.originalFileName)
        print(msg)
        argsOk = False

    if args.validationSetSize < 1:
        msg = 'The validationSetSize, {0}, is less than 1.'
        msg = msg.format(args.validationSetSize)
        print(msg)
        argsOk = False

    if not argsOk:
        sys.exit(1)

    return args

def printArgs(args):
    
    """
    For testing and debugging.
    """

    print('args.originalFileName: {0}'.format(args.originalFileName))
    print('args.validationSetSize: {0}'.format(args.validationSetSize))
    print('args.outputFileName: {0}'.format(args.outputFileName))

    print

def getOriginalFileLineCount(originalFileName):
    cmd = 'wc -l {0}'.format(originalFileName)
    cmd = cmd + "| awk '{print $1}'"

    try:
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, \
                                   stderr=subprocess.STDOUT, shell=True)
        (out, err) = process.communicate()
        returnCode = process.returncode
    
        if returnCode != 0:
            msg = 'Failure getting original file line count'
            msg += '\nCommand: {0}\nResult:{1}'
            result = formatCmdResult(returnCode, out, err)
            msg = msg.format(cmd, result)
            print(msg)
            sys.exit(0)
        
    except (OSError, ValueError) as e:
        msg = 'Failure getting original file line count'
        msg += '\nCommand: {0}\nException:\n{1}'
        msg = msg.format(cmd, traceback.format_exc())
        print(cmd)
        sys.exit(0)

    try:
        originalLineCount = int(out)
    except (ValueError) as e:
            msg = 'Failure getting original file line count'
            msg += 'Count is not an integer'
            msg += '\nCommand: {0}\nResult:{1}'
            result = formatCmdResult(returnCode, out, err)
            msg = msg.format(cmd, result)
            print(msg)
            sys.exit(0)

    return originalLineCount

def getOriginalFileColumnCount(originalFileName):
    cmd = "head -n1 {0} | sed 's#,#\\n#g' | wc -l | awk '{{print $1}}'"
    cmd = cmd.format(originalFileName)

    try:
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, \
                                   stderr=subprocess.STDOUT, shell=True)
        (out, err) = process.communicate()
        returnCode = process.returncode
    
        if returnCode != 0:
            msg = 'Failure getting original file column count'
            msg += '\nCommand: {0}\nResult:{1}'
            result = formatCmdResult(returnCode, out, err)
            msg = msg.format(cmd, result)
            print(msg)
            sys.exit(0)
        
    except (OSError, ValueError) as e:
        msg = 'Failure getting original file column count'
        msg += '\nCommand: {0}\nException:\n{1}'
        msg = msg.format(cmd, traceback.format_exc())
        print(cmd)
        sys.exit(0)

    try:
        originalColumnCount = int(out)
    except (ValueError) as e:
            msg = 'Failure getting original file column count'
            msg += 'Count is not an integer'
            msg += '\nCommand: {0}\nResult:{1}'
            result = formatCmdResult(returnCode, out, err)
            msg = msg.format(cmd, result)
            print(msg)
            sys.exit(0)

    return originalColumnCount


args = defineArgs()
#printArgs(args)

# Ensure the output file has a specific suffix.
suffix = '.pickle'
if not args.outputFileName.endswith(suffix):
    args.outputFileName += suffix

originalLineCount = getOriginalFileLineCount(args.originalFileName)
originalColumnCount = getOriginalFileColumnCount(args.originalFileName)

nonHeaderCount = originalLineCount - 1
if args.validationSetSize >= nonHeaderCount:
    msg = 'The validation set size, {0}, is greater than or equal to'
    msg += ' the number of original file non-header lines, {1}.'
    print(msg.format(args.validationSetSize, nonHeaderCount))
    sys.exit(1)

# Generate the set of validation line ordinals.
# 2 to avoid the first line of the file, the header line.
# Make it a set for more efficient use by other scripts.
validationLineOrdinals = random.sample(range(2, originalLineCount+1),
                                       args.validationSetSize)
vloSet = set(validationLineOrdinals)

# Create a single data structure for writing to the Pickle file.
saveInfo = (originalLineCount, originalColumnCount, vloSet)

# Write the Pickle file.
with open(args.outputFileName, 'wb') as outputFile:
    pickle.dump(saveInfo, outputFile)

