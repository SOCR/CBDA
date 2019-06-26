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
#
#     The name of a Python Pickle file containing a Python set of the
#     validation line ordinals.
#
#     THe name of the output file to contain the validation set.
#
# Outputs:
#    The output file contains the lines from the original data file specified
#    by the validation line ordinals read form the Pickl file.

import sys
import argparse
import os
import pickle

def defineArgs(args=None):

    parser = argparse.ArgumentParser()

    msg = 'The file name of the original data set'
    parser.add_argument('-i', '--original-file', dest='originalFileName', \
                        help=msg, type=str, default=None, required=True)

    msg = 'The file name of the Pickle file with the validation line ordinal'
    msg += ' set'
    parser.add_argument('--vof', '--validation-ordinal-file', \
                        dest='validationOrdinalFileName', help=msg, type=str, \
                        default=None, required=True)

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

    if not os.path.isfile(args.validationOrdinalFileName):
        msg = '\nValidation ordinal file "{0}" does not exist.\n'
        msg = msg.format(args.validationOrdinalFileName)
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
    print('args.validationOrdinalFileName: {0}'.format(args.validationOrdinalFileName))
    print('args.outputFileName: {0}'.format(args.outputFileName))

    print

args = defineArgs()
#printArgs(args)

# Ensure the output file has a specific suffix.
suffix = '.csv'
if not args.outputFileName.endswith(suffix):
    args.outputFileName += suffix

# Load saved information from creating the validation ordinals.
with open(args.validationOrdinalFileName, 'rb') as validationOrdinalFile:
    (originalLineCount, originalColumnCount, validationLineOrdinals) = \
    pickle.load(validationOrdinalFile)

ordinalCount = len(validationLineOrdinals)

# Write the original file lines corresponding to the validation line ordinals
# to the output file.
outputCount = 0
with open(args.outputFileName, 'w') as outputFile:
    with open(args.originalFileName, 'r') as inputFile:
        for (ordinal, line) in enumerate(inputFile, 1):
            if ordinal in validationLineOrdinals:
                outputFile.write(line)
                outputCount = outputCount + 1
                if outputCount == ordinalCount:
                    break

if outputCount != ordinalCount:
    msg = 'Not all validation ordinals were written to the outputfile'
    msg += '{0} of {1} were written'
    print(msg)
