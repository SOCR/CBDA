#!/usr/bin/env python3

# CBDA validation and training set creation.

# This script lists the contents of the Pickle file containing the validation
# ordinals, etc.

import sys
import argparse
import os
import pickle

def defineArgs(args=None):

    parser = argparse.ArgumentParser()

    msg = 'The file name of the Pickle file with the validation line ordinal'
    msg += ' set'
    parser.add_argument('--vof', '--validation-ordinal-file',
                        dest='validationOrdinalFileName', help=msg, type=str, \
                        default=None, required=True)


    args = parser.parse_args()
    
    # Check the arguments for validity.
    
    argsOk = True

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

    print('args.validationOrdinalFileName: {0}'.format(args.validationOrdinalFileName))

    print

args = defineArgs()
#printArgs(args)

with open(args.validationOrdinalFileName, 'rb') as validationOrdinalFile:
    (originalLineCount, originalColumnCount, validationLineOrdinals) = \
    pickle.load(validationOrdinalFile)

print('originalLineCount: {0}'.format(originalLineCount))
print('originalColumnCount: {0}'.format(originalColumnCount))

# Display the validation ordinals in ascending order.
print('\nValidation ordinals:')
for vlo in sorted(validationLineOrdinals):
    print(str(vlo))
