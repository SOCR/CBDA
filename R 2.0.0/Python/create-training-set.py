#!/usr/bin/env python3

# CBDA validation and training set creation.

# This script defines multiple trainig sets for a CBDA project.
#
# Inputs:
#     File name of original data file.
#         The following assumptions are made about the contents of the file:
#             It is a text file with standard Unix style end of line markers (\n).
#
#             It has a header line with column names.
#
#             It is a csv file.
#
#             All lines have the same number of columns, including the header
#             line.
#
#     The name of a Python Pickle file containing a Python set of the
#     validation line ordinals.
#
#     The number of rows to extract.
#         The specific rows to extract are chosen at random from the original
#         file. The first row is not included, nor are any rows in the
#         validation set.
#
#     The number of columns to extract.
#         The specific columns to extract are chosen at random.
#
#     The case number column ordinal.
#         To exclude from the selection of data columns for a data set, but to
#         be written to each training set in addition to the selected data
#         columns.  This is the column whose value corresponds to each patient.
#
#     The output column ordinal.
#         To exclude from the selection of data columns for a data set, but to
#         be written to each training set in addition to the selected data
#         columns.  This is the outcome column whose value is to be predicted
#         by the algorithm defined by the machine learning processing of the
#         training sets generated here.
#
#     The number of training sets to create.
#
# Outputs:
#    An output file for each training file.

import sys
import argparse
import os
import pickle
import random

class TrainingSet:

    """
    A class to represent the information for a training set to be created.

    Each training set has an output file name, output file object and a set of
    row ordinals for the lines of the original file for this training set and
    a set of column ordinals for the columns of the original file for this
    training set.
    """

    def __init__(self, trainingOrdinal, originalLineCount, \
                 originalColumnCount, validationLineOrdinals, args):

        # Used for output file names and error mesages.
        self.ordinal = trainingOrdinal

        # 2, to skip the ordinal for the original file header line.
        self.rowOrdinals = self.getRandomOrdinals(args.rowCount, 2, originalLineCount, validationLineOrdinals)

		# For efficiently checking if a column from an original column line is
		# to be written for this training set.
        self.columnOrdinals = self.getRandomOrdinals(args.columnCount, 1, \
                                   originalColumnCount, \
                                   set([args.caseColumn, args.outcomeColumn]))

        # For writing the selected columns in the same order as they are in the
        # original file. Also, to include the case number and output columns in
        # the output.
        self.outputColumns = list(self.columnOrdinals)
        self.outputColumns.append(args.caseColumn)
        self.outputColumns.append(args.outcomeColumn)
        self.outputColumns.sort()

        self.fileName = 'training-set-{0}'.format(trainingOrdinal)

        f = 'training-set-{0}-row-ordinals'.format(trainingOrdinal)
        self.rowOrdinalFileName = f

        f = 'training-set-{0}-column-ordinals'.format(trainingOrdinal)
        self.columnOrdinalFileName = f
        
        self.writeOrdinals(self.rowOrdinals, self.rowOrdinalFileName)
        self.writeOrdinals(self.columnOrdinals, self.columnOrdinalFileName)

        self.trFile = open(self.fileName, 'w')


    def getRandomOrdinals(self, count, start, end, exclude):

        """
        Get a set of count random integers between start and end, inclusive,
        but not including any integers in the exclude.

        count, start and end should be integers.
      
        start should be < end

        count should be < (end - start + 1)

        exclude should be a set of integers. It may be empty.

        """
        ordinals = set()
        while len(ordinals) < count: 
            r = random.randint(start, end)
            if not r in exclude:
                ordinals.add(r)

        return ordinals

    def writeOrdinals(self, ordinals, fileName):

        """
        Write a set of ordinals to a file, in ascending numerical order.
        """

        sortedOrdinals = sorted(ordinals)
        with open(fileName, 'w') as ordinalFile:
            for o in sortedOrdinals:
                ordinalFile.write(str(o) + '\n')

    def checkLine(self, ordinal, fields):

        """
        Check a line from the original file, to see if fields from it should be
        written for this training set.

        outputColumns is sorted in ascending order by column ordinal, so the
        columns will be written in the same order they are in the original
        file. It also includes the case number column and output column, in
        addition to the selected data columns.
        """

        if ordinal in self.rowOrdinals:
            # This line is for this training set.

            # Get the fields for this training set.
            fieldsToWrite = []
            for o in self.outputColumns:

                # Because we count column ordinals from 1, but list indices
                # start at 0.
                o1 = o - 1

                fieldsToWrite.append(fields[o1]) 

            # Write the fields to the training set file.
            fieldStr = ','.join(fieldsToWrite) + '\n'
            self.trFile.write(fieldStr)

def defineArgs(args=None):

    parser = argparse.ArgumentParser()

    msg = 'The file name of the original data set'
    parser.add_argument('-i', '--original-file', dest='originalFileName', \
                        help=msg, type=str, default=None, required=True)

    msg = 'The file name of the Pickle file with the validation line ordinal'
    msg += ' set'
    parser.add_argument('--vof', '--validation-ordinal-file',
                        dest='validationOrdinalFileName', help=msg, type=str, \
                        default=None, required=True)

    msg = 'The number of rows to extract for each training set.'
    parser.add_argument('--rc', '--row-count', dest='rowCount', help=msg, \
                        type=int, required=True)

    msg = 'The number of columns to extract for each training set.'
    parser.add_argument('--cc', '--column-count', dest='columnCount', \
                        help=msg, type=int, required=True)

    msg = 'The case number column ordinal'
    parser.add_argument('--cn', '--case-column', dest='caseColumn', \
                        help=msg, type=int, required=True)

    msg = 'The outcome column ordinal'
    parser.add_argument('--oc', '--outcome-column', dest='outcomeColumn', \
                        help=msg, type=int, required=True)

    msg = 'The number of training sets to create'
    parser.add_argument('--tsc', '--training-set-count', \
                        dest='trainingSetCount', help=msg, type=int, \
                        required=True)

    args = parser.parse_args()
    
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

    if args.rowCount < 1:
        msg = 'The row count, {0}, is less than 1.'
        msg = msg.format(args.rowCount)
        print(msg)
        argsOk = False

    if args.columnCount < 1:
        msg = 'The column count, {0}, is less than 1.'
        msg = msg.format(args.columnCount)
        print(msg)
        argsOk = False

    if args.caseColumn < 1:
        msg = 'The case number column ordinal, {0}, is less than 1.'
        msg = msg.format(args.caseColumn)
        print(msg)
        argsOk = False

    if args.outcomeColumn < 1:
        msg = 'The outcome column ordinal, {0}, is less than 1.'
        msg = msg.format(args.outcomeColumn)
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
    print('args.rowCount: {0}'.format(args.rowCount))
    print('args.columnCount: {0}'.format(args.columnCount))
    print('args.caseColumn: {0}'.format(args.caseColumn))
    print('args.outcomeColumn: {0}'.format(args.outcomeColumn))
    print('args.trainingSetCount: {0}'.format(args.trainingSetCount))

    print

def checkArgs(args, originalLineCount, originalColumnCount, \
              validationLineOrdinals):

    """
    Perform argument checks that require information read from files.
    """
    
    argsOk = True

    # -1 to exclude the header line.
    availableTrainingRows = originalLineCount - 1 - len(validationLineOrdinals)
    if args.rowCount > availableTrainingRows:
        msg = 'The row count, {0}, is greater than the available training rows,'
        msg += ' {1} = {2}(original file rows) - 1(exclude header line) -'
        msg += ' {3}(exclude validation rows).'
        msg = msg.format(args.rowCount, availableTrainingRows, originalLineCount, \
                         len(validationLineOrdinals))
        print(msg)
        argsOk = False

    if args.caseColumn > originalColumnCount:
        msg = 'The case number column ordinal, {0}, is greater than the number'
        msg += ' of columns in the original file, {1}.'
        msg = msg.format(args.caseColumn, originalColumnCount)
        print(msg)
        argsOk = False

    if args.outcomeColumn > originalColumnCount:
        msg = 'The output column ordinal, {0}, is greater than the number'
        msg += ' of columns in the original file, {1}.'
        msg = msg.format(args.outcomeColumn, originalColumnCount)
        print(msg)
        argsOk = False

    if args.caseColumn == args.outcomeColumn:
        msg = 'The case number column ordinal and outcome column are the'
        msg += ' same, {0}'
        msg = msg.format(args.caseColumn)
        print(msg)
        argsOk = False

    if not argsOk:
        sys.exit(1)

args = defineArgs()
#printArgs(args)

# Load saved information from creating the validation ordinals.
with open(args.validationOrdinalFileName, 'rb') as validationOrdinalFile:
    (originalLineCount, originalColumnCount, validationLineOrdinals) = \
    pickle.load(validationOrdinalFile)

checkArgs(args, originalLineCount, originalColumnCount, validationLineOrdinals)

# Create training objects.
print('Creating training set objects...', end='')
trSets = []
for i in range(1, args.trainingSetCount + 1):
    try:
        trSet = TrainingSet(i, originalLineCount, originalColumnCount, \
                            validationLineOrdinals, args)
        trSets.append(trSet)
    except (Exception) as e:
        msg = 'An exception occured creating TrainingSet {0} of {1}:\n{2}'
        msg = msg.format(i, args.trainingSetCount, e)
        print(msg)
        sys.exit(1)
print('...Done')

print('Creating training set files...', end='')
with open(args.originalFileName, 'r') as inputFile:
    # Count the line ordinals starting from 1, not the default of 0.
    ordinalbase = 1
    for (ordinal, line) in enumerate(inputFile, ordinalbase):

        # Delete trailing newline from last column, otherwise, if the last
        # column is written to a training set then that training set will have
        # extra blank lines.
        fields = line.rstrip('\n').split(',')
        for trSet in trSets:
            trSet.checkLine(ordinal, fields)
print('...Done')
