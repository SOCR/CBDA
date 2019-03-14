
This project is a set of scripts for creating validation and training sets for
the CBDA machine learning project.

The idea is to use machine learning to examine patient data, including patient
outcomes, using machine learning algorithms, to determine those patient
attributes that best determine patient outcomes, and how those attributes map
to outcomes.

The intent is that once the mapping is determined, it can be used in a clinical
setting to map a particular patient's attributes to the most likely outcome. In
effect, to improve the diagnoses of clinical conditions.

The machine learning process is to take an actual clinical data set (typically
a large one) of known patient attributes and outcomes and divide into various
subsets.

One subset is a validation set - a (typically small) subset of patients from
the data set set aside to test the mahcine learning derived mapping between
attributes and outcomes. This is chosen randomly from the original data set.
For example, 1,000 patient records from a file of 100,000 patient records.

A collection of training subsets is also created, used as inputs to the machine
learning algorithm. Each training subset is a randomly selected set of patient
records, but also only a randomly selected subset of attributes from those
records. For example, from a file of 100,000 patient records, 1,000 training
sets might be created each consisting of 100 patient records with 10 randomly
selected attributes out of 1,000 attributes per patient record.

A training set might (and typically will) have some of the same patient records
and one or more other training subsets, but it is unlikely two training subsets
will have the exact same set of records. Similarly for the patient attributes -
there might, and typicall will be, some overlap among different training set,
but it is unlikely any 2 will have exactl the same attributes.

However, it is desirable for none of the validation set records to be included
in any of the training sets, so the validation step operates on records not
included in the machine learning step.

The training sets are used as input to the machine learning algorithm,
producing a mapping between attributes and outcomes. Then a validation step is
performed which uses the mapping to map attributes in the records of the
validation set to expected outcomes. The expected outcomes are compared to the
actual outcomes for the validation set as a test of the accuracy of the
atrtibute to outcome mapping.

There are 5 scripts in this part of the CBDA project, 4 Python3 scripts and 1
bash script. For the python scripts use the "-h" command line option to see the
command line options for running the script.

These scripts assume the original data file from which validation and training
sets are to be produced conforms to the following:

Is a csv file with a header line that contains text labels for each column.

There is column that contains a case id value.

There is column that contains the outcome.

All other columns are patient attributes.

The scripts are:

*********************************
create-validation-row-ordinals.py
*********************************

This script reads the original data file and produces a set of irandomly
selected line number ordinals that are the lines to be the validation set.

A command line option specifies the number of validation line ordinals to
select.

It writes these  ne ordinals and some other information to a Python Pickle file
(a binary file that is used to save and load Python data structures without the
need for the program to have custome parsing code).

It does not write the actual validation lines.

This requires one complete pass of the original data file, to determine the
number of lines in the file.

***********************
list-validation-info.py
***********************

Lists the contents of a Pickle file create by create-validation-row-ordinals.py
to standard output. Useful for testing and debugging the Python scripts.

************************
create-validation-set.py
************************

This creates the validation file by reads the Pickle file prodcued by
create-validation-row-ordinals.py and the original data file.  It reads the
original file until the last validation line. In practice this is esssentially
a single pass of the original data file.

**********************
create-training-set.py
**********************

This creates the training sets. It reads the Pickle file prodcued by
create-validation-row-ordinals.py and the original data file.  It uses command
line options for the number of rows and columns to include in each training
set, the column ordinals for the case number and outcome columns and the number
of training sets to produce.

It uses the validation ordinals from the Pickle file to exclude those rows from
the training sets.

It also excludes the case number and outcome column from the randomly selected
attribute column ordinals to include for a training set, but it does write
those columns to each training set, in addition to the randomly selected
attribute columns.

It makes a single pass of the original data set to create all the training
sets. If 1,000 training sets are created, it takes one pass of the original
data set, not 1,000 passes.

This is limited by the maximum number of open files a user or user process can
have on a system, which is usually around 1,020 on a Linux system.

This script produces 3 files per training set:

training-set-i:

	The training set data, the randomly selected row and columns (plus the case
	number and outcome column). Ex. training-set-1, training-set-2, etc.

training-set-i-row-ordinals:

	The	row ordinals for the rows in the training set data.  This may be needed
	by the machine learning step or the validation step.

training-set-i-column-ordinals:

	The	column ordinals for the columns in the training set data, not including
	the case number or outcome column ordinals.  This may be needed by the
	machine learning step or the validation step.

********************
create-test-data-set
********************

This created a test data set, including a header line, with command line
options for specifying the number of rows and number of columns.

Each value in a field contains the row number and column number (except for the
header, which just has the column number).

This makes it easy to determine if the training sets are correct - include the
correct rows and columns for each training set, exclude the rows for the
validation set and the rows and columns of a training set are not exactly the
same as those of another (in the vast majority of cases).

An example test data set with 10 rows and 10 columns:

h1,h2,h3,h4,h5,h6,h7,h8,h9,h10
2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,2.10
3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,3.10
4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,4.9,4.10
5.1,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9,5.10
6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9,6.10
7.1,7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.9,7.10
8.1,8.2,8.3,8.4,8.5,8.6,8.7,8.8,8.9,8.10
9.1,9.2,9.3,9.4,9.5,9.6,9.7,9.8,9.9,9.10
10.1,10.2,10.3,10.4,10.5,10.6,10.7,10.8,10.9,10.10
11.1,11.2,11.3,11.4,11.5,11.6,11.7,11.8,11.9,11.10



