#!/usr/bin/env python
#Filename: 2HDECAY.py


 #######################################################################################
#																						#
#									2HDECAY												#
#																						#
#	Purpose:	Program for the automated calculation of full one-loop electroweak 		#
#				and QCD corrections for one-to-two decays of Higgs particles in the		#
#				two-Higgs doublet model (2HDM).											#
#	Author: 	Marcel Krause (marcel.krause@kit.edu)									#
#				Prof. Dr. M. Margarete Mühlleitner (milada.muehlleitner@kit.edu)		#
#	Version:	1.0.0																	#
#	Date:		26.06.2018																#
#	License:	TBA																		#
#																						#
 #######################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import sys
import os
from shutil import copyfile, rmtree
from math import pi, sqrt
import subprocess
# import multiprocessing
import CommonFunctions			# Provides common, often used functions for different scripts of 2HDECAY



#-------------------------#
#		 Functions		  #
#-------------------------#

#-------------------------#
#		 Settings		  #
#-------------------------#
# WARNING: do not change these settings if you do not know what they do!
lineToInsert = 120		# This is the line at which the original input file ends and at which we append the electroweak corrections
lineWhereAlphaAtMZ = 26	# This is the line at which in the original input file the fine-structure constant at the Z boson mass MZ is specified
lineWhereGFCalc = 28	# This is the line at which in the original input file the calculated Fermi constant GFCALC is specified
lineWhereMZ = 31		# This is the line at which in the original input file the Z boson mass MZ is specified
lineWhereMW = 32		# This is the line at which in the original input file the W boson mass MW is specified

#----------------------------#
#		 Main Program		 #
#----------------------------#

if __name__ == "__main__":		# This is necessary for correct parallelisation under Windows (Windows does not know fork)
	# Print the welcome screen
	print('''
	+---------------------------------------+
	|                                       |
	|             2HDECAY 1.0.0             |
	|                                       |
	|                             /         |
	|                            /          |
	|                           /           |
	|                      --- /            |
	|      ______________/     \            |
	|                    \     /            |
	|                      --- \            |
	|                           \           |
	|                            \          |
	|                             \         |
	|                                       |
	+---------------------------------------+
	''')
	
	# Copy the file name to the result folder and truncate it at the end
	print("Copying input files into output folder...\n")
	filenameIn = "Parameters" + os.sep + "hdecay.in"
	filenameOut = "Results" + os.sep + "hdecay.in"
	# copyfile(filenameIn, filenameOut)
	fileHandler = open(filenameIn, "r")
	convertedFileHandler = []
	lineCount = 1
	for line in fileHandler:
		# Write the current line in an array
		convertedFileHandler.append(line)
		# Pick out the values of MW, MZ and alphaAtMZ
		if lineCount == lineWhereMZ:
			massMZ = float((line.split())[2])
		if lineCount == lineWhereMW:
			massMW = float((line.split())[2])
		if lineCount == lineWhereAlphaAtMZ:
			alphaAtMZ = float((line.split())[2])
		lineCount += 1
	fileHandler.close()
	# Write a copy of the file to the output folder, but replace GFCALC in the file with the calculated value
	GFcalc = pi/sqrt(2)*alphaAtMZ/(massMW**2*(1-massMW**2/massMZ**2))
	GFline = "GFCALC   = " + str(GFcalc) + "\n"
	lineCount = 1
	convertedFile = ''
	for line in convertedFileHandler:
		if lineCount == lineWhereGFCalc:
			convertedFile += GFline
		else:
			convertedFile += line
		lineCount += 1
	fileHandler = open(filenameOut, "w")
	fileHandler.write(convertedFile)
	fileHandler.close()
	print("Copying of input files done.\n")
	
	# Calculate the electroweak corrections
	print("Calculating electroweak corrections...\n")
	prompt = ['electroweakCorrections', '0', '0', '0', '1', 'InputFiles' + os.sep + 'input.in', 'test.txt']
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print("Calculation of electroweak corrections done.\n")

	# Replace the newline character in each file with a proper newline
	print("Postprocessing output file...\n")
	fileHandler = open(filenameOut, "r")
	convertedFile = ''
	lineCount = 1
	for line in fileHandler:
		# Convert the literal newlines to actual ones and remove the leading whitespace from the Fortran output
		if (lineCount == lineToInsert):
			lineToReplace = line.replace('\\n', '\n')[1:]
		else:
			lineToReplace = line.replace('\\n', '\n')
		convertedFile += lineToReplace
		# print(lineToReplace)
		lineCount += 1
	fileHandler.close()

	# Store the results file in the correct directory
	fileHandler = open(filenameOut, "w")
	fileHandler.write(convertedFile)
	fileHandler.close()
	print("Postprocessing of output files done.\n")

	sys.exit()
