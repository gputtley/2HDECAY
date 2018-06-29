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
import subprocess
# import multiprocessing
import CommonFunctions			# Provides common, often used functions for different scripts of 2HDECAY



#-------------------------#
#		 Functions		  #
#-------------------------#


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

	
	# Copy the file name to the result folder
	print("Copying input files into output folder...\n")
	filenameIn = "Parameters" + os.sep + "hdecay.in"
	filenameOut = "Results" + os.sep + "hdecay.in"
	copyfile(filenameIn, filenameOut)
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
	for line in fileHandler:
		# Convert the literal newlines to actual ones
		convertedFile += line.replace('\\n', '\n')
	fileHandler.close()

	# Store the results file in the correct directory
	fileHandler = open(filenameOut, "w")
	fileHandler.write(convertedFile)
	fileHandler.close()
	print("Postprocessing of output files done.\n")

	sys.exit()
