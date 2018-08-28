#!/usr/bin/env python
#Filename: 2HDECAY.py


 ###############################################################################################################
#																												#
#														2HDECAY													#
#																												#
#	Purpose:	Program for the automated calculation of full one-loop electroweak 								#
#				and QCD corrections for one-to-two decays of Higgs particles in the								#
#				two-Higgs doublet model (2HDM).																	#
#	Authors: 	Marcel Krause (marcel.krause@kit.edu)															#
#				Prof. Dr. M. Margarete Muehlleitner (milada.muehlleitner@kit.edu)								#
#	Version:	1.0.0																							#
#	Date:		05.09.2018																						#
#	Copyright:	Copyright 2018, Marcel Krause & Milada Margarete Muehlleitner									#
#	License:	GNU Lesser General Public License (GNU LGPL)													#
#				You may use this program as you wish, for free, as long as you follow							#
#				the restrictions given by the GNU LGPL. When you use this program, 								#
#				please acknowledge our work by citing the following papers:										#
#					The manual for 2HDECAY:																		#
#					 - M. Krause, M. Muhlleitner, M. Spira, arXiv:YYMM.XXX										#
#					The manual for HDECAY:																		#
#					 - A. Djouadi, J. Kalinowski, M. Spira, Comp. Phys. Commun. 108 (1998) 56, hep-ph/9704448	#
#					 - A. Djouadi, J. Kalinowski, M. Muhlleitner, M. Spira, arXiv:1801.09506 [hep-ph]			#
#					The papers on the EW correction to the 2HDM decays:											#
#					 - M. Krause, R. Lorenz, M. Muhlleitner, R. Santos, H. Ziesche, JHEP 1609 (2016) 143		#
#					 - M. Krause, M. Muhlleitner, R. Santos, H. Ziesche, Phys.Rev. D95 (2017) no.7, 075019		#
#					The publication of LoopTools:																#
#					 - T. Hahn, M. Perez-Victoria, Comp. Phys. Commun. 118 (1999) 153-165, hep-ph/9807565		#
#																												#
 ###############################################################################################################


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
#		 Settings		  #
#-------------------------#
# WARNING: do not change these settings if you do not know what they do!
lineToInsert = 122		# This is the line at which the temporary input file ends and at which we append the electroweak corrections
lineWhereAlphaAtMZ = 26 # This is the line at which in the temporary input file the fine-structure constant at the Z boson mass MZ is specified
lineWhereGFCalc = 28	# This is the line at which in the temporary input file the calculated Fermi constant GFCALC is specified
lineWhereMZ = 31		# This is the line at which in the temporary input file the Z boson mass MZ is specified
lineWhereMW = 32		# This is the line at which in the temporary input file the W boson mass MW is specified
lineWhereOSMC = 22		# This is the line at which the OS MC value has to be inserted in the temporary input file
lineWhereOSMB = 23		# This is the line at which the OS MB value has to be inserted in the temporary input file

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

When you use this program please cite:
	The manual for 2HDECAY:
	 - M. Krause, M. Muhlleitner, M. Spira, arXiv:YYMM.XXX
	The manual for HDECAY:
	 - A. Djouadi, J. Kalinowski, M. Spira, Comp. Phys. Commun. 108 (1998) 56, hep-ph/9704448
	 - A. Djouadi, J. Kalinowski, M. Muhlleitner, M. Spira, arXiv:1801.09506 [hep-ph]
	The papers on the EW correction to the 2HDM decays:
	 - M. Krause, R. Lorenz, M. Muhlleitner, R. Santos, H. Ziesche, JHEP 1609 (2016) 143
	 - M. Krause, M. Muhlleitner, R. Santos, H. Ziesche, Phys.Rev. D95 (2017) no.7, 075019
	The publication of LoopTools:
	 - T. Hahn, M. Perez-Victoria, Comp. Phys. Commun. 118 (1999) 153-165, hep-ph/9807565

2HDECAY is released under GNU Lesser General Public License. LoopTools is released under GNU Lesser General Public License.
Copyright 2018, Marcel Krause & Milada Margarete Muehlleitner.
	''')

	# Get a list of all input files
	inputPath = "Input"
	inputFileList = os.listdir(inputPath)
	if '..' in inputFileList:
		inputFileList.remove('..')
	if '.' in inputFileList:
		inputFileList.remove('.')

	# Iterate over all input files
	for inputFileTemp in inputFileList:
		print("Calculating corrections for input file " + inputFileTemp + " ...\n")

		# Copy the input file to the HDECAY subfolder
		print("Copying input files into HDECAY folder...\n")
		filenameIn = "Input" + os.sep + inputFileTemp
		filenameOut = "HDECAY" + os.sep + "hdecay.in"
		# Remove any existing input and fermion masses file in HDECAY
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)
		if os.path.isfile("HDECAY" + os.sep + "fermionmasses.dat"):
			os.remove("HDECAY" + os.sep + "fermionmasses.dat")
		copyfile(filenameIn, filenameOut)

		# Let HDECAY run in minimal mode to produce the fermion mass file
		print("Starting HDECAY in minimal mode...")
		os.chdir('HDECAY')
		prompt = ['./run', '1']
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		os.chdir('..')
		print("HDECAY in minimal mode terminated.\n")

		# Read the ferminon masses from the fermion mass file
		filenameMasses = "HDECAY" + os.sep + "fermionmasses.dat"
		massFileLines = list(line.rstrip('\n') for line in open(filenameMasses))
		MCOSCalc = float((massFileLines[0].split('='))[1].strip())
		MBOSCalc = float((massFileLines[1].split('='))[1].strip())
		
		# Copy the file name to the HDECAY folder and truncate it at the end
		print("Copying input files into HDECAY folder...")
		filenameIn = "Input" + os.sep + inputFileTemp
		filenameOut = "HDECAY" + os.sep + "hdecay.in"
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)
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

		# Write a copy of the file to the output folder, but add GFCALC, MCOSCALC and MBOSCALC with the calculated values
		GFcalc = pi/sqrt(2)*alphaAtMZ/(massMW**2*(1-massMW**2/massMZ**2))
		GFline = "GFCALC   = " + str(GFcalc) + "\n"
		MCOSline = "MCOSCALC = " + str(MCOSCalc) + "\n"
		MBOSline = "MBOSCALC = " + str(MBOSCalc) + "\n"
		lineCount = 1
		convertedFile = ''
		for line in convertedFileHandler:
			if lineCount == lineWhereGFCalc:
				convertedFile += GFline
			elif lineCount == lineWhereOSMC:
				convertedFile += MCOSline
				convertedFile += MBOSline
				convertedFile += line
			else:
				convertedFile += line
			lineCount += 1
		fileHandler = open(filenameOut, "w")
		fileHandler.write(convertedFile)
		fileHandler.close()
		print("... done.\n")
		
		# Calculate the electroweak corrections
		print("Calculating electroweak corrections...\n")
		prompt = ['./electroweakCorrections', '0', '0', '0', '1', 'HDECAY' + os.sep + 'hdecay.in', 'hdecay.in']
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		print("Calculation of electroweak corrections done.\n")

		# Replace the newline character in each file with a proper newline
		print("Postprocessing temporary input file...")
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
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)
		fileHandler = open(filenameOut, "w")
		fileHandler.write(convertedFile)
		fileHandler.close()
		print("... done.\n")

		# Start HDECAY in the normal (non-minimal) configuration
		print("Starting HDECAY in standard mode...")
		os.chdir('HDECAY')
		prompt = ['./run']
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		os.chdir('..')
		print("HDECAY in standard mode terminated.\n")

		# Copy the output file to the results folder
		print("Copying input files into output folder...")
		filenameIn = "HDECAY" + os.sep + "slha.out"
		filenameOut = "Results" + os.sep + inputFileTemp.replace('.in', '.out')
		copyfile(filenameIn, filenameOut)
		print("... done.\n")

		# Cleaning 
		if os.path.isfile("HDECAY" + os.sep + "fermionmasses.dat"):
			os.remove("HDECAY" + os.sep + "fermionmasses.dat")
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)

		print("Corrections for input file " + inputFileTemp + " done.\n")

	# End of program is reached
	print("\nCalculation finished. Thanks for using 2HDECAY!\n")
	sys.exit()
