#!/usr/bin/env python
#Filename: setup.py


 ###################################################################################
#																					#
#									setup.py 										#
#																					#
#	Purpose:																		#
#				* Calls the LoopTools installer										#
#				* Makes the HDECAY sub-program										#
#				* Sets up 2HDECAY 													#
#				* Creates the makefile and electroweakCorrections.F90				#
#				* Makes 2HDECAY														#
#																					#
 ###################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import os
from shutil import rmtree
import subprocess
import sys
from fnmatch import fnmatch
import CommonFunctions				# Provides common, often used functions for different scripts of 2HDECAY
import Config						# Provides paths 

#-------------------------#
#		 Functions		  #
#-------------------------#


def createMakefile(pathToMakefile, relativePathToLoopTools, relativePathToLoopToolsLibs, relativePathToLoopToolsExecs, useRelativePath, chosenCompiler):
	# Get a list of all processes
	pathToProcesses = 'BuildingBlocks' + os.sep + 'Processes'
	processDirList = os.listdir(pathToProcesses)

	# Check whether the OS is Windows or not for giving the decayWidth application the correct file ending 
	applicationEnding = ''
	if os.name == 'nt':
		applicationEnding = '.exe'

	makefile = open(pathToMakefile, 'w')
	makefile.truncate()
	makefile.write("###################################\n")
	makefile.write("#       Variables and Paths       #\n")
	makefile.write("###################################\n\n")
	makefile.write("# Specify the path to the LoopTools library:\n")
	if useRelativePath:
		makefile.write("PWD=$(strip $(shell pwd))\n")
		makefile.write("LT=$(PWD)/" + relativePathToLoopTools + "\n")
	else:
		makefile.write("LT=" + relativePathToLoopTools + "\n")
	makefile.write("LTCOMP = $(LT)/" + relativePathToLoopToolsExecs + "/fcc\n")
	makefile.write("IFlags = -I$(LT)/include\n")
	makefile.write("LFlags = -L$(LT)/" + relativePathToLoopToolsLibs + " -looptools\n\n")
	makefile.write("# Choose your compiler:\n")
	makefile.write("FCOMP = " + chosenCompiler + "\n\n")
	makefile.write("# Do NOT change anything below this line by hand!\n")
	makefile.write("SELFENERGIESUSU = BuildingBlocks/SelfEnergies/Usual\n")
	makefile.write("SELFENERGIESALT = BuildingBlocks/SelfEnergies/Alternative\n")
	makefile.write("SELFENERGIESDERIV = BuildingBlocks/SelfEnergiesDerivatives\n")
	makefile.write("PROCESSDEPENDENTSCHEME = BuildingBlocks/ProcessDependentScheme\n")
	makefile.write("TADPOLES = BuildingBlocks/Tadpoles\n")
	for singleProcess in processDirList:
		makefile.write("PROCESS" + singleProcess.upper() + " = BuildingBlocks/Processes/" + singleProcess + "\n")
	makefile.write("\n%.o: %.F90\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(SELFENERGIESALT)/%.o: $(SELFENERGIESALT)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(SELFENERGIESUSU)/%.o: $(SELFENERGIESUSU)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(SELFENERGIESDERIV)/%.o: $(SELFENERGIESDERIV)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(PROCESSDEPENDENTSCHEME)/%.o: $(PROCESSDEPENDENTSCHEME)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(TADPOLES)/%.o: $(TADPOLES)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	for singleProcess in processDirList:
		makefile.write("$(PROCESS" + singleProcess.upper() + ")/%.o: $(PROCESS" + singleProcess.upper() + ")/%.F90 constants.o counterterms.o\n")
		makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n")
	makefile.write("\nelectroweakCorrections: constants.o $(SELFENERGIESUSU)/SelfAA.o $(SELFENERGIESUSU)/SelfAALight.o $(SELFENERGIESUSU)/SelfWpWp.o $(SELFENERGIESUSU)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfAZ0.o $(SELFENERGIESUSU)/SelfAZ0ZeroMom.o $(SELFENERGIESALT)/SelfAA.o $(SELFENERGIESALT)/SelfWpWp.o $(SELFENERGIESALT)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfAZ0.o $(SELFENERGIESALT)/SelfAZ0ZeroMom.o $(SELFENERGIESUSU)/SelfA0A0.o $(SELFENERGIESUSU)/SelfG0A0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfG0G0.o $(SELFENERGIESUSU)/SelfGpGp.o $(SELFENERGIESUSU)/SelfGpHp.o $(SELFENERGIESUSU)/Selfh0h0.o $(SELFENERGIESUSU)/SelfHHh0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfHHHH.o $(SELFENERGIESUSU)/SelfHpHp.o $(SELFENERGIESALT)/SelfA0A0.o $(SELFENERGIESALT)/SelfG0A0.o $(SELFENERGIESALT)/SelfG0G0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfGpGp.o $(SELFENERGIESALT)/SelfGpHp.o $(SELFENERGIESALT)/Selfh0h0.o $(SELFENERGIESALT)/SelfHHh0.o $(SELFENERGIESALT)/SelfHHHH.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfHpHp.o $(SELFENERGIESDERIV)/DSelfAA.o $(SELFENERGIESDERIV)/DSelfAALight.o $(SELFENERGIESDERIV)/DSelfWpWp.o $(SELFENERGIESDERIV)/DSelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfAZ0.o $(SELFENERGIESDERIV)/DSelfA0A0.o $(SELFENERGIESDERIV)/DSelfG0A0.o $(SELFENERGIESDERIV)/DSelfG0G0.o $(SELFENERGIESDERIV)/DSelfGpGp.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfGpHp.o $(SELFENERGIESDERIV)/DSelfHpHp.o $(SELFENERGIESDERIV)/DSelfh0h0.o $(SELFENERGIESDERIV)/DSelfHHh0.o $(SELFENERGIESDERIV)/DSelfHHHH.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfHHh0Add.o $(SELFENERGIESALT)/SelfG0A0Add.o $(SELFENERGIESALT)/SelfGpHpAdd.o $(TADPOLES)/TadHH.o $(TADPOLES)/Tadh0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuENeuELeft.o $(SELFENERGIESUSU)/SelfNeuENeuERight.o $(SELFENERGIESUSU)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuMNeuMLeft.o $(SELFENERGIESUSU)/SelfNeuMNeuMRight.o $(SELFENERGIESUSU)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuTNeuTLeft.o $(SELFENERGIESUSU)/SelfNeuTNeuTRight.o $(SELFENERGIESUSU)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfElElLeft.o $(SELFENERGIESUSU)/SelfElElRight.o $(SELFENERGIESUSU)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfMuMuLeft.o $(SELFENERGIESUSU)/SelfMuMuRight.o $(SELFENERGIESUSU)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeft.o $(SELFENERGIESUSU)/SelfTauTauRight.o $(SELFENERGIESUSU)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftQED.o $(SELFENERGIESUSU)/SelfTauTauRightQED.o $(SELFENERGIESUSU)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftWeak.o $(SELFENERGIESUSU)/SelfTauTauRightWeak.o $(SELFENERGIESUSU)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDDLeft.o $(SELFENERGIESUSU)/SelfDDRight.o $(SELFENERGIESUSU)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUULeft.o $(SELFENERGIESUSU)/SelfUURight.o $(SELFENERGIESUSU)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSSLeft.o $(SELFENERGIESUSU)/SelfSSRight.o $(SELFENERGIESUSU)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCCLeft.o $(SELFENERGIESUSU)/SelfCCRight.o $(SELFENERGIESUSU)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfBBLeft.o $(SELFENERGIESUSU)/SelfBBRight.o $(SELFENERGIESUSU)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTTLeft.o $(SELFENERGIESUSU)/SelfTTRight.o $(SELFENERGIESUSU)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUCLeft.o $(SELFENERGIESUSU)/SelfUCRight.o $(SELFENERGIESUSU)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUTLeft.o $(SELFENERGIESUSU)/SelfUTRight.o $(SELFENERGIESUSU)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCTLeft.o $(SELFENERGIESUSU)/SelfCTRight.o $(SELFENERGIESUSU)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDSLeft.o $(SELFENERGIESUSU)/SelfDSRight.o $(SELFENERGIESUSU)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDBLeft.o $(SELFENERGIESUSU)/SelfDBRight.o $(SELFENERGIESUSU)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSBLeft.o $(SELFENERGIESUSU)/SelfSBRight.o $(SELFENERGIESUSU)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuENeuELeft.o $(SELFENERGIESALT)/SelfNeuENeuERight.o $(SELFENERGIESALT)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuMNeuMLeft.o $(SELFENERGIESALT)/SelfNeuMNeuMRight.o $(SELFENERGIESALT)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuTNeuTLeft.o $(SELFENERGIESALT)/SelfNeuTNeuTRight.o $(SELFENERGIESALT)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfElElLeft.o $(SELFENERGIESALT)/SelfElElRight.o $(SELFENERGIESALT)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfMuMuLeft.o $(SELFENERGIESALT)/SelfMuMuRight.o $(SELFENERGIESALT)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeft.o $(SELFENERGIESALT)/SelfTauTauRight.o $(SELFENERGIESALT)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftQED.o $(SELFENERGIESALT)/SelfTauTauRightQED.o $(SELFENERGIESALT)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftWeak.o $(SELFENERGIESALT)/SelfTauTauRightWeak.o $(SELFENERGIESALT)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDDLeft.o $(SELFENERGIESALT)/SelfDDRight.o $(SELFENERGIESALT)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUULeft.o $(SELFENERGIESALT)/SelfUURight.o $(SELFENERGIESALT)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSSLeft.o $(SELFENERGIESALT)/SelfSSRight.o $(SELFENERGIESALT)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCCLeft.o $(SELFENERGIESALT)/SelfCCRight.o $(SELFENERGIESALT)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfBBLeft.o $(SELFENERGIESALT)/SelfBBRight.o $(SELFENERGIESALT)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTTLeft.o $(SELFENERGIESALT)/SelfTTRight.o $(SELFENERGIESALT)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUCLeft.o $(SELFENERGIESALT)/SelfUCRight.o $(SELFENERGIESALT)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUTLeft.o $(SELFENERGIESALT)/SelfUTRight.o $(SELFENERGIESALT)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCTLeft.o $(SELFENERGIESALT)/SelfCTRight.o $(SELFENERGIESALT)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDSLeft.o $(SELFENERGIESALT)/SelfDSRight.o $(SELFENERGIESALT)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDBLeft.o $(SELFENERGIESALT)/SelfDBRight.o $(SELFENERGIESALT)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSBLeft.o $(SELFENERGIESALT)/SelfSBRight.o $(SELFENERGIESALT)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuENeuELeft.o $(SELFENERGIESDERIV)/DSelfNeuENeuERight.o $(SELFENERGIESDERIV)/DSelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuMNeuMLeft.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMRight.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuTNeuTLeft.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTRight.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfElElLeft.o $(SELFENERGIESDERIV)/DSelfElElRight.o $(SELFENERGIESDERIV)/DSelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfMuMuLeft.o $(SELFENERGIESDERIV)/DSelfMuMuRight.o $(SELFENERGIESDERIV)/DSelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeft.o $(SELFENERGIESDERIV)/DSelfTauTauRight.o $(SELFENERGIESDERIV)/DSelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftQED.o $(SELFENERGIESDERIV)/DSelfTauTauRightQED.o $(SELFENERGIESDERIV)/DSelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftWeak.o $(SELFENERGIESDERIV)/DSelfTauTauRightWeak.o $(SELFENERGIESDERIV)/DSelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDDLeft.o $(SELFENERGIESDERIV)/DSelfDDRight.o $(SELFENERGIESDERIV)/DSelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUULeft.o $(SELFENERGIESDERIV)/DSelfUURight.o $(SELFENERGIESDERIV)/DSelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSSLeft.o $(SELFENERGIESDERIV)/DSelfSSRight.o $(SELFENERGIESDERIV)/DSelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCCLeft.o $(SELFENERGIESDERIV)/DSelfCCRight.o $(SELFENERGIESDERIV)/DSelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfBBLeft.o $(SELFENERGIESDERIV)/DSelfBBRight.o $(SELFENERGIESDERIV)/DSelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTTLeft.o $(SELFENERGIESDERIV)/DSelfTTRight.o $(SELFENERGIESDERIV)/DSelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUCLeft.o $(SELFENERGIESDERIV)/DSelfUCRight.o $(SELFENERGIESDERIV)/DSelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUTLeft.o $(SELFENERGIESDERIV)/DSelfUTRight.o $(SELFENERGIESDERIV)/DSelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCTLeft.o $(SELFENERGIESDERIV)/DSelfCTRight.o $(SELFENERGIESDERIV)/DSelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDSLeft.o $(SELFENERGIESDERIV)/DSelfDSRight.o $(SELFENERGIESDERIV)/DSelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDBLeft.o $(SELFENERGIESDERIV)/DSelfDBRight.o $(SELFENERGIESDERIV)/DSelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSBLeft.o $(SELFENERGIESDERIV)/DSelfSBRight.o $(SELFENERGIESDERIV)/DSelfSBScalar.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/HHtoTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/h0toTauPTauMProcDepVC.o \\\n")
	for singleProcess in processDirList:
		makefile.write("\t$(PROCESS" + singleProcess.upper() + ")/TreeLevelWidthRed.o $(PROCESS" + singleProcess.upper() + ")/NLOWidthRed.o $(PROCESS" + singleProcess.upper() + ")/NLOTadWidthRed.o $(PROCESS" + singleProcess.upper() + ")/Counterterm.o $(PROCESS" + singleProcess.upper() + ")/RealCorrections.o \\\n")
	makefile.write("\tcounterterms.o getParameters.o electroweakCorrections.o\n")
	makefile.write("\t$(LTCOMP) $(IFlags) constants.o $(SELFENERGIESUSU)/SelfAA.o $(SELFENERGIESUSU)/SelfAALight.o $(SELFENERGIESUSU)/SelfWpWp.o $(SELFENERGIESUSU)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfAZ0.o $(SELFENERGIESUSU)/SelfAZ0ZeroMom.o $(SELFENERGIESALT)/SelfAA.o $(SELFENERGIESALT)/SelfWpWp.o $(SELFENERGIESALT)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfAZ0.o $(SELFENERGIESALT)/SelfAZ0ZeroMom.o $(SELFENERGIESUSU)/SelfA0A0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfG0A0.o $(SELFENERGIESUSU)/SelfG0G0.o $(SELFENERGIESUSU)/SelfGpGp.o $(SELFENERGIESUSU)/SelfGpHp.o $(SELFENERGIESUSU)/Selfh0h0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfHHh0.o $(SELFENERGIESUSU)/SelfHHHH.o $(SELFENERGIESUSU)/SelfHpHp.o $(SELFENERGIESALT)/SelfA0A0.o $(SELFENERGIESALT)/SelfG0A0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfG0G0.o $(SELFENERGIESALT)/SelfGpGp.o $(SELFENERGIESALT)/SelfGpHp.o $(SELFENERGIESALT)/Selfh0h0.o $(SELFENERGIESALT)/SelfHHh0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfHHHH.o $(SELFENERGIESALT)/SelfHpHp.o $(SELFENERGIESDERIV)/DSelfAA.o $(SELFENERGIESDERIV)/DSelfAALight.o $(SELFENERGIESDERIV)/DSelfWpWp.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfZ0Z0.o $(SELFENERGIESDERIV)/DSelfAZ0.o $(SELFENERGIESDERIV)/DSelfA0A0.o $(SELFENERGIESDERIV)/DSelfG0A0.o $(SELFENERGIESDERIV)/DSelfG0G0.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfGpGp.o $(SELFENERGIESDERIV)/DSelfGpHp.o $(SELFENERGIESDERIV)/DSelfHpHp.o $(SELFENERGIESDERIV)/DSelfh0h0.o $(SELFENERGIESDERIV)/DSelfHHh0.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfHHHH.o $(SELFENERGIESALT)/SelfHHh0Add.o $(SELFENERGIESALT)/SelfG0A0Add.o $(SELFENERGIESALT)/SelfGpHpAdd.o \\\n")
	makefile.write("\t$(TADPOLES)/TadHH.o $(TADPOLES)/Tadh0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuENeuELeft.o $(SELFENERGIESUSU)/SelfNeuENeuERight.o $(SELFENERGIESUSU)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuMNeuMLeft.o $(SELFENERGIESUSU)/SelfNeuMNeuMRight.o $(SELFENERGIESUSU)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuTNeuTLeft.o $(SELFENERGIESUSU)/SelfNeuTNeuTRight.o $(SELFENERGIESUSU)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfElElLeft.o $(SELFENERGIESUSU)/SelfElElRight.o $(SELFENERGIESUSU)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfMuMuLeft.o $(SELFENERGIESUSU)/SelfMuMuRight.o $(SELFENERGIESUSU)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeft.o $(SELFENERGIESUSU)/SelfTauTauRight.o $(SELFENERGIESUSU)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftQED.o $(SELFENERGIESUSU)/SelfTauTauRightQED.o $(SELFENERGIESUSU)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftWeak.o $(SELFENERGIESUSU)/SelfTauTauRightWeak.o $(SELFENERGIESUSU)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDDLeft.o $(SELFENERGIESUSU)/SelfDDRight.o $(SELFENERGIESUSU)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUULeft.o $(SELFENERGIESUSU)/SelfUURight.o $(SELFENERGIESUSU)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSSLeft.o $(SELFENERGIESUSU)/SelfSSRight.o $(SELFENERGIESUSU)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCCLeft.o $(SELFENERGIESUSU)/SelfCCRight.o $(SELFENERGIESUSU)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfBBLeft.o $(SELFENERGIESUSU)/SelfBBRight.o $(SELFENERGIESUSU)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTTLeft.o $(SELFENERGIESUSU)/SelfTTRight.o $(SELFENERGIESUSU)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUCLeft.o $(SELFENERGIESUSU)/SelfUCRight.o $(SELFENERGIESUSU)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUTLeft.o $(SELFENERGIESUSU)/SelfUTRight.o $(SELFENERGIESUSU)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCTLeft.o $(SELFENERGIESUSU)/SelfCTRight.o $(SELFENERGIESUSU)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDSLeft.o $(SELFENERGIESUSU)/SelfDSRight.o $(SELFENERGIESUSU)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDBLeft.o $(SELFENERGIESUSU)/SelfDBRight.o $(SELFENERGIESUSU)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSBLeft.o $(SELFENERGIESUSU)/SelfSBRight.o $(SELFENERGIESUSU)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuENeuELeft.o $(SELFENERGIESALT)/SelfNeuENeuERight.o $(SELFENERGIESALT)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuMNeuMLeft.o $(SELFENERGIESALT)/SelfNeuMNeuMRight.o $(SELFENERGIESALT)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuTNeuTLeft.o $(SELFENERGIESALT)/SelfNeuTNeuTRight.o $(SELFENERGIESALT)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfElElLeft.o $(SELFENERGIESALT)/SelfElElRight.o $(SELFENERGIESALT)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfMuMuLeft.o $(SELFENERGIESALT)/SelfMuMuRight.o $(SELFENERGIESALT)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeft.o $(SELFENERGIESALT)/SelfTauTauRight.o $(SELFENERGIESALT)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftQED.o $(SELFENERGIESALT)/SelfTauTauRightQED.o $(SELFENERGIESALT)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftWeak.o $(SELFENERGIESALT)/SelfTauTauRightWeak.o $(SELFENERGIESALT)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDDLeft.o $(SELFENERGIESALT)/SelfDDRight.o $(SELFENERGIESALT)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUULeft.o $(SELFENERGIESALT)/SelfUURight.o $(SELFENERGIESALT)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSSLeft.o $(SELFENERGIESALT)/SelfSSRight.o $(SELFENERGIESALT)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCCLeft.o $(SELFENERGIESALT)/SelfCCRight.o $(SELFENERGIESALT)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfBBLeft.o $(SELFENERGIESALT)/SelfBBRight.o $(SELFENERGIESALT)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTTLeft.o $(SELFENERGIESALT)/SelfTTRight.o $(SELFENERGIESALT)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUCLeft.o $(SELFENERGIESALT)/SelfUCRight.o $(SELFENERGIESALT)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUTLeft.o $(SELFENERGIESALT)/SelfUTRight.o $(SELFENERGIESALT)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCTLeft.o $(SELFENERGIESALT)/SelfCTRight.o $(SELFENERGIESALT)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDSLeft.o $(SELFENERGIESALT)/SelfDSRight.o $(SELFENERGIESALT)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDBLeft.o $(SELFENERGIESALT)/SelfDBRight.o $(SELFENERGIESALT)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSBLeft.o $(SELFENERGIESALT)/SelfSBRight.o $(SELFENERGIESALT)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuENeuELeft.o $(SELFENERGIESDERIV)/DSelfNeuENeuERight.o $(SELFENERGIESDERIV)/DSelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuMNeuMLeft.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMRight.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuTNeuTLeft.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTRight.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfElElLeft.o $(SELFENERGIESDERIV)/DSelfElElRight.o $(SELFENERGIESDERIV)/DSelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfMuMuLeft.o $(SELFENERGIESDERIV)/DSelfMuMuRight.o $(SELFENERGIESDERIV)/DSelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeft.o $(SELFENERGIESDERIV)/DSelfTauTauRight.o $(SELFENERGIESDERIV)/DSelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftQED.o $(SELFENERGIESDERIV)/DSelfTauTauRightQED.o $(SELFENERGIESDERIV)/DSelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftWeak.o $(SELFENERGIESDERIV)/DSelfTauTauRightWeak.o $(SELFENERGIESDERIV)/DSelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDDLeft.o $(SELFENERGIESDERIV)/DSelfDDRight.o $(SELFENERGIESDERIV)/DSelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUULeft.o $(SELFENERGIESDERIV)/DSelfUURight.o $(SELFENERGIESDERIV)/DSelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSSLeft.o $(SELFENERGIESDERIV)/DSelfSSRight.o $(SELFENERGIESDERIV)/DSelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCCLeft.o $(SELFENERGIESDERIV)/DSelfCCRight.o $(SELFENERGIESDERIV)/DSelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfBBLeft.o $(SELFENERGIESDERIV)/DSelfBBRight.o $(SELFENERGIESDERIV)/DSelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTTLeft.o $(SELFENERGIESDERIV)/DSelfTTRight.o $(SELFENERGIESDERIV)/DSelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUCLeft.o $(SELFENERGIESDERIV)/DSelfUCRight.o $(SELFENERGIESDERIV)/DSelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUTLeft.o $(SELFENERGIESDERIV)/DSelfUTRight.o $(SELFENERGIESDERIV)/DSelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCTLeft.o $(SELFENERGIESDERIV)/DSelfCTRight.o $(SELFENERGIESDERIV)/DSelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDSLeft.o $(SELFENERGIESDERIV)/DSelfDSRight.o $(SELFENERGIESDERIV)/DSelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDBLeft.o $(SELFENERGIESDERIV)/DSelfDBRight.o $(SELFENERGIESDERIV)/DSelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSBLeft.o $(SELFENERGIESDERIV)/DSelfSBRight.o $(SELFENERGIESDERIV)/DSelfSBScalar.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/HHtoTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/h0toTauPTauMProcDepVC.o \\\n")
	for singleProcess in processDirList:	
		makefile.write("\t$(PROCESS" + singleProcess.upper() + ")/TreeLevelWidthRed.o $(PROCESS" + singleProcess.upper() + ")/NLOWidthRed.o $(PROCESS" + singleProcess.upper() + ")/NLOTadWidthRed.o $(PROCESS" + singleProcess.upper() + ")/Counterterm.o $(PROCESS" + singleProcess.upper() + ")/RealCorrections.o \\\n")
	makefile.write("\tcounterterms.o getParameters.o electroweakCorrections.o $(LFlags) -o electroweakCorrections" + applicationEnding + "\n\n")
	makefile.write("clean:\n")
	makefile.write("\trm -f *.o\n")
	makefile.write("\trm -f $(SELFENERGIESALT)/*.o\n")
	makefile.write("\trm -f $(SELFENERGIESUSU)/*.o\n")
	makefile.write("\trm -f $(SELFENERGIESDERIV)/*.o\n")
	makefile.write("\trm -f $(PROCESSDEPENDENTSCHEME)/*.o\n")
	makefile.write("\trm -f $(TADPOLES)/*.o\n")
	for singleProcess in processDirList:
		makefile.write("\trm -f $(PROCESS" + singleProcess.upper() + ")/*.o\n")

	makefile.close()

def createElectroweakCorrections():
	# Process exclusion list (decays to electrons, up and down quarks are not calculated, since these particles are massless in HDECAY)
	# decayExclusion = ["A0toDDBar", "A0toElElBar", "A0toUUBar", "h0toDDBar", "h0toElElBar", "h0toUUBar"]

	# Get the correct OS separator for OS-independent support of the input read-in
	currentOS = sys.platform
	if currentOS == 'win32':
		OStype = 0
		pathToOutputSeparator = '\\\\'
	else:
		OStype = 1
		pathToOutputSeparator = '/'

	# Get the process list and the list of masses 
	processDir = "BuildingBlocks" + os.sep + "Processes"
	processList = os.listdir(processDir)
	processDescriptionList = [None] * len(processList)
	longestName = 0
	for pickProcess in processList:
		if len(pickProcess) > longestName:
			longestName = len(pickProcess)
		processDescFilePath = processDir + os.sep + pickProcess + os.sep + "processDescription.txt"
		if not os.path.isfile(processDescFilePath):
			print('Error: process description file for process {} not found! Please add the processDescription.txt file first to the {} folder. Terminating the setup of 2HDECAY now. Setup incomplete.'.format(pickProcess, pickProcess))
			sys.exit()
		else:
			lines = [line.rstrip('\n') for line in open(processDescFilePath)]
			massString = lines[1]
			massStringList = massString.split(',')
			symmetryFactorString = (lines[4].split())[0]
			processDesc = [pickProcess, lines[0], massStringList[0], massStringList[1], massStringList[2], symmetryFactorString]
			isIncluded = (lines[2].split())[0]
			wantedPosition = int((lines[3].split())[0]) - 1
			if isIncluded == '1':
				processDescriptionList[wantedPosition] = processDesc
	processDescriptionList = [x for x in processDescriptionList if x is not None]

	# Print the electroweakCorrections.F90 file
	electroweakCorrectionsFile = open("electroweakCorrections.F90", 'w')
	electroweakCorrectionsFile.truncate()
	electroweakCorrectionsFile.write("program electroweakCorrections\n")
	electroweakCorrectionsFile.write("\tuse constants\n")
	electroweakCorrectionsFile.write("\tuse counterterms\n")
	electroweakCorrectionsFile.write("\timplicit none\n")
	electroweakCorrectionsFile.write('#include "looptools.h"\n')

	electroweakCorrectionsFile.write("\tcharacter(len=26) :: tempVal\n")
	electroweakCorrectionsFile.write("\tcharacter(len=2) :: tempVal2, tempVal3\n")
	electroweakCorrectionsFile.write("\tcharacter(len=32) :: arg\n")
	electroweakCorrectionsFile.write("\tcharacter(len=50) :: fileName, fileNameFilled, targetName\n")
	electroweakCorrectionsFile.write("\tcharacter(len=600000) :: outputFileContent\n")
	electroweakCorrectionsFile.write("\tcharacter(300), parameter :: pathToOutputFiles = 'HDECAY" + pathToOutputSeparator + "'\n")
	electroweakCorrectionsFile.write("\tinteger arguments(5)\n")
	electroweakCorrectionsFile.write("\tinteger, parameter :: maxNumberSchemes = 14\n")
	electroweakCorrectionsFile.write("\tlogical :: debugModeOn = .false.\n")
	electroweakCorrectionsFile.write("\tdouble precision prefactor, treeLevelWidth, NLOWidth(maxNumberSchemes), fullamplitude(maxNumberSchemes)\n")
	electroweakCorrectionsFile.write("\tdouble precision NLOVCwidth, NLOVCwoIRwidth, NLOIRonlywidth\n")
	for x in range(0, len(processDescriptionList)):
		electroweakCorrectionsFile.write("\tdouble precision " + processDescriptionList[x][0] + "Tree, " + processDescriptionList[x][0] + "CT, " + processDescriptionList[x][0] + "Real\n")
		electroweakCorrectionsFile.write("\tdouble complex " + processDescriptionList[x][0] + "VC, " + processDescriptionList[x][0] + "Tad\n")

	electroweakCorrectionsFile.write("\tdouble precision treeLevelTemp, realCorrectionsTemp\n")
	electroweakCorrectionsFile.write("\tdouble complex vertexCorrectionsTemp, vertexTadpolesTemp\n")
	electroweakCorrectionsFile.write("\tdouble precision m1, m2, m3, kinematicThreshold\n")
	electroweakCorrectionsFile.write("\tinteger m, n, o, p, q, r, fileNameLength, point, statWrite\n\n")

	electroweakCorrectionsFile.write("\t! Get the command line arguments standing for the different running options\n")
	electroweakCorrectionsFile.write("\t! Argument 1: perform UV divergence check (1: true, 0: false; default: 0)\n")
	electroweakCorrectionsFile.write("\t! Argument 2: perform IR divergence check (1: true, 0: false; default: 0)\n")
	electroweakCorrectionsFile.write("\t! Argument 3: perform gauge dependence check (2: true (prompt for continuation of program if gauge-dependence is detected), 1: true (no prompts for continuation), 0: false; default: 0)\n")
	electroweakCorrectionsFile.write("\t! Argument 4: perform numerical evaluation (1: true, 0: false; default: 1)\n")
	electroweakCorrectionsFile.write("\t! Argument 5: relative path to the 2HDM input parameter file, starting from the Parameters directory of 2HDECAY\n")
	electroweakCorrectionsFile.write("\t! Argument 6: relative path to the target file containing the results of the calculation, starting from the Temp/Results directory of 2HDECAY\n")
	electroweakCorrectionsFile.write("\tdo o = 1, iargc()\n")
	electroweakCorrectionsFile.write("\t\tcall getarg(o, arg)\n")
	electroweakCorrectionsFile.write("\t\tif (arg == '1') then\n")
	electroweakCorrectionsFile.write("\t\t\targuments(o) = 1\n")
	electroweakCorrectionsFile.write("\t\telse if (arg == '2') then\n")
	electroweakCorrectionsFile.write("\t\t\targuments(o) = 2\n")
	electroweakCorrectionsFile.write("\t\telse if (arg == '0') then\n")
	electroweakCorrectionsFile.write("\t\t\targuments(o) = 0\n")
	electroweakCorrectionsFile.write("\t\telse\n")
	electroweakCorrectionsFile.write("\t\t\tif (o == 5) then\n")
	electroweakCorrectionsFile.write("\t\t\t\tfileName = arg\n")
	electroweakCorrectionsFile.write("\t\t\telse if (o == 6) then\n")
	electroweakCorrectionsFile.write("\t\t\t\ttargetName = arg\n")
	electroweakCorrectionsFile.write("\t\t\tend if\n")
	electroweakCorrectionsFile.write("\t\tend if\n")
	electroweakCorrectionsFile.write("\tend do\n\n")

	electroweakCorrectionsFile.write("\t! Perform the numerical evaluation\n")
	electroweakCorrectionsFile.write('\t\tprint *, "Starting the numerical evaluation ..."\n\n')
	
	electroweakCorrectionsFile.write("\t\t! Reset all values\n")
	electroweakCorrectionsFile.write("\t\tGaugeXiA = 1D0\n")
	electroweakCorrectionsFile.write("\t\tGaugeXiW = 1D0\n")
	electroweakCorrectionsFile.write("\t\tGaugeXiZ = 1D0\n\n")
	
	electroweakCorrectionsFile.write("\t\t! Calculate all values\n")
	electroweakCorrectionsFile.write("\t\tcall ltini\n")
	electroweakCorrectionsFile.write("\t\t\t! Set default values for the loop calculations\n")
	electroweakCorrectionsFile.write("\t\t\tcall setlambda(1D0)\n")
	electroweakCorrectionsFile.write("\t\t\tcall setdelta(0D0)\n")
	electroweakCorrectionsFile.write("\t\t\tIRLambda = getlambda()\n\n")
	
	electroweakCorrectionsFile.write('\t\t\t! Use this hack to "fill up" the string to the maximum length with whitespace characters so that it can be passed to the subroutine call\n')
	electroweakCorrectionsFile.write("\t\t\tfileName = fileName // ' '\n")
	electroweakCorrectionsFile.write("\t\t\ttargetName = targetName // ' '\n\n")
	
	electroweakCorrectionsFile.write("\t\t\t! Get all parameters\n")
	electroweakCorrectionsFile.write('\t\t\tcall getParameters(' + str(OStype) + ')\n')
	electroweakCorrectionsFile.write("\t\t\tcall setmudim(InputScale**2)\n\n")
	
	electroweakCorrectionsFile.write("\t\t\t! Prepare the output file header\n")
	electroweakCorrectionsFile.write('\t\t\toutputFileContent = ""\n')
		
	electroweakCorrectionsFile.write('\t\t\t\t! Print out the current point in phase-space (debug mode only)\n')
	electroweakCorrectionsFile.write('\t\t\t\tif (debugModeOn) then\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "omitELCorr: ", omitELCorr\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MW: ", MW\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MZ: ", MZ\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "SW: ", SW\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CW: ", CW\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "alphaAtMZ: ", alphaAtMZ\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "EL: ", EL\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "vev: ", (2D0*MW*SW/EL)\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "ME: ", ME\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MM: ", MM\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "ML: ", ML\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MU: ", MU\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MD: ", MD\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MS: ", MS\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MC: ", MC\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MB: ", MB\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MT: ", MT\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM11: ", CKM11\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM12: ", CKM12\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM13: ", CKM13\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM21: ", CKM21\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM22: ", CKM22\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM23: ", CKM23\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM31: ", CKM31\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM32: ", CKM32\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKM33: ", CKM33\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC11: ", CKMC11\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC12: ", CKMC12\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC13: ", CKMC13\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC21: ", CKMC21\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC22: ", CKMC22\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC23: ", CKMC23\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC31: ", CKMC31\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC32: ", CKMC32\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CKMC33: ", CKMC33\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Mh0: ", Mh0\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MHH: ", MHH\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MA0: ", MA0\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "MHp: ", MHp\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "alpha: ", alpha\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "beta: ", beta\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "SA: ", SA\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CA: ", CA\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "TA: ", TA\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "SB: ", SB\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "CB: ", CB\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "TB: ", TB\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Yuk1: ", Yuk1\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Yuk2: ", Yuk2\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Yuk3: ", Yuk3\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Yuk4: ", Yuk4\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Yuk5: ", Yuk5\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "Yuk6: ", Yuk6\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "m12squared: ", m12squared\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "2HDM Type: ", TypeOf2HDM\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "InputScale: ", InputScale\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "ParamType: ", parameterType\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "RenormScheme: ", RenormScheme\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "hdecayLam1: ", hdecayLam1\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "hdecayLam2: ", hdecayLam2\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "hdecayLam3: ", hdecayLam3\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "hdecayLam4: ", hdecayLam4\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "hdecayLam5: ", hdecayLam5\n')
	electroweakCorrectionsFile.write('\t\t\t\tend if\n\n')
	
	for x in range(0, len(processDescriptionList)):
		starString = ""
		for y in range(0, (32 - len(processDescriptionList[x][1]))):
			starString += "*"
		additionalWhitespaces = ""
		for y in range(0, (longestName - len(processDescriptionList[x][0]))):
			additionalWhitespaces += " "
		symmetryFactor = processDescriptionList[x][5]
		electroweakCorrectionsFile.write('\t\t\t\t! PROCESS ' + processDescriptionList[x][1] + '\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t! Prepare the output file content\n')
		electroweakCorrectionsFile.write('\t\t\t\t\toutputFileContent = trim(outputFileContent) // "**************"\n')
		electroweakCorrectionsFile.write('\t\t\t\t\toutputFileContent = trim(outputFileContent) // " ' + processDescriptionList[x][1] + '"\n')
		electroweakCorrectionsFile.write('\t\t\t\t\toutputFileContent = trim(outputFileContent) // " (electroweak corrections) ' + starString + '\\n"\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t! Kinematic prefactor together with the symmetry factor of the process\n')
		electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "----- ' + processDescriptionList[x][1] + ' -----"\n')
		electroweakCorrectionsFile.write('\t\t\t\t\tm1 = ' + processDescriptionList[x][2] + '\n')
		electroweakCorrectionsFile.write('\t\t\t\t\tm2 = ' + processDescriptionList[x][3] + '\n')
		electroweakCorrectionsFile.write('\t\t\t\t\tm3 = ' + processDescriptionList[x][4] + '\n')
		electroweakCorrectionsFile.write('\t\t\t\t\tkinematicThreshold = m1**2 - (m2 + m3)**2\n')
		electroweakCorrectionsFile.write('\t\t\t\t\tif (omitELCorr .EQ. 1) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\twrite (*,*) "The electroweak corrections to the process ' + processDescriptionList[x][1] + ' are not calculated since OMIT ELW2 is set to 1."\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\ttreeLevelWidth = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Write the tree-level width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) treeLevelWidth\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'LO    ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tif (RenormScheme .EQ. 0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\telse if (RenormScheme .GT. maxNumberSchemes) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tm = RenormScheme\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tend if\n')
		electroweakCorrectionsFile.write('\t\t\t\t\telse if (m1 .LE. 0D0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\twrite (*,*) "The process ' + processDescriptionList[x][1] + ' has a massless particle in the initial state. A decay of massless&\n\t\t\t\t\t\t\t\t& particles is not supported. The LO and NLO widths are set to zero manually."\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\ttreeLevelWidth = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Write the tree-level width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) treeLevelWidth\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'LO    ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tif (RenormScheme .EQ. 0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\telse if (RenormScheme .GT. maxNumberSchemes) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tm = RenormScheme\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tend if\n')		
		electroweakCorrectionsFile.write('\t\t\t\t\telse if (kinematicThreshold .LT. 0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\twrite (*,*) "The process ' + processDescriptionList[x][1] + ' does not fulfill the kinematic threshold.&\n\t\t\t\t\t\t\t\t& The LO and NLO widths are set to zero manually."\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\ttreeLevelWidth = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Write the tree-level width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) treeLevelWidth\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'LO    ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tif (RenormScheme .EQ. 0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\telse if (RenormScheme .GT. maxNumberSchemes) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tm = RenormScheme\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tend if\n')
		electroweakCorrectionsFile.write('\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Kinematic prefactor\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tprefactor = 1D0/' + symmetryFactor + 'D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)\n\n')
		
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Calculate the NLO ingredients\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tcall clearcache\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\ttreeLevelTemp = ' + processDescriptionList[x][0] + 'Tree()\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tvertexCorrectionsTemp = ' + processDescriptionList[x][0] + 'VC()\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tvertexTadpolesTemp = ' + processDescriptionList[x][0] + 'Tad()\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\trealCorrectionsTemp = ' + processDescriptionList[x][0] + 'Real()\n\n')
		
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Get the full tree-level decay width\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\ttreeLevelWidth = prefactor*treeLevelTemp\n\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Write the tree-level width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) treeLevelWidth\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'LO    ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		
		electroweakCorrectionsFile.write('\t\t\t\t\t\t! Check if all schemes shall be calculated or only a specific one\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tif (RenormScheme .EQ. 0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t! Get the full NLO decay width for all schemes\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tcall clearcache\n\n')
		
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Schemes 1, 2, 9, 11 and 13 are without tadpoles\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tif ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tfullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\t\t\t& 2D0*' + processDescriptionList[x][0] + 'CT(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tcall clearcache\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tNLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tfullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\t\t\t& 2D0*' + processDescriptionList[x][0] + 'CT(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tcall clearcache\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tNLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tend if\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\telse if (RenormScheme .GT. maxNumberSchemes) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\twrite (*,*) "Invalid renormalization scheme. The chosen scheme number must be below the maximum&\n\t\t\t\t\t\t\t\t\t& number of schemes implemented. The widths are set to zero manually."\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\ttreeLevelWidth = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t! Write the tree-level width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) treeLevelWidth\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'LO    ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tNLOWidth(m) = 0D0\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tend do\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t! Get the full NLO decay width for the chosen scheme\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\tm = RenormScheme\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tcall clearcache\n\n')
		
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Schemes 1, 2, 9, 11 and 13 are without tadpoles\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tif ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tfullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\t\t\t& 2D0*' + processDescriptionList[x][0] + 'CT(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tcall clearcache\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tNLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tfullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\t\t\t& 2D0*' + processDescriptionList[x][0] + 'CT(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tcall clearcache\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\tNLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\tend if\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t! Write the NLO width to the output file\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal, '(ES23.15E3)' ) NLOWidth(m)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal2, '(I1)' ) m\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\twrite( tempVal3, '(I1)' ) (m-10)\n")
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tif (m .lt. 10) then\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // trim(tempVal2) // "  ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\telse\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // "' + processDescriptionList[x][0] + 'NLO" // "1" // trim(tempVal3) // " ' + additionalWhitespaces + '="\n')
		electroweakCorrectionsFile.write("\t\t\t\t\t\t\t\tend if\n")
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\t\toutputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\\n")\n')

		electroweakCorrectionsFile.write('\t\t\t\t\t\tend if\n')

		electroweakCorrectionsFile.write('\t\t\t\t\tend if\n\n')
		
		electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "TreeLevelWidth = ", treeLevelWidth\n')

		electroweakCorrectionsFile.write('\t\t\t\t\tif (RenormScheme .EQ. 0) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\twrite (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\telse if (RenormScheme .GT. maxNumberSchemes) then\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tdo m = 1, maxNumberSchemes, 1\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\twrite (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tend do\n')
		electroweakCorrectionsFile.write('\t\t\t\t\telse\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\tm = RenormScheme\n')
		electroweakCorrectionsFile.write('\t\t\t\t\t\t\twrite (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)\n')
		electroweakCorrectionsFile.write('\t\t\t\t\tend if\n')

		electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) "------------------------"\n')
		electroweakCorrectionsFile.write('\t\t\t\t\twrite (*,*) ""\n\n')
	
	electroweakCorrectionsFile.write('\t\t\t! Write the results to the output file\n')
	electroweakCorrectionsFile.write("\t\t\topen(unit=44, file=trim(pathToOutputFiles)//trim(targetName), status='old', &\n")
	electroweakCorrectionsFile.write("\t\t\t&action='write', position='append', iostat=statWrite)\n")
	electroweakCorrectionsFile.write('\t\t\t\tif ( statWrite == 0) then\n')
	electroweakCorrectionsFile.write('\t\t\t\t\twrite(44,*) trim(outputFileContent)\n')
	electroweakCorrectionsFile.write('\t\t\t\telse\n')
	electroweakCorrectionsFile.write("\t\t\t\t\twrite(*,*) 'ERROR: could not create output file for writing!'\n")
	electroweakCorrectionsFile.write('\t\t\t\tend if\n')
	electroweakCorrectionsFile.write('\t\t\tclose(unit=44)\n')
	electroweakCorrectionsFile.write('\t\tcall ltexi\n\n\n')
	
	electroweakCorrectionsFile.write('end program electroweakCorrections\n')
	
	electroweakCorrectionsFile.close()


#----------------------------#
#		 Main Program		 #
#----------------------------#
print("Starting the installation script.\n")

# Find the LoopTools install file and directory, if it exists already in the repo (per default, it does not)
filenameLoopTools = ''
for file in os.listdir('.'):
	if fnmatch(file, 'LoopTools-*.tar.gz'):
		filenameLoopTools = file
		loopToolsDirectory = filenameLoopTools.replace('.tar.gz', '')

# Ask the user whether LoopTools shall be installed
fileLoopToolsExists = os.path.isfile(filenameLoopTools)
if fileLoopToolsExists:
	loopToolsCreationWanted = CommonFunctions.queryBoolean("Found the LoopTools install file " + filenameLoopTools + ". Do you want me to install LoopTools automatically?\nWARNING: this will delete the current LoopTools instance installed under 2HDECAY/" + loopToolsDirectory + ", if it exists.")
else:
	# Ask for a download of the archive
	downloadLoopToolsWanted = CommonFunctions.queryBoolean("LoopTools-*.tar.gz file not found. Do you want me to download version " + Config.loopToolsVersion + " of LoopTools (if you want to change the version, modify Config.py and re-run setup.py)?")
	if downloadLoopToolsWanted:
		# Check for the used OS
		currentOS = sys.platform

		# Windows (cygwin) download routine
		if currentOS == 'win32':
			prompt = [Config.pathToCygwin, '-c', "wget http://www.feynarts.de/looptools/" + Config.loopToolsVersion + ".tar.gz"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		# Linux/macOS download routine
		else:
			prompt = ['bash', '-c', "wget http://www.feynarts.de/looptools/" + Config.loopToolsVersion + ".tar.gz"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		
		# Refresh the name of the installation file and directory
		filenameLoopTools = Config.loopToolsVersion + ".tar.gz"
		loopToolsDirectory = Config.loopToolsVersion

		loopToolsCreationWanted = CommonFunctions.queryBoolean("Found the LoopTools install file " + filenameLoopTools + ". Do you want me to install LoopTools automatically?\nWARNING: this will delete the current LoopTools instance installed under 2HDECAY/" + loopToolsDirectory + ", if it exists.")
	else:
		print('LoopTools will not be downloaded. WARNING: LoopTools cannot be build without the archive in the 2HDECAY folder.')
		loopToolsCreationWanted = False
# Start the LoopTools installation routine
if loopToolsCreationWanted:
	# Check for the used OS
	currentOS = sys.platform

	# Windows (cygwin) installation routine
	if currentOS == 'win32':
		print("\nStarting the Windows (cygwin) installation routine.")
		print("\nI will use the following path to Cygwin: " + Config.pathToCygwin)
		cygwinIsCorrectPath = CommonFunctions.queryBoolean("Is this the correct path to Cygwin on your machine?")
		if cygwinIsCorrectPath:
			if os.path.isdir(loopToolsDirectory):
				print("\nRemoving existing LoopTools installation...")
				rmtree(loopToolsDirectory)
			print("\nStarting the LoopTools installation routine...")
			# Unzip the archive
			prompt = [Config.pathToCygwin, '-c', "gunzip -c " + filenameLoopTools + " | tar xvf -"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
			# Change to the LoopTools folder and configure
			os.chdir(loopToolsDirectory)
			prompt = [Config.pathToCygwin, '-c', "./configure"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
			# Replace 'find' in the LoopTools makefile to '/bin/find' (this is necessary since otherwise, Windows' FIND is used instead of Cygwin's find!)
			os.rename('makefile', 'makefileTemp')
			with open("makefileTemp", "rt") as fin:
				with open("makefile", "wt") as fout:
					for line in fin:
						fout.write(line.replace('find', '/bin/find'))
			os.remove("makefileTemp")
			# Make the program
			prompt = [Config.pathToCygwin, '-c', "make lib"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
			prompt = [Config.pathToCygwin, '-c', "make install"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
			prompt = [Config.pathToCygwin, '-c', "make clean"]
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
			os.chdir('..')
		else:
			print("\nPlease enter the correct path to Cygwin in Config.py first and then re-run the setup.py script.")
			sys.exit()

	# Linux/macOS installation routine
	else:
		print("Starting the Linux/macOS installation routine.")
		# Remove the existing copy of LoopTools
		if os.path.isdir(loopToolsDirectory):
			print("\nRemoving existing LoopTools installation...")
			rmtree(loopToolsDirectory)
		# Unzip the archive
		prompt = ['bash', '-c', "gunzip -c " + filenameLoopTools + " | tar xvf -"]
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		# Change to the LoopTools folder and configure and make the program 
		os.chdir(loopToolsDirectory)
		prompt = ['bash', '-c', "./configure"]
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		prompt = ['bash', '-c', "make lib"]
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		prompt = ['bash', '-c', "make install"]
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		prompt = ['bash', '-c', "make clean"]
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		os.chdir('..')

	# Search for the correct folders within the LoopTools main folder that contains the libraries and executables
	for subdir in os.listdir(loopToolsDirectory):
		if os.path.isdir(loopToolsDirectory + os.sep + subdir):
			tempList = os.listdir(loopToolsDirectory + os.sep + subdir)
			if 'bin' in tempList and 'include' in tempList and 'lib' in tempList:
				loopToolsLibRootFolder = (loopToolsDirectory + os.sep + subdir).replace('\\', '/')
				loopToolsLibSubFolder = 'lib'
				loopToolsExecSubFolder = 'bin'
				break
			elif 'bin' in tempList and 'include' in tempList and 'lib64' in tempList:
				loopToolsLibRootFolder = (loopToolsDirectory + os.sep + subdir).replace('\\', '/')
				loopToolsLibSubFolder = 'lib64'
				loopToolsExecSubFolder = 'bin'
				break
			elif 'bin64' in tempList and 'include' in tempList and 'lib' in tempList:
				loopToolsLibRootFolder = (loopToolsDirectory + os.sep + subdir).replace('\\', '/')
				loopToolsLibSubFolder = 'lib'
				loopToolsExecSubFolder = 'bin64'
				break
			elif 'bin64' in tempList and 'include' in tempList and 'lib64' in tempList:
				loopToolsLibRootFolder = (loopToolsDirectory + os.sep + subdir).replace('\\', '/')
				loopToolsLibSubFolder = 'lib64'
				loopToolsExecSubFolder = 'bin64'
				break
	useRelativeLoopToolsPath = True
	
	# Save the correct folders to Config.py
	os.rename('Config.py', 'ConfigTemp.py')
	with open("ConfigTemp.py", "rt") as fin:
		with open("Config.py", "wt") as fout:
			for line in fin:
				if 'pathLoopToolsLibs' in line:
					tempLine = "pathLoopToolsLibs = '" + loopToolsLibSubFolder + "'\t\t\t\t\t\t\t\t\t# Specify the LoopTools subfolder (relative to pathLoopTools) where the LoopTools libraries are contained (NOTE: this depends on the OS and chip architecture; on Windows, this is normally 'lib', on Linux and macOS, it is normally 'lib64')\n"
					fout.write(tempLine)
				elif 'pathLoopToolsExecs' in line:
					tempLine = "pathLoopToolsExecs = '" + loopToolsExecSubFolder + "'\t\t\t\t\t\t\t\t\t# Specify the LoopTools subfolder (relative to pathLoopTools) where the LoopTools libraries are contained (NOTE: this depends on the OS and chip architecture; on Windows, this is normally 'lib', on Linux and macOS, it is normally 'lib64')\n"
					fout.write(tempLine)
				elif 'pathLoopTools' in line:
					tempLine = "pathLoopTools = '" + loopToolsLibRootFolder + "'\t# Specify the path to the LoopTools root folder (IMPORTANT: the path must never *end* with '/' and if useRelativePath is True, it must not *start* with '/' either! If useRelativePath is False, it depends on the OS if the full absolute path starts with '/' or not: on Windows, it typically does not, on Linux, it typically does)\n"
					fout.write(tempLine)
				elif 'useRelativeLoopToolsPath' in line:
					tempLine = "useRelativeLoopToolsPath = True\t\t\t\t\t\t\t\t# Set True if you want to set the path to LoopTools relative to the 2HDECAY installation path (useful if you installed LoopTools e.g. in a subdirectory of the 2HDECAY folder) or False if you want to use an absolute path to LoopTools\n"
					fout.write(tempLine)
				else:
					fout.write(line)
	os.remove('ConfigTemp.py')

	print('\nInstallation of LoopTools is finished.')

	# Ask whether the zip file should be deleted
	deleteLoopToolsZip = CommonFunctions.queryBoolean("Do you want to remove the zip archive of LoopTools now (not needed anymore after successful installation)?")
	if deleteLoopToolsZip:
		os.remove(filenameLoopTools)
else:
	# If LoopTools is not installed automatically, then the paths are read from Config.py
	print('\nLoopTools is not installed automatically. Please make sure now that the root path to LoopTools and the relative paths to the library and binary folder are set correctly in Config.py. If you use absolute paths, please make sure to set "useRelativeLoopToolsPath = False" in Config.py.')
	continueWithManualInstallation = CommonFunctions.queryBoolean("\nAre all paths set correctly in Config.py?")
	if continueWithManualInstallation:
		useRelativeLoopToolsPath = Config.useRelativeLoopToolsPath
		loopToolsLibRootFolder = Config.pathLoopTools
		loopToolsLibSubFolder = Config.pathLoopToolsLibs
		loopToolsExecSubFolder = Config.pathLoopToolsExecs
	else:
		print('\nPlease correct the paths in Config.py manually and re-run the setup.py script.')
		sys.exit()

# Ask the user if the makefile should be created (if it already exists, ask if it should be re-created)
makefileExists = os.path.isfile("makefile")
if makefileExists:
	makefileCreationWanted = CommonFunctions.queryBoolean("\nmakefile already exists. Do you want to recreate it (WARNING: this overwrites the existing makefile)?")
else:
	makefileCreationWanted = CommonFunctions.queryBoolean("\nmakefile not found. Do you want to create it now?")
if makefileCreationWanted:
	print('  Creating makefile...')
	createMakefile("makefile", loopToolsLibRootFolder, loopToolsLibSubFolder, loopToolsExecSubFolder, useRelativeLoopToolsPath, 'gfortran')
	print('    done.\n')
else:
	print('  makefile is not created.\n')
makefileExistsNow = os.path.isfile("makefile")

# Ask the user if the electroweak corrections should be created 
ewFileExists = os.path.isfile("electroweakCorrections.F90")
if ewFileExists:
	ewFileCreationWanted = CommonFunctions.queryBoolean("electroweakCorrections.F90 already exists. Do you want to recreate it (WARNING: this overwrites the existing electroweakCorrections.F90)?")
else:
	ewFileCreationWanted = CommonFunctions.queryBoolean("electroweakCorrections.F90 not found. Do you want to create it now?")
if ewFileCreationWanted:
	print('  Creating electroweakCorrections.F90...')
	createElectroweakCorrections()
	print('    done.\n')
else:
	print('  electroweakCorrections.F90 is not created.\n')
ewFileExistsNow = os.path.isfile("electroweakCorrections.F90")

# Check if the necessary files are present; if not, end the program
missingFiles = False
if not makefileExistsNow:
	print('Error: makefile not found!')
	missingFiles = True
if not ewFileExistsNow:
	print('Error: electroweakCorrections.F90 not found!')
	missingFiles = True
if missingFiles:
	print('\nError: major components of 2HDECAY missing. Terminating 2HDECAY now.')
	sys.exit()

# Make the program 
makeWanted = CommonFunctions.queryBoolean("Do you want to make the program now (the make process may take a few minutes)?")
if makeWanted:
	print('Making main program 2HDECAY...\n')
	try:
		subprocess.check_output('make')
		print('\nMaking process of main program 2HDECAY terminated.\n')
	except subprocess.CalledProcessError as e:
		print(e.output)

	print('Making subprogram HDECAY...\n')
	try:
		os.chdir('HDECAY')
		subprocess.check_output('make')
		print('\nMaking process of subprogram HDECAY terminated.\n')
		os.chdir('..')
	except subprocess.CalledProcessError as e:
		print(e.output)
else:
	print('Make process skipped.\n')

# Cleaning the installation
cleanWanted = CommonFunctions.queryBoolean("Do you want to make clean (optional)?")
if cleanWanted:
	print('Cleaning main program 2HDECAY...\n')
	try:
		subprocess.check_output('make clean')
		print('\nCleaning process terminated.\n')
	except subprocess.CalledProcessError as e:
		print(e.output)

	print('Cleaning subprogram HDECAY...\n')
	try:
		os.chdir('HDECAY')
		subprocess.check_output('make clean')
		print('\nCleaning process terminated.\n')
		os.chdir('..')
	except subprocess.CalledProcessError as e:
		print(e.output)
else:
	print('Make clean process skipped.\n')

print("Setup completed.\n")
