#!/usr/bin/env python
#Filename: setup.py


 ###################################################################################
#																					#
#									setup.py 										#
#																					#
#	Purpose:	* Sets up 2HDECAY 													#
#				* Calls the LoopTools installer										#
#				* Creates the makefile												#
#				* Makes the program													#
#																					#
 ###################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import os
import subprocess
import sys
import CommonFunctions			# Provides common, often used functions for different scripts of 2HDECAY

#-------------------------#
#		 Functions		  #
#-------------------------#


def createMakefile(pathToMakefile, relativePathToLoopTools, relativePathToLoopToolsLibs, useRelativePath, chosenCompiler):
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
	makefile.write("LTCOMP = $(LT)/bin/fcc\n")
	makefile.write("IFlags = -I$(LT)/include\n")
	makefile.write("LFlags = -L$(LT)/" + relativePathToLoopToolsLibs + " -looptools\n\n")
	makefile.write("# Choose your compiler:\n")
	makefile.write("FCOMP = " + chosenCompiler + "\n\n")
	makefile.write("# Do NOT change anything below this line by hand!\n")
	makefile.write("SELFENERGIESUSU = BuildingBlocks/SelfEnergies/Usual\n")
	makefile.write("SELFENERGIESALT = BuildingBlocks/SelfEnergies/Alternative\n")
	makefile.write("SELFENERGIESDERIV = BuildingBlocks/SelfEnergiesDerivatives\n")
	makefile.write("PROCESSDEPENDENTSCHEME = BuildingBlocks/ProcessDependentScheme\n")
	makefile.write("PARAMETERS = Parameters\n")
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
	makefile.write("\n$(PARAMETERS)/%.o: $(PARAMETERS)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("electroweakCorrections: constants.o $(SELFENERGIESUSU)/SelfAA.o $(SELFENERGIESUSU)/SelfAALight.o $(SELFENERGIESUSU)/SelfWpWp.o $(SELFENERGIESUSU)/SelfZ0Z0.o \\\n")
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
	makefile.write("\tcounterterms.o $(PARAMETERS)/getParameters.o electroweakCorrections.o\n")
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
	makefile.write("\tcounterterms.o $(PARAMETERS)/getParameters.o electroweakCorrections.o $(LFlags) -o electroweakCorrections" + applicationEnding + "\n\n")
	makefile.write("clean:\n")
	makefile.write("\trm -f *.o\n")
	makefile.close()

#----------------------------#
#		 Main Program		 #
#----------------------------#
print("Starting the installation script.\n")

# TEMP - Config 
useRelativeLoopToolsPath = True
pathLoopTools = 'LoopTools-2.12/i686-CYGWIN_NT-6.1-WOW'
pathLoopToolsLibs = 'lib'
yourChosenCompiler = 'gfortran'

# Ask the user if the makefile should be created 
makefileCreationWanted = CommonFunctions.queryBoolean("Do you want to create the makefile?")
if makefileCreationWanted:
	print('  Creating makefile...')
	createMakefile("makefile", pathLoopTools, pathLoopToolsLibs, useRelativeLoopToolsPath, yourChosenCompiler)
	print('    done.\n')
else:
	print('  makefile is not created.\n')

# Calculate the vertex corrections used for the process-dependent scheme of alpha and beta
# prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepAlpha2FA.m"'
# subprocess.call(prompt)


print("Installation finished.\n")
