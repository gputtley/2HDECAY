#!/usr/bin/env python
#Filename: CommonFunctions.py


 ###################################################################################
#																					#
#								CommonFunctions 									#
#																					#
#	Purpose:	Function library for 2HDMCalc. Contains often used functions.		#
#																					#
 ###################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import sys
import os
import errno

#-------------------------#
#		 Functions		  #
#-------------------------#
def queryBoolean(question):
	'''
		For a given yes/no question, check the validity of the answer and return the corresponding Boolean value.
	'''
	validTrue = {"yes": True, "y": True, "ye": True, "j": True, "ja": True, "1": True}
	validFalse = {"no": False, "n": False, "nein": False, "0": False}
	prompt = " [y/n] "

	while True:
		sys.stdout.write(question + prompt)
		choice = input().lower()
		if choice in validTrue:
			return True
		elif choice in validFalse:
			return False
		else:
			sys.stdout.write('Error: invalid input. Enter "y" or "n".\n\n')
