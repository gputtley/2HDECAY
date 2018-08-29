#!/usr/bin/env python
#Filename: Config.py


 ###################################################################################
#																					#
#								Configuration 										#
#																					#
#	Purpose:	Main configuration file of 2HDECAY. Contains all configuration		#
#				settings needed to change the program.								#
#																					#
 ###################################################################################


#---------------------#
#		Shared		  #
#---------------------#
useRelativeLoopToolsPath = True								# Set True if you want to set the path to LoopTools relative to the 2HDMCalc installation path (useful if you installed LoopTools e.g. in a subdirectory of the 2HDMCalc folder) or False if you want to use an absolute path to LoopTools
pathLoopTools = 'LoopTools-2.12/i686-CYGWIN_NT-10.0-WOW'	# Specify the path to the LoopTools root folder (IMPORTANT: the path must never *end* with '/' and if useRelativePath is True, it must not *start* with '/' either! If useRelativePath is False, it depends on the OS if the full absolute path starts with '/' or not: on Windows, it typically does not, on Linux, it typically does)
pathLoopToolsLibs = 'lib'									# Specify the LoopTools subfolder (relative to pathLoopTools) where the LoopTools libraries are contained (NOTE: this depends on the OS and chip architecture; on Windows, this is normally 'lib', on Linux and macOS, it is normally 'lib64')
pathLoopToolsExecs = 'bin'									# Specify the LoopTools subfolder (relative to pathLoopTools) where the LoopTools libraries are contained (NOTE: this depends on the OS and chip architecture; on Windows, this is normally 'lib', on Linux and macOS, it is normally 'lib64')
pathToCygwin = 'C:\\cygwin\\bin\\bash.exe'                  # Specify the path to the Cygwin bash executable (for Windows only)
loopToolsVersion = 'LoopTools-2.14'                         # Specify the LoopTools version that shall be downloaded (recommended, as checked for compatibility: LoopTools-2.14)