program electroweakCorrections
	use constants
	use counterterms
	implicit none
#include "looptools.h"
	character(len=26) :: tempVal
	character(len=2) :: tempVal2, tempVal3
	character(len=32) :: arg
	character(len=50) :: fileName, fileNameFilled, targetName
	character(len=600000) :: outputFileContent
	character(300), parameter :: pathToOutputFiles = 'Results\\'
	integer arguments(5)
	integer, parameter :: maxNumberSchemes = 14
	logical :: debugModeOn = .true.
	double precision prefactor, treeLevelWidth, NLOWidth(maxNumberSchemes), fullamplitude(maxNumberSchemes)
	double precision NLOVCwidth, NLOVCwoIRwidth, NLOIRonlywidth
	double precision h0toBBBarTree, h0toBBBarCT, h0toBBBarReal
	double complex h0toBBBarVC, h0toBBBarTad
	double precision h0toTauTauBarTree, h0toTauTauBarCT, h0toTauTauBarReal
	double complex h0toTauTauBarVC, h0toTauTauBarTad
	double precision h0toMuMuBarTree, h0toMuMuBarCT, h0toMuMuBarReal
	double complex h0toMuMuBarVC, h0toMuMuBarTad
	double precision h0toSSBarTree, h0toSSBarCT, h0toSSBarReal
	double complex h0toSSBarVC, h0toSSBarTad
	double precision h0toCCBarTree, h0toCCBarCT, h0toCCBarReal
	double complex h0toCCBarVC, h0toCCBarTad
	double precision h0toTTBarTree, h0toTTBarCT, h0toTTBarReal
	double complex h0toTTBarVC, h0toTTBarTad
	double precision h0toWmWpTree, h0toWmWpCT, h0toWmWpReal
	double complex h0toWmWpVC, h0toWmWpTad
	double precision h0toZ0Z0Tree, h0toZ0Z0CT, h0toZ0Z0Real
	double complex h0toZ0Z0VC, h0toZ0Z0Tad
	double precision h0toHHHHTree, h0toHHHHCT, h0toHHHHReal
	double complex h0toHHHHVC, h0toHHHHTad
	double precision h0toA0A0Tree, h0toA0A0CT, h0toA0A0Real
	double complex h0toA0A0VC, h0toA0A0Tad
	double precision h0toA0Z0Tree, h0toA0Z0CT, h0toA0Z0Real
	double complex h0toA0Z0VC, h0toA0Z0Tad
	double precision h0toHpWmTree, h0toHpWmCT, h0toHpWmReal
	double complex h0toHpWmVC, h0toHpWmTad
	double precision h0toHmHpTree, h0toHmHpCT, h0toHmHpReal
	double complex h0toHmHpVC, h0toHmHpTad
	double precision HHtoBBBarTree, HHtoBBBarCT, HHtoBBBarReal
	double complex HHtoBBBarVC, HHtoBBBarTad
	double precision HHtoTauTauBarTree, HHtoTauTauBarCT, HHtoTauTauBarReal
	double complex HHtoTauTauBarVC, HHtoTauTauBarTad
	double precision HHtoMuMuBarTree, HHtoMuMuBarCT, HHtoMuMuBarReal
	double complex HHtoMuMuBarVC, HHtoMuMuBarTad
	double precision HHtoSSBarTree, HHtoSSBarCT, HHtoSSBarReal
	double complex HHtoSSBarVC, HHtoSSBarTad
	double precision HHtoCCBarTree, HHtoCCBarCT, HHtoCCBarReal
	double complex HHtoCCBarVC, HHtoCCBarTad
	double precision HHtoTTBarTree, HHtoTTBarCT, HHtoTTBarReal
	double complex HHtoTTBarVC, HHtoTTBarTad
	double precision HHtoWmWpTree, HHtoWmWpCT, HHtoWmWpReal
	double complex HHtoWmWpVC, HHtoWmWpTad
	double precision HHtoZ0Z0Tree, HHtoZ0Z0CT, HHtoZ0Z0Real
	double complex HHtoZ0Z0VC, HHtoZ0Z0Tad
	double precision HHtoh0h0Tree, HHtoh0h0CT, HHtoh0h0Real
	double complex HHtoh0h0VC, HHtoh0h0Tad
	double precision HHtoA0A0Tree, HHtoA0A0CT, HHtoA0A0Real
	double complex HHtoA0A0VC, HHtoA0A0Tad
	double precision HHtoA0Z0Tree, HHtoA0Z0CT, HHtoA0Z0Real
	double complex HHtoA0Z0VC, HHtoA0Z0Tad
	double precision HHtoHpWmTree, HHtoHpWmCT, HHtoHpWmReal
	double complex HHtoHpWmVC, HHtoHpWmTad
	double precision HHtoHmHpTree, HHtoHmHpCT, HHtoHmHpReal
	double complex HHtoHmHpVC, HHtoHmHpTad
	double precision A0toBBBarTree, A0toBBBarCT, A0toBBBarReal
	double complex A0toBBBarVC, A0toBBBarTad
	double precision A0toTauTauBarTree, A0toTauTauBarCT, A0toTauTauBarReal
	double complex A0toTauTauBarVC, A0toTauTauBarTad
	double precision A0toMuMuBarTree, A0toMuMuBarCT, A0toMuMuBarReal
	double complex A0toMuMuBarVC, A0toMuMuBarTad
	double precision A0toSSBarTree, A0toSSBarCT, A0toSSBarReal
	double complex A0toSSBarVC, A0toSSBarTad
	double precision A0toCCBarTree, A0toCCBarCT, A0toCCBarReal
	double complex A0toCCBarVC, A0toCCBarTad
	double precision A0toTTBarTree, A0toTTBarCT, A0toTTBarReal
	double complex A0toTTBarVC, A0toTTBarTad
	double precision A0toZ0h0Tree, A0toZ0h0CT, A0toZ0h0Real
	double complex A0toZ0h0VC, A0toZ0h0Tad
	double precision A0toHHZ0Tree, A0toHHZ0CT, A0toHHZ0Real
	double complex A0toHHZ0VC, A0toHHZ0Tad
	double precision A0toHmWpTree, A0toHmWpCT, A0toHmWpReal
	double complex A0toHmWpVC, A0toHmWpTad
	double precision HptoBBarCTree, HptoBBarCCT, HptoBBarCReal
	double complex HptoBBarCVC, HptoBBarCTad
	double precision HptoNeuTTauBarTree, HptoNeuTTauBarCT, HptoNeuTTauBarReal
	double complex HptoNeuTTauBarVC, HptoNeuTTauBarTad
	double precision HptoMuBarNeuMTree, HptoMuBarNeuMCT, HptoMuBarNeuMReal
	double complex HptoMuBarNeuMVC, HptoMuBarNeuMTad
	double precision HptoSBarUTree, HptoSBarUCT, HptoSBarUReal
	double complex HptoSBarUVC, HptoSBarUTad
	double precision HptoCSBarTree, HptoCSBarCT, HptoCSBarReal
	double complex HptoCSBarVC, HptoCSBarTad
	double precision HptoBBarTTree, HptoBBarTCT, HptoBBarTReal
	double complex HptoBBarTVC, HptoBBarTTad
	double precision HptoCDBarTree, HptoCDBarCT, HptoCDBarReal
	double complex HptoCDBarVC, HptoCDBarTad
	double precision HptoBBarUTree, HptoBBarUCT, HptoBBarUReal
	double complex HptoBBarUVC, HptoBBarUTad
	double precision HptoSBarTTree, HptoSBarTCT, HptoSBarTReal
	double complex HptoSBarTVC, HptoSBarTTad
	double precision HptoDBarTTree, HptoDBarTCT, HptoDBarTReal
	double complex HptoDBarTVC, HptoDBarTTad
	double precision HptoWph0Tree, HptoWph0CT, HptoWph0Real
	double complex HptoWph0VC, HptoWph0Tad
	double precision HptoHHWpTree, HptoHHWpCT, HptoHHWpReal
	double complex HptoHHWpVC, HptoHHWpTad
	double precision HptoA0WpTree, HptoA0WpCT, HptoA0WpReal
	double complex HptoA0WpVC, HptoA0WpTad
	double precision treeLevelTemp, realCorrectionsTemp
	double complex vertexCorrectionsTemp, vertexTadpolesTemp
	double precision m1, m2, m3, kinematicThreshold
	integer m, n, o, p, q, r, fileNameLength, point, statWrite

	! Get the command line arguments standing for the different running options
	! Argument 1: perform UV divergence check (1: true, 0: false; default: 0)
	! Argument 2: perform IR divergence check (1: true, 0: false; default: 0)
	! Argument 3: perform gauge dependence check (2: true (prompt for continuation of program if gauge-dependence is detected), 1: true (no prompts for continuation), 0: false; default: 0)
	! Argument 4: perform numerical evaluation (1: true, 0: false; default: 1)
	! Argument 5: relative path to the 2HDM input parameter file, starting from the Parameters directory of 2HDMCalc
	! Argument 6: relative path to the target file containing the results of the calculation, starting from the Temp/Results directory of 2HDMCalc
	do o = 1, iargc()
		call getarg(o, arg)
		if (arg == '1') then
			arguments(o) = 1
		else if (arg == '2') then
			arguments(o) = 2
		else if (arg == '0') then
			arguments(o) = 0
		else
			if (o == 5) then
				fileName = arg
			else if (o == 6) then
				targetName = arg
			end if
		end if
	end do

				! TEMPORARY 
				targetName = "hdecay.in"
	! Perform the numerical evaluation
		print *, "Starting the numerical evaluation ..."

		! Reset all values
		GaugeXiA = 1D0
		GaugeXiW = 1D0
		GaugeXiZ = 1D0

		! Calculate all values
		call ltini
			! Set default values for the loop calculations
			call setlambda(1D0)
			call setdelta(0D0)
			IRLambda = getlambda()

			! Use this hack to "fill up" the string to the maximum length with whitespace characters so that it can be passed to the subroutine call
			fileName = fileName // ' '
			targetName = targetName // ' '

			! Get all parameters
			call getParameters(fileName)
			call setmudim(InputScale**2)

			! Prepare the output file header
			outputFileContent = ""
				! Print out the current point in phase-space (debug mode only)
				if (debugModeOn) then
					write (*,*) "omitELCorr: ", omitELCorr
					write (*,*) "MW: ", MW
					write (*,*) "MZ: ", MZ
					write (*,*) "SW: ", SW
					write (*,*) "CW: ", CW
					write (*,*) "alphaAtMZ: ", alphaAtMZ
					write (*,*) "EL: ", EL
					write (*,*) "vev: ", (2D0*MW*SW/EL)
					write (*,*) "ME: ", ME
					write (*,*) "MM: ", MM
					write (*,*) "ML: ", ML
					write (*,*) "MU: ", MU
					write (*,*) "MD: ", MD
					write (*,*) "MS: ", MS
					write (*,*) "MC: ", MC
					write (*,*) "MB: ", MB
					write (*,*) "MT: ", MT
					write (*,*) "CKM11: ", CKM11
					write (*,*) "CKM12: ", CKM12
					write (*,*) "CKM13: ", CKM13
					write (*,*) "CKM21: ", CKM21
					write (*,*) "CKM22: ", CKM22
					write (*,*) "CKM23: ", CKM23
					write (*,*) "CKM31: ", CKM31
					write (*,*) "CKM32: ", CKM32
					write (*,*) "CKM33: ", CKM33
					write (*,*) "CKMC11: ", CKMC11
					write (*,*) "CKMC12: ", CKMC12
					write (*,*) "CKMC13: ", CKMC13
					write (*,*) "CKMC21: ", CKMC21
					write (*,*) "CKMC22: ", CKMC22
					write (*,*) "CKMC23: ", CKMC23
					write (*,*) "CKMC31: ", CKMC31
					write (*,*) "CKMC32: ", CKMC32
					write (*,*) "CKMC33: ", CKMC33
					write (*,*) "Mh0: ", Mh0
					write (*,*) "MHH: ", MHH
					write (*,*) "MA0: ", MA0
					write (*,*) "MHp: ", MHp
					write (*,*) "alpha: ", alpha
					write (*,*) "beta: ", beta
					write (*,*) "SA: ", SA
					write (*,*) "CA: ", CA
					write (*,*) "TA: ", TA
					write (*,*) "SB: ", SB
					write (*,*) "CB: ", CB
					write (*,*) "TB: ", TB
					write (*,*) "Yuk1: ", Yuk1
					write (*,*) "Yuk2: ", Yuk2
					write (*,*) "Yuk3: ", Yuk3
					write (*,*) "m12squared: ", m12squared
					write (*,*) "2HDM Type: ", TypeOf2HDM
					write (*,*) "InputScale: ", InputScale
					write (*,*) "ParamType: ", parameterType
					write (*,*) "RenormScheme: ", RenormScheme
					write (*,*) "hdecayLam1: ", hdecayLam1
					write (*,*) "hdecayLam2: ", hdecayLam2
					write (*,*) "hdecayLam3: ", hdecayLam3
					write (*,*) "hdecayLam4: ", hdecayLam4
					write (*,*) "hdecayLam5: ", hdecayLam5
				end if

				! PROCESS h -> bb
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> bb"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> bb -----"
					m1 = Mh0
					m2 = MB
					m3 = MB
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> bb are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> bb has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> bb does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toBBBarTree()
						vertexCorrectionsTemp = h0toBBBarVC()
						vertexTadpolesTemp = h0toBBBarTad()
						realCorrectionsTemp = h0toBBBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toBBBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toBBBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> tau tau
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> tau tau"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> tau tau -----"
					m1 = Mh0
					m2 = ML
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> tau tau are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> tau tau has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> tau tau does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toTauTauBarTree()
						vertexCorrectionsTemp = h0toTauTauBarVC()
						vertexTadpolesTemp = h0toTauTauBarTad()
						realCorrectionsTemp = h0toTauTauBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toTauTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toTauTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> mu mu
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> mu mu"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> mu mu -----"
					m1 = Mh0
					m2 = MM
					m3 = MM
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> mu mu are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> mu mu has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> mu mu does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toMuMuBarTree()
						vertexCorrectionsTemp = h0toMuMuBarVC()
						vertexTadpolesTemp = h0toMuMuBarTad()
						realCorrectionsTemp = h0toMuMuBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toMuMuBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toMuMuBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "h0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> ss
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> ss"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> ss -----"
					m1 = Mh0
					m2 = MS
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> ss are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> ss has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> ss does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toSSBarTree()
						vertexCorrectionsTemp = h0toSSBarVC()
						vertexTadpolesTemp = h0toSSBarTad()
						realCorrectionsTemp = h0toSSBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toSSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toSSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> cc
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> cc"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> cc -----"
					m1 = Mh0
					m2 = MC
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> cc are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> cc has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> cc does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toCCBarTree()
						vertexCorrectionsTemp = h0toCCBarVC()
						vertexTadpolesTemp = h0toCCBarTad()
						realCorrectionsTemp = h0toCCBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toCCBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toCCBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> tt
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> tt"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> tt -----"
					m1 = Mh0
					m2 = MT
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> tt are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> tt has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> tt does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toTTBarTree()
						vertexCorrectionsTemp = h0toTTBarVC()
						vertexTadpolesTemp = h0toTTBarTad()
						realCorrectionsTemp = h0toTTBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toTTBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toTTBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "h0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> WW
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> WW"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> WW -----"
					m1 = Mh0
					m2 = MW
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> WW are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> WW has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> WW does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toWmWpTree()
						vertexCorrectionsTemp = h0toWmWpVC()
						vertexTadpolesTemp = h0toWmWpTad()
						realCorrectionsTemp = h0toWmWpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toWmWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toWmWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> ZZ
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> ZZ"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> ZZ -----"
					m1 = Mh0
					m2 = MZ
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> ZZ are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> ZZ has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> ZZ does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/2D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toZ0Z0Tree()
						vertexCorrectionsTemp = h0toZ0Z0VC()
						vertexTadpolesTemp = h0toZ0Z0Tad()
						realCorrectionsTemp = h0toZ0Z0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toZ0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toZ0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> HH
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> HH"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> HH -----"
					m1 = Mh0
					m2 = MHH
					m3 = MHH
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> HH are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHHHHLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> HH has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHHHHLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> HH does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHHHHLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/2D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toHHHHTree()
						vertexCorrectionsTemp = h0toHHHHVC()
						vertexTadpolesTemp = h0toHHHHTad()
						realCorrectionsTemp = h0toHHHHReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHHHHLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toHHHHCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toHHHHCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHHHHNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> AA
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> AA"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> AA -----"
					m1 = Mh0
					m2 = MA0
					m3 = MA0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> AA are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> AA has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> AA does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/2D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toA0A0Tree()
						vertexCorrectionsTemp = h0toA0A0VC()
						vertexTadpolesTemp = h0toA0A0Tad()
						realCorrectionsTemp = h0toA0A0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toA0A0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toA0A0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> ZA
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> ZA"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> ZA -----"
					m1 = Mh0
					m2 = MA0
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> ZA are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> ZA has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> ZA does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toA0Z0Tree()
						vertexCorrectionsTemp = h0toA0Z0VC()
						vertexTadpolesTemp = h0toA0Z0Tad()
						realCorrectionsTemp = h0toA0Z0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toA0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toA0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> H+ W-
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> H+ W-"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> H+ W- -----"
					m1 = Mh0
					m2 = MHp
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> H+ W- are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> H+ W- has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> H+ W- does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toHpWmTree()
						vertexCorrectionsTemp = h0toHpWmVC()
						vertexTadpolesTemp = h0toHpWmTad()
						realCorrectionsTemp = h0toHpWmReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toHpWmCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toHpWmCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h -> H+ H-
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " h -> H+ H-"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h -> H+ H- -----"
					m1 = Mh0
					m2 = MHp
					m3 = MHp
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process h -> H+ H- are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process h -> H+ H- has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h -> H+ H- does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toHmHpTree()
						vertexCorrectionsTemp = h0toHmHpVC()
						vertexTadpolesTemp = h0toHmHpTad()
						realCorrectionsTemp = h0toHmHpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "h0toHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toHmHpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toHmHpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "h0toHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> bb
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> bb"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> bb -----"
					m1 = MHH
					m2 = MB
					m3 = MB
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> bb are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> bb has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> bb does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoBBBarTree()
						vertexCorrectionsTemp = HHtoBBBarVC()
						vertexTadpolesTemp = HHtoBBBarTad()
						realCorrectionsTemp = HHtoBBBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoBBBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoBBBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> tautau
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> tautau"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> tautau -----"
					m1 = MHH
					m2 = ML
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> tautau are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> tautau has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> tautau does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoTauTauBarTree()
						vertexCorrectionsTemp = HHtoTauTauBarVC()
						vertexTadpolesTemp = HHtoTauTauBarTad()
						realCorrectionsTemp = HHtoTauTauBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoTauTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoTauTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> mu mu
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> mu mu"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> mu mu -----"
					m1 = MHH
					m2 = MM
					m3 = MM
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> mu mu are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> mu mu has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> mu mu does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoMuMuBarTree()
						vertexCorrectionsTemp = HHtoMuMuBarVC()
						vertexTadpolesTemp = HHtoMuMuBarTad()
						realCorrectionsTemp = HHtoMuMuBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoMuMuBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoMuMuBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> ss
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> ss"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> ss -----"
					m1 = MHH
					m2 = MS
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> ss are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> ss has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> ss does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoSSBarTree()
						vertexCorrectionsTemp = HHtoSSBarVC()
						vertexTadpolesTemp = HHtoSSBarTad()
						realCorrectionsTemp = HHtoSSBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoSSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoSSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> cc
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> cc"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> cc -----"
					m1 = MHH
					m2 = MC
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> cc are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> cc has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> cc does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoCCBarTree()
						vertexCorrectionsTemp = HHtoCCBarVC()
						vertexTadpolesTemp = HHtoCCBarTad()
						realCorrectionsTemp = HHtoCCBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoCCBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoCCBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> tt
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> tt"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> tt -----"
					m1 = MHH
					m2 = MT
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> tt are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> tt has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> tt does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoTTBarTree()
						vertexCorrectionsTemp = HHtoTTBarVC()
						vertexTadpolesTemp = HHtoTTBarTad()
						realCorrectionsTemp = HHtoTTBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoTTBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoTTBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> WW
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> WW"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> WW -----"
					m1 = MHH
					m2 = MW
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> WW are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> WW has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> WW does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoWmWpTree()
						vertexCorrectionsTemp = HHtoWmWpVC()
						vertexTadpolesTemp = HHtoWmWpTad()
						realCorrectionsTemp = HHtoWmWpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoWmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoWmWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoWmWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoWmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> ZZ
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> ZZ"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> ZZ -----"
					m1 = MHH
					m2 = MZ
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> ZZ are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> ZZ has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> ZZ does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/2D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoZ0Z0Tree()
						vertexCorrectionsTemp = HHtoZ0Z0VC()
						vertexTadpolesTemp = HHtoZ0Z0Tad()
						realCorrectionsTemp = HHtoZ0Z0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoZ0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoZ0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoZ0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoZ0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> hh
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> hh"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> hh -----"
					m1 = MHH
					m2 = Mh0
					m3 = Mh0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> hh are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoh0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> hh has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoh0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> hh does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoh0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/2D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoh0h0Tree()
						vertexCorrectionsTemp = HHtoh0h0VC()
						vertexTadpolesTemp = HHtoh0h0Tad()
						realCorrectionsTemp = HHtoh0h0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoh0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoh0h0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoh0h0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoh0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> AA
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> AA"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> AA -----"
					m1 = MHH
					m2 = MA0
					m3 = MA0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> AA are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> AA has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> AA does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/2D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoA0A0Tree()
						vertexCorrectionsTemp = HHtoA0A0VC()
						vertexTadpolesTemp = HHtoA0A0Tad()
						realCorrectionsTemp = HHtoA0A0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0A0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoA0A0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoA0A0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0A0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> ZA
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> ZA"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> ZA -----"
					m1 = MHH
					m2 = MA0
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> ZA are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> ZA has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> ZA does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoA0Z0Tree()
						vertexCorrectionsTemp = HHtoA0Z0VC()
						vertexTadpolesTemp = HHtoA0Z0Tad()
						realCorrectionsTemp = HHtoA0Z0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoA0Z0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoA0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoA0Z0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoA0Z0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> H+ W-
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> H+ W-"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> H+ W- -----"
					m1 = MHH
					m2 = MHp
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> H+ W- are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> H+ W- has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> H+ W- does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoHpWmTree()
						vertexCorrectionsTemp = HHtoHpWmVC()
						vertexTadpolesTemp = HHtoHpWmTad()
						realCorrectionsTemp = HHtoHpWmReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHpWmLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoHpWmCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoHpWmCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHpWmNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H -> H+ H-
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H -> H+ H-"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H -> H+ H- -----"
					m1 = MHH
					m2 = MHp
					m3 = MHp
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H -> H+ H- are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H -> H+ H- has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H -> H+ H- does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoHmHpTree()
						vertexCorrectionsTemp = HHtoHmHpVC()
						vertexTadpolesTemp = HHtoHmHpTad()
						realCorrectionsTemp = HHtoHmHpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HHtoHmHpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoHmHpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoHmHpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HHtoHmHpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> bb
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> bb"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> bb -----"
					m1 = MA0
					m2 = MB
					m3 = MB
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> bb are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> bb has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> bb does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toBBBarTree()
						vertexCorrectionsTemp = A0toBBBarVC()
						vertexTadpolesTemp = A0toBBBarTad()
						realCorrectionsTemp = A0toBBBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toBBBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toBBBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toBBBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toBBBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> tau,tau
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> tau,tau"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> tau,tau -----"
					m1 = MA0
					m2 = ML
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> tau,tau are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> tau,tau has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> tau,tau does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toTauTauBarTree()
						vertexCorrectionsTemp = A0toTauTauBarVC()
						vertexTadpolesTemp = A0toTauTauBarTad()
						realCorrectionsTemp = A0toTauTauBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTauTauBarLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toTauTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toTauTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTauTauBarNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> mu,mu
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> mu,mu"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> mu,mu -----"
					m1 = MA0
					m2 = MM
					m3 = MM
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> mu,mu are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> mu,mu has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> mu,mu does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toMuMuBarTree()
						vertexCorrectionsTemp = A0toMuMuBarVC()
						vertexTadpolesTemp = A0toMuMuBarTad()
						realCorrectionsTemp = A0toMuMuBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toMuMuBarLO       ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toMuMuBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toMuMuBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // trim(tempVal2) // "     ="
							else
								outputFileContent = trim(outputFileContent) // "A0toMuMuBarNLO" // "1" // trim(tempVal3) // "    ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> ss
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> ss"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> ss -----"
					m1 = MA0
					m2 = MS
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> ss are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> ss has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> ss does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toSSBarTree()
						vertexCorrectionsTemp = A0toSSBarVC()
						vertexTadpolesTemp = A0toSSBarTad()
						realCorrectionsTemp = A0toSSBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toSSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toSSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toSSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toSSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> cc
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> cc"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> cc -----"
					m1 = MA0
					m2 = MC
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> cc are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> cc has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> cc does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toCCBarTree()
						vertexCorrectionsTemp = A0toCCBarVC()
						vertexTadpolesTemp = A0toCCBarTad()
						realCorrectionsTemp = A0toCCBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toCCBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toCCBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toCCBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toCCBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> tt
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> tt"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> tt -----"
					m1 = MA0
					m2 = MT
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> tt are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> tt has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> tt does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toTTBarTree()
						vertexCorrectionsTemp = A0toTTBarVC()
						vertexTadpolesTemp = A0toTTBarTad()
						realCorrectionsTemp = A0toTTBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toTTBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toTTBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toTTBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "A0toTTBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> Zh
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> Zh"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> Zh -----"
					m1 = MA0
					m2 = MZ
					m3 = Mh0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> Zh are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toZ0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> Zh has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toZ0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> Zh does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toZ0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toZ0h0Tree()
						vertexCorrectionsTemp = A0toZ0h0VC()
						vertexTadpolesTemp = A0toZ0h0Tad()
						realCorrectionsTemp = A0toZ0h0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toZ0h0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toZ0h0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toZ0h0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toZ0h0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> ZH
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> ZH"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *************************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> ZH -----"
					m1 = MA0
					m2 = MHH
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> ZH are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHHZ0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> ZH has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHHZ0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> ZH does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHHZ0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toHHZ0Tree()
						vertexCorrectionsTemp = A0toHHZ0VC()
						vertexTadpolesTemp = A0toHHZ0Tad()
						realCorrectionsTemp = A0toHHZ0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHHZ0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toHHZ0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toHHZ0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHHZ0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A -> H- W+
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " A -> H- W+"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) **********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A -> H- W+ -----"
					m1 = MA0
					m2 = MHp
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process A -> H- W+ are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process A -> H- W+ has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A -> H- W+ does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toHmWpTree()
						vertexCorrectionsTemp = A0toHmWpVC()
						vertexTadpolesTemp = A0toHmWpTad()
						realCorrectionsTemp = A0toHmWpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "A0toHmWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toHmWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toHmWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "A0toHmWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> bbar c
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> bbar c"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> bbar c -----"
					m1 = MHp
					m2 = MB
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> bbar c are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarCLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> bbar c has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarCLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> bbar c does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarCLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoBBarCTree()
						vertexCorrectionsTemp = HptoBBarCVC()
						vertexTadpolesTemp = HptoBBarCTad()
						realCorrectionsTemp = HptoBBarCReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarCLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoBBarCCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoBBarCCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarCNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> taubar nu
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> taubar nu"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) *****************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> taubar nu -----"
					m1 = MHp
					m2 = 0
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> taubar nu are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarLO    ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // trim(tempVal2) // "  ="
							else
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // "1" // trim(tempVal3) // " ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> taubar nu has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarLO    ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // trim(tempVal2) // "  ="
							else
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // "1" // trim(tempVal3) // " ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> taubar nu does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarLO    ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // trim(tempVal2) // "  ="
							else
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // "1" // trim(tempVal3) // " ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoNeuTTauBarTree()
						vertexCorrectionsTemp = HptoNeuTTauBarVC()
						vertexTadpolesTemp = HptoNeuTTauBarTad()
						realCorrectionsTemp = HptoNeuTTauBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarLO    ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoNeuTTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoNeuTTauBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // trim(tempVal2) // "  ="
							else
								outputFileContent = trim(outputFileContent) // "HptoNeuTTauBarNLO" // "1" // trim(tempVal3) // " ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> mubar nu
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> mubar nu"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ******************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> mubar nu -----"
					m1 = MHp
					m2 = MM
					m3 = 0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> mubar nu are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> mubar nu has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> mubar nu does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoMuBarNeuMTree()
						vertexCorrectionsTemp = HptoMuBarNeuMVC()
						vertexTadpolesTemp = HptoMuBarNeuMTad()
						realCorrectionsTemp = HptoMuBarNeuMReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMLO     ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoMuBarNeuMCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoMuBarNeuMCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // trim(tempVal2) // "   ="
							else
								outputFileContent = trim(outputFileContent) // "HptoMuBarNeuMNLO" // "1" // trim(tempVal3) // "  ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> sbar u
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> sbar u"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> sbar u -----"
					m1 = MHp
					m2 = MS
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> sbar u are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> sbar u has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> sbar u does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoSBarUTree()
						vertexCorrectionsTemp = HptoSBarUVC()
						vertexTadpolesTemp = HptoSBarUTad()
						realCorrectionsTemp = HptoSBarUReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoSBarUCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoSBarUCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> sbar c
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> sbar c"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> sbar c -----"
					m1 = MHp
					m2 = MC
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> sbar c are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> sbar c has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> sbar c does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoCSBarTree()
						vertexCorrectionsTemp = HptoCSBarVC()
						vertexTadpolesTemp = HptoCSBarTad()
						realCorrectionsTemp = HptoCSBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCSBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoCSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoCSBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCSBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> bbar t
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> bbar t"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> bbar t -----"
					m1 = MHp
					m2 = MB
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> bbar t are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> bbar t has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> bbar t does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoBBarTTree()
						vertexCorrectionsTemp = HptoBBarTVC()
						vertexTadpolesTemp = HptoBBarTTad()
						realCorrectionsTemp = HptoBBarTReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoBBarTCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoBBarTCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> dbar c
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> dbar c"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> dbar c -----"
					m1 = MHp
					m2 = MC
					m3 = MD
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> dbar c are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCDBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> dbar c has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCDBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> dbar c does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCDBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoCDBarTree()
						vertexCorrectionsTemp = HptoCDBarVC()
						vertexTadpolesTemp = HptoCDBarTad()
						realCorrectionsTemp = HptoCDBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoCDBarLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoCDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoCDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoCDBarNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> bbar u
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> bbar u"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> bbar u -----"
					m1 = MHp
					m2 = MB
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> bbar u are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> bbar u has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> bbar u does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoBBarUTree()
						vertexCorrectionsTemp = HptoBBarUVC()
						vertexTadpolesTemp = HptoBBarUTad()
						realCorrectionsTemp = HptoBBarUReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoBBarULO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoBBarUCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoBBarUCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoBBarUNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> sbar t
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> sbar t"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> sbar t -----"
					m1 = MHp
					m2 = MS
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> sbar t are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> sbar t has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> sbar t does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoSBarTTree()
						vertexCorrectionsTemp = HptoSBarTVC()
						vertexTadpolesTemp = HptoSBarTTad()
						realCorrectionsTemp = HptoSBarTReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoSBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoSBarTCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoSBarTCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoSBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> dbar t
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> dbar t"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> dbar t -----"
					m1 = MHp
					m2 = MD
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> dbar t are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoDBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> dbar t has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoDBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> dbar t does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoDBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoDBarTTree()
						vertexCorrectionsTemp = HptoDBarTVC()
						vertexTadpolesTemp = HptoDBarTTad()
						realCorrectionsTemp = HptoDBarTReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoDBarTLO         ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoDBarTCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoDBarTCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // trim(tempVal2) // "       ="
							else
								outputFileContent = trim(outputFileContent) // "HptoDBarTNLO" // "1" // trim(tempVal3) // "      ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> W h
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> W h"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ***********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> W h -----"
					m1 = MHp
					m2 = MW
					m3 = Mh0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> W h are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoWph0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> W h has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoWph0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> W h does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoWph0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoWph0Tree()
						vertexCorrectionsTemp = HptoWph0VC()
						vertexTadpolesTemp = HptoWph0Tad()
						realCorrectionsTemp = HptoWph0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoWph0LO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoWph0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoWph0CT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoWph0NLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> W H
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> W H"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ***********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> W H -----"
					m1 = MHp
					m2 = MHH
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> W H are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoHHWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> W H has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoHHWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> W H does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoHHWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoHHWpTree()
						vertexCorrectionsTemp = HptoHHWpVC()
						vertexTadpolesTemp = HptoHHWpTad()
						realCorrectionsTemp = HptoHHWpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoHHWpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoHHWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoHHWpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoHHWpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS H+ -> W A
					! Prepare the output file content
					outputFileContent = trim(outputFileContent) // "**************"
					outputFileContent = trim(outputFileContent) // " H+ -> W A"
					outputFileContent = trim(outputFileContent) // " (electroweak corrections) ***********************\n"
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- H+ -> W A -----"
					m1 = MHp
					m2 = MA0
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (omitELCorr .EQ. 1) then
						write (*,*) "The electroweak corrections to the process H+ -> W A are not calculated since OMIT ELW2 is set to 1."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoA0WpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (m1 .LE. 0D0) then
						write (*,*) "The process H+ -> W A has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoA0WpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process H+ -> W A does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoA0WpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoA0WpTree()
						vertexCorrectionsTemp = HptoA0WpVC()
						vertexTadpolesTemp = HptoA0WpTad()
						realCorrectionsTemp = HptoA0WpReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Write the tree-level width to the output file
						write( tempVal, '(ES23.15E3)' ) treeLevelWidth
						outputFileContent = trim(outputFileContent) // "HptoA0WpLO          ="
						outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoA0WpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoA0WpCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
							! Write the NLO width to the output file
							write( tempVal, '(ES23.15E3)' ) NLOWidth(m)
							write( tempVal2, '(I1)' ) m
							write( tempVal3, '(I1)' ) (m-10)
							if (m .lt. 10) then
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // trim(tempVal2) // "        ="
							else
								outputFileContent = trim(outputFileContent) // "HptoA0WpNLO" // "1" // trim(tempVal3) // "       ="
							end if
							outputFileContent = trim(outputFileContent) // " " // (trim(tempVal) // "\n")
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

			! Write the results to the output file
			open(unit=44, file=trim(pathToOutputFiles)//trim(targetName), status='old', &
			&action='write', position='append', iostat=statWrite)
				if ( statWrite == 0) then
					write(44,*) trim(outputFileContent)
				else
					write(*,*) 'ERROR: could not create output file for writing!'
				end if
			close(unit=44)
		call ltexi


end program electroweakCorrections
