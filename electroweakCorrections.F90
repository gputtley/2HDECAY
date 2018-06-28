program electroweakCorrections
	use constants
	use counterterms
	implicit none
#include "looptools.h"
	character(len=20) :: tempVal
	character(len=32) :: arg
	character(len=50) :: fileName, fileNameFilled, targetName
	character(len=600000) :: outputFileContent
	character(300), parameter :: pathToOutputFiles = 'Temp\\Results\\'
	integer arguments(5)
	integer, parameter :: maxNumberSchemes = 14
	logical :: debugModeOn = .false.
	double precision prefactor, treeLevelWidth, NLOWidth(maxNumberSchemes), fullamplitude(maxNumberSchemes)
	double precision NLOVCwidth, NLOVCwoIRwidth, NLOIRonlywidth
	double precision A0toBBBarTree, A0toBBBarCT, A0toBBBarReal
	double complex A0toBBBarVC, A0toBBBarTad
	double precision A0toCCBarTree, A0toCCBarCT, A0toCCBarReal
	double complex A0toCCBarVC, A0toCCBarTad
	double precision A0toDDBarTree, A0toDDBarCT, A0toDDBarReal
	double complex A0toDDBarVC, A0toDDBarTad
	double precision A0toElElBarTree, A0toElElBarCT, A0toElElBarReal
	double complex A0toElElBarVC, A0toElElBarTad
	double precision A0toHHZ0Tree, A0toHHZ0CT, A0toHHZ0Real
	double complex A0toHHZ0VC, A0toHHZ0Tad
	double precision A0toHmWpTree, A0toHmWpCT, A0toHmWpReal
	double complex A0toHmWpVC, A0toHmWpTad
	double precision A0toMuMuBarTree, A0toMuMuBarCT, A0toMuMuBarReal
	double complex A0toMuMuBarVC, A0toMuMuBarTad
	double precision A0toSSBarTree, A0toSSBarCT, A0toSSBarReal
	double complex A0toSSBarVC, A0toSSBarTad
	double precision A0toTauTauBarTree, A0toTauTauBarCT, A0toTauTauBarReal
	double complex A0toTauTauBarVC, A0toTauTauBarTad
	double precision A0toTTBarTree, A0toTTBarCT, A0toTTBarReal
	double complex A0toTTBarVC, A0toTTBarTad
	double precision A0toUUBarTree, A0toUUBarCT, A0toUUBarReal
	double complex A0toUUBarVC, A0toUUBarTad
	double precision A0toZ0h0Tree, A0toZ0h0CT, A0toZ0h0Real
	double complex A0toZ0h0VC, A0toZ0h0Tad
	double precision h0toA0A0Tree, h0toA0A0CT, h0toA0A0Real
	double complex h0toA0A0VC, h0toA0A0Tad
	double precision h0toA0Z0Tree, h0toA0Z0CT, h0toA0Z0Real
	double complex h0toA0Z0VC, h0toA0Z0Tad
	double precision h0toBBBarTree, h0toBBBarCT, h0toBBBarReal
	double complex h0toBBBarVC, h0toBBBarTad
	double precision h0toCCBarTree, h0toCCBarCT, h0toCCBarReal
	double complex h0toCCBarVC, h0toCCBarTad
	double precision h0toDDBarTree, h0toDDBarCT, h0toDDBarReal
	double complex h0toDDBarVC, h0toDDBarTad
	double precision h0toElElBarTree, h0toElElBarCT, h0toElElBarReal
	double complex h0toElElBarVC, h0toElElBarTad
	double precision h0toHHHHTree, h0toHHHHCT, h0toHHHHReal
	double complex h0toHHHHVC, h0toHHHHTad
	double precision h0toHmHpTree, h0toHmHpCT, h0toHmHpReal
	double complex h0toHmHpVC, h0toHmHpTad
	double precision h0toHpWmTree, h0toHpWmCT, h0toHpWmReal
	double complex h0toHpWmVC, h0toHpWmTad
	double precision h0toMuMuBarTree, h0toMuMuBarCT, h0toMuMuBarReal
	double complex h0toMuMuBarVC, h0toMuMuBarTad
	double precision h0toSSBarTree, h0toSSBarCT, h0toSSBarReal
	double complex h0toSSBarVC, h0toSSBarTad
	double precision h0toTauTauBarTree, h0toTauTauBarCT, h0toTauTauBarReal
	double complex h0toTauTauBarVC, h0toTauTauBarTad
	double precision h0toTTBarTree, h0toTTBarCT, h0toTTBarReal
	double complex h0toTTBarVC, h0toTTBarTad
	double precision h0toUUBarTree, h0toUUBarCT, h0toUUBarReal
	double complex h0toUUBarVC, h0toUUBarTad
	double precision h0toWmWpTree, h0toWmWpCT, h0toWmWpReal
	double complex h0toWmWpVC, h0toWmWpTad
	double precision h0toZ0Z0Tree, h0toZ0Z0CT, h0toZ0Z0Real
	double complex h0toZ0Z0VC, h0toZ0Z0Tad
	double precision HHtoA0A0Tree, HHtoA0A0CT, HHtoA0A0Real
	double complex HHtoA0A0VC, HHtoA0A0Tad
	double precision HHtoA0Z0Tree, HHtoA0Z0CT, HHtoA0Z0Real
	double complex HHtoA0Z0VC, HHtoA0Z0Tad
	double precision HHtoBBBarTree, HHtoBBBarCT, HHtoBBBarReal
	double complex HHtoBBBarVC, HHtoBBBarTad
	double precision HHtoCCBarTree, HHtoCCBarCT, HHtoCCBarReal
	double complex HHtoCCBarVC, HHtoCCBarTad
	double precision HHtoDDBarTree, HHtoDDBarCT, HHtoDDBarReal
	double complex HHtoDDBarVC, HHtoDDBarTad
	double precision HHtoElElBarTree, HHtoElElBarCT, HHtoElElBarReal
	double complex HHtoElElBarVC, HHtoElElBarTad
	double precision HHtoh0h0Tree, HHtoh0h0CT, HHtoh0h0Real
	double complex HHtoh0h0VC, HHtoh0h0Tad
	double precision HHtoHmHpTree, HHtoHmHpCT, HHtoHmHpReal
	double complex HHtoHmHpVC, HHtoHmHpTad
	double precision HHtoHpWmTree, HHtoHpWmCT, HHtoHpWmReal
	double complex HHtoHpWmVC, HHtoHpWmTad
	double precision HHtoMuMuBarTree, HHtoMuMuBarCT, HHtoMuMuBarReal
	double complex HHtoMuMuBarVC, HHtoMuMuBarTad
	double precision HHtoSSBarTree, HHtoSSBarCT, HHtoSSBarReal
	double complex HHtoSSBarVC, HHtoSSBarTad
	double precision HHtoTauTauBarTree, HHtoTauTauBarCT, HHtoTauTauBarReal
	double complex HHtoTauTauBarVC, HHtoTauTauBarTad
	double precision HHtoTTBarTree, HHtoTTBarCT, HHtoTTBarReal
	double complex HHtoTTBarVC, HHtoTTBarTad
	double precision HHtoUUBarTree, HHtoUUBarCT, HHtoUUBarReal
	double complex HHtoUUBarVC, HHtoUUBarTad
	double precision HHtoWmWpTree, HHtoWmWpCT, HHtoWmWpReal
	double complex HHtoWmWpVC, HHtoWmWpTad
	double precision HHtoZ0Z0Tree, HHtoZ0Z0CT, HHtoZ0Z0Real
	double complex HHtoZ0Z0VC, HHtoZ0Z0Tad
	double precision HptoA0WpTree, HptoA0WpCT, HptoA0WpReal
	double complex HptoA0WpVC, HptoA0WpTad
	double precision HptoBBarCTree, HptoBBarCCT, HptoBBarCReal
	double complex HptoBBarCVC, HptoBBarCTad
	double precision HptoBBarTTree, HptoBBarTCT, HptoBBarTReal
	double complex HptoBBarTVC, HptoBBarTTad
	double precision HptoBBarUTree, HptoBBarUCT, HptoBBarUReal
	double complex HptoBBarUVC, HptoBBarUTad
	double precision HptoCDBarTree, HptoCDBarCT, HptoCDBarReal
	double complex HptoCDBarVC, HptoCDBarTad
	double precision HptoCSBarTree, HptoCSBarCT, HptoCSBarReal
	double complex HptoCSBarVC, HptoCSBarTad
	double precision HptoDBarTTree, HptoDBarTCT, HptoDBarTReal
	double complex HptoDBarTVC, HptoDBarTTad
	double precision HptoDBarUTree, HptoDBarUCT, HptoDBarUReal
	double complex HptoDBarUVC, HptoDBarUTad
	double precision HptoElBarNeuETree, HptoElBarNeuECT, HptoElBarNeuEReal
	double complex HptoElBarNeuEVC, HptoElBarNeuETad
	double precision HptoHHWpTree, HptoHHWpCT, HptoHHWpReal
	double complex HptoHHWpVC, HptoHHWpTad
	double precision HptoMuBarNeuMTree, HptoMuBarNeuMCT, HptoMuBarNeuMReal
	double complex HptoMuBarNeuMVC, HptoMuBarNeuMTad
	double precision HptoNeuTTauBarTree, HptoNeuTTauBarCT, HptoNeuTTauBarReal
	double complex HptoNeuTTauBarVC, HptoNeuTTauBarTad
	double precision HptoSBarTTree, HptoSBarTCT, HptoSBarTReal
	double complex HptoSBarTVC, HptoSBarTTad
	double precision HptoSBarUTree, HptoSBarUCT, HptoSBarUReal
	double complex HptoSBarUVC, HptoSBarUTad
	double precision HptoWph0Tree, HptoWph0CT, HptoWph0Real
	double complex HptoWph0VC, HptoWph0Tad
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
			outputFileContent = "MHH,"
			outputFileContent = trim(outputFileContent) // "Mh0,"
			outputFileContent = trim(outputFileContent) // "MA0,"
			outputFileContent = trim(outputFileContent) // "MHp,"
			outputFileContent = trim(outputFileContent) // "alpha,"
			outputFileContent = trim(outputFileContent) // "beta,"
			outputFileContent = trim(outputFileContent) // "m122,"
			outputFileContent = trim(outputFileContent) // "Lambda5,"
			outputFileContent = trim(outputFileContent) // "2HDMType,"
			outputFileContent = trim(outputFileContent) // "InputScale,"
			outputFileContent = trim(outputFileContent) // "WidthLO,"
			if (debugModeOn) then
				outputFileContent = trim(outputFileContent) // "WidthNLOVC,"
				outputFileContent = trim(outputFileContent) // "WidthNLOVCwoIR,"
				outputFileContent = trim(outputFileContent) // "WidthIRonly,"
				outputFileContent = trim(outputFileContent) // "dMW2Usual,"
				outputFileContent = trim(outputFileContent) // "dMW2Alter,"
				outputFileContent = trim(outputFileContent) // "dMZ2Usual,"
				outputFileContent = trim(outputFileContent) // "dMZ2Alter,"
				outputFileContent = trim(outputFileContent) // "dMLOSUsual,"
				outputFileContent = trim(outputFileContent) // "dMLOSAlter,"
				outputFileContent = trim(outputFileContent) // "dMBOSUsual,"
				outputFileContent = trim(outputFileContent) // "dMBOSAlter,"
				outputFileContent = trim(outputFileContent) // "dZHHHHOS,"
				outputFileContent = trim(outputFileContent) // "dZHHh0OSUsual,"
				outputFileContent = trim(outputFileContent) // "dZHHh0OSAlter,"
				outputFileContent = trim(outputFileContent) // "dZh0HHOSUsual,"
				outputFileContent = trim(outputFileContent) // "dZh0HHOSAlter,"
				outputFileContent = trim(outputFileContent) // "dZh0h0OS,"
				outputFileContent = trim(outputFileContent) // "dZA0A0OS,"
				outputFileContent = trim(outputFileContent) // "dZG0A0OSUsual,"
				outputFileContent = trim(outputFileContent) // "dZG0A0OSAlter,"
				outputFileContent = trim(outputFileContent) // "dZBotBotOSLeft,"
				outputFileContent = trim(outputFileContent) // "dZBotBotOSRight,"
				outputFileContent = trim(outputFileContent) // "dZTauTauOSLeft,"
				outputFileContent = trim(outputFileContent) // "dZTauTauOSRight,"
				outputFileContent = trim(outputFileContent) // "dBeta1KanUsual,"
				outputFileContent = trim(outputFileContent) // "dBeta1KanAlter,"
				outputFileContent = trim(outputFileContent) // "dBeta1PinchPStar,"
				outputFileContent = trim(outputFileContent) // "dBeta1PinchOS,"
			end if
			outputFileContent = trim(outputFileContent) // "WidthKanOdd,"
			outputFileContent = trim(outputFileContent) // "WidthKanChar,"
			outputFileContent = trim(outputFileContent) // "WidthTadOdd,"
			outputFileContent = trim(outputFileContent) // "WidthTadChar,"
			outputFileContent = trim(outputFileContent) // "WidthPStarOdd,"
			outputFileContent = trim(outputFileContent) // "WidthPStarChar,"
			outputFileContent = trim(outputFileContent) // "WidthOSPinOdd,"
			outputFileContent = trim(outputFileContent) // "WidthOSPinChar,"
			outputFileContent = trim(outputFileContent) // "WidthUsuProcDep1,"
			outputFileContent = trim(outputFileContent) // "WidthAltProcDep1,"
			outputFileContent = trim(outputFileContent) // "WidthUsuProcDep2,"
			outputFileContent = trim(outputFileContent) // "WidthAltProcDep2,"
			outputFileContent = trim(outputFileContent) // "WidthUsuProcDep3,"
			outputFileContent = trim(outputFileContent) // "WidthAltProcDep3,"
			outputFileContent = trim(outputFileContent) // "DifWidthLO,"
			outputFileContent = trim(outputFileContent) // "DifWidthKanOdd,"
			outputFileContent = trim(outputFileContent) // "DifWidthKanChar,"
			outputFileContent = trim(outputFileContent) // "DifWidthTadOdd,"
			outputFileContent = trim(outputFileContent) // "DifWidthTadChar,"
			outputFileContent = trim(outputFileContent) // "DifWidthPStarOdd,"
			outputFileContent = trim(outputFileContent) // "DifWidthPStarChar,"
			outputFileContent = trim(outputFileContent) // "DifWidthOSPinOdd,"
			outputFileContent = trim(outputFileContent) // "DifWidthOSPinChar,"
			outputFileContent = trim(outputFileContent) // "DifWidthUsuProcDep1,"
			outputFileContent = trim(outputFileContent) // "DifWidthAltProcDep1,"
			outputFileContent = trim(outputFileContent) // "DifWidthUsuProcDep2,"
			outputFileContent = trim(outputFileContent) // "DifWidthAltProcDep2,"
			outputFileContent = trim(outputFileContent) // "DifWidthUsuProcDep3,"
			outputFileContent = trim(outputFileContent) // "DifWidthAltProcDep3\n"

				! Print out the current point in phase-space (debug mode only)
				if (debugModeOn) then
					write (*,*) "MW: ", MW
					write (*,*) "MZ: ", MZ
					write (*,*) "SW: ", SW
					write (*,*) "CW: ", CW
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
				end if

				! PROCESS A0 -> B,BBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> B,BBar -----"
					m1 = MA0
					m2 = MB
					m3 = MB
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> B,BBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> B,BBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> C,CBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> C,CBar -----"
					m1 = MA0
					m2 = MC
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> C,CBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> C,CBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> D,DBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> D,DBar -----"
					m1 = MA0
					m2 = MD
					m3 = MD
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> D,DBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> D,DBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toDDBarTree()
						vertexCorrectionsTemp = A0toDDBarVC()
						vertexTadpolesTemp = A0toDDBarTad()
						realCorrectionsTemp = A0toDDBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toDDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toDDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> El,ElBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> El,ElBar -----"
					m1 = MA0
					m2 = ME
					m3 = ME
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> El,ElBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> El,ElBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toElElBarTree()
						vertexCorrectionsTemp = A0toElElBarVC()
						vertexTadpolesTemp = A0toElElBarTad()
						realCorrectionsTemp = A0toElElBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toElElBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toElElBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> HH,Z0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> HH,Z0 -----"
					m1 = MA0
					m2 = MHH
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> HH,Z0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> HH,Z0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> Hm,Wp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> Hm,Wp -----"
					m1 = MA0
					m2 = MHp
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> Hm,Wp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> Hm,Wp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> Mu,MuBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> Mu,MuBar -----"
					m1 = MA0
					m2 = MM
					m3 = MM
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> Mu,MuBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> Mu,MuBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> S,SBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> S,SBar -----"
					m1 = MA0
					m2 = MS
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> S,SBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> S,SBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> Tau,TauBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> Tau,TauBar -----"
					m1 = MA0
					m2 = ML
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> Tau,TauBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> Tau,TauBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> T,TBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> T,TBar -----"
					m1 = MA0
					m2 = MT
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> T,TBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> T,TBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> U,UBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> U,UBar -----"
					m1 = MA0
					m2 = MU
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> U,UBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> U,UBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = A0toUUBarTree()
						vertexCorrectionsTemp = A0toUUBarVC()
						vertexTadpolesTemp = A0toUUBarTad()
						realCorrectionsTemp = A0toUUBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*A0toUUBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*A0toUUBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS A0 -> Z0,h0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- A0 -> Z0,h0 -----"
					m1 = MA0
					m2 = MZ
					m3 = Mh0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process A0 -> Z0,h0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process A0 -> Z0,h0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> A0,A0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> A0,A0 -----"
					m1 = Mh0
					m2 = MA0
					m3 = MA0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> A0,A0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> A0,A0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toA0A0Tree()
						vertexCorrectionsTemp = h0toA0A0VC()
						vertexTadpolesTemp = h0toA0A0Tad()
						realCorrectionsTemp = h0toA0A0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> A0,Z0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> A0,Z0 -----"
					m1 = Mh0
					m2 = MA0
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> A0,Z0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> A0,Z0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> B,BBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> B,BBar -----"
					m1 = Mh0
					m2 = MB
					m3 = MB
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> B,BBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> B,BBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> C,CBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> C,CBar -----"
					m1 = Mh0
					m2 = MC
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> C,CBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> C,CBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> D,DBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> D,DBar -----"
					m1 = Mh0
					m2 = MD
					m3 = MD
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> D,DBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> D,DBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toDDBarTree()
						vertexCorrectionsTemp = h0toDDBarVC()
						vertexTadpolesTemp = h0toDDBarTad()
						realCorrectionsTemp = h0toDDBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toDDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toDDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> El,ElBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> El,ElBar -----"
					m1 = Mh0
					m2 = ME
					m3 = ME
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> El,ElBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> El,ElBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toElElBarTree()
						vertexCorrectionsTemp = h0toElElBarVC()
						vertexTadpolesTemp = h0toElElBarTad()
						realCorrectionsTemp = h0toElElBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toElElBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toElElBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> HH,HH
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> HH,HH -----"
					m1 = Mh0
					m2 = MHH
					m3 = MHH
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> HH,HH has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> HH,HH does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toHHHHTree()
						vertexCorrectionsTemp = h0toHHHHVC()
						vertexTadpolesTemp = h0toHHHHTad()
						realCorrectionsTemp = h0toHHHHReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> Hm,Hp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> Hm,Hp -----"
					m1 = Mh0
					m2 = MHp
					m3 = MHp
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> Hm,Hp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> Hm,Hp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> Hp,Wm
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> Hp,Wm -----"
					m1 = Mh0
					m2 = MHp
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> Hp,Wm has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> Hp,Wm does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> Mu,MuBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> Mu,MuBar -----"
					m1 = Mh0
					m2 = MM
					m3 = MM
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> Mu,MuBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> Mu,MuBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> S,SBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> S,SBar -----"
					m1 = Mh0
					m2 = MS
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> S,SBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> S,SBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> Tau,TauBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> Tau,TauBar -----"
					m1 = Mh0
					m2 = ML
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> Tau,TauBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> Tau,TauBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> T,TBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> T,TBar -----"
					m1 = Mh0
					m2 = MT
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> T,TBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> T,TBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> U,UBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> U,UBar -----"
					m1 = Mh0
					m2 = MU
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> U,UBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> U,UBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toUUBarTree()
						vertexCorrectionsTemp = h0toUUBarVC()
						vertexTadpolesTemp = h0toUUBarTad()
						realCorrectionsTemp = h0toUUBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*h0toUUBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*h0toUUBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> Wm,Wp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> Wm,Wp -----"
					m1 = Mh0
					m2 = MW
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> Wm,Wp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> Wm,Wp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS h0 -> Z0,Z0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- h0 -> Z0,Z0 -----"
					m1 = Mh0
					m2 = MZ
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process h0 -> Z0,Z0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process h0 -> Z0,Z0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = h0toZ0Z0Tree()
						vertexCorrectionsTemp = h0toZ0Z0VC()
						vertexTadpolesTemp = h0toZ0Z0Tad()
						realCorrectionsTemp = h0toZ0Z0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> A0,A0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> A0,A0 -----"
					m1 = MHH
					m2 = MA0
					m3 = MA0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> A0,A0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> A0,A0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoA0A0Tree()
						vertexCorrectionsTemp = HHtoA0A0VC()
						vertexTadpolesTemp = HHtoA0A0Tad()
						realCorrectionsTemp = HHtoA0A0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> A0,Z0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> A0,Z0 -----"
					m1 = MHH
					m2 = MA0
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> A0,Z0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> A0,Z0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> B,BBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> B,BBar -----"
					m1 = MHH
					m2 = MB
					m3 = MB
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> B,BBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> B,BBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> C,CBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> C,CBar -----"
					m1 = MHH
					m2 = MC
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> C,CBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> C,CBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> D,DBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> D,DBar -----"
					m1 = MHH
					m2 = MD
					m3 = MD
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> D,DBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> D,DBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoDDBarTree()
						vertexCorrectionsTemp = HHtoDDBarVC()
						vertexTadpolesTemp = HHtoDDBarTad()
						realCorrectionsTemp = HHtoDDBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoDDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoDDBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> El,ElBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> El,ElBar -----"
					m1 = MHH
					m2 = ME
					m3 = ME
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> El,ElBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> El,ElBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoElElBarTree()
						vertexCorrectionsTemp = HHtoElElBarVC()
						vertexTadpolesTemp = HHtoElElBarTad()
						realCorrectionsTemp = HHtoElElBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoElElBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoElElBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> h0,h0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> h0,h0 -----"
					m1 = MHH
					m2 = Mh0
					m3 = Mh0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> h0,h0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> h0,h0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoh0h0Tree()
						vertexCorrectionsTemp = HHtoh0h0VC()
						vertexTadpolesTemp = HHtoh0h0Tad()
						realCorrectionsTemp = HHtoh0h0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> Hm,Hp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> Hm,Hp -----"
					m1 = MHH
					m2 = MHp
					m3 = MHp
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> Hm,Hp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> Hm,Hp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> Hp,Wm
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> Hp,Wm -----"
					m1 = MHH
					m2 = MHp
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> Hp,Wm has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> Hp,Wm does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> Mu,MuBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> Mu,MuBar -----"
					m1 = MHH
					m2 = MM
					m3 = MM
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> Mu,MuBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> Mu,MuBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> S,SBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> S,SBar -----"
					m1 = MHH
					m2 = MS
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> S,SBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> S,SBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> Tau,TauBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> Tau,TauBar -----"
					m1 = MHH
					m2 = ML
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> Tau,TauBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> Tau,TauBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> T,TBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> T,TBar -----"
					m1 = MHH
					m2 = MT
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> T,TBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> T,TBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> U,UBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> U,UBar -----"
					m1 = MHH
					m2 = MU
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> U,UBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> U,UBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoUUBarTree()
						vertexCorrectionsTemp = HHtoUUBarVC()
						vertexTadpolesTemp = HHtoUUBarTad()
						realCorrectionsTemp = HHtoUUBarReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HHtoUUBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HHtoUUBarCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> Wm,Wp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> Wm,Wp -----"
					m1 = MHH
					m2 = MW
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> Wm,Wp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> Wm,Wp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS HH -> Z0,Z0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- HH -> Z0,Z0 -----"
					m1 = MHH
					m2 = MZ
					m3 = MZ
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process HH -> Z0,Z0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process HH -> Z0,Z0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HHtoZ0Z0Tree()
						vertexCorrectionsTemp = HHtoZ0Z0VC()
						vertexTadpolesTemp = HHtoZ0Z0Tad()
						realCorrectionsTemp = HHtoZ0Z0Real()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> A0,Wp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> A0,Wp -----"
					m1 = MHp
					m2 = MA0
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> A0,Wp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> A0,Wp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> BBar,C
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> BBar,C -----"
					m1 = MHp
					m2 = MB
					m3 = MC
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> BBar,C has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> BBar,C does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> BBar,T
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> BBar,T -----"
					m1 = MHp
					m2 = MB
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> BBar,T has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> BBar,T does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> BBar,U
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> BBar,U -----"
					m1 = MHp
					m2 = MB
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> BBar,U has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> BBar,U does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> C,DBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> C,DBar -----"
					m1 = MHp
					m2 = MC
					m3 = MD
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> C,DBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> C,DBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> C,SBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> C,SBar -----"
					m1 = MHp
					m2 = MC
					m3 = MS
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> C,SBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> C,SBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> DBar,T
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> DBar,T -----"
					m1 = MHp
					m2 = MD
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> DBar,T has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> DBar,T does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> DBar,U
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> DBar,U -----"
					m1 = MHp
					m2 = MD
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> DBar,U has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> DBar,U does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoDBarUTree()
						vertexCorrectionsTemp = HptoDBarUVC()
						vertexTadpolesTemp = HptoDBarUTad()
						realCorrectionsTemp = HptoDBarUReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoDBarUCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoDBarUCT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> ElBar,NeuE
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> ElBar,NeuE -----"
					m1 = MHp
					m2 = ME
					m3 = 0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> ElBar,NeuE has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> ElBar,NeuE does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else
						! Kinematic prefactor
						prefactor = 1D0/1D0 * DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 &
								&  - 2D0*m2**2*m3**2 )/(16D0*PI*m1**3)

						! Calculate the NLO ingredients
						call clearcache
						treeLevelTemp = HptoElBarNeuETree()
						vertexCorrectionsTemp = HptoElBarNeuEVC()
						vertexTadpolesTemp = HptoElBarNeuETad()
						realCorrectionsTemp = HptoElBarNeuEReal()

						! Get the full tree-level decay width
						treeLevelWidth = prefactor*treeLevelTemp

						! Get the full NLO decay width for all schemes
						do m = 1, maxNumberSchemes, 1
							call clearcache

							! Schemes 1, 2, 9, 11 and 13 are without tadpoles
							if ((m == 1) .OR. (m == 2) .OR. (m == 9) .OR. (m == 11) .OR. (m == 13)) then
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
										& 2D0*HptoElBarNeuECT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							else
								fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
										& 2D0*HptoElBarNeuECT(m)
								call clearcache
								NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
							end if
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> HH,Wp
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> HH,Wp -----"
					m1 = MHp
					m2 = MHH
					m3 = MW
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> HH,Wp has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> HH,Wp does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> MuBar,NeuM
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> MuBar,NeuM -----"
					m1 = MHp
					m2 = MM
					m3 = 0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> MuBar,NeuM has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> MuBar,NeuM does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> NeuT,TauBar
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> NeuT,TauBar -----"
					m1 = MHp
					m2 = 0
					m3 = ML
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> NeuT,TauBar has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> NeuT,TauBar does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> SBar,T
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> SBar,T -----"
					m1 = MHp
					m2 = MS
					m3 = MT
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> SBar,T has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> SBar,T does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> SBar,U
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> SBar,U -----"
					m1 = MHp
					m2 = MS
					m3 = MU
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> SBar,U has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> SBar,U does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! PROCESS Hp -> Wp,h0
					! Kinematic prefactor together with the symmetry factor of the process
					write (*,*) "----- Hp -> Wp,h0 -----"
					m1 = MHp
					m2 = MW
					m3 = Mh0
					kinematicThreshold = m1**2 - (m2 + m3)**2
					if (m1 .LE. 0D0) then
						write (*,*) "The process Hp -> Wp,h0 has a massless particle in the initial state. A decay of massless&
								& particles is not supported. The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
						end do
					else if (kinematicThreshold .LT. 0) then
						write (*,*) "The process Hp -> Wp,h0 does not fulfill the kinematic threshold.&
								& The LO and NLO widths are set to zero manually."
						treeLevelWidth = 0D0
						do m = 1, maxNumberSchemes, 1
							NLOWidth(m) = 0D0
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
						end do
					end if

					write (*,*) "TreeLevelWidth = ", treeLevelWidth
					do m = 1, maxNumberSchemes, 1
						write (*,*) "NLOWidth(", m, ") = ", NLOWidth(m)
					end do

					write (*,*) "------------------------"
					write (*,*) ""

				! Write the results to the output string
				! Format: we use 18 characters in total for double precision. 3 are reserved for the exponent, 1 for the sign of the exponent,
				! 1 for the symbol E denoting the exponent, 1 for the dot, 1 for a possible negative sign, 1 for the digit before the comma
				! and 10 for the digits after the comma
				write( tempVal, '(ES18.10E3)' ) MHH
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) Mh0
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) MA0
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) MHp
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) alpha
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) beta
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) m12squared
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) Lambda5
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(I1)' ) int(TypeOf2HDM)
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) InputScale
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				write( tempVal, '(ES18.10E3)' ) treeLevelWidth
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				if (debugModeOn) then
					write( tempVal, '(ES18.10E3)' ) NLOVCwidth
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) NLOVCwoIRwidth
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) NLOIRonlywidth
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMW2Usual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMW2Alter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMZ2Usual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMZ2Alter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMLOSUsual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMLOSAlter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMBOSUsual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dMBOSAlter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZHHHHOS()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZHHh0OSUsual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZHHh0OSAlter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZh0HHOSUsual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZh0HHOSAlter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZh0h0OS()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZA0A0OS()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZG0A0OSUsual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZG0A0OSAlter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZTauTauOSLeft()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZTauTauOSRight()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZBBOSLeft()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dZBBOSRight()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dBeta1KanUsual()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dBeta1KanAlter()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dBeta1PinchPStar()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					write( tempVal, '(ES18.10E3)' ) dBeta1PinchOS()
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				end if
				do m = 1, maxNumberSchemes, 1
					write( tempVal, '(ES18.10E3)') NLOWidth(m)
					outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				end do
				write( tempVal, '(ES18.10E3)' ) ((treeLevelWidth - treeLevelWidth)*100D0/treeLevelWidth)
				outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
				do m = 1, maxNumberSchemes, 1
					write( tempVal, '(ES18.10E3)') ((NLOWidth(m) - treeLevelWidth)*100D0/treeLevelWidth)
					if (m == maxNumberSchemes) then
						outputFileContent = trim(outputFileContent) // (trim(tempVal) // "\n")
					else
						outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
					end if
				end do

			! Write the results to the output file
			open(unit=44, file=trim(pathToOutputFiles)//trim(targetName), status='new', &
			&action='write', iostat=statWrite)
				if ( statWrite == 0) then
					write(44,*) trim(outputFileContent)
				else
					write(*,*) 'ERROR: could not create output file for writing!'
				end if
			close(unit=44)
		call ltexi


end program electroweakCorrections
