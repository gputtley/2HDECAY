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
    ! logical :: isIRDivergent = .false.
    ! logical :: isUVDivergent = .false.
    ! logical :: isGaugeDependent = .false.
    ! logical :: isGaugeDependentAList(maxNumberSchemes), isGaugeDependentWList(maxNumberSchemes), &
    !     & isGaugeDependentZList(maxNumberSchemes)
    ! logical :: isIRDivergentList(maxNumberSchemes), isUVDivergentList(maxNumberSchemes)
    ! character isIRDivergentContinue, isUVDivergentContinue, isGaugeDependentContinue
    ! double precision, parameter :: GaugeDependenceThreshold = 1D-5
    ! double precision, parameter :: IRDivergenceThreshold = 1D-11
    ! double precision, parameter :: UVDivergenceThreshold = 1D-3
    double precision prefactor, treeLevelWidth, NLOWidth(maxNumberSchemes), fullamplitude(maxNumberSchemes)
    double precision NLOVCwidth, NLOVCwoIRwidth, NLOIRonlywidth
    ! double precision GaugeXiAChecks(maxNumberSchemes), GaugeXiWChecks(maxNumberSchemes), GaugeXiZChecks(maxNumberSchemes), &
    !     & IRChecks(maxNumberSchemes), UVChecks(maxNumberSchemes)
    double precision A0toBBBarTree, A0toBBBarCT, A0toBBBarReal
    double complex A0toBBBarVC, A0toBBBarTad
    double precision A0toTTBarTree, A0toTTBarCT, A0toTTBarReal
    double complex A0toTTBarVC, A0toTTBarTad
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

            do point = 1, maxPoint, 1
                ! Set the 2HDM parameters according to the current point in phase-space
                MHH = MHHList(point)
                MA0 = MA0List(point)
                MHp = MHpList(point)
                MHH = MHHList(point)
                alpha = alphaList(point)
                beta = betaList(point)
                CA = CAList(point)
                SA = SAList(point)
                TA = TAList(point)
                CB = CBList(point)
                SB = SBList(point)
                TB = TBList(point)
                S2A = S2AList(point)
                C2A = C2AList(point)
                S2B = S2BList(point)
                C2B = C2BList(point)
                CAB = CABList(point)
                SAB = SABList(point)
                CBA = CBAList(point)
                SBA = SBAList(point)
                Yuk1 = Yuk1List(point)
                Yuk2 = Yuk2List(point)
                Yuk3 = Yuk3List(point)
                m12squared = m12squaredList(point)
                Lambda5 = Lambda5List(point)
                TypeOf2HDM = TypeOf2HDMList(point)

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
                    write (*,*) "Mh0: ", Mh0
                    write (*,*) "MHH: ", MHH
                    write (*,*) "MA0: ", MA0
                    write (*,*) "MHp: ", MHp
                    write (*,*) "alpha: ", alpha
                    write (*,*) "beta: ", beta
                    write (*,*) "CA: ", CA
                    write (*,*) "CB: ", CB
                    write (*,*) "Yuk1: ", Yuk1
                    write (*,*) "Yuk2: ", Yuk2
                    write (*,*) "Yuk3: ", Yuk3
                    write (*,*) "m12squared: ", m12squared
                    write (*,*) "2HDM Type: ", TypeOf2HDM
                    write (*,*) "InputScale: ", InputScale
                end if

                ! Calculate the square of the 2HDM input parameters
                MHH2 = MHH**2
            	MA02 = MA0**2
            	MHp2 = MHp**2
                CA2 = CA**2
            	SA2 = SA**2
            	TA2 = TA**2
            	TB2 = TB**2
            	SB2 = SB**2
            	CB2 = CB**2
            	C2A2 = C2A**2
            	S2A2 = S2A**2
            	C2B2 = C2B**2
            	S2B2 = S2B**2
            	CAB2 = CAB**2
            	SAB2 = SAB**2
            	CBA2 = CBA**2
            	SBA2 = SBA**2

                ! Kinematic prefactor together with the symmetry factor of the process
                write (*,*) "----- A0 -> B,BBar -----"
                m1 = MA0 
                m2 = MB 
                m3 = MB
                kinematicThreshold = m1**2 - (m2**2 + m3**2)
                if (kinematicThreshold .LT. 0) then 
                    write (*,*) "The process A0 -> B,BBar does not fulfill the kinematic threshold."
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
                write( tempVal, '(I1)' ) TypeOf2HDM
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
