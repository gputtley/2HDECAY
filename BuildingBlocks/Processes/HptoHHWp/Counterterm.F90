double precision function HptoHHWpCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 select case (x)
    case (1)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSUsual()/2D0 + dZh0HHOSUsual()/2D0 + dAlphaKanUsual() - dBeta1KanUsual()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (2)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSUsual()/2D0 + dZh0HHOSUsual()/2D0 + dAlphaKanUsual() - dBeta2KanUsual()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (3)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaKanAlter() - dBeta1KanAlter()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (4)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaKanAlter() - dBeta2KanAlter()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (5)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaPinchPStar() - dBeta1PinchPStar()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (6)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaPinchPStar() - dBeta2PinchPStar()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (7)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaPinchPStar() - dBeta1PinchOS()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (8)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaPinchPStar() - dBeta2PinchOS()) ) * (EL2/SW2)*SBA2 * &
            & ( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (9)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSUsual()/2D0 + dZh0HHOSUsual()/2D0 + dAlphaProcDep1Usual() - dBetaProcDep1Usual()) ) * (EL2/SW2)*SBA2&
            & *( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (10)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaProcDep1Alter() - dBetaProcDep1Alter()) ) * (EL2/SW2)*SBA2&
            & *( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (11)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSUsual()/2D0 + dZh0HHOSUsual()/2D0 + dAlphaProcDep2Usual() - dBetaProcDep2Usual()) ) * (EL2/SW2)*SBA2&
            & *( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (12)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaProcDep2Alter() - dBetaProcDep2Alter()) ) * (EL2/SW2)*SBA2&
            & *( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (13)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSUsual()/2D0 + dZh0HHOSUsual()/2D0 + dAlphaProcDep3Usual() - dBetaProcDep3Usual()) ) * (EL2/SW2)*SBA2&
            & *( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case (14)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZHHHHOS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 - &
            & CBA/SBA*(dZGpHpOSAlter()/2D0 + dZh0HHOSAlter()/2D0 + dAlphaProcDep3Alter() - dBetaProcDep3Alter()) ) * (EL2/SW2)*SBA2&
            & *( (MHp2**2 + MW2**2 + MHH2**2 - 2D0*MHp2*MW2 - 2D0*MHp2*MHH2 - 2D0*MW2*MHH2)/(4D0*MW2) )
    case default
        totalAmplitude = 0D0
 end select

 HptoHHWpCT = totalAmplitude
end function HptoHHWpCT
