double precision function HptoWph0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 select case (x)
    case (1)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSUsual()/2D0 - dZHHh0OSUsual()/2D0 + dAlphaKanUsual() - dBeta1KanUsual()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (2)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSUsual()/2D0 - dZHHh0OSUsual()/2D0 + dAlphaKanUsual() - dBeta2KanUsual()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (3)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaKanAlter() - dBeta1KanAlter()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (4)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaKanAlter() - dBeta2KanAlter()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (5)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaPinchPStar() - dBeta1PinchPStar()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (6)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaPinchPStar() - dBeta2PinchPStar()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (7)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaPinchOS() - dBeta1PinchOS()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (8)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaPinchOS() - dBeta2PinchOS()) ) * (EL2/SW2)*CBA2 * &
            & ( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (9)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSUsual()/2D0 - dZHHh0OSUsual()/2D0 + dAlphaProcDep1Usual() - dBetaProcDep1Usual()) ) * (EL2/SW2)*CBA2&
            & *( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (10)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaProcDep1Alter() - dBetaProcDep1Alter()) ) * (EL2/SW2)*CBA2&
            & *( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (11)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSUsual()/2D0 - dZHHh0OSUsual()/2D0 + dAlphaProcDep2Usual() - dBetaProcDep2Usual()) ) * (EL2/SW2)*CBA2&
            & *( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (12)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaProcDep2Alter() - dBetaProcDep2Alter()) ) * (EL2/SW2)*CBA2&
            & *( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (13)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSUsual()/2D0 - dZHHh0OSUsual()/2D0 + dAlphaProcDep3Usual() - dBetaProcDep3Usual()) ) * (EL2/SW2)*CBA2&
            & *( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case (14)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dZh0h0OS()/2D0 + dZHpHpOS()/2D0 + dZWpWpOS()/2D0 + &
            & SBA/CBA*(dZGpHpOSAlter()/2D0 - dZHHh0OSAlter()/2D0 + dAlphaProcDep3Alter() - dBetaProcDep3Alter()) ) * (EL2/SW2)*CBA2&
            & *( (MHp2**2 + MW2**2 + Mh02**2 - 2D0*MHp2*MW2 - 2D0*MHp2*Mh02 - 2D0*MW2*Mh02)/(4D0*MW2) )
    case default
        totalAmplitude = 0D0
 end select
 HptoWph0CT = totalAmplitude
end function HptoWph0CT
