double precision function HptoMuBarNeuMCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1KanUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2KanUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1KanAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2KanAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1PinchPStar() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2PinchPStar() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1PinchOS() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2PinchOS() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep1Usual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep1Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep2Usual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep2Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep3Usual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep3Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk3*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
 end select

 HptoMuBarNeuMCT = totalAmplitude
end function HptoMuBarNeuMCT
