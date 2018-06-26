double precision function HHtoh0h0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBeta1KanUsual()&
	& - dAlphaKanUsual()) - 2D0*C2B/S2B*dBeta1KanUsual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBeta1KanUsual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanUsual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1KanUsual()&
  &)
	case (2)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBeta2KanUsual()&
	&- dAlphaKanUsual()) - 2D0*C2B/S2B*dBeta2KanUsual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBeta2KanUsual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanUsual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2KanUsual()&
  &)
	case (3)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta1KanAlter()&
	& - dAlphaKanAlter()) - 2D0*C2B/S2B*dBeta1KanAlter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1KanAlter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanAlter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1KanAlter()&
  &)
	case (4)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta2KanAlter()&
	& - dAlphaKanAlter()) - 2D0*C2B/S2B*dBeta2KanAlter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2KanAlter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanAlter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2KanAlter()&
  &)
	case (5)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta1PinchPStar()&
	& - dAlphaPinchPStar()) - 2D0*C2B/S2B*dBeta1PinchPStar() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1PinchPStar() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchPStar() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1PinchPStar()&
  &)
	case (6)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta2PinchPStar()&
	& - dAlphaPinchPStar()) - 2D0*C2B/S2B*dBeta2PinchPStar() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2PinchPStar() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchPStar() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2PinchPStar()&
  &)
	case (7)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta1PinchOS()&
	& - dAlphaPinchOS()) - 2D0*C2B/S2B*dBeta1PinchOS() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1PinchOS() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchOS() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1PinchOS()&
  &)
	case (8)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta2PinchOS()&
	& - dAlphaPinchOS()) - 2D0*C2B/S2B*dBeta2PinchOS() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2PinchOS() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchOS() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2PinchOS()&
  &)
	case (9)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBetaProcDep1Usual()&
	& - dAlphaProcDep1Usual()) - 2D0*C2B/S2B*dBetaProcDep1Usual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBetaProcDep1Usual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep1Usual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep1Usual()&
  &)
	case (10)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaProcDep1Alter()&
	& - dAlphaProcDep1Alter()) - 2D0*C2B/S2B*dBetaProcDep1Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep1Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep1Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep1Alter()&
  &)
	case (11)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBetaProcDep2Usual()&
	& - dAlphaProcDep2Usual()) - 2D0*C2B/S2B*dBetaProcDep2Usual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBetaProcDep2Usual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep2Usual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep2Usual()&
  &)
	case (12)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaProcDep2Alter()&
	& - dAlphaProcDep2Alter()) - 2D0*C2B/S2B*dBetaProcDep2Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep2Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep2Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep2Alter()&
  &)
	case (13)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBetaProcDep3Usual()&
	& - dAlphaProcDep3Usual()) - 2D0*C2B/S2B*dBetaProcDep3Usual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBetaProcDep3Usual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep3Usual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep3Usual()&
  &)
	case (14)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaProcDep3Alter()&
	& - dAlphaProcDep3Alter()) - 2D0*C2B/S2B*dBetaProcDep3Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep3Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep3Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep3Alter()&
  &)
	case default
		totalAmplitude = 0D0
 end select

 HHtoh0h0CT = totalAmplitude
end function HHtoh0h0CT
