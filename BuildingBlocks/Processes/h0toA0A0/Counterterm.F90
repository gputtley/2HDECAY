double precision function h0toA0A0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSUsual()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSUsual() - dMh02OSUsual()) + CBA*(2D0*MA02 - Mh02)*(dBeta1KanUsual() - dAlphaKanUsual()) + &
		& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta1KanUsual()) + ( &
		& - SAB/S2B*(dBeta1KanUsual() + dAlphaKanUsual()) - 2D0*CAB*C2B/S2B2*dBeta1KanUsual() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (2)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSUsual()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSUsual() - dMh02OSUsual()) + CBA*(2D0*MA02 - Mh02)*(dBeta2KanUsual() - dAlphaKanUsual()) + &
		& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta2KanUsual()) + ( &
		& - SAB/S2B*(dBeta2KanUsual() + dAlphaKanUsual()) - 2D0*CAB*C2B/S2B2*dBeta2KanUsual() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (3)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBeta1KanAlter() - dAlphaKanAlter()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta1KanAlter()) + ( &
		& - SAB/S2B*(dBeta1KanAlter() + dAlphaKanAlter()) - 2D0*CAB*C2B/S2B2*dBeta1KanAlter() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (4)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBeta2KanAlter() - dAlphaKanAlter()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta2KanAlter()) + ( &
		& - SAB/S2B*(dBeta2KanAlter() + dAlphaKanAlter()) - 2D0*CAB*C2B/S2B2*dBeta2KanAlter() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (5)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBeta1PinchPStar() - dAlphaPinchPStar()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta1PinchPStar()) + ( &
		& - SAB/S2B*(dBeta1PinchPStar() + dAlphaPinchPStar()) - 2D0*CAB*C2B/S2B2*dBeta1PinchPStar() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (6)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBeta2PinchPStar() - dAlphaPinchPStar()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta2PinchPStar()) + ( &
		& - SAB/S2B*(dBeta2PinchPStar() + dAlphaPinchPStar()) - 2D0*CAB*C2B/S2B2*dBeta2PinchPStar() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (7)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBeta1PinchOS() - dAlphaPinchOS()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta1PinchOS()) + ( &
		& - SAB/S2B*(dBeta1PinchOS() + dAlphaPinchOS()) - 2D0*CAB*C2B/S2B2*dBeta1PinchOS() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (8)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBeta2PinchOS() - dAlphaPinchOS()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBeta2PinchOS()) + ( &
		& - SAB/S2B*(dBeta2PinchOS() + dAlphaPinchOS()) - 2D0*CAB*C2B/S2B2*dBeta2PinchOS() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (9)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSUsual()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSUsual() - dMh02OSUsual()) + CBA*(2D0*MA02 - Mh02)*(dBetaProcDep1Usual() - dAlphaProcDep1Usual()) + &
		& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBetaProcDep1Usual()) + ( &
		& - SAB/S2B*(dBetaProcDep1Usual() + dAlphaProcDep1Usual()) - 2D0*CAB*C2B/S2B2*dBetaProcDep1Usual() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (10)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBetaProcDep1Alter() - dAlphaProcDep1Alter()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBetaProcDep1Alter()) + ( &
		& - SAB/S2B*(dBetaProcDep1Alter() + dAlphaProcDep1Alter()) - 2D0*CAB*C2B/S2B2*dBetaProcDep1Alter() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (11)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSUsual()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSUsual() - dMh02OSUsual()) + CBA*(2D0*MA02 - Mh02)*(dBetaProcDep2Usual() - dAlphaProcDep2Usual()) + &
		& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBetaProcDep2Usual()) + ( &
		& - SAB/S2B*(dBetaProcDep2Usual() + dAlphaProcDep2Usual()) - 2D0*CAB*C2B/S2B2*dBetaProcDep2Usual() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (12)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBetaProcDep2Alter() - dAlphaProcDep2Alter()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBetaProcDep2Alter()) + ( &
		& - SAB/S2B*(dBetaProcDep2Alter() + dAlphaProcDep2Alter()) - 2D0*CAB*C2B/S2B2*dBetaProcDep2Alter() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (13)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSUsual()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSUsual() - dMh02OSUsual()) + CBA*(2D0*MA02 - Mh02)*(dBetaProcDep3Usual() - dAlphaProcDep3Usual()) + &
		& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBetaProcDep3Usual()) + ( &
		& - SAB/S2B*(dBetaProcDep3Usual() + dAlphaProcDep3Usual()) - 2D0*CAB*C2B/S2B2*dBetaProcDep3Usual() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case (14)
		totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(CBA*&
		&(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZHHh0OSAlter()/2D0 ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(EL*CBA*(MA02 &
		&- Mh02)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*((2D0*MA02 - Mh02)*SBA + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*( &
		& SBA*(2D0*dMA02OSAlter() - dMh02OSAlter()) + CBA*(2D0*MA02 - Mh02)*(dBetaProcDep3Alter() - dAlphaProcDep3Alter()) + &
		& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2/EL2*C2B/S2B*dBetaProcDep3Alter()) + ( &
		& - SAB/S2B*(dBetaProcDep3Alter() + dAlphaProcDep3Alter()) - 2D0*CAB*C2B/S2B2*dBetaProcDep3Alter() )*(2D0*Mh02 - &
		& 4D0*Lambda5*MW2*SW2/EL2) &
		& )
	case default
		totalAmplitude = 0D0
 end select

 h0toA0A0CT = totalAmplitude
end function h0toA0A0CT
