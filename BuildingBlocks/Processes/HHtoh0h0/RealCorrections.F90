double precision function HHtoh0h0Real()
 use constants
 use counterterms
 implicit none
#include "looptools.h"

 double precision :: totalAmplitude
 double precision :: p2, p3, E2, E3, I11, I22, I33, I12, I13, I23, m1, m2, m3

 m1 = MHH
 m2 = Mh0
 m3 = Mh0

 p2 = DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2)/(2D0*m1)
 p3 = DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2)/(2D0*m1)
 E2 = DSQRT( m2**2 + p2**2 )
 E3 = DSQRT( m3**2 + p3**2 )

 I11 = 2D0*PI*( DLOG(4D0*DelE**2/IRLambda) - 2D0 )
 I22 = 2D0*PI*( DLOG(4D0*DelE**2/IRLambda) + E2*DLOG( (E2-p2)/(E2+p2) )/p2 )
 I33 = 2D0*PI*( DLOG(4D0*DelE**2/IRLambda) + E3*DLOG( (E3-p3)/(E3+p3) )/p3 )
 I12 = PI*E2/(2D0*p2)*( 2D0*DLOG((E2+p2)**2/m2**2)*DLOG(4D0*DelE**2/IRLambda) - DLOG((E2-p2)/(E2+p2))**2 ) -&
     & 4D0*Li2(2D0*p2/(E2+p2))
 I13 = PI*E3/(2D0*p3)*( 2D0*DLOG((E3+p3)**2/m3**2)*DLOG(4D0*DelE**2/IRLambda) - DLOG((E3-p3)/(E3+p3))**2 ) -&
     & 4D0*Li2(2D0*p3/(E3+p3))
 I23 = PI*(E2*E3+p2**2)/(2D0*p2*(E2+E3))*( 2D0*DLOG((E2+p2)**2/m2**2)*DLOG(4D0*DelE**2/IRLambda) +&
     & 2D0*DLOG((E3+p3)**2/m3**2)*DLOG(4D0*DelE**2/IRLambda) - DLOG((E2+p2)**2/m2**2)**2 - DLOG((E3+p3)**2/m3**2)**2 -&
     & 4D0*Li2(2*p2/(E2+p2)) - 4D0*Li2(2*p3/(E3+p3)) )

 totalAmplitude = 0D0

 HHtoh0h0Real = totalAmplitude
end function HHtoh0h0Real
