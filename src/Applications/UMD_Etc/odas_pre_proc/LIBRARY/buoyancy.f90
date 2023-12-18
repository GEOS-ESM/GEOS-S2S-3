!DIR$ ID "@(#)    buoyancy.f90 2.2 2/19/98 P Schopf"
!
! This routine modified by D. Walsh (10/20/98) to incorporate 
! Dan Wright's higher order approximation to EOS80, using the 
! "extended formula fit over the reduced range."
! (Wright, D.G., 1997, J. Atmos. & Oceanic Technology 735-740)
!
! 2/11/99: MODIFIED AGAIN BY D.WALSH.  NOW THE "DBDTEMP" AND
! "DBDSALT" ROUTINES ARE RE-CODED AND THE POLYNOMIALS IN THE
! EOS ARE EXPLICITY DIFFERENTIATED, RATHER THAN CALLING "BUOY"
! TWICE AND USING A FIRST-DIFFERENCE.  THIS SHOULD GET AROUND 
! SOME OF THE NUMERICAL PRECISION PROBLEMS WE WERE HAVING BEFORE.
!
! 4/15/99 modified to work with salinity ANOMALIES rather than
!         actual salinity values...

    module BUOYANCY_MODULE
      
      
      implicit none
      
      private
      
      public :: BUOYANCY, DBDTEMP, DBDSALT, DBYNCY, POTEMP, INSITU
      
      public :: BUOYANCY_TREF,BUOYANCY_SREF,SET_TREF,SET_SREF
      
      
      interface BUOYANCY
       module procedure BYNCY0
       module procedure BYNCY1
       module procedure BYNCY2
       module procedure BYNCY3
       module procedure BYNCY4
       module procedure BYNCY1P
       module procedure BYNCY2P
       module procedure BYNCY3P
       module procedure BYNCY4P
      end interface
      
      interface DBDTEMP
       module procedure DBDTEMP0
       module procedure DBDTEMP1
       module procedure DBDTEMP2
       module procedure DBDTEMP3
       module procedure DBDTEMP4
       module procedure DBDTEMP1P
       module procedure DBDTEMP2P
       module procedure DBDTEMP3P
       module procedure DBDTEMP4P
      end interface
      
      interface DBDSALT
       module procedure DBDSALT0
       module procedure DBDSALT1
       module procedure DBDSALT2
       module procedure DBDSALT3
       module procedure DBDSALT4
       module procedure DBDSALT1P
       module procedure DBDSALT2P
       module procedure DBDSALT3P
       module procedure DBDSALT4P
      end interface
      
      interface DBYNCY
       module procedure DBYNCY0
       module procedure DBYNCY1
       module procedure DBYNCY2
       module procedure DBYNCY3
       module procedure DBYNCY4
       module procedure DBYNCY1P
       module procedure DBYNCY2P
       module procedure DBYNCY3P
       module procedure DBYNCY4P
      end interface
      
      interface POTEMP
       module procedure POTEMP0
       module procedure POTEMP1
       module procedure POTEMP2
       module procedure POTEMP3
       module procedure POTEMP4
       module procedure POTEMP1P
       module procedure POTEMP2P
       module procedure POTEMP3P
       module procedure POTEMP4P
      end interface
      interface INSITU
       module procedure INSITU0
       module procedure INSITU1
       module procedure INSITU2
       module procedure INSITU3
       module procedure INSITU4
       module procedure INSITU1P
       module procedure INSITU2P
       module procedure INSITU3P
       module procedure INSITU4P
       module procedure INSITU0P2
      end interface
      
      real, parameter :: TLAPSRT  = 0.4E-04

      real, parameter :: REF_DENSITY = 30.0
      real, parameter :: Gfact = 9.7976/(1000.0+REF_DENSITY)

      real, parameter :: a0  =  7.057924e-4;
      real, parameter :: a1  =  3.480336e-7;
      real, parameter :: a2  = -1.112733e-7;

      real, parameter :: b0  =  5.790749e+8;
      real, parameter :: b1  =  3.516535e+6;
      real, parameter :: b2  = -4.002714e+4;
      real, parameter :: b3  =   2.084372e+2;
      real, parameter :: b4  =  5.944068e+5;
      real, parameter :: b5  = -9.643486e+3;

      real, parameter :: c0  =  1.704853e+5;
      real, parameter :: c1  =  7.904722e+2;
      real, parameter :: c2  = -7.984422;
      real, parameter :: c3  =  5.140652e-2;
      real, parameter :: c4  = -2.302158e+2;
      real, parameter :: c5  = -3.079464;

      real :: TREF = (-1.0)
      real :: SREF = 34.75

      real :: BREF = - Gfact* ( &
( b0+(-1.0)*(b1+(-1.0)*(b2+b3*(-1.0))+b5*(34.75))+b4*(34.75) ) / &
(   c0+(-1.0)*(c1+(-1.0)*(c2+c3*(-1.0))+c5*34.75)+c4*34.75 &
+ (a0+a1*(-1.0)+a2*(34.75)) * ( b0+(-1.0)*(b1+(-1.0)*(b2+b3*(-1.0))+ &
b5*(34.75))+b4*(34.75) )  )  - 1000.0 - REF_DENSITY )
     
      CONTAINS

      real function BUOYANCY_TREF()
      BUOYANCY_TREF = TREF
      end function BUOYANCY_TREF
      
      real function BUOYANCY_SREF()
      BUOYANCY_SREF = SREF
      end function BUOYANCY_SREF

      subroutine SET_TREF(T)
      real, intent(in) :: T
      TREF = T
      BREF = BYNCY0(TREF,SREF,0.0)       
      end subroutine SET_TREF

      subroutine SET_SREF(S)
      real, intent(in) :: S
      SREF = S
      BREF = BYNCY0(TREF,SREF,0.0)       
      end subroutine SET_SREF

#define D1	DIMENSION(:)
#define D2	DIMENSION(:,:)
#define D3	DIMENSION(:,:,:)
#define D4	DIMENSION(:,:,:,:)

#define R1  DIMENSION( SIZE(T) )
#define R2  DIMENSION( SIZE(T(:,:),DIM=1), SIZE(T(:,:),DIM=2) )
#define R3  DIMENSION( SIZE(T,DIM=1), SIZE(T,DIM=2), SIZE(T,DIM=3))
#define R4  DIMENSION( SIZE(T,DIM=1), SIZE(T,DIM=2), SIZE(T,DIM=3),SIZE(T,DIM=4))

#define R11  DIMENSION( SIZE(T1) )
#define R12  DIMENSION( SIZE(T1,DIM=1), SIZE(T1,DIM=2) )
#define R13  DIMENSION( SIZE(T1,DIM=1), SIZE(T1,DIM=2), SIZE(T1,DIM=3))
#define R14  DIMENSION( SIZE(T1,DIM=1), SIZE(T1,DIM=2), SIZE(T1,DIM=3),SIZE(T1,DIM=4))

#define R2P  DIMENSION( SIZE(P,DIM=1), SIZE(P,DIM=2) )

!------------------------

#define p_0      b0+T*(b1+T*(b2+b3*T)+b5*(S+35.))+b4*(S+35.)
#define alpha_0  (a0+a1*T+a2*(S+35.))
#define lambda   c0+T*(c1+T*(c2+c3*T)+c5*(S+35.))+c4*(S+35.)
#define NUMERATOR     (P1+p_0)
#define DENOMINATOR    lambda+alpha_0*NUMERATOR
#define NUMERATOR2     (P+p_0)
#define DENOMINATOR2    lambda+alpha_0*NUMERATOR2

  function BYNCY0 (T,S,P) RESULT(b)
      real, intent(in) :: T,S
      real, optional   :: P
      real	       :: b
      real             :: top, bottom, P1
      P1 = 0.
      if (present(P)) P1 = P
      top = NUMERATOR
      bottom = DENOMINATOR
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function 
  function BYNCY1 (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S
      real, optional      :: P
      real, R1		  :: b
      real, R1            :: top, bottom, P1
      P1 = 0.
      if (present(P)) P1 = P
      top = NUMERATOR
      bottom = DENOMINATOR
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function 
  function BYNCY2 (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S
      real, optional      :: P
      real, R2		  :: b
      real, R2          :: top, bottom,  P1
      P1 = 0.
      if (present(P)) P1 = P
      top = NUMERATOR
      bottom = DENOMINATOR
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function
  function BYNCY3 (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S
      real, optional      :: P
      real, R3		  :: b
      real, R3            :: top, bottom,  P1
      P1 = 0.
      if (present(P)) P1 = P
      top = NUMERATOR
      bottom = DENOMINATOR
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function 
  function BYNCY4 (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S
      real, optional      :: P
      real, R4		  :: b
      real, R4          :: top, bottom, P1
      P1 = 0.
      if (present(P)) P1 = P
      top = NUMERATOR
      bottom = DENOMINATOR
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function 
  function BYNCY1P (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S,P
      real, R1	          :: b
      real, R1            :: top, bottom
      top = NUMERATOR2
      bottom = DENOMINATOR2
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function
  function BYNCY2P (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S,P
      real, R2	          :: b
      real, R2          :: top, bottom
      top = NUMERATOR2
      bottom = DENOMINATOR2
      b = - Gfact*(top/ &
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function
  function BYNCY3P (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S,P
      real, R3            :: b
      real, R3          :: top, bottom
      top = NUMERATOR2
      bottom = DENOMINATOR2
      b = - Gfact*(top/ & 
                    bottom &
                    - 1000.0 - REF_DENSITY) - BREF
  end function 
  function BYNCY4P (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S,P
      real, R4            :: b
      real, R4          :: top, bottom
      top = NUMERATOR2
      bottom = DENOMINATOR2
      b = - Gfact*(top/ &
                    bottom  &
                    - 1000.0 - REF_DENSITY) - BREF
  end function 

#undef p_0
#undef alpha_0
#undef lambda
#undef NUMERATOR
#undef DENOMINATOR
#undef NUMERATOR2
#undef DENOMINATOR2

!------------------------------------------------------          

#define p_0       b0+T*(b1+T*(b2+b3*T)+b5*(S+35.))+b4*(S+35.)
#define alpha_0   (a0+a1*T+a2*(S+35.))
#define lambda    c0+T*(c1+T*(c2+c3*T)+c5*(S+35.))+c4*(S+35.)
#define dp0ds     (b4+b5*T)
#define dp0dt     (b1 + T*(2.0*b2+3.0*b3*T) + b5*(S+35.))
#define dLds      (c4+c5*T)
#define dLdt      (c1 + T*(2.0*c2 + 3.0*c3*T) + c5*(S+35.))
#define dads      (a2)
#define dadt      (a1)

! drhods = ( (lambda+alpha_0*(p+p_0))*dp0ds - (p+p_0)*(dLds     &
!    + alpha_0*dp0ds + (p+p_0)*dads) )      &
!    / (lambda+alpha_0*(p+p_0))**2

  function DBDTEMP0 (T,S,P) RESULT(b)
      real, intent(in) :: T,S
      real, optional   :: P
      real             :: b 
      real             ::  P1

      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0dt                &
            -(P1+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P1+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2


  end function 
  function DBDTEMP1 (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S
      real, optional      :: P
      real, R1            :: b
      real, R1              :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0dt                &
            -(P1+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P1+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 

  function DBDTEMP2 (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S
      real, optional      :: P
      real, R2	          :: b
      real, R2              :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0dt                &
            -(P1+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P1+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function

  function DBDTEMP3 (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S
      real, optional      :: P
      real, R3		  :: b
      real, R3              ::  P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0dt                &
            -(P1+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P1+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 

  function DBDTEMP4 (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S
      real, optional      :: P
      real, R4	          :: b
      real, R4              ::  P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0dt                &
            -(P1+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P1+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 

  function DBDTEMP1P (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S,P
      real, R1		  :: b
      b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0dt                &
            -(P+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function 
  function DBDTEMP2P (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S,P
      real, R2		      :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0dt                &
            -(P+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function
  function DBDTEMP3P (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S,P
      real, R3		      :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0dt                &
            -(P+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function 
  function DBDTEMP4P (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S,P
      real, R4		      :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0dt                &
            -(P+p_0)*(dLdt        &
            +alpha_0*dp0dt        & 
            +(P+p_0)*dadt) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function 
      
!------------------------------------------------------          

  function DBDSALT0 (T,S,P) RESULT(b)
      real, intent(in) :: T,S
      real, optional   :: P
      real             :: b 
      real             :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0ds                &
            -(P1+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P1+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 
  function DBDSALT1 (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S
      real, optional      :: P
      real, R1            :: b
      real, R1            :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0ds                &
            -(P1+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P1+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 
  function DBDSALT2 (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S
      real, optional      :: P
      real, R2            :: b
      real, R2            :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0ds                &
            -(P1+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P1+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function
  function DBDSALT3 (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S
      real, optional      :: P
      real, R3            :: b
      real, R3            :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0ds                &
            -(P1+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P1+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 
  function DBDSALT4 (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S
      real, optional      :: P
      real, R4            :: b
      real, R4            :: P1
      P1 = 0.
      if (present(P)) P1 = P
       b = -Gfact* ( (lambda      &
            +alpha_0*(P1+p_0))     &
            *dp0ds                &
            -(P1+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P1+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P1+p_0))**2
  end function 
  function DBDSALT1P (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S,P
      real, R1            :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0ds                &
            -(P+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function 
  function DBDSALT2P (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S,P
      real, R2            :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0ds                &
            -(P+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function
  function DBDSALT3P (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S,P
      real, R3            :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0ds                &
            -(P+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function 
  function DBDSALT4P (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S,P
      real, R4            :: b
       b = -Gfact* ( (lambda      &
            +alpha_0*(P+p_0))     &
            *dp0ds                &
            -(P+p_0)*(dLds        &
            +alpha_0*dp0ds        & 
            +(P+p_0)*dads) )      &
            /(lambda              &
            +alpha_0*(P+p_0))**2
  end function 

!------------------------      

  function DBYNCY0(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in) :: T1,S1,T2,S2
      real, intent(in), optional :: P1,P2
      real             :: b
        b = BYNCY0(T1,S1) - BYNCY0(T2,S2)
  end function
  function DBYNCY1(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D1 :: T1,S1,T2,S2
      real, intent(in), optional :: P1,P2
      real, R11             :: b
        b = BYNCY1(T1,S1) - BYNCY1(T2,S2)
  end function
  function DBYNCY2(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D2 :: T1,S1,T2,S2
      real, intent(in), optional :: P1,P2
      real, R12             :: b
        b = BYNCY2(T1,S1) - BYNCY2(T2,S2)
  end function
  function DBYNCY3(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D3 :: T1,S1,T2,S2
      real, intent(in), optional :: P1,P2
      real, R13             :: b
        b = BYNCY3(T1,S1)-BYNCY3(T2,S2)
  end function
  function DBYNCY4(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D4 :: T1,S1,T2,S2
      real, intent(in), optional :: P1,P2
      real, R14             :: b
        b = BYNCY4(T1,S1) - BYNCY4(T2,S2)
  end function
  function DBYNCY1P(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D1 :: T1,S1,T2,S2,P1,P2
      real, R11             :: b
       b = BYNCY1P(T1,S1,P1) - BYNCY1P(T2,S2,P2)
  end function
  function DBYNCY2P(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D2 :: T1,S1,T2,S2,P1,P2
      real, R12             :: b
       b = BYNCY2P(T1,S1,P1) - BYNCY2P(T2,S2,P2)
  end function
  function DBYNCY3P(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D3 :: T1,S1,T2,S2,P1,P2
      real, R13             :: b
       b = BYNCY3P(T1,S1,P1) - BYNCY3P(T2,S2,P2)
  end function
  function DBYNCY4P(T1,S1,P1,T2,S2,P2) RESULT(b)
      real, intent(in),D4 :: T1,S1,T2,S2,P1,P2
      real, R14             :: b
       b = BYNCY4P(T1,S1,P1) - BYNCY4P(T2,S2,P2)
  end function

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#define EQUATION 	T - TLAPSRT * P

  function POTEMP0 (T,S,P) RESULT(b)
      real, intent(in) :: T,S
      real			   :: P
      real			   :: b
       b = EQUATION
  end function 
  function POTEMP1 (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S
      real			      :: P
      real, R1		      :: b
       b = EQUATION
  end function 
  function POTEMP2 (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S
      real			      :: P
      real, R2		      :: b
       b = EQUATION
  end function
  function POTEMP3 (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S
      real			      :: P
      real, R3		      :: b
       b = EQUATION
  end function 
  function POTEMP4 (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S
      real			      :: P
      real, R4		      :: b
       b = EQUATION
  end function 
  function POTEMP1P (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S,P
      real, R1		      :: b
       b = EQUATION
  end function 
  function POTEMP2P (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S,P
      real, R2		      :: b
       b = EQUATION
  end function
  function POTEMP3P (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S,P
      real, R3		      :: b
       b = EQUATION
  end function 
  function POTEMP4P (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S,P
      real, R4		      :: b
       b = EQUATION
  end function 
#undef EQUATION      
      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#define EQUATION	T + TLAPSRT * P

  function INSITU0 (T,S,P) RESULT(b)
      real, intent(in) :: T,S
      real			   :: P
      real			   :: b
       b = EQUATION
  end function 
  function INSITU1 (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S
      real			      :: P
      real, R1		      :: b
       b = EQUATION
  end function 
  function INSITU2 (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S
      real			      :: P
      real, R2		      :: b
       b = EQUATION
  end function
  function INSITU3 (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S
      real			      :: P
      real, R3		      :: b
       b = EQUATION
  end function 
  function INSITU4 (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S
      real			      :: P
      real, R4		      :: b
       b = EQUATION
  end function 
  function INSITU1P (T,S,P) RESULT(b)
      real, intent(in),D1 :: T,S,P
      real, R1		      :: b
       b = EQUATION
  end function 
  function INSITU2P (T,S,P) RESULT(b)
      real, intent(in),D2 :: T,S,P
      real, R2		      :: b
       b = EQUATION
  end function
  function INSITU3P (T,S,P) RESULT(b)
      real, intent(in),D3 :: T,S,P
      real, R3		      :: b
       b = EQUATION
  end function 
  function INSITU4P (T,S,P) RESULT(b)
      real, intent(in),D4 :: T,S,P
      real, R4		      :: b
       b = EQUATION
  end function 
  function INSITU0P2 (T,S,P) RESULT(b)
      real, intent(in)    :: T,S
      real, intent(in),D2 :: P
      real, R2P		  :: b
       b = EQUATION
  end function 
#undef EQUATION      
end module



