C
C =====================================================================
      SUBROUTINE CUBIC(DSPL, STP, U, V, L)
C
C     Compute the displacements along the length of the beam by
C     interpolation using cubic Hermitian shape functions
C
C     .. Array Arguments ..
C     REAL*8           DISPL(6) : Local displacements
C     ..
C     .. Scalar Arguments..
C     REAL*8           STP      : Local x-coordinate
C                      U        : Displacement along local x-axis
C                      V        : Displacement along local y-axis
C                      L        : Length of the element
C     ..
C     .. Array Arguments ..
      REAL*8           DSPL(6)
C     ..
C     .. Scalar Arguments..
      REAL*8           STP, U, V, L
C     ..
C =====================================================================
C     .. Local Scalars ..
        
C     ..
C     .. Local Arrays ..

C     ..
C     .. Common Scalars ..
      INTEGER*4        NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
      COMMON /CONFIG/  NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
C     ..
C     .. Executable statements ..
C
C     Axial displacement
C     
      U = DSPL(1)*(1 - STP/L) + DSPL(4)*(STP/L)
C     
C     Vertical Displacement     
C     
      V = DSPL(2)*( 1 - 3*(STP/L)**2 + 2*(STP/L)**3 ) 
     ;  + DSPL(3)*( STP - 2*(STP**2)/L + (STP**3)/(L**2) )
     ;  + DSPL(5)*( 3*(STP/L)**2 - 2*(STP/L)**3 )
     ;  + DSPL(6)*( -(STP**2)/L + (STP**3)/(L**2) )
      RETURN
C     
C     .. End of SHAPE ..
C     
      END
         
