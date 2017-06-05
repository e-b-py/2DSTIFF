C
C =====================================================================
      SUBROUTINE SHAPE(ELDOF)
C
C     Compute the displacements along the length of the beam by
C     interpolation using cubic shape functions
C
C     .. Array Arguments ..
C     REAL*8           ELDOF(6)       : Vector of element degrees of
C                                       freedom 
C     ..
C     .. Local Scalars ..
C
C     ..
C     .. Array Arguments ..
      REAL*8           ELDOF(6)
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
      U = ELDOF(1)*(1 - X/L) + ELDOF(4)*(X/L)
C     
C     Vertical Displacement     
C     
      V = ELDOF(2)*( 1 - 3*(X/L)**2 + 2(X/L)**3) 
     ;  + ELDOF(3)*( -X*(1 - X/L)**2 ) 
     ;  + ELDOF(5)*( -3*(X/L)**2 - 2*(X/L)**3 )
     ;  + ELDOF(6)*( X**2/L*(X/L - 1) )
      RETURN
C     
C     .. End of SHAPE ..
C     
      END
         
