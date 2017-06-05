C
C =====================================================================
      SUBROUTINE STRE(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, 
     ;                ELDS, LDELDS, DSPG)
C
C     Compute the member end forces, internal 
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDCON, LDCRD, LDIDOF, LDELDS, 
C     ..
C     .. Array Arguments ..
C     INTEGER*4        CON(LDCON, *)  : Connectivity matrix
C                      IDOF(LDIDOF, *): Matrix of the degrees of freedom
C
C     REAL*8           CRD(LDCRD, *)  : Matrix of coordinates
C                      ELDS(LDELDS, *): Matrix of element loads
C                      DSPG(*)        : Global nodal displacement vector
C     ..
C     .. Local Scalars ..
C     INTEGER*4        SNOD : Start node of the element
C                      ENOD : End node of the element
C                      CDOF : Current DOF inside a loop
C                      RI   : Raw index  
C                      CI   : Column index  
C
C     REAL*8           SX   : x-coordinate of the start node
C                      SY   : y-coordinate of the start node
C                      LX   : Member length in x direction
C                      LY   : Member length in y direction
C                      L    : Total length of member
C                      DIV  : Number of subdivisions along the length
C                      STP  : Current step
C                      INC  : Increment 
C                      AXF  : Axial force
C                      SHF  : Shear force
C                      BMO  : Bending moment
C                      DV   : Vertical displacement
C                      PX   : Distributed load axial load
C                      PY1  : Value of distributed vertical load at SNOD
C                      PY2  : Value of distributed vertical load at ENOD
C                      QY   : Difference between PY2 and PY1
C                      N1   : Axial force at member end (start)
C                      T1   : Shear force at member end (start)
C                      M1   : Bending moment at member end (start)
C                      D1   : Vertical displacement at member end
C                      THE1 : Rotation at member end
C                 
C                      
C     ..
C     .. Local Arrays ..
C     INTEGER*4        ELDOF(6) : Global DOFs corresponding to locals 
C
C     REAL*8           T(6, 6)        : Transformation matrix
C                      KL(6, 6)       : Local stiffness matrix
C                      FL(6)          : Local force matrix
C                      DSPL(6)        : Local force matrix
C     ..
C     .. Common Scalars ..
C     INTEGER*4        NNODE: number of nodes
C                      NELE : number of elements
C                      NSEC : number of cross sections
C                      NMAT : number of materials
C                      NRST : number of restraints
C                      NLNK : number of links
C                      NDOF : number of degrees of freedom
C                      NNDL : number of nodal loads
C                      NELL : number of element loads
C     ,,
C     .. Scalar Arguments ..
      INTEGER*4        LDCON, LDIDOF, LDCRD, LDELDS
C     ..
C     .. Array Arguments ..
      INTEGER*4        CON(LDCON, *), IDOF(LDIDOF, *)
      REAL*8           CRD(LDCRD, *), ELDS(LDELDS, *), DSPG(*)
C     ..
C =====================================================================
C     .. Local Scalars ..
      INTEGER*4        SNOD, ENOD, RI, CI, CDOF
C     ..
C     .. Local Arrays ..
      REAL*8           T(6, 6), KL(6, 6), FEF(6), FL(6), DSPL(6), SX,
     ;                 SY, LX, LY, L, DIV, INC, STP, PX, PY1, PY2, N1,
     ;                 T1, M1, D1, THE1
      INTEGER*4        ELDOF(6)

C     .. Common Scalars ..
      INTEGER*4        NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
      COMMON /CONFIG/  NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
C     ..
C     .. Executable statements ..
C
C     Rewind the unformatted data file and open internal stress files
C     
      REWIND(16)
      OPEN(UNIT=18, FILE='axial.out')
      OPEN(UNIT=20, FILE='shear.out')
      OPEN(UNIT=22, FILE='moment.out')
      OPEN(UNIT=24, FILE='disp.out')
C
C     Loop over elements
C
      DO 10 I = 1, NELE
C
C     Retrieve the matrices, initialize FL and DSPL
C     
         READ(16) T
         READ(16) KL        
         READ(16) FEF
         WRITE(15, *)
         WRITE(15, '(A, I3, A)') 'Element', I, ': KL in STRE'
         DO 11 J = 1, 6
            WRITE(15, *)
            DO 12 K = 1, 6
               WRITE(15, '(D14.4)', ADVANCE='NO') KL(J, K)
   12       CONTINUE
   11    CONTINUE
         WRITE(15, *)
         WRITE(15, '(A, I3, A)') 'Element', I, ': T in STRE'
         DO 13 J = 1, 6
            WRITE(15, *)
            DO 14 K = 1, 6
               WRITE(15, '(F8.4)', ADVANCE='NO') T(J, K)
   14       CONTINUE
   13    CONTINUE
         WRITE(15, *)
         WRITE(15, '(A, I3, A)') 'Element', I, ': FEF in STRE'
         DO 15 J = 1, 6
               WRITE(15, '(F8.4)') FEF(J)
   15    CONTINUE

         WRITE(15, *)
         FL = 0.D0
         DSPL = 0.D0
C
C     Retrieve global displacements, transform to local coordinates
C
         SNOD = CON(I, 1)
         ENOD = CON(I, 2)
         DO 20 J = 1, 3
            ELDOF(J) = IDOF(SNOD, J)
   20    CONTINUE
         DO 30 J = 1, 3
            ELDOF(J+3) = IDOF(ENOD, J)
   30    CONTINUE
         DO 40 J = 1, 6
            CDOF = ELDOF(J)
            IF( CDOF.EQ.-1 ) THEN
               DSPL(J) = 0
               GO TO 40
            ENDIF
            DSPL(J) = DSPG(CDOF)
   40    CONTINUE
         WRITE(15,*)
         WRITE(15, '(A, I3, A)') 'Element', I, ': DSPL before transf.'
         DO 16 J = 1, 6
            WRITE(15, '(F14.8)') DSPL(J)
   16    CONTINUE
         DSPL = MATMUL(TRANSPOSE(T), DSPL)
         WRITE(15,*)
         WRITE(15, '(A, I3, A)') 'Element', I, ': DSPL after transf.'
         DO 17 J = 1, 6
            WRITE(15, '(F14.8)') DSPL(J)
   17    CONTINUE

C
C     Compute the member end forces
C
         FL = MATMUL(KL, DSPL) + FEF
         WRITE(15, *)
         WRITE(15, '(A, I3, A)') 'Element', I, ': Member end forces'
         DO 18 J = 1, 6
            WRITE(15, *) FL(J) 
   18    CONTINUE
C
C     Compute internal stresses and displacements inside the element
C
         DIV = 40.D0
         SCX = CRD(SNOD, 1)
         SCY = CRD(SNOD, 2)
         ECX = CRD(ENOD, 1)
         ECY = CRD(ENOD, 2)
         LY  = ECY - SCY
         LX  = ECX - SCX
         L   = DSQRT(LX**2 + LY**2)
         INC = L / DIV
         STP = 0.D0
         N1  = -FL(1)
         T1  = FL(2)
         M1  = -FL(3)
         PX  = ELDS(I, 1)
         PY1 = ELDS(I, 2)
         PY2 = ELDS(I, 3)
         QY  = PY2 - PY1
         D1  = DSPL(2)
         THE1= DSPL(3)
         WRITE(18, '(A, I3)') 'Element', I
         WRITE(20, '(A, I3)') 'Element', I
         WRITE(22, '(A, I3)') 'Element', I
         WRITE(24, '(A, I3)') 'Element', I
         WRITE(18, '(F15.4 F15.4)') STP, N1
         WRITE(20, '(F15.4, F15.4)') STP, T1
         WRITE(22, '(F15.4, F15.4)') STP, M1
         WRITE(24, '(F15.4, F15.4)') STP, DV
         WRITE(18, *) STP, L
   60    IF( DABS(STP - L).GT.1.D-10 ) THEN
            STP = STP + INC
            AXF = N1 - PX*STP
            SHF = T1 + PY1*STP + QY*STP**2/(2.D0*L)
            BMO = M1 + T1*STP + PY1*STP**2/2.D0 + QY*STP**3/(6.D0*L)
            WRITE(18, '(F15.4 F15.4)') STP, AXF
            WRITE(20, '(F15.4, F15.4)') STP, SHF
            WRITE(22, '(F15.4, F15.4)') STP, BMO
            GO TO 60
         ENDIF
   10 CONTINUE
      RETURN
      END
         
