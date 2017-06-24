C
C =====================================================================
      SUBROUTINE STRE(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, 
     ;                ELDS, LDELDS, EPRS, LDEPRS, DSPG)
C
C     Compute the member end forces, internal 
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDCON, LDCRD, LDIDOF, LDELDS, LDEPRS
C     ..
C     .. Array Arguments ..
C     INTEGER*4        CON(LDCON, *)  : Connectivity matrix
C                      IDOF(LDIDOF, *): Matrix of the degrees of freedom
C
C     REAL*8           CRD(LDCRD, *)  : Matrix of coordinates
C                      ELDS(LDELDS, *): Matrix of element loads
C                      EPRS(LDEPRS, *): Matrix of prestressing
C                      DSPG(*)        : Global nodal displacement vector
C     ..
C     .. Local Scalars ..
C     INTEGER*4        SNOD : Start node of the element
C                      ENOD : End node of the element
C                      CDOF : Current DOF inside a loop
C
C     REAL*8           SCX  : x-coordinate of the start node
C                      SCY  : y-coordinate of the start node
C                      ECX  : x-coordinate of the end node
C                      ECY  : y-coordinate of the end node
C                      LX   : Member length in x direction
C                      LY   : Member length in y direction
C                      L    : Total length of member
C                      CSA  : Direction cosine - x
C                      SNA  : Direction cosine - y
C                      DIV  : Number of subdivisions along the length
C                      STP  : Current step
C                      INC  : Increment 
C                      AXF  : Axial force
C                      SHF  : Shear force
C                      BMO  : Bending moment
C                      DV   : Vertical displacement
C                      DH   : Horizontal displacement
C                      PX   : Distributed load axial load
C                      PY1  : Value of distributed vertical load at SNOD
C                      PY2  : Value of distributed vertical load at ENOD
C                      QY   : Difference between PY2 and PY1
C                      ECC1 : Eccenctricity of cable at the start node
C                      ECC2 : Eccenctricity of cable at the end node
C                      ECCM : Eccenctricity of cable at the mid-span
C                      PPRS : Prestressing force applied at the ends
C                      THE1 : Angle of cable at the start node
C                      WP   : Balancing force to ensure cable equilibr.
C                      N1   : Axial force at member end (start)
C                      T1   : Shear force at member end (start)
C                      M1   : Bending moment at member end (start)
C                      CDSIP: Current displacement inside loop
C                      SGX  : Sign specifier for diagrams
C                      SGY  : Sign specifier for diagrams
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
C                      NRSTE: number of elastic restraints
C                      NLNKE: number of elastic links
C                      NDOF : number of degrees of freedom
C                      NNDL : number of nodal loads
C                      NELL : number of element loads
C                      NPRES: number of prestressed elements
C     ,,
C     .. Scalar Arguments ..
      INTEGER*4        LDCON, LDIDOF, LDCRD, LDELDS, LDEPRS
C     ..
C     .. Array Arguments ..
      INTEGER*4        CON(LDCON, *), IDOF(LDIDOF, *)
      REAL*8           CRD(LDCRD, *), ELDS(LDELDS, *), EPRS(LDEPRS, *),
     ;                 DSPG(*)
C     ..
C =====================================================================
C     .. Local Scalars ..
      INTEGER*4        SNOD, ENOD, CDOF
      REAL*8           SCX, SCY, ECX, ECY, LX, LY, L, CSA, SNA, DIV, 
     ;                 STP, INC, AXF, SHF, BMO, DV, DH, PX, PY1, PY2,
     ;                 QY, ECC1, ECC2, ECCM, PPRS, THE1, WP, N1, T1, M1,
     ;                 CDISP, SGX, SGY
C     ..
C     .. Local Arrays ..
      INTEGER*4        ELDOF(6)
      REAL*8           T(6, 6), KL(6, 6), FEF(6), FL(6), DSPL(6)

C     .. Common Scalars ..
      INTEGER*4       NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL, NPRES
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL, NPRES
C     ..
C     .. Executable statements ..
C
C     Write results to output file 
C
      WRITE(12, *)
      WRITE(12, '(A)') '================================================
     ;================================'
      WRITE(12, '(A)') 'ANALYSIS RESULTS'
      WRITE(12, '(A)') '================================================
     ;================================'
      WRITE(12, '(/A/)') '>> NODAL DISPLACEMENTS'     
      WRITE(12, '(A, T17, A, T39, A, T61, A)') 'Node', 'DX', 'DY', 'PHI'
      DO 901 I = 1, NNODE
         WRITE(12, '(/I3)', ADVANCE='NO') I
         DO 902 J = 1, 3
            IF( IDOF(I, J).EQ.-1 ) THEN
               CDISP = 0.D0
            ELSE
               CDISP = DSPG(IDOF(I, J))
            ENDIF
            WRITE(12, '(D22.10)', ADVANCE='NO') CDISP
  902    CONTINUE
  901 CONTINUE
      WRITE(12, '(///A/)') '>> MEMBER END FORCES'     
C      WRITE(12, '(A, T12, A, T24, A, T36, A, T48, A, T60, A, T72, A)') 
C    ; 'Member', 'N1', 'T1', 'M1', 'N2', 'T2', 'M2'
       WRITE(12, '(A, T9, A, T23, A, T36, A, T49, A, T62, A, T75, A)') 
     ; 'MEM', 'N1', 'T1', 'M1', 'N2', 'T2', 'M2'
C
C     Create the Vtk files
C
      DIV = 40.D0
      CALL VTKOUT(DIV, CON, LDCON, CRD, LDCRD)
      
C
C     Rewind the unformatted data file 
C     
      REWIND(16)
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
         WRITE(12, '(I2, 6D13.5)') I, FL(1), FL(2), FL(3), FL(4), FL(5),
     ;                            FL(6)
   19    CONTINUE
C
C     Compute internal stresses and displacements inside the element
C
         SCX = CRD(SNOD, 1)
         SCY = CRD(SNOD, 2)
         ECX = CRD(ENOD, 1)
         ECY = CRD(ENOD, 2)
         LY  = ECY - SCY
         LX  = ECX - SCX
         L   = DSQRT(LX**2 + LY**2)
         CSA = T(1, 1)
         SNA = T(2, 1)
         INC = L / DIV
         STP = 0.D0
         PX  = ELDS(I, 1)
         PY1 = ELDS(I, 2)
         PY2 = ELDS(I, 3)
         QY  = PY2 - PY1
         ECC1= EPRS(I, 1)
         ECC2= EPRS(I, 2)
         ECCM= EPRS(I, 3)
         PPRS= EPRS(I, 4)
         THE1= -1.D0/L*(3*ECC1 + ECC2 - 4*ECCM)
         WP  = PPRS*4.D0/L**2*(ECC1 + ECC2 - 2*ECCM)
         N1  = -FL(1) - PPRS
         T1  = FL(2) + PPRS*THE1
         M1  = -FL(3) + PPRS*ECC1
         DH  = DSPL(1)
         DV  = DSPL(2)
C
C     VTK warp vector components' orientation according to diagram
C     convention - positive moment top, negatvie moment bottom
C
         SGX = -1.D0
         SGY = 1.D0
C
C     Write the initial values before loop along element length
C     according to vtk warpvector convention (and transform local disp. 
C     to global 
C
         WRITE(18, '(F24.16, F24.16, F24.16)') 
     ;                                 N1*SNA*SGX, N1*CSA*SGY, 0.D0
         WRITE(20, '(F24.16, F24.16, F24.16)') 
     ;                                 T1*SNA*SGX, T1*CSA*SGY, 0.D0
         WRITE(22, '(F24.16, F24.16, F24.16)') 
     ;                                 M1*SNA*SGX, M1*CSA*SGY, 0.D0
         WRITE(24, '(F24.16, F24.16, F24.16)') 
     ;               (CSA*DH - SNA*DV), (SNA*DH + CSA*DV), 0.D0
   60    IF( DABS(STP - L).GT.1.D-10 ) THEN
            STP = STP + INC
            AXF = N1 - PX*STP
            SHF = T1 + (PY1 + WP)*STP + QY*STP**2/(2.D0*L)
            BMO = M1 + T1*STP + (PY1 + WP)*STP**2/2.D0 
     ;           + QY*STP**3/(6.D0*L)
            CALL CUBIC(DSPL, STP, DH, DV, L)
            WRITE(18, '(F24.16, F24.16, F24.16)') 
     ;                                 AXF*SNA*SGX, AXF*CSA*SGY, 0.D0
            WRITE(20, '(F24.16, F24.16, F24.16)') 
     ;                                 SHF*SNA*SGX, SHF*CSA*SGY, 0.D0
            WRITE(22, '(F24.16, F24.16, F24.16)') 
     ;                                 BMO*SNA*SGX, BMO*CSA*SGY, 0.D0
            WRITE(24, '(F24.16, F24.16, F24.16)') 
     ;                  (CSA*DH - SNA*DV), (SNA*DH + CSA*DV), 0.D0
            GO TO 60
         ENDIF
   10 CONTINUE
      WRITE(12, '(A)') '================================================
     ;================================'
      RETURN
C
C     .. End of STRE ..
C
      END
         
