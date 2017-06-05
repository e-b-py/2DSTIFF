C
C =====================================================================
      SUBROUTINE GEOMET(CRD, LDCRD, CON, LDCON, CSEC, LDCSEC, ISEC,
     ;                  CMAT, LDCMAT, IMAT, RST, LDRST, LNK, LDLNK)
C
C     Read the node, element, cross section, material, restraint, and
C     link data provided in the input file into the related arrays.
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDCRD, LDCON, LDCSEC, LDCMAT, LDRST, LDLNK
C     ..
C     .. Array Arguments ..
C     INTEGER*4        CON(LDCON, *)  : Element connectivity matrix
C                      ISEC(*)        : Section incidence vector
C                      IMAT(*)        : Material incidence vector
C                      RST(LDRST, *)  : Matrix of restraints
C                      LNK(LDLNK, *)  : Matrix of internal links
C
C     REAL*8           CRD(LDCRD, *)  : Matrix of nodal coordinates
C                      CSEC(LDCSEC, *): Matrix of section properties
C                      CMAT(LDCMAT, *): Matrix of material properties
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
      INTEGER*4        LDCRD, LDCON, LDSEC, LDCMAT, LDRST, LDLNK
C     ..
C     .. Array Arguments ..
      INTEGER*4        CON(LDCON, *), ISEC(*), IMAT(*), RST(LDRST, *),
     ;                 LNK(LDLNK, *)
      REAL*8           CRD(LDCRD, *), CSEC(LDCSEC, *), CMAT(LDCMAT, *)
C     ..
C =====================================================================
C     .. Common Scalars ..
      INTEGER*4        NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
      COMMON /CONFIG/  NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
C     ..
C
C     .. Executable statements ..
C
C     Open the files for I/O
C
      OPEN(UNIT=11, FILE='input.dat')
      OPEN(UNIT=12, FILE='output.dat')
C
C     Read the nodal coordinates
C
C     Skip the headers
      READ(11, '(//)')
C      
      READ(11, *) NNODE
      DO 10 I = 1, NNODE
         READ(11, *) CRD(I, 1), CRD(I, 2)
   10 CONTINUE
C
C     Read elemement connectivity
C
      READ(11, *)
      READ(11, *) NELE
      DO 20 I = 1, NELE
         READ(11, *) CON(I, 1), CON(I, 2)
   20 CONTINUE
C
C     Read section properties
C
      READ(11, *)
      READ(11, *) NSEC
      DO 30 I = 1, NSEC
         READ(11, *) CSEC(I, 1), CSEC(I, 2), CSEC(I, 3), CSEC(I, 4)
   30 CONTINUE
C
C     Read section incidences
C
      READ(11, *)
      DO 40 I = 1, NELE
         READ(11, *) ISEC(I)
   40 CONTINUE
C
C     Read material properties
C
      READ(11, *)
      READ(11, *) NMAT
      DO 50 I = 1, NMAT
         READ(11, *) CMAT(I, 1), CMAT(I, 2), CMAT(I, 3), CMAT(I, 4)
   50 CONTINUE
C
C     Read material incidences
C
      READ(11, *)
      DO 60 I = 1, NELE
         READ(11, *) IMAT(I)
   60 CONTINUE
C
C     Read restraints
C
      READ(11, *)
      READ(11, *) NRST
      DO 70 I = 1, NRST
         READ(11, *) (RST(I, J), J=1,2)
   70 CONTINUE
C
C     Read links
C
      READ(11, *)
      READ(11, *) NLNK
      DO 80 I = 1, NLNK
         READ(11, *) (LNK(I, J), J=1,3)
   80 CONTINUE
C =====================================================================
C 
C Write all to output file
C 
      WRITE(12, '(A)') 'INPUT DATA: [kN/m/C]'
      WRITE(12, '(A)') '-----------------------------------------------'
      WRITE(12, '(A)') '# NODE COORDINATES'
      WRITE(12, '(A, T14, A, T26, A)') 'Node', 'x-coord', 'y-coord'
      DO 90 I = 1, NNODE
         WRITE(12, '(I3, T8, 2F12.2)') I, CRD(I, 1), CRD(I, 2)
   90 CONTINUE
      WRITE(12, '(/A)') '# SECTIONS'
      WRITE(12, '(A, T16, A, T32, A, T45, A, T56, A)') 
     ;           'Sec.No','A', 'I', 'h', 'X'
      DO 100 I = 1, NSEC
         WRITE(12, '(I3, T12, D9.4, T28, D9.4, T42, F7.4, T52, F7.4)')
     ;               I, (CSEC(I, J), J=1,4)
  100 CONTINUE
      WRITE(12, '(/A)') '# MATERIALS'
      WRITE(12, '(A, T20, A, T40, A, T56, A, T75, A)') 
     :           'Mat.No', 'E', 'v', 'alpha', 'gamma'
      DO 110 I = 1, NMAT
         WRITE(12, '(I3, T16, D9.4, T36, F6.2, T54, D9.4, T74, F7.4)')
     ;              I, (CMAT(I, J), J=1,4)
  110 CONTINUE
      WRITE(12, '(/A)') '# ELEMENTS'
      WRITE(12, '(A, T12, A, T28, A, T41, A, T56, A)') 
     ;           'El.No','Start Node', 'End Node', 'Section', 'Material'
      DO 120 I = 1, NELE
         WRITE(12, '(I3, T14, I3, T28, I3, T42, I3, T57, I3)')
     ;               I, CON(I, 1), CON(I, 2), ISEC(I), IMAT(I)
  120 CONTINUE
      WRITE(12, '(/A)') '# RESTRAINTS'
      WRITE(12, '(A, T22, A, T38, A)')
     ;           'Restraint No', 'Node', 'Direction'
      DO 130 I = 1, NRST
         WRITE(12, '(I3, T22, I3, T40, I3)')
     ;               I, RST(I, 1), RST(I, 2)
  130 CONTINUE
      WRITE(12, '(/A)') '# LINKS'
      WRITE(12, '(A, T18, A, T38, A, T54, A)')
     ;           'Link No', 'Master Node', 'Slave Node', 'Direction'
      DO 140 I = 1, NLNK
         WRITE(12, '(I3, T20, I3, T40, I3, T56, I3)') 
     ;               I, (LNK(I, J), J=1,3)
  140 CONTINUE
      RETURN
C
C     .. End of GEOMET ..
C
      END

