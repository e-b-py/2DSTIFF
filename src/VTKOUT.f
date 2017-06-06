C
C =====================================================================
      SUBROUTINE VTKOUT(DIV, CON, LDCON, CRD, LDCRD) 
C
C     Create the common parts of all vtk files
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDCON, LDCRD
C     REAL*8           DIV
C     ..
C     .. Array Arguments ..
C     INTEGER*4        CON(LDCON, *)  : Connectivity matrix
C
C     REAL*8           CRD(LDCRD, *)  : Matrix of coordinates
C     ..
C     .. Local Scalars ..
C     INTEGER*4        NPTS : number of points in vtk dataset
C                      NCELL: number of cells in vtk dataset
C                      SNOD : start node of the element
C                      ENOD : end node of the element
C
C     REAL*8           SCX  : Start node x coordinate of the element
C                      SCY  : Start node y coordinate of the element
C                      ECX  : End node x coordinate of the element
C                      ECY  : End node y coordinate of the element
C                      LX   : Member length in x direction
C                      LY   : Member length in y direction
C                      L    : Total length of member 
C                      CSA  : Direction cosine - x
C                      SNA  : Direction cosine - y
C                      INC  : Increment of local length during loop
C                      STP  : Current step
C                      CRX  : Current x coordinate
C                      CRY  : Current y coordinate
C                      
C     ..
C     .. Local Arrays ..
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
      INTEGER*4        LDCON, LDCRD
      REAL*8           DIV
C     ..
C     .. Array Arguments ..
      INTEGER*4        CON(LDCON, *)
      REAL*8           CRD(LDCRD, *)
C     ..
C =====================================================================
C     .. Local Scalars ..
      INTEGER*4        NPTS, NCELL, SNOD, ENOD
      REAL*8           SCX, SCY, ECX, ECY, LX, LY, L, CSA, SNA, INC,
     ;                 STP, CRX, CRY
C     ..
C     .. Local Arrays ..

C     .. Common Scalars ..
      INTEGER*4        NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
      COMMON /CONFIG/  NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
C     ..
C     .. Executable statements ..
C
C     Write the headers
C      
      NPTS = NELE*(IDINT(DIV)+1)
      DO 10 I = 18, 26, 2
         WRITE(I, '(A)') '# vtk DataFile Version 2.0'      
         WRITE(I, '(A)') '2DSTIFF output'      
         WRITE(I, '(A)') 'ASCII'      
         WRITE(I, '(A)') 'DATASET UNSTRUCTURED_GRID'      
         WRITE(I, '(A, I6, A)') 'POINTS', NPTS, ' float'
   10 CONTINUE
C           
C     Loop over elements, write point data
C      
      DO 20 I = 1, NELE
         SNOD = CON(I, 1)
         ENOD = CON(I, 2)
         SCX  = CRD(SNOD, 1)
         SCY  = CRD(SNOD, 2)
         ECX  = CRD(ENOD, 1)
         ECY  = CRD(ENOD, 2)
         LX   = ECX - SCX
         LY   = ECY - SCY
         L    = DSQRT(LX**2 + LY**2)
         CSA  = LX / L
         SNA  = LY / L
         INC  = L / DIV
         STP  = 0.D0
         DO 30 J = 18, 26, 2
            WRITE(J, '(F24.16, F24.16)') SCX, SCY
   30    CONTINUE
   40    IF ( DABS(STP - L).GT.1.D-10 ) THEN
            STP = STP + INC
            CRX = SCX + STP*CSA
            CRY = SCY + STP*SNA
            DO 50 J = 18, 26, 2
               WRITE(J, '(F24.16, F24.16)') CRX, CRY
   50       CONTINUE
            GO TO 40
         ENDIF 
   20 CONTINUE
C           
C     Cell data
C      
      NCELL = NELE*IDINT(DIV)
      DO 60 J = 18, 26, 2
         WRITE(J, '(/A, I6, I6)') 'CELLS', NCELL, NCELL*3
   60 CONTINUE
C           
C     Loop over elements again (connection nodes are duplicate in point
C     data, therefore increment num when the element changes
C      
      NUM = 0 
      DO 70 I = 1, NELE
         DO 80 J = 1, IDINT(DIV)
            DO 90 K = 18, 26, 2
               WRITE(K, '(I3, I4, I4)') 2, NUM, NUM+1
   90       CONTINUE
         NUM = NUM + 1
   80    CONTINUE
      NUM = NUM + 1
   70 CONTINUE
C           
C     Cell types
C      
      DO 100 J = 18, 26, 2
         WRITE(J, '(/A, I6)') 'CELL_TYPES', NCELL
  100 CONTINUE
      DO 110 I = 1, NCELL
         DO 120 J = 18, 24, 2
            WRITE(J, '(I3)') 3
  120    CONTINUE
  110 CONTINUE
C           
C     Point data headers (skip 26 for this <undeformed>)
C      
      DO 130 J = 18, 24, 2
         WRITE(J, '(/A, I6)') 'POINT_DATA', NPTS
  130 CONTINUE
      WRITE(18, '(A)') 'VECTORS axial_force float'
      WRITE(20, '(A)') 'VECTORS shear_force float'
      WRITE(22, '(A)') 'VECTORS bending_moment float'
      WRITE(24, '(A)') 'VECTORS deformed_shape float'
      RETURN
C
C     .. End of VTKOUT ..
C
      END


