      program trans
C PROGRAM TO TRANSPOSE A MATRIX
      common /flag/iflag
	        common /in/ win,ncon
      DIMENSION WIN(130,130),out(130,130),error(130)
      CHARACTER FORM*80
      character*40 labels(130)
      character*1 noy
      character*80 ifile,hfile,pname
      logical net,CSP
      data bigone/.0000001/,nlbl/130/
	  data error/130*0.0/
      net=.false.
      CSP=.FALSE.
      mid=0
      LFNO=9
      lfnp=10
      iflag=0
	  
	  

        pname='Space'
        hfile='/Applications/catpac/help/space.txt'
        call what(pname,hfile)
		
		noy='y'
		go to 301  ! from now on, it's always a real Galileo jw 11/7/13
		
  300 write(6,4)
    4 format('$Is this a real Galileo?')
      read(5,8)noy
    8 format(a1)

      hfile='/Applications/catpac/help/real.hlp'
      call assist(noy,hfile,*300)


301      if(noy.eq.'y'.or.noy.eq.'Y')iflag=1
      if(iflag.eq.1)go to 23

   31 write(6,3)
    3 format(' Then I need to know many objects...')
      read(5,*,err=32)ncon
        go to 41

   32 hfile='/Applications/catpac/help/ncon.hlp'
      call assist('?',hfile,*31)

   41 write(6,5)
    5 Format('$...and how many attributes?')
      read(5,*,err=42)natt
        go to 40

   42 hfile='/Applications/catpac/help/natt.hlp'
      call assist('?',hfile,*41)

   40 nobj=ncon+natt
      nlbl=natt+ncon
      go to 23

   90 print*,' Ooops! No labels here, Error Breath.'
   23 print *, ' Where are the labels?'
      read(5,12,err=90)ifile

      hfile='/Applications/catpac/help/lbl.hlp'
      call assist(ifile(1:1),hfile,*23)

      open(unit=8,file=ifile,status='old',err=90)
      do 13 i=1,nlbl
      read(8,14,err=23,end=30)labels(i)
   14 format(a40)
   13 continue

   30 if(iflag.eq.1)ncon=i-1
      if(iflag.eq.1)natt=ncon
      if(iflag.eq.1)nlbl=ncon


c      write(6,6)ncon,natt   !  debug **
    6 format(' O.K., I''ve read ',i2,' objects and '
     +/,i2,' attributes.') !  debug **
      close(8)
    2 FORMAT(8F10.4)
    
9		continue 
		ndim=3   

c    9 write(6,7)
c    7 format('$How many dimensions do you wish?') ! why would anyone want less than 3?
c      read(5,*,err=52)ndim
        go to 50

   52 hfile='/Applications/catpac/help/ndim.hlp'
      call assist('?',hfile,*9)

50		continue
		noy='y'

c  50  print*, ' Is it a CENTROID SCALAR PRODUCTS MATRIX?'
c      print*, ' (Unless you''re sure it is, it probably isn''t)'
c      read(5,8)noy

c      hfile='\galileo\help\scpin.hlp'
c      call assist(noy,hfile,*50)

      if(noy.eq.'y'.or.noy.eq.'Y')CSP=.true.
   25 print *, ' Where are the data?'

      read(5,12) ifile
   12 format(a80)

      hfile='/Applications/catpac/help/winin.hlp'
      call assist(ifile(1:1),hfile,*25)

      open(unit=8,file=ifile,status='old',err=25)
      read(8,15,end=16)form
   15 format(a80)
   16 DO 1 I=1,ncon
      READ(8,form,err=25)(WIN(I,J),J=1,natt)
    1 CONTINUE
      read(8,17,end=18)(error(k),k=1,ncon)
   17 format(11f7.4)
   18 close(unit=8)

c -- engorge data as needed --
c	  go to 60 ! ** debug ** skipping engorgement

      do 22 i=1,ncon
      do 22 j=1,ncon
      if (abs(win(i,j)).gt.bigone)bigone=abs(win(i,j)) ! find the largest
  22  continue
c		print*,'at 124, bigone=',bigone ! ** debug **
      if(bigone.gt.10)go to 60
      print*, ' Renormalizing....'
c      print*, ' Please enter scaling factor.'
c      read(5,*)dick
      dick=1000.
       factor=dick/bigone
c      print*,' dick, factor = ', dick, factor! **  debug **
      do 24 i=1,ncon
      do 24 j=1,ncon
      win(i,j)=win(i,j)*factor
c	  print*,' win(i,j), i, j =',win(i,j),i,j ! ** debug **
c and now, to enlargen further and take the log... ! jw 1/30/16
		call squash
c so let's see how that has worked ! jw 1/30/16		
   24 continue

c -- end of engorgement
   60 print*, ' And where would you like the output, Wily One?'
      read(5,15)ifile

      hfile='/Applications/catpac/help/crdsout.hlp'
      call assist(ifile(1:1),hfile,*60)


      open(unit=lfno,file=ifile,status='unknown')
      if(iflag.eq.1)go to 10
      CALL COVAR(WIN,ncon,natt,out)
      go to 11
   10 continue
      if(CSP)go to 21
      call centrx(ncon,win,mid)
   21 CALL BIGEIG(NCON,NDIM,win,labels,error,win,ncon)
      go to 20
   11 call bigeig(ncon,ndim,out,labels,error,win,nobj)
   20 STOP
      END

      SUBROUTINE COVAR(IN,M,N,out)
      REAL IN(130,130),OUT(130,130),MEAN
      character*1 noy
      character*80 ifile,hfile
C
C GENERALIZED COVARARIANCE SUBROUTINE
C IN-INPUT MATRIX
C OUT-OUTPUT COVARIANCE MATRIX OF IN (COV IN)
C RICK HOLMES AND JOE WOELFEL...MARCH 3, 1981...COPYRIGHT RESERVED
C
C STEP1.  DEVIATION SCORES
      DO 1 I=1,N
      SUM=0
      DO 2 J=1,M
      out(j,i)=0.
      SUM=SUM+IN(J,I)
2     CONTINUE
      MEAN=SUM/M
      DO 3 J=1,M
      IN(J,I)=IN(J,I)-MEAN
3     CONTINUE
1     CONTINUE

      PRINT*,' Do you want I should squash these guys, Boss?'
      read(5,12)noy
   12 format(a1)
      if(noy.ne.'y'.and.noy.ne.'Y')GO TO 8

      do 9 j=1,n
      sum=0
      do 10 i=1,m
      sum=sum+in(i,j)**2
   10 continue
      sum=sqrt(sum)
      do 11 i=1,m
      in(i,j)=in(i,j)/sum
   11 continue
    9 continue


C
C STEP2. PREMULT. IN BY IT'S TRANSPOSE
    8 DO 4 I=1,M
      DO 4 J=1,M
      DO 5 K=1,N
      OUT(I,J)=OUT(I,J)+(IN(I,K)*IN(J,K))
5     CONTINUE
      OUT(I,J)=OUT(I,J)/N
4     CONTINUE
c      write(6,6)
c      do 7 i=1,m
c      do 7 j=1,m
c      in(i,j)=out(i,j)
c    7 continue
c    6 format(' Welcome to covar!')

      if(iflag.ne.1)go to 25 ! (can't do this if not a real galileo)
      print*,' Do you want me to write out a .WIN matrix?'
      read(5,16)noy
   16 format(a1)
      if(noy.ne.'y'.and.noy.ne.'Y')go to 25
   19 print*,' Where should I put it?'
      read(5,20)ifile
   20 format(a80)
      open(unit=10,file=ifile, status='unknown',err=19)
      write(10,26)
   26 format('(6f12.4)')
      do 17 i=1,m
      write(10,18)(out(i,j),j=1,m)
   18 format(6f12.4)
   17 continue
  25  RETURN
      END
      SUBROUTINE CENTRX(MSIZE,SMEAN,concpt)
C
C         SUBROUTINE CENTRIX WILL COMPUTE A DOUBLE CENTERED
C         CENTROID SCALAR PRODUCTS MATRIX ACCORDING TO THE
C         ALGORITHM PROVIDED BY YOUNG AND HOUSEHOLDER (1938)
C         AND ADAPTED BY TORGERSON (1958)
C
C      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION SQJ(130),SQK(130),SMEAN(130,130)
      INTEGER CONCPT
      character*80 hfile
C
      DO 5 K=1,MSIZE
      SQJ(K)=0.0
      SQK(K)=0.0
    5 CONTINUE
C
12    CONTINUE
C
C SQUARE ALL MATRIX ELEMENTS
C COMPUTE SUM OF SQUARES OF ROWS
C COMPUTE SUM OF SQUARES OF COLUMNS
C
611   SSQ=0.
      DO 50 J=1,MSIZE
      DO 50 K=1,MSIZE
      SMEAN(J,K)=SMEAN(J,K)**2
      SSQ=SSQ+SMEAN(J,K)
      SQJ(J)=SQJ(J)+SMEAN(J,K)
50    CONTINUE
C
      DO 70 K=1,MSIZE
      DO 70 J=1,MSIZE
      SQK(K)=SQK(K)+SMEAN(J,K)
   70 CONTINUE
C
C         COMPUTE EACH ELEMENT OF THE MATRIX
C
      N=MSIZE
      TERM=SSQ/MSIZE**2
      DO 80 J=1,MSIZE
      DO 80 K=1,MSIZE
      SMEAN(J,K)=((SQJ(J)/N)+(SQK(K)/N)-   TERM   -SMEAN(J,K))/2
   80 CONTINUE


C
C CENTER CENTROID ON MIDPOINT IF INDICATED
C
      IF(CONCPT.LE.0)GO TO 6
      DO 7 I=1,MSIZE
      CON=SMEAN(CONCPT,I)
      DO 7 J=1,MSIZE
      SMEAN(J,I)=SMEAN(J,I)-CON
7     CONTINUE
C
      DO 8 I=1,MSIZE
      CON=SMEAN(I,CONCPT)
      DO 8 J=1,MSIZE
      SMEAN(I,J)=SMEAN(I,J)-CON
8     CONTINUE
c
6     continue
      RETURN
      END

        SUBROUTINE BIGEIG(MSIZE,NDIM,Z,labels,error,win,nobj)
C
C Joseph Woelfel
C Copyright 1984
C All Rights Reserved
C
C CALCULATES NDIM EIGENVECTORS FROM AN MSIZE BY
C MSIZE MATRIX OF SCALAR PRODUCTS OR COVARIANCES
C
C THIS PROGRAM IS NOT READY FOR PRIME TIME...
C
      DIMENSION Z(130,130),VT(130),C(130),ITER(130),fax(130,130)
     +,ROOT(130),error(130),win(130,130),vec1(130),vec2(130)
      real ptot(130)
      character*40 labels(130)
      integer cutoff
      DATA T/.001/,MAXITR/200/,LFNO/6/,LFNP/9/
     +,sum1/0./,sum2/0./
C
  601 DO 150 KOUNT=1,NDIM
C
C
C
C         ITERATE TO EQUIVALENCE PRODUCT VECTORS
C
   26 DO 40 J=1,MSIZE
      VT(J)=1.0
   40 CONTINUE
      ITER(KOUNT)=1
C
C SAVE OLD TOLERANCE IN CASE OF MAXITR LIMIT ALTERATIONS
      OLDT=T
  120 DO 50 M=1,MSIZE
      C(M)=0.0
      DO 50 N=1,MSIZE
      C(M)=C(M)+Z(M,N)*VT(N)
   50 CONTINUE
      ITER(KOUNT)=ITER(KOUNT)+1
      DIV=C(1)
      DO 60 J=2,MSIZE
      IF(ABS(C(J)).GT.ABS(DIV))DIV=C(J)
   60 CONTINUE
      IF(DIV.NE.0)GO TO 9
      WRITE(LFNO,10) KOUNT
   10 FORMAT('  CANNOT FACTOR SPACE   SUM OF ELEMENTS IN ',I3,'  RO
     +W ARE 0')
      STOP
C
9     CONTINUE
      DO 70 K=1,MSIZE
      CTMP=C(K)/DIV
      IF(ABS(CTMP-VT(K))-T)70,70,90
   70 VT(K)=CTMP
      GO TO 110
   90 DO 1231 L=K ,MSIZE
      VT(L)=C(L)/DIV
 1231 CONTINUE
C
C  CHECK THE ITERATIONS AGAINST MAXITR FOR THIS ROOT...REDUCE T IF NEE
1230  IF(ITER(KOUNT).LE.MAXITR)GO TO 120
      T=T*10.
      IF(T.GE.1) GO TO 4184
c      ICC=' '
      WRITE(LFNO,1947) ICC,KOUNT,T
1947  FORMAT(A3,'MAXITR REACHED ON ROOT ',I2,' TOLERANCE REDUCED TO',
     +F13.9)
      ITER(KOUNT)=1
      GO TO 120
4184  WRITE(LFNO,4176)
4176  FORMAT('  A TOLERANCE OF 1 IS NOT NORMALLY ACCEPTABLE. '/
     +'  CHECK YOUR INPUT')
      STOP
C
C         RETRIEVE ROOT AND NORMALIZE VECTOR
C
  110 DIV=SIGN(ABS(DIV),DIV)
      ROOT(KOUNT)=DIV
      SUMC=0.0
      DO 130 K=1,MSIZE
      SUMC=SUMC+VT(K)**2
  130 CONTINUE
      P=SQRT(ABS(SUMC))
      Q=SQRT(ABS(DIV))
      DO 140 K=1,MSIZE
      FAX(K,KOUNT)=(VT(K)/P)*Q
c	  print*,'at 383, fax(k,kount)=',fax(k,kount) ! ** debug **
  140 CONTINUE
C
C RESTORE THE TOLERANCE TO THE PARAMETER VALUE
      T=OLDT
C
C        COMPUTE RESIDUAL MATRIX
C
      SGN=1
      IF(DIV.GT.0)SGN= -SGN
      DO 150 K=1,MSIZE
      DO 150 L=1,MSIZE
      Z(K,L)=Z(K,L)+SGN*(FAX(K,KOUNT)*FAX(L,KOUNT))
c	  print*,'at line 395, z(k,l)=',z(k,l) ! ** debug **
  150 CONTINUE
C          SORT VECTORS FROM NATURAL TO DESCENDING ORDER
c      NUML1=MSIZE-1
		numl1=ndim-1
 4000 NDEX=0
C
      DO 3000 IXE=1,NUML1
c	  print*,'@403,root(ixe),ixe =',root(ixe),ixe  !  ** debug **
      IF (ROOT(IXE).GE.ROOT(IXE+1)) GO TO 3000
      NDEX=1
      TMP=ROOT(IXE)
      ROOT(IXE)=ROOT(IXE+1)
      ROOT(IXE+1)=TMP
      ITMP=ITER(IXE)
      ITER(IXE)=ITER(IXE+1)
      ITER(IXE+1)=ITMP
      DO 2050 J=1,MSIZE
      TMP=FAX(J,IXE)
      FAX(J,IXE)=FAX(J,IXE+1)
 2050 FAX(J,IXE+1)=TMP
c		print*,' at 414, fax(j,ixe+1)=',fax(j,ixe+1) ! ** debug **
 3000 CONTINUE
      IF(NDEX.NE.0)GO TO 4000
C
C         COMPUTE CUMULATIVE PERCENTAGES OF DISTANCE
C
C KEEP RECORD OF THE 'ZERO-ROOT', AS THIS DETERMINES
C THE IMAGINARY PART OF THE SPACE
C
  811  RMIN=ABS(ROOT(1))
      STOT=ROOT(1)
      CUTOFF=1
      DO 525 K=2,MSIZE
      STOT=STOT+ROOT(K)
      IF(ABS(ROOT(K)).GT.RMIN)GO TO 525
      RMIN=ABS(ROOT(K))
      CUTOFF=K
  525 CONTINUE
      SREAL=0
      CUTOFF=CUTOFF-1
      DO 1806 K=1,CUTOFF
      SREAL=SREAL+ROOT(K)
 1806  CONTINUE
      SIMAG=STOT-SREAL
      WARP=SREAL/STOT
C      DIM(INCMNT)=CUTOFF
C
      DO 530 K=1,MSIZE
      PTOT(K)=     ABS  ((ROOT(K)/STOT)*100.0)
  530 CONTINUE
C
      DO 64 I=1,CUTOFF
      VT(I)=ABS(ROOT(I)/SREAL*100.)
  64  CONTINUE
      IF(CUTOFF+1.GT.MSIZE)GO TO 65
      ICT=CUTOFF+1
      DO 66 I=ICT,MSIZE
      if(simag.eq.0)go to 66
      VT(I)=ABS(ROOT(I)/SIMAG*100.)
   66 CONTINUE
  65  CONTINUE
C

      ZZERO = 0.0
      DO 790 K=1,MSIZE
790   CCUBE=ZZERO+VEC2(K)**3
      DO 793 K=1,MSIZE
793   IF(CCUBE.LE.0.0) VEC2(K)=-VEC2(K)

c     Find coordinates of attribute endpoints
      nfac=msize+1
      natt=nobj-msize
      do 1 i=1,natt
      do 2 j=1,ndim
      do 3 k=1,msize
      vec1(k)=win(k,i)
      vec2(k)=fax(k,j)
    3 continue

      do 4 k=1,msize
c      write(10,5)vec1(k),vec2(k),k
c    5 format(1x,2f10.2,i2)
    4 continue
c      print*,' att # =',i,' dim # = ',j
      call cosin(vec1,vec2,fax(nfac,j),msize)
    2 continue
      nfac=nfac+1  ! I changed this after losing crdneg -- jw
    1 continue
C
c      nobj=((nobj-msize)*2)+msize       ! goes away with crdneg -- jw
C ALL DONE, WRITE IT OUT AND GO HOME...
C
      print *, '                Eigenvalues'
      do 5904 i=1,ndim
      WRITE(LFNO,7)i,ROOT(I)
    7 FORMAT(' Root ',i2,'=,'f15.2)
 5904 continue
      print *, 'Warp Factor = ', warp
      WRITE(LFNP,5903)nobj,cutoff,NDIM
 5903 FORMAT('(6F12.4)',2X,3I3)
      DO 400 K=1,nobj
      WRITE(LFNP, 270)  (FAX(K,J),J=1,NDIM)
  270 FORMAT (6F12.4)
  400 CONTINUE
      do 401 i=1,nobj
      write(lfnp,402)labels(i)
  402 format(a40)
  401 continue
      if(error(1).eq.0)go to 404
      write(lfnp,403)(error(i),i=1,nobj)
  403 format(11f7.3)
  404 continue
      STOP
      END

      subroutine cosin(vec1,vec2,crdpos,ncon)
c This little devil finds the coordinates of the endpoints
c of attributes projected on an orthogonal coordinate system
      dimension vec1(130),vec2(130)
      AL=0
      BL=0
      SCP=0.
      DO 1 I=1,ncon
      AL=AL+VEC1(I)**2
      BL=BL+VEC2(I)**2
      SCP=SCP+VEC1(I)*VEC2(I)
  1   CONTINUE
      AL=SQRT(AL)
      BL=SQRT(BL)
      COr=SCP/(AL*BL)
c      PRINT*, ' COR = ', COR
c      READ(5,*)                 ! WAIT FOR WILLIAM
      ANG=(ACOS(COR))*57.29578
c       crdpos=cos(ang)*al
       crdpos=cor*al   ! let's try this, for a change -- jw 4/6/92
c       print*,' Angle = ', ang
c       read(5,*)     ! wait for william
c       if(ang.gt.90.and.ang.lt.180) crdpos=-crdpos
c      crdpos=crdpos/2
       crdpos=crdpos*(bl/al)    ! this is the renormalizing miracle!
c      crdneg=-crdpos
c      crdneg=cos((180-ang))*al
c      crdneg=crdneg/2
c      WRITE(10, 2) AL,BL,SCP,COR,ANG
c   2   FORMAT('0   debugging'/
c     +'   LENGTH OF attribute ',F12.3/
c     +'   LENGTH OF dimension ',F12.3/
c     +'   SCALAR PRODUCT ',F12.2/
c     +'   COSIN ',F12.6/
c     +'   ANGLE ',F7.2)
      return
      end

c______________________________Subroutine Assist____________________

      subroutine assist(ans,hfile,*)
      character*1 ans
      character*80 hfile,htext

c      print*,' hfile = ', hfile

      if(ans.eq.'?')go to 7
      go to 6

    7 open(unit=19,file=hfile,status='old',err=2)
      do 3 i=1,100
      read(19,4,end=5)htext
   4  format(a80)
      write(6,4)htext
   3  continue
      go to 5
   2  print*,' Sorry, I can''t help you. You''re on your own!'
c   5  print*,' Press ''Enter'' to Continue.'
c      read(5,*)     ! wait for william
    5 close (unit=19)
      return 1

   6  close (unit=19)
      return
      end

c___________________________Subroutine What________________________

      subroutine what(pname,hfile)
      character*1 ans
      character*80 pname,hfile,htext
      j=0

      write(6,1)pname
    1 format(//////////////////'      Hello, I''m ',a80,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//,
     +'      ([ctrl C] will send you back to Galileo Control.)',//////)

      read(5,8)ans
    8 format(a1)

      if(ans.eq.'?')go to 7
      if(ans.eq.'@')then
      hfile='/Applications/catpac/help/parrot.fun'
      go to 7
      end if
      go to 6

    7 open(unit=19,file=hfile,status='old',err=2)
      do 3 i=1,100
      j=j+1
      if(j.eq.22)then
      j=0
      print*,' Press ''ENTER'' to continue.'
      read(5,*)
      end if
      read(19,4,end=6)htext
   4  format(a80)
      write(6,4)htext
   3  continue
      go to 6
   2  print*,' Sorry, I can''t help you. You''re on your own!'

   6  close (unit=19)
      return
      end
c  now for squash from pony
	  subroutine squash
	  
	  parameter(msize=130)
      common /in/ win,ncon

      real*4 win(msize,msize)
	  print*,' Here we are in squash... '
		x=100
	  do 1 i=1,ncon
	  do 1 j=1,ncon
c	  win(i,j)=win(i,j)*10
c		print*,' win(3,j) = ', win(3,j)
	  if(win(i,j).eq.0) go to 1
c	  print*,' win(i,j) before = ', win(i,j)
	  if(win(i,j).lt.0)win(i,j)=log(x*(abs((win(i,j))-1)))*(-1)
	  if(win(i,j).gt.0) win(i,j)= log(x*((win(i,j))+1))
c	  print*,' win(i,j) after = ',win(i,j)
c	  print*,'win(2,j) = ', win(2,j)
1		continue
	  return
	  end