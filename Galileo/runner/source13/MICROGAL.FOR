      PROGRAM MICROGAL
c    ***  Version 2.0  February 2, 1988  ***

c  Joseph Woelfel
c  Copyright 4/26/83
c  Modified 10/19/90 to output scalar products
c  All Rights reserved

c  This program reads in raw Galileo data in standard format, computes
c  standard errors around each point following Pruzak's method and
c  saves a table of errors, and a down file on user selected files.

      dimension ii(8),jj(8),dist(8),sdist(40,40),ssdist(40,40),
     +stderr(40),n(40,40)
      character*1 noy
      character*8 id
      character*40 labels(40)
      character*80 pname,hfile,nseg,ifile,ifile2
      real n
      integer fink,concpt
      data fink/0/
      data sdist/1600*0.0/,ssdist/1600*0.0/,n/1600*0/,concpt/0/,
     +filter/99999./
      data bigone/0/

c      call datwrt('Microgal_PC','V2.0')

      pname='Microgal'
      hfile='/Applications/galileo/help/microgal.doc'
      call what(pname,hfile)

 206  write(6,263)
 263  format(' Please enter name of segment.')
c      write(*,'(a,\)') ' ====>'
      read(*,300)nseg

      hfile='/Applications/galileo/help/tit.hlp'
      call assist(nseg(1:1),hfile,*206)

 2620 write(6,26)
  26  format('0Where are your labels?')
c      write(*,'(a,\)') ' ====>'
      read (*,300)ifile
 300  format(a40)

      hfile='/Applications/galileo/help/lbl.hlp'
      call assist(ifile(1:1),hfile,*2620)
      open(unit=8,file=ifile,status='old',err=2620)

c     read in concepts.

      do 2661 i=1,40
      read(8,300,end=989)labels(i)
c      print*, i, labels(i) ! ** debug
 2661 continue
 989  ncon=i-1
      close(8)

 266  write(6,265)
 265  format(' Where are your data?')
c      write(*,'(a,\)') ' ====>'
      read(5,300)ifile

      hfile='/Applications/galileo/help/data.hlp'
      call assist(ifile(1:1),hfile,*266)
      open(unit=8,file=ifile,status='old',err=266)

  201 write(6,25)
  25  format(' Enter Maximum Value, s''il vous plait.')
c      write(*,'(a,\)') ' ====>'
      read(5,*,err=205)filter
      go to 204

  205 hfile='/Applications/galileo/help/maxval.hlp'
      call assist('?',hfile,*201)

  204 write(*,381)
  381 format(' Do you want a log transform?')
c      write(*,'(a,\)') ' ====>'
      read(5,226)noy
      hfile='/Applications/galileo/help/log.hlp'
226   format(a1)
      if(noy.eq.'y'.or.noy.eq.'Y')fink=1

 267  write(6,268)
 268  format(' What do you want to name the print file?')
c      write(*,'(a,\)') ' ====>'
      read(5,300)ifile

      hfile='/Applications/galileo/help/file.hlp'
      call assist(ifile(1:1),hfile,*267)

      open(unit=18,file=ifile,status='unknown',err=267)

  269 print 617
  617 format(' ...and where would you like the coordinate file?')
c      write(*,'(a,\)') ' ====>'
      read(5,300)ifile2

      hfile='/Applications/galileo/help/crdfile.hlp'
      call assist(ifile2(1:1),hfile,*269)
      open(unit=17,file=ifile2,status='unknown',err=269)

c     read and tally

      write(*,371)
  371 format(' Reading data...')
   1  read(8,2,end=4)id,(ii(k),jj(k),dist(k),k=1,8)
   2  format(a8,8(2i2,f5.0))
      if(id.eq.'ENDOFSET')go to 4

      do 3 k=1,8
      if(ii(k) .le. 0 .or. ii(k) .gt. ncon .or. jj(k) .le. 0
     +.or. jj(k) .gt. ncon .or. dist(k) .eq. -1)go to 3

C     if(lval)then
C         if (dist(k).gt.filter)dist(k)=filter
C       else
         if(dist(k) .gt. filter) go to 3
c       endif
c
      i=ii(k)
      j=jj(k)
      if(fink.eq.1)dist(k)=alog(dist(k)+1)
      sdist(i,j)=sdist(i,j)+dist(k)
      sdist(j,i)=sdist(i,j)
      ssdist(i,j)=ssdist(i,j)+(dist(k)**2)
      ssdist(j,i)=ssdist(i,j)
      n(i,j)=n(i,j)+1
      n(j,i)=n(i,j)
      if(dist(k).gt.abs(bigone))bigone=dist(k)

C ** DEBUG **
C      open(unit=25,file='new.dbg')
C      WRITE(25,100)I,J,SDIST(I,J),SSDIST(I,J),N(I,J)
c  100 FORMAT(1X,I2,1X,I2,F10.2,1X,F10.2,1X,f3.0)
C ** DEBUG **
   3  continue
      go to 1

c       stats...

  4     close(8)
        do 5 i=1,ncon
        do 6 j=1,ncon
        if(i.eq.j)go to 6

c        if(n(i,j).le.1)then
c        se=1
c        ssdist(i,j)=.05*bigone
c        go to 66
c        endif

        se=abs(ssdist(i,j)-((sdist(i,j)**2)/n(i,j)))

        if(n(i,j).eq.1)then
        ssdist(i,j)=(sqrt(se/(n(i,j))))/sqrt(n(i,j))
        else
        ssdist(i,j)=(sqrt(se/(n(i,j)-1)))/sqrt(n(i,j)-1)
        endif

        if(fink.eq.1)ssdist(i,j)=(2.7183**ssdist(i,j))-1
        sdist(i,j)=sdist(i,j)/n(i,j)
        if(fink.eq.1)sdist(i,j)=(2.7183**sdist(i,j))-1
   6    continue
   5    continue

c       standard errors....

        call lstsqr(ncon,ssdist,stderr)

c       now write out errors

        write(18,30)nseg
  30    format('0',/a79/,20X,'Standard Errors of Estimate',//)
        do 8 i=1,ncon
   8    write(18,9)i,labels(i),stderr(i)
   9    format(5x,i2,2x,a40,' = ',f10.4)

c       coordinates......

        call centrx(ncon,sdist,concpt)
        call bigeig(ncon,ncon,sdist,labels,stderr)

        write(6,69)ifile2,ifile
  69    format(//////////' PAU.'//' Your coordinate file is on ',a40,//
     +' Your print file is on ',a40,//' Press "ENTER" to return to Galil
     +eo Control. MAHALO!'/)
        read(*,*)                 ! wait for william
        end

        SUBROUTINE CENTRX(MSIZE,SMEAN,CONCPT)
C
C         SUBROUTINE CENTRIX WILL COMPUTE A DOUBLE CENTERED
C         CENTROID SCALAR PRODUCTS MATRIX ACCORDING TO THE
C         ALGORITHM PROVIDED BY YOUNG AND HOUSEHOLDER (1938)
C         AND ADAPTED BY TORGERSON (1958)
C
        IMPLICIT REAL (A-H,O-Z)
        DIMENSION SQJ(40),SQK(40),SMEAN(40,40)
        INTEGER CONCPT
        character*1 noy
        character*80 ofile,hfile
C
        DO 5 K=1,MSIZE
        SQJ(K)=0.0
        SQK(K)=0.0
   5    CONTINUE
C
C SQUARE ALL MATRIX ELEMENTS
C COMPUTE SUM OF SQUARES OF ROWS
C COMPUTE SUM OF SQUARES OF COLUMNS
C
6611    SSQ=0.

        DO 50 J=1,MSIZE
        DO 50 K=1,MSIZE
        SMEAN(J,K)=SMEAN(J,K)**2
        SSQ=SSQ+SMEAN(J,K)
        SQJ(J)=SQJ(J)+SMEAN(J,K)
50      CONTINUE
C
        DO 70 K=1,MSIZE
        DO 70 J=1,MSIZE
        SQK(K)=SQK(K)+SMEAN(J,K)
70      CONTINUE
C
        N=MSIZE
        TERM=SSQ/MSIZE**2
        DO 80 J=1,MSIZE
        DO 80 K=1,MSIZE
        SMEAN(J,K)=((SQJ(J)/N)+(SQK(K)/N)-   TERM   -SMEAN(J,K))/2
80      CONTINUE
C
C CENTER CENTROID ON MIDPOINT IF INDICATED
C
        IF(CONCPT.LE.0)GO TO 6
        DO 7 I=1,MSIZE
        CON=SMEAN(CONCPT,I)
        DO 7 J=1,MSIZE
        SMEAN(J,I)=SMEAN(J,I)-CON
7       CONTINUE
C
        DO 8 I=1,MSIZE
        CON=SMEAN(I,CONCPT)
        DO 8 J=1,MSIZE
        SMEAN(I,J)=SMEAN(I,J)-CON
8       CONTINUE
6       CONTINUE

c_______________Routine to output scalar products 19/19/90__________

 101    print 312
 312    format('0Do you want the scalar products?')
c      write(*,'(a,\)') ' ====>'
        read(5,10)noy

      hfile='\galileo\help\scp.hlp'
      call assist(noy,hfile,*101)


   10 format(a1)
        if(noy.ne.'y'.and.noy.ne.'Y')go to 11
   13 print 431
  431 format('0Where would you like them?')
c      write(*,'(a,\)') ' ====>'
        read(5,12)ofile

      hfile='\galileo\help\scpout.hlp'
      call assist(ofile(1:1),hfile,*13)


   12 format(a80)
        open(unit=15,file=ofile,status='unknown',err=13)
        write(15,14)
   14 format('(6f12.3)')
        do 15, i=1,msize
        write(15,16)(smean(i,j),j=1,msize)
   16 format(6f12.3)
   15 continue


   11 RETURN
        END
C
        SUBROUTINE BIGEIG(MSIZE,NDIM,Z,LABELS,ERROR)
C
C  Joseph Woelfel
C  Copyright 1984
C  All Rights Reserved
C
C  CALCULATES NDIM EIGENVECTORS FROM AN MSIZE BY
C  MSIZE MATRIX OF SCALAR PRODUCTS OR COVARIANCES
C
C  THIS PROGRAM IS NOT READY FOR PRIME TIME...
C
       DIMENSION Z(40,40),VT(40),C(40),ITER(40),FAX(40,40),
     + ROOT(40),ERROR(40)
       REAL DIM(40),PTOT(40)
       CHARACTER*40 LABELS(40)
       INTEGER CUTOFF
       DATA T /.001/,MAXITR /200/,LFNI /5/,LFNO /6/,LFNP /17/,
     * SUM1 /0./,SUM2 /0./
601     DO 150 KOUNT=1,NDIM
C
C
C         ITERATE TO EQUIVALENCE PRODUCT VECTORS
C
  26    DO 40 J=1,MSIZE
        VT(J)=1.0
  40    CONTINUE
        ITER(KOUNT)=1
C
        OLDT=T
 120    DO 50 M=1,MSIZE
        C(M)=0.0
        DO 50 N=1,MSIZE
        C(M)=C(M)+Z(M,N)*VT(N)
  50    CONTINUE
        ITER(KOUNT)=ITER(KOUNT)+1
        DIV=C(1)
        DO 60 J=2,MSIZE
        IF(ABS(C(J)).GT.ABS(DIV))DIV=C(J)
  60    CONTINUE
        IF(DIV.NE.0)GO TO 9
        WRITE(LFNO,10) KOUNT
10      FORMAT('0CANNOT FACTOR SPACE  SUM OF ELEMENTS IN ', I3 ,
     1  ' ROW ARE 0')
        STOP
9       CONTINUE
        DO 70 K=1,MSIZE
        CTMP=C(K)/DIV
        IF(ABS(CTMP-VT(K))-T)70,70,90
  70    VT(K)=CTMP
        GO TO 110
  90    DO 1231 L=K ,MSIZE
        VT(L)=C(L)/DIV
1231    CONTINUE
C
230     IF(ITER(KOUNT).LE.MAXITR)GO TO 120
        T=T*10.
        IF(T.GE.1) GO TO 4184
        WRITE(LFNO,1947)KOUNT,T
        write(18,1947)kount,t
1947    FORMAT('0MAXITR REACHED ON ROOT', I2 ,'TOLERANCE REDUCED TO',
     2  F13.9)
        ITER(KOUNT)=1
        GO TO 120
4184    WRITE(LFNO,4176)
        write(18,4176)
4176    FORMAT('0A TOLERANCE OF 1 IS NOT NORMALLY ACCEPTABLE. '/,
     +   'CHECK YOUR INPUT')
        STOP
C
 110    DIV=SIGN(ABS(DIV),DIV)
        ROOT(KOUNT)=DIV
        SUMC=0.0
        DO 130 K=1,MSIZE
        SUMC=SUMC+VT(K)**2
 130    CONTINUE
        P=SQRT(ABS(SUMC))
        Q=SQRT(ABS(DIV))
        DO 140 K=1,MSIZE
        FAX(K,KOUNT)=(VT(K)/P)*Q
 140    CONTINUE
C
C      RESTORE THE TOLERANCE TO THE PARAMETER VALUE
C
        T=OLDT
C
C        COMPUTE RESIDUAL MATRIX
C
        SGN=1
        IF(DIV.GT.0) SGN= -SGN
        DO 150 K=1,MSIZE
        DO 150 L=1,MSIZE
        Z(K,L)=Z(K,L)+SGN*(FAX(K,KOUNT)*FAX(L,KOUNT))
 150    CONTINUE
C          SORT VECTORS FROM NATURAL TO DESCENDING ORDER
        NUML1=MSIZE-1
4000    NDEX=0
C
        DO 3000 IXE=1,NUML1
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
2050    FAX(J,IXE+1)=TMP
3000    CONTINUE
        IF(NDEX.NE.0)GO TO 4000
C
C         COMPUTE CUMULATIVE PERCENTAGES OF DISTANCE
C
C KEEP RECORD OF THE 'ZERO-ROOT', AS THIS DETERMINES
C THE IMAGINARY PART OF THE SPACE
C
 811    RMIN=ABS(ROOT(1))
        STOT=ROOT(1)
        CUTOFF=1
        DO 525 K=2,MSIZE
        STOT=STOT+ROOT(K)
        IF(ABS(ROOT(K)).GT.RMIN)GO TO 525
        RMIN=ABS(ROOT(K))
        CUTOFF=K
 525    CONTINUE
        SREAL=0
        CUTOFF=CUTOFF-1
        DO 1806 K=1,CUTOFF
        SREAL=SREAL+ROOT(K)
1806    CONTINUE
        SIMAG=STOT-SREAL
        WARP=SREAL/STOT
        DIM(INCMNT)=CUTOFF
C
        DO 530 K=1,MSIZE
        PTOT(K)=     ABS  ((ROOT(K)/STOT)*100.0)
 530    CONTINUE
C
        DO 64 I=1,CUTOFF
        VT(I)=ABS(ROOT(I)/SREAL*100.)
 64     CONTINUE
        IF(CUTOFF+1.GT.MSIZE)GO TO 65
        ICT=CUTOFF+1
        DO 66 I=ICT,MSIZE
        if(simag.eq.0)go to 66
        VT(I)=ABS(ROOT(I)/SIMAG*100.)
  66    CONTINUE
 65     CONTINUE
C
C ALL DONE, WRITE IT OUT AND GO HOME...
C
c        print 700
        write(18,700)
  700   format(/'                               Eigenvalues')

        do 5904 i=1,ndim
c        WRITE(LFNO,7)i,ROOT(I)
        write(18,7)i,root(i)
   7    FORMAT(22x,' Root ',i2,'= 'f15.2)
5904    continue
c        print 701,WARP
        write(18,701)warp
  701   format(///,22x,' Warp Factor = ',f5.2)
        WRITE(LFNP,5903)msize,cutoff,NDIM
5903    FORMAT('(6F12.4)',2X,3I3)
        DO 400 K=1,msize
        WRITE(LFNP, 270)  (FAX(K,J),J=1,NDIM)
 270    FORMAT (6F12.4)
 400    CONTINUE
        do 401 i=1,msize
        write(lfnp,402)labels(i)
 402    format(a40)
 401    continue
        if(error(1).eq.0)go to 404
        write(lfnp,403)(error(i),i=1,msize)
 403    format(11f7.3)
 404    continue
        return
        END


c SUBROUTINE LSTSQR: Solves simultaneous equations
c following Pruzek's least-squares method.
c See Psychometrika, March, '83, p.34
c Joseph Woelfel
c Copyright 4/14/83
c All Rights Reserved
c
        SUBROUTINE LSTSQR(NCON,DIF,RADII)
        DIMENSION DIF(40,40),RADII(40)
        R=0.0
        RSQ=0.0
        TU=0.0
        N1=NCON-1
        N2=NCON-2
C
C
        DO 1 I=1,NCON
        JJ=I+1
        DO 2 J=JJ,NCON
        TU=TU+DIF(I,J)
        RT=2*TU
   2    CONTINUE
   1    CONTINUE
        DO 3 I=1,NCON
        DO 4 J=1,NCON
        R=R+DIF(I,J)
        RSQ=RSQ+(DIF(I,J)**2)
   4    CONTINUE
C       WRITE(6,100)R,TU,NCON
C  100  FORMAT(' R= ',F10.2,' TU=',F10.2,' NCON=',I2)
        RADII(I)=R/(2*N1)
        R=0.0
   3    CONTINUE
        RETURN
        END
c
      subroutine assist(ans,hfile,*)
      character*1 ans
      character*80 hfile,htext

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
   5  close (unit=19)
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
    1 format(/////////////'      Hello, I''m ',a80,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//,
     +'      ([ctrl C] will send you back to Galileo Control.)',//////)

c      print*,' Hello, I''m ',pname
c      print*,' You can enter ''?'' anytime you need help,'
c      print*, '...or just press ''ENTER'' to continue.'
      read(5,8)ans
    8 format(a1)

      if(ans.eq.'?')go to 7
      if(ans.eq.'@')then
      hfile='Applications/galileo/help/parrot.fun'
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


c      subroutine datwrt(prog,vers) !,pname)
c
c     Scott Danielsen
c     11/27/89
c
c      character*(*) prog,vers
c      character*80 pname,ans*1,hfile
c      integer  hour,minute,second,hund,year,month,day
c      call getdat(year,month,day)
c      call gettim(hour,minute,second,hund)
c      year = year-1900
c      write(6,100)prog,vers,month,day,year,hour,minute,second
c 100  format(/////////'0',5x,a11,5x,a5,
c     +15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2)
c      return
c      end
c
c      write(6,1)pname
c    1 format(//'      Hello, I''m ',a80,//,
c     +'      Please enter ''?'' anytime you need help,'//,
c     +'      ...or press ''ENTER'' to continue,',//,
c     +'      ([ctrl C] will send you back to Galileo'
c     +' Control.)',//////)
c      read(5,226)ans
c     if(ans.eq.' ')return
c      hfile='tables.doc'
c      if(ans.eq.'@')hfile='parrot.fun'
c      call assist(hfile)
c  226 format(a1)
c      return
c      end