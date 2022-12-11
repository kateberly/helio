      program indstar
c      Made into a Mac program 3/14/13 jw 
C	use msflib

      parameter (msize=5000)
	

c implements Woelfel's simplified interactive activation and competition
c model artificial neural net.

c     Scott Danielsen  11-13-91  Standardized interface.  Implemented
c     'cross-training' portability in assist, prompt, and datwrt.
c     Began making changes to standardize help (somewhere between
c     catpac, stables, and oreseme lies the final help system:  we just
c     have to discover it and we will, very soon).  Added yorn and byorn.
c     Added UPPERC subroutine to remove case sensitivity.
c     Made into HOLMES on 4/9/98 JW All Rights Reserved
c	Revised into DENNIS (tire) 7/23/00 JW All Rights Reserved
c	Became LISTIAC 10/23/01 JW All Rights Reserved
c
      common /in/ win,ncon
      common /active/ av
      common sense,on,off
      common /lbl/labels

      dimension win(msize,msize),av(msize,10),ext(msize)
      character*30 labels(msize),ANS
      character*80 ifile,hfile,form,ifile2,afile
      logical sense,log,train,lbl
      integer z ! 3/14/13 jw

      data thresh/0.0001/,rf/.1/,h/.005/,log/.false./,train/.false./
      data form/'(6f12.5)'/,nfunc/2/,lbl/.false./


      on=1.0
      off=0.0

      call intro('Indstar','v3.00 alpha')

  202 print*,' Do you have new cases for me to study now?'
      call yorn(*777,*31,*201)
  201 hfile='tfile.hlp'
      call assist('?',hfile,*202)
	go to 666


  777 call makelbl(labels,nlbl,afile)
	ncon=nlbl
	ifile2=afile
c	PRINT*, IFILE2
C       print*, 'upon return from makelbl, ncon=', ncon
C       stop

	go to 666      ! made for the dennis alpha demo


  26  print 314
  314 format(' These can''t be the labels; there''s a problem.')
c     +' Since I''m only a demo program, I''m going to die ugly.')
c	stop
c
   31 print 315
  315 format(' Where are the labels?')
c      call prompt
      read(5,122,err=26)ifile
c
c      hfile='or_lbl.hlp'
c      call assist(ifile(1:1),hfile,*31)
c    31 ifile='c:\listiac\file.lbl'
      open(unit=8,file=ifile,status='old',err=26)
      do 13 i=1,MSIZE
   13 read(8,144,ERR=26,end=500)labels(i)
  144 format(a30)

  500 ncon=i-1 ! This is i-1 in earlier oresmes jw
c	 Print*, ' Number of labels = ', ncon
      close(8)
C      if(lbl)go to 112
      go to 30

   25 PRINT 316
  316 format(' Sorry, Luminosity, there are no data here.')
c     +'  I''m croaking.')
c	 stop

   30 print 317
 317  format(' Where are the data?')
c      call prompt
c   30 ifile='c:\listiac\file.win'
      read(5,122,err=25) ifile

c      hfile='or_winin.hlp'
c      call assist(ifile(1:1),hfile,*30)

  122 format(a80)
      open(unit=9,file=ifile,status='old',ERR=25)
      read(9,122,end=16,err=25)form

   16 DO 1 I=1,ncon
      READ(9,form,ERR=25,end=18)(WIN(I,J),J=1,ncon)
    1 CONTINUE
   18 close(unit=9)
c    2 FORMAT(8F10.4)


c 112  print 318
c 318  format(' And where would you like the output, Crime Fighter?')
c      call prompt
c
C  112	ifile='c:\listiac\out.txt'  ! CAREFUL; WE LOST STATEMENT NUMBER 112
c      read(5,122)ifile

c      hfile='or_orout.hlp'
c      call assist(ifile(1:1),hfile,*112)


C      open(unit=9,file=ifile,status='unknown')
c 113  print 319
c 319  format(' Where would you like the modified weights saved?')
c      call prompt
c      read(5,10)Ifile
c	ifile='c:\listiac\file.wgt'

c      hfile='or_wnout.hlp'
c      call assist(ifile(1:1),hfile,*113)


   10 FORMAT(A80)
c      open(unit=16,file=Ifile,status='unknown')

      call norm  

C zero out the activations for a fresh start

  20  do 200 i=1,NCON
      EXT(I)= 0.0        ! "off' is too severe; it's the equivalent
      do 200 j=1,10      ! of asserting that the "off" things ARE NOT
      av(i,j)= 0.0       ! the "on" things.
  200 continue

      call reset(thresh,rf,h,LOG,nfunc)


   23 print 321
  321 format(' Care to see the labels, Definer of Things?')
      call yorn(*397,*7,*398)

 398  hfile='or_prmpt.hlp'
      call assist('?',hfile,*23)


 397  do 5 i=1,ncon
      write(6,6)labels(i) !i ! ** debug  get rid of the i
       if(pstop.eq.20)then
       pstop=1
       print*,' ...press enter for more...'
       read(5,*)
       GO TO 5
       else
       pstop=pstop+1
       endif
C    6 format(1X,a30,3x, i4)  !  ** debug get rid of the ,3x, i4
    6 format(1X,a30)
    5 continue
      PSTOP=1

c now get the external inputs to the system

   7  do 88 i=1,ncon
      print 322
 322  format(' Enter as many items as you want, Doctor.
     + -1 when done)')
C      call prompt
      read(5,9,end=12)ANS
      IF(ANS.EQ.'-1')GO TO 12  ! 3/14/13/JW
      call upperc(ans)

C      hfile='or_list.hlp'
C      call assist(ans,hfile,*7)


    9 format(a30)
      do 110 j=1,ncon
      JJ=J
      if(labels(j).eq.ANS)go to 11
  110 continue
      print 323
  323 format('Good Grief, Literati! Learn to spell!')
      go to 7

c   11 print*, ' Please enter strength of impact.'
c      read(5,*,err=11)ext(Jj)
   11 ext(jj)=on
   88 continue
	go to 396

   12 print 324
  324 format(10x,' And the winners are...',5x,'        Activation',//)
C      call yorn(*396,*14,*391)

C  391 hfile='or_clamp.hlp'
C      call assist('?',hfile,*12)

  396 sense=.true.
      go to 155

   14 sense=.false.
  155 call cycle(thresh,rf,EXT,NCYCLE,h,LOG,nfunc,train)

C	ifile='c:\listiac\file.dat'
C      open(unit=16,file=Ifile,status='unknown')
c      call output(labels,thresh,rf,NCYCLE,h)
      close (16)
  156 train=.false.
      print 21
   21 format(/,' Shall we go on, Carbon Life Form?')
      call yorn(*20,*395,*394)

 394  hfile='or_again.hlp'
      call assist('?',hfile,*156)


  395 print 325
  325 format(' I''m outta here!')
      go to 6666 ! normal termination of program
66666 print*, 'Something is wrong with file.dat.'
  666 train=.true.
c	print*, 'I''m at train=true.'     ! debug
c      print*,' And where might those cases be, Doctor?'
c      read(5,10)ifile2
c	ifile2='c:\listiac\health.dat'
      open(unit=15,file=ifile2,status='old',err=66666)

c      print*,' How many times through?'
C      read(*,*)it
      DO 1561 II=1,1   ! MAKE THE LAST PARAMETER "IT" IF WE GO BACK TO NISKET
c	print*, 'Now I''m in the 1561 loop'   ! debug
c      if(ii.ne.1)PRINT*,' I''m about to rewind 15.'
c      IF(II.NE.1)REWIND 15
  70  do 880 i=1,ncon
c	print*, 'Now I''m in the 880 loop.'   ! debug
C      call prompt
      read(15,9,end=1561)ANS
c      print*,' Ans = ',ans       ! ** debug
      if(ans.eq.'-2')go to 1561
      if(ans.eq.'-1')go to 109
      call upperc(ans)

      do 1100 j=1,ncon
      JJ=J
      if(labels(j).eq.ANS)go to 1110
 1100 continue
      print*,' Bad input word found and skipped.', ans ! jw debug 3/19/02
      go to 70
 1110 ext(jj)=on
  880 continue

  109 sense=.true.
      call cycle(thresh,rf,EXT,NCYCLE,h,LOG,nfunc,train)
C	ifile='c:\LISTIAC\out.txt'
C      open(unit=16,file=Ifile,status='unknown')
      call output(labels,thresh,rf,NCYCLE,h)
C      close (16)

c      call norm   !*** debug *** put back 8/27/98

C zero out the activations for a fresh start

 220  do 2200 i=1,NCON
      EXT(I)= 0.0        ! "off' is too severe; it's the equivalent
      do 2200 j=1,10     ! of asserting that the "off" things ARE NOT
      av(i,j)=0.0        ! the "on" things.
 2200 continue
      go to 70
 1561 CONTINUE
      go to 156
 6666 end
c ______________________________Subroutine Norm__________________

      Subroutine norm
      parameter (msize=5000)
      common /in/ win,ncon

      dimension win(msize,msize)

c subtract the grand mean from every entry to center...

      SUM=0.

      do 5 i=1,ncon
      do 5 j=1,ncon
      sum=sum+win(i,j)
    5 continue
      if(sum.eq.0)go to 7
      sum=sum/(ncon*ncon)

   7  do 6 i=1,ncon
      do 6 j=1,ncon
      win(i,j)=win(i,j)-sum
    6 continue

c now divide every entry by the absolute value of the largest entry
c so that the largest absolute value will be 1

      big=0.
      do 1 i=1,ncon
      do 1 j=1,ncon
      if(abs(win(i,j)).gt.abs(big))big=win(i,j)
    1 continue
      if((abs(big)).le.1)go to 8
      do 2 i=1,ncon
      do 2 j=1,ncon
      if(big.eq.0)go to 2
      win(i,j)=win(i,j)/abs(big)
    2 continue

C DEBUG
C      DO 3 I=1,NCON
C      WRITE(6,4)(WIN(I,J),J=1,NCON)
C    4 FORMAT(' DEBUG WIN AFTER NORMALIZATION',//,5(F5.2,2X))
C    3 continue
C DEBUG

    8 return
      end
c________________________________Subroutine Reset________________

      subroutine reset(thresh,rf,h,LOG,nfunc)
      character*80 hfile
      character*28 fdisc(4),pdisc
      logical log
      data fdisc/'SIGMOID (0 - +1)','SIGMOID (-1 - +1)',
     +'HYBERBOLIC TANGENT (-1 - +1)','LINEAR (-1 - +1)'/

155   print*, ' '
      print*, ' Care to set any values?'
      call yorn(*22,*4,*2234)

 2234 hfile='or_opt.hlp'
      call assist('?',hfile,*155)

c set some constants

  22  write(*,541)
 541  format(/'Press enter to accept default in brackets [].')

   14 write(*,111)fdisc(nfunc)
  111 format('Care to speculate on a functional form, Chiphead?'
     +/' [  ',a28,']')
       call byorn(*21,*200,*530,*14)

 530  hfile='or_func.hlp'
      call assist('?',hfile,*14)

   21 write (6,20)fdisc
   20 format(/' Please enter a number from 1 to 4, mon ami.'//
     +'      1 = ',a28/
     +'      2 = ',a28/
     +'      3 = ',a28/
     +'      4 = ',a28/)
c      call prompt
      read(5,*,err=2229)nfunc

      go to 2228
 2229 hfile='or_func2.hlp'
      call assist('?',hfile,*21)

 2228 if(nfunc.gt.4.or.nfunc.lt.1)go to 21

      if(nfunc.eq.1)then
        log = .true.
        off = 0.
        thresh = .5
      else
        log = .false.
        thresh = 0.
        off = -1.0
      endif

  200 write(*,'(a,f3.2,a)')
     +'Do you wish to set a new threshhold level? [',thresh,']'
      call byorn(*2221,*2,*531,*200)

 531  hfile='or_trsh.hlp'
      call assist('?',hfile,*200)

 2221 write(*,115)
  115 format('Enter new threshhold level.')
c      call prompt
      read(5,*,err=2223)thresh

      go to 2
 2223 hfile='or_trsh2.hlp'
      call assist('?',hfile,*2221)

    2 write(*,'(a,f3.2,a)')'How about a new decay rate? [',rf,']'
      call byorn(*2225,*3,*533,*2)

 533  hfile='or_dcay.hlp'
      call assist('?',hfile,*2)

 2225 write(*,117)
  117 format(' Enter new decay rate.')
c      call prompt
      read(5,*,err=2231)rf

      go to 2224
 2231 hfile='or_dcay2.hlp'
      call assist('?',hfile,*2225)


 2224 rf=1-rf

    3 write(*,'(a,f3.2,a)')'Learning Rate? [',h,']'
      call byorn(*2227,*4,*534,*3)

 534  hfile='or_srat.hlp'
      call assist('?',hfile,*3)

 2227 write(*,119)
  119 format('Enter new Learning Factor.')
c      call prompt
      read(5,*,err=2226)h

      go to 4   !14
 2226 hfile='or_srat2.hlp'
      call assist('?',hfile,*2227)

    4 pdisc=fdisc(nfunc)
c      print*,pdisc
      return
      end
c_________________________________Subroutine Cycle_________________

      Subroutine Cycle(thresh,rf,ext,ncycle,h,LOG,nfunc,train)
      parameter (msize=5000)
      common /in/ win,ncon
      common /active/ av
      common sense,on,off
      common /lbl/labels
      logical learn,sense,log,lanalog,train
      character*80 hfile
      character*30 labels(msize)
      Dimension win(msize,msize),av(msize,10),ext(msize)

       data anet/0./

       PSTOP=1

C      print*, ' How many cycles, hysteresis breath?'
C      read(5,*)ncycle
       NCYCLE=1

          if(train)then
          learn=.true.
          lanalog=.true.
          go to 306
          endif
140	LEARN=.FALSE.
	LANALOG=.TRUE.
C  140 print 400
C  400 format('Should I learn?')
C      call yorn(*301,*300,*302)
C
C  302 hfile='or_learn.hlp'
C      call assist('?',hfile,*140)
C 301  learn=.TRUE.
C        go to 4
C 300  LEARN=.FALSE.
C
C  4   print 401
C 401  format('Analog?')
C      call yorn(*303,*304,*305)
C
C  305 hfile='or_anlog.hlp'
C      call assist('?',hfile,*4)
C  303 lanalog=.true.
C      go to 306
C  304 lanalog=.false.

C set the activation values of the input variables
C and zero out the external inputs for the next cycle
 306  do 5 i=1,ncon
      IF(EXT(I).GT.0)AV(I,1)=EXT(I) ! CHANGE IF WE GO BACK TO GALIAC
C      PRINT*,' AV(I,1)= ',AV(I,1), ' I = ', I
      if(sense)go to 5
      ext(i)=0.0
    5 continue

      do 1, i=2,10   ! I COMMENTED THIS OUT 7/23/00 JW
c	DO 1, i=1,10 
      DO 10 II=1,NCYCLE
       do 2 j=1,ncon
        do 3 k=1,ncon
c
c     This line implements Jordan's totally self-reflexive model
        if(j.eq.k)go to 3   ! WHY NOT LET IT SPEAK TO ITSELF?
c     it interacts with the momentum and restoration terms

        anet= anet+(win(j,K)*av(K,i-1))
    3   continue

       go to(13,14,15,16)nfunc

c__________________The activation function stolen from JSPOT_______________

   13    Av(j,i)=(1.0/(1.0+(2.7183**(-1.0*anet))))
     +  +(rf*av(j,i-1))
         go to 17

c__________________Sigmoid varying from -1 to +1 (p708, NN, Vol 3 No 6, 1990)

   14   Av(j,i)=(1-(2.7183**(-1.0*anet)))/(1+(2.7183**(-1.0*anet)))
     +  +(rf*av(j,i-1))
        go to 17
c__________________The hyperbolic tangent function (Neuralware Demo)_______

   15   Av(j,i)= ((2.7183**anet)-(2.7183**(-1.0*anet)))/
     +((2.7183**anet)+(2.7183**(-1.0*anet)))
     +  +(rf*av(j,i-1))
        go to 17
c__________________A simple linear function_________________________________
   16  av(j,i)=anet+(rf*av(j,i-1))

   17 continue

        IF(EXT(J).NE.0.)AV(J,I)=EXT(J) ! external constraints, as in life.
        anet=0.


        if(lanalog)go to 2

       if(av(j,i).gE.thresh)av(j,i)=on
       if(av(j,i).lT.thresh)av(j,i)=off

    2  continue

                 if(.not.train)then
      DO 19 J=1,NCON
       if(av(j,i).gT.thresh)write(6,18)labels(j),av(j,i)
   18  format(20X,a30,1x,f6.4)
       if(pstop.eq.20)then
       pstop=1
       print*,' ...press enter for more...'
       read(5,*)
       GO TO 19
       else
       if(av(j,i).gt.thresh)pstop=pstop+1
       endif
   19 CONTINUE
       PSTOP=1
                 endif

       if(learn)then
C       PRINT*, ' Learning...'
       call hebb(i,h,log)
       ELSE
       endif
   10 CONTINUE

      if(train)go to 311
  405 go to 311
c  405 print 315
c  315 format(' That''s it.  Shall I think it over one more time?')
c        call yorn(*1,*311,*312)

c  312 hfile='or_recyc.hlp'
c      call assist('?',hfile,*405)

c  310  continuelanalog=.false.  !used to be noy='n' why?  in this loop noy
c                        !controls whether or not its analog.
    1 continue

  311 return
      end

C_______________________________subroutine Hebb_________________________

      SUBROUTINE HEBB(itime,h,log)
      parameter (msize=5000)
      common /in/ win,ncon
      common /active/ av
      logical log
      DIMENSION WIN(msize,msize),av(msize,10)
C      bigwin=0

      if(log)then  !
      xbar=.5      ! set up the mean
      else         ! so that the logistic
      xbar=0       ! can take on positive and negative values
      endif        ! call only when logistic function is used

      do 1 i=1,ncon
c      do 2 j=2,ncon
	do 2 j=1,ncon   ! Made this fix on july 23, 2000. Great Gosh Almighty!  JW

c if both nodes are on, increment the connection by h;
c if one is on and the other off, decrement the connection by h;
c if both nodes are off, leave the connection alone
c if a node is on, increase its diagonal value (connection to
c itself) by h
c if a node is off, leave its diagonal value alone

c      if(av(i,itime).eq.1.and.av(j,itime).eq.1)win(i,j)=win(i,j)+h
c      IF(av(i,itime).ne.av(j,itime))win(i,j)=win(i,j)-h
C      PRINT*,' ooooooooooohhhhhhHHH!  WIN(I,J) = ', WIN(I,J) ! ** DEBUG
      win(i,j)=win(i,j)+((av(i,itime)-xbar)*(av(j,itime)-xbar)*h)
C      if(abs(win(i,j)).gt.bigwin)bigwin=win(i,j)
    2 continue
    1 continue
C      if(abs(bigwin).gt.1)CALL NORM ! don't normalize until limit is reached
c      CALL NORM   ! *** debug *** put back put back 8/27/98 
      return
      end
c -------------------------------Random-----------------------

       subroutine random
      parameter (msize=5000)
       common /in/ win,ncon
       real*8 y,rand
       integer z
       dimension win(msize,msize)

       data z/0.836528945/

       write (*,'(a)')  '  Randomizing...'
       do 100 i=1,ncon
         do 101 j = 1,ncon
             z = rand(z)
             y = (z*2.0) - 1.0
C             WIn(i,j)=y
             WIn(i,j)=y*0.10
  101    continue
  100  continue
      return
      end
c  ---------------------------------Rand------------------------

       real*8 function rand(z)
       real*8 y,z
c
c    Returns a pseudo-random number between 0.0 and 1.0
c
       data r/3.9999/
       if (z.eq.0.0) z=0.836538945
10     z = r*z*(1.0-z)
       y = (z*1.5125)-.1
       if ((y.ge.1.0).or.(y.le.0.0)) go to 10
c      if the number is out of range, get another one
c      otherwise,...
       rand = y
       return
       end

c_______________________________Subroutine Output________________________

      Subroutine output(labels,thresh,rf,ncycle,h)

      parameter (msize=5000)
      common /in/ win,ncon
      common /active/ av
      DIMENSION WIN(msize,msize),av(msize,10)
      character*30 labels(msize)




c      write(6,1)thresh, rf,h,ncycle
C      write(9,1)thresh,rf,h,ncycle
C    1 format(/,' Threshhold = ', f5.3,' Restoring Force = ', f5.3,
C     +' Learning Rate = ',f5.3, /
C     +' Concept',t39,'Cycles X ',i3,/,T25,'1',4X,'2',4X,'3',4X,'4',
C     +4X,'5',4X,'6',4X,'7',4X,'8',4X,'9',4X,'10',/)

	
c	open(unit=16,file="C:\LISTIAC\FILE.WIN",status='unknown')

C      do 2 i=1,ncon
c      write(6,3)labels(i),(av(i,j),j=1,10)
C      write(9,3)labels(i),(av(i,j),j=1,10)
C    3 format(1X,a30,10f5.1)
C    2 continue
C DEBUG
c      WRITE(9,4)h
c    4 FORMAT(/' Modified Weights, h = ',F4.2)
      write(16,35)
  35  format('(8f10.6)')
      DO 30 I=1,NCON
c      WRITE(9,40)(WIN(I,J),J=1,NCON)
      WRITE(16,36)(WIN(I,J),J=1,NCON)
   36 format(8f10.6)
   40 FORMAT(/,10(F5.2,2X))
   30 continue
	CLOSE(16)
C DEBUG
      return
      end
C

c___________________________Subroutine What________________________

      subroutine what(pname,hfile)
      character*1 ans
      character*80 pname,hfile

      print 1,pname
    1 format(////////'      Hello, I''m ',a80,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//,
     +'      ([ctrl C] will send you back to Galileo Control.)',//////)
c      call prompt
      read(5,8)ans
    8 format(a1)

      if(ans.eq.'?')hfile='oreseme.doc'
      if(ans.eq.'@')hfile='parrot.fun'
      call assist(ans,hfile,*2)
 2    return
      end
c
      subroutine yorn(*,*,*)
c
      character*1 ans
c    1 call prompt
1		continue
      read(*,2,end=3)ans
    2 format(a1)
      if(ans.eq.'Y'.or.ans.eq.'y')return 1
      if(ans.eq.'N'.or.ans.eq.'n')return2
      if(ans.eq.'?')return 3
      write(*,4)
    4 format('I need a YES or NO.  Enter ''?'' for help')
      go to 1
    3 write(*,5)
    5 FORMAT(' PLEASE, answer the question.')
      go to 1
      end
c
      subroutine byorn(*,*,*,*)
c
      character*1 ans
c    1 call prompt
1		continue
      read(*,2,end=3)ans
    2 format(a1)
      if(ans.eq.'Y'.or.ans.eq.'y')return 1
      if(ans.eq.'N'.or.ans.eq.'n'.or.ans.eq.' ')return 2
      if(ans.eq.'?')return 3
      write(*,4)
    4 format('I need a YES or NO.'/' Type a ? for help.'/
     +' Press Enter on a blank line to accept default in brackets []'/)
      return 4
    3 write(*,5)
    5 FORMAT('PLEASE, answer the question.')
      go to 1
      end

c---------------------------Subroutine Upperc--------------------------

      subroutine upperc(line)

c    no upperc function on the ibm this will do it.

      character*30 line

      len=index(line,' ')
      do 10 j=1,len
      num=ichar(line(j:j))
 10   if(num .ge.97 .and. num .le.122)line(j:j)=char(num-32)
      return
      end


c    -------------------------Cross Training---------------------------

c     CROSS TRAINING SECTION: This set of subroutines will provide
c     for easy migration from system to system in spite of nasty I/O
c     contentions,  system calls. etc.  When porting from one system to
c     another, make sure all system specific commands are used and comment
c     out all others (in most cases this is one or two lines).  This should
c     take, maybe, 10 minutes tops.
c
c     These system specific commands have been clearly commented in the code.

c    ------------------------------------------------------------------



c-----------------------------Subrootine Prompt------------------------
c      subroutine prompt
c
c     for da dos

c      print 1

c     for da vax

c      print 2

c  1   format(' >',\)  !this one format statement may work on the VAX too.
                          !someone should check it out, then we can lose
                          !this subroutine.
c  2   format('$')
c      return
c      end

c---------------------------Subrootine Intro-----------------------------


      subroutine intro(prog,vers)
c
c     Scott Danielsen
c     11/27/89
c
      character*(*) vers
      character*80 hfile
      character*(*) prog
      character*1 ans

c      for da vax

c      character*8 tim
c      character*9 dat

c      for da dos

c      integer  hour,minute,second,hund,year,month,day
c
c     for da vax

c      call date(dat)
c      call time(tim)
c      print 101,prog,vers,dat,time
c 101  format(//////////5x,a10,5x,a15,10x,a9,5x,a8)

c      for da dos

c      call getdat(year,month,day)
c      call gettim(hour,minute,second,hund)
c      year = year-1900
c      print 100,prog,vers,month,day,year,hour,minute,second
c 100  format(///5x,a10,5x,a5,
c     +15x,i2.2,'/'i2.2,'/',i4.2,6x,i2.2,':'i2.2,':',i2.2)

c     for everybody!

      write(6,1)prog
    1 format(///////////////'      Hello, I''m ',a10,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//,
     +'      ([ctrl C] will send you back to Galileo'
     +' Control.)',//)
      read(5,226)ans
      if(ans.eq.' ')return
      hfile=prog//'.hlp' !    Too much sophistication can really slow you down!
      if(ans.eq.'@')hfile='parrot.fun'

      call assist(ans,hfile,*3)
  226 format(a1)
  3   return
      end


c------------------------------Subrootine Assist-------------------------

      subroutine assist(ans,hfile,*)
      character*1 ans
      character*80 hfile,ifile,htext
c
      j=0

      if(ans.eq.'?'.or.ans.eq.'@')go to 7
      go to 6

  7   len=index(hfile,' ')

c     for dos

      ifile='/Applications/indstar/help/'//hfile(1:len)


c     for mr. vax

c      ifile='[com0.comjoew.indstarcatpac/help/.help]'//hfile(1:len)

      open(unit=19,file=ifile,status='old',err=2)

      do 3 i=1,100
      j=j+1
      if(j.eq.22)then
      j=0
      print 300
 300  format('Press ''ENTER'' to continue.')
      read(5,*)
      end if
      read(19,4,end=5)htext
      write(6,4)htext
    3 continue
    4 format(a80)
      go to 5
   2  print*,' Sorry, I can''t help you. You''re on your own!'
   5  close(19)
      return 1
   6  close(19)
      return
      end
      
      	subroutine makelbl (labels,nlbl,afile)
c
c
c	a program to read an Oresme formatted data file and produce
c	a properly formatted labels file
c	Copyright 2000 All Rights Reserved Joe Woelfel
c
Cc 	use msflib

      parameter (msize=5000)
	  parameter (ncon=10000)

      character*30 labels(msize),ANS
	 character*80 ifile,afile
	k=1

      go to 2

  666 print 1
    1 format(' Pity! No cases here, Dr. Inch.')	
    2 print*,' And where might those cases be, Doctor?'
      read(5,7)ifile 
	afile=ifile
C    2	ifile='c:\listiac\file.dat'
      open(unit=15,file=ifile,status='old',err=666)


    5 do 3 i=1,ncon
      read(15,7,end=8)ANS
    7 format(a30)
c	write(6,7)ANS    ! don't forget to comment me out!
c	read*
      if(ans.eq.'-2')go to 11
      if(ans.eq.'-1')go to 5
	
      do 4 j=1,k
c	write(6,7)ans
      if(labels(j).eq.ANS)go to 3
c	print*, j
    4 continue
   40 labels(k)=ans
	k=k+1
	nlbl=k
c      go to 5
    3 continue
	
    8 REWIND (15)
	close (15)
c    	print*, ' Where do you want the goddamn labels, ASSHOLE?'
c      read(5,7)ifile
	ifile='c:\LISTIAC\FILE.lbl'
	open(unit=15,file=ifile,status='unknown',err=666)
	do 9 i=1, nlbl
c	print*,i
	write(15,7)labels(i)
    9 continue
   11 print*,' OK, I''ve got it.'
c	print*, 'nlbl= ',nlbl
	close (15)
	return
	end


