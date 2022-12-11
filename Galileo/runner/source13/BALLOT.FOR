
      program ballot
c
c    Roger Wilkinson 8/9/1988
c
c    Accepts raw galileo data, now with or without demographic lines.
c    Works out each respondent's discrete purchasing choice between 2 products.
c    Totals them up.
c    If either pair not answered or over filter treats as missing.
c    If both distances the same treats as undecided.
c    Sorry, Joe. I stole a few bits of galtab.
c
c     ***** see comments on filter about line 70 *****
c
c
c    s.d. 3/22/92 gave ballot the capacity to read n products.
c                 next step is the capacity for n self-points.
c
c    r.w. 24/3/1992 (rebelling against U.S. date conventions!)
c    Scott, you gave ballot the capacity to read 10 products.
c    I tried to make it truly read n products, but gave up on
c    the adjustable size arrays.  If you can do it, good luck
c    to you.  Meanwhile, I upped it to 98.  (98 products + 1
c    self concept gives 99 concepts total, the most concepts
c    possible to stay with I2 format).  There seems little point
c    in going larger because we need raw galileo data, not
c    neural network data, so ncon is restricted.  I also built in
c    some sense-checking for the concept numbers (try to enter an
c    impossible combination now!).  I made it skip over the
c    demographic lines.  And I removed the filter, because it
c    doesn't make sense.

      common /data/labels,nlign,product,ntarg,nseg,ifile
     +,ofile,nprod,maxpr !filter

      DIMENSION II(8),JJ(8),DIST(8),pct(98),pdist(98)
      integer product(98),pref(98)
      CHARACTER*8 IDEN
      CHARACTER*80 NSEG,ifile,ofile
      CHARACTER*20 LABELS(99)
      logical lequal
      real*4 nx  ! added 3/6/13 jw
      data ncase/0/,miss/0/,labels/99*' '/,maxpr/98/   !filter/99999./
      call intro('Ballot','v2.1')

      call gather

c    start of case-by-case handling

c    initialize distances to something sort of missing-sounding

   20 do 91 j = 1,nprod
   91 pdist(j)=-1


c  start line-by-line handling

      do 19 nn=1,nlign
   21 READ(20,2,END=10)IDEN,(II(K),JJ(K),DIST(K),K=1,8)

    2 FORMAT(A8,8(2I2,F5.0))

      IF(IDEN.EQ.'ENDOFSET')GO TO 10
c
c       check for demographic lines and skips them
c       rw 29/7/1992
c
c      print*,nn,iden(7:8),ii(1),jj(1),ii(2),jj(2)  !rw debug
c
c
c      After reading all galileo lines, Ballot thinks it's reading
c      the first line of the next case.  Once this condition is
c      satisfied, any one of the following will indicate the line
c      is not the first line of galileo data.
c
      if(nn.eq.1)then
        if(iden(7:8).ne.'01')go to 21 ! Any one of these indicates
        if(ii(1).ne.1)go to 21        ! the line can't be the first
        if(jj(1).ne.2)go to 21        ! line of galileo data.
        if(ii(2).ne.1)go to 21        ! If it isn't, then read a
        if(jj(2).ne.3)go to 21        ! new line.
      endif

c
      DO 33 K=1,8

c   look for the distances we want

      do 5 jjj = 1,nprod
      if(product(jjj).eq.ii(k).and.ntarg.eq.jj(k))pdist(jjj)=dist(k)
  5   if(product(jjj).eq.jj(k).and.ntarg.eq.ii(k))pdist(jjj)=dist(k)

   33 CONTINUE
   19 continue

c   end line-by-line

c    reject missing and > filter values

      do 88 jjj = 1,nprod
c
c
c      No justification for filter in ballot because we are not averaging
c      responses across individuals so outliers will not skew the data.
c      It does not matter if a respondent uses a lot of large distances
c      because we only want the smallest self-product distance they give.
c      I think ballot is unique in the galileo suite in this regard.
c      All we need to reject is cases with at least one missing self-
c      product distance.
c      Roger Wilkinson 27 July 1992.

		data isdone/-1/
c
c      if(pdist(jjj).eq.-1.or.pdist(jjj).gt.filter)then
      if(pdist(jjj).eq.-1)then
        miss=miss+1
        go to 7
      endif
 88   continue

c     set up to find smallest

      nx=pdist(1)
      index=1
      lequal=.true.

      do 8 jjj=2,nprod

c     check for equal

       if(pdist(jjj).ne.nx)lequal=.false.

c     find smallest

       if(min(pdist(jjj),nx).lt.nx)then  !why not just if(pdist(jjj).lt.nx)
         index=jjj                       !rw
         nx=pdist(jjj)
       endif
   8  continue

c      add counters

      if(.not.lequal)pref(index)=pref(index)+1
      if(lequal)nundec=nundec+1

  7   ncase=ncase+1
      GO TO 20

c   end of case-by-case handling


 10   do 9 jjj = 1,nprod
   9  pct(jjj)=real(pref(jjj))/real(ncase)*100.0

      pctmis=real(miss)/real(ncase)*100.0
      pctund=real(nundec)/real(ncase)*100.0
c
      write(21,113)nseg
  113 format(' Purchase or Voting Intentions',/,a79,/,40('_'))

      do 43 jjj = 1,nprod
   43 write(21,110)pref(jjj),labels(product(jjj)),pct(jjj)
  110 format(/,i4,' respondents prefer ',a20,//,'...This is ',
     +f5.1,' %')


      write(21,112)nundec,pctund
  112 format(/,i4,' respondents were undecided',//,'...This is ',
     +f5.1,' %')
      write(21,111)miss,pctmis,ncase
  111 format(/,'There were ',i4,' missing cases',//,'...This is ',
     +f5.1,' %',//,'Total of ',i4,' cases')
      write(6,114)ofile
  114 format(' No worries, mate.',/,' Your output is in ',a20)
      close(unit=21)
      print*,' Please press [ENTER] to return to Galileo Control.'
      read(*,*)          ! wait for william
c      stop 'See you later!'
      end

      subroutine gather
      common /data/labels,nlign,product,ntarg,nseg,ifile
     +,ofile,nprod,maxpr   !filter

      character*80 nseg,ifile,ofile
      character*20 labels(99)
      integer product(98)

      go to 361
 501  write(*,700)
 700  format(' That can''t be right; there''s nary a label here.')
 361  WRITE(6,26)

   26 FORMAT(' What''s your labels file called, cobber?')
c      call prompt
      read(5,360,err=361)ifile
      if(ifile.eq.' ')go to 501
  360 format(A80)

      call assist(ifile(1:1),'lbl.hlp',*361)

      open(unit=20,file=ifile,status='old',err=501)
      DO 261 I=1,100

  261 READ(20,262,end=500,err=501)LABELS(I)
  262 FORMAT(a20)
  500 ncon=i-1
      close(20)

c     work out how many lines per case

      nlign=int((real(ncon)*(real(ncon)-1)/16.0)+1.0)

c
c   find out appropriate concept numbers
c

      nprod=1
 151  WRITE(6,11)nprod
   11 format(' What concept number is product ',i2,
     +' (press enter for list, -1 when done)')
c      call prompt
      read(5,131,err=507,end=513)product(nprod)
c      print*,'  product(nprod) is  ',product(nprod)  ! ** debug
c      if(product(nprod).eq.isdone) go to 513 ! to replace control z command 3/13/13 jw
      if(product(nprod).eq.0)call liscon(ncon,labels,*151) !rw eq to le
      if(product(nprod).lt.0)go to 513
c            print*,'  product(nprod) is  ',product(nprod)  ! ** debug
c
c     check for concept numbers out of bounds or already given (RW)
c
      do 601 nnn=1,nprod-1
      if(product(nprod).eq.product(nnn))then
        write(6,602)
 602    format(' Careful, mate!  You''ve already given me that one!')
        go to 151
      endif
 601  continue
      if(product(nprod).gt.ncon)then
        write(6,603)
 603    format(' This is dodgy; there aren''t that many concepts.')
        go to 151
      endif
      if(nprod.ge.ncon-1.or.nprod.ge.maxpr)then    !  up to 98 prods
        write(6,263)product(nprod),labels(product(nprod))
        go to 525
      endif
c
c      end check
c
      go to 506
  507 call assist('?','objno.hlp',*151)
c
c
  506 write(6,263)product(nprod),labels(product(nprod))
      nprod=nprod+1
      go to 151
  513 nprod=nprod-1

  131 format(bz,i2)
  263 format(26x,i2,2x,a20)

  525 write(6,13)
   13 format(' What number is the self concept?'
     +' (press enter for list)')
c      call prompt
      read(5,131,err=527)ntarg
      if(ntarg.le.0)call liscon(ncon,labels,*525) !rw eq to le
c
c      again, check for already given or out of bounds  (RW)
c
c      print*,' nprod= ',nprod     !rw debug
      do 604 nnn=1,nprod
      if(ntarg.eq.product(nnn))then
c      print*,' target con = ',ntarg,' product(',nnn,')' !rw debug
        write(6,605)
 605    format(' Looks like you''ve got a few kangaroos loose in the'
     +' top paddock.'/' That''s one of the products!')
        go to 525
      endif
 604  continue
      if(ntarg.gt.ncon)then
        write(6,606)
 606    format(' This is dodgy; there aren''t that many concepts.')
        go to 525
      endif
c
c      end check
c
      go to 526

  527 call assist('?','nself.hlp',*525)

  526 write(6,263)ntarg,labels(ntarg)
c
c    remove stuff concerning filter
c
c  528 WRITE(6,25)

c   25 FORMAT('0What''s the maximum value, mate?')
c      call prompt
c      READ(5,*,err=539)FILTER
c      go to 531

c  539 call assist('?','maxval.hlp',*528)

  531 WRITE(6,2630)

 2630 FORMAT(' What do you want to call this run, old wombat?')
c      call prompt
      READ(5,264)NSEG

  264 FORMAT(a80)

      call assist(nseg(1:1),'tit.hlp',*531)

  362 WRITE(6,265)NSEG

  265 FORMAT(' What''s the name of your data file for',/,1X,a79)
c      call prompt
      read(5,360)ifile

      if (ifile(1:5).eq.'     ')then
        write(6,610)
        go to 362
      endif
  610 format(/,' Just type in the name of your data file,'
     +' or type ''?'' for help.')

      call assist(ifile(1:1),'data.hlp',*362)

      open(unit=20,file=ifile,status='old',err=362)

  201 write(6,200)
  200 format(/,' What do you want to call the output file?')
c      call prompt
      read(5,360)ofile

      if (ofile(1:5).eq.'     ')then
        write(6,611)
        go to 201
      endif
  611 format(/,' Just type in a name for your output file,'
     +' or type ''?'' for help.')

      call assist(ofile(1:1),'ofile.hlp',*201)

      open(unit=21,file=ofile,status='unknown',err=201)

c    reassure the punters it's still thinking

      write(6,999)
  999 format(////,' DO NOT ADJUST YOUR SET.',/,
     +' TRANSMISSION WILL RESUME IN A FEW MOMENTS.'////)

      return
      end
C________________________________Subroutine liscon____________________

      subroutine liscon(ncon,labels,*)

      character*20 labels(99)
c      print*,' Here I am in liscon at line 362...'  ! ** debug jw
      pstop=1
 397  do 5 i=1,ncon
       write(*,6)i,labels(i)
       if(pstop.eq.21)then
        pstop=1
        write(*,4321)
 4321 format(30x,' ...press enter for more...')
        read(5,*)
        GO TO 5
       else
        pstop=pstop+1
       endif
    6 format(24x,i2,2x,a20)
    5 continue
      return 1
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

c  1   format(' ====>',\)  !this one format statement may work on the VAX too.
                          !someone should check it out, then we can lose
                          !this subroutine.
c  2   format('$====>')
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
c 101  format(//////////5x,a10,5x,a5,15x,a9,5x,a8)

c      for da dos

c      call getdat(year,month,day)
c      call gettim(hour,minute,second,hund)
c      year = year-1900
c      print 100,prog,vers,month,day,year,hour,minute,second
c 100  format(//////////5x,a10,5x,a5,
c     +15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2)

c     for everybody!

      write(6,1)prog
    1 format(/////////////'      Hello, I''m ',a31,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//////)
     
      read(5,226)ans
      if(ans.eq.' ')return
      hfile=prog//'.doc'
      if(ans.eq.'!')hfile='cheese.fun'
      if(ans.eq.'@')hfile='parrot.fun'
      if(ans.eq.'#')hfile='nudge.fun'
      call assist(ans,hfile,*3)
  226 format(a1)
  3   return
      end


c------------------------------Subrootine Assist-------------------------

      subroutine assist(ans,hfile,*)
      character*1 ans
      character*(*)hfile
      character*80 ifile,htext
c
      j=0

c      if(ans.eq.'?'.or.ans.eq.'@')go to 7
c      if(ans.eq.'!'.or.ans.eq.'#')go to 7
c      return
c     for dos
c   7  continue

      ifile='/Applications/galileo/help/'//hfile


c     for mr. vax

c      ifile='[com0.comjoew.galileo.help]'//hfile(1:len)

      open(unit=19,file=ifile,status='old',err=2)

      do 3 i=1,100
      j=j+1
      if(j.eq.22)then
      j=0
      print 300
 300  format(' Press ''ENTER'' to continue.')
      read(5,*)
      end if
      read(19,4,end=5)htext
      write(6,4)htext
    3 continue
    4 format(a80)
      go to 5
   2  print*, ' '
      print*,' Sorry, I can''t help you. You''re on your own!'
   5  close(19)
      return 1
   6  close(19)
      return
      end
c
      subroutine yorn(*,*)
c
      character*1 ans
c    1 call prompt
1      read(*,2,end=3)ans
    2 format(a1)
      if(ans.eq.'Y'.or.ans.eq.'y')return 1
      if(ans.eq.'N'.or.ans.eq.'n')return 2
      if(ans.eq.'?')return
      write(*,4)
    4 format(' I need a YES or NO.  Enter ''?'' for help')
      go to 1
    3 write(*,5)
    5 FORMAT(' PLEASE, answer the question.')
      go to 1
      end