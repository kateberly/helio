c Program KATMANDU
c 
c Joe Woelfel
c Scott Danielsen
c Hao Chen
c
c *** VAX Version 1.00 *****
c   
c   VAXified by jw 10/8/91
c
c   SD 3-4-91 to 3-12-91 made changes to allow compatibility in
c   IBMland and trim down size.  Compatible with Microsoft Fortran 5.0.
c   Major changes include modular call structure, and new kounting logic
c   to cut down on matrix storage
c
c   SD 3-13-91 added window routines.  You can have any size window with
c   any size slide or still case delimit with -1.
c
c   SD- Trans and f(i,j) laid to rest this day 3-15-91 (check out ccount).
c   f(i,j) premultiplied by its transpose = sum of each vector f(j)
c   premulitplied by it's transpose.  Count of words in each episode is read
c   in as vector f, is pre-multiplied by it's transpose and summed to win.
c   You may now have a whole lot of cases.
c
c   SD 3-18-91 added net and attendant routines.  It works like this
c   catalac will learn the words and spit out a .win and .lbl file.
c   If you want to interact, use creator.
c
c   SD 3-26-91 increased unique word capacity from 100 to 300.  This
c   makes more sense if you are analyzing books, or large text db's.
c   Size of unique word limiter (nlim) is user selected: 100 <= nlim <= 300.
c
c *** Katmandu Version 1.00 *****
c   Put at the CCR's U2 server
c   Compiled with Intel FORTRAN compiler
c 
c   HC 4-12-07 twords(1300). Increased unique 
c   word capacity from 300 to 1300. (BUG: hit the wall at 300)
c   
c   HC 4-23-07 "300 Wall" Bug found and fixed (subroutine hiclus)
c
c   HC 4-24-07 new bug, nlim will truncate 4-digit number to 3-digit
c   11 format(i5) - allow 5-digit now
c
c	February 25, 2013 recompiling using gnufortran on a MacBook Air...jw
c	Renamed Catpac III March 17, 2013


      common /in/win,ncon
      common /freq/kwords
      common /vector/ext
      common /alpha/words
      common /temp/tfreq,twords,tdex
      common /stuff/nlines,nuniqe,ntepi,nepi,nwrds,nwords
      common /parm/thresh,rf,h,logs,nfunc
c

      logical isnet,logs      ! added logs on February 25, 2013  jw
      integer kwords(4350),tfreq(1300),tdex(1300)
c       integer kwords(43500),tfreq(1300),tdex(1300)  ! just a shot in the dark
      real win(1300,1300),ext(1300)
      character*15 words(4350),twords(1300)
c      character*15 words(43500),twords(1300)  ! just a shot in the dark
      character*80 tit,clamp*3
      character*80 pname,hfile
      character*250 line

c     ================
c     !Hao Chen 
c     timer added
c     ================
      real cpu(3)
      real etime
      real t1(2)
      real t2(2)
      real t3(2)
c     ++++++++++++++++



c      data thresh/0.0/,rf/.1/,h/.05/,logs/.true./,nfunc/2/
      data nuniqe/0/,kwords/4350*0/,words/4350*' '/,ext/1300*0.0/,
     +win/1690000*0.0/,isnet/.false./,thresh/0.0/,rf/.1/,h/.01/,
     +logs/.true./,nfunc/2/
      on=1.0
      off=-1.0

c     The main controlling program.
c        pname='Katmandu'
			pname='Catpac'
        hfile='catpac/help/catpac.hlp'
        
        call what(pname,hfile)


c     ================
c     !Hao Chen 
c     timer added
c     ================
c      cpu(1) = etime (t1)
c      write ( *, '(a)' ) ' '
c      write ( *, '(a)' ) '  ETIME reports:'
c      write ( *, '(a,g14.6)' ) 
c     &  '    The current CPU time is            ', cpu(1)
c      write ( *, '(a,g14.6)' ) 
c     &  '    Time1_ARRAY(1) =                   ', t1(1)
c      write ( *, '(a,g14.6)' ) 
c     &  '    Time1_ARRAY(2) =                   ', t1(2)      
c     ++++++++++++++++
c


c
c   1  call clear
      write(*,1113)
c      call catwrt('Catpac','1.01',5,1)
      call gather(tit,ncycle,isnet,clamp,nlim)

c     *** DEBUG *** HC 04-24-07
c      stop

      call exclude
c      call catwrt('Catpac','1.01',3,2)
      write(*,1113)
c     *** DEBUG *** change subroutine read into readin
    3 call readin(line,nlines,ntepi,*4)
      call parse(line,nuniqe,nwrds,*3,*4)
    4 call sort(nuniqe,nwords,nwrds,ntepi,nlim)
      nepi=0
      rewind(14)

c     ================
c     !Hao Chen 
c     timer added
c     ================
c      cpu(2) = etime (t2)
c      write ( *, '(a)' ) ' '
c      write ( *, '(a)' ) '  ETIME reports:'
c      write ( *, '(a,g14.6)' ) 
c     &  '    The current CPU time is            ', cpu(2)
c      write ( *, '(a,g14.6)' ) 
c     &  '    Time2_ARRAY(1) =                   ', t2(1)
c      write ( *, '(a,g14.6)' ) 
c     &  '    Time2_ARRAY(2) =                   ', t2(2)      
c     ++++++++++++++++
c



      if(isnet)call net(nepi,nuniqe,twords,ncycle,ntepi,isnet,*5)
      call ccount(nepi,nuniqe,isnet,ntepi,*5)
 5    call prints(nepi,nuniqe,nlines,nwords,tit,isnet,ncycle,clamp)
      call hiclus(nuniqe,twords)

c     ================
c     !Hao Chen 
c     timer added
c     ================
      cpu(3) = etime (t3)
c      write ( *, '(a)' ) ' '
c      write ( *, '(a)' ) '  ETIME reports:'
c      write ( *, '(a,g14.6)' ) 
c     &  '    The current CPU time is            ', cpu(3)
c      write ( *, '(a,g14.6)' ) 
c     &  '    Time3_ARRAY(1) =                   ', t3(1)
c      write ( *, '(a,g14.6)' ) 
c     &  '    Time3_ARRAY(2) =                   ', t3(2)      
cc     ++++++++++++++++

c      print*, ' Time elapse = ', cpu(3)-cpu(2)
c      print*, ' T3(1)-T2(1) = ', t3(1)-t2(1)
c      print*, ' T3(2)-T2(2) = ', t3(2)-t2(2)

      close(2)
      close(3)
      close(9)
      close(14,status='delete')
      close(16)
      close(17)
c      write(*,592)
c 592  format(///'Another go-round?')
c      call yorn(*1,*2)
   2  write(*,591)
c     *** DEBUG ***
  591 format(//'  All done.'
     +////' Sh bop, sh bop, sh bang.',/////)
 1113 format(/////)
      end
c
      subroutine gather(tit,ncycle,isnet,clamp,nlim)
      common /window/iswin,winsize,winwrds,nslide
      common sense,on,off

      integer winsize,winwrds
      logical iswin,isnet,sense
      character*80 tit,ifile,clamp*3,hfile
      clamp = ' NO'

c
c     collects run info.
c
      sense=.false.
2222  print*, ' '
      print*, 'What would you like to call this run?'
c      write(*,'(a,\)') ' ====>'
      read(*,10)tit

      hfile='catpac/help/htit.hlp'
      call assist(tit(1:1),hfile,*2222)
c
  222 print*, ' '
      print*, 'Please type in the name of the data file.'
c      write(5,'(a,\)') ' ====>'
      read(5,10)ifile

      hfile='catpac/help/HTXT.HLP'
      call assist(ifile(1:1),hfile,*222)

      open(unit=2,file=ifile,status='old',err=222,recl=250)
c
      go to 2221
  221 continue
c     print*, ' Your random file is missing. I''m croaking!'
c     print*, ' ...unless you''d care to name a scratch file for me...'
c     write(5,'(a,\)') ' ====>'
c      read(5,10)ifile
      ifile='random'

      hfile='catpac/help/rndfil.hlp'
      call assist(ifile(1:1),hfile,*221)

      go to 2225
2221  open(unit=11,file='rndm.fil', 
     +status='old',err=221)
      read(11,10)ifile
      close (unit=11)
2225  open(unit=14,file=ifile,status='unknown',err=221)
 228  print*,' '
      print*, ' Enter number of unique words.'
c      write(5,'(a,\)') ' ====>'
c     *** DEBUG *** HC 04-24-07
      read(5,11,ERR=2223)nlim
c      read*,nlim

c     *** DEBUG ***
c      print*, 'nlim = ', nlim

      

      go to 2224
 2223 hfile='catpac/help/nuniq.hlp'
      call assist('?',hfile,*2221)


 2224 if(nlim.gt.1300)then              !
        print*, ' '
        print*,' Number must be 1300 or less.'
        go to 228
      endif
c 2224 continue
      
 11   format(i5)
c
  2   iswin = .false.
 111  write(*,100)
  100 format(' Please enter window size or -1 for case delimit.')
c      write(*,'(a,\)') ' ====>'
      read(*,*,err=2227)winsize
      go to 2226

 2227 hfile='catpac/help/hwsize.hlp'

      call assist('?',hfile,*111)

 2226 if(winsize.eq.-1)go to 234
      iswin = .true.
   1  write(*,101)
 101  format(' Enter size of slide.')
c      write(*,'(a,\)') ' ====>'
      read(*,*,err=2228)nslide
      go to 234

 2228 hfile='catpac/help/hslide.hlp'
      call assist('?',hfile,*1)


c      if(nslide.gt.winsize)then
c         print*, '  Slide should be less than window size'
c         go to 1
c      endif
c
  234 print*, ' '
      print*, ' Enter file name descriptor.'
c      write(5,'(a,\)') ' ====>'
      read(5,10)ifile

      hfile='catpac/help/hdesc.hlp'
      call assist(ifile(1:1),hfile,*234)

      call feol(ifile,len)
      ifile(len+1:len+4)='.cat'
c      ofile=ifile
c      read(5,10)ofile
      open(unit=3,file=ifile,status='unknown',recl=132,err=234)
c
 235  print*, ' '
      print*, ' Would you like a Network analysis?'
      call yorn(*5,*6,*2229)

2229  hfile='catpac/help/net.hlp'
      call assist(ifile(1:1),hfile,*235)

c
  5   isnet = .true.
      print*, ' '
   16 continue
c     print*, ' How many cycles, hysteresis breath?'
c      write(5,'(a,\)') ' ====>'
c      read(5,*,err=2230)ncycle
      ncycle=1
      
      print*,' '
      go to 2231

2230  hfile='catpac/help/ncycle.hlp'
      call assist('?',hfile,*16)


 2231 if(ncycle.lt.5)go to 12
      ncycle=4
      print*,' Sorry, Boss. Four is about all I can handle.'
      print*,' Will that be all right?'
      call yorn(*12,*16,*2232)

 2232 hfile='catpac/help/four.hlp'
      call assist('?',hfile,*2231)


   12 continue
c     print*,' Do you want nodes clamped?'
c     call yorn(*14,*155,*2233)
      go to 14      

 2233 hfile='catpac/help/clamp.hlp'
      call assist('?',hfile,*12)

   14 sense=.true.
      clamp='YES'
155   print*, ' '
      print*, ' Care to set any values?'
      call yorn(*22,*23,*2234)

 2234 hfile='catpac/help/options.hlp'
      call assist('?',hfile,*155)

  22  call reset
c
  23  ifile(len+1:len+4)='.win'
c      print*, ifile
      open(unit=16,file=Ifile,status='unknown',err=234)
      print*, ' '
      print*, ' Weights saved in ',ifile
c
      ifile(len+1:len+4)='.lbl'
      open(unit=17,file=ifile,status='unknown',err=234)
      print*, ' '
      print*, ' Words saved in ',ifile
  6   print*, ' '
      print*, ' Cluster analysis saved in ',ifile(1:len),'.cat'
   10 format(a80)
      return
      end
c
      subroutine exclude
      common/del/nwrds,Bwords
      character*15 bwords(1300)
c
c     This opens the exclude file, and reads it in.
c
      open(unit=1,file='/Applications/catpac/exclude.dat'
		
     +,status='old')
      do 10 i=1,1300
      read(1,100,end=88)bwords(i)
c      print*, bwords(i)  !** debug **
      nwrds=nwrds+1
  100 format(a15)
   10 continue
   88 close(1)
      return
      end
c
      subroutine readin(line,nlines,ntepi,*)
      common /window/iswin,winsize,winwrds,nslide
      logical iswin
      integer winsize,winwrds
      character*250 line
c
    3 read(2,555,end=55)line
      if (line .eq.' ')go to 3
      nl=nl+1
      if(line.eq.'-1'.and. iswin)go to 3
      if (line.eq.'-1')then
        if(nl.eq. 1)go to 3
        write(14,556)line
        ntepi=ntepi+1
        nl=0
        go to 3
      endif
      nlines=nlines+1
c      write(*,1112)nlines
      return
   55 return 1
 1112 format('+ Reading data:      line ',i5,'               ')
  555 format(a250)
  556 format(a2)
      end
c
      subroutine parse(ans,nuniqe,nwords,*,*)
c
c     This will parse a user response
c     change letters to upperc, delete unecessary words
c     and  keep an initial tally of words and frequency.
c
      character*15 word
      character*250 ans
      data nend/250/
c
      nx=1
      num=0
    1 do 10 i=nx,nend
         num=num+1
         ntest=ichar(ans(i:i))
         if(ntest.ge.97.and.ntest.le.122)
     +   ans(i:i)=char(ntest-32)
         if(ichar(ans(i:i)).ge.65.and.ichar(ans(i:i)).le.90)go to 10
           if(num .le. 3)go to 9
           word=ans(nx:i-1)
           call delete(word,*9)
           nwords=nwords+1
    8      call icount(word,nuniqe,*55)
           write(14,100)word
    9      if (ans(i+1:nend).eq.' ')return 1
           nx=i+1
           num=0
         go to 1
   10 continue
      return 1
  100 format(a15)
   55 return 2
      end
c
      subroutine delete(word,*)
      common /del/nwrds,Bwords
c
c     deletes un-needed words.
c
      character*15 bwords(1300),word
    1 format(a15)
      do 5 k=1,nwrds
      if(word.eq.bwords(k)) return 1
    5 continue
      return
      end
c
      subroutine icount(word,nuniqe,*)
      common /freq/kwords
      common /alpha/words
      dimension kwords(4350)
      character*15 words(4350),word
c
c     will keep an initial tally of words and kwords.
c
      do 6 J=1,nuniqe
        if(word.eq.words(j))go to 7
    6 continue
c
      nuniqe=nuniqe+1
      if(nuniqe.gt.4350)then
c         write(*,1312)
c 1312 format(/'+ Unique word limit of 4350 has been reached.')
c         call timer(5)
c         write(*,1313)
c 1313  format('+                                             ')
         return 1
      endif
c
      words(nuniqe)=word
      kwords(nuniqe)=kwords(nuniqe)+1
  100  format(a15)
      return
    7 kwords(j)=kwords(j)+1
      return
      end
c
      subroutine sort(nuniqe,nwords,nwrds,ntepi,nlim)
      common /freq/kwords
      common /alpha/words
      common /temp/tfreq,twords,tdex
      common /prnt/percnt
      common /window/iswin,winsize,winwrds,nslide
      logical iswin
      real percnt(1300)
      integer tfreq(1300),tdex(1300),winsize,winwrds
      character*15 words(4350),ait,twords(1300)
      dimension kwords(4350)
c
c     This will sort kwords in descending frequency.
c
c     first find total episodes for output purposes
c
      if(iswin)then
      tepi=(nwrds+nslide-winsize)/nslide
c      ntepi=tepi-amod(tepi,1)    ! I changed for VAX jw
       ntepi=tepi-(tepi-int(tepi))! an made dis beauty
      endif
c
      nwords=0
102   flip = 0
      num=num+1
c      write(*,1213)num
c 1213 format('+ Sorting.  Iteration: ',i5,'      ',
c     +       '                                       ')
      do 103 i=1,nuniqe-1
        if(kwords(i).ge.kwords(i+1))go to 103
        flip=1
        it=kwords(i)
        kwords(i)=kwords(i+1)
        kwords(i+1)=it
        ait=words(i)
        words(i)=words(i+1)
        words(i+1)=ait
  103 continue
      if(flip.ne.0)go to 102
c
      nuniqe=0
      do 10 i = 1,nlim
      new=0
      j=i
      do while(kwords(j).eq.kwords(i))
       j=j+1
       new=new+1
       if(nuniqe+new.gt.nlim)go to 11
  9   enddo
      nuniqe=nuniqe+1
      tfreq(i) = kwords(i)
      twords(i)= words(i)
      tdex(i)  = i
      rwords=rwords + tfreq(i)
 10   continue
c
  11  do 15 i = 1, nuniqe
  15  percnt(i)=(tfreq(i)/rwords)*100
      nwords=rwords
      return
      end
c
      subroutine prints(eps,tuwds,lines,twds,tit,isnet,ncycle,clamp)
      common /prnt/percnt
      common /temp/wfreq,words2,windex
      common /window/iswin,winsize,winwrds,nslide
      common /parm/thresh,rf,h,logs,nfunc
      logical iswin,isnet
c
      character*80 tit
      character*15 words2(1300),clamp*3
      integer twds,tuwds,eps,lines,winsize,winwrds
      integer wfreq(1300),windex(1300),aindex(1300)
      real percnt(1300)
c
c     prints out stats for words.  Thanks once again to the mighty RA.
c     killed some avar matrices and used windex as sort index for
c     others.  Ascending sort is now done in sort, added print control
c     calls (sd 3-10-91).
c
      write(3,66)tit
  66  format('  TITLE: ',A72//)
      if (.not.isnet)then
        if(.not.iswin)write(3,6)twds,tuwds,'EPISODES',eps,lines
        if(iswin)write(3,6)twds,tuwds,'WINDOWS ',eps,lines
    6 FORMAT('  TOTAL UNIQUE WORDS   ',I6/
     +       '  TOP   UNIQUE WORDS   ',I5/
     +       '  TOTAL ',A8,  '       ',I8/
     +       '  TOTAL LINES          ',I6)
      else
        if(.not.iswin)write(3,17)twds,thresh,tuwds,rf,
     +   'EPISODES',eps,ncycle,lines
        if(iswin)write(3,17)twds,thresh,tuwds,rf,
     +   'WINDOWS',eps,ncycle,lines
      endif
  17  FORMAT('  TOTAL UNIQUE WORDS   ',I8,8X,'THRESHOLD',11x,F5.3/
     +       '  TOP   UNIQUE WORDS   ',I8,8X,'RESTORING FORCE',5x,F5.3/
     +       '  TOTAL',A8,  '        ',I8,8X,'CYCLES',14x,i5/
     +       '  TOTAL LINES          ',I8)

      if(.not.iswin.and.isnet)write(3,76)clamp
  76   format(32x,'CLAMPING              ',A3)
C

      if(iswin.and..not.isnet)write(3,77)winsize,nslide
  77  format('  WINDOW SIZE          ',I8/
     +       '  SLIDE SIZE           ',I8)
C
      if(iswin.and.isnet)write(3,78)winsize,clamp,nslide
  78   format('  WINDOW SIZE          ',I8,8x,'CLAMPING',14x,a3/
     +       '  SLIDE SIZE           ',I8)
c
c      if(isnet)write(3,1)thresh,rf,ncycle
c    1 format(/'  NETWORK PARAMETERS:'
c     +//'  Threshhold = ', f4.3,'/Restoring Force = ', f4.3,
c     +'/Cycles ',i3)
      write(3,79)
  79  format(/)
c
c     copy vars list into "avars" for alpha sort purposes
c
      do 101 i=1,tuwds
  101 aindex(i)=windex(i)
c
c sort aindex into alpahbetic order using asword as guide.
c
  202 flip=0
      do 203 i=1,tuwds-1
      if(words2(aindex(i)).le.words2(aindex(i+1)))go to 203
      flip=1
      it=aindex(i)
      aindex(i)=aindex(i+1)
      aindex(i+1)=it
  203 continue
      if(flip.ne.0)go to 202
c
c output sorted lists with headings
c
      ipage=1
      write(3,300)
  300 format(T5,'DESCENDING FREQUENCY LIST',
     +T46,'ALPHABETICALY SORTED LIST'//
     +3X,'INDEX',2X,'FREQ.',2X,'PCENT.',2X,'WORD',15X
     +,'INDEX',2X,'FREQ.',2X,'PCENT.',2X,'WORD'/
     +3X,'-----',2X,'-----',2X,'------',2X,'---------------',4X
     +,'-----',2X,'-----',2X,'------',2X,'---------------'/)
      nprint=21
      if(iswin)nprint=23
c
      do 7 i=1,tuwds
      write(3,8)windex(i),wfreq(windex(i)),
     +percnt(windex(i)),words2(windex(i)),
     +aindex(i),wfreq(aindex(i)),percnt(aindex(i))
     +,words2(aindex(i))
    8 format(1X,I5,2X,I5,4X,f5.2,2X,A15,3X,I5,2X,I5,4X,f5.2,2X,A15)
c
c     paginiation
c
      nprint=nprint+1
      if(nprint.le. 60)go to 7
      if(i.eq.tuwds)go to 88
c      call prntcon(12,3)         !eject page
      nprint=9
      ipage=ipage+1
      write(3,301)ipage
  301 format('  Page ',i2/)
      write(3,300)
    7 continue
c
c     eject page
c
c 88   call prntcon(12,3)      !eject page
 88     close(23)
      return
      end
c
      subroutine ccount(nepi,nuniqe,isnet,ntepi,*)
      common /vector/ext
      common /temp/tfreq,twords,tdex
      common /in/win,ncon
      logical iswin,isnet
      common /window/iswin,winsize,winwrds,nslide
      real win(1300,1300),ext(1300)
      integer tdex(1300),tfreq(1300),winsize,winwrds
      character*15 word,twords(1300)
c
  6   if(iswin)go to 3
  1   read(14,100,end=55)word
 100  format(a15)
      if(word.eq.'-1')then
        nepi=nepi+1
        ntepi=ntepi-1
        go to 2
      endif
c
      do 15 i=1,nuniqe
      if(word.eq.twords(i))ext(i)=ext(i)+1
   15 continue
      go to 1
c
   3  do 11 i = 1,winsize
      read(14,100,end=55)word
      do 115 j=1,nuniqe
  115 if(word.eq.twords(j))ext(j)=ext(j)+1
  11  continue
      nepi=nepi+1
      ntepi=ntepi-1
      do 12 i = 1,winsize-nslide
  12  backspace(14)
c
   2  continue 
c write(*,133)nepi,ntepi
 133  format('+ Adding case: ',i5,5x,'Cases left: ',i5)
c      write(10,1001)ext
c 1001 format(8f5.2)
      if(isnet)return
      do 13 i=1,nuniqe
      do 17 j=1,nuniqe
      win(i,j)=win(i,j)+ext(i)*ext(j)
   17 continue
   13 continue
      do 16 i = 1,nuniqe
  16  ext(i)=0
      go to 6
  55  return 1
      end
c
      subroutine hiclus(n,words3)
c
c     if only i knew....
c
c     sd 3-20-91  e(100,100) = win(100,100)
c
C      NOTE   --   THIS TAKES 28 K ON THE GE 645--MODEL A
C       SMALLER DIMENSIONS, LESS CORE.

C      THE GE 645 HAS 36 BIT WORDS
C       IF YOUR MACHINE HAS LONGER OR SHORTER WORDS, YOU WILL
C       WANT TO MODIFY ARRAY  FORM  AND THE ASSOCIATED READ STATEMENT
C       ACCORDINGLY

C       THIS WILL TREAT UP TO 100 BY 100 DATA INPUT MATRICES
C        IF YOU WANT MORE, YOU WILL HAVE TO CHANGE THE OUTPUT ROUTINES
C       AND POSSIBLY NEED SECONDARY STORAGE

C      CURRENTLY, THE MAXIMUM AND MINIMUM METHODS ARE OUTPUTED
C       OTHER METHODS (SUCH AS AVERAGEING) MAY BE ADDED

C       I HAVE TRIED TO INDICATE WHERE YOU WOULD ADD THESE

      common /in/win,ncon
c
      LOGICAL W(1300)
      character*15 words3(1300)
      CHARACTER*1 WORD(15,1300)
      DIMENSION win(1300,1300)  ! d(100,100)
      DIMENSION XX(1300),LK(1300),LKI(1300),LKJ(1300)
c     *** DEBUG *** 
c     HC - 04/23/2007
c     Set P as 2600 = 2 * 1300
      DIMENSION P(2600)
c     DIMENSION P(600) ! 300 unique words
      DATA XXXX/1H^/, BLANK/1H /, PERIOD/1H./
      DATA is/-1/
      data WORD/19500*' '/ ! 19500 = 15 * 1300
c
c       call prntcon(15,3)      !start condensed printing.
c
C        READ IN   N = NUMBER OF VARIABLES
C                  IS = +1 IF MATRIX IS MATRIX OF DISTANCES
C                       -1 IF MATRIX IS MATRIX OF PROXIMITIES
c
c 10   READ(9,11)N,IS
C        IF N = NEGATIVE OR 0,  STOP

      IF (N .LE. 0) STOP
      S = IS
      N1 = N-1
      N2 = N1-1
      XN = N
c
c        READ IN TRIANGULAR MATRIX WITHOUT DIAGONAL
c        MAKE IT INTO A SYMMETRIC ARRAY WITH 0 DIAG.,  win(I,J)
c
      win(1,1) = 0.0
      DO 20 I=2,N
      win(I,I) = 0.0
      I1 = I-1
      DO 20 J=1,I1
 20   win(J,I) = win(I,J)
c
c      catexp1.for goes here
c
c        INITIALIZE TO FIRST METHOD
c RZ 3/13/84  INITIALIZE TO SECOND METHOD

C     K=1

      K = 2
C        START A METHOD

C        PRINT A TITLE

 50    GOTO(51,52),K
 51    CONTINUE !PRINT61
       write(3,61)
       GOTO 98
 52    CONTINUE !PRINT62
       write(3,62)

C             SET W(I) = .FALSE.
C                 LK(I)=0
C           W(I) = .TRUE. IF LI I HAS BEEN MERGED WITH SOME OTHER PT
C           LK(I) = LIST OF PTS MERGED, FOR OUTPUT PURPOSES---SEE BELOW

 98   DO 99 I=1,N
      W(I) = .FALSE.
      LK(I) = 0

C        COPY OFF THE ORIGINAL MATRIX INTO TEMPORARY STORAGE

C           ****   NOTE   ****

C     YOU MAY NOT WANT TO DO THIS IF PRESSED FOR SPACE

C           YOU COULD READ THE DATA DIRECTLY INTO THE D ARRAY, AND NOT
C           NEED THE SPACE FOR THE E ARRAY AT ALL

C       THEN USING TWO METHODS WOULD REQUIRE READING THE DATA TWICE

      DO 99 J=1,N
 99   CONTINUE
C99   D(I,J)=win(I,J)   SCOTT DANIELSEN 3-3-90, I WAS PRESSED FOR SPACE.

C        II COUNTS THE NUMBER OF PAIRS MERGED
      DO 181 II=1,N1
c      write(*,1112)ii
 1112 format('+ Cluster analysis: iteration ',i5,'             '
     +,'                           ')

C        FIND MIN PAIR DISTANCE


C        FIRST FIND TWO UNMERGED POINTS -- INITIALIZE X
C             X IS THE RUNNING MINIMUM DISTANCE

 100  DO 102 NI=2,N
      IF (W(NI)) GO TO 102
      NI1 = NI-1
      DO 101 NJ=1,NI1
 101  IF (.NOT. W(NJ)) GOTO 104
 102  CONTINUE

C        MUST BE ONE LEFT

      STOP
 104  X = win(NI,NJ)

C        BEGIN THE REAL BUSINESS OF FINDING THE CLOSEST PAIR
C        ONLY LOOK AT UNMERGED POINTS

      DO 150 I=2,N
      IF (W(I)) GO TO 150
      I1 = I-1
      DO 149 J=1,I1
      IF (W(J)) GO TO 149

C        THIS IS THE CENTRAL COMPARE STATEMENT

      IF((win(I,J)-X)*S .GT. 0.0) GO TO 149

C        WE HAVE FOUND A CLOSER PAIR -- UPDATE X, NI, NJ

      NI = I
      NJ = J
      X = win(I,J)
 149  CONTINUE
 150  CONTINUE

C        WE HAVE NOW LOOKED AT EVERY PAIR OF POINTS --
C             STORE X IN XX, NI AND NJ IN ARRAYS LKI AND LKJ

       XX(II) = X
       LKI(II) = NI
       LKJ(II) = NJ

C        POINT NJ IS CONSIDERED 'MERGED', WHERE BY THIS IS MEANT THAT
C                  IT NO LONGER POSSESSES A SEPARATE ROW OF THE MATRIX,
C                  BUT IS ENTIRELY SUBSUMED TO NI, (WHICH IS LARGER).

      W(NJ) = .TRUE.

C        SELECT UNMERGED POINTS, L, WHICH ARE NOT NEITHER NI NOR NJ

      DO 179 L=1,N
      IF (W(L)) GOTO 179
      IF (L .EQ. NI) GOTO 179

C        BRANCH ACCORDING TO METHOD, AND FIND D(NI,L) AND D(L,NI)

 160  GOTO(200,300),K

C        *****MINIMUM METHOD*****

C          win(NI,L)= MIN(win(NI,L),win(NJ,L))

 200  IF((win(NI,L)-win(NJ,L))*S .LE. 0.0) GOTO 170
      win(NI,L) = win(NJ,L)
      win(L,NI) = win(L,NJ)
      GOTO 170

C        *****MAXIMUM METHOD*****

C          win(NI,L)= MAX(win(NI,L),win(NJ,L))

 300  IF((win(NI,L)-win(NJ,L))*S .GE. 0.0) GOTO 170
      win(NI,L) = win(NJ,L)
      win(L,NI) = win(L,NJ)
 170  CONTINUE

C        GO GET MORE L'S

 179  CONTINUE

C        NOW GO BACK AND DO AGAIN USING NEW D ARRAY --(ITERATE)

 181  CONTINUE

C        ITERATION COMPLETE -- BEG IN PRINTOUT ROUTINE
C        WE NOW HAVE
C             1.   LKI ARRAY
C             2.   LKJ ARRAY
C                  THESE CONTANN SUCCESSIVE LINKS MERGED AT EACH
C                       ITERATI ON
C                  LKI(I) IS THE LARGER OF THE TWO
C                  LKJ(I) IS THE SMALLER OF THE TWO, THE ONE THAT IS
C                       MERGED (ELIMINATED) AT STEP I
C             3.  XX(I) = THE SIZE (VALUE) OF THE LINK MERGED AT STEP I


C        ARRANGE THE LINKS IN ORDER FOR PRINTING OUT


C        FIRST PUT THE LAST TWO POINTS CONNECTED INTO THE LK ARRAY

       LK(1) = LKJ(N1)
       LK(2) = LKI(N1)

C        THEN WORK BACKWARDS -- PICK A LINKAGE.   THE LARGER MEMBER OF
C                  THIS GROUP MUST BE ALREADY PLACED IN THE LK LIST.

C                  MOVE THE ENTRIES IN THE LK LIST OVER ONE, AND 'MAKE
C                  ROOM' FOR THE NEW ENTRY, WHICH GOES IMMEDIATELY TO
C                  THE LEFT OF THE LARGER ELEMENT.

C                  IF THESE COMMENTS ARE NOT CLEAR, IT IS IN THE COMBIN-
C                       ATORIAL NATURE OF THE SUBJECT THAT THIS BE SO,
C                       AND WORKING THROUGH A SIMPLE EXAMPLE SHOULD BE
C                       VERY EDIFYING.   IM DOING THE BEST I CAN.

       DO 550 II=2,N1
       JJ = N-II

C        FIND THE LKI ENTRY IN THE LK LIST

       DO 510 KK=1,II
 510   IF(LKI(JJ) .EQ. LK(KK)) GO TO 515

C      IT MUST BE SOMEWHERE

       STOP

C        SHOVE THINGS OVER AND PUT THE LKJ ENTRY IN THE LK LIST

 515  DO 530 IK=KK,II
      KI = II+KK-IK
 530  LK(KI+1) = LK(KI)
 550  LK(KK) = LKJ(JJ)

C        OVER AND OVER IN THE DO LOOP UNTIL ALL IS DONE

C        THE LK ORDER IS THE ORDER IN WHICH THE FINAL OUTPUT WILL
C                  BE ARRANGED

C        INDEX THROUGH THE LKI AND LKJ ARRAYS, USING I

      DO 585 I=1,N1

C        FIND LKI(I) IN THE LK ARRAY-- THE INDEX IS JI

       DO 565 JI=1,N
 565   IF(LK(JI) .EQ. LKI(I)) GO TO 570

C        IT MUST BE SOMEWHERE

       STOP

C        FIND LKJ(I) IN THE LK ARRAY-- THE INDEX IS JJ

 570   DO 575 JJ=1,N
 575   IF(LK(JJ) .EQ. LKJ(I)) GO TO 580

C        IT MUST BE SOMEWHERE

       STOP

580   LKI(I) = JI
      LKJ(I) = JJ
585   CONTINUE

C     LKI(II)AND LKJ(II)NOW CONTAIN THE ORDINAL POSITIONS OF THE II LINK


C        THE FOLLOWING CODE MAY BE DONE MORE THAN ONCE, DEPENDING ON
C             HOW LARGE N IS -- IF N IS MORE THAN 50, THE CODE IS DONE
C             TWICE.   THE ONLY DIFFERENCE BETWEEN THE TIMES IS WHICH
C             PROTION OF THE IP AND P ARRAYS IS PRINTED OUT -- OTHERWISE
C             THE SITUATION IS IDENTICAL.

C        M1 = STARTING I VALUE
C        M2 = FINAL I VALUE
C        IP1 = STARTING P VALUE
C        IP2 = FINAL P VALUE

C        THE FIRST TIME,
C             M1 = 1, IP1=1,  M2=MIN(50,N), IP2=100 OR 2*N-1, DEPENDING
C                            AS N GREATER THAN 50, OR NOT

C        THE SECOND TIME(WHEN N GREATER THAN 50),
C             M1=51, M2=N, IP1=100, IP2=2*N-1

C        SET UP FIRST TIME
c
c      3-26-91  Scott Danielsen  changed printout routine to
c               handle up to 300 unique words (more can be added
c               when storage is poss.-os/2)
c
      r = n
      rtest = r/50
c      rreps=(r/50)-amod(r/50,1)      ! changed amod to mod jw
       rreps = rtest-(rtest-int(rtest)) ! see p246 Microsoft Fortran Manual

      if(rtest.ne.rreps)rreps=rreps+1
      m1 = -49
      ip1 = -99
c
      do 132 kkk=1,rreps
         m1 = m1+50
         m2 = 50*kkk
         ip1 = ip1+100
         ip2 = kkk*100
         if(kkk.eq.rreps)then
           m2 = n
           ip2 = 2*n-1
         endif
c        *** DEBUG ***
c         print*, 'yo ',m1,m2,ip1,ip2
c
C 700   M1=1
C       IP1=1
C         IF (N .GT. 50) GO TO 710
C      M2 = N
C       ISAIL=1
C       IP2=2*N-1
C        GOTO800
C 710   M2=50
C       ISAIL=2
C       IP2=100
C       GOTO800
C
CC        SET UP SECOND TIME
C 750   CONTINUE !PRINT751
CC       write(3,751)
C       ISAIL=1
C       M1=51
C       M2=N
C       IP1=101
C       IP2=2*N-1
CC       PRINT*,'M1=',M1,'M2=',M2
Cc
C SCOTT DANIELSEN 11-6-85
C CHANGED HICLUS TO WRITE OUT WORDS
C INSTEAD OF NUMBERS AT THE TOP OF THE
C DENDOGRAM.
C
C
  800 DO 501 NC=M1,M2
      DO 500 NR=1,15
      WORD(NR,NC)=WORDS3(LK(NC))(NR:NR)
      IF (WORD(NR,NC).EQ.' ')WORD(NR,NC)='.'
  500 CONTINUE
  501 CONTINUE
C
c  997 if(m1.gt.50)call prntcon(12,3)       ! page eject
 997  DO 498 NR=1,15
      write(3,497)(word(nr,nc),nc=m1,m2)
C      if(m1.lt.50)write(3,497)(word(nr,nc),nc=m1,m2)
C      if(m1.gt.50)write(3,499)(word(nr,nc),nc=m1,m2)
  497 FORMAT(20X,50(1X,A1))
  499 FORMAT(21X,50(1X,A1))
  498 CONTINUE
C
C        PRINT FIRST DIGITS
c
C 800   DO551I=M1,M2
C 551   IP(I)=LK(I)/10
C       PRINT553,(IP(I),I=M1,M2)

C        PRINT SECOND DIGITS

C       DO552I=M1,M2
C 552   IP(I)=LK(I)-10*IP(I)
C       PRINT553,(IP(I),I=M1,M2)
C       PRINT554

C        INITIALIZE THE P ARRAY BY FILLING WITH BLANKS AND PERIODS

C        THE PERIODS REPRESENT VARIABLES AS YET UNCLUSTERED -- THE
C             BLANKS ARE FOR SPACING

c     HC - 04/23/2007 
c     2599 = 2 * 1300 - 1
c     For P array, now the program can analyze 1300 unique words

c     Old format (for 300 unique words)
C     DO 560 I =1,599,2 ! 599 = 2 * 300 - 1
       DO 560 I=1,2599,2
      P(I+1) = BLANK
 560  P(I) = PERIOD

      NC = 1
C     INDEX THROUGH THE MERGES, USIGG I

       DO 600 I=1,N1
C        COMPUTE THE APPROPRIATE PLACES IN THE P ARRAY

      JI = 2*LKI(I)-1
      JJ = 2*LKJ(I)-1

C        F+LL IN XS BETWEEN JI AND JJ IN THE P ARRAY

       DO 590 KK=JJ,JI
 590  P(KK) = XXXX

C        IF NEXT ONE IS CLUSTERED AT SAME LEVEL, DONT PRINT YET

      IF ((XX(I) .EQ. XX(I+1)) .AND. (I .NE. N1)) GO TO 600

C        PRINT OUT THE P ARRAY

 595  CONTINUE !PRINT596,XX(I),(P(III),III=IP1,IP2)
      write(3,596)xx(i),(p(iii),iii=ip1,ip2)
 600   CONTINUE
C
 132   continue


C C        GO TO(900,750),ISAIL
C 900   CONTINUE!PRINT901
c       write(3,901)
c
c      catexp2.for goes here.
c
C        GOTO NEXT METHOD -- IF DONE, GO READ ANOTHER N

C      K=K+1
       IF (K .EQ. 3)then
         print*, ' stopped in hiclus'
         stop   ! GO TO 10
       endif
C      GO TO 50

 11    FORMAT(I3,I2)
 12    FORMAT(11A4)
 61    FORMAT(20HCONNECTEDNESS METHOD//)
 62    FORMAT(15HDIAMETER METHOD//)
 751   FORMAT(9HCONTINUED)
 553   FORMAT(20X,50(1X,I1))
 554   FORMAT(//)
 596   FORMAT(1X,E16.8,4X,100A1)
 901   FORMAT(//,14H END OF METHOD)
c       call prntcon(18,3)          !end condensed printing
       return
       end
c
      subroutine net(nepi,nuniqe,labels,ncycle,ntepi,isnet,*)
      common /in/ win,ncon
      common /active/ av
      common /vector/ext
      common /parm/thresh,rf,h,logs,nfunc
      dimension ext(1300),win(1300,1300),av(1300,5)
      character*25 labels(1300)
      logical logs,isnet
c      data thresh/0.0/,rf/.1/,h/.01/,logs/.true./
c
      
      ncon=nuniqe
      do 1 i = 1,ncon
      do 1 j = 1,ncon
      win(i,j)=0.0
    1 continue
c      call random
c      call norm
  20  call zero(ncon)
      call ccount(nepi,ncon,isnet,ntepi,*55)
      call cycle(thresh,rf,EXT,NCYCLE,h,LOGS,nepi,ntepi,nfunc)
      go to 20
  55  call output(labels)
      return 1
      end
c ______________________________Subroutine Norm__________________

      Subroutine norm
      common /in/ win,ncon

      dimension win(1300,1300)

c subtract the grand mean from every entry to center on zero...

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
      do 2 i=1,ncon
      do 2 j=1,ncon
      if(win(i,j).eq.0)go to 2
      win(i,j)=win(i,j)/abs(big)
    2 continue

C DEBUG
C      DO 3 I=1,NCON
C      WRITE(6,4)(WIN(I,J),J=1,NCON)
C    4 FORMAT(' DEBUG WIN AFTER NORMALIZATION',//,5(F5.2,2X))
C    3 continue
C DEBUG

      return
      end

c_________________________________Subroutine Cycle_________________

      Subroutine Cycle(thresh,rf,ext,ncycle,h,LOGS,nepi,ntepi,nfunc)
      common /in/ win,ncon
      common /active/ av
      common sense,on,off
      logical learn
      logical sense
      logical logs
      CHARACTER*1 noy
      Dimension win(1300,1300),av(1300,5),ext(1300)

      data anet/0./
c      analog values.
c
        learn=.true.

c    4   print*, ' Analog?'
c        read(5,12)noy
c   12 FORMAT(A1)
c


        noy = 'Y'     !for now sd 3-14-91
C set the activation values of the input variables
C and zero out the external inputs for the next cycle
      do 5 i=1,ncon
      av(i,1)=av(i,1)+ext(i)
      if(sense)go to 5
      ext(i)=0.0
    5 continue
c
      do 1, i=2,ncycle+1
c      DO 10 II=1,1
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
c       IF(EXT(J).NE.0.)AV(J)=EXT(J) ! external constraints, as in life.

        anet=0.

        if(noy.eq.'y'.or.noy.eq.'Y')go to 2

       if(av(j,i).ge.thresh)av(j,i)=ON
       if(av(j,i).lt.thresh)av(j,i)=OFF
c
c       if(av(j).ge.thresh)av(j)=1.0
c       if(av(j).lt.thresh)av(j)=0.0

    2  continue
       if(learn)then
       call hebb(i,h,logs)       !(i,h,logs)
       ELSE
       endif
        ncyc=i-2
c       if(learn)write(*,1313)nepi,ntepi,i
c       if(learn)write(*,1313)nepi,ntepi,ncyc
 1313  format('+ Adding case: ',i5,5x,'Cases left: ',i5,
     +        t47,' Learning: cycle ',i5)
c  10   CONTINUE
c
        ncyc=i-2
c       if(learn)then
c       write(*,1313)nepi,ntepi,i
c 1313  format('+ Adding case: 'i5,5x,'Cases left: ',i5,
c     +        t47,' Learning: cycle ',i5)
c       endif
    1 continue
      return
      end

C_______________________________subroutine Hebb_________________________

      SUBROUTINE HEBB(itime,h,logs)
c      SUBROUTINE HEBB(h,logs)
      common /in/ win,ncon
      common /active/ av
      logical logs     !,isdone
      DIMENSION win(1300,1300),av(1300,5)
c
c       sd 3-15-91
c      added isdone,change,delta.  Delta is increment change in connection
c      for win(i,j).  Continue learning only if change > delta. It will get
c      stuck if it osscilates so clamping is on.
c
c      isdone = .true.
c      change = .01
      if(logs)then  !
      xbar=.5      ! set up the mean
      else         ! so that the logistic
      xbar=0       ! can take on positive and negative values
      endif        ! call only when logistic function is used

      do 1 i=1,ncon
      do 2 j=2,ncon

c      win(i,j)=win(i,j)+((av(i,itime)-xbar)*(av(j,itime)-xbar)*h)
      win(i,j)=win(i,j)+((av(i,itime))*(av(j,itime))*h)
c      win(i,j)=win(i,j)+((av(i)-xbar)*(av(j)-xbar)*h)
c      delta=abs(twin-win(i,j))
c      if(delta.gt.change)then
c        isdone = .false.
c      endif
    2 continue
    1 continue
      CALL NORM
c      if(isdone)print*, ' Boy, am I bored'
      return
      end
c_______________________________Subroutine Output________________________

      Subroutine output(labels)

      common /in/ win,ncon
      DIMENSION win(1300,1300)
      Character*15 labels(1300)
c
      write(17,44)(labels(i),i=1,ncon)
  44  format(a15)
      write(16,35)
  35  format('(8f10.6)')
      DO 30 I=1,NCON
      WRITE(16,36)(WIN(I,J),J=1,NCON)
   36 format (8f10.6)
   30 continue
c
      return
      end


c________________________________Subroutine Reset________________

      subroutine reset
      common /parm/thresh,rf,h,logs,nfunc
      character*1 noy
      character*80 hfile
      logical logs

c set some constants
  200 print*, ' '
      print*, ' Do you wish to set a new threshhold level?'
      read(5,1)noy
    1 format(a1)

      hfile='catpac/help/thresh.hlp'
      call assist(noy,hfile,*200)

      if(noy.ne.'y'.and.noy.ne.'Y')go to 2
 2221 print*, ' Enter new threshhold level.Current threshholdis ',thresh
      read(5,*,err=2223)thresh

      go to 2
 2223 hfile='catpac/help/thresh2.hlp'
      call assist('?',hfile,*2221)


    2 print*, ' How about a new decay rate?'
      read(5,1)noy

      hfile='catpac/help/decay.hlp'
      call assist(noy,hfile,*2)


      if(noy.ne.'y'.and.noy.ne.'Y')go to 3
 2225 print*, ' Enter new decay rate. Current decay rate is ', rf
      read(5,*,err=2231)rf

      go to 2224
 2231 hfile='catpac/help/decay2.hlp'
      call assist('?',hfile,*2225)


 2224 rf=1-rf

    3 PRINT*, ' Learning Rate?'
      read(5,1)noy

      hfile='catpac/help/srate.hlp'
      call assist(noy,hfile,*3)


      if(noy.ne.'y'.and.noy.ne.'Y')go to 14
 2227 PRINT*, ' Enter new Learning Factor. Current rate is ', h
      read(5,*,err=2226)h

      go to 14
 2226 hfile='catpac/help/srate2.hlp'
      call assist('?',hfile,*2227)


   14 print*,' Care to speculate on a functional form, Chiphead?'
      read(5,1)noy

      hfile='catpac/help/func.hlp'
      call assist(noy,hfile,*14)


      if(noy.ne.'y'.and.noy.ne.'Y')go to 4

   21 write (6,20)
   20 format(/' Please enter a number from 1 to 4, mon ami.'//
     +'      1 = Sigmoid (0 - +1)'/
     +'      2 = Sigmoid (-1 - +1)'/
     +'      3 = Hyperbolic Tangent (-1 - +1)'/
     +'      4 = Linear (-1 - +1)')
      read(5,*,err=2229)nfunc

      go to 2228
 2229 hfile='gaileo/help/srate2.hlp'
      call assist('?',hfile,*21)

 2228 if(nfunc.gt.4.or.nfunc.lt.1)go to 21
      if(nfunc.eq.1)logs=.true.
      if(nfunc.eq.1)off=0
      if(nfunc.ne.1)off=-1.0
      if(nfunc.ne.1)logs=.false.

    4 return
      end

c
c     Utilities
c
c      subroutine catwrt(prog,vers,unit,num)
c
c     writes date time program and version to file
c     with printer control characters or not.
c
c      character*(*) prog,vers
c      character*1 cc,esc
c      integer  hour,minute,second,hund,year,month,day,unit
c      call getdat(year,month,day)
c      call gettim(hour,minute,second,hund)
c      year = year-1900
c      if(num .eq.1)then
c      write(unit,1000)prog,vers,month,day,year,hour,minute,second
c 1000 format(8x,a10,5x,a5,
c     +15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2//)
c      return
c      endif
c      esc=char(27)
c      cc=char(14)
c      write(unit,100)esc,cc,prog,vers
c      call prntcon(20,3)
c      write(unit,101)month,day,year,hour,minute,second
c 100  format(2a1,10x,a10,2x,a5/)
c 101  format(27x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2//)
c      return
c      end
c
c       subroutine prntcon(ncc,nunit)
c       CHARACTER*1 cc,esc
c
c      this will send a control code to the printer with
c      esc before it.  Based on simple ASCII control codes
c      should work with any printer (ha ha).
c
c       esc=char(27)
c       cc=char(ncc)
c       write(nunit,313)esc,cc
c  313 format(2A1)
c      return
c      end
c
      subroutine yorn(*,*,*)
c
      character*1 ans
c    1 write(*,'(a,\)') ' ====>'
    1 read(*,2,end=3)ans
    2 format(a1)
      if(ans.eq.'Y'.or.ans.eq.'y')return 1
      if(ans.eq.'N'.or.ans.eq.'n')return2
      if(ans.eq.'?')return3
      write(*,4)
    4 format('  Please answer Yes or No.')
      go to 1
    3 write(*,5)
    5 FORMAT(' PLEASE, answer the question.')
      go to 1
      end
c
c      subroutine timer(ntime)
c      integer  hour,minute,second,hund,min,ttime
cc
c      call gettim(hour,minute,second,hund)
c      ttime=second+ntime
c      if(ntime.gt.60)then
c        minute=minute+1
c        ttime=ttime-60
c      endif
c 2    call gettim(hour,min,second,hund)
c      if(min.ge.minute.and.second.gt.ttime)return
c      go to 2
c      end
c
      subroutine feol(word,len)
c
c     will find end of line by ichar = 32 (sp)
c     not fool proof but good enough.
c
      character*80 word
      do 10 i = 1,80
      if(ichar(word(i:i)).eq.32)then
        len = i-1
        return
      endif
  10  continue
      return
      end
c
      subroutine zero(ncon)
      common /vector/ext
      common /active/ av
      real ext(1300),av(1300,5)
      do 200 i=1,NCON
      EXT(I)=0.
      do 200 j=1,5
      av(i,j)=0.0
  200 continue
      return
      end
C
c      subroutine clear
c      common /in/        win,ncon
c      common /freq/      kwords
c      common /vector/    ext
c      common /alpha/     words
c      common /temp/      tfreq,twords,tdex
c      common /window/    iswin,winsize,winwrds,nslide
c      common /prnt/      percnt
c      common /parm/      thres,rf,h,logs,nfunc
c      common /active/    av
c      common /stuff/nlines,nuniqe,ntepi,nepi,nwrds,nwords
cc
c      integer kwords(4350),tfreq(300),tdex(300)
c      real win(300,300),ext(300),av(300,5),percnt(300)
c      character*15 words(4350),twords(300)
c      logical logs,iswin
cc
c      data win/90000*0.0/,ncon/0/,kwords/4350*0/,
c     +ext/100*0.0/,words/4350*' '/,
c     +tfreq/100*0/,twords/100*' '/,tdex/100*0/,
c     +iswin/.false./,winsize/0.0/,
c     +winwrds/0.0/,nslide/0/,percnt/100*0.0/,
c     +thres/0.5/,rf/25/,h/.05/,logs/.true./,av/100*0.0/,
c     +nlines/0/,nuniqe/0/,ntepi/0/,nepi/0/,nwrds/0/,nwords/0/
cc
cc     serves as data dictionary and to zero out all v's.
cc
cc
cc      win     -  Main covariance or network matrix depending on function
cc      ncon    -  Number of concepts, lables, words whatever you want to call em.
cc      kwords  -  Initial tally of each unique word
c      words   -  Initial words
c      tfreq   -  Final tally of each unique word after stripping
c      twords  -  Final words after stripping
c      tdex    -  Index # of words after stripping
c      ext     -  either external input for network analysis or vector of
c                 of word frequncy/episode.  In either case it is a simple
c                 count of each uniqe word in an episode.
c      iswin   - logical: window analysis or not.
c      winsize - size of window.
c      nslide  - size of window slide e.g. 10 word window, slide 5 words.
c      winwrds - number of words that can be analyzed for a given
c                winsize & nslide.
c      percnt  - percentage occurance of each word after stripping
c      thresh  - activation threshold
c      rf      - restoring force
c      h       - hebb constant
c      logs     - logical: logistic or not
c      av      - activation matrix (i think we can get rid of this and use ext)
c      sense   - logical: clamp neurons on or not.
c       return
c       end
c________________________subroutine assist___________________________

      subroutine assist(ans,hfile,*)
      character*1 ans
      character*80 hfile,htext,ifile
c           ifile='catpac/help/'//hfile  ! hail Mary, full of grass... jw 3/17/13 

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
    1 format(//////////////////'      Hello, I''m ',a80,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//////)
c     	print*,'hfile=',hfile  ! ** debug **

c      print*,' Hello, I''m ',pname
c      print*,' You can enter ''?'' anytime you need help,'
c      print*, '...or just press ''ENTER'' to continue.'
      read(5,8)ans
    8 format(a1)

      if(ans.eq.'?')go to 7
      if(ans.eq.'@')then
      hfile='catpac/help/parrot.fun'
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
