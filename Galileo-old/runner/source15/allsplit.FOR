c Program Allsplit
c all tripes reserved
c
c scott danielsen
c
c     This program will split 1 or more columns
c      on values or ranges.

c    12-28-92  added open-ended capability.  Increased line-length to 250.
c    removed reference to c:/ in assist.
C
      common /parm/ start,end,ncrds,ncard,datfil,ncols

      character*250 line(100),title,ofile(25),datfil
      character*1 split
      character*10 pass
      character*40 prog  ! added 3/13/13 jw
      character*6 vers   ! added 3/13/13 jw
      character*80 hfile ! added 3/13/13
      character*1 ans    ! added 3/13/13 jw
      integer start,end,stran(25),endran(25),value(25)
     +,stcrd,encrd,nout(25)
      integer*4 charint
      logical isopen/.false./
      
      prog='allsplit'
c
c 11   call intro('Allsplit','v2.00') ! eliminated subroutine intro 3/13/13 jw
11      write(6,1)prog
    1 format(///////////////'      Hello, I''m ',a10,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',/////)
     
      read(5,226)ans
c      if(ans.eq.' ')return ! removed 4/18/2013 jw
		if(ans.eq.' ')go to 1000
      hfile=prog//'.doc'
      if(ans.eq.'@')hfile='parrot.fun'
      call help(ans,hfile,*1000)
  226 format(a1)
c

c     get split paramaters. if study is registered with newsped,
c     col and card supplied by 'choice', otherwise user supplies info.
c

 1000 write(6,83)
   83 format('0What would you like to call this split?')
c      call prompt
      read(5,101)title
      hfile='al_tit.hlp'
      call help(title(1:1),'al_tit.hlp',*1000)

c    5 write(6,9669)
c 9669 format('0Do you currently have your study registered ',
c    +'with sped?')
c      call yorn(*647,*1001)
c
c  647 call choice(*600,*1001)
c
 1001 write(6,100)
  100 format(' Where is your data?')
c      call prompt
      read(5,101)datfil
      call help(datfil(1:1),'al_data.hlp',*1001)
  101 format(a80)

c     open data file
c
  996 open(unit=1,file=datfil,status='old',err=999)


      go to 1002
  999 write(6,998)
  998 format(' Error opening data file.'/' Please re-enter.')
      go to 1001

c
 1002  write(6,600)
  600  format (' Splitting an Open-ended, Oh Macedonian wonder?')
       call yorn(*611,*1222)
  611  isopen=.true.
       go to 1003

 1222 write(6,508)
  508 format(' How many lines per case?')
c      call prompt
      read(*,*,iostat=nerr)ncrds
      if(nerr.ne.0)call help('?','al_cards.hlp',*1222)

c
 1003 write(6,499)
  499 format('0 On what card would you like to split?')
c      call prompt
      read(*,*,iostat=nerr)ncard
      if(nerr.ne.0)call help('?','al_ncard.hlp',*1003)
c
 1004 write(6,500)
  500 format(' On how many columns would you like to split?')
c      call prompt
      read(*,*,iostat=nerr)ncols
      if(nerr.ne.0)call help('?','al_ncols.hlp',*1004)


      if(ncols.gt.10)then
        write(6,541)
  541 format(' You can only split on up to 10 columns.')
        go to 1004
      end if
c
c     get start and end column.
c

 1005 write(6,502)
  502 format(' What is the start column?')
c      call prompt
      read(*,*,iostat=nerr)start
      if(nerr.ne.0)call help('?','al_start.hlp',*1005)

      if(ncols.eq.1)then
        end=start
        go to 1007
      end if

 1006 write(6,503)
  503 format(' What is the end column?')
c      call prompt
      read(*,*,iostat=nerr)end
      if(nerr.ne.0)call help('?','al_end.hlp',*1006)
c
 1007 if(isopen)go to 1011

 1777 stcrd=1
      encrd=ncrds
      write(6,509)
  509 format(' Would you like all lines written out?')
      call yorn(*1011,*1008)
 2000 call help('?','al_alcrds.hlp',*1777)

c

 1008 write(6,9667)
 9667 format(' Please enter 1st line to be written out.')
c      call prompt
      read(*,*,iostat=nerr)stcrd
      if(nerr.ne.0)call help('?','al_stcrd.hlp',*1008)

 1009 write(6,9668)
 9668 format(' Please enter last line to be written out.')
c      call prompt
      read(*,*,iostat=nerr)encrd
      if(nerr.ne.0)call help('?','al_encrd.hlp',*1009)

c
 1011 write(6,504)
  504 format(' Would you like to split on (V)alues or (R)anges?')
c      call prompt
      read(5,106)split
      call help(split(1:1),'al_split.hlp',*1011)
      if((split .ne. 'v' .and. split .ne. 'V') .and.
     +(split .ne. 'r'.and. split .ne. 'R'))go to 1011
  106 format(a1)

c
 1012 write(6,505)
  505 format(' How many tests would you like to make?')
c      call prompt
      read(*,*,iostat=nerr)ntest
      if(nerr.ne.0)call help('?','al_ntest.hlp',*1012)
c
c     If the split is on ranges, get the ranges for each test.
c
      if(split.eq.'r'.or.split.eq.'R')then
      do 700 i=1,ntest
 1013 write(6,506)i
  506 format(' What is the start of range ',i2)
c      call prompt
      read(*,*,iostat=nerr)stran(i)
      if(nerr.ne.0)call help('?','al_stran.hlp',*1013)


 1014 write(6,507)i
  507 format(' What is the end of range ',i2)
c      call prompt
      read(*,*,iostat=nerr)endran(i)
      if(nerr.ne.0)call help('?','al_enran.hlp',*1014)

  700 continue
      go to 601
      end if
c
c     the split is on values get value for each test.
c
c      if(ncols.eq.1)then
c      write(6,540)
c  540 format('0 Would you like to split on all values (0-9)?')
c      call yorn(*888,*889)
c  888 do 24 i=0,9
c      j=i+1
c      value(j)=i
c   24 continue
c     go to 601
c      ntest=10
c      end if


c
 889  do 710 i=1,ntest
 1015 write(6,521)i
  521 format(' What is value of test ',i2)
c      call prompt
      read(*,*,iostat=nerr)value(i)
      if(nerr.ne.0)call help('?','al_value.hlp',*1015)
  710 continue
c
  601 do 23 i=1,ntest
      nuf=i+9
  605 if(split.eq.'V'.or.split.eq.'v')write(6,555)value(i)
      if(split.eq.'R'.or.split.eq.'r')write(6,556)stran(i),endran(i)

  555 format(' Where would you like to store value ',i10)
  556 format(' Where would you like to store range ',i10,1x,i10)
c      call prompt
      read(5,101)ofile(i)
      call help(ofile(i)(1:1),'al_ofile.hlp',*605)
      open(unit=nuf,file=ofile(i),status='unknown',err=605)
   23 continue


c     read in a case, assign a file number.

      write(*,1021)
 1021 format(' Splitting, boss man....'//)


  567 if(isopen)then
        i=1
        stcrd=1
 604    read(1,201,end=2)line(i)
 201    format(a250)
        if(line(i).eq.' ')go to 604
        if(line(i).eq.'-1')then
           encrd=i
           pass=line(ncard)(start:end)
           do 871 j = 1,ncols
  871      if(pass(1:1).eq.' ')pass=pass(2:10)
           num=charint(pass)
           go to 655
        endif
        i=i+1
        go to 604
      endif


      read(1,201,end=2)(line(I),I=1,ncrds)
      pass=line(ncard)(start:end)
      do 771 j = 1,ncols
  771 if(pass(1:1).eq.' ')pass=pass(2:10)
      num=charint(pass)
c
c     now make a test and write out the case.
c
  655  do 701 i=1,ntest
        nfile=i+9
        if(split.eq.'r'.or.split.eq.'R')then
          if(num.ge.stran(i).and.num.le.endran(i))go to 800
        else
          if(num.eq.value(i))go to 800
        endif
  701 continue
      go to 567
c
c      write out a case to appropriate file
c
  800 nout(nfile)=nout(nfile)+1
      ntot=ntot+1
        write(nfile,201)(line(j),j=stcrd,encrd)
      go to 567
c
    2 if(ntot.eq.0)then
      write(6,656)          ! ** changed unit from 5 to 6   JW 5/3/17
  656 format(' There were no cases written out.')
      go to 603
      end if
c
      if(split.eq.'R'.or.split.eq.'r')then
         write(6,530)'Range' ! commented out 5/6/2014 jw 
      else
         write(6,530)'Value' ! a wild stab in the dar 5/6/2014 jw
      end if
  530 format('1',5x,a10,5x,'Cases',5x,' Output File'/)
c
       do 988 j = 1,ntest
         if(split.eq.'r'.or.split.eq.'R')then
           write(6,911)endran(j),nout(j+9),ofile(j) ! a desperate attempt ** jw
 911       format('1',5x,i10,5x,i5,6x,a39)
         else
           write(6,911)value(j),nout(j+9),ofile(j) ! same panic mode
         endif
  988  continue
c
      do 667 i=1,ntest
      ii=i+9
  667 close(unit=ii)
c
  603 continue
c  603 write(6,105)
c  105 format('0 Would you like to make another split?')
c      call yorn(*12,*659)
c   12 rewind(1)
c      go to 11
c
  659 continue
      write(*,1020)
 1020 format (//' Press "ENTER" to return to Galieo Control.  Whewpf!')
      read(*,*)           ! wait for william
      end
c
      subroutine datwrt(prog,vers)
c
c     Scott Danielsen
c     11/27/89
c
      character*(*) prog,vers
      integer  hour,minute,second,hund,year,month,day
c      call getdat(year,month,day)
c      call gettim(hour,minute,second,hund)
c      year = year-1900
      write(6,100)prog,vers   ! no mas, por favor jw 4/27/2013month,day,year,hour,minute,second
 100  format('0',5x,a10,5x,a5)  !,
          !  NO MAS 4.17.2013+    15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2)
      return
      end
c
      integer * 4 function charint(anum)
c
c     this will convert an integer in 'character' form into an integer.
c
      character*10 anum
      len=index(anum,' ')-1
      charint=0
      do 9 j=1,len
         charint = charint + (ichar(anum(j:j))-48)*10**(len-j)
 9    continue
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

c  1   format(' ====>',/)  !this one format statement may work on the VAX too.
                          !someone should check it out, then we can lose
                          !this subroutine.
c  2   format('$====>')
c      return
c      end

c---- -----------------------Subrootine Intro-----------------------------


c      subroutine intro(prog,vers)
c      subroutine intro(prog,vers)
c
c     Scott Danielsen
c     11/27/89
c
c     character*(6) vers
c      character*80 hfile
c      character*(40) prog
c      character*1 ans

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

c      write(6,1)prog
c    1 format(//'      Hello, I''m ',a10,//,
c     +'      Please enter ''?'' anytime you need help,'//,
c     +'      ...or press ''ENTER'' to continue,',//,
c     +'      ([ctrl C] will send you back to Galileo'
c     +' Control.)',//////)
c      read(5,226)ans
c      if(ans.eq.' ')return
c      hfile=prog//'.doc'
c      if(ans.eq.'@')hfile='parrot.fun'
c      call help(ans,hfile,*3)
c  226 format(a1)
c  3   return
c      end




c------------------------------Subrootine help-------------------------

      subroutine help(ans,hfile,*)

      character*1 ans
      character*80 hfile
      character*80 ifile,htext
c
      j=0

      if(ans.eq.'?'.or.ans.eq.'@')go to 7
      return

c     for dos
   7  continue

c      ifile='/galileo/help/'//hfile
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
   2  print*,' Sorry, I can''t help you. You''re on your own!'
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
1      continue
      read(*,2,end=3)ans
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