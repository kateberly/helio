C  Scott Danielsen
C  All rights absurd
C  march 5, 1987
C
C
      character*80 cltnm/' '/,crit/' '/,dir/' '/,qdisc(3),studnm/' '/,
     +study/' '/,userp(100,5)/500*' '/
      character*25 lbls(40)/40*' '/
      character*132 form/' '/
      integer lower(100)/100*0/,miss(100)/100*0/,qtype(100)/100*0/
     +,upper(100)/100*0/,year,month,day
      data ntype/0/,ncons/0/,numpar/0/
      data qdisc/'Open-Ended','Magnitude Estimation',
     +         'Multiple Choice/Likert-Type'/
c
c *********************************************************
c
      call intro('Read','v2.00')

    5 write(6,1013)
 1013 format('0Please type in Study Directory.')
      call prompt
      read(5,110)dir
  110 format(a71)

      call assist(dir(1:1),'re_sdir.hlp',*5)
c
      nume=index(dir,' ')-1
      study=dir
      study(nume+1:nume+10)='\study.dat'
      open(unit=1,file=study,status='unknown',err=5,recl=132)
      open(unit=4,status='unknown')
      go to 3
c
c     this section of code will direct user in case of
c     error opening the data file.
c
    4 noops=noops+1
      if(noops.gt.1)then
      print*,' I''m sorry your study is not active.'
      print*,' Please check with your supervisor immediately.'
c      call exit
      stop
      end if
      dir=' '
      study=' '
      go to 5
c
c     Read in the relavant information from study file:
c     demographic questions, upperbounds, lowerbounds and missing
c     values for each. also output format and number of demo quests.
c
 3    read(1,3000)numpar
 3000 format(i3)
      read(1,3000)ntype
      read(1,3000)ncons
      read(1,3001)cltnm
 3001 format(a80)
      read(1,3001)studnm
      read(1,3002)(lbls(i),i=1,ncons)
 3002 format(a20)
      read(1,3001)crit
      read(1,3003)form
 3003 format(a132)
      do 356 i = 1,numpar
      read(1,3001)(userp(i,j),j=1,5)
  356 read(1,3004)qtype(i),lower(i),upper(i),miss(i)
 3004 format(4i10)
c      do 310 i=1,numpar
c 310    read(1,3001)(choise(i,j),j=1,20)
c
c
      call getdat(year,month,day)
      year=year-1900
      write(*,333)month,day,year
      write(4,333)month,day,year
  333 format(/////24x,' Galileo Research Systems'
     +/'0',31x,i2.2,'/',i2.2,'/',i2.2)
      write(*,334)cltnm,studnm
      write(4,334)cltnm,studnm
  334 format(////'0Client Name: ',a67/'0Study Name: ',a68)
c
      go to(1,2)ntype
    1 write(*,300)ncons
      write(4,300)ncons
  300 format(/////' This is a ',i2,' concept Galileo.'//)
      do 10 i=1,ncons
      write(*,111)i,lbls(i)
  10  write(4,111)i,lbls(i)
  111 format(1x,i2,') ',a25)
      write(*,337)crit
      write(4,337)crit
  337 format('0Criterion Pair = ',a63/' ')
      write(*,309)
      write(4,309)
c
c
    2 write(*,301)numpar
      write(4,301)numpar
  301 format('0There are ',i3,' demographic questions.')
      write(*,309)
      write(4,309)
      do 11 i=1,numpar
      print*,' Press [ENTER] to continue.'
      read(*,*)                           ! wait for william
c      print*,' '
      do 1111 j=1,5
      if(userp(i,j).eq.'-1')go to 1113
      write(*,302)userp(i,j)
 1111 write(4,302)userp(i,j)
  302 format(1x,a79/)
 1113 write(*,306)qdisc(qtype(i))
      write(4,306)qdisc(qtype(i))
  306 format(1x,a30)
c      if(qtype(i).eq.3)then
c      write(*,303)(choise(i,j),j=1,upper(i))
c      write(4,303)(choise(i,j),j=1,upper(i))
c      end if
      write(*,305)upper(i),lower(i),miss(i)
      write(4,305)upper(i),lower(i),miss(i)
      write(*,309)
      write(4,309)
  309 format(' ---------------------------------------------------------
     +--------------------')
   11 continue
  305  format('0Upper Bound = ',i5,2x,'Lower Bound = ',i5,
     +2x,'Missing Value = ',i5)
  303 format(10x,a70)
      write(*,304)form
      write(4,304)form
  304 format('0The output format for the demographics is:',/'0',1x,a79)
      close(3)
      print*, ' '
c      write(*,77)
c77    format('0Would you like to print a copy of the study?'
c     +/'$====>')
c      call yorn(*55,*56)
c   55 close(unit=4,dispose='print/delete')
c      go to 57
c   56 close(unit=4,dispose='delete')
c   57 call exit
   57 continue
      end
c

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
      subroutine prompt
c
c     for da dos

      print 1

c     for da vax

c      print 2

  1   format(' ====>',\)  !this one format statement may work on the VAX too.
                          !someone should check it out, then we can lose
                          !this subroutine.
  2   format('$====>')
      return
      end

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

      integer  hour,minute,second,hund,year,month,day
c
c     for da vax

c      call date(dat)
c      call time(tim)
c      print 101,prog,vers,dat,time
c 101  format(//////////5x,a10,5x,a5,15x,a9,5x,a8)

c      for da dos

      call getdat(year,month,day)
      call gettim(hour,minute,second,hund)
      year = year-1900
      print 100,prog,vers,month,day,year,hour,minute,second
 100  format(//////////5x,a10,5x,a5,
     +15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2)

c     for everybody!

      write(6,1)prog
    1 format(//'      Hello, I''m ',a10,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//,
     +'      ([ctrl C] will send you back to Galileo'
     +' Control.)',//////)
      read(5,226)ans
      if(ans.eq.' ')return
      hfile=prog//'.doc'
      if(ans.eq.'@')hfile='parrot.fun'
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

      if(ans.eq.'?'.or.ans.eq.'@')go to 7
      return

c     for dos
   7  continue

      ifile='c:\galileo\help\'//hfile

c     for mr. vax

c      ifile='[com0.comjoew.galileo.help]'//hfile(1:len)

      open(unit=19,file=ifile,status='old',err=2)

      do 3 i=1,100
      j=j+1
      if(j.eq.22)then
      j=0
      print 300
 300  format('0Press ''ENTER'' to continue.')
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
    1 call prompt
      read(*,2,end=3)ans
    2 format(a1)
      if(ans.eq.'Y'.or.ans.eq.'y')return 1
      if(ans.eq.'N'.or.ans.eq.'n')return 2
      if(ans.eq.'?')return
      write(*,4)
    4 format('0I need a YES or NO.  Enter ''?'' for help')
      go to 1
    3 write(*,5)
    5 FORMAT(' PLEASE, answer the question.')
      go to 1
      end