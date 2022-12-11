      character*80 icons,code,ofile,dset,ifile
      character*6 maxval
      character*20 labels(40)
      logical mess/.false./
c
c     program to write a galileo control program
c
c     Scott Danielsen, 3-26-90.  Made intergal PC compatible.
c
c
      call intro('Intergal','V1.0')

  178 write(6,179)
  179 format(' Where would you like to save your runstream,'
     +,' O wise one?')
c      write(*,'(a,\)') ' ====>'
      read (5,2)ofile
      call assist(ofile(1:1),'in_rs.hlp',*178)
      open(unit=20,file=ofile,status='unknown',err=178)

 1001 write(6,300)
 300  format(' And where would you like the print file?')
c      write(*,'(a,\)') ' ====>'
      read(5,2)ifile
      call assist(ifile(1:1),'in_print.hlp',*1001)
      write(20,360)ifile

 1002 write(6,301)
 301  format(' ...and the coordinates?')
c      write(*,'(a,\)') ' ====>'
      read(5,2)ifile
      call assist(ifile(1:1),'in_crds.hlp',*1002)
      write(20,2)ifile

 1003 write(6,1)
    1 format(' What would you like to call this run?')
c      write(*,'(a,\)') ' ====>'
      read(5,2)code
      call assist(code(1:1),'in_tit.hlp',*1003)
    2 format(A40)
      write(20,3)code
    3 format('RUN NAME',T16,A40)

   51 write(*,216)
  216 format(' Where are your labels?')
c      write(*,'(a,\)') ' ====>'
      read(*,360)ifile
      call assist(ifile(1:1),'in_lfile.hlp',*51)
  360 format(A80)
      open(unit=21,file=ifile,status='old',err=51)
      do 261 I=1,50
      read(21,262,end=777)labels(I)
  262 format(A20)
  261 continue
  777 ncon=i-1
      close(unit=21)
      write(20,5)ncon
    5 format('N-CONCEPTS',T16,I2)

 1004 write(6,6)
    6 format(' How many datasets?')
c      write(*,'(a,\)') ' ====>'
      read(5,*,iostat=nerr)ndata
      if(nerr.ne.0)call assist('?','in_ndata.hlp',*1004)
      write(20,66)ndata
   66 format('N-DATASETS',T16,I2)

 1005 write(6,7)
    7 format(' What is your criterion pair?')
c      write(*,'(a,\)') ' ====>'
      read(5,2)code
      call assist(code(1:1),'in_crit.hlp',*1005)
      write(20,8)code
    8 format('CRITERION PAIR',T16,A40,/'CONLABELS')
      do 10 i=1,ncon
      write(20,12)labels(i)
   12 format(T16,A20)
   10 continue
c
 1006 write(6,13)
   13 format(' Do you want messages?')
      call yorn(*15,*16)
      call assist('?','in_mess.hlp',*1006)
      GO TO 16 ! all Galileos thru v54 can't do comparisons
               ! and messages in the same run. Don't fix this!
   15 write(20,17)
   17 format('OPERATIONS     COORDINATES')
c
      mess=.true.
 1007 write(6,39)
   39 format(' What is the number of the concept you wish to move?'
     +       /' (press return for list).')
c      write(*,'(a,\)') ' ====>'
      read(5,1021,iostat=nerr)mstart
      if(nerr.ne.0)call assist('?','in_start.hlp',*1007)
 1021 format(i2)
      if(mstart.eq.0)then
        write(*,1020)(i,labels(i),i=1,ncon)
        go to 1007
      endif
 1020 format(1x,i2,2x,a20)


 1015 write(6,41)
   41 format(' What is the number of the target concept?'
     +       /' (press return for list).' )
c      write(*,'(a,\)') ' ====>'
      read(5,1021,iostat=nerr)mtarg
      if(nerr.ne.0)call assist('?','in_targ.hlp',*1015)
      if(mtarg.eq.0)then
        write(*,1020)(i,labels(i),i=1,ncon)
        go to 1015
      endif


      go to 19
c
   16 if(ndata.le.1)then
         write(20,17)
         go to 19
      endif
      write(20,18)
   18 format('OPERATIONS     COMPARISONS')
c
   19 if(ndata .le. 1 .or. mstart .ne. 0)iflag=1

 1008 write(6,20)
  20  format(' Do you have any specifications?')
      call yorn(*83,*38)
      call assist('?','in_spec.hlp',*1008)
   83 write(20,23)
   23 format('SPECIFICATIONS')

 1009 write(6,24)
   24 format(' What is your maximum value filter?')
c      write(*,'(a,\)') ' ====>'
      read(5,67)maxval
      call assist(maxval(1:1),'in_maxval.hlp',*1009)
   67 format(A6)
      write(20,25)maxval
   25 format('MAXVAL',T16,A6)
c
      if(iflag.eq.1)go to 31

 1010 write(6,26)
   26 format(' Is this a time series?')
      call yorn(*28,*82)
      call assist('?','in_time.hlp',*1010)

   82 write(6,29)
   29 format(' Which space is your mainspace?')
c      write(*,'(a,\)') ' ====>'
      read(5,*,iostat=nerr)nmain
      if(nerr.ne.0)call assist('?','in_main.hlp',*82)
      write(20,30)nmain
   30 format('MAINSPACE',T16,I3)

   28 write(6,32)
   32 format(' Write the numbers of the free concepts'/
     +' separated by commas')
c      write(*,'(a,\)') ' ====>'
      read(5,2)icons
      call assist(icons(1:1),'in_fcons.hlp',*28)
      if(icons.eq.' ')go to 31
      write(20,33)icons
   33 format('FCONS',T16,A40)
   31 write(20,34)
   34 format('END OF SPECIFICATIONS')
c
   38 if(mess)then
         write(20,40)mstart
   40    format('AMG',/'START',T16,i2)  ! changed A2 to i2 3/14/13 jw
         write(20,42)mtarg
   42    format('TARGET',T16,i2/'PAIRS',T16,'1,2,3,4'/'END AMG') ! ditto jw
      endif
c
      write(20,43)
   43 format('OPTIONS        1,23,24,22,18,8,9,12,13,14,15,16'/
     +   'READ DATA')

C r zimmelman   6/22/84

      do 111 ii=1,ndata
 1011 write(6,55)ii
   55 format(' Where is dataset number',i2,' ?')
c      write(*,'(a,\)') ' ====>'
      read(5,76) dset
      call assist(dset(1:1),'in_dset.hlp',*1011)
   76 format(a50)
      write(20,56)dset
   56 format(a50)
  111 continue
      write(6,44)ofile
   44 format(' The Galileo Control Program is on '/,1x,a80/
     + ' by your command...'//' Press "ENTER" to return to Galileo Contr
     +ol')
      read(*,*)                 ! wait for william
      close (18)
      close(20)
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
c     subroutine prompt
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
    1 format(/////////////'      Hello, I''m ',a10,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//,
     +'      ([ctrl C] will send you back to Galileo'
     +' Control.)',//////)
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

      if(ans.eq.'?'.or.ans.eq.'@')go to 7
      if(ans.eq.'!'.or.ans.eq.'#')go to 7
      return
c     for dos
   7  continue

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
    1  read(*,2,end=3)ans
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