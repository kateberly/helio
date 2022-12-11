		program sped
c  Scott Danielsen
c  All rights absurd
c  February 16, 1986( happy observed birthday George and Abe.)
c
c  PC VERSION November 27, 1989
c  OSX version March 24, 2013  jw
c
c  Hi, this program allows entry of
c  Galileo type data, demographic type data, and
c  open-ended type data.
c
      common/data/numpar,qtype,lower,upper,miss,demos,nlin
c      common/gal/nums,addr,ncons,lbls,nlines,npairs,id,mm
      common/gal/nums,addr,ncons,lbls,nlines,npairs,id ! took mm out 3/6/13 jw      
c      common/gal/addr,ncons,lbls,nlines,npairs,id,mm ! took nums out of commons 3/6/13 jw
      character*80 cltnm,crit,dir,studnm,study,userp(100,15)
      character*25 lbls(40)
      character*132 form/' '/,id*6
      integer lower(100),miss(100),qtype(100),upper(100),nums(500),
     +addr(500),demos(100),nlin(100),numpar,ncons
c      logical ask/.true./
		data ncases/0/
c
      call intro('SPED','v2.02')



c
c     get the name of the study directory & build image of
c     data file.
c
c    5 write(5,100)
c 100  format('0Please type in Study Directory.')
 5     print*,' What is the name of your study directory? '
C      write(*,'(a,\)') ' ====>'
      read(5,110,err=5,end=55)dir
      call assist(dir(1:1),'sp_sdir.hlp',*5)
c
      nume=index(dir,' ')-1
      study=dir
      study(nume+1:nume+10)='/study.dat'
      open(unit=11,file=study,status='old',err=4,recl=132)
  
      go to 3
c
c     this section of code will direct user in case of
c     error opening the data file.
c
    4 noops=noops+1
      if(noops.gt.1)then
      print*,' I''m sorry your study is not active or entry error.'
      print*,' Please check with your supervisor immediately.'
      stop
      end if
      dir=' '
      study=' '
      go to 5
c
c     Read in the relavant information from study file. Put back 3/13/13 jw
c
c 3    write(*,555)
c 555  format('0Data entry (Y/N)?')
c      call yorn(*556,*557)
c 556  ask = .false.
c
3     read(11,101)numpar
 101  format(i3) 
      read(11,101)ntype
      read(11,101)ncons
      read(11,102)cltnm
 102  format(a80)
      read(11,102)studnm
      read(11,103)(lbls(i),i=1,ncons)
 103  format(a25)
      read(11,102)crit
      read(11,104)form
 104  format(a132)
      do 8 i = 1,numpar
      read(11,102)(userp(i,j),j=1,15)
   8  read(11,105)qtype(i),lower(i),upper(i),miss(i)
 105  format(4i10)
c      do 310 i=1,numpar
c         do 309 j=1,10
c         read(11,3001)choise(i,j)
c    309  if(choise(i,j).eq.' ')go to 310
c    310  continue
c
c     Build images of output files, open temporary storage.
c
      call build(*4,dir,numpar,qtype)
c
c     Begin data collection.
c
    7 write(6,445)
  445 format(' Type in ID (-1 when done)')
C      write(*,'(a,\)') ' ====>'
      read(5,115,err=7,end=55)id
      call assist(id(1:1),'sp_id.hlp',*7)
      if (id .eq. ' ')go to 7
      if(id.eq.'-1')go to 55 ! ** debug another goddam hail mary  jw
      ncases=ncases+1
c      print*, ' at lin 100, ncases =  ',ncases  ! ** debug jw
      go to (1,2)ntype
c
   1  call askgal(mm)
   			print*,' Wierd!  '  ! ** debug jw
      call wrtout (mm)
c
   2  call askdem(id,userp)
      write(21,form)id,mm,(demos(i),i=1,numpar)
c
      npairs=0
      mm=1
      go to 7
   55 continue
c
      write(6,177)ncases
  177 format('  You have entered ',i2,' cases.')
      close(1)
  110 format(a79)
  115 format(a6)
      end
c
      subroutine askdem(id,userp)
c
      common/data/numpar,qtype,lower,upper,miss,demos,nlin
      character*80 userp(100,15)
      character id*6
      integer qtype(100),lower(100),upper(100),miss(100)
     +,demos(100),nlin(100)
c      logical ask
c
      int=0
c
      do 10 i=1,numpar
c      write(5,1000)i,userp(i,1)
      print*, i, userp(i,1)
      do 15 j=2,15
      if(userp(i,j).eq.'-1')go to 4
c      write(5,100)userp(i,j)
		print*, userp(i,j)
   15 continue
 1000 format(/1x,i3,') ',a76)
  100 format(1x,a79)
c
    4 go to (2,1,3)qtype(i)
c
c      if(ask)then
c      write((6,100)(choise(i,j),j=1,upper(i))
    3 continue
      go to 1
c
    2 int=int+1
      call open(int,id,demos(i),*10)
c
    1 call retans(demos(i),*888)
c
      if(demos(i).gt.upper(i))go to 5
      if(demos(i).lt.lower(i))go to 5
      go to 10
    5 print*, ' Value out of range, please reenter.'
      go to 1
c
  888 demos(i)=miss(I)
   10 continue
c
c     check for errors.
c
 1010 print*, ' '
C      write(*,'(a,\)') ' Demographic corrections(Y/N)? '
		PRINT*,' Demographic corrections (Y/N)? '
      call yorn(*61,*62)
      call assist('?','sp_demo.hlp',*1010)
  61  print*, ' '
      print*, ' Type in Question number to correct.'
      print*,' '
      print*,'         (-1 when done)'
C      write(*,'(a,\)') ' ====>'
      read(*,811,iostat=nerr,end=62)ndemo
      if(ndemo.lt.0)go to 62   ! ** replaces the now fatal ctrl z ** jw 10/16/13
      if(nerr.ne.0)call assist ('?','sp_ndemo.hlp',*61)
      if (ndemo.eq.0)go to 61
      if(ndemo.gt.numpar)then
         print*, ' '
         print*, ' Question number out of range, please re-enter.'
         go to 61
      endif
      if(upper(ndemo).eq.0)then
        print*, ' '
        print*, ' Cannot correct an open-ended response.'
        go to 61
      endif
      print*, ' '
   56 write(5,1000)ndemo,userp(ndemo,1)
      write(5,1001)demos(ndemo)
 1001 format(' Incorrect response: ',i10)
      print*,' '
      print*, 'Enter correct response.'
      call retans(ntemp,*666)
      if(ntemp .gt. upper(ndemo).or.
     +  ntemp .lt.lower(ndemo))then
        print*,' '
        print*, ' Value out of range, please reenter.'
        go to 56
      endif
      demos(ndemo)=ntemp
      go to 61
 666  demos(ndemo)=miss(ndemo)
      go to 61
 811  format(i10)
c
c      end error checking.
c
  62  return
      end
c
      subroutine wrtout(mm)
c		subroutine wrtout
c      common/gal/nums,addr,ncons,lbls,nlines,npairs,id,mm
      common/gal/nums,addr,ncons,lbls,nlines,npairs,id ! took mm out 3/6/13 jw      
c      common/gal/addr,ncons,lbls,nlines,npairs,id,mm ! took out nums 3/6/13 jw
      integer nums(500),addr(500),st,en  ! added st,en 3/6/13 jw
      character*25 lbls(40)
      character*6 id
c      print*,' Hello from wrtout...'    ! ** debug
      st=1
      en=8
c      print*,' do de do de do.'   ! ** debug
c      do 17 mm=1,nlines
      do 17 i=1,nlines  ! ** debug

c      print*,'dirtbag revolution.'  ! ** debug
      if(en.gt.npairs)en=npairs
c      print*,' la, la, la, a, la,'  ! ** debug
    5 write(21,107)id,mm,(addr(no),nums(no),no=st,en)
  107 format(a6,i2.2,8(i4.4,i5))
      st=en+1
      en=st+7
   17 continue
      return
      end
c
      subroutine open(num,id,nans,*)
c
c     Scott Danielsen
c     February 16, 1986 (Happy observed birthday George!)
c     all rights reserved
c
c     this will allow open ended interview
c     data to be entered along with demographics and galileo
c     data. Data will be written to [study]quest.dat.
c     [study] and questnn is supplied by newsped where
c     nn=question number.
c
      common /bild/file
      character*79 line
      character file(25)*80,id*6
      logical wrt
c
c     Collect a response.
c
      nans=1
      nline=0
      wrt=.true.
c
c		print*,' at line 257, file(num) =   ',file(num)   ! ** debug jw
      open(unit=25,file=file(num),status='unknown',err=88)
      do 25 kk=1,50000
  25    read(25,14,end=27)ans
  27  backspace 25
c
   71 print*, ' '  ! ** debug **

      print*, '(type "done" when done) '
C   13 write(*,'(a,\)') ' ====>'
13     continue
         read(5,14,err=13,end=99)line
c  13       read(5,14,end=99)line
        if(line.eq.' ')go to 71
        if(line.eq.'one')go to 99  ! ** debug another goddam hail mary jw
c        print*,' at 271, wrt, id, line =  ',wrt,id, line ! ** debug so what is wrt anyway?
        if(wrt)write(25,15)id
c        write(25,15)id
        wrt=.false.
        nline=nline+1
c        print*,' at line 274, line =   ',line   ! ** debug
        write(25,14)line
      go to 13
c
   99 if(nline .eq. 0)nans=9
      if(.not.wrt)write(25,16)
c      write(25,16)
      close(25)
      return 1
c
   88 print*,' Error opening interview data file.'
      Print*, ' Please contact your supervisor immediately.'
      stop
   16 format('-1')
   15 format(a6)
   14 format(a80)  ! changed to a 80 3/13/13/ jw 
      end
c
      subroutine inst(crit)
      character*80 crit
      write(6,100)crit
  100 format('1',5x,' Please estimate how different or "far apart" each of the
     +following'/5x,' words or phrases is from each of the others.
     +The more different,'/5x,' or further apart they seem to be,
     +the larger the number you should'/5x,' write.  To help you
     +know what size number to write, remember'//10x,a70//5x,' If two
     +words or pharases are not different at all, please write'
     +/5x,' zero(0).  If you have no idea, we will leave the space
     +blank.')
      return
      end
c
      subroutine build(*,dir,numpar,qtype)
      common /bild/file
      character*(*)dir
      character*80 file(25),ogal
      character*13 gfile/'/galileo.dat'/
      integer qtype(100)
      character*10 intchar,anum
      character*1 ans
c
c     There has got to be a better way.
c       (there is 12-31-87!)
c
c     Now build the image of the gal/demo output file.
c
      nume=index(dir,' ')-1
      ogal=dir
      ogal(nume+1:nume+12)=gfile
c
      open (unit=21,file=ogal,status='unknown',err=88)
      do 1 j = 1,50000
   1  read(21,131,end=2)ans
   2  backspace 21
c
c     build images of open-ended output files.
c
      do 55 j = 1,numpar
         if(qtype(j) .eq. 1) then
            int=int+1
            nhold=j
            anum=intchar(nhold,len)
            if(anum(9:9).eq.' ')anum(9:9)='0'
            if(anum(8:8).eq.' ')anum(8:8)='0'
            file(int)=dir!//extra//anum(10-len:10)
            file(int)(nume+1:nume+6)='/quest'
            file(int)(nume+7:nume+9)=anum(8:10)!(10-len:10)
            file(int)(nume+10:nume+14)='.dat'
c            print*,' file(int)=',file(int) ! ** debug jw
         end if
 55   continue
      return
c
   88 print*, ' Error opening data file.'
      print*, ' Please contact your supervisor immediately.'
      stop
 131  format(a1)
      end
c
      subroutine retans(newnum,*)
c
c     this subroutine will read a number in character
c     form, verify that it is a number and return
c     an integer.
c
c
      character*80 anum
      integer*4 newnum,charint
c
c    1 write(*,'(a,\)') ' ====>'
1      read(5,100,err=2,end=3)anum
      call assist(anum(1:1),'sp_anum.hlp',*1)
      if(anum.eq.' ')return 1
      len=index(anum,' ')-1
      do 4 i=1,len
   4  if(index('1234567890',anum(i:i)).eq.0)go to 2
      newnum=charint(anum)
      return
c
    2 write(6,101)anum
  101 format(1x,a10,' is not a number please re-enter.')
      go to 1
c
   3  print*,' Please don''t give up.'
      print*, ' Re-enter number.'
      go to 1
c
  100 format(a10)
      end
c
       integer * 4 function charint(anum)
c		real*8 function charint(anum)  ! changed 3/6/13 jw
c		real*8 ii,real ! added 3/6/13 jw
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
c
      character * 10 function intchar(k,len)
      real*8 i,real  ! stab in the dark **      
      logical skip
      intchar = '          '
c
c     this function will accept an integer and return
c     the right-justified character equivalent.
c
      i=k 
      skip=.true.
      do 9 j=1,10
        n=10-j
        real=dint(i/10**n)
        if(real .eq. 0 .and. skip)then
           len=10-j
           go to 8
        endif
        skip = .false.
        intchar(j:j)=char(idint(real)+48)
 8      i=i-real*10**n
 9    continue
c      print*,'intchar+len =',intchar,len
      return
      end
c
      subroutine askgal(mm)
c
c      common/gal/nums,addr,ncons,lbls,nlines,npairs,id,mm
      common/gal/nums,addr,ncons,lbls,nlines,npairs,id  ! took mm out 3/6/13 jw     
      integer nums(500),addr(500),ncons,cell
      character*25 lbls(40)
      character*6 id
c
c  1   if(ask)call inst(crit)
      do 11 i=1,ncons-1
      do 9 j=1,ncons
      if(j.le.i)go to 9
c      if(np.eq.8.and.ask)then
c         write(5,399)crit
c  399 format('0Remember, ',a69)
c         np=0
c      end if
c      np=np+1
      npairs=npairs+1
      addr(npairs)=i*100+j
      write(*,1212)addr(npairs),lbls(i),lbls(j)
      call retans(nums(npairs),*888)
      nums(npairs)=min(nums(npairs),99999)   !if resp is >99999 ans=99999
      go to 9
  888 nums(npairs)=-1
    9 continue
   11 continue
c
c
      real=npairs/8
      nlines=aint(real)+1
c
c     check for errors
c
c      if(.not. ask)then
 1013    print*, ' '
c         write(*,'(a,\)') ' Galileo corrections (Y/N)? '
			print*,' Galileo corrections (Y/N)? '
         call yorn(*77,*78)
         call assist('?','sp_gacor.hlp',*1013)
   77    print*, ' '
         print*, 'Type in cell #.  (-1 when done).'
C         write(*,'(a,\)') ' ====>'
         read(*,711,iostat=nerr,end=78)cell
         if(cell.lt.0)go to 78   ! ** can't be doing ctlz's anymore -- it stops the process.
         if(nerr.ne.0)call assist('?','sp_cell.hlp',*77)
         if (cell.eq.0)go to 77
         do 71,j=1,npairs
         if(cell .eq. addr(j))go to 37
   71    continue
         print*, 'Incorrect cell address, try again'
         go to 77
   37    print*, ' '
         write(*,345)addr(j),nums(j)
 345     format(1x,i4.4,2x,i5)
         print*, 'Enter correct response.'
         call retans(nums(j),*777)
         go to 77
 777     nums(j)=-1
         go to 77
 711     format(i4)

c      endif
 78    return
 1212 format(1x,i4.4,5x,a20,5x,a20)
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
C      subroutine prompt
c
c     for da dos

C      print 1

c     for da vax

c      print 2

C  1   format(' ====>',\)  !this one format statement may work on the VAX too.
                          !someone should check it out, then we can lose
                          !this subroutine.
C  2   format('$====>')
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

C      call getdat(year,month,day)
C      call gettim(hour,minute,second,hund)
C      year = year-1900
C      print 100,prog,vers,month,day,year,hour,minute,second
C 100  format(//////////5x,a10,5x,a5,
C     +15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2)

c     for everybody!

      write(6,1)prog
    1 format(/////////////'      Hello, I''m ',a30,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',//////)
     
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
 1     read(*,2,end=3)ans
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