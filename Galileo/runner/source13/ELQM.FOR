      program ELQM !(ELectronic Questionairre Maker)
c     Scott Danielsen
c     March 5, 1987
c     All Rights Reserved
c
c     PC VERSION November 27, 1989.
c	  OSX version March 24, 2013. jw
c
c     This program will set up a data element for
c     Newsped, the electronic Questionnairre.
c
      common /frm/upper,miss
      character*80 cltnm/' '/,crit/' '/,dir/' '/,studnm/' '/,
     +study/' '/,study2/' '/,userp(100,15)/1500*' '/,infile
      character*25 lbls(40)/40*' '/
      character*132 form /'(a6,i2.2,'/
      integer*4 lower(100)/100*0/,miss(100),
     +qtype(100)/100*0/,upper(100)
      data ntype/0/,ncons/0/,numpar/0/
c
c     This program will work online or with an input file.
c
      call intro('ELQM','v2.01')
c

c
 1001 write(*,160)
 160  format(' Do you have an input file?')
      call yorn(*40,*41)
      call assist('?','el_input.hlp',*1001)

  40  write(*,161)
 161  format(' Please enter name of input file')
c      write(*,'(a,\)') ' ====>'
      read(*,102)infile
      call assist(infile(1:1),'el_infil.hlp',*40)
      open(unit=3,file=infile,err=40)
      open(unit=4,file='temp',err=40)
      lfni= 3
      lfno= 4
      go to 5

  41  lfni= 5
      lfno= 6
c
c
    5 write(lfno,1013)
 1013 format(' Please type in Study Directory.')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,110,err=999,end=999)dir
      call assist(dir(1:1),'el_sdir.hlp',*5)

  110 format(a71)
      nume=index(dir,' ')-1
      study=dir
      study2=dir
      study(nume+1:nume+10)='/study.dat'
      open(unit=1,file=study,status='unknown',err=5,recl=132)
      study2(nume+1:nume+10)='/study.lbl'
      open(unit=2, file=study2, status='unknown',err=5)
c
 1002  write(lfno,204)
  204 format('  Please enter Name of Project Supervisor.')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines+1
      read(lfni,102,err=999,end=999)cltnm
      call assist(cltnm(1:1),'el_cltnm.hlp',*1002)
c
 1003 write(lfno,205)
  205 format('  Please enter Title of Study.')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,102,err=999,end=999)studnm
      call assist(studnm(1:1),'el_stunm.hlp',*1003)
c
 1004 write(lfno,201)
  201 format(' ',10x,' Please enter type of study:'
     +/' ',12x,' (1)  Galileo and Survey Questions '
     +/' ',12x,' (2)     Survey Questions only ')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,100,iostat=nerr)ntype
      if(nerr.ne.0)call assist('?','el_ntype.hlp',*1004)

c
      go to (1,2)ntype
c
c     collect concept labels if Galileo.
c
    1 ncons=0
   77 ncons=ncons+1
 1005 write(lfno,335)ncons
  335 format(' Please enter concept ',i2,' (-2 to end)')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,101,end=999,err=999)lbls(ncons)
      call assist(lbls(ncons)(1:1),'el_lbls.hlp',*1005)
      if(lbls(ncons).eq.'-2')go to 78
      if(lbls(ncons).eq.' ')go to 1005
      write(2,101)lbls(ncons)
      go to 77
  78  ncons=ncons-1
c
 1006 write(lfno,336)
  336 format(' What is your Criterion Pair?')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,102,err=999,end=999)crit
      call assist(crit(1:1),'el_crit.hlp',*1006)
c
c     collect demographic information.
c
    2 numpar=1
    6 write(lfno,333)numpar
      j=1
  333 format(' Enter userp ',i2,'. -1 for end of question,'
     +' -2 to end')
c   65 write(lfno,'(a,\)') ' ====>'
   65 continue
      nlines=nlines +1
      read(lfni,102,end=999)userp(numpar,j)
      call assist(userp(numpar,j)(1:1),'el_parm.hlp',*6)
      if(userp(numpar,j).eq.'-1')go to 66
      if(userp(numpar,j).eq.'-2')go to 55
      j=j+1
      go to 65
c
   66 write(lfno,200)
  200 format(' Enter type of question.'/' ',10x,' (1) Open-Ended'
     +/' ',10x,' (2) Magnitude Estimation'
     +/' ',10x,' (3) Multiple Choice or Likert-Type')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,100,end=999,iostat=nerr)qtype(numpar)
      if(nerr.ne.0)call assist('?','el_qtype.hlp',*66)
c
      if(qtype(numpar).eq.1)then
         miss(numpar)=9
         go to 7
      end if
c
 1007 write(lfno,667)
  667 format (' Its lower bound.')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,105,end=999,iostat=nerr)lower(numpar)
      if(nerr.ne.0)call assist('?','el_lower.hlp',*1007)
c
 1008 write(lfno,666)
 666  format(' Its upper bound.')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,105,end=999,iostat=nerr)upper(numpar)
      if(nerr.ne.0)call assist('?','el_upper.hlp',*1008)
c
 1009 write(lfno,668)
  668 format(' And its missing value.')
c      write(lfno,'(a,\)') ' ====>'
      nlines=nlines +1
      read(lfni,105,iostat=nerr,end=999)miss(numpar)
      if(nerr.ne.0)call assist('?','el_miss.hlp',*1009)
c

c 67  if(qtype(numpar).ne.3)go to 7
c
c      do 11 i=1,upper(numpar)
c      write(lfno,3001)i
c 3001 format('0Please enter choice ',i2)
c      read(lfni,102,end=999,err=999)choise(numpar,i)
c   11 continue
c
      go to 7
 999  write(6,1313)nlines
 1313 format(' Error reading data on line', i3,/
     +'Try again or check your input file.')
      stop
    7 numpar=numpar+1
      go to 6
   55 numpar=numpar-1
      call frmat(form,numpar)
c
c     write it out.
c
      write(1,103)numpar
      write(1,103)ntype
      write(1,103)ncons
      write(1,102)cltnm
      write(1,102)studnm
      write(1,101)(lbls(i),i=1,ncons)
      write(1,102)crit
      write(1,104)form
      do 310 i = 1,numpar
        write(1,102)(userp(i,j),j=1,15)
        write(1,106)qtype(i),lower(i),upper(i),miss(i)
 310  continue
c      do 310 i=1,numpar
c 310    write(1,102)(choise(i,j),j=1,10)
      write(6,207)study
  207 format('  All done.  Information stored on file:'/2x,a78
     +//' WHEWPF! Press "ENTER" to return to Galileo Control')
      read(*,*)          ! wait for william

   15 close(1)
      close(2)
      close(3)
      close(4,status='delete')
  100 format(i2)
  101 format(a25)
  102 format(a80)
  103 format(i3)
  104 format(a132)
  105 format(i10)
  106 format(4i10)
      end
c
      subroutine frmat(form,numpar)
c     Scott Danielsen
c     01/08/88
c     all etc.
c
c     This subroutine will generate a character string
c     'form' based on the demographic information supplied
c     by the user.  Users will no longer be required to
c     supply the output format for demographics.
c     This is limited to integer format strings.
c
      common /frm/upper,miss
      character*132 form
      character*80 amiss,aup,acnt,alen
      integer*4 upper(100), miss(100), tstlen, st, tcol/8/
      character*10 intchar
c
c     get the first one, encode to character and
c     count length of string = number of columns.
c
      amiss=intchar(miss(1),lenmiss)
      aup=intchar(upper(1),lenup)
      tstlen = max0(lenmiss,lenup)         !tstlen=# of cols in last demo
      tcol = tstlen + tcol                 !tcol kps trk of # of cols on a ln.
      ncnt=1                               !ncnt kps trk of # of cnsec matches.
      st=10                                !st kps trk of pstin in form stng.
c
c     go through the rest of the demos, compare new (ntest)
c     w/ last (tstlen) and build form accordingly.
c
      do 11 j=1,numpar
          i=j+1
          if(i .gt. numpar)go to 2
          amiss=intchar(miss(i),lenmiss)
          aup=intchar(upper(i),lenup)
          ntest=max0(lenmiss,lenup)        !ntest=# of cols in current demo.
c
          if(tcol+ntest .gt. 80) go to 2
c
          tcol=tcol+ntest
c
c         keep track of = length columns.
c
          if(ntest .eq. tstlen)then
             ncnt=ncnt+1
             go to 11
          end if
c
c         this is where we actually build the format string.
c         work through an example it is not a diff. algorithm.
c         form = nim where:
c
c                         n = ncnt   |
c                         i = 'i'    |-- e.g. (2I3)
c                         m = tstlen |
c
c
   2      alen=intchar(tstlen,lenlen)
          tstlen=ntest
          if(ncnt .eq. 1)go to 3           !skip if no consec.  matches.
          acnt=intchar(ncnt,lencnt)

          form(st:st+lencnt-1)=acnt(10-lencnt+1:10)
          st=st+lencnt
    3     form(st:st)='i'
          form(st+1:st+lenlen+1)=alen(10-lenlen+1:10)
          st=st+lenlen+1
c
c         if tcol > 80 insert a '/' for new line.
c         (if we go to unformatted data storage,
c          this will not be necessary)
c
          if (tcol + ntest .gt. 80) then
             form(st:st) = '/'
             tcol = ntest
          else
             form(st:st)= ','
          end if
c
          st=st+1
          if(st .gt. 132)then
             write(6,113)
 113         format('  Maximum number of characters execeeded',
     +              ' on format line (132).  Please reconsider.')
          endif
          ncnt=1
c
   11 continue
c
c     all done.  Replace last character of form w/ ')'.
c
      lenfrm=index(form,' ')-1
      form(lenfrm:lenfrm)=')'
      return
      end
c
      character * 10 function intchar(k,len)
      real*8 i,real
c
c     Scott Danielsen
c     11/10/89
c     all rights reserved
c
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
    1 format(/////////////'      Hello, I''m ',a30,//,
     +'      Please enter ''?'' anytime you need help,'//,
     +'      ...or press ''ENTER'' to continue,',/////)
     
      read(5,226)ans
      if(ans.eq.' ')return
      hfile=prog//'.doc'
c		hfile='/galileo/help/elqm.hlp'
      if(ans.eq.'@')hfile='/galileo/help/parrot.fun'
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

      ifile='galileo/help/'//hfile

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