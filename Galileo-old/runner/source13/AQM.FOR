      PROGRAM AQM
c                 AQM MAIN
c
c***********************************************************
c************Copyright Reserved 1980 Richard A. Holmes, Jr.*
c************************************Bldg. 4 Apt. 302   GPA*
c******************************Guilerland Cntr., NY 12085 *
c************************************518-861-8070***********
c***********************************************************
c
c (24 MAR 84) UPDATED TO 1) BE A PROCESSOR AND 2) BE AN FTN PROG.
c
c  02-27-90  Scott Danielsen,  updated aqm and made dos compatible.
c            Added yorn & datwrt, removed rdcons & feol.
c
C		OSX VERSION MARCH 24, 2013 JW

      implicit integer (a-z)
      dimension lens(50),row(1225),col(1225)
      character*51 cons(50)
      character*68 crit/' '/
      character*80 ifile,lfile

c
      call intro('AQM','v2.00')

1111  write(6,111)
 111  format (' Where do you want your questionnaire, Boss?')
c       write(*,'(a,\)') ' ====>'
      read(5,360)ifile
      
      go to 1001    ! a wuick kludge to get a quairre done by classtime   fix me!!!!!
      
      call assist(ifile(1:1),'aq_out.hlp',*1111)
      open(unit=10,file=ifile,status='unknown',err=1111)


 1001 write(6,20)
   20 format(' Please type in your criterion pair')
c      write(*,'(a,\)') ' ====>'
      read(5,21)crit
      go to 1002  ! another nasty kludge! ******
      call assist(crit(1:1),'aq_crit.hlp',*1001)
   21 format(a68)


 1002  write(6,17)
   17 format('  Do you want the standard instructions?')
      call yorn(*7,*51)
      call assist('?','aq_inst.hlp',*1002)
    7 write(10,22)crit
   22 format(t30,'Instructions',//,3x,'Please estimate how different',
     +' or "far apart" each of the following '/'  words or phrases is',
     +' from each of the others.  The more different,',/,'  or further',
     +' apart they seem to be, the larger the number you should ',/
     +,2x,'write. To help you know what size number to write, remember',
     +/,5X,A68,/,
     +'   If two words or phrases are not different at all, please',
     +' write '/3x,'zero (0).  If you have no idea, just leave the spa',
     +'ce blank.',//,'       Thank you very much for your help.'/)
c

   51 write(*,216)
  216 format(' Where are your labels?')
c      write(*,'(a,\)') ' ====>'
      read(*,360)lfile
  360 format(A80)
c      call assist(lfile(1:1),'aq_lfile.hlp',*51)   ! commented out in kludgeomania ******
      open(unit=20,file=lfile,status='old',err=51)
      do 261 i=1,50
         read(20,262,end=777)cons(i)
         call feol(cons(i),lens(i))
  261 continue
  777 nc=i-1
      close(unit=20)
  262 format(a51)
c
c  INITIALIZE ROW AND COL NUMBERS
   12 K=0
      DO 14 I=1,NC-1
      DO 14 J=I+1,NC
      K=K+1
      ROW(K)=I
      COL(K)=J
   14 CONTINUE
c
c BEGIN CALLING BLOCK,  THE REAL WORKER
c
      CDN=1
      PRS=K
      FR=1
      WRITE(10,88)
   88 FORMAT(14x,58(1H-))
   15 TO=MIN0(FR+7,PRS)
      CALL BLOCK(CDN,ROW,COL,FR,TO,CONS,LENS,CRIT)
      FR=TO+1
      CDN=CDN+1
      IF(FR.LE.PRS)GO TO 15
c
c END CALLING BLOCK.
c
 1003  WRITE(6,19)
   19 FORMAT(' Do you want the standard demographics?')
      CALL YORN(*104,*25)
      call assist('?','aq_demo.hlp',*1003)
104   CDN=CDN+1
c      WRITE(10,26) CDN
c   26 FORMAT(1X,'Duplicate 1-6'/1x,'Card ',I3,3X,'7-8')
      WRITE(10,27)
   27 FORMAT(1X,//'What is your age?_____   (cols. 8-9)',
     +/1x,'What is your sex?_____   (col. 10, male=0, 1=female)',
     +/1x,'What is your occupation?__________________________',
     +/1x,'                        (cols. 11-13)_____',
     +/1x,'What is your highest level of education?____________________',
     +/1x,'                        (cols. 14-15)_____')
c
c
   25 WRITE(6,100)
  100 FORMAT(' Do you want the standard telephone survey questions?')
      call yorn(*105,*101)
      call assist('?','aq_tele.hlp',*25)
105   CDN=CDN+1
c      WRITE(10,26) CDN
      WRITE(10,102)
  102 FORMAT(1x,'Administrator:  Please write the phone number',
     +/T18, 'called   (______)-_____-___________   (cols. 9-18)')
      WRITE(10,103)
  103 FORMAT(17X,'The date in YYMMDD format  ___________  (cols. 19-24)
     +  '/,17X,'The time in  HHMM  format  ___________  (cols. 25-28)'/
     +      17X,'The time for administration in minutes _______  (cols.'
     +,'29-30)')
c
  101 write(6,16)ifile
   16 format(' Your questionaire is on ',a40/
     +,'...by your command.       ')
      close(10)
      end
c
      subroutine block(cdn,row,col,fr,to,cons,lens,crit)
c
c THE WORKHORSE FOR THE AQM
c EACH TIME CALLED, BLOCK PRODUCES AN 8 OR LESS BLOCK
c QAIRE.  ONE CARD, ACTUALLY
c
      IMPLICIT INTEGER (A-Z)
      DIMENSION LENS(50),ROW(1225),COL(1225)
      CHARACTER*51 T1,T2,CONS(50),crit*68
      DIMENSION SCOL(8),FCOL(8)
      DATA (SCOL(I),FCOL(I),I=1,8)/9,17,18,26,27,35,36,44,45,53,54,62,
     +63,71,72,80/
C
C PRODUCE BLOCK HEADER
      IF(CRIT.NE.' ') THEN
      WRITE(10,1)CRIT
      ELSE
      WRITE(10,1)CRIT
      ENDIF
C    1 FORMAT(1X,'Duplicate 1-6'/1x,'Card ',I3,2X,'7-8 ',A9,1X,A51/
C     +T9,'COL.  How Far Apart Are',T68,'Units',T15,58(1H-))
    1 FORMAT(14x,1X,A54/8x,'COL.',2X,58(1H-))
C
C PRODUCE BODY OF BLOCK
      K=0
      DO 2 PR=FR,TO
      R=ROW(PR)
      C=COL(PR)
      K=K+1
      T1(1:LENS(R))=CONS(R)(1:LENS(R))
      T1(LENS(R)+1:51)=' '
      T2(1:LENS(C))=CONS(C)(1:LENS(C))
      T2(LENS(C)+1:51)=' '
C
      IF(LENS(R).LE.22.AND.LENS(C).LE.22)GO TO 3
C
C TWO LINE FORMAT
      WRITE(10,6) R,C,SCOL(K),FCOL(K),T1,T2
    6 FORMAT(1X,2I2.2,2X,I2,'-',I2,2X,A51/T10,'and',2X,A51,2X,'_____')
      GO TO 2
C
C ONE LINE FORMAT
    3 WRITE(10,7) R,C,SCOL(K),FCOL(K),T1(1:22),T2(1:22)
    7 FORMAT(1X,2I2.2,2X,I2,'-',I2,2X,A22,2X,'and',2X,A22,2X,'_____')
    2 CONTINUE
C
C PRODUCE BLOCK FOOTER
      WRITE(10,8)
    8 FORMAT(14x,58(1H-))
      RETURN
      END
C
      subroutine feol(word,len)
c
c     will find end of line by ichar = 32 (sp)
c     not fool proof but good enough.
c
      character*51 word
      do 10 i = 51,1,-1
      if(ichar(word(i:i)).ne.32)then
        len = i
        return
      endif
  10  continue
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

      integer  hour,minute,second,hund,year,month,day
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
 100  format(//////////5x,a10,5x,a5,
     +15x,i2.2,'/'i2.2,'/',i2.2,6x,i2.2,':'i2.2,':',i2.2)

c     for everybody!

      write(6,1)prog
    1 format(////////////'      Hello, I''m ',a30,//,
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

c      if(ans.eq.'?'.or.ans.eq.'@')go to 7
c      return

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