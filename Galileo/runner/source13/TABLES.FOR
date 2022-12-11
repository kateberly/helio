      program galtab
c made into Mac os 3/16/13 jw Good Lord, you're almost 30 years old and still the best there is!      
c
c ** version 2.0 2/2/88  **
c
c program TABLES: this program reads raw pair comparison data
c in standard galileo format,computes the means and
c standard errors, and writes the means and z-scores onto user selected
c files.
c
c Joseph Woelfel
c copyright 4/26/83
c all rights reserved
c
c srd 6-30-86 added array num to index concept # of object
c 1 to n.
C
c Joe Woelfel 9-1-91 added help, standardized front-end.
c
c Scott Danielsen 10-22-91 fixed zpf3 to provide zs for all object
c                          combinations.  Cleaned up interface(===>).
c                          Included output for zpf in first loop.

      dimension ii(8),jj(8),dist(8),sdist(40,40),ssdist(40,40),
     +labels(40),xbar(40,40),n(40,40),num(25)
c
c
c rob zimmelman january 1985 dimension iflag as an
c array of flags which will be set to zero if a concept
c is not a object and 1 if a concept is to be treated as
c a object
c
c
      dimension iflag(40)
      real n
      integer fink
      character*8 iden
      character*80 nseg,ifile,ifile2,hfile,pname
      character*20 labels
      character*1 noy, mark
      data fink /0/
      data sdist/1600*0.0/,ssdist/1600*0.0/,N/1600*0/
      filter=99999.
      Lfno=6
c

        pname='tables.'
        hfile='galileo/help/tables.doc'
        call what(pname,hfile)

  361 Write(6,26)
   26 format(' Where are your labels?')
c      write(*,'(a,\)') ' ====>'
      read(5,360)ifile
  360 format(A80)

      hfile='galileo/help/lbl.hlp'
      call assist(ifile(1:1),hfile,*361)

      open(unit=20,file=ifile,status='old',err=361)
      do 261 i=1,100
      read(20,262,end=500,err=361)labels(i)
  262 format(a20)
  263 format(1x,i2,1x,a20)
  261 continue
  500 ncon=i-1
      close(unit=20)

  504 print*, ' '
      do 700 i=1,ncon
      iflag(i)=0
      write(6,263)i,labels(i)
  700 continue

      write(6,11)
   11 format(' How many of the concepts are objects?')
c      write(*,'(a,\)') ' ====>'
      Read(5,*,err=505)nbrand
      go to 503

  505 hfile='galileo/help/nobj.hlp'
      call assist('?',hfile,*504)

C now put 1's where concepts are objects

  503 do 77 l=1,nbrand
  513 print456,l
  456 format(' Enter the concept # of object #',i3)
c      write(*,'(a,\)') ' ====>'
      read(5,*,err=507)nflag
        go to 506

  507 hfile='galileo/help/objno.hlp'
      call assist('?',hfile,*513)

  506 Iflag(nflag)=1
      num(l)=nflag
   77 continue
  510 write(6,25)
   25 format(' Enter maximum value, por favor.')
c      write(*,'(a,\)') ' ====>'
      Read(5,*,err=509)filter
        go to 511

  509 hfile='galileo/help/maxval.hlp'
      call assist('?',hfile,*510)

  511 write(*,5116)
 5116 format(' Do you want a log transform?')
c      write(*,'(a,\)') ' ====>'
      read(5,226,err=601)noy
 226  format(a1)
c      go to 602

  601 hfile='galileo/help/log.hlp'
      call assist(noy,hfile,*511)

  602 if(noy.eq.'y'.or.noy.eq.'Y')fink=1
  512 write(6,2630)
 2630 format(' Please enter a name for this run.')
c      write(*,'(a,\)') ' ====>'
      Read(5,360)nseg

      hfile='galileo/help/tit.hlp'
      call assist(nseg(1:1),hfile,*512)


  362 Write(6,265)nseg
c      write(*,'(a,\)') ' ====>'
  265 format(' Where are the data for',1x,a57)
      read(5,360)ifile

      hfile='galileo/help/data.hlp'
      call assist(ifile(1:1),hfile,*362)

      open(unit=20,file=ifile,status='old',err=362)

c     read and tally

    1 read(20,2,end=4)iden,(ii(k),jj(k),dist(k),k=1,8) !END=10
    2 format(a8,8(2i2,f5.0))
      If(iden.Eq.'ENDOFSET')go to 4
      do 3 k=1,8
      if(ii(k).Eq.0.Or.Jj(k).Eq.0)Go to 3
      if(ii(k).Le.0.Or.Ii(k).Gt.Ncon)go to 3
      if(jj(k).Le.0.Or.Jj(k).Gt.Ncon)go to 3
      if(dist(k).Eq.-1)Go to 3
c
c      the next line puts the maxval into distance for ed fink-sk
c      if (dist(k).Gt.Filter)dist(k)=filter  ! screw ed fink

      if(dist(k).gt.filter.or.dist(k).lt. 0)go to 3
      I=ii(k)
      j=jj(k)
      if(fink.eq.1)dist(k)=alog(dist(k)+1)
      Sdist(i,j)=sdist(i,j)+dist(k)
      sdist(j,i)=sdist(i,j)
      ssdist(i,j)=ssdist(i,j)+(dist(k)**2)
      ssdist(j,i)=ssdist(i,j)
      n(i,j)=n(i,j)+1
      n(j,i)=n(i,j)

c ** debug **
c      write(6,100)i,j,sdist(i,j),ssdist(i,j),n(i,j)
c  100 format(1x,i2,1x,i2,f10.2,1X,f10.2,1X,i3)
c ** debug **

    3 continue
      go to 1

    4 do 5 i=1,ncon
      do 6 j=1,ncon
      if(i.Eq.J)go to 6
      xbar(i,j)=sdist(i,j)/n(i,j)
      se=abs(ssdist(i,j)-((sdist(i,j)**2)/n(i,j)))
      ssdist(i,j)=(sqrt(se/(n(i,j)-1)))/sqrt(n(i,j)-1)
      if(fink.eq.1)ssdist(i,j)=Exp(ssdist(i,j))-1
      sdist(i,j)=sdist(i,j)/n(i,j)

      if(fink.eq.1)sdist(i,j)=Exp(sdist(i,j))-1

c ** debug **
c      write(6,101)ssdist(i,j)
c  101 format(' ssdist(i,j)= ',f15.2)
C ** debug **

    6 continue
    5 continue
      close(unit=20)

c now write out the means...

  364 write(*,5114)
 5114 format(' Where do you want the means, boss?')
c      write(*,'(a,\)') ' ====>'
      read(5,360)ifile

      hfile='/galileo/help/xout.hlp'
      call assist(ifile(1:1),hfile,*364)


      open(unit=20,file=ifile,status='unknown',err=364)
  365 write(*,5113)
 5113 format(' ...and where do you want the z-scores?')
c      write(*,'(a,\)') ' ====>'
      read(5,360)ifile2

      hfile='galileo/help/zout.hlp'
      call assist(ifile2(1:1),hfile,*365)

      open(unit=21,file=ifile2,status='unknown',err=365)

C r zimmelman 12-14-84 write out tables (ncons) ideal
c jan 13 1984 write out all  distances from each of the objects

      write(20,35)nseg
   35 format(29x,'MEAN GALILEO DISTANCES'//'Run Name: ',a72)

      do 44 nrow=1,ncon
      if(iflag(nrow).Ne.1)Go to 44
      write(20,31)labels(nrow)
   31 format(//'Object: ',a20//,21x,'Attribute',t47,'Distance',
     +4x,'N',/21X,'=========',t47,'========',2x,'====')
      do 32 mcol=1,ncon
      if(iflag(mcol).Eq.1)Go to 32
      write(20,33)labels(mcol),xbar(mcol,nrow),
     +int(n(mcol,nrow))
c     print*,xbar(mcol,nrow)
   33 format(21x,a20,2x,f10.2,2X,i4)
   32 Continue
   44 continue
c
  629 continue

c
      call zpf3(21,xbar,ssdist,ncon,labels,nseg,nbrand,num)
C      go to 1
   10  write(6,12) ifile2,ifile
   12 format( ' z-scores on ',a40/'0means on ' a40,20x,//
     +'  BY YOUR COMMAND...'/' Press "ENTER" to return to Galileo Contro
     +l')
      read(*,*)         ! wait for william)
      end
c__________________________subroutine zpf3________________________

      subroutine zpf3(lfno,xbar,ssdist,ncon,labels,nseg,nbrand,
     +num)
c
c a subroutine to calculate z-scores for pf-2.
C joseph woelfel
c october 29, 1983
c all rights reserved
c
c srd655 june 30 1986.  Added array num to properly format
c title of z score output.
C
      dimension xbar(40,40),ssdist(40,40),labels(40),
     +mark(25,40),num(25),z(25,40)
      character*80 nseg
      character*20 labels
	character*1 mark
c
      data z/1000*0.0/
      Data mark/1000*' '/
      data test/1.282/
C
      write(lfno,4)nseg
    4 format(19x,'STANDARD ERRORS OF DIFFERENCES (Z-SCORES)'///
     +'Run Name: ',a70)
C
      do 2 j=2,nbrand
      do 1 ii = 1,nbrand-1
      if(ii.Ge.J)go to 1
      write(lfno,41)labels(num(ii)),labels(num(j))
c      ii=j-1
      do 3 k=1,ncon
c
      z(ii,k)=(xbar(num(ii),k)-xbar(num(j),k))
     +/sqrt((ssdist(num(ii),k))**2+(ssdist(num(j),k))**2)
      ztest=abs(z(ii,k))
      if(ztest.Gt.Test)mark(ii,k)='S'
      if(ztest.Le.Test)mark(ii,k)=' '
c
      write(lfno,6)labels(k),z(ii,k),mark(ii,k)
    3 continue
    1 continue
    2 continue

   41 format(//'Objects: ',a20,/9x,a20//24x,'Attribute',
     +17x,'Z-Score'/24x,'=========',17x,'=======')
    6 format(24x,a20,5x,f6.2,1X,a1)
      return
      end
c
      subroutine yorn(*,*)
c
c (30 mar 83)   richard a. Holmes, jr.  *All rights reserved*
c provides common "yes or no" questioning.
C y or y returns 1..N or n returns 2 quest from caller. Eof not
c valid.
C
      character*1 ans
    1 read(5,2,end=3) ans
    2 format(a1)
      if(ans.Eq.'Y'.Or.Ans.Eq.'y') Return 1
      if(ans.Eq.'N'.Or.Ans.Eq.'n') Return 2
      print4
    4 format('I need a yes or no answer.')
c      Write(6,100)
c  100 format('$====> ')
      go to 1 
    3 print5
    5 format(' I said "yes or no", mush-for-brains.')
c      write(6,100)
      Go to 1
      end

c______________________________Subroutine Assist____________________

      subroutine assist(ans,hfile,*)
      character*1 ans
      character*80 hfile,htext

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
c   5  print*,' Press ''Enter'' to Continue.'
c      read(5,*)     ! wait for william
    5 close (unit=19)
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

      read(5,8)ans
    8 format(a1)

      if(ans.eq.'?')go to 7
      if(ans.eq.'@')then
      hfile='galileo/help/parrot.fun'
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