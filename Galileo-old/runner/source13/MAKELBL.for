	subroutine makelbl (labels,nlbl,afile)
c
c
c	a program to read an Oresme formatted data file and produce
c	a properly formatted labels file
c	Copyright 2000 All Rights Reserved Joe Woelfel
c
	use msflib

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
   11 print*,' That''s it, Dudes and Dudettes. Pau!'
c	print*, 'nlbl= ',nlbl
	close (15)
	return
	end

