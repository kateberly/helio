		program strategy
c **** this program will generate message from 3 dimensional coordinates made by galnet from catac win matrices
c Joe Woelfel
c November 6, 2013
c back in the saddle again
c back where a friend is a friend...
c
c
		Dimension crds(130,3)
		dimension lbl(130)
		dimension vec1(3),vec2(3)
		character*1 yorn
		character*8 form
		character*40 ifile,lbl,ofile,ans,hfile
		hfile='/Applications/catpac/help/strategy.txt'

		write(6,7)		
7        format(//////////////,'Hi, I''m Strategy',//
     +'Be the first kid on your block to rule the world!',//
     +'Enter ? anytime you need help; enter 1 to continue.')
     
     
     	read(5,*)ans
     	if(ans.eq.'?')then
     	call help(hfile)
     	else
     	go to 444
     	endif
		
		
444		print*,' So, you want to rule the world?'
4		print*,' Ok, then you need to tell me where your crds are'
		hfile='/Applications/catpac/help/crdfile.txt'
		read(5,1) ifile
		
		if(ifile.eq.'?')then
		call help(hfile)
		else 
		go to 88
		endif
		go to 4
		
1		format(a40)
c		print*, ' ifile = ', ifile
88		open(unit=10,file=ifile, status='old',err=2)
		read(10, 5) form,ncon
c		print*,'form, ncon=',form,ncon
5        format(a8,3x,i3)
		do 6 i=1,ncon
		read(10,form,err=2) (crds(i,j), j=1,3)
c		print*,' crds=',crds(i,1),crds(i,2),crds(i,3)	
6		continue			
		go to 3
2  	    print*,' sorry world ruler wannabe, no crds here'
		go to 4
3		continue	

30		print*,'Where do you want the results, world ruler wannabe?'
		read(5,1)ofile
c		print*,'ofile=',ofile
		open(unit=9, file=ofile,status='unknown',err=30)
		kount=0

		do 8, i=1,ncon
		read(10,1,end=9)lbl(i)
c		print*,'lbl(i),i=',lbl(i),i
8		continue
9		print*,'What concept were you planning to move?'
		print*,'press enter to see list.'
		read(5,1)ans
c		if(ans.ne.'*')go to 20
		do 21,i=1,ncon
		print*,i,lbl(i)
23		format(i3,2x,a40)
21		continue
20		read(5,24)istart
24		format(i3)
		print*,'and where are you planning to put it, aspiring despot?'
		read(5,24)itarg
		write(9,34),lbl(istart),lbl(itarg)
34		format('start concept is ',a40,/'target concept is ',a40)

c			find the start to target distance	
	
			do 13,i=1,3
			vec1=crds(istart,i)
			vec2=crds(itarg,i)
13			continue
		
			call dist(vec1,vec2,d)

35     print*,'Want to move closer (1) or further (2) from the target?'
		read(5,*)if
		if(if.eq.1)write(9,38)
38		format('Moving closer to target...')
		if(if.eq.2)write(9,381)
381		format('Moving further from target...')
		if(if.ne.1.and.if.ne.2)go to 35
		print*,'What percentage improvement are you willing to accept?'
		read(5,*)pct
		write(9,36)pct
36		format('Percent improvement requested= ',f5.2)
			write(9,33)d,lbl(itarg)
			targdist=d
33		format('st to tgt dist is ',f7.2,//,40x,'Distance from ',a40)	

		if(if.eq.1)crit=(1.0-(pct/100))*targdist
c		print*,'from closer, crit=',crit
		if(if.eq.2)crit=((pct/100)*targdist)+targdist
c 		print*,'from further, crit=',crit
c		calculate the single concept messages

c		print*,'calculating one concept mess'  ! ** debug **
		write(9,433)
433		format(' One-concept messages'///)

c			vec2=crds(itarg,i)
			do 10, i=1,ncon
			if(i.eq.istart.or.i.eq.itarg)go to 10  ! ** trying to skip **
			kount=kount+1
			do 11, j=1,3
			vec1=crds(i,j)
11			continue

c			if(i.eq.itarg)go to 10
			call dist(vec1,vec2,d)
			if(if.eq.2)go to 37
			if(d.gt.crit)go to 10
			go to 39			
37			if(d.lt.crit)go to 10
39			write(9,31)lbl(i),d
31			format(a40,2x,f7.2)
10			continue

c			print*,' about to start 2 pair mess'  ! ** debug **

c		calculate the two concept messages

			write(9,43)
43			format(///,'Now for the two-concept messages')
			do 40,i=1,ncon
			do 40 j=2,ncon
			if(i.eq.j)go to 40 ! eliminate repeats
			if(i.eq.istart.or.i.eq.itarg)go to 40  ! ** tryng to skip **			
			if(j.eq.istart.or.j.eq.itarg)go to 40   ! ** trying to skip **
			kount=kount+1
			do 41 k=1,3
			vec1(k)=(crds(i,k)+crds(j,k))/2
41 			continue

			call dist(vec1,vec2,d)
			
			if(if.eq.2)go to 370
			if(d.gt.crit)go to 40
			go to 390			
370			if(d.lt.crit)go to 40

390			write(9,42)lbl(i),lbl(j),d
42	 		format(//,a40/a40,2x,f7.2)
40			continue


c		calculate the three concept messages
		
		write(9,44)
44		format(///,'Now for the three-concept messages')
		do 45 i=1,ncon
		do 45 j=2,ncon
		do 45 k=3,ncon
		if(i.eq.istart.or.i.eq.itarg)go to 45 ! ** trying to skip **
		if(j.eq.istart.or.j.eq.itarg)go to 45 ! ** trying to skip **		
		if(k.eq.istart.or.k.eq.itarg)go to 45 ! ** trying to skip **
		if(i.eq.j.or.i.eq.k.or.j.eq.k)go to 45! ** skip repeats
		kount=kount+1
		do 46 kk=1,3
		vec1(kk)=(crds(i,kk)+crds(j,kk)+crds(k,kk))/3
46 		continue

		call dist(vec1,vec2,d)
		
		
		
			if(if.eq.2)go to 49
			if(d.gt.crit)go to 45
			go to 48		
49			if(d.lt.crit)go to 45



48		write(9,47)lbl(i),lbl(j),lbl(k),d
47		format(//,A40/A40/A40,2X,F7.2)
45 		continue

			write(9,50)kount
50			format(i10,' messages checked',///,'        That''s All, Folks!')


		print*,' That''s it. Good luck with the world conquest thing!'
		 stop
		 end
		 
		 subroutine dist(vec1,vec2,d)
		 dimension vec1(3),vec2(3)
		 do 1, i=1,3
		 temp=((vec1(i)-vec2(i))**2)
1		 continue		 
		 d=sqrt(temp)	
		 return
		 end
		 
c===================================================Help================================
		       subroutine help(hfile)
      character*80 text
      character*40 hfile
      
      open(unit=19,file=hfile, status='old',err=2)
      do 1, i=1, 1000
      read(19,4,end=5)text
      write(6,4)text
1		continue
4 		format(a80) 
3		go to 5
2		print*,' Sorry, the help file is missing -- you''re on your own.'
5		close(19)
		return
		end
		     
			