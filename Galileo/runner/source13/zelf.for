		Program zelf
c   program to switch personal references to target
c
c  joe woelfel
c	november 
c
c
		dimension word(80)
		character word
		character*40 ifile,ofile
3		print*,' Zo, where are your words, wordmeister?'
		read(5,4) ifile
4 		format(a40)
		print*,'ifile=',ifile
		
		open(unit=8,status='old',file=ifile, err=3)

		print*,' and where do you want me to put them?'
		read(5,4)ofile
		open(unit=9,status='unknown', file=ofile)
		print*,'ofile=',ofile
	
		read(8,*,end=5)(word(i),i=1,80)

		if(word(i).eq.'i')word(i)='target'
5		j=i
		write(6,*) (word(i),i=1,j)
		write(9,*) (word(i),i=1,j)
2		print*,' yikes!'
		stop
		end	
	