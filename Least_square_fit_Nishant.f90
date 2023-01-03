Program Least_square_fit

!to find best fit line to set of data for 06 january 

implicit none 
 integer ,parameter::n=200
Real ::pressure(n),height(n),temp1(n),dew(n),rhumid1(n) ,a1,b1,slope1,intercept1,temp(n),rhumid(n)

Integer:: i,count,p

 Open (50,file='Radiosonde_06jan.dat')

  Open (200,file='least square fit Nishant.dat')
   

   55 format ( 5x,F5.3,10x,f5.3)
    6 format (5x,I3, 8x, F7.3, 5x ,f5.2	)


Do i=1,6
  Read(50,*) 
 
  end do


! 06 jan : 00Z

 count=0
 do i=6,62
 count=count+1
   read (50,*)pressure(i),height(i),temp1(i),dew(i),rhumid1(i)
   write(*,*)pressure(i),height(i),temp1(i),dew(i),rhumid1(i)

	temp(count)=temp1(i)
	rhumid(count)=rhumid1(i)
	
 end do

	call least_fit(temp,rhumid,count,slope1,intercept1,a1,b1)
	
	
	 write (200,*)' Least fit results for 06 january at time 00GMT'
   write(200,*)'-------------------------------------'

   write (200,*) 'no.of variables   slope   intercept'
   write (200,6) count,slope1, intercept1   

   write(200,*)'-------------------------------------'
   write(200,*)'error in slope   error in intercept'
   write(200,55)a1,b1

   slope1=0
   intercept1=0
   a1=0
   b1=0
   do i=1,68
   read(50,*)
   end do
   p=0
   do i=69,122
	 p=p+1
   read (50,*)pressure(i),height(i),temp1(i),dew(i),rhumid1(i)
	temp(p)=temp1(i)
	rhumid(p)=rhumid1(i)

  end do

	call least_fit(temp,rhumid,p,slope1,intercept1,a1,b1)

	 write(200,*)'   '
	 write(200,*)'   '

	 write (200,*)' Least fit results for 06 january at time 12GMT'
   write(200,*)'-------------------------------------'

   write (200,*) 'no.of variables   slope   intercept'
   write (200,6) p,slope1, intercept1   

   write(200,*)'-------------------------------------'
   write(200,*)'error in slope   error in intercept'
   write(200,55)a1,b1

  contains 
  subroutine least_fit (temp1,rhumid1,n,slope,intercept,a_error,b_error)
  integer ::i,n

  real::a_error,b_error,sum_temp,sum_humid,sum_t_square,sum_temp_rh,slope,intercept,xy,x_y,x2,x_2,line,S ,temp1(n),rhumid1(n)
	
  
 sum_temp=0.0
  sum_humid=0.0
  sum_t_square=0.0
  sum_temp_rh=0.0

 do i=1,n


  sum_temp=sum_temp+ temp1 (i)

  sum_humid=sum_humid+ rhumid1(i)

  sum_t_square=sum_t_square+ (temp1(i)**2)

  sum_temp_rh=sum_temp_Rh+ (temp1(i)*rhumid1(i))


   end do
 
 slope=0.0
 intercept=0.0
 a_error=0.0
 b_error=0.0

 xy= count*sum_temp_rh

 x_y= sum_temp*sum_humid

 x2=count*sum_t_square

 x_2=sum_temp**2

   slope= (xy-x_y)/(x2-	x_2)
   
   intercept=((sum_t_square*sum_humid)-(sum_temp*sum_temp_rh))/(x2-x_2)


  
 
 line=0.0
 Do i=1,n
   
   line=line+((rhumid(i)-(slope*temp(i))-intercept)**2)

end do
 S=sqrt(line/(count-2))

 a_error= S* sqrt( count/(x2-x_2))
 b_error= S* sqrt(sum_t_square/(x2-x_2))

   
  

  
  end subroutine

 end program Least_square_fit