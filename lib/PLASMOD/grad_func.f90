!these are all numerical functions, I am not going to comment on these, accept them.
module grad_func
 implicit none
contains

    function gradient(x, y)
    
        ! Arguments 
	
	real(kind(1.0D0)), intent(in) :: x(:), y(:)
	real(kind(1.0D0)), dimension(size(x)) :: gradient

        !  Local variables
	
	integer :: i, npts
        real(kind(1.0D0)) :: dx, dy
	real(kind(1.0d0)), dimension(size(x)) :: r
	
	npts = size(x)
	dy = y(2)-y(1)
	r(1) = (x(2)-x(1))/dy
	dy = y(npts)-y(npts-1)
	r(npts) = (x(npts)-x(npts-1))/dy
	
	do i = 2, npts-1
	  dx = (x(i+1) - x(i-1))/2
	  dy = (y(i+1) - y(i-1))/2
	  r(i) = dx/dy
	end do
	
	gradient = r
	
    end function gradient

	function integrcc(nx,x,y)

	implicit none
	integer i,nx
	double precision x(nx),y(nx),drho,y1tmp
	double precision sy(nx),integrcc(nx)

	! not used
	! integer j

	sy=0.
	do i=2,nx
		drho=(x(i)-x(i-1))
		y1tmp=(y(i)+y(i-1))/2.
	 sy(i)=sy(i-1)+y1tmp*drho
	enddo

	integrcc=sy

	end function integrcc


	function derivcc(nx,x,y,gga)

	implicit none
	integer i,nx,gga
	double precision x(nx),y(nx),dy(nx),derivcc(nx)
	double precision P(3)

	! not used
	! integer j
	
	dy=0.
	do i=2,nx-1
		dy(i)=(y(i+1)-y(i-1))/(x(i+1)-x(i-1))
	enddo
	if (gga.eq.1)	dy(1)=0.
	if (gga.eq.2) dy(1)=(y(2)-y(1))/(x(2)-x(1))

	call polyfitcc(x(nx-2:nx),y(nx-2:nx),P)

	dy(nx)=2.*P(1)*x(nx)+P(2)

	derivcc=dy
	end function derivcc

	subroutine polyfitcc(x,y,P)
	implicit none
	double precision x(3),y(3),P(3)
	double precision y21,y32,x21,x32,h21,h32

	y32=y(3)-y(2)				 
	y21=y(2)-y(1)				 
	x32=x(3)-x(2)				 
	x21=x(2)-x(1)				 
	h32=x(3)+x(2)				 
	h21=x(2)+x(1)				 

	P(1)=(x21*y32-x32*y21)/(x21*x32*(h32-h21))
	P(2)=y21/x21-P(1)*h21
	P(3)=y(3)-P(1)*x(3)**2.-P(2)*x(3)


	end subroutine polyfitcc
    
    function gradient1(x)
    
        ! Arguments
       
        real(kind(1.0d0)), intent(in) :: x(:)
        real(kind(1.0d0)), dimension(size(x)) :: gradient1

        !  Local variables
       
        integer :: i, npts
        real(kind(1.0D0)) :: dx
        real(kind(1.0d0)), dimension(size(x)) :: r
       
	npts = size(x)
	do i = 2, npts-1
	  dx = (x(i+1) - x(i-1))/2
	  r(i) = dx
	end do
	
	r(1) = x(2)-x(1)
	r(npts) = x(npts)-x(npts-1)
	
	gradient1 = r
	
    end function gradient1   
    
    function cumint(x, y)
    
        ! Arguments 
	
	real(kind(1.0D0)), intent(in) :: x(:), y(:)
	real(kind(1.0D0)), dimension(size(x)) :: cumint

        !  Local variables
	
	integer :: i, npts
	real(kind(1.0d0)), dimension(size(x)) :: r, dx
	
	npts = size(x)
	dx = gradient1(x)
	r(1) = y(1) * dx(1)
	do i = 2, npts
	  r(i) = y(i)*dx(i) + r(i-1)
	end do
	
	cumint = r
	
    end function cumint
!C----------------------------------------------------------------------|
		function linterp_ef(x1,y1,Nx1, &
     &   x2,Nx2)
!Cend call
!C
!C----------------------------------------------------------------------|
!		 use parameters_cpef2a
	implicit none

	integer Nx1,Nx2,i,j,jdone

	double precision x1(Nx1)
	double precision y1(Nx1)
	double precision x2(Nx2)
	double precision y2(Nx2),A,B
	double precision linterp_ef(Nx2)
	double precision z1,z2,z3,z4
	double precision t1,t3,t4

	! not used
	! double precision t2, t5, z5, k


	do i=1,Nx2
		jdone=0
		
		t1=x2(i)

		do j=2,Nx1
		z1=x1(j-1)
		z2=x1(j)
		
	if (t1.eq.z1 .and. jdone.eq.0) then
		y2(i)=y1(j-1)
		jdone=1
	endif

	if (t1.eq.z2 .and. jdone.eq.0) then
		y2(i)=y1(j)
		jdone=1
	endif
	
	if (t1.gt.z1 .and. t1.lt.z2 .and. jdone.eq.0) then
		z3=y1(j-1)
		z4=y1(j)
		t3=x1(j-1)
		t4=x1(j)
		
		A=(z4-z3)/(t4-t3)
		B=z3-t3*A
		y2(i)=A*t1+B

		jdone=1
	endif


	if (t1.lt.z1 .and. j.eq.2 .and. jdone.eq.0) then
		z3=y1(j-1)
		z4=y1(j)
		t3=x1(j-1)**2.0
		t4=x1(j)**2.0
		
		A=(z4-z3)/(t4-t3)
		B=z3-t3*A
		y2(i)=A*t1**2.0+B

		jdone=1
	endif

	if (t1.gt.z2 .and. j.eq.Nx1 .and. jdone.eq.0) then
		z3=y1(j-1)
		z4=y1(j)
		t3=x1(j-1)
		t4=x1(j)
		
		A=(z4-z3)/(t4-t3)
		B=z3-t3*A
		y2(i)=A*t1+B

		jdone=1
	endif

		enddo
	enddo

	linterp_ef=y2


	end  function linterp_ef
!C end of GS SOLVER EF
 
	
	
		function	INTEGR_CDE(x_input,y_input, &
     &    nagrid)
	implicit none
	integer	j,nagrid
	double precision x_input(nagrid)
	double precision y_input(nagrid),ys_output
	double precision y1tmp,drho
	double precision integr_cde

	! Not used
	! doubel precision y0, y2tmp, y3tmp, i, k, order_d, x_type
	! double precision Acoef, Bcoef, Ccoef
	! double precision x1tmp,x2tmp,x3tmp

!C Normalized grid , GRP style
							j=1
							ys_output=0.0
							do j=2,nagrid
          			drho=x_input(j)-x_input(j-1)
								y1tmp=(y_input(j)+y_input(j-1))/2.0
								ys_output=ys_output+y1tmp*drho
							enddo

	integr_cde=ys_output

end function integr_cde	
	
	   
    function cumint1(x)
    
        ! Arguments 
	
	real(kind(1.0D0)), intent(in) :: x(:)
	real(kind(1.0D0)), dimension(size(x)) :: cumint1

        !  Local variables
	
	integer :: i, npts
	real(kind(1.0d0)), dimension(size(x)) :: r
	
	npts = size(x)
	r(1) = x(1)
	do i = 2, npts
	  r(i) = x(i) + r(i-1)
	end do
	
	cumint1 = r
	
    end function cumint1
    
    function trapz(x)
    
        ! Arguments
	
	real(kind(1.0D0)), intent(in) :: x(:)
	real(kind(1.0D0)) :: trapz
	
	! Local variables
	
	integer :: i, npts
	real(kind(1.0d0)) :: dum
	
	npts = size(x)
	dum = 0.0
	do i = 2, npts
	  dum = dum + (x(i)+x(i-1))/2.
	end do
	
	trapz = dum

    end function trapz
    
    function maxvec(x, y)
        
	! Arguments
	
	real(kind(1.0D0)), intent(in) :: x(:), y(:)
	real(kind(1.0d0)), dimension(size(x)) :: maxvec
	
	! Local variables
	
	real(kind(1.0d0)), dimension(size(x)) :: dum
	integer :: i, npts
	
	npts = size(x)
	do i = 1, npts
	  dum(i) = max(x(i), y(i))
	end do
	
	maxvec = dum
	
    end function maxvec
    
    function linspace(x, y, nx)
    
        ! Arguments
	
	real(kind(1.0D0)), intent(in) :: x, y
	integer, intent(in) :: nx
	real(kind(1.0d0)), dimension(nx) :: linspace
	
	! Local variables
	
	real(kind(1.0d0)) :: dx
	real(kind(1.0d0)), dimension(nx) :: dum
	integer :: i
	
	dx = (y - x)/(nx - 1.0d0)
	do i = 1, nx
	  dum(i) = x + (i-1.0d0)*dx
	end do
	
	linspace = dum
    
    end function linspace

    
    function interp1(x, y1, x2)
    
        ! Arguments
	
	real(kind(1.0d0)), intent(in) :: x(:), y1(:), x2(:)
	real(kind(1.0d0)), dimension(size(x2)) :: interp1
	real(kind(1.0d0)), dimension(size(x)) :: x1
	
	! Local variables

	real(kind(1.0d0)), dimension(size(x2)) :: y2
	integer :: i, j,nx1,nx2,jdone
	real(kind(1.0d0)) ::  z1,z2,z3
	real(kind(1.0d0)) ::  t1,t2,t3,t4,A,B,C
	
	! Not used
	! real(kind(1.0d0)) :: dx, dy, dv
	! real(kind(1.0d0)) :: t5, t11, z4, z5, z11

	Nx2=size(x2)
	Nx1=size(x)
	
	x1=x

	do i=1,Nx2
		jdone=0
		
		t4=x2(i)

		do j=2,Nx1-1
		z1=x1(j-1)
		z2=x1(j)
		z3=x1(j+1)
		
	if (t4.eq.z1 .and. jdone.eq.0) then
		y2(i)=y1(j-1)
		jdone=1
	endif

	if (t4.eq.z2 .and. jdone.eq.0) then
		y2(i)=y1(j)
		jdone=1
	endif

	if (t4.eq.z3 .and. jdone.eq.0) then
		y2(i)=y1(j+1)
		jdone=1
	endif
	
	if (t4.gt.z1 .and. t4.lt.z3 .and. jdone.eq.0) then
		t1=y1(j-1)
		t2=y1(j)
		t3=y1(j+1)
		
				A=(t3-t2-(z3-z2)*(t1-t2)/(z1-z2)) &
     &     /((z3-z2)*(z3-z1))
				B=(t1-t2)/(z1-z2) &
     &     -A*(z1+z2)
				C=t2-A*(z2**2.0)-B*z2

		y2(i)=A*(t4**2.0)+B*t4+C

		jdone=1
	endif

	if (t4.lt.z1 .and. jdone.eq.0  &
     &   .and. j.eq.2) then
		z1=x1(j-1)**2.0
		z2=x1(j)**2.0
!		z3=x1(j+1)**2.0
		t1=y1(j-1)
		t2=y1(j)
!		t3=y1(j+1)
		

!				A=(t3-t2-(z3-z2)*(t1-t2)/(z1-z2))
!     &     /((z3-z2)*(z3-z1))
!				B=(t1-t2)/(z1-z2)
!     &     -A*(z1+z2)
!				C=t2-A*(z2**2.0)-B*z2
				A=0.0
				B=(t1-t2)/(z1-z2) &
     &     -A*(z1+z2)
				C=t2-A*(z2**2.0)-B*z2


		y2(i)=A*(t4**4.0)+B*(t4**2.0)+C
!		y2(i)=B*t4**2.0+C
		jdone=1
	endif


	if (t4.gt.z3 .and. jdone.eq.0  &
     &   .and. j.eq.Nx1-1) then

		t1=y1(j-1)
		t2=y1(j)
		t3=y1(j+1)
		

				A=(t3-t2-(z3-z2)*(t1-t2)/(z1-z2)) &
     &     /((z3-z2)*(z3-z1))
				B=(t1-t2)/(z1-z2) &
     &     -A*(z1+z2)
				C=t2-A*(z2**2.0)-B*z2


		y2(i)=A*(t4**2.0)+B*t4+C
		jdone=1
	endif

		enddo
	enddo

	
	interp1 = y2
    
    end function interp1


    function interp1_ef(nx1,nx2,x, y1, x2)
    
        ! Arguments
	
	real(kind(1.0d0)), intent(in) :: x(:), y1(:), x2(:)
	real(kind(1.0d0)), dimension(size(x2)) :: interp1_ef
	real(kind(1.0d0)), dimension(size(x)) :: x1
	
	! Local variables

	real(kind(1.0d0)), dimension(size(x2)) :: y2
	integer :: i, j,nx1,nx2,jdone
	real(kind(1.0d0)) ::  z1,z2,z3
	real(kind(1.0d0)) ::  t1,t2,t3,t4,A,B,C
	! Not used
	! real(kind(1.0d0)) :: dx, dy, dv
	! real(kind(1.0d0)) ::  t5, t11, z11, z4, z5

	Nx2=size(x2)
	Nx1=size(x)
	
	x1=x

	do i=1,Nx2
		jdone=0
		
		t4=x2(i)

	if (t4.gt.x(nx1).and.jdone.eq.0) then
		y2(i)=y1(nx1)
		jdone=1
	endif



		do j=2,Nx1-1
		z1=x1(j-1)
		z2=x1(j)
		z3=x1(j+1)
		
	if (t4.eq.z1 .and. jdone.eq.0) then
		y2(i)=y1(j-1)
		jdone=1
	endif

	if (t4.eq.z2 .and. jdone.eq.0) then
		y2(i)=y1(j)
		jdone=1
	endif

	if (t4.eq.z3 .and. jdone.eq.0) then
		y2(i)=y1(j+1)
		jdone=1
	endif
	
	if (t4.gt.z1 .and. t4.lt.z3 .and. jdone.eq.0) then
		t1=y1(j-1)
		t2=y1(j)
		t3=y1(j+1)
		
				A=(t3-t2-(z3-z2)*(t1-t2)/(z1-z2)) &
     &     /((z3-z2)*(z3-z1))
				B=(t1-t2)/(z1-z2) &
     &     -A*(z1+z2)
				C=t2-A*(z2**2.0)-B*z2

		y2(i)=A*(t4**2.0)+B*t4+C

		jdone=1
	endif

	if (t4.lt.z1 .and. jdone.eq.0  &
     &   .and. j.eq.2) then
		z1=x1(j-1)**2.0
		z2=x1(j)**2.0
!		z3=x1(j+1)**2.0
		t1=y1(j-1)
		t2=y1(j)
!		t3=y1(j+1)
		

!				A=(t3-t2-(z3-z2)*(t1-t2)/(z1-z2))
!     &     /((z3-z2)*(z3-z1))
!				B=(t1-t2)/(z1-z2)
!     &     -A*(z1+z2)
!				C=t2-A*(z2**2.0)-B*z2
				A=0.0
				B=(t1-t2)/(z1-z2) &
     &     -A*(z1+z2)
				C=t2-A*(z2**2.0)-B*z2


		y2(i)=A*(t4**4.0)+B*(t4**2.0)+C
!		y2(i)=B*t4**2.0+C
		jdone=1
	endif


	if (t4.gt.z3 .and. jdone.eq.0  &
     &   .and. j.eq.Nx1-1) then

		t1=y1(j-1)
		t2=y1(j)
		t3=y1(j+1)
		

				A=(t3-t2-(z3-z2)*(t1-t2)/(z1-z2)) &
     &     /((z3-z2)*(z3-z1))
				B=(t1-t2)/(z1-z2) &
     &     -A*(z1+z2)
				C=t2-A*(z2**2.0)-B*z2


		y2(i)=A*(t4**2.0)+B*t4+C
		jdone=1
	endif

		enddo
	enddo

	
	interp1_ef = y2
    
    end function interp1_ef
    
    function eye2(nx)
    
        ! Arguments
	
	integer, intent(in) :: nx
	real(kind(1.0d0)), dimension(nx, nx) :: eye2
	
	! Local variables
	
	integer :: i
	real(kind(1.0d0)), dimension(nx, nx) :: dum
	
	dum = 0.0d0
	do i = 1, nx
	  dum(i, i) = 1.0d0
	end do
	
	eye2 = dum
    
    end function eye2
    
    function sign1(x)
    
        ! Arguments
	
	real(kind(1.0d0)), intent(in) :: x(:)
	real(kind(1.0d0)), dimension(size(x)) :: sign1
	
	! Local variables
	
	integer :: i
	real(kind(1.0d0)), dimension(size(x)) :: dum
	
	dum = x/abs(x)
	do i = 1, size(x)
	  if (x(i) == 0.0d0) then
	    dum(i) = 0.0d0
          end if
	end do
	
	sign1 = dum
    
    end function sign1
        
end module grad_func

