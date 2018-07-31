!%newton scheme to produce jacobian and inverse jacobiam
	if (jipper.ge.jipperdo.or.jiter.eq.1) then

	do j2=1,num%nchannels
	do j1=1,num%nxt
		gytemp=gy0
		ytemp=y0
!%%%%%produce transport , takes gy0 in input this gives chat, c, in output, also F
	include 'produce_transport.inc'
	F1vec=reshape(F,(/num%nxt*num%nchannels/))

!%%%%produce transport , takes gy0 in input this gives chat, c, in output, also 	F
	gy0(j1,j2)=gy0(j1,j2)+num%dgy
	y0=ytemp
	include 'produce_transport.inc'
	F2vec=reshape(F,(/num%nxt*num%nchannels/))
	jacob(:,(j2-1)*num%nxt+j1)=(F2vec-F1vec)/num%dgy
	y0=ytemp
	gy0=gytemp
	enddo
	enddo


!	ijacob=inv(jacob)

	call migs(jacob,nchannels*nxt,ijacob)

	jipper = 1



endif
