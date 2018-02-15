 a=load('fort.1441');
npoints=5;
x=a(1,1:npoints);
dn=a(:,npoints+2:2*npoints+1);
he=a(:,2*npoints+2:3*npoints+1);
xi=a(:,3*npoints+2:4*npoints+1);
cn=a(:,4*npoints+2:5*npoints+1);
ce=a(:,5*npoints+2:6*npoints+1);
ci=a(:,6*npoints+2:7*npoints+1);
sn=a(:,7*npoints+2:8*npoints+1);
se=a(:,8*npoints+2:9*npoints+1);
si=a(:,9*npoints+2:10*npoints+1);
ne=a(:,10*npoints+2:11*npoints+1);
te=a(:,11*npoints+2:12*npoints+1);
ti=a(:,12*npoints+2:13*npoints+1);
qn=a(:,13*npoints+2:14*npoints+1);
qe=a(:,14*npoints+2:15*npoints+1);
qi=a(:,15*npoints+2:16*npoints+1);
gn=a(:,16*npoints+2:17*npoints+1);
ge=a(:,17*npoints+2:18*npoints+1);
gi=a(:,18*npoints+2:19*npoints+1);


%        write(1321,'(911E25.11)') %         num%etol,toleq,trapz((peaux+piaux)*dV),trapz((pidt+pedt)*dV),& 
%								& tepr(1),nepr(1),neb,teb,tipr(1),&
%								& trapz((powe+powi)*dV),trapz((prad)*dV),cxe,che,psep/plh


b=load('fort.1321');
