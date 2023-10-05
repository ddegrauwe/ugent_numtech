% aliasing test with variable speed
%
% phi is a wave with wavelength 4dx
% c   contains a constant and a wave with wavelength 2dx

clear all
close all

% parameters
nx=8;
dx=1;
nt=100;
dt=.5;

% solution
phi=zeros(nt+1,nx);

% grid
xx=(0:nx-1)*dx;

% speed components
c0=.6;
cn=.5;
cc=c0+cn*cos(2*pi*xx/(2*dx));

% initial condition
phi(1,:)=sin(2*pi*xx/(4*dx));

rhs=inline('-dt*cc.*(y([2:end,1])-y([end,1:end-1]))/(2*dx)','y');

% integrate with RK-4
for it=1:nt
	q1=rhs(phi(it,:));
	q2=rhs(phi(it,:)+q1/2);
	q3=rhs(phi(it,:)+q2/2);
	q4=rhs(phi(it,:)+q3);
	phi(it+1,:)=phi(it,:)+(q1+2*q2+2*q3+q4)/6;
	plot(xx,phi(it+1,:))
	set(gca,'ylim',[-1.5,1.5]);
	pause(.1)
end
