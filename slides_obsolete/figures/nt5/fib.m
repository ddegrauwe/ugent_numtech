% fibrillation with different schemes

clear all
close all

% parameters
K0=10;
P=2;

% time settings
dt=1;
nt=100;

% loop over different degrees of implicitness
gammar=[.5,1,1.5];
for ii = 1:length(gammar)
	
	gamma=gammar(ii);
	
	% solution
	tt=(0:nt)*dt;
	phi=nan(1,nt+1);

	% initial condition
	phi(1)=0.5;

	% time integration with implicit scheme
	for it=1:nt
		K=K0*phi(it)^P;
		S=1+sin(2*pi*tt(it)/20);
		phi(it+1)=((1-dt*K*(1-gamma))*phi(it)+dt*S)/(1+dt*K*gamma);
	end

	plot(tt,phi)
	set(gca,'xlim',tt([1,end]),'ylim',[-2,2],'position',[.15,.15,.75,.75])
	xlabel('t');ylabel('\psi')
	set(gcf,'papersize',[4,3],'paperposition',[0,0,4,3],'paperorientation','landscape')
	title(sprintf('\\gamma = %4.1f',gamma))
	print('-depsc2',sprintf('fib_%c.eps','a'-1+ii))
	close
end
