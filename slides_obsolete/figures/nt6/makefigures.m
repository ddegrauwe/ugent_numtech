close all

if 0
	% semi-Lagrangian
	figure('papersize',[4 4],'paperposition',[0 0 4 4])
	axes('position',[.1 .1 .8 .8])
	[x,y]=meshgrid(linspace(0,1,5));
	x=x(:);
	y=y(:);
	u=2+cos(2*x)-cos(2*y);
	v=1+sin(2*y)+sin(2*x);
	
	s=.05;
	plot(x,y,'linestyle','none','color','k','marker','o','markersize',6,'markerfacecolor','k','linewidth',5)
	xlim([-.1,1.1])
	ylim([-.1,1.1])

	for i=1:length(x)
		arrow([x(i)-.0*s*u(i),y(i)-.0*s*u(i)],[x(i)-s*u(i) ,y(i)-s*v(i)],'length',6,'ends','begin','facecolor','b','edgecolor','b')
	end
	
	axis off
	
	print('-deps','-solid','-color','semiLagrangian.eps')
	close
end

