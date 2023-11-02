clear all
close all
rand('state',0)

if 0
	% 2D grid
	N=128;
	k=3;
	xr=(0:N)/N;
	[x,y]=meshgrid(xr);
	
	figure('papersize',[3 3],'paperposition',[0 0 3 3]);
	imagesc(xr,xr,sin(2*pi*k*(x+y)))
	
	set(gca,'xtick',[],'ytick',[],'ydir','normal')
	
	s=50;x0=.02;y0=.02;
	
	line([1.25/k 2.25/k],y0+[0 0],'linewidth',2,'color','k')
	patch(1.25/k-[0 -1 -1]/s,y0+[0 .5 -.5]/s,0,'facecolor','k')
	patch(2.25/k+[0 -1 -1]/s,y0+[0 .5 -.5]/s,0,'facecolor','k')
	text(1.75/k,.05,'$2\Delta s$','horizontalalignment','center')
	
	line(x0+[0 0],[1.25/k 2.25/k],'linewidth',2,'color','k')
	patch(x0+[0 .5 -.5]/s,1.25/k-[0 -1 -1]/s,0,'facecolor','k')
	patch(x0+[0 .5 -.5]/s,2.25/k+[0 -1 -1]/s,0,'facecolor','k')
	text(.05,1.75/k,'$2\Delta s$','horizontalalignment','left')
	
	line([1.125 1.625]/k,[1.125 1.625]/k,'linewidth',2,'color','k')
	patch(1.125/k-[0 -1.5 -.5]/s/sqrt(2),1.125/k-[0 -.5 -1.5]/s/sqrt(2),0,'facecolor','k')
	patch(1.625/k+[0 -1.5 -.5]/s/sqrt(2),1.625/k+[0 -.5 -1.5]/s/sqrt(2),0,'facecolor','k')
	text(1.4/k,1.35/k,'$\sqrt{2}\Delta s$','horizontalalignment','left')
	
	% make colormap a bit lighter
	C=colormap;C=(1-C/2);colormap(C)
	
	
	print('-depslatex','-color','2D.tex');
	%close
end

if 0
	% diffusion flattening peaks
	figure('papersize',[3 6],'paperposition',[0 0 6 3]);
	axes('position',[.1 .1 .8 .8])
	
	x=linspace(0,5,101);
	y=cos(1.5*x)+sin(2.5*x);
	plot(x,y,'linewidth',2)
	i=10; arrow([x(i);y(i)],[x(i);y(i)-.7],'ends','end','length',6);text(x(i)+.1,y(i)-.35,'$\frac{d^2\psi}{dx^2}<0$','horizontalalignment','left')
	i=69; arrow([x(i);y(i)],[x(i);y(i)-.7],'ends','end','length',6);text(x(i)+.1,y(i)-.35,'$\frac{d^2\psi}{dx^2}<0$','horizontalalignment','left')
	i=40; arrow([x(i);y(i)],[x(i);y(i)+.7],'ends','end','length',6);text(x(i)+.1,y(i)+.35,'$\frac{d^2\psi}{dx^2}>0$','horizontalalignment','left')
	i=91; arrow([x(i);y(i)],[x(i);y(i)+.7],'ends','end','length',6);text(x(i)-.1,y(i)+.35,'$\frac{d^2\psi}{dx^2}>0$','horizontalalignment','right')
	set(gca,'xtick',[],'ytick',[])
	%return
	print('-depslatex','-color','diffusion.tex');
	close
end


