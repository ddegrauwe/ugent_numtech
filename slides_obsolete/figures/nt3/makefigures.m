close all

if 0
	figure('papersize',[4 3],'paperposition',[0 0 4 3])
	x=linspace(0,pi,101);
	h=plot(x,x,x,sin(x),x,4/3*sin(x)-1/6*sin(2*x));
	set(h,'linewidth',4)
	xlim([0 pi]),ylim([0,pi])
	set(gca,'xtick',(0:4)*pi/4,'xticklabel',{'0','$\\pi/4$','$\\pi/2$','$3\\pi/4$','$\\pi$'},'ytick',(0:2)*pi/2,'yticklabel',{'0','$\\pi/2$','$\\pi$'})
	xlabel('$k\Delta x$')
	ylabel('$\omega/c$')
	legend('Analytic','2nd order','4th order')
	print('-depslatex','scalfreq.tex')
	close
end

if 0
	figure('papersize',[4 3],'paperposition',[0 0 4 3])
	x=linspace(0,pi,101);
	h=plot(x,x.^0,x,sin(x)./x,x,(4/3*sin(x)-1/6*sin(2*x))./x);
	set(h,'linewidth',4)
	xlim([0 pi]),ylim([0,1.1])
	set(gca,'xtick',(0:4)*pi/4,'xticklabel',{'0','$\\pi/4$','$\\pi/2$','$3\\pi/4$','$\\pi$'},'ytick',(0:1),'yticklabel',{'0','$c$'})
	xlabel('$k\Delta x$')
	ylabel('$\omega/k$')
	legend('Analytic','2nd order','4th order')
	print('-depslatex','phasespeed.tex')
	close
end

if 0
	figure('papersize',[4 3],'paperposition',[0 0 4 3])
	x=linspace(0,pi,101);
	h=plot(x,x.^0,x,cos(x),x,4/3*cos(x)-1/3*cos(2*x));
	set(h,'linewidth',4)
	xlim([0 pi]),ylim([-2,1.1])
	set(gca,'xtick',(0:4)*pi/4,'xticklabel',{'0','$\\pi/4$','$\\pi/2$','$3\\pi/4$','$\\pi$'},'ytick',(-2:1),'yticklabel',{'$-2c$','$-c$','0','$c$'})
	xlabel('$k\Delta x$')
	ylabel('$\partial\omega/\partial k$')
	legend('Analytic','2nd order','4th order')
	print('-depslatex','groupvelocity.tex')
	close
end

if 1
	figure('papersize',[4 2.5],'paperposition',[0 0 4 2.5])
	x=linspace(0,4*pi,101);
	plot(x,cos(x),'linewidth',4,'color','b','linestyle','-');
	line(x,cos(x-1),'linewidth',4,'color','r','linestyle','--');
	xx=(0:4)*pi;
	line(xx,cos(xx),'linestyle','none','marker','o','color','k','markersize',6)
	line(xx,cos(xx-1),'linestyle','none','marker','s','color','k','markersize',6)
	xlim([0 4*pi+.001]),ylim([-1.2,1.2])
	set(gca,'xtick',(0:4)*pi,'xticklabel',{'0','$\\Delta x$','$2\\Delta x$','$3\\Delta x$','$4\\Delta x$'},'ytick',[])
	xlabel('$x$')
	ylabel('$\cos\left(\frac{\pi}{\Delta x}x\right)$')
	print('-depslatex','-dashed','phase2dx_1.tex')
	close

	figure('papersize',[4 2.5],'paperposition',[0 0 4 2.5])
	x=linspace(0,4*pi,101);
	plot(x,cos(x),'linewidth',4,'color','b','linestyle','-');
	line(x,cos(x(1)-1)*cos(x),'linewidth',4,'color','r','linestyle','--');
	xx=(0:4)*pi;
	line(xx,cos(xx),'linestyle','none','marker','o','color','k','markersize',6)
	line(xx,cos(xx-1),'linestyle','none','marker','s','color','k','markersize',6)
	xlim([0 4*pi+.001]),ylim([-1.2,1.2])
	set(gca,'xtick',(0:4)*pi,'xticklabel',{'0','$\\Delta x$','$2\\Delta x$','$3\\Delta x$','$4\\Delta x$'},'ytick',[])
	xlabel('$x$')
	ylabel('$\cos\left(\frac{\pi}{\Delta x}x\right)$')
	print('-depslatex','-dashed','phase2dx_2.tex')
	close
end

if 0
	% spike test
	c=1;
	dx=1;
	dt=1;
	nt=5;
	nx=32;
	
	x=(0:nx-1)*dx;
	k=[0:nx/2-1,-nx/2:-1]*2*pi/(nx*dx);
	%k=[0:(nx-1)/2,-(nx-1)/2:-1]*2*pi/(nx*dx);
	y0=zeros(1,nx);y0(15)=1;
	Y0=fft(y0);
	% exact
	we=c*k;
	ye=real(ifft(Y0.*exp(-i*we*nt*dt)));
	% 1s
	w=c/(i*dx)*(1-exp(-i*k*dx));
	y1s=real(ifft(Y0.*exp(-i*w*nt*dt)));
	% 2c
	w=c*sin(k*dx)/dx;
	y2c=real(ifft(Y0.*exp(-i*w*nt*dt)));
	% 3s
	w=c/dx*(4/3*sin(k*dx)-1/6*sin(2*k*dx)-i/3*(1-cos(k*dx)).^2);
	y3s=real(ifft(Y0.*exp(-i*w*nt*dt)));
	% 4c
	w=c*(4/3*sin(k*dx)-1/6*sin(2*k*dx))/dx;
	y4c=real(ifft(Y0.*exp(-i*w*nt*dt)));
	
	% figure for centered schemes
	figure('papersize',[4 2.5],'paperposition',[0 0 4 2.5])
	h=plot(x,ye,x,y2c,x,y4c);
	set(h,'linewidth',4);
	xlim(x([1,end]))
	set(gca,'xtick',x,'xticklabel',{''},'ytick',[])
	legend('exact','2-c','4-c')
	print('-depslatex','spike_c.tex')
	close

	% figure for decentered schemes
	figure('papersize',[4 2.5],'paperposition',[0 0 4 2.5])
	h=plot(x,ye,x,y1s,x,y3s);
	set(h,'linewidth',4);
	xlim(x([1,end]))
	set(gca,'xtick',x,'xticklabel',{''},'ytick',[])
	legend('exact','1-s','3-s')
	print('-depslatex','spike_s.tex')
	close
	
end

if 0
	% spike test
	c=1;
	dx=1;
	dt=1;
	nt=5;
	nx=30;
	
	x=(0:nx-1)*dx;
	k=[0:nx/2-1,-nx/2:-1]*2*pi/(nx*dx);
	%k=[0:(nx-1)/2,-(nx-1)/2:-1]*2*pi/(nx*dx);
	y0=cos(2*pi*x/(10*dx))+sin(2*pi*x/(7.5*dx));
	Y0=fft(y0);
	% exact
	we=c*k;
	ye=real(ifft(Y0.*exp(-i*we*nt*dt)));
	% 1s
	w=c/(i*dx)*(1-exp(-i*k*dx));
	y1s=real(ifft(Y0.*exp(-i*w*nt*dt)));
	% 2c
	w=c*sin(k*dx)/dx;
	y2c=real(ifft(Y0.*exp(-i*w*nt*dt)));
	% 3s
	w=c/dx*(4/3*sin(k*dx)-1/6*sin(2*k*dx)-i/3*(1-cos(k*dx)).^2);
	y3s=real(ifft(Y0.*exp(-i*w*nt*dt)));
	% 4c
	w=c*(4/3*sin(k*dx)-1/6*sin(2*k*dx))/dx;
	y4c=real(ifft(Y0.*exp(-i*w*nt*dt)));
	
	% figure for centered schemes
	figure('papersize',[4 2.5],'paperposition',[0 0 4 2.5])
	h=plot(x,ye,x,y1s,x,y2c,x,y3s,x,y4c);
	set(h,'linewidth',4);
	xlim(x([1,end]))
	set(gca,'xtick',x,'xticklabel',{''},'ytick',[])
	legend('exact','1-s','2-c','3-s','4-c')
	print('-depslatex','two_wave.tex')
	close
	
end
