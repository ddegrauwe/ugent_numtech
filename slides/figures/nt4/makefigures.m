clear all
close all
rand('state',0)

N0=4;
x0=(0:N0)/N0*2*pi;
y0=rand(1,N0+1);

N=12;
x=(0:2*N)/(2*N+1)*2*pi;
y=spline(x0,[y0(1) y0(2:N0+1) y0(2) y0(1)],x);
y=y-mean(y);

xx=(0:16*2*N)/(16*2*N+1)*2*pi;
xxx=(0:16*2*N+1)/(16*2*N+1)*2*pi;

Y=fft(y)/(2*N+1);
YY=[Y(1:N+1) zeros(1,15*2*N) Y(N+2:end)];

yy=(16*2*N+1)*ifft(YY);
yyy=[yy yy(1)];

if 0
	% decomposition: original function
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	plot(xxx/(2*pi),yyy,'k-','linewidth',4)
	set(gca,'xlim',[0 1],'xtick',[0 .25 .5 .75 1],'xticklabel',{'$0$','$\\pi/2$','$\\pi$','$3\\pi/2$','$2\\pi$'},'ylim',[-.5,.5],'ytick',[])
	print('-depslatex','-dashed','-color','decomp_GP.tex');
	close
end

if 0
	% decomposition: components
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	k=0;
	plot(xxx/(2*pi),real(Y(1)*exp(i*k*xxx)),'k','linewidth',4)
	set(gca,'xlim',[0 1],'xtick',[0 .25 .5 .75 1],'xticklabel',{'$0$','$\\pi/2$','$\\pi$','$3\\pi/2$','$2\\pi$'},'ylim',[-.5,.5],'ytick',[])
	print('-depslatex','-dashed','-color',sprintf('decomp_SP_%i.tex',k));
	close
	for k=1:4
		figure('papersize',[2 3],'paperposition',[0 0 3 2]);
		plot(xxx/(2*pi),real(Y(1+k)*exp(i*k*xxx)+Y(2*N+2-k)*exp(-i*k*xxx)),'k','linewidth',4)
		set(gca,'xlim',[0 1],'xtick',[0 .25 .5 .75 1],'xticklabel',{'$0$','$\\pi/2$','$\\pi$','$3\\pi/2$','$2\\pi$'},'ylim',[-.5,.5],'ytick',[])
		print('-depslatex','-dashed','-color',sprintf('decomp_SP_%i.tex',k));
		close
	end
end

if 0
	% spectrum barplot
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	bar(0:N,abs(Y(1:N+1)))
	set(gca,'xlim',[-.5,12.5],'xtick',[0 2 12],'xticklabel',{'0','2','$K$'},'ytick',[])
	xlabel('$k$','interpreter','none')
	ylabel('$|\alpha_k|$','interpreter','none')
	print('-depslatex','-dashed','-color','spectrum.tex');
	close
end

if 0
	% aliasing (GP)
	figure('papersize',[2 6],'paperposition',[0 0 6 2]);
	k=21;
	t=2*pi*rand*0;
	h=plot(xxx/(2*pi),cos(k*xxx+t),'k',...
		x/(2*pi),cos(k*x+t),'ks','markersize',2);
	set(h,'linewidth',1)
	set(gca,'xlim',[0 1],'xtick',[0 .25 .5 .75 1],'xticklabel',{'$0$','$\\pi/2$','$\\pi$','$3\\pi/2$','$2\\pi$'},'ylim',[-1.5,1.5],'ytick',[])
	print('-depslatex','-solid','-color','alias_1.tex');
	
	line(xxx/(2*pi),cos((2*N+1-k)*xxx+t),'color','b','linewidth',2)
	print('-depslatex','-solid','-color','alias_2.tex');
	
	close
end

if 0
	% aliasing (SP)
	figure('papersize',[2 6],'paperposition',[0 0 6 2]);
	k=linspace(-4.5,4.5,91);
	h=plot(k,exp(-k.^2))
	set(h,'linewidth',2,'color','k')
	set(gca,'xlim',[-4.5 4.5],'xtick',[-3 -1.5 0 1.5 3],'xticklabel',{'$-2K$','$-K$','$0$','$K$','$2K$'},'ylim',[0 1.2],'ytick',[])
	grid on
	xlabel('$k$','interpreter','none')
	print('-depslatex','-solid','-color','alias_3.tex');


	line(k,exp(-(k-3).^2),'color','r','linewidth',2)
	line(k,exp(-(k+3).^2),'color','r','linewidth',2)
	print('-depslatex','-solid','-color','alias_4.tex');

	kk=linspace(-1.5,1.5,31);
	patch([kk fliplr(kk)],[exp(-kk.^2)+exp(-(kk-3).^2)+exp(-(kk+3).^2)+exp(-(kk-6).^2)+exp(-(kk+6).^2) fliplr(exp(-kk.^2))],0,'facecolor','r')
	line(k,exp(-k.^2)+exp(-(k-3).^2)+exp(-(k+3).^2)+exp(-(k-6).^2)+exp(-(k+6).^2),'color','b','linewidth',2);	
	print('-depslatex','-solid','-color','alias_5.tex');
	
	close
end

if 0
	% LAM truncation
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	Nx=20;
	Ny=15;
	Kx=2/3*Nx;
	Ky=2/3*Ny;
	theta=linspace(0,pi/2,101);
	kx=Kx*cos(theta);
	ky=Ky*sin(theta);
	plot(kx,ky,'color','b','linewidth',2)
	line([0 Kx Kx],[Ky Ky 0],'color','r','linewidth',2)
	set(gca,'xlim',[0 Nx],'xtick',[0 Kx Nx],'xticklabel',{'$0$','$K_x$','$N_x$'},...
		'ylim',[0 Ny],'ytick',[0 Ky Ny],'yticklabel',{'$0$','$K_y$','$N_y$'})
	xlabel('$k_x$','interpreter','none')
	ylabel('$k_y$','interpreter','none')
	print('-depslatex','-solid','-color','LAM_truncation.tex');

	close

end


clear all
close all

if 0
	% smoothness vs. spectrum
	N=64;
	theta=2*pi*rand(1,N/2-1);
	A=(1:N/2-1).^(-5/3);
	Y=[0,A.*exp(i*theta),0,fliplr(A.*exp(-i*theta))];
	y=real(ifft(Y));y=y/max(abs(y))/1.1;
	x=(1:N);
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	plot(A,'color','b','linewidth',3)
	set(gca,'xlim',[1,N/2-1],'xtick',[],'ytick',[])
	print('-depslatex','-solid','-color','smoothspec1.tex');
	close
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	plot(x,y,'color','b','linewidth',3)
	set(gca,'xlim',[1,N],'xtick',[],'ytick',[])
	print('-depslatex','-solid','-color','smoothfunc1.tex');
	close
	
	theta=2*pi*rand(1,N/2-1);
	A=(1:N/2-1).^(-2/3);
	Y=[0,A.*exp(i*theta),0,fliplr(A.*exp(-i*theta))];
	y=real(ifft(Y));y=y/max(abs(y))/1.1;
	x=(1:N);
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	plot(A,'color','r','linewidth',3)
	set(gca,'xlim',[1,N/2-1],'xtick',[],'ytick',[])
	print('-depslatex','-solid','-color','smoothspec2.tex');
	close
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	plot(x,y,'color','r','linewidth',3)
	set(gca,'xlim',[1,N],'xtick',[],'ytick',[])
	print('-depslatex','-solid','-color','smoothfunc2.tex');
	close
	
	

end

if 0
	% GIBBS
	N=1024;
	xx=(0:N-1)/N;
	yy0=xx>.25&xx<.75;
	cols=rand(3,3);

	for j=1:3
		n=2*4^j;
		x=(0:n-1)/n;
		y=x>.25&x<.75;
		Y=fft(y);
		YY=N/n*[Y(1:n/2),Y(n/2+1)/2,zeros(1,N-n-1),Y(n/2+1)/2,Y(n/2+2:n)];
		yy{j}=real(ifft(YY));
	
		figure('papersize',[2 3],'paperposition',[0 0 3 2]);
		h=plot(xx,yy{j},'-',x,y,'s',xx,yy0,'k');
		set(h(1:2),'color',cols(j,:),'markersize',3)
		set(h,'linewidth',2)
		set(gca,'xtick',[],'ytick',[])
		print('-depslatex','-solid','-color',sprintf('gibbs%i.tex',j));
		close
		
	end
	
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	hold on	
	for j=1:3
		plot(xx,yy{j},'color',cols(j,:),'linewidth',2)
	end
	plot(xx,yy0,'k','linewidth',2);	
	set(gca,'xtick',[],'ytick',[],'linewidth',2)
	print('-depslatex','-solid','-color','gibbs.tex');
	close
	
end


if 0
	% ORDER OF ACCURACY
	Nr=16:16:256;
	e=nan(length(Nr),4);
	for iN=1:length(Nr)
		N=Nr(iN);

		% exact
		x=(-1+2*(.5:N-.5)/N);dx=x(2)-x(1);
		y=10^6*exp(-10./sqrt(1-x.^2));
		ze=-10*y.*(1-x.^2).^-1.5.*x;ze(1)=0;

		% spectral
		k=[0:N/2-1,-N/2:-1]*2*pi/(N*dx);
		Y=fft(y);
		Z=i*k.*Y;
		zs=real(ifft(Z));
		e(iN,1)=norm(zs-ze);

		% 1st order
		z1=(y(mod((1:N),N)+1)-y(mod((1:N)-1,N)+1))/dx;
		e(iN,2)=norm(z1-ze);
		
		% 2nd order
		z2=(y(mod((1:N),N)+1)-y(mod((1:N)-2,N)+1))/(2*dx);
		e(iN,3)=norm(z2-ze);

		% 4th order
		z4=4/3*(y(mod((1:N),N)+1)-y(mod((1:N)-2,N)+1))/(2*dx)-1/3*(y(mod((1:N)+1,N)+1)-y(mod((1:N)-3,N)+1))/(4*dx);
		e(iN,4)=norm(z4-ze);
		
	end
	
	figure('papersize',[2 3],'paperposition',[0 0 3 2]);
	plot(Nr,e)
	set(gca,'yscale','log','xscale','log','xlim',Nr([1,end]),'ylim',[1e-8,1e2],...
		'xtick',16*2.^(0:4),'xticklabel',{'16','32','64','128','256'},...
		'ytick',10.^(-8:2:2),'yticklabel',{'$10^{-8}$','$10^{-6}$','$10^{-4}$','$10^{-2}$','$10^{0}$','$10^{2}$'})
	xlabel('\# gridpoints')
	ylabel('error')
	print('-depslatex','-solid','-color','derivaccur.tex');
	close
end

if 1
	% POLE PROBLEM
	[x,y,z]=sphere(20);
	figure
	surf(x,y,z,'facecolor','w','linewidth',5)
	axis equal off
	set(gcf,'papersize',[10,10],'paperposition',[0,0,10,10])
	set(gca,'position',[0,0,1,1])
	print -deps2 poleproblem.eps
	
end