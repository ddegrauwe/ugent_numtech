% make figures for Lesson 2: time discretization
close all
set(0,'defaulttexthorizontalalignment','center')

if false
	% amplification, damping and exact
	figure('papersize',[3,4],'paperposition',[0,0,4,3])
	axes('position',[.1 .1 .8 .8])
	dt=.1;
	tr=(0:200)*dt;
	Ae=exp(i); ye=real(Ae.^tr);
	Aa=1.02*exp(i); ya=real(Aa.^tr);
	Ad=exp(i)/1.02; yd=real(Ad.^tr);
	h=plot(tr,ye,tr,ya,tr,yd);
	set(h,'linewidth',4)
	legend('Exact','Amplifying','Damping','location','northwest')
	set(gca,'xlim',[tr(1),tr(end)],'ylim',[-2,2.2],'xtick',[],'ytick',[])
	xlabel('$t$')
	ylabel('$\psi(t)$')
	print('-depslatex','-dashed','-color','amp_damp.tex')
	close	
end

if false
	% amplification, damping and exact
	figure('papersize',[3,4],'paperposition',[0,0,4,3])
	axes('position',[.1 .1 .8 .8])
	dt=.1;
	tr=(0:200)*dt;
	Ae=exp(i); ye=real(Ae.^tr);
	Aa=exp(i*1.05); ya=real(Aa.^tr);
	Ad=exp(i/1.05); yd=real(Ad.^tr);
	h=plot(tr,ye,tr,ya,tr,yd);
	set(h,'linewidth',4)
	legend('Exact','Accelerating','Decelerating','location','northwest')
	set(gca,'xlim',[tr(1),tr(end)],'ylim',[-2,2.2],'xtick',[],'ytick',[])
	xlabel('$t$')
	ylabel('$\psi(t)$')
	print('-depslatex','-dashed','-color','acc_dec.tex')
	close	
end
if true
	% 2TL amplification
	figure('papersize',[3,4],'paperposition',[0,0,4,3])
	axes('position',[.1 .1 .8 .8])
	kr=linspace(0,pi/2,101);
	Ae=kr.^0;
	Af=abs(1+i*kr);
	Ab=abs(1./(1-i*kr));
	Ar=abs(1+i*kr-.5*kr.^2);
	Am=abs(1+i*kr-kr.^2);
	h=plot(kr,Ae,kr,Af,kr,Ab,kr,Ar,kr,Am);
	set(h,'linewidth',4)
	legend('Exact','Forward','Backward','Heun','Matsuno','location','northwest')
	set(gca,'xlim',[0,pi/2],'ylim',[0 2],'xtick',[0,1],'xticklabel',{'0','1'})
	xlabel('$\kappa\Delta t$')
	ylabel('$|A|$')
	print('-depslatex','-dashed','-color','2TLA.tex')
	close
end

if true
	% 2TL phase change
	figure('papersize',[3,4],'paperposition',[0,0,4,3])
	axes('position',[.1 .1 .8 .8])
	kr=linspace(0,pi/2,101);
	Re=kr.^0;
	Rf=atan(kr)./kr;
	Rt=atan(kr./(1-.25*kr.^2))./kr;
	Rr=atan(kr./(1-.5*kr.^2))./kr;
	Rm=atan(kr./(1-kr.^2))./kr;Rm(kr>1)=nan;
	h=plot(kr,Re,kr,Rf,kr,Rt,kr,Rr,kr,Rm);
	set(h,'linewidth',4)
	legend('Exact','Fwd/Bwd','Trapezium','Heun','Matsuno','location','northwest')
	set(gca,'xlim',[0,1],'ylim',[.5,1.5],'xtick',[0,1],'xticklabel',{'0','1'})
	xlabel('$\kappa\Delta t$')
	ylabel('$R$')
	print('-depslatex','-dashed','-color','2TLR.tex')
	close
end
