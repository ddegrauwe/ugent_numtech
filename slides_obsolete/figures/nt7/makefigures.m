clear all
close all

% amdahl
if (true)
	figure('papersize',[9,4],'paperposition',[0 0 9 4],'paperorientation','landscape')
	
	logN=0:16;						% logarithm of number of processors
	N=2.^logN;
	
	pp=[.5,.75,.9,.95];		% parallel portion
	
	[N,pp]=ndgrid(N,pp);
	
	s=1./(pp./N+(1-pp));
	
	plot(logN,s,'linewidth',2)
	set(gca,'xtick',logN,'xticklabel',mat2cell(reshape(sprintf('%5i',N(:,1)),5,length(logN))',ones(length(logN),1),5))
	xlim(logN([1,end]))
	xlabel('Number of processors')
	ylabel('Speedup')
	legend({'50\%','75\%','90\%','95\%'})
	
	print('-depslatex','amdahl.tex')
	
	close
	
end

% communications
if (false)
	N=4;
	theta=2*pi*(1:N)/N+.1*rand(1,N);
	x=[nan,cos(theta)];
	y=[nan,sin(theta)];
	[i1,i2]=ndgrid(2:N+1);
	J=[ones(N**2,1),i1(:),i2(:)]';
	plot(x(J(:)),y(J(:)),'color','k');
	axis equal off;
	print -deps2 comm4.eps


	N=20;
	theta=2*pi*(1:N)/N+.1*rand(1,N);
	x=[nan,cos(theta)];
	y=[nan,sin(theta)];
	[i1,i2]=ndgrid(2:N+1);
	J=[ones(N**2,1),i1(:),i2(:)]';
	plot(x(J(:)),y(J(:)),'color','k');
	axis equal off;
	print -deps2 comm20.eps


	N=100;
	theta=2*pi*(1:N)/N+.1*rand(1,N);
	x=[nan,cos(theta)];
	y=[nan,sin(theta)];
	[i1,i2]=ndgrid(2:N+1);
	J=[ones(N**2,1),i1(:),i2(:)]';
	plot(x(J(:)),y(J(:)),'color','k');
	axis equal off;
	print -deps2 comm100.eps
end
