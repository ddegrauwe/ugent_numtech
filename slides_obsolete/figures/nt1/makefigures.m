% make figures for Lesson 2: time discretization
close all
set(0,'defaulttexthorizontalalignment','center')

if true
	% advection equation
	xr=linspace(0,10,101);
	y1=5*exp(-(xr-5).^2/4);
	y2=5*exp(-(xr-6).^2/4);
	
	fid=fopen('y1.dat','wt');fprintf(fid,'%8.4f %8.4f\n',[xr;y1]);fclose(fid);
	fid=fopen('y2.dat','wt');fprintf(fid,'%8.4f %8.4f\n',[xr;y2]);fclose(fid);
end
