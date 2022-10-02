# file to show results of the 1D shallow water equation toy model
#
# the results are supposed to be in an ascii file
# first line: nx, dx, nt, dt, um
# from third line on: numerical results: first U, then H

# load tk-tool for animation
if ( file.exists('H:/numtech/animateR/animatefun.R') ) {
	source('H:/numtech/animateR/animatefun.R',chdir=TRUE)
	animCtrl=TRUE
} else {
	cat('Animation auxiliary functions not found\n',
		'To use these, please execute the following on helios:\n',
		'    mkdir -p /files/${HOME}/home/numtech/\n',
		'    cp -r /users/d/ddgrauwe/numtech/animateR/ /files/${HOME}/home/numtech/\n')
	animCtrl=FALSE
}

# function to read results
readResults=function(fileName='output.dat') {
	# read parameters
	zz=as.numeric(read.table(fileName,nrows=1,stringsAsFactors=F))
	nx=zz[1]; dx=zz[2]; nt=zz[3]; dt=zz[4]; um=zz[5]
	# read results
	y=read.table(fileName,skip=1)
	# number of timesteps successfully read
	nt=nrow(y)/2-1
	# u and h
	u=as.matrix(y[2*(0:nt)+1,],nt+1,nx)
	h=as.matrix(y[2*(0:nt)+2,],nt+1,nx)
	# put everything together
	L=list(nx=nx,nt=nt,x=(1:nx)*dx,t=(0:nt)*dt,u=u,h=h,um=um)
	return(L)
}

plot_fcn=function(it,L) {
	# fish coordinates
	xfish=.3*c(-2.21421,-0.70711,-0.38268,0.00000,0.38268,0.70711,0.92388,1.00000,0.92388,0.70711,0.38268,0.00000,-0.38268,-0.70711,-2.21421,-2.21421)
	yfish=.5+.1*c(0.80000,-0.70711,-0.92388,-1.00000,-0.92388,-0.70711,-0.38268,0.00000,0.38268,0.70711,0.92388,1.00000,0.92388,0.70711,-0.80000,0.80000)
	# create axes with correct limits
	plot(L$x,L$h[it,],xlim=range(L$x),ylim=c(0,2),xlab='x',ylab='h',type='l',lwd=2,col='blue')
	# plot dead fish
	x0=(L$um*L$t[it]+max(L$x)/2)%%max(L$x)
	polygon(x0+xfish,yfish,col=2)
	points(x0+.1,.53,pch=4)
	# add title
	title(sprintf('%6.2f (%3i/%3i)',L$t[it],(it-1),L$nt))
}

# function to show results
animateResults=function(fileName='output.dat',delay=.1) {

	# open a new window
	windows(20,10)

	# first read results
	L=readResults(fileName)
	
	
	# animate
	if (animCtrl) {
		# with animation control
		animate(plot_fcn,L$nt,startPlaying=FALSE,L)
	} else {
		# just plot all timesteps
		for (it in seq(1,L$nt+1,by=stride)) {
			# create axes with correct limits
			plot_fcn(it,L)
			# for smooth animation in RStudio
			dev.flush()
			Sys.sleep(delay)
		}		
	}
}

