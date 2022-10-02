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
	nx=zz[1]; nt=zz[2]; dx=zz[3]; dt=zz[4];
	# read results
	y=read.table(fileName,skip=1)
	# number of timesteps successfully read
	nt=nrow(y)-1
	# u and h
	phi=as.matrix(y[(0:nt)+1,],nt+1,nx)
	# put everything together
	L=list(nx=nx,nt=nt,x=(1:nx)*dx,t=(0:nt)*dt,phi=phi)
	return(L)
}

# function to plot one timestep
plot_fcn=function(it,L) {
	# create axes with correct limits
	plot(L$x,L$phi[it,],xlim=range(L$x),ylim=c(-2,2),xlab='x',ylab='phi',type='l',lwd=2,col='blue')
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

