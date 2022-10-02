# R script to show results from advection equation
#
# after loading this file with source('show.R'), use animateResults('output.dat') to animate results of an experiment
#
# the results in 'output.dat' are supposed to be in an ascii file
# 	first line: nx, dx, nt, dt, nexp 
# 	second line: experiment names
# 	from third line on: numerical results
#

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
	zz=as.numeric(read.table(fileName,nrows=1))
	nx=zz[1]; dx=zz[2]; nt=zz[3]; dt=zz[4]; nexp=zz[5];
	# read experiment names
	expNames=rep('',nexp)
	zz=read.table(fileName,skip=1,nrows=1)
	for (iExp in 1:nexp) { expNames[iExp]=as.character(as.list(zz)[[iExp]]) }
	# read results
	y=read.table(fileName,skip=2)
	nt=nrow(y)/nexp-1
	y=array(as.matrix(y,nexp*(nt+1),nx),c(nexp,nt+1,nx))
	# put everything together
	L=list(nx=nx,nt=nt,nexp=nexp,x=(1:nx)*dx,t=(0:nt)*dt,y=y,expNames=expNames)
	return(L)
}

# function to plot results for a single timestep
plot_fcn=function(it,expNames,L) {
	# create axes with correct limits
	plot(NA,NA,xlim=range(L$x),ylim=c(-2,2),xlab='x',ylab='phi')
	# plot experiments
	for (iExp in 1:length(expNames) ) {
		# look for experiment with this name
		iiExp=which(L$expNames==expNames[iExp])
		# error if unknown experiment
		if (length(iiExp)==0) stop('unknown experiment')
		# actual plotting
		points(L$x,L$y[iiExp,it,],col=iExp,type='l',lwd=2)
	}
	# add legend
	legend('topleft',expNames,col=1:length(expNames),lwd=2)
	# add title
	title(sprintf('%6.2f (%3i/%3i)',L$t[it],(it-1),L$nt))
}

# function to animate results
animateResults=function(fileName='output.dat',expNames=NULL,stride=1,delay=0.025) {

	# open a new window
	windows(20,10)

	# first read results
	L=readResults(fileName)
	
	# select experiments
	if ( is.null(expNames) ) expNames=L$expNames
	
	# animate
	if (animCtrl) {
		# with animation control
		animate(plot_fcn,L$nt+1,startPlaying=FALSE,expNames,L)
	} else {
		# just plot all timesteps
		for (it in seq(1,L$nt+1,by=stride)) {
			# create axes with correct limits
			plot_fcn(it,expNames,L)
			# for smooth animation in RStudio
			dev.flush()
			Sys.sleep(delay)
		}		
	}
}

# function to plot results at specified timestep
plotResults=function(fileName='output.dat',timestep=0,expNames=NULL) {

	# open a new window
	windows(20,10)

	# first read results
	L=readResults(fileName)
	
	# select experiments
	if ( is.null(expNames) ) expNames=L$expNames
	
	# plot
	plot_fcn(timestep+1,expNames,L)
}