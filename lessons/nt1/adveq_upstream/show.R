# R script to show results from advection equation
#
# the results are supposed to be in an ascii file
# first line: nx, dx, nt, dt, nexp 
# second line: experiment names
# from third line on: numerical results

# load tool for animation
source('/user/gent/407/vsc40744/ugent/numtech/aux/animateR/animatefun.R',chdir=TRUE)

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

# function to plot results for one timestep
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

# function to show results
animateResults=function(fileName='output.dat',expNames=c('exact','upstream')) {

	# open a new window
	x11(width=11,height=6)

	# first read results
	L=readResults(fileName)
	
	# animate
	animate(plot_fcn,L$nt,startPlaying=FALSE,expNames,L)
}

