# R script to show results from advection equation
#
# the results are supposed to be in an ascii file
# first line: nx, dx, nt, dt, nexp 
# second line: experiment names
# from third line on: numerical results

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

# function to show results
showResults=function(fileName='output.dat',expNames=NULL,
                     freq=1,delay=0.1) {

	# open a new window
	windows(20,10)

	# first read results
	L=readResults(fileName)

	# default is to plot everything
	if (is.null(expNames) ) expNames=L$expNames

	# then animate
	for (it in seq(1,L$nt+1,by=freq)) {
		# create axes with correct limits
		plot(NA,NA,xlim=range(L$x),ylim=range(L$y),xlab='x',ylab='phi')
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
		# for smooth animation in RStudio
		dev.flush()
		Sys.sleep(delay)
	}
}

