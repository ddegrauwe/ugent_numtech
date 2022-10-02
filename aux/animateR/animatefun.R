# load tcltk library
library(tcltk)

# control panel icons
tkPlayImage = tclVar(); tkimage.create("photo",tkPlayImage,file='./icons/play.gif')
tkPauseImage = tclVar(); tkimage.create("photo",tkPauseImage,file='./icons/pause.gif')
tkPrevImage = tclVar(); tkimage.create("photo",tkPrevImage,file='./icons/prev.gif')
tkNextImage = tclVar(); tkimage.create("photo",tkNextImage,file='./icons/next.gif')
tkFirstImage = tclVar(); tkimage.create("photo",tkFirstImage,file='./icons/first.gif')
tkLastImage = tclVar(); tkimage.create("photo",tkLastImage,file='./icons/last.gif')


# main animation function
animate=function(plot_fcn,nFrames,startPlaying=FALSE,...) {
  
  # open animation window
  if ( dev.cur()==1 ) x11(20,10)
  xdev=dev.cur()
  devName=attributes(dev.list()[dev.list()==xdev])$names
  
  # main tk window
  tkControlPanel=tktoplevel()
  tktitle(tkControlPanel)='Animation Control'
  
  # tcl variables
  tkTheFrame=tclVar(1)
  tkTheFrameEntry=tclVar(1)
  tkIsPlaying=tclVar(0)
  tkTheSpeed=tclVar(0)
  #tkTheFrameSpeedEntry=tclVar(10)
  tkDoLoop=tclVar(0)

  # tk widgets
  tkFrameLabel=tklabel(tkControlPanel,text='Frame')
  tkFrameEntry=tkentry(tkControlPanel,textvariable = tkTheFrameEntry,width=5)
  tkPlayButton=tkbutton(tkControlPanel,image=tkPlayImage)
  tkPrevButton=tkbutton(tkControlPanel,image=tkPrevImage)
  tkNextButton=tkbutton(tkControlPanel,image=tkNextImage)
  tkFirstButton=tkbutton(tkControlPanel,image=tkFirstImage)
  tkLastButton=tkbutton(tkControlPanel,image=tkLastImage)
  tkLoopCheckBox=tkcheckbutton(tkControlPanel,text="Loop")
  
  tkSpeedScale=tkscale(tkControlPanel, from=-5, to=5, showvalue=FALSE,
	variable=tkTheSpeed, resolution=0.1, orient="horizontal",
	length=94)

	# show widgets in control panel
	tkgrid(tkFirstButton,tkPrevButton,tkPlayButton,tkNextButton,tkLastButton,padx=10,pady=5,row=0)
	tkgrid(tkLoopCheckBox,padx=0,pady=5,row=1,column=0)
	tkgrid(tkSpeedScale,padx=0,pady=5,row=1,column=1,columnspan=2)
	tkgrid(tkFrameLabel,padx=0,pady=5,column=3,row=1)
	tkgrid(tkFrameEntry,padx=0,pady=5,column=4,row=1)

  # update function
  update_plot=function() {
    # check if window is still open
    if ( ! any( dev.list() == xdev ) ) return()
    
    # update frame number in tcl window
    tclvalue(tkTheFrameEntry)=sprintf('%i',as.integer(tclvalue(tkTheFrame)))
    # update plot
    dev.set(xdev)
    it=as.integer(tclvalue(tkTheFrame))
    plot_fcn(it,...)
    dev.flush()	# only on windows
  }
  
  # widget callbacks
  tkPrevButton_fcn=function() {
    if (tclvalue(tkDoLoop)==1) {
      tclvalue(tkTheFrame)=(as.integer(tclvalue(tkTheFrame))-2)%%nFrames+1
    } else {
      tclvalue(tkTheFrame)=max(1,as.integer(tclvalue(tkTheFrame))-1)
    }
    update_plot()
  }
  tkNextButton_fcn=function() {
    if (tclvalue(tkDoLoop)==1) {
      tclvalue(tkTheFrame)=(as.integer(tclvalue(tkTheFrame)))%%nFrames+1
    } else {
      tclvalue(tkTheFrame)=min(nFrames,as.integer(tclvalue(tkTheFrame))+1)
    }
    update_plot()
  }
  tkFirstButton_fcn=function() {
    tclvalue(tkTheFrame)=1
    update_plot()
  }
  tkLastButton_fcn=function() {
    tclvalue(tkTheFrame)=nFrames
    update_plot()
  }
  tkPlayButton_fcn=function() {
    if ( tclvalue(tkIsPlaying)==0 ) {
      tclvalue(tkIsPlaying)=1
      tkconfigure(tkPlayButton,image=tkPauseImage)
      tkconfigure(tkFrameEntry,state='disabled')
    } else {
      tclvalue(tkIsPlaying)=0
      tkconfigure(tkPlayButton,image=tkPlayImage)
      tkconfigure(tkFrameEntry,state='normal')
    }
  }
  tkFrameEntry_fcn=function() {
    tclvalue(tkTheFrame)=as.integer(tclvalue(tkTheFrameEntry))
    update_plot()
  }
  tkSpeedScale_fcn=function(...) {
  }
  
  
  # bind callback functions to widgets
  tkbind(tkFrameEntry,'<Return>',tkFrameEntry_fcn)
  tkconfigure(tkFirstButton,command=tkFirstButton_fcn)
  tkconfigure(tkPrevButton,command=tkPrevButton_fcn)
  tkconfigure(tkPlayButton,command=tkPlayButton_fcn)
  tkconfigure(tkNextButton,command=tkNextButton_fcn)
  tkconfigure(tkLastButton,command=tkLastButton_fcn)
  tkconfigure(tkSpeedScale,command=tkSpeedScale_fcn)
  tkconfigure(tkLoopCheckBox,variable=tkDoLoop)
  
  # initial plot
  update_plot()
  
  # should playing start?
  if (startPlaying) tkPlayButton_fcn()
  
  # start loop until control panel is closed
  while ( TRUE ) {
  	# check if figure still exists
    if ( ! any( dev.list() == xdev ) ) {
  		# close control panel
  		tkdestroy(tkControlPanel)
  		break
  	}
    # test if control panel still exists
    zz=try(tkwm.state(tkControlPanel),TRUE)
    if (class(zz)=='try-error') {
      # close figure
      dev.off(xdev)
      break
    }
    # update plot
    ii=as.integer(tclvalue(tkTheFrame))
    if  ( as.integer(tclvalue(tkIsPlaying))==1 ) {
		
		# calculate stride and delay; no use to have more than 25fps
		ss=as.integer(tclvalue(tkTheSpeed))  # between -5 and 5
		if ( ss >0 ) {
			delay=0.05
			stride=(nFrames)**(1+ss/5)
		} else {
			delay=0.2-ss/10
			stride=1
		}
		
		# next frame
		ii=ii+stride
		if (ii>nFrames) {
			if (tclvalue(tkDoLoop)==1) {
				# start over again
				ii=(ii-1)%%nFrames+1
			} else {
				# stop playing
				ii=nFrames
				tkPlayButton_fcn()
			}
		}
		tclvalue(tkTheFrame)=ii
		Sys.sleep(delay)
		update_plot()
    }
  }
}
