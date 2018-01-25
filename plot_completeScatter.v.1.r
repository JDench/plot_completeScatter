# This function produces a scatterplot for categorical X- values using data where different elements of a group 
# are coloured indepednently and the global mean as well as significance box is presented.  Points can be plotted 
# with jitter.  The minimum input is a matrix/data.frame with at least 4 columns.  If nothing else is provided it 
# will be assumed that the first column is the y-axis vlues and the second is for x-axis categories, the third column 
# is a sub-categorisation for the main x-categories, and the fourth is a unique ID column.  Additionally users can indicate which columns to use, colours to use, 
# type of significance box, and whether or not to use jitter.  Also other plotting elements like mar and mfcol can be specified.
# This function depends on the define_plotSpace, definedJitter, and colTransparency functions I've written.
# NOTE: This default plotting space I've worked with will look nice at height = 16, width =32, for 6 different subX types and 2 categoricalX
plot_completeScatter <- function(func_inData, func_plotY = 1, func_plotX = 2, func_subX =3, func_IDcol = 4, 
									func_plotJitter = TRUE, func_jitterRange = NULL, func_visualSeparators = FALSE,
									func_plotCI = TRUE, func_CIoppacity = 0.3, func_sigFig = 3,
									func_plot_mfcol = c(1,1), func_plotMar = c(10,10,5,2)+0.1, func_plotCols = NULL,
									func_xLab = "X Types", func_xLas = 2, func_yLab = "Y Values",
									func_cexAxis = 3, func_cexLab = 4, func_cexLegend = 2, func_cexPch = 3.5, 
									func_categoricalOrder = NULL, func_subOrder = NULL,
									func_plotMean = TRUE, func_addLine = NULL, func_yRange = NULL, 
									func_legendSpot = "topright", func_legendArgs = NULL,
									func_spaceParms = list("bufferPerc"=0.1,"bufferMax"=0.4,"bufferMin"=0.15,"scaleX"=20,
															"mean"=c("lwd"=2,"col"="black"))){
										
	# This creates a reference vector for our categorical types of X, order the factors based on a reference if one exists
	# We also do the same for the sub-categories.  While updating the categoricals of the data, 
	# we'll similarly update it but only if something different was passed
	all_categoricalX <- as.character(unique(func_inData[, func_plotX]))
	all_categoricalX <- factor(all_categoricalX,levels=if(!is.null(func_categoricalOrder)){func_categoricalOrder}else{unique(all_categoricalX)})
	all_categoricalX <- all_categoricalX[order(all_categoricalX)]
	if(!is.null(func_categoricalOrder)){
		func_inData[, func_plotX] <- factor(as.character(func_inData[, func_plotX]), levels = func_categoricalOrder)
		func_inData <- func_inData[order(func_inData[, func_plotX]),]
	}
	
	all_subX <- as.character(unique(func_inData[, func_subX]))
	all_subX <- factor(all_subX,levels=if(!is.null(func_subOrder)){func_subOrder}else{unique(all_subX)})
	all_subX <- all_subX[order(all_subX)]
	if(!is.null(func_subOrder)){
		func_inData[, func_subX] <- factor(as.character(func_inData[, func_subX]), levels = func_subOrder)
		func_inData <- func_inData[order(func_inData[,func_subX]),]
	}
	
	
	# We formalise the colour set to be plotted.  This uses the supplied values or draws from rainbow if nothing was supplied
	# The point with the colours is to have the categorical types separated by colour, and then the replicates within sub-groups
	# to use unique shades.
	func_plotCols <- if(is.list(func_plotCols) && length(func_plotCols) == length(all_categoricalX)) {
						# This means we'll trust the user supplied a proper list.
						func_plotCols
					} else {
						lapply(if(is.null(func_plotCols)){
								rainbow(length(all_categoricalX))
							# If the user supplied a list of length equal to the number of categories, we'll assume they've supplied a sufficient list
							} else {
								func_plotCols <- unlist(func_plotCols)
								# Now if the user has supplied the correct number of colours we simply return this
								if(length(func_plotCols) == length(all_categoricalX)){
									unlist(func_plotCols)
								# This means that we'll be recycling some colours
								} else if(length(func_plotCols) < length(all_categoricalX)){
									c(rep(func_plotCols,floor(length(all_categoricalX)/length(func_plotCols))),
											func_plotCols[1: (length(all_categoricalX) %% length(func_plotCols))])
								# This means we'll only use some colours	
								} else {
									func_plotCols[1: (length(all_categoricalX) %% length(func_plotCols))]
								}
							} ,function(x){ return(x) })
							
					}
	# We name the colour list to be the same as the categories that exist.
	names(func_plotCols) <- all_categoricalX
	# Here we check that for each element of func_plotCols, there are enough ID colours
	for(this_categoryX in names(func_plotCols)){
		# For each subX, we count the number of IDcol, and return that value, then take the max
		func_numCols <- max(unlist(lapply(unique(func_inData[,func_subX]),function(this_sub){
							length(unique(func_inData[intersect(which(func_inData[, func_plotX] == this_categoryX),
																which(func_inData[, func_subX] == this_sub)), 
														func_IDcol]))
						})))
		# If the required number of colours is not met, then we take the first value of the set and simply use shades thereof.
		if(length(func_plotCols[[this_categoryX]]) < func_numCols){
			# I put the darkest first for reasons of inheritance from the polygon downstream
			func_plotCols[[this_categoryX]] <- colTransparency(func_plotCols[[this_categoryX]][1],seq(1,0.25,length.out= func_numCols))
		}
	}
	
	# This is the total xSpace we'll need, this is blocked by the all_categoricalX
	tmp_xRange <- c(0,length(unique(func_inData[,func_subX])) * func_spaceParms[["scaleX"]])
	# We will be divindg the x-axis space into equal portions, this is, for these plots, separated by growths, and ID
	# we have a master object which defines the scale of space 
	tmp_xAxis <- define_plotSpace(func_subDivs= func_inData[,c(func_subX, func_plotX)], 
									func_tmpSpace= tmp_xRange,
									func_tmpBuffer = func_spaceParms[["bufferPerc"]],
									func_tmpBuffer_max = func_spaceParms[["bufferMax"]],
									func_tmpBuffer_min = func_spaceParms[["bufferMin"]])
	# Now if there has been now y-Range supplied, we'll use the data
	if(is.null(func_yRange)){
		func_yRange <- round(seq(min(func_inData[, func_plotY]),max(func_inData[, func_plotY]),length.out=7), func_sigFig)
	}
	
	# These are parameters for the plot
	par(mfcol= func_plot_mfcol, mar = func_plotMar)
	# Right now we can setup our blank plotting space
	plot(x=NULL,y=NULL,xlab=NULL,xlim= tmp_xRange, ylab=NULL,ylim=c(min(func_yRange),max(func_yRange)),
			axes=FALSE, ann = FALSE, bty="n", cex.lab= func_cexLab)
	
	# This is the x-lab and axis tickmarks
	axis(side=1,padj=1.25,at= sapply(1:length(tmp_xAxis),function(x){ mean(unlist(tmp_xAxis[[x]])) }),
			labels=as.character(all_subX), las= func_xLas,cex.axis= func_cexAxis)
	mtext(func_xLab,side=1,line=8,outer=FALSE, cex = func_cexLab)
	
	# This is the y-lab and axis tick marks
	axis(side=2,at=func_yRange, las=1,cex.axis= func_cexAxis)
	mtext(func_yLab,side=2,line=7,outer=FALSE, cex = func_cexLab)
	
	# If the user has requested that there be visual separators , we plot boxes around alternating subX ranges
	# these will be done as light grey boxes, I'm not giving choices at the moment for customisation, ask if you care.
	if(func_visualSeparators){
		# We do alternating boxes around the even valued area
		for(func_thisSeparator in seq(2,length(all_subX),by=2)){
			polygon(x=c(rep(mean(c(max(unlist(tmp_xAxis[[func_thisSeparator-1]])),min(unlist(tmp_xAxis[[func_thisSeparator]])))),2),
						rep(mean(c(max(unlist(tmp_xAxis[[func_thisSeparator]])),if(length(tmp_xAxis)> func_thisSeparator){min(unlist(tmp_xAxis[[func_thisSeparator + 1]]))}else{max(tmp_xRange)})),2)),
					y=c(max(func_yRange),min(func_yRange),rev(c(max(func_yRange),min(func_yRange)))),
					col= colTransparency("grey90",0.5), border = NA)	
		}
	}
	
	# If the user has request to plot an abline we pass their arguments assuming they've specified this properly.
	if(!is.null(func_addLine)){
		do.call(abline, func_addLine)
	}
	
	# First step is to sub-divide by the main categorical type
	for(this_categoryX in all_subX){
		func_categoryRows <- which(func_inData[, func_subX] == this_categoryX)
		# Now we subdivide by a secondary X-factor
			if(length(func_categoryRows) > 0){
			for(this_subCategory in unique(func_inData[func_categoryRows, func_plotX])){
				# We define the rows for our sub-division
				func_subRows <- func_categoryRows[which(as.character(func_inData[func_categoryRows, func_plotX]) == this_subCategory)]
				if(length(func_subRows) > 0){
					# We find how many unique ID's there are
					func_theseIDs <- unique(func_inData[func_subRows, func_IDcol])					
					# We now define a vector of colours which is based on which of these rows is a unique genotype contruct
					tmp_colVec <- func_plotCols[[this_subCategory]][unlist(lapply(func_inData[func_subRows, func_IDcol], function(x){ 
															which(unique(func_inData[func_subRows, func_IDcol]) == x) }))]
					# We now define the x-axis point for this mutant by background, found in our x-axis space lists, we also adjust this by our buffer space, use the Rows which were defined
					# just prior to establishing the list's X space list (see order of columns sent to your call to define_plotSpace(func_subDivs....)
					tmpPlot_xRange <- unlist(tmp_xAxis[[which(all_subX == this_categoryX)]][[which(unique(func_inData[func_categoryRows, func_plotX]) == this_subCategory)]])
					tmpPlot_xRange <- c(tmpPlot_xRange[1] + max(func_spaceParms[["bufferMin"]], min(func_spaceParms[["bufferMax"]],(func_spaceParms[["bufferPerc"]] * (tmpPlot_xRange[2]-tmpPlot_xRange[1])))),
											tmpPlot_xRange[2] - max(func_spaceParms[["bufferMin"]], min(func_spaceParms[["bufferMax"]],(func_spaceParms[["bufferPerc"]] * (tmpPlot_xRange[2]-tmpPlot_xRange[1])))))
					tmpX <- mean(tmpPlot_xRange)
					# As a visual convenience I 'll calculate the grand mean and SD values for these row
					# The "CI" value will be normal standard deviation * 1.96 to be 95% CI, if func_plotCI = TRUE, else we return only the SEM
					tmpValues <- c("Mean"= if(func_plotMean){mean(func_inData[func_subRows, func_plotY],na.rm=TRUE)}else{median(func_inData[func_subRows, func_plotY],na.rm=TRUE)},
									"CI"= sd(func_inData[func_subRows, func_plotY],na.rm=TRUE) * if(func_plotCI){1.96}else{(sqrt(length(func_subRows)))^-1})
					
					# We add a polygon of the CI and then the mean lines first so they are behind the points themselves
					polygon(x=c(tmpPlot_xRange,rev(tmpPlot_xRange)),
							y=c(rep(tmpValues["Mean"] - tmpValues["CI"],2), rep(tmpValues["Mean"] + tmpValues["CI"],2)),
							col= colTransparency(func_plotCols[[this_subCategory]][1], func_CIoppacity))
					lines(x= tmpPlot_xRange , y= rep(tmpValues["Mean"],2), 
							col = func_spaceParms[["mean"]]["col"],lwd=func_spaceParms[["mean"]]["lwd"])
					# Now we add points to out plot and colour each based on the mutant type
					points(x=if(func_plotJitter){ definedJitter(length(func_subRows), if(is.null(func_jitterRange)){tmpPlot_xRange}else{func_jitterRange}) }else{ rep(tmpX,length(func_subRows)) },
							y= func_inData[func_subRows, func_plotY],
							col="black",pch=21,bg= tmp_colVec,cex= func_cexPch)
				}
			}
		}
	}
	# We now create a legend, one for each background, for the colours used in our plot.
	# The user may define a particular legend, otherwise these defaults should be mostly appropriate.		
	if(!is.null(func_legendArgs)){
		do.call(legend, func_legendArgs)
	} else {
		legend(func_legendSpot, legend= unlist(lapply(all_categoricalX,function(thisSet){ paste(thisSet, 1:length(func_plotCols[[thisSet]]),sep =" ") })),
				fill= unlist(func_plotCols), bty="n",cex= func_cexLegend, ncol = length(all_categoricalX))
	}

}

