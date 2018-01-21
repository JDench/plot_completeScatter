compFitness <- read.csv("compFitness.csv",header=TRUE)
pdf("compFitness_categoricalScatter.pdf",height=16,width=32)
plot_completeScatter(func_inData = compFitness, 
					func_plotY = "compFitness", 
					func_plotX = "Background", 
					func_subX ="Mutant", 
					func_IDcol = "ID", 
 					func_plotJitter = TRUE, 
 					func_jitterRange = NULL, 
 					func_CIoppacity = 0.3,
 					func_plot_mfcol = c(1,1), 
 					func_plotMar = c(10,10,5,2)+0.1, 
 					func_plotCols = list("PA01"=c("deepskyblue","lightskyblue1","deepskyblue4","darkturquoise","royalblue1","steelblue2"),
										"PA14"=c("darkorchid1","violet","slateblue2","darkmagenta","plum","violetred")),
 					func_xLab = "Genotype", 
 					func_yLab = "Competitive Fitness", 
 					func_categoricalOrder = c("PA01","PA14"),
 					func_subOrder = c("WT","gyrA_T83I","parC_S87L","parC_S87W","gyrA_T83I--parC_S87L","gyrA_T83I--parC_S87W")
 					func_plotMean = TRUE, 
 					func_addLine = list("h"=1,"col"="red","lty"=2,"lwd"=2), 
 					func_yRange = seq(0.94,1.06,by=0.02), 
 					func_legendSpot = "topright",
 					func_spaceParms = list("bufferPerc"=0.1,"bufferMax"=0.4,"bufferMin"=0.15,"scaleX"=20,
 											"mean"=c("lwd"=5,"col"="black")))

dev.off()