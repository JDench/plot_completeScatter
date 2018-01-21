# plot_completeScatter
This function can be used to generate a scatterplot for set of categorical data you want to compare.  The categorical data is assumed to have two nested categories, and plots each sub category sequentially, with the hierarchial categories side-by-side for comparison.  It was designed to quickly give me publication ready plots for competitive fitness data.

This function will depend on 3 other functions I've written: colTransparency.v.1.r; definedJitter.v.1.r; define_plotSpace.v.1.r

Feel free to download the compFitness.csv and testScript.r files in order to get a feel for this function.  You should be able to replicate the pdf I've uploaded using the information provided.  Just open R, add in the 4 functions (this one and the 3 dependencies), then run the testScript.r lines!  Enjoy.
