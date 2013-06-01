\name{niche}
\alias{niche}
\title{Environmental niche}
\description{
  Plot the environmental conditions with higher number of species, a boxplot
with the range of environmental variables under the environmental conditions
selected by the user and the list of species under the  environmental conditions
selected by the user, using an output file of ModestR.

}

 
\usage{
  niche(data=data, pixs=3, d.main=1, xlab="Polar coordinate X",
  ylab="Polar coordinate Y", cex.lab=1.5, font.lab=1, main=NULL, zmax=NULL,
  ztransf = function(x){x}, colramp = IDPcolorRamp, cex = 1,cex.main = 1,
  font.main=1, legend = TRUE, d.legend = 1, nlab.xaxis = 5, nlab.yaxis = 5, 
  minL.axis = 3, las = 1, border = FALSE,oma = c(5,4,1,0)+0.1, tcl = -0.3,
  outline=FALSE, color="NULL", range = 1.5, width = NULL, varwidth = FALSE,
  plot=TRUE, log = "", pars = list(boxwex = 0.8, staplewex=0.5, outwex=0.5),
  cex.boxplot=1.5, family="serif", line=1, file1 = "List of species.txt",
  file2 = "Environmental variables.txt", append = FALSE, quote = TRUE,
  sep = " ", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE,
  qmethod = c("escape", "double"), fileEncoding = "")
}
\arguments{
  \item{data}{A CSV file obtained from ModestR with data of the presence of
   the species and the mean environmental variables.}
  \item{pixs}{Scatter plot. Pixelsize in mm.}
  \item{d.main}{Scatter plot. Vertical distance between upper border of
   scatter plots and the title line in multiples of title height.}
  \item{xlab}{Scatter plot. Label for x-axis.}
  \item{ylab}{Scatter plot. Label for y-axis.}
  \item{cex.lab}{Scatter plot. Magnification used for text in axis labels
   relative to the current setting of cex.}
  \item{font.lab}{Scatter plot. The font to be used for x and y labels.}
  \item{main}{Scatter plot. Title of the plot.}
  \item{zmax}{Scatter plot. Maximum number of counts per pixel in the plot.
   When NULL, the density in the scatter plot is encoded from 0 to maximum
   number of counts per pixel observed. zmax must be equal or larger than
   maximum number of counts found. The maximum number of counts per pixel
   is delivered by the return value.}
  \item{ztransf}{Scatter plot. Function to transform the number of counts
   per pixel, which will be mapped by the function in colramp to well defined
   colors. The user has to make sure that the transformed density lies in the
   range [0,zmax], where zmax is any positive number (>=2). For examples see
   ipairs and ilagplot of the package IDPmisc.}
  \item{colramp}{Scatter plot. Color ramp to encode the number of counts
   within a pixel.}
  \item{cex}{Scatter plot. Magnification of text relative to default.}
  \item{cex.main}{Scatter plot. Magnification used for title relative to
   the current setting of cex.}
  \item{font.main}{Scatter plot. The font to be used for plot main titles.}
  \item{legend}{Scatter plot. Logical. When FALSE, no legend is plotted and
   space is saved in figure region.}
  \item{d.legend}{Scatter plot. Horizontal distance between right border of
   scatter plot and legend in multiples of title height.}
  \item{nlab.xaxis}{Scatter plot. Approximate number of labels on x-axes.}
  \item{nlab.yaxis}{Scatter plot. Approximate number of labels on y-axes.}
  \item{minL.axis}{Scatter plot. The minimum length of the abbreviations of
   factor levels, used to label the axes ticks.}
  \item{las}{Scatter plot. Orientation of labels on axes.}
  \item{border}{Scatter plot. Logical. When TRUE, a border is drawn around
   the individual colors in the legend.}
  \item{oma}{Scatter plot. A vector of the form c(bottom, left, top, right)
   giving the size of the outer margins in lines of text.}
  \item{tcl}{Scatter plot. The length of tick marks as a fraction of the
   height of a line of text. The default value is -0.5; setting tcl = NA
   sets tck = -0.01 which is S' default.}
  \item{outline}{Boxplot. If outline is not true, the outliers are not drawn
   (as points whereas S+ uses lines).}
  \item{color}{Boxplot. If col is non-null it is assumed to contain colors
   to be used to colour the bodies of the box plots.}
  \item{range}{Boxplot. This determines how far the plot whiskers extend out
   from the box. If range is positive, the whiskers extend to the most
   extreme data point which is no more than range times the interquartile
   range from the box. A value of zero causes the whiskers to extend to the
   data extremes.}
  \item{width}{Boxplot. A vector giving the relative widths of the boxes
   making up the plot.}
  \item{varwidth}{Boxplot. If varwidth is TRUE, the boxes are drawn with
   widths proportional to the square-roots of the number of observations
   in the groups.}
  \item{plot}{Boxplot. If TRUE (the default) then a boxplot is produced.
   If not, the summaries which the boxplots are based on are returned.}
  \item{log}{Boxplot. Character indicating if x or y or both coordinates
   should be plotted in log scale.}
  \item{pars}{Boxplot. A list of (potentially many) more graphical
   parameters, e.g., boxwex or outpch; these are passed to bxp (if plot
   is true); for details, see there.}
  \item{cex.boxplot}{Boxplot. Magnification used for axis annotation.}  
  \item{line}{mtext. On which margin line, starting at 0 counting outwards.}
  \item{family}{The name of a font family for drawing text.}
  \item{file1}{Text file. A character string naming the file of the list of
   species.}
  \item{file2}{Text file. A character string naming the file with the summary
   of the environmental variables.}
  \item{append}{Text file. Logical. Only relevant if file is a character
   string. If TRUE, the output is appended to the file. If FALSE, any
   existing file of the name is destroyed.}
  \item{quote}{Text file.  Logical value (TRUE or FALSE) or a numeric vector.
   If TRUE, any character or factor columns will be surrounded by double
   quotes. If a numeric vector, its elements are taken as the indices of
   columns to quote. In both cases, row and column names are quoted if they
   are written. If FALSE, nothing is quoted.}
  \item{sep}{Text file. The field separator string. Values within each row
   of x are separated by this string.}
  \item{na}{Text file. The string to use for missing values in the data.}
  \item{dec}{Text file. The string to use for decimal points in numeric or
   complex columns: must be a single character.}
  \item{row.names}{Text file. Either a logical value indicating whether the
   row names of x are to be written along with x, or a character vector of
   row names to be written.}
  \item{col.names}{Text file. Either a logical value indicating whether the
   column names of x are to be written along with x, or a character vector
   of column names to be written. See the section on CSV files for the
   meaning of col.names = NA.}
  \item{qmethod}{Text file. A character string specifying how to deal with
   embedded double quote characters when quoting strings. Must be one of
   "escape" (default for write.table), in which case the quote character is
   escaped in C style by a backslash, or "double" (default for write.csv
   and write.csv2), in which case it is doubled. You can specify just the
   initial letter.}
  \item{fileEncoding}{Text file. Character string: if non-empty declares
   the encoding to be used on a file (not a connection) so the character
   data can be re-encoded as they are written. See file.}
}
\details{The first plot shows the polar coordinates of the cells selected by
the user (in the example 1 degree x 1 degree) using the environmental variables
selected by the user in the CSV file obtained from ModestR (in the example
BIO1 BIO12 and altitude). In this first plot, a more intense color of the
square indicates a higher number of species in the cell. In this plot it is
necessary to click four times with the mouse to select one o several cells.
The second plot shows a boxplot with the median and range of the environmental
variables in each cell selected with the mouse.
}
\value{A list of the species present in the cells is selected by the user with
the mouse and a summary of the environmental variables are saved in two txt files.}
\examples{

data(Carnivores)
niche(data=Carnivores,cex.boxplot=1.7)

#Remove the data set
rm(Carnivores)

}
\keyword{niche}
