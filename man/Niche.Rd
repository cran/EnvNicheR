\author{Cástor Guisande González, Universidad de Vigo, Spain.}
\name{Niche}
\alias{Niche}
\title{Niche}
\description{
This function allows to show a plot overlying the niche of multiple species, so it is possible to determine the niche conditions which favor a higher species richness and, to create boxplots with the range of environmental variables and list of species in an area of the niche selected by the user.
}
\usage{
Niche(data, variables, Level="NULL", Taxon="NULL", cor=TRUE, d.main=0.5,
xlab="Polar coordinate X in pixels",ylab="Polar coordinate Y in pixels",
cex.labS=1.5, font.lab=1, main="", colramp = IDPcolorRamp, cex.main = 2,
font.main=2, nlab.xaxis = 5, nlab.yaxis = 5, minL.axis = 3, las = 1,
border = FALSE, tcl = -0.3, boxplot=TRUE, outline=FALSE, color="NULL",
range = 1.5, width = NULL, varwidth = FALSE, plot = TRUE, 
pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5), cex.boxplot=1.5,
cex.labB=1.5, namesB, family="serif", line=1, file1 = "List of species.csv",
file2 = "Environmental variables.csv", file3 = "Polar coordinates.csv",
na = "NA", dec = ",", row.names = FALSE, fileEncoding = "")
}
\arguments{
  \item{data}{A CSV file obtained from ModestR (García-Roselló et al., 2013) with data which show the presence of the species and abiotic and/or biotic factors.}
  \item{variables}{Selection of the variables for the estimation of the niche.}
  \item{Level}{Taxonomic level to be selected, i.e., Class, Family, Order, or Genus.}
  \item{Taxon}{Name of the taxon or taxa selected within the level, i.e., name of the Order, Family, etc. Can be a vector, so several taxa.}
  \item{cor}{If TRUE the variables are ordered according to the correlation between them. Therefore, the next variable to another variable is the one that has a greater positive correlation.}
  \item{d.main}{Scatter plot. Vertical distance between upper border of scatter plots and the title line in multiples of title height.}
  \item{xlab}{Scatter plot. Label for x-axis.}
  \item{ylab}{Scatter plot. Label for y-axis.}
  \item{cex.labS}{Scatter plot. Magnification used for text in axis labels
relative to the current setting of cex.}
  \item{font.lab}{Scatter plot. The font to be used for x and y labels.}
  \item{main}{Scatter plot. Title of the plot.}
  \item{colramp}{Scatter plot. Color ramp to encode the number of counts within a pixel.}
  \item{cex.main}{Scatter plot. Magnification used for title relative to the current setting of cex.}
  \item{font.main}{Scatter plot. The font to be used for plot main titles.}
  \item{nlab.xaxis}{Scatter plot. Approximate number of labels on x-axes.}
  \item{nlab.yaxis}{Scatter plot. Approximate number of labels on y-axes.}
  \item{minL.axis}{Scatter plot. The minimum length of the abbreviations of factor levels, used to label the axes ticks.}
  \item{las}{Scatter plot. Orientation of labels on axes.}
  \item{border}{Scatter plot. Logical. When TRUE, a border is drawn around the individual colors in the legend.}
  \item{tcl}{Scatter plot. The length of tick marks as a fraction of the height of a line of text. The default value is -0.5; setting tcl = NA sets tck = -0.01 which is S' default.}
  \item{boxplot}{If TRUE (the default) then a boxplot with the range of environmental variables in an area of the niche selected by the user is produced.}
  \item{outline}{Boxplot. If outline is not true, the outliers are not drawn (as points whereas S+ uses lines).}
  \item{color}{Boxplot. If col is non-null it is assumed to contain colors to be used to colour the bodies of the box plots.}
  \item{range}{Boxplot. This determines how far the plot whiskers extend out from the box. If the range is positive, the whiskers extend to the most extreme data point which is no more than range times the interquartile range from the box. A value of zero causes the whiskers to extend to the data extremes.}
  \item{width}{Boxplot. A vector giving the relative widths of the boxes making up the plot.}
  \item{varwidth}{Boxplot. If varwidth is TRUE, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups.}
  \item{plot}{Boxplot. If TRUE (the default) then a boxplot is produced. If not, the summaries which the boxplots are based on are returned.}
  \item{pars}{Boxplot. A list of (potentially many) more graphical
parameters, e.g., boxwex or outpch; these are passed to bxp (if plot is true).}
  \item{cex.boxplot}{Boxplot. Magnification used for axis annotation.}
  \item{cex.labB}{Boxplot. Magnification used for group labels which will be printed under each boxplot.}
  \item{namesB}{Boxplot. Group labels which will be printed under each boxplot. It can be a character vector.}
  \item{family}{The name of a font family for drawing text.}
  \item{line}{mtext. On which margin line, starting at 0 counting outwards.}
  \item{file1}{CSV file. A character string naming the file of the list of species.}
  \item{file2}{CSV file. A character string naming the file with the summary of the environmental variables.}
  \item{file3}{CSV file. A character string naming the file with the summary of the polar coordinates.}
  \item{na}{CSV files. The string to use for missing values in the data.}
  \item{dec}{CSV files. The string to use for decimal points in numeric or complex columns: must be a single character.}
  \item{row.names}{CSV files. Either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.}
  \item{fileEncoding}{CSV files. Character string: if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written.}
}
\details{The first plot shows the polar coordinates using the environmental variables selected by the user in the CSV file obtained from ModestR (in the example BIO1, BIO3, BIO4 and BIO12 and altitude). In this first plot, a darker color of the square indicates a higher number of species in the cell. In this plot it is necessary to click four times with the mouse to select one or several cells.

The second plot shows a boxplot with the median and range of the environmental variables and/or latitude and longitude, in the cells selected with the mouse.
}
\value{A list of the species present in the cells selected by the user with the mouse, a summary of the environmental variables and the polar coordinates are saved in three CSV files.}
\references{
García-Roselló, E., Guisande, C., González-Dacosta, J., Heine, J.,
Pelayo-Villamil, P., Manjarrés-Hernández, A., Vaamonde, A. & Granado-Lorencio, C. (2013) ModestR: a software tool for managing and analyzing species distribution map databases. \emph{Ecography}, 36, 1202-1207.

Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G. and Jarvis, A. (2005)Very high resolution interpolated climate surfaces for global land areas. \emph{International Journal of Climatology}, 25, 1965-1978.

IUCN (2012) The IUCN Red List of Threatened Species. Version 2012.2. <http://www.iucnredlist.org>. Downloaded on 17 October 2012.
}
\examples{

data(Carnivores)

Niche(data=Carnivores, variables= c("Altitude", "BIO1", "BIO3",
"BIO4", "BIO12"), Level="Genus", Taxon= c("Leopardus", "Puma", "Panthera"),
cex.boxplot=1.7)

#Remove the data set
rm(Carnivores)

}
\keyword{Niche}
