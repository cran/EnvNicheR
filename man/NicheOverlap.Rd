\author{Cástor Guisande González, Universidad de Vigo, Spain.}
\name{NicheOverlap}
\alias{NicheOverlap}
\title{Niche overlap between two taxa}
\description{
This function shows in a plot the niche overlap between two taxa, at any
level of the taxonomy (order, family, genus or species), using the file
Polar coordinates.CSV obtained from the function \link{Niche}.
}
\usage{
NicheOverlap(data, Level1, Taxon1, Level2=Level1, Taxon2,
colA=hsv(h=0,s=1,v=1,alpha=0.4), colB=hsv(h=0.7,s=1,v=1, alpha=0.4),
xlab="Polar coordinate X in pixel", ylab="Polar coordinate Y in pixels",
cex=1.57, cex.lab=1.5,font.lab=1, main="", cex.main = 2, font.main=2,
family="serif", digits =2, xlegend="topleft", ylegend=NULL, pch=15, bty="n",
text.font=3, cex.legend=1.2, ncol=1, x.intersp=1, y.intersp=1, legend=TRUE)
}
\arguments{
  \item{data}{The file Polar coodinates.CSV obtained from the function Niche().}
  \item{Level1}{Taxonomic level to be selected, i.e., Class, Order, Family,
or Genus, of the first taxon.}
  \item{Taxon1}{Name of the taxon of the first taxonomic level.}
  \item{Level2}{Taxonomic level to be selected, i.e., Class, Order, Family,
or Genus, of the second taxon. If missing the default value is Level1. }
  \item{Taxon2}{Name of the taxon of the second taxonomic level.}
  \item{colA}{Color of Taxon1.}
  \item{colB}{Color of Taxon2.}
  \item{xlab}{Label for x-axis.}
  \item{ylab}{Label for y-axis.}
  \item{cex}{Size of the symbols.This argument and the function omi() are
useful to adjust the overlap among symbols and to avoid white lines.}
  \item{cex.lab}{Magnification used for text in axis labels relative to the
current setting of cex.}
  \item{font.lab}{The font to be used for x and y labels.}
  \item{main}{Title of the plot.}
  \item{cex.main}{Magnification used for title relative to the current
setting of cex.}
  \item{font.main}{The font to be used for plot main titles.}
  \item{family}{The name of a font family for drawing text.}
  \item{digits}{Integer indicating the number of decimal places of the overlap
values.}
  \item{xlegend}{The x co-ordinates to be used to position the legend.}
  \item{ylegend}{The y co-ordinates to be used to position the legend.}
  \item{pch}{The plotting symbols appearing in the plot and in the legend.}
  \item{bty}{The type of box to be drawn around the legend. The allowed
values are "o" and "n" (the default).}
  \item{text.font}{The font used for the legend text, see text.}
  \item{cex.legend}{Character expansion factor relative to current par("cex") of the
legend.}
  \item{ncol}{The number of columns in which to set the legend items.}
  \item{x.intersp}{Character interspacing factor for horizontal (x) spacing of the
legend.}
  \item{y.intersp}{Character interspacing factor for vertical (y) spacing of the
legend.}
  \item{legend}{If TRUE the legend is shown.}
}
\details{The plot shows the niche overlap between two taxa using the file Polar
coordinates.CSV obtained from the function Niche(). The overlap is estimated
counting the overlapping pixels.
}
\value{A plot shows the values of niche overlap for both taxa and the overlapping
area.}
\examples{

data(Polar.coordinates)

NicheOverlap(Polar.coordinates, Level1="Species", Taxon1="Panthera onca",
Taxon2="Panthera leo", xlegend="topright")

#Remove the data set
rm(Polar.coordinates)

}
\keyword{NicheOverlap}
