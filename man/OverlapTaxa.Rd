\author{Cástor Guisande González, Universidad de Vigo, Spain.}
\name{OverlapTaxa}
\encoding{latin1}
\alias{OverlapTaxa}
\title{Niche overlap among all species within a taxa}
\description{
Estimation of the niche overlap among all species within a taxa of a taxonomic level
selected by the user (order, family or genus), using the file Polar coordinates.CSV
obtained from the function \link{Niche}.
}

 
\usage{
OverlapTaxa(data, Level, digits =2, file1 = "Overlap among taxa.csv",
file2 = "Mean overlap among taxa.csv", na = "NA", dec = ",", row.names = FALSE,
fileEncoding = "")
}
\arguments{
  \item{data}{The file Polar coodinates.CSV obtained from the function Niche().}
  \item{Level}{Taxonomic level to be selected, i.e., Class, Order, Family,
or Genus}
  \item{digits}{Integer indicating the number of decimal places of the overlap
values.}
  \item{file1}{CSV file. A character string naming the file with the overllap
among species.}
  \item{file2}{CSV file. A character string naming the file with mean and 
standard deviation of the overlap for each taxonomic level selected by
the user.}
  \item{na}{CSV files. The string to use for missing values in the data.}
  \item{dec}{CSV files. The string to use for decimal points in numeric or
complex columns: must be a single character.}
  \item{row.names}{CSV files. Either a logical value indicating whether the
row names of x are to be written along with x, or a character vector of
row names to be written.}
  \item{fileEncoding}{CSV files. Character string: if non-empty declares
the encoding to be used on a file (not a connection) so the character
data can be re-encoded as they are written.}
}
\details{In the first CSV file this function estimates the niche overlap, using
the file Polar coordinates.CSV obtained from the function Niche(), among all
species within the taxonomic level selected by the user. In the second CSV file
the function estimates the mean overlap within the taxonomic level selected by
the user. If there are many species, e.g. over 5000, the process can take several
hours.
}
\value{Two CSV files with the overlap among species and the mean overlap among
the species for the taxonomic level selected by the user.}
\examples{

\dontrun{

data(Polar.coordinates)

OverlapTaxa(data=Polar.coordinates, Level="Genus")

#Remove the data set
rm(Polar.coordinates)

}
}
\keyword{Polar.coordinates}
