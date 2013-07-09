pkgname <- "EnvNicheR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('EnvNicheR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("niche")
### * niche

flush(stderr()); flush(stdout())

### Name: niche
### Title: Environmental niche
### Aliases: niche
### Keywords: niche

### ** Examples


data(Carnivores)
niche(data=Carnivores,cex.boxplot=1.7)

#Remove the data set
rm(Carnivores)




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
