library(finch)

file <- simple_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091")

file <- system.file("examples", "example_simple.xml", package = "finch")
simple_read(file)

## DwC Reading with finch
file <- dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip")
fileWithReading <- dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip", read = TRUE)


# Timing for reading paths using dwca_read
ptm <- proc.time()
file <- dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip")
proc.time() - ptm
##
##   user  system elapsed
## 222.19    1.02  225.36


# Timing for reading files using dwca_read Zip
ptmReadZip <- proc.time()
fileWithReadZip <- dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip", read = TRUE)
proc.time() - ptmReadZip
##
##   user  system elapsed
## 217.83    0.87  220.04




# Timing for reading files using dwca_read directory
ptmReadDirectory <- proc.time()
fileWithReadDirectory <- dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091", read = TRUE)
proc.time() - ptmReadDirectory
##
##    user  system elapsed
## 213.17    0.14  214.06





















