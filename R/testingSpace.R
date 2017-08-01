library(finch)
library(rgbif)
library(spocc)

file <-
    simple_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091")

file <-
    system.file("examples", "example_simple.xml", package = "finch")
simple_read(file)

file <-
    read.csv("sources/GBIF Downloaded Files/CSV/0090369-160910150852091.csv")

## DwC Reading with finch
file <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip")
fileWithReading <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip",
              read = TRUE)


# Timing for reading paths using dwca_read
ptm <- proc.time()
file <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip")
proc.time() - ptm
##
##   user  system elapsed
## 222.19    1.02  225.36


# Timing for reading files using dwca_read Zip
ptmReadZip <- proc.time()
fileWithReadZip <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip",
              read = TRUE)
proc.time() - ptmReadZip
##
##   user  system elapsed
## 217.83    0.87  220.04


# Timing for reading files using dwca_read directory
ptmReadDirectory <- proc.time()
fileWithReadDirectory <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091",
              read = TRUE)
proc.time() - ptmReadDirectory
##
##    user  system elapsed
## 213.17    0.14  214.06



# doing a ML using mammals data
country = "AU"
hasCoordinate = TRUE
limit = 5000
classKey = 121
# Required Libraries
library(rgbif)
library(plyr)
library(rgeospatialquality)

# Retrieving data
rawData <-
    occ_data(classKey = classKey,
             country = country,
             limit = limit)
#hasCoordinate = hasCoordinate,)




library("taxize")
get_tsn(searchterm = "Quercus b")



ptmReadZip <- proc.time()
gnr_resolve(fileWithReadDirectory$data$occurrence.txt$scientificName,
            best_match_only = TRUE)
proc.time() - ptmReadZip

#    user  system elapsed
#   10.76    0.14  107.22



ptmReadZip <- proc.time()
gnr_resolve(levels(t), best_match_only = TRUE)
proc.time() - ptmReadZip

mynames <- c("Felis californica",
             "Felis concolor",
             "Puma concolor",
             "Felis californicus")

mynames <- c(
    "Felis californica May, 1896",
    "Felis concolor subsp. azteca Merriam, 1901",
    "Puma concolor subsp. mayensis (Nelson & Goldman, 1929)"
)


mynames <-
    c(
        "Puma concolor subsp. couguar (Kerr, 1792)",
        "Felis californica May, 1896",
        "Felis californicus subsp. fischeri"
    )

tsn <- get_tsn(mynames)


lapply(tsn, itis_acceptname)

unique(sub("^(\\S*\\s+\\S+).*", "\\1", pumaConcolorData$scientificName))


australian1999Data <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0096066-160910150852091.zip",
              read = TRUE)


australianMammals <-
    dwca_read("sources/GBIF Downloaded Files/DwC/0096297-160910150852091.zip",
              read = TRUE)


australianMammalsData <-
    australianMammals$data$occurrence.txt[sample(1:nrow(australianMammals$data$occurrence.txt),
                                                 500000,
                                                 replace = FALSE),]

otherRankData <-
    australianMammalsData[australianMammalsData$taxonRank != "SPECIES" &
                              australianMammalsData$taxonRank != "SUBSPECIES",]

sub("^(\\upper\\+lower\\blank\\lower)",
    otherRankData$scientificName)


get_ids("Puma concolor")

res <- occ_names(query = 'Puma concolor', from = 'gbif')

bold_search(name = "Puma concolor")
downstream("Puma", downto = 'subspecies', db = 'gbif')

downstream("Trichosurus Lesson", downto = 'subspecies', db = 'gbif')




gnr_resolve("Trichosurus Lesson")

one <- gnr_resolve("Trichosurus Lesson")
one[order(one$matched_name), ]

namelookup$data$scientificName
#[1] "Trichosurus Lesson, 1828"   "Trichosurus Lesson, 1828"   "Trichosurus Lesson, 1828"   "Trichosurus Lesson, 1828"   "Trichosurus Lesson 1828"    "Trichosurus Lesson, 1828"   "Trichosurus Lesson 1828"
#[8] "Trichosurus Lesson, 1828"   "Trichosurus Lesson, 1828"   "Trichosurus (Lesson, 1828)"






#two <- gnr_resolve("Trichosurus Lesson", with_context = T)

#three <- gnr_resolve("Trichosurus Lesson", canonical = TRUE)

four <- gnr_resolve("Trichosurus Lesson", highestscore = TRUE)

#four <- gnr_resolve("Trichosurus Lesson", best_match_only = TRUE)

pageid <-
    eol_search("Trichosurus Lesson")$id[1]  # first need to search for the taxon's page on EOL
out <-
    eol_pages(taxonconceptID = pageid)  # then we need to get the taxon ID used by EOL

summary(as.factor(otherRankData$scientificName))

possibleSpecies <-
    unique(namelookup$data[namelookup$data$rank == "SPECIES", ]$scientificName)
possibleSpecies <- data.frame(possibleSpecies)
possibleSpecies <- possibleSpecies[-c(2), ]
dummy <- function(df) {

}


for (val in possibleSpecies$possibleSpecies) {
    ans <- name_backbone(name = val) ## 'Trichosurus johnstonii'
    key <- ans$speciesKey
    key
    print(occ_count(taxonKey = key))
}

occspocc <-
    occ(
        query = "Trichosurus hamiltonensis",
        from = c("bison", "inat", "ebird", "ecoengine" , "vertnet"),
        limit = 10000
    )
occspocc
occspoccDF <- occ2df(occspocc)
range(as.numeric(occspoccDF$latitude) , na.rm = TRUE)
range(as.numeric(occspoccDF$longitude) , na.rm = TRUE)

range(otherRankData[otherRankData$scientificName == "Trichosurus Lesson, 1828", ]$decimalLatitude, na.rm = TRUE)

t <- summary(as.factor(otherRankData$taxonRank))
t

ans <-
    list("META" = list(
        "The total records submitted: 500000",
        head(otherRankData$scientificName)
    ))
ans
ans$META[[2]][7] <- "dump"
ans$META[[2]]

dad <- load("answer.RData")

df <- as.data.frame(answer)

df
do.call("cbind", df[1])


as.data.frame(do.call("cbind", df[1]))

tbl2 <- unlist(answer)
attributes(tbl2) <- attributes(answer)
DF <- as.data.frame(tbl2)


sapply(answer, function(a) {
    (a)
})


occ <-
    occ(query = "Vulpes vulpes",
        from = c("bison", "inat", "ebird", "ecoengine" , "vertnet"))

occCoord <-
    occ(
        query = "Vulpes vulpes",
        from = c("bison", "inat", "ebird", "ecoengine" , "vertnet"),
        has_coords = T
    )


t <- Sys.time()
occCoord <-
    occ(
        query = "Vulpes vulpes",
        from = c("vertnet", "bison", "ecoengine"),
        has_coords = T
    )
print(Sys.time() - t)

t <- Sys.time()
occCoord <-
    occ(
        query = "Tintinnopsis macropus",
        from = c("vertnet", "bison", "ecoengine"),
        limit = 1
    )
occCoord$vertnet$meta$found
print(Sys.time() - t)


save(otherRankData, file = "otherRankData.RData")


occ <-
    occ("Macropus robustus", from = c("vertnet", "bison", "ecoengine"))
occ2d <- occ2df(occ)


newmap <- getMap(resolution = "high")
plot(
    newmap,
    xlim = c(-180, 180),
    ylim = c(-90, 90),
    asp = 1
)

points(occ2d$longitude,
       occ2d$latitude,
       col = "red",
       cex = .9)

class(r2$longitude)
r2$latitude <- as.numeric(r2$latitude)

points(t$decimalLongitude,
       t$decimalLatitude,
       col = "blue",
       cex = .9)

map_ggmap(t, lon = "decimalLongitude", lat = "decimalLatitude")

jpeg(file = "C://Users//Thiloshon//desktop//plot.jpeg")

points(t$decimalLongitude,
       t$decimalLatitude,
       col = "blue",
       cex = .6)


dev.print(png, 'filename.jpg', width = 1500, height = 1500)

dev.off()

if (class(file$data) == "dwca_gbif") {
    print("yes")
} else{
    print("no")
}

list[1:10]

lapply(list[1:10], length)

lapply(inspect2$resolveResults[1:10], length)

lapply(resolveOutput$resolveResults[1:10], dim)

inspect2$resolveResults[1:10, ]



