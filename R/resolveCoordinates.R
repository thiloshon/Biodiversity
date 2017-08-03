resolveCoordinates <- function(issues) {
    issues$projectedLatitude <- NA
    issues$projectedLongitude <- NA
    issues$coordinatesProjected <- 0
    
    localities <- names(sort(table(issues$locality), decreasing = T))
    # unique(issues$locality)
    
    
    
    
    stopwords = c("CAPTIVE", "BRED", "Captive", "Bred", "captive", "bred", "-", "Locality Unknown", "NA", "BETWEEN")
    
    require("tm")
    
    localitiesClean <- removeWords(localities, stopwords)
    localitiesClean[nchar(localitiesClean) > 3] <- paste(localitiesClean[nchar(localitiesClean) > 3], "Australia", sep = ", ")
    print(length(localities))
    
    for (j in 1:400) {
        # length(localities)
        
        
        geoCode <- suppressMessages(geocode(localitiesClean[j]))
        print(paste(j, "is", localitiesClean[j], geoCode$lat, geoCode$lon))
        
        if (!is.na(geoCode)) {
            print(paste("changing records:", dim(issues[issues$locality == localities[j], ][1])))
            
            issues[issues$locality == localities[j], ]$projectedLatitude <- geoCode$lat
            issues[issues$locality == localities[j], ]$projectedLongitude <- geoCode$lon
            issues$coordinatesProjected <- 1
        }
        print(j)
    }
    
    issues
    
    
}
