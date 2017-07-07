coordinatesDecimalMismatch <- function(GBIF_DataFrame) {
    t <- Sys.time()
    lat <- sapply(GBIF_DataFrame$decimalLatitude, function(lat) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(lat)),
                         ".", fixed = TRUE)

        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    long <- sapply(GBIF_DataFrame$decimalLongitude, function(long) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(long)),
                         ".", fixed = TRUE)
        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })

    print(Sys.time() - t)

    GBIF_DataFrame$decimalPointDifference <- abs(lat - long)
    return(GBIF_DataFrame)
}


repeatingDigits <- function(GBIF_Data){
    t <- Sys.time()


    latRepeat <- sapply(GBIF_Data$decimalLatitude, function(lat){
        x <- as.character.numeric_version(lat)
        rmword <- grepl("(\\w)\\1{2, }", x)
        return(rmword)
    })

    longRepeat <- sapply(GBIF_Data$decimalLongitude, function(long){
        x <- as.character.numeric_version(long)
        rmword <- grepl("(\\w)\\1{2, }", x)
        return(rmword)
    })

    require(data.table)

    t1 <- sapply(1:dim(GBIF_Data)[1], function(counter){
        if(latRepeat[counter]){
            lat <- as.character.numeric_version(GBIF_Data[counter,c("decimalLatitude")])

            list = as.vector(strsplit(lat, ""))
            table <- as.data.table(list)
            frameCount <- table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]

            max(frameCount$count)
        }else{
            0
        }
    })

    GBIF_Data$latRepeatCount <- t1

    t2 <- sapply(1:dim(GBIF_Data)[1], function(counter){
        if(longRepeat[counter]){
            long <- as.character.numeric_version(GBIF_Data[counter,c("decimalLongitude")])

            list = as.vector(strsplit(long, ""))
            table <- as.data.table(list)
            frameCount <- table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]

            max(frameCount$count)
        }else{
            0
        }
    })

    GBIF_Data$longRepeatCount <- t2

    print(Sys.time() - t)
    return(GBIF_Data)


}
