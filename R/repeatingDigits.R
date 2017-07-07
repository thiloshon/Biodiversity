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

    #l1 <- which(latRepeat==T)
    #print(l1)


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

    # sapply(l1, function(counter){
    #     lat <- as.character.numeric_version(GBIF_Data[counter,c("decimalLatitude")])
    #
    #     list = as.vector(strsplit(lat, ""))
    #     table <- as.data.table(list)
    #     frameCount <- table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]
    #
    #     GBIF_Data[counter,]$latRepeatCount <- max(frameCount$count)
    #     print(GBIF_Data[counter,]$latRepeatCount)
    # })
    # print(sumFac(GBIF_Data$latRepeatCount))

    # l1 <- which(longRepeat==T)
    # GBIF_Data$longRepeatCount <- 0
    #
    # sapply(l1, function(counter){
    #     long <- as.character.numeric_version(GBIF_Data[counter,c("decimalLongitude")])
    #     list = as.vector(strsplit(long, ""))
    #     table <- as.data.table(list)
    #     frameCount <- table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]
    #     GBIF_Data[counter,]$longRepeatCount <- max(frameCount$count)
    # })

    print(Sys.time() - t)
    return(GBIF_Data)

    # GBIF_Data$latRepeatCount <- sapply(GBIF_Data$decimalLatitude, function(lat){
    #     lat <- as.character.numeric_version(lat)
    #     list = as.vector(strsplit(lat, ""))
    #     table <- as.data.table(list)
    #     frameCount <- table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]
    #     max(frameCount$count)
    # })

    #xvec <- unlist(strsplit(x, ""))
    #rmword <- grepl("(\\w)\\1{2, }", x)
    #return(paste(xvec[!rmword], collapse = " "))
    #k<- grepl("(\\w)\\1{2, }", xvec)
    #print(x)
    #print(rmword)
    #print("Done")
    #return(length(xvec[rmword]))
    #return(rmword)
}
