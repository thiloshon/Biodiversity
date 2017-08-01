addPartition <- function(tree = NULL,  grouping, applyTo = "leaf", column = NULL, data = NULL) {
    require(data.tree)
    if (is.null(tree)) {
        tree <- Node$new("Partition Tree")
    }

    if (grouping == "all") {
        if (applyTo == "leaf") {
            for (x in 1:length(classes)) {
                if (x == 1) {
                    treeNew <- Node$new("Partition Tree")
                }
                treeNew <-
                    addPartition(treeNew,
                                 grouping =  classes[x],
                                 applyTo = "root")
            }
            tree$Do(addPartitionToLeaf,
                     value = treeNew,
                     filterFun = isLeaf)
        }else if (applyTo == "root"){
            for (x in 1:length(classes)) {
                addPartition(tree,
                                 grouping =  classes[x],
                                 applyTo = "root")
            }
        }

        return(tree)
    }

    if (grouping == "primary3") {
        if (applyTo == "leaf") {
            for (x in 1:3) {
                if (x == 1) {
                    treeNew <- Node$new("Partition Tree")
                }
                treeNew <-
                    addPartition(treeNew,
                                 grouping =  classes[x],
                                 applyTo = "root")
            }
            tree$Do(addPartitionToLeaf,
                    value = treeNew,
                    filterFun = isLeaf)
        }else if (applyTo == "root"){
            for (x in 1:3) {
                addPartition(tree,
                             grouping =  classes[x],
                             applyTo = "root")
            }
        }

        return(tree)
    }

    if (grepl("greaterThan", grouping)) {
        #print("hi")
        value <- as.integer(gsub("greaterThan","",grouping))
        if (applyTo == "leaf") {
            treeNew <- Node$new("Partition Tree")
            treeNew <-
                addPartition(treeNew,
                             grouping =  paste("GreaterThan",value,sep = ""),
                             applyTo = "root")
            #print(treeNew)
            treeNew <-
                addPartition(treeNew,
                             grouping =  paste("LessThan",value,sep = ""),
                             applyTo = "root")
            #print(treeNew)
            tree$Do(addPartitionToLeaf,
                    value = treeNew,
                    filterFun = isLeaf)
        } else if (applyTo == "root") {
            addPartition(tree,
                         grouping =  paste("GreaterThan",value,sep = ""),
                         applyTo = "root")
            addPartition(tree,
                         grouping =  paste("LessThan",value,sep = ""),
                         applyTo = "root")
        }
        return(tree)
    }

    if (grouping=="column"){
        if (length(strsplit(column,"~")[[1]])==2){
            field <- strsplit(column,"~")[[1]][1]
            value <- strsplit(column,"~")[[1]][2]


            if (applyTo == "leaf") {
                treeNew <- Node$new("Partition Tree")
                treeNew <-
                    addPartition(treeNew,
                                 grouping =  paste(field,"=",value, sep = ""),
                                 applyTo = "root")
                #print(treeNew)
                treeNew <-
                    addPartition(treeNew,
                                 grouping =  paste(field, "!=",value,sep = ""),
                                 applyTo = "root")
                #print(treeNew)
                tree$Do(addPartitionToLeaf,
                        value = treeNew,
                        filterFun = isLeaf)
            } else if (applyTo == "root") {
                addPartition(tree,
                             grouping =  paste(field,"=",value, sep = ""),
                             applyTo = "root")
                addPartition(tree,
                             grouping =  paste(field, "!=",value,sep = ""),
                             applyTo = "root")
            }

        }else{
            column <- strsplit(column,"~")[[1]]
            #print(column)
            values <- unique(data[,column])
            if (applyTo == "leaf") {
                for (x in 1:length(values)) {
                    #print(x)
                    if (x == 1) {
                        treeNew <- Node$new("Partition Tree")
                    }
                    treeNew <-
                        addPartition(treeNew,
                                     grouping =  values[x],
                                     applyTo = "root")
                }
                tree$Do(addPartitionToLeaf,
                        value = treeNew,
                        filterFun = isLeaf)
            }else if (applyTo == "root"){
                for (x in 1: length(values)) {
                    addPartition(tree,
                                 grouping =  values[x],
                                 applyTo = "root")
                }
            }

        }

        return(tree)
    }

    if (applyTo == "leaf") {
        tree$Get(addPartitionToLeaf,
                 value = grouping,
                 filterFun = isLeaf)
    } else if (applyTo == "root") {
        tree$AddChild(grouping)
    }
    tree
}

listPartitions <- function(tree, onlyLeaf = FALSE) {
    if (onlyLeaf){
        operations <- as.vector(tree$Get('pathString', traversal = "level", filterFun = isLeaf))
    }else {
        operations <- as.vector(tree$Get('pathString', traversal = "level"))
    }

    fix <-
        sapply(operations, function(string) {
            gsub("/", " -> ", string)
        })
    return(as.vector(fix))
}

applyPartitions <- function(tree, data, return = "list") {
    require(dplyr)
    operations <- getPartitions(tree)
    results <- lapply(operations, function(set) {
        groups <- unlist(strsplit(set, "/"))
        for (counter in 1:length(groups)) {
            # ---------- class groupings | Column ------------ #
            if (groups[counter] %in% classes) {
                logic <- get(groups[counter]) %in% names(data)
                data <- data[, c(get(groups[counter])[logic])]
            }
            # ---------- End of class groupings ------------ #

            # ---------- all grouping | Column ------------ #
            if (groups[counter] == "all") {
                logic <- get(groups[counter]) %in% names(data)
                data <- data[, c(get(groups[counter])[logic])]
            }



            if (grepl("GreaterThan", groups[counter])) {
                value <- as.integer(gsub("GreaterThan","",groups[counter]))
                log1 <- which(colMeans(is.na(data)) > value/10)
                log2 <- which(colMeans(data!="") > value/10)

                log <- any(log1,log2)

                return(data[-log,])
            }

            if (grepl("LessThan", groups[counter])) {
                value <- as.integer(gsub("LessThan","",groups[counter]))
                log1 <- which(colMeans(is.na(data)) > value/10)
                log2 <- which(colMeans(data!="") > value/10)

                log <- any(log1,log2)

                return(data[log,])
            }

        }

        return(data)

    })

    names(results) <- operations

    if (return == "dataframe"){
        g <- bind_cols(results[2:length(results)])
        temp <- g[, !duplicated(colnames(g))]
        return(temp)
    }

    return(results)

}



# ------------------------ Internal Functions ------------------------------#

addPartitionToLeaf <- function(node, value = NULL) {
    if ((class(value) == "character")[1]) {
        node$AddChild(value)
    }
    else {
        for(x in 1 : length(value$children)){
            node$AddChild(value$children[[x]]$name)
        }

    }

}

getPartitions <- function(tree) {
    return(as.vector(tree$Get('pathString', traversal = "level")))
}

# ------------------------ Grouping Classes ------------------------------#

classes <- c(
    "taxonClass",
    'locationClass',
    'eventClass',
    'occurenceClass',
    "recordlevelTermsClass",
    'geologicalContextClass',
    'identificationClass',
    'resourceRelationshipClass',
    'measurementOrFactClass'
)


taxonClass <- c(
    "taxonID",
    "scientificNameID",
    "acceptedNameUsageID",
    "parentNameUsageID",
    "originalNameUsageID",
    "nameAccordingToID",
    "namePublishedInID",
    "taxonConceptID",
    "scientificName",
    "acceptedNameUsage",
    "parentNameUsage",
    "originalNameUsage",
    "nameAccordingTo",
    "namePublishedIn",
    "namePublishedInYear",
    "higherClassification",
    "kingdom",
    "phylum",
    "class",
    "order",
    'family',
    'genus',
    'subgenus',
    'specificEpithet',
    'infraspecificEpithet',
    'taxonRank',
    'verbatimTaxonRank',
    'scientificNameAuthorship',
    'vernacularName',
    'nomenclaturalCode'
)

recordlevelTermsClass <- c(
    'institutionID',
    'collectionID',
    'datasetID',
    'institutionCode',
    'collectionCode' ,
    'datasetName',
    'ownerInstitutionCode',
    'basisOfRecord',
    'informationWithheld',
    'dataGeneralizations',
    'dynamicProperties'
)

occurenceClass <- c(
    'occurrenceID' ,
    'catalogNumber' ,
    'occurrenceDetails' ,
    'occurrenceRemarks' ,
    'recordNumber' ,
    'recordedBy' ,
    'individualID' ,
    'individualCount' ,
    'sex' ,
    'lifeStage' ,
    'reproductiveCondition' ,
    'associatedTaxa',
    'behavior',
    'establishmentMeans',
    'occurrenceStatus',
    'preparations',
    'disposition',
    'otherCatalogNumbers',
    'previousIdentifications',
    'associatedMedia',
    'associatedReferences',
    'associatedOccurrences',
    'associatedSequences'
)

eventClass <- c(
    'eventID' ,
    'samplingProtocol' ,
    'samplingEffort' ,
    'eventDate' ,
    'eventTime' ,
    'startDayOfYear' ,
    'endDayOfYear' ,
    'year',
    'month',
    'day',
    'verbatimEventDate',
    'habitat',
    'fieldNumber',
    'fieldNotes',
    'eventRemarks'
)

locationClass <- c(
    'locationID' ,
    'higherGeographyID' ,
    'higherGeography' ,
    'continent' ,
    'waterBody' ,
    'islandGroup' ,
    'island' ,
    'country' ,
    'countryCode' ,
    'stateProvince' ,
    'county' ,
    'municipality' ,
    'locality' ,
    'verbatimLocality' ,
    'verbatimElevation' ,
    'minimumElevationInMeters' ,
    'maximumElevationInMeters' ,
    'verbatimDepth' ,
    'minimumDepthInMeters' ,
    'maximumDepthInMeters' ,
    'minimumDistanceAboveSurfaceInMeters' ,
    'verbatimLatitude' ,
    'verbatimLongitude',
    'verbatimCoordinateSystem',
    'verbatimSRS',
    'decimalLatitude',
    'decimalLongitude',
    'geodeticDatum',
    'coordinateUncertaintyInMeters',
    'coordinatePrecision',
    'pointRadiusSpatialFit',
    'footprintWKT',
    'footprintSRS',
    'footprintSpatialFit',
    'georeferencedBy',
    'georeferencedDate',
    'georeferenceProtocol',
    'georeferenceSources',
    'georeferenceVerificationStatus',
    'georeferenceRemarks',
    'maximumDistanceAboveSurfaceInMeters',
    'locationAccordingTo',
    'locationRemarks',
    'verbatimCoordinates'
)


geologicalContextClass <- c(
    'geologicalContextID' ,
    'earliestEonOrLowestEonothem' ,
    'latestEonOrHighestEonothem' ,
    'earliestEraOrLowestErathem' ,
    'latestEraOrHighestErathem' ,
    'earliestPeriodOrLowestSystem' ,
    'latestPeriodOrHighestSystem' ,
    'earliestEpochOrLowestSeries' ,
    'latestEpochOrHighestSeries' ,
    'earliestAgeOrLowestStage',
    'latestAgeOrHighestStage',
    'lowestBiostratigraphicZone',
    'highestBiostratigraphicZone',
    'lithostratigraphicTerms',
    'group',
    'formation',
    'member',
    'bed'
)

identificationClass <- c(
    'identificationID' ,
    'identifiedBy' ,
    'dateIdentified' ,
    'identificationReferences' ,
    'identificationVerificationStatus',
    'identificationRemarks',
    'identificationQualifier',
    'typeStatus'
)

resourceRelationshipClass <- c(
    'resourceRelationshipID' ,
    'resourceID' ,
    'relatedResourceID' ,
    'relationshipOfResource' ,
    'relationshipAccordingTo',
    'relationshipEstablishedDate',
    'relationshipRemarks',
    'relationshipAccordingTo'
)

measurementOrFactClass <- c(
    'measurementID' ,
    'measurementType' ,
    'measurementValue' ,
    'measurementAccuracy' ,
    'measurementUnit',
    'measurementDeterminedBy',
    'measurementMethod',
    'measurementRemarks'
)

