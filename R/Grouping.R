addPartition <- function(tree = NULL, grouping, applyTo = NULL){
    require(data.tree)
    if (is.null(tree)){
        root <- Node$new("Partition Tree")
        root$AddChild(grouping)
        return (root)
    }

    if (is.null(applyTo)){
        tree$Get(addPartitionToLeaf, value = grouping, filterFun = isLeaf)
    }else if (applyTo=="root"){
        tree$AddChild(grouping)
    }
    tree

}

listPartitions <- function(tree){
    operations <- as.vector(tree$Get('pathString', traversal = "level"))
    fix <- sapply(operations, function(string){gsub("/", " -> ", string)})
    return(as.vector(fix))
}



# Internal Functions

addPartitionToLeaf <- function(node, value = NULL) {
    node$AddChild(value)
}

getPartitions <- function(tree){
    return(as.vector(tree$Get('pathString', traversal = "level")))
}
