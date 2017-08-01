groups <- addPartition(grouping = "taxonClass")
addPartition(groups, grouping = "eventClass", applyTo = "root")
addPartition(groups, grouping = "locationClass", applyTo = "root")

addPartition(groups, grouping = "greaterThan50")

listPartitions(groups, onlyLeaf = TRUE)

groups2 <- addPartition(grouping = "all")

print(groups3)

addPartition(groups2, grouping = "primary3")


groups <- addPartition(grouping = "column", column = "scientificName~", data = australianMammals)

groups3 <- addPartition(grouping = "greaterThan50")
addPartition(groups3, grouping = "LessThan 50", applyTo = "root")
addPartition(groups3, "primary3", applyTo = "root")
addPartition(groups3, "primary3")
addPartition(groups3, grouping = "smaller 50")

results <- applyPartitions(groups, australianMammals)

acme <- Node$new("Partition Tree")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")


print(acme)

acme$Get('pathString', traversal = "pre-order")

g <- lol$Get('pathString', traversal = "pre-order")


acme <- Node$new("Acme Inc.")
acme$AddChild("Accounting")
acme$accounting$AddChild("New Software")
acme$accounting$AddChild("New Accounting Standards")
acme$AddChild("Research")
acme$research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")
