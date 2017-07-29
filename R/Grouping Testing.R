lol <- addPartition(grouping = "spatial")
addPartition(lol, grouping = "Temporal",applyTo = "root")
addPartition(lol, grouping = "Taxonomic",applyTo = "root")

addPartition(lol, grouping = "GreaterThan 50")


print(lol)
print(lol, "cost")



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

