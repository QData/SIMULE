
pause <- function() {}

### load the simule library
library(simule)

pause <- function() {}

### load the cancer data
data(cancer)
X = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]), as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))

### run the wsimule
results = wsimule(X, 0.05, 1, W = matrix(1,26,26) , covType = "cov", TRUE)
results

pause()

### plot the estimated graphs by wsimule
plot.wsimule(results)


pause()

### plot the shared subgraph that is shared by all estimated graphs by wsimule
plot.wsimule(results, type="share")


pause()

### plot the estimated task-specific graph whose task index="sub" by wsimule
plot.wsimule(results, type="sub", subID=1)


pause()

### plot the estimated subgraphs that is about a specific node
plot.wsimule(results, type="neighbor", index=15)


### load the  example data (larger p)

data(exampleData)
exampleData

pause()

### run simule algorithm to estimate two sparse precision matrices from the exampleData

results = wsimule(X = exampleData , 0.05, 1, W = matrix(1,100,100), covType = "cov", TRUE)
results


### Output the top-10 hubs in each identified graph.

print("Output the top-10 hubs in each identified graph.")
hub = net.hubs(results$Graphs)
hub

pause()

### Output the degrees of nodes in each identified graph.

print("Output the degrees of nodes in each identified graph.")
degree = net.degree(results$Graphs)
degree

pause()

### Output the list of edges in each identified graph.

print("Output the list of edges in each identified graph.")
edges = net.edges(results$Graphs)
edges

pause()

### Output the list of edges in each identified graph.

print("Output the list of edges in each identified graph.")
edges = net.edges(results$Graphs)
edges

pause()

### Output the neighbors of 50th node in each identified graph.

print("Output the neighbors of 50th node in each identified graph.")
neighbors = net.neighbors(results$Graphs,index=50)
neighbors

pause()

### plot the estimated graphs by wsimule
plot.wsimule(results)


pause()

### plot the shared subgraph that is shared by all estimated graphs by wsimule
plot.wsimule(results, type="share")


pause()

### plot the estimated task-specific graph whose task index="sub" by wsimule
plot.wsimule(results, type="sub", subID=1)


pause()

### plot the estimated subgraphs that is about a specific node
plot.simule(results, type="neighbor", index=50)
