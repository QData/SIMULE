
pause <- function() {}

### load the simule library
library(simule)

pause <- function() {}

### load the example data

data(exampleData)
exampleData

pause()

### run simule algorithm to estimate two sparse precision matrices from the exampleData

results = simule(X = exampleData , 0.05, 1, covType = "cov", TRUE)
results
plot.simule(results)

pause()

plot.simule(results, type="share")

pause()

plot.simule(results, type="sub", subID=1)

pause()

plot.simule(results, type="neighbor", index=50)

pause()

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

### plot the estimated graphs by simule
plot.simule(results)


pause()

### plot the shared subgraph that is shared by all estimated graphs by simule
plot.simule(results, type="share")


pause()

### plot the estimated task-specific graph whose task index="sub" by simule
plot.simule(results, type="sub", subID=1)


pause()

### plot the estimated subgraphs that is about a specific node
plot.simule(results, type="neighbor", index=50)
