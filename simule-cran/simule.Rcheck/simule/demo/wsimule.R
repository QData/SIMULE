library(simule)


### plotting window reset routine
graphics.off()
par(ask=F)
par(mfrow=c(1,1))


readline(prompt="Press [enter] to continue to cancer demo with 2 tasks (not v. pcr) and 26 features (26 cancer types) ")

### load cancer data (cancer)
data(cancer)

### create a list of cancer data (cancerlist)
cancerlist = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]),
                  as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))

### run wsimule
result = wsimule(cancerlist, 0.2, 1, W = matrix(1,26,26), covType = "cov", TRUE)
label = colnames(cancer[[1]])

graph = returngraph(result)


layout = layout_nicely(graph,dim=2)



### multiple plotting
readline(prompt="Press [enter] to display four plots showing all graphs, shared graph, task specific 1 and task specific 2")

par(mfrow=c(2,2))

{
  plot.simule(result, graphlabel = label, type="task", graphlayout = layout)

  plot.simule(result, graphlabel = label, type="share", graphlayout = layout)

  plot.simule(result, graphlabel = label, type="taskspecific", subID=1, graphlayout = layout)

  plot.simule(result, graphlabel = label, type="taskspecific", subID=2, graphlayout = layout)
}


readline(prompt="Press [enter] to continue to synthetic Gaussian data demo with 2 tasks and 20 features")


### load the example data
data(exampleData)

### run simule algorithm to estimate two sparse precision matrices from example data
result = wsimule(X = exampleData , lambda = 0.1, epsilon = 0.45, W = matrix(1,20,20), covType = "cov", TRUE)
graph = returngraph(result)


layout = layout_nicely(graph,dim=2)
label = NULL

### multiple display
readline(prompt="Press [enter] to view the four plots showing all graphs, shared graph, task 1 and task 2 specific graphs")
par(mfrow=c(2,2))

{
  plot.simule(result, graphlabel = label, type="task", graphlayout = layout)

  plot.simule(result, graphlabel = label, type="share", graphlayout = layout)

  plot.simule(result, graphlabel = label, type="taskspecific", subID=1, graphlayout = layout)

  plot.simule(result, graphlabel = label, type="taskspecific", subID=2, graphlayout = layout)
}


readline(prompt="Press [enter] to continue to NIPS word count demo with 2 tasks (before 2006 and after 2006) and 37 features (37 words)")

### load the nips word count data
data(nip_37_data)
label = colnames(nip_37_data[[1]])
result = wsimule(nip_37_data, lambda = 0.13, epsilon = 0.5, W = matrix(1,37,37) , covType = "kendall", parallel = TRUE)

graph = returngraph(result)


# fix plot layout
layout = layout_nicely(graph, dim = 2)

### multiple plotting
readline(prompt="Press [enter] to display four plots showing all graphs, shared graph, task specific 1 and task specific 2")

par(mfrow=c(2,2))

{
  plot.simule(result, graphlabel = label, type="task", graphlayout = layout)

  plot.simule(result, graphlabel = label, type="share", graphlayout = layout)

  plot.simule(result, graphlabel = label, type="taskspecific", subID=1, graphlayout = layout)

  plot.simule(result, graphlabel = label, type="taskspecific", subID=2, graphlayout = layout)
}

