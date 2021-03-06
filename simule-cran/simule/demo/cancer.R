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

result = simule(cancerlist, 0.2, 1, covType = "cov", TRUE)
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


readline(prompt="Press [enter] to display four plots zooming into node MELK, E2F3 and BB_S4 on previous four plots")


nodeid = which(label %in% c("MELK","E2F3", "BB_S4")) ### look for id
{
  plot.simule(result, graphlabel = label, type="neighbour", index = nodeid, graphlayout = layout)

  plot.simule(result, graphlabel = label, type="neighbour", subID = 0, index = nodeid,graphlayout = layout)

  plot.simule(result, graphlabel = label, type="neighbour", neighbouroption = "taskspecific",
              subID=1, index = nodeid,
              graphlayout = layout)

  plot.simule(result, graphlabel = label, type="neighbour", neighbouroption = "taskspecific",
              subID=2, index = nodeid,
              graphlayout = layout)
}

### it is also possible to zoom into multiple nodes on multiple graphs specified by input
