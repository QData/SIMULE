
### helper function to make adjacency matrix, theta is a list of graphs
.make.adj.matrix <-
  function(theta, separate=FALSE)
  {
    K = length(theta)
    adj = list()

    if(separate)
    {
      for(k in 1:K)
      {
        adj[[k]] = (abs(theta[[k]])>1e-5)*1
      }
    }

    if(!separate)
    {
      adj = 0*theta[[1]]
      for(k in 1:K)
      {
        adj = adj+(abs(theta[[k]])>1e-5)*2^(k-1)
      }
    }

    return(adj)
  }

### helper function to make title for graph
.maketitle <-
  function(type = "task",
           subID = NULL,
           index = NULL,
           neighbouroption = "task",
           graphlabel = NULL)
  {
    if (type == "share") {
      return ("Shared Graph")
    }

    if (type == "taskspecific") {
      temp = paste(as.character(subID), collapse = ", ")
      return (paste("Task", temp, "Specific Graph"))
    }

    if (type == "task") {
      if (is.null(subID)) {
        return ("All Graphs")
      }
      else {
        if (length(subID) == 1) {
          if (subID == 0) {
            return ("Shared Graph")
          }
          else{
            return (paste("Task", subID, "Graph"))
          }
        }
        else {
          if (0 %in% subID) {
            temp = subID[-(which(subID %in% 0))]

            return(paste("Task", paste(as.character(temp), collapse = ", ")), "Graph")
          }
          else {
            return (paste("Task", paste(
              as.character(subID), collapse = ", "
            ), "Graph"))
          }
        }
      }
    }

    if (type == "neighbour") {
      second = ""
      first = ""

      if (neighbouroption == "task") {
        if (length(subID) == 1) {
          if (subID == 0) {
            second = "on shared graph"
          }
          else {
            second = paste("on task",
                           paste(as.character(subID), collapse = ", "),
                           "graph")
          }
        }
        else {
          if (!is.null(subID)) {
            if (0 %in% subID) {
              temp = subID[-(which(subID %in% 0))]

              second = paste("on task",
                             paste(as.character(temp), collapse = ", "),
                             "graph")
            }
            else {
              second = paste("on task",
                             paste(as.character(subID), collapse = ", "),
                             "graph")
            }
          }
          else {
            second = "on all graphs"
          }
        }
      }
      else{
        second = paste("on task",
                       paste(as.character(subID), collapse = ", "),
                       "specific graph")
      }

      if (is.null(graphlabel) || is.na(graphlabel)) {
        first = paste("Zoom in at node", paste(as.character(index), collapse = ", "))
      }

      else {
        first = paste("Zoom in at node", paste(as.character(graphlabel[index]), collapse = ", "))
      }

      return (paste(first, second))
    }

  }

## helper function to create layout for graph
.makelayout <-
  function(x,
           graphlayout = NULL)
  {
    ### igraph automatically layout the graph
    if (is.null(graphlayout)) {
      graphlayout = layout_nicely(x, dim = 2)
    }
    return(graphlayout)
  }


#' return igraph object from simule result specified by user input
#'
#' This function can return an igraph object from simule result for user to work with directly
#' @author Beilun Wang, Zhaoyang Wang (Author), Beilun Wang (maintainer)
#' @param x output generated from simule/wsimule function (simule/wsimule class)
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @return an igraph object of graph / subgraph from simule result specified by user input
#' @details the function aims to provide users the flexibility to explore and visualize the graph own their own
#' generated from simule / wsimule
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", TRUE)
#' graph = returngraph(result, type="task")
#' }
#' @export
#' @import igraph
returngraph <-
  function(x,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL) {
    adj = .make.adj.matrix(x$Graphs)
    diag(adj) = 0
    gadj = graph.adjacency(adj, mode = "upper", weighted = TRUE)

    K = length(x$Graphs)

    ### color the graph edges with rainbow()
    if (!is.null(E(gadj)$weight)) {
      E(gadj)$color = rainbow(K + 1)[E(gadj)$weight]
    }

    if (type == "share") {
      ### ignore subID and index
      gadj = subgraph.edges(gadj, which(E(gadj)$weight == K + 1), delete.vertices = FALSE)
    }

    else if (type == "taskspecific") {
      ### ignore index
      if (0 %in% subID) {
        stop("please specify valid task number(s)")
      }
      if (is.null(subID)) {
        stop("please specify task number(s)")
      }
      if (!prod(subID %in% (1:K))) {
        stop("please specify valid task number(s)")
      }
      gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% subID), delete.vertices = FALSE)

    }

    else if (type == "task") {
      if (!is.null(subID)) {
        if (!prod(subID %in% (0:K))) {
          stop("please specify valid task number(s)")
        }
        ### when subID = 0, gadj will be shared graph
        ### when subID = others, gadj will be graph for task with subID (including shared part)
        gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% c(subID, K + 1)), delete.vertices = FALSE)
      }
      ### when subID is null, return all graphs
    }

    else if (type == "neighbour") {
      if (!prod(index %in% (1:vcount(gadj)))) {
        stop("please specify valid index number(s)")
      }

      gadj = subgraph.edges(gadj, unlist(incident_edges(gadj, index)) , delete.vertices = FALSE)
      if (neighbouroption == "task") {
        if (!is.null(subID)) {
          if (!prod(subID %in% (0:K))) {
            stop("please specify valid task number(s)")
          }
          gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% c(subID, K + 1)), delete.vertices = FALSE)
        }
      }
      else if (neighbouroption == "taskspecific") {
        if (!prod(subID %in% (1:K))) {
          stop("please specify valid task number(s)")
        }
        gadj = subgraph.edges(gadj, which(E(gadj)$weight %in% subID), delete.vertices = FALSE)
      }
      else {
        stop("please specify a valid neighbouroption")
      }

    }

    else {
      stop("please specify a correct type")
    }

    return(gadj)
  }


#' Plot simule result specified by user input
#'
#' This function can plot and return multiple sparse graphs distinguished by edge colors
#' from the result generated by simule
#'
#' @author Beilun Wang, Zhaoyang Wang (Author), Beilun Wang (maintainer)
#' @param x output generated from simule/wsimule function (simule/wsimule class)
#' @param graphlabel vertex names for the graph, there are three options:
#' (1) NA (no label)
#' (2) NULL (default numeric label according to the feature order)
#' (3) a vector of labels (a vector of labels cooresponding to x)
#' deault value is NULL
#' @param type type of graph, there are four options:
#' (1) "task" (graph for each task (including shared part) specified further by subID (task number))
#' (2) "share" (shared graph for all tasks)
#' (3) "taskspecific" (graph for each task specific (excluding shared part)
#' specified further by subID (task number) )
#' (4) "neighbour" (zoom into nodes in the graph specified further by neighbouroptoin, subID (task number)
#' and index (node id))
#' @param neighbouroption determines what type of graph to zoom into when parameter type is "neighbour"
#' There are two options:
#' (1) "task" (zoom into graph for each task (including shared part))
#' (2) "taskspecific" (zoom into graph for each task specific (excluding shared part))
#' @param subID selects which task to display
#' (1) 0 (only allowed when type is task or type is neighbour and neighbouroption is task) (selecting share graph)
#' (2) positive task number (selects a task number)
#' (3) a vector of task number (selects multiple tasks)
#' (4) NULL (selects all tasks (all graphs))
#' @param index determines which node(s) to zoom into when parameter type is "neighbour"
#' could either be an integer or vector of integers representing node ids
#' (zoom into one node or multiple nodes)
#' @param graphlayout layout for the graph (two column matrix specifying x,y coordinates of each node in graph)
#' if not provided, igraph will use the default layout_nicely() function present the graph
#' @param ... extra parameters passed to plot.igraph
#' @return a plot of graph / subgraph from simule result specified by user input
#' @details when only the simulresult is provided, the function will plot all graphs with default numeric labels
#' User can specify multiple subID and multiple index to zoom in multiple nodes on multiple graphs
#' Each graph will include a decriptive title and legend to indicate correspondence between edge color and task.
#' The function will plot graph and return an igraph object at the same time
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = simule(X = exampleData , lambda = 0.1, epsilon = 0.45, covType = "cov", FALSE)
#' plot.simule(result,  graphlabel = NULL, type="task", graphlayout = NULL)
#' }
#' @export
#' @import igraph
#' @importFrom grDevices rainbow
#' @importFrom graphics legend
#' @importFrom graphics plot
plot.simule <-
  function(x,
           graphlabel = NULL,
           type = "task",
           neighbouroption = "task",
           subID = NULL,
           index = NULL,
           graphlayout = NULL,
           ...)
  {
    .env = "environment: namespace:simule"
    ### make unique subID and index
    subID = unique(subID)
    index = unique(index)
    ### get igraph from returngraph
    gadj = returngraph(
      x,
      type = type,
      neighbouroption = neighbouroption,
      subID = subID,
      index = index
    )
    ### create graphlayout
    graphlayout = .makelayout(gadj, graphlayout = graphlayout)

    ### create title
    title = .maketitle(
      type = type,
      subID = subID,
      index = index,
      graphlabel = graphlabel,
      neighbouroption = neighbouroption
    )

    ### plot using igraph with legend
    plot(
      gadj,
      layout = graphlayout,
      vertex.label.font = 2,
      vertex.shape = "none",
      vertex.label.color = "gray40",
      vertex.label = graphlabel,
      vertex.label.cex = .7,
      vertex.frame.color = "white",
      vertex.size = 10 ,
      main = title,
      ...
    )

    ### legend on topleft
    legend(
      "topleft" ,
      legend = c(paste("task", c(
        1:length(x$Graphs)
      ), "specific"), "share"),
      ### using rainbow to create distinct color
      col = rainbow(length(x$Graphs) + 1),
      pch = 16
    )


  }
