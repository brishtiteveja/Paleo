t<- list()
t$name <- 'F'
t$FAD <- 1 - 0
t$LAD <- 1 - 0

t$child <- list()
t$child[[1]] <- list()
t$child[[1]]$name <- 'A'
t$child[[1]]$FAD <- 1 - 0
t$child[[1]]$LAD <- 1 - 0.1

t$child[[2]] <- list()
t$child[[2]]$name <- 'B'
t$child[[2]]$FAD <- 1 - 0
t$child[[2]]$LAD <- 1 - 0.2

t$child[[3]] <- list()
t$child[[3]]$name <- 'E'
t$child[[3]]$FAD <- 1 - 0
t$child[[3]]$LAD <- 1 - 0.5

t$child[[3]]$child <- list()

t$child[[3]]$child[[1]] <- list()
t$child[[3]]$child[[1]]$name <- 'C'
t$child[[3]]$child[[1]]$FAD <- 1 - 0.5
t$child[[3]]$child[[1]]$LAD <- 1 - (0.5 + 0.3)

t$child[[3]]$child[[2]] <- list()
t$child[[3]]$child[[2]]$name <- 'D'
t$child[[3]]$child[[2]]$FAD <- 1 - 0.5
t$child[[3]]$child[[2]]$LAD <- 1 - (0.5 + 0.4)

getTreeNode <- function(tree, node) {
  if (tree$name == node) 
    return(tree)
  
  if (is.null(tree$child)) {
    return()
  }
  
  children <- tree$child
  for(c in children) {
    res <- getTreeNode(c, node)
    if (!is.null(res))
      break
  }
  return(res)
}

# get Path to a node
getPathToNode <- function(tree, node, path) {
  if (tree$name == node) { 
    return(tree$name)
  }
  
  if (is.null(tree$child)) {
    return()
  }
  
  children <- tree$child
  for(c in children) {
    prevPath <- path
    path <- c(tree$name,getPathToNode(c, node, path))
    if (node %in% path)
      return(path)
    
    path <- prevPath
  }
  return(path)
}


# get the parent node of a tree node
getParentNode <- function(tree, node) {
  nodeR = getTreeNode(tree, node)
  parent = nodeR$parent
  parentR = getTreeNode(tree, parent)
  
  return(parentR)
}

getTreeNode(t, 'C')
getPathToNode(t, 'C', c())

# ((B:0.2,(C:0.3,D:0.4)E:0.5)A:0.1)F;

# convert tree in Newwick format
getTreeInNewickFormat1 <- function(tree) {
  if (is.null(tree$child)) {
    d <- paste("(", tree$FAD, ",", tree$LAD, ")", sep = " ")
    tr <- c(tree$name, ":", d) 
    return(tr)
  }
  
  children <- tree$child
  clist <- c("(")
  cn <- 1
  for(c in children) {
    cr <- getTreeInNewickFormat1(c)
    clist <- c(clist, cr)
    if (cn != length(children)) {
      clist <- c(clist, ",") 
    } else {
      # Add itself 
      #if(c$LAD)
    }
    cn <- cn + 1
  }
  d <- paste("(", tree$FAD, ",", tree$LAD, ")", sep = " ")
  clist <- c(clist, ")" , tree$name, ":", d)
  
  return(clist)
}

getTreeInNewickFormat1(t)

r <- getTreeInNewickFormat(evtree$`Hedbergella [ancestor]`)
txt <- paste(as.character(r), collapse=" ")
txt
write.csv(txt, file='tree.txt')

#((raccoon:19.19959,bear:6.80041):0.84600,((sea_lion:11.99700, seal:12.00300):7.52973,((monkey:100.85930,cat:47.14069):20.59201, weasel:18.87953):2.09460):3.87382,dog:25.46154);

# ((B:2,(C:3,D:4)E:5)A:1)F;

# * sort children by FAD
# for each child divide the parent into two branches with a new parent.
# the parent gets the FAD of the child 
# and this new parent gets two child : one is the original child with the distance of it's LAD (recursive)
# the other child is the broken original parent with its own LAD

# convert tree in Newwick format
getTreeInNewickFormat <- function(tree, parent) {
  if (is.null(tree$child)) {
    if (is.null(parent$LAD)) {
      d <- tree$LAD
    } else {
      # essentially lifespan
      d <- tree$LAD - parent$LAD
    }
    d <- abs(d)
    tname <- paste("'", tree$name, "'", sep="" )
    tr <- c(tname, ":", d)
    #print(tr)
    return(tr)
  }
  
  children <- tree$child
  
  # get a sorted list of children by the FAD (origination)
  c_FAD <- c()
  for (c in children) {
    c_FAD <- c(c_FAD, c$FAD)
  }
  
  # get the order by decreasing FADs
  oi <- order(c_FAD, decreasing = T)
  max_child_id <- oi[1]
  max_child_FAD <- max(c_FAD)
  
  clist <- c("(")
  new_parent <- tree
  # essentially lifespan
  # break the current parent tree and add it as a child if LAD is greater than the minimum FAD of child
  if (new_parent$LAD <= max_child_FAD) {
    # create a new child from the parent
    new_child <- new_parent
    new_child$FAD <- max_child_FAD
    new_parent$LAD <- max_child_FAD
    
    # after splitting the parent
    # remove the child with max FAD
    new_children <- list()
    cin1 <- 1
    cin2 <- 1
    for(c in children) {
      if (cin1 != max_child_id) {
        new_children[[cin2]] <- children[[cin1]]
        cin2 <- cin2 + 1 
      }
      cin1 <- cin1 + 1
    }
    
    if (length(new_children) > 0) {
      new_child$child <- new_children
    } else {
      new_child$child <- NULL
    }
    
    cr1 <- getTreeInNewickFormat(children[[max_child_id]], new_parent)
    clist <- c(clist, cr1)
    if (tree$LAD != max_child_FAD) {
      cr2 <- getTreeInNewickFormat(new_child, new_parent)
      clist <- c(clist, "," , cr2)
    } else {
      cn <- 1
      noi <- oi[-max_child_id]
      for(ci in noi) {
        c <- children[[ci]]
        if (new_parent$LAD == c$FAD) {
          #print(noi)
          cr <- getTreeInNewickFormat(c, new_parent)
          clist <- c(clist, ",", cr)
          cn <- cn + 1
        }
      }
    }
  } 
  
  if (is.null(parent)) {
    d <- tree$LAD - tree$FAD
  } else {
    d <- tree$LAD - parent$LAD
  }
  d <- abs(d)
  tname <- paste("'", tree$name, "'", sep="" )
  clist <- c(clist, ")" , tname, ":", d)
  
  if (is.null(parent)) {
    clist <- c("(", clist, ");")
  }
  
  return(clist)
}

tree <- t #evtree$`Hedbergella [ancestor]`$child[[1]]$child[[1]]$child[[1]]
r <- getTreeInNewickFormat(tree, NULL)
txt <- paste(as.character(r), collapse=" ")
txt

# subtree of Globanomalina compressa
tree <- evtree$`Hedbergella [ancestor]`$child[[1]]$child[[1]]$child[[1]]
r <- getTreeInNewickFormat(tree, NULL)
txt <- paste(as.character(r), collapse="")
txt


# convert newick to TSCreator evtree format
txt
