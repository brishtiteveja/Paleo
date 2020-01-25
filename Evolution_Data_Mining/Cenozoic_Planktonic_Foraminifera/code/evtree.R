#data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator_development/Developers/Andy/Projects/EvolutionaryTree/Fordham and Zehady shared/180724'
data_dir <- '/Users/andy/Documents/TSCreator/EvolutionaryTree/Fordham and Zehady shared/180724'
setwd(data_dir)
dp_fname1 <- 'qryTSCAze_MorphospeciesAzeTableS3_ColourMorphogroup.xls'
dp_fname2 <- 'qryTSCAze_MorphospeciesAzeTableS3_ColourEcogroup.xls'
dp_fname3 <- 'qryTSCAze_MorphospeciesAzeTableS3_ColourGenus.xls'
dp_fname4 <- 'qryTSCAze_BiospeciesAze_ColourMorphogroup.xls'
dp_fname5 <- 'qryTSCAze_BiospeciesAze_ColourEcogroup.xls' 

dp_fname <- dp_fname2

library(readxl)
# extract the FAD and LAD points from the tscreator datapack in excel format
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
c

# TSCreator evolutionary range/tree format
# Data Row for range
#         `1.3`         X__1    X__2          X__3          X__4    X__5            X__6            X__7      X__8            X__9  # read_excel column names
# <blank> <label>       <age>   <abundance>   <popup>
# Data Row for branch
# <blank> <parent name> <age>   <branch>      <child name>  on/off  <branch label>  dashed/dotted   <popup>  <branch color>   <priority>
#

name_col <- dfxl$`1.3`
name_col

age_col <- dfxl$X__1
age_col

type_col <- dfxl$X__2
type_col

popup <- dfxl$X__3
#popup

branch_to_col <- dfxl$X__3
#branch_to_col

branch_on_off <- dfxl$X__4
# branch_on_off
# 
branch_label <- dfxl$X__5
#branch_label
# 
branch_line_type <- dfxl$X__6
# branch_line_type
# 
branch_popup <- dfxl$X__7
# branch_popup
# 
branch_color <- dfxl$X__8
# branch_color
# 
priority <- dfxl$X__9
# priority

# get the data points with TOP and branch
LAD_ix <- which(type_col == 'TOP')
LAD_ix
FAD_ix <- which(type_col == 'branch')
FAD_ix
# handle other range point types
flood_ix <- which(type_col == 'flood')
abundant_ix <- which(type_col == 'abundant')
frequent_ix <- which(type_col == 'frequent')
common_ix <- which(type_col == 'common')
rare_ix <- which(type_col == 'rare')
conjectured_ix <- which(type_col == 'conjectured')
sample_missing_ix <- which(type_col == 'sample missing')

# creating data frame with LAD points per pseudolevel
lad_df <- data.frame(LAD=age_col[LAD_ix], name=as.character(name_col[LAD_ix]))
lad_df <- lad_df[order(lad_df$name),]
lad_df

# creating data frame for FAD points per pseudolevel
fad_df <- data.frame(FAD=age_col[FAD_ix], name=as.character(branch_to_col[FAD_ix]), parent=as.character(name_col[FAD_ix]))
fad_df

# get the parent for each of the range in the branch and create a branching data frame
branch_label_ix <- which(!is.na(branch_label))
branch_label_ix <- branch_label_ix[-1]
branch_label_df <- data.frame(branch_age=age_col[branch_label_ix],
                              parent=name_col[branch_label_ix],
                              child=branch_to_col[branch_label_ix], 
                              branch_label=branch_label[branch_label_ix])

head(branch_label_df)

# get the root of the tree
root_bool <- lad_df$name %in% fad_df$name
root_ix <- root_bool ==FALSE
root <- lad_df[root_ix,]
root$name <- as.character(root$name)
root



# create data frame for other point types
flood_df <- data.frame(flood=age_col[flood_ix], name=name_col[flood_ix])
flood_df
abundant_df <- data.frame(abundant=age_col[abundant_ix], name=name_col[abundant_ix])
abundant_df
frequent_df <- data.frame(frequent=age_col[frequent_ix], name=name_col[frequent_ix])
frequent_df
common_df <- data.frame(common=age_col[common_ix], name=name_col[common_ix])
common_df
rare_df <- data.frame(rare=age_col[rare_ix], name=name_col[rare_ix])
rare_df
conjectured_df <- data.frame(conjectured=age_col[conjectured_ix], name=name_col[conjectured_ix])
conjectured_df
sample_missing_df <- data.frame(sample_missing=age_col[sample_missing_ix], name=name_col[sample_missing_ix])
sample_missing_df

fl <- which(frequent_df$name == root$name)
frequent_df[fl,]

# add the root node to the FAD data frame
fad_df <- rbind(fad_df, data.frame(FAD=frequent_df[fl,]$frequent, name=frequent_df[fl,]$name, parent='.'))

fad_df <- fad_df[order(fad_df$name),]

# creating data frame to obtain range points
range_df <- fad_df
range_df$LAD <- rep(-1, dim(range_df)[1])
range_df$flood <- rep(-1, dim(range_df)[1])
range_df$abundant <- rep(-1, dim(range_df)[1])
range_df$frequent <- rep(-1, dim(range_df)[1])
range_df$common <- rep(-1, dim(range_df)[1])
range_df$rare <- rep(-1, dim(range_df)[1])
range_df$conjectured <- rep(-1, dim(range_df)[1])
range_df$sample_missing <- rep(-1, dim(range_df)[1])
range_df$branch_age <- rep(NA, dim(range_df)[1])
range_df$branch_label <- rep(NA, dim(range_df)[1])
  
i <- 1
for(n in fad_df$name) {
  ix <- which(lad_df$name == n)
  range_df$LAD[i] = lad_df$LAD[ix] 
  
  ix <- which(flood_df$name == n)
  range_df$flood[i] = ifelse(length(ix) != 0, flood_df$flood[ix], NA)
  
  ix <- which(abundant_df$name == n)
  range_df$abundant[i] = ifelse(length(ix) != 0, abundant_df$abundant[ix], NA)
  
  ix <- which(frequent_df$name == n)
  range_df$frequent[i] = ifelse(length(ix) != 0, frequent_df$frequent[ix], NA)
  
  ix <- which(common_df$name == n)
  range_df$common[i] = ifelse(length(ix) != 0, common_df$common[ix], NA)
  
  ix <- which(rare_df$name == n)
  range_df$rare[i] = ifelse(length(ix) != 0, rare_df$rare[ix], NA)
  
  ix <- which(conjectured_df$name == n)
  range_df$conjectured[i] = ifelse(length(ix) != 0, conjectured_df$conjectured[ix], NA)
  
  ix <- which(sample_missing_df$name == n)
  range_df$sample_missing[i] = ifelse(length(ix) != 0, sample_missing_df$sample_missing[ix], NA)
  
  
  i <- i+1
}

range_df$Lifespan = range_df$FAD - range_df$LAD

# branch_age = FAD
range_df$branch_age = range_df$FAD
# add branch_age and branch_label info to the ranges in the range data frame
ri <- which(range_df$name %in% branch_label_df$child)
for (ir in ri) {
  bci <- which(as.character(branch_label_df$child) == range_df[ir,]$name)
  range_df[ir,]$branch_label = as.character(branch_label_df[bci,]$branch_label)
}

head(range_df)
dim(range_df)

library(DT)
datatable(range_df)

# At this point we have the branch label for only the parent range
# Assign the same branch label to the child ranges successively until the child has a branch label itself
# takes the index of the current child
assignBranchLabelToChild <- function(ci, range_df) {
  # get the child
  child = as.character(range_df[ci,]$name)
  # try to get the branch label of the child
  b_label = as.character(range_df$branch_label[ci])
  
  # if no branchlabel
  na_cond = is.na(b_label)
  na_cond = ifelse(identical(na_cond, logical(0)), TRUE, FALSE)
  if (length(b_label) != 0 & !is.na(b_label)) {
    return(range_df)
  }
  else {
    # get the parent of the child
    parent = as.character(range_df$parent[ci])
    # get the index of the parent
    pi = which(range_df$name == parent)
    #print(parent)
    range_df = assignBranchLabelToChild(pi, range_df)
    range_df$branch_label[ci] = range_df$branch_label[pi]
    
    return(range_df)
  }
}

# test whether the function works
child <- as.character("Acarinina africana")
ci <- which(range_df$name == child)
range_df = assignBranchLabelToChild(ci, range_df)
datatable(range_df)


# function to find branch label list
findBranchLabelToChild <- function(ci, branch_label_list) {
  # get the child
  child = as.character(range_df$name[ci])
  # try to get the branch label of the child
  b_label = as.character(range_df$branch_label[ci])
  
  # if no branchlabel
  na_cond = is.na(b_label)
  na_cond = ifelse(identical(na_cond, logical(0)), TRUE, FALSE)
  if (length(b_label) != 0 & !is.na(b_label)) {
    branch_label_list[[child]] = b_label
    return(branch_label_list)
  }
  else {
    # get the parent of the child
    parent = as.character(range_df$parent[ci])
    # get the index of the parent
    pi <- which(range_df$name == parent)
    branch_label_list = findBranchLabelToChild(pi, branch_label_list)
    branch_label_list[[child]] = branch_label_list[[parent]]
    
    return(branch_label_list)
  }
}


child <- as.character("Acarinina bullbrooki")
ci <- which(range_df$name == child)
branch_label_list <- list()
branch_label_list = findBranchLabelToChild(ci, branch_label_list)
branch_label_list

# find the child which does not have branch label
no_blabel_children_ix <- which(is.na(range_df$branch_label))
no_blabel_children_ix
for(ci in no_blabel_children_ix) {
  child_range = range_df[ci,]
  child = child_range$name
  # avoid the root range
  if (child == ".")
    next
  if (is.na(child_range$branch_label)) { # this way avoids duplicate calls to findBranchLabelToChild recursive function
    range_df = assignBranchLabelToChild(ci, range_df)
  }
}

datatable(range_df)
datatable(range_df[,c('name', 'branch_label')])

# show temporal diversity of morphogroups
morphogroup_cnt <- 19
morphlbls <- paste("Morphogroup", 1:morphogroup_cnt)
morphlblids <- c('M1-flat', 'M2-globular', 'M3-globular with supplementary apertures', 
'M4-spherical', 'M5-clavate', 'M6-planispiral', 'M7-globular', 'M8-globular, keeled', 
'M9-planispiral', 'M10-tubulospinate', 'M11-keeled spines', 'M12-turborotaliform, keeled',
'M13-turborotaliform, non-keeled', 'M14-globorotaliform, keeled', 'M15-globorotaliform, anguloconical', 
'M16-globorotaliform, non-keeled', 'M17-muricate, acarininiform', 'M18-muricocarinate, keeled',
'M19-muricocarinate, anguloconical')

morphdf <- t(data.frame(rep(0, morphogroup_cnt)))
colnames(morphdf) <- morphlbls
morphdf <- morphdf[-1,]

fami_cnt <- length(unique(families))
familbls <- unique(families)
famidf <- t(data.frame(rep(0, fami_cnt)))
colnames(famidf) <- familbls
famidf <- famidf[-1,]

morphogroups <- list()
famigroups <-list()
i <- 1
for(lsp in existing_sp) {
  age <- lsp$age
  lsp$morphogroup <- c()
  for(sp in lsp$existing_species) {
     r <- range_df[range_df$name == sp,]
     lsp$morphogroup <- c(lsp$morphogroup, r$branch_label)
  }
  
  morphogroups[[i]] <- lsp
  
  # temporal morphogroup count per group
  ttdf <- t(data.frame(rep(0, morphogroup_cnt)))
  colnames(ttdf) <- morphlbls
  tbl <- table(lsp$morphogroup)
  for(lbl in morphlbls) {
    v <- as.integer(tbl[lbl])
    if (!is.na(v)) { 
      ttdf[, lbl] <- v
    }
  }
  rownames(ttdf) <- age
  
  morphdf <- rbind(morphdf, ttdf)
  
  # temporal famigroup count per group
  ttdf <- t(data.frame(rep(0, fami_cnt)))
  colnames(ttdf) <- familbls
  tbl <- table(lsp$families)
  for(lbl in familbls) {
    v <- as.integer(tbl[lbl])
    if (!is.na(v)) { 
      ttdf[, lbl] <- v
    }
  }
  rownames(ttdf) <- age
  
  famidf <- rbind(famidf, ttdf)
  
  i <- i+1
}

library(DT)
datatable(morphdf)

fami_cnt = length(unique(families))
i=1
x=-1 * as.numeric(rownames(famidf))
y=famidf[,i]
par(mar=c(4,4,4,4))
plot(x, y, t='l', xlab='Age (Myr)', ylab='Number of species', lwd=2, ylim=c(0,40))

for(i in 2:fami_cnt) {
  y=famidf[,i]
  lines(x,y, col=i, lwd=2)
}

legend("topleft", legend=familbls, col=1:fami_cnt, lty=1, lwd=2)

par(mar=c(4,4,4,4))
morphogroup_cnt = 19
i=1
x=-1 * as.numeric(rownames(morphdf))
y=morphdf[,i]
plot(x, y, t='l', xlab='Age (Myr)', ylab='Number of species', ylim=c(0, 25), lwd=2)

mcols <- c('Black','Red','Green','Blue','Yellow','Brown','Plum', 
           'Magenta', 'Cyan','Purple','Navy blue','Hot pink','Gray',
           'Orange','Maroon','Khaki','Aquamarine','Coral','Orchid')

for(i in 2:morphogroup_cnt) {
  y=morphdf[,i]
  lines(x,y, col=mcols[i], lwd=2)
}


legend("topleft", legend=morphlblids, col=mcols, lty=1, lwd=2, cex=0.59)


# show temporal diversity of ecogroups
ecogroup_cnt <- 6
ecolbls <- paste("Ecogroup", 1:ecogroup_cnt)
ecodf <- t(data.frame(rep(0, ecogroup_cnt)))
colnames(ecodf) <- ecolbls
ecodf <- ecodf[-1,]

ecogroups <- list()
ecodf <- data.frame()
i <- 1
for(lsp in existing_sp) {
  age <- lsp$age
  lsp$ecogroup <- c()
  for(sp in lsp$existing_species) {
    r <- range_df[range_df$name == sp,]
    lsp$ecogroup <- c(lsp$ecogroup, r$branch_label)
  }
  lsp$ecogroup_cnt <- length(lsp$ecogroup)
  
  ecogroups[[i]] <- lsp
  
  # temporal morphogroup count per group
  ttdf <- t(data.frame(rep(0, ecogroup_cnt)))
  colnames(ttdf) <- ecolbls
  tbl <- table(lsp$ecogroup)
  for(lbl in ecolbls) {
    v <- as.integer(tbl[lbl])
    if (!is.na(v)) { 
      ttdf[, lbl] <- v
    }
  }
  rownames(ttdf) <- age
  
  ecodf <- rbind(ecodf, ttdf)
  
  i <- i+1
}

library(DT)
datatable(ecodf)
head(ecodf)

ecogroup_cnt = 6
i=1
x=-1 * as.numeric(rownames(ecodf))
y=ecodf[,i]
plot(x, y, t='l', xlab='Age (Myr)', ylab='Number of species', lwd=2, ylim=c(0,10))

for(i in 2:ecogroup_cnt) {
  y=ecodf[,i]
  lines(x,y, col=i, lwd=2)
}

ecolblids<-c('E1-With Symbionts', 'E2-Without Symbionts', 'E3-thermocline', 
'E4-sub-thermocline', 'E5-high-latitude', 'E6-upwelling/high')
legend("topleft", legend=ecolblids, col=1:ecogroup_cnt, lty=1, lwd=2)


# create evolutionary tree list which contains parent child range relationship
parent <- name_col[FAD_ix]
child <- branch_to_col[FAD_ix]
parent
length(parent)
uparent <- unique(parent)
length(uparent)
child
uchild <- unique(child)
length(uchild)

# list of parent child relationships
evlist <- list()
# traverse through all parents
for (p in uparent) {
  # get the id of the parent
  pi <- which(parent == p)
  
  # get the row id in the range_df for the parent
  pci <- which(range_df$name == p)
  # create the parent node
  evlist[[p]]$name <- p
  evlist[[p]]$FAD <- range_df[pci,]$FAD
  evlist[[p]]$LAD <- range_df[pci,]$LAD
  evlist[[p]]$flood <- range_df[pci,]$flood
  evlist[[p]]$abundant <- range_df[pci,]$abundant
  evlist[[p]]$frequnt <- range_df[pci,]$frequent
  evlist[[p]]$common <- range_df[pci,]$common
  evlist[[p]]$rare <- range_df[pci,]$rare
  evlist[[p]]$conjectured <- range_df[pci,]$conjectured
  evlist[[p]]$sample_missing <- range_df[pci,]$sample_missing
  evlist[[p]]$branch_label <- NA
  evlist[[p]]$viewed <- FALSE
  
  # create the child list of the parent
  evlist[[p]]$child <- list()
  ck <- 1
  for (ci in pi) {
    # create the child node
    curChild <- list()
    curChild$name <- child[ci]
    cci <- which(range_df$name == curChild$name)
    curChild$FAD <- range_df[cci,]$FAD
    curChild$LAD <- range_df[cci,]$LAD
    curChild$flood <- range_df[cci,]$flood
    curChild$abundant <- range_df[cci,]$abundant
    curChild$frequnt <- range_df[cci,]$frequent
    curChild$common <- range_df[cci,]$common
    curChild$rare <- range_df[cci,]$rare
    curChild$conjectured <- range_df[cci,]$conjectured
    curChild$sample_missing <- range_df[cci,]$sample_missing
    curChild$branch_label <- branch_label[cci]
    curChild$viewed <- FALSE
    
    # add child to parent
    evlist[[p]]$child[[ck]] <- curChild
    
    ck = ck + 1
  }
}
#View(evlist)


# empty tree
nextParent <- function(evtree, p) {
  # parent already found as a child of another parent
  if (is.null(evlist[[p]]))
    return(evtree)
  
  if (is.null(evtree[[p]])) {
  } else if (evtree[[p]]$viewed == TRUE) {
      return(evtree)
  }
  
  if (!("name" %in% names(evtree))) { 
    evtree[[p]] = evlist[[p]]
    evtree[[p]]$viewed = TRUE
  }
  
  
  # recursively use each of the child as the next parent (depth first search like)
  children <- evlist[[p]]$child
  nc <- length(children)
  ck <- 1
  for (c in children) {
    # add the current child
    c$viewed = TRUE
    if (!is.null(evtree[[p]])) {
      evtree[[p]]$child[[ck]] <- c
      evtree[[p]]$child[[ck]] <- nextParent(evtree[[p]]$child[[ck]], c$name)
      evtree[[p]]$child[[ck]]$parent <- p
    } else if (!is.null(evtree)){
      evtree$child[[ck]] <- c
      evtree$child[[ck]] <- nextParent(evtree$child[[ck]], c$name)
      evtree$child[[ck]]$parent <- p
    }
    ck <- ck + 1
  }
  
  return(evtree)
}

# start from the first parent
evtree <- list()

p <- as.character(root$name)
evtree = nextParent(evtree, p)
View(evtree)

# Calculate the depth of the tree
getTreeDepth <- function(tree) {
  if (is.null(tree$child)) {
    return(0)
  }
  maxDepth <- -1
  children <- tree$child
  for(c in children) {
    l = 1 + getTreeDepth(c)
    maxDepth = max(l, maxDepth)
  }
  
  return(maxDepth)
}
depth = getTreeDepth(evtree[[root$name]])
depth

# calculate the shortest path from root to a leaf
getMinTreeDepth <- function(tree) {
  if (is.null(tree$child)) {
    return(0)
  }
  minDepth <- getTreeDepth(tree)
  children <- tree$child
  for(c in children) {
    l = 1 + getMinTreeDepth(c)
    minDepth = min(l, minDepth)
  }
  
  return(minDepth)
}
minDepth <- getMinTreeDepth(evtree[[root$name]])
minDepth

# get a tree node
# dfs search
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

tree <- evtree[[root$name]]
search_node <- "Igorina broedermanni"
node <- getTreeNode(tree, search_node) 
node
#View(node)

path <- c(p)
path <- getPathToNode(tree, search_node, path)
path

# get the parent node of a tree node
getParentNode <- function(tree, node) {
  nodeR = getTreeNode(tree, node)
  parent = nodeR$parent
  parentR = getTreeNode(tree, parent)
  
  return(parentR)
}

p <- "Morozovella edgari"
parent <- getParentNode(tree, p)
parent$name

# find common ancestor of two tree node
findCommonAncestor <- function(tree, node1, node2) {
  node = NULL
  if (node1 == node2) {
    nodeR1 = getTreeNode(tree, node1)
    parent = nodeR1$parent
    node = getTreeNode(tree, parent)
    return(node)
  }
  
  path1 <- c(tree$name)
  path1 <- getPathToNode(tree, node1, path1)
  path2 <- c(tree$name)
  path2 <- getPathToNode(tree, node2, path2)
  
  m = path1 %in% path2
  mi = which(m==TRUE)
  common_ancestor_name = path1[mi[length(mi)]]
  
  return(common_ancestor_name)
}

node1 <- 'Morozovella angulata'
node2 <- 'Morozovella velascoensis'
cA = findCommonAncestor(tree, node1, node2)
cA


# 
# dfs search
getTreeNodeWithBranchLabel <- function(tree, branch_label) {
  if (!is.na(tree$branch_label) & as.character(tree$branch_label) == branch_label) {
    return(tree)
  }
  if (is.null(tree$child)) {
    return()
  }
  
  
  children <- tree$child
  for(c in children) {
    res <- getTreeNodeWithBranchLabel(c, branch_label)
    if (!is.null(res))
      break
  }
  return(res)
}

branch_label = "Morphogroup 7"
tree = evtree[[root$name]]
tree$name
node = getTreeNodeWithBranchLabel(tree, branch_label)
node$name
evlist[[node$name]]

