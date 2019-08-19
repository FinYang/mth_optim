library(tidyverse)

# k-means -----------------------------------------------------------------

cost <- function(D, alloc, centre){
  sum(sapply(1:length(alloc), function(i) sum(D[i, ] - centre[[alloc[[i]]]]))^2)
}

locate_centres <- function(k, D, alloc){
  n <- dim(D)[[1]]
  d <- dim(D)[[2]]
  centre <- replicate(k, numeric(2), simplify = F)
  cnt <- numeric(k)
  for(i in 1:n){
    centre[[alloc[[i]]]] <- centre[[alloc[[i]]]] + D[i, ]
    cnt[[alloc[[i]]]] <- cnt[[alloc[[i]]]] + 1
  }
  for(j in 1:k){
    if(cnt[[j]] == 0)
      centre[[j]] <- D[sample(n, 1), ]
    else
      centre[[j]] <- centre[[j]]/cnt[[j]]
  }
  return(centre)
}

closest_allocation <- function(D, centre){
  k <- length(centre)
  n <- dim(D)[[1]]
  d <- dim(D)[[2]]
  alloc <- numeric(n)
  for(i in 1:n){
    dis <- sapply(1:k, function(j) norm(D[i,]-centre[[j]], "2"))
    # minDist <- min(dis)
    alloc[[i]] <- which.min(dis)
  }
  return(alloc)
}

kmeans <- function(k, pts){
  n <- dim(pts)[[1]]
  d <- dim(pts)[[2]]
  centre <- replicate(k, pts[sample(n,1),], simplify = F)
  alloc <- closest_allocation(pts, centre)
  bestCost <- Inf
  while(cost(pts, alloc, centre) < (1-1e-6)*bestCost){
    bestCost <- cost(pts, alloc, centre)
    centre <- locate_centres(k, pts, alloc)
    alloc <- closest_allocation(pts, centre)
  }
  return(list(alloc, centre))
}

# readin data
raw <- read_delim("http://cs.joensuu.fi/sipu/datasets/s2.txt"," ", col_names = F) %>% 
  apply(2, as.numeric)
S2 <- raw/max(raw)
qplot(x = S2[,1], y=S2[,2])

# perform
k <- 14
cluster <- kmeans(k, S2)
alloc <- cluster[[1]]
centre <- cluster[[2]]
cost(S2, alloc, centre)

plot_clusters <- function(D, alloc, centre){
ggplot() +
  geom_point(aes(x=D[,1], y=D[,2], color = as.factor(alloc)))+
  theme(legend.position="none") +
  geom_text(aes(x=X1, y=X2), data = as.data.frame(do.call(rbind, centre)), 
            size=10, label = "★")
}

plot_clusters(S2, alloc, centre)




bestCost <- cost(S2, alloc, centre)  
pb <- txtProgressBar(min = 0, max = 50, style = 3)
for(i in 1:50){
  tem <- kmeans(k, S2)
  A <- tem[[1]]
  C <- tem[[2]]
  if(cost(S2, A, C) < bestCost){
    alloc <- A
    centre <- C
    bestCost <- cost(S2, alloc, centre)  
    cat("New better cost", bestCost, "\n")
  }  
  setTxtProgressBar(pb, i)
}

plot_clusters(S2, alloc, centre)


# pareto frontier
paretofront <- function(D){
  n <- dim(D)[[1]]
  d <- dim(D)[[2]]
  kval <- 1
  obj <- cost(D, rep(1, n), locate_centres(1, D, rep(1, n)))
  k <- 3
  while(k<100){
    tem <- kmeans(k, S2)
    A <- tem[[1]]
    C <- tem[[2]]
    kval <- c(kval, k)
    obj <- c(obj, cost(D, A, C))
    cat(k, ":", "cost =", obj[[length(obj)]], "smallest cluster =", min(table(A)), "\n")
    k <- k*2
  }
  return(list(kval, obj))
}

pareto <- paretofront(S2)

qplot(x = pareto[[1]], y = pareto[[2]], geom = "line")
