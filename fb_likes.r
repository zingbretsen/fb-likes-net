library(Rfacebook)
library(ggplot2)
library(scales)
library(grid)
library(igraph)
library(httpuv)

# Yo token here
token <- ""

my_friends <- getFriends(token=token, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=token, private_info=TRUE)
bak <- my_friends_info

# Get rid of people who have NA usernames
valid_usernames <- which(!is.na(my_friends_info$username))
my_friends_info <- my_friends_info[valid_usernames,]

# Store each person's likes in their my_friend_info$likes
for (i in seq(my_friends_info$username)){
  friend_likes <- getLikes(my_friends_info[i,]$username,n=5000,token=token)$names
  my_friends_info[i,]$likes <- list(unique(friend_likes))
}

# Please tell me there's a better way of doing this...
# You really only need to go through half of the matrix,
# but since I'm adding it to a vector, I'd have to pad
# with 0s, and that doesn't sound like the right way
# of doing it either...
# 
# And don't even get me started on these two NA 
# assignments. But instantiating them as a list()
# didn't seem to work for me when I try to add to it.
# 
# TL;DR halp
like_intersection <- NA
like_intersection_length <- NA
for (i in seq(my_friends_info$username)){
  likes1 <- my_friends_info[i,]$likes[[1]]
  for (j in seq(my_friends_info$username)){
    likes2 <- my_friends_info[j,]$likes[[1]]
    intersection <- intersect(likes1,likes2)
    like_intersection <-c(like_intersection,intersection)
    like_intersection_length <-c(like_intersection_length,length(intersection))
  }  
}

# Gets rid of the stupid leading NA
# I'm sorry
good <- which(!is.na(like_intersection_length))
like_intersection_length <- like_intersection_length[good]

# Make a matrix out of the 1D vector of # of common likes
mat <- matrix(like_intersection_length,nrow=length(my_friends_info$username))
mat_bak <- mat

# Should probably be addressed when creating the matrix
# I don't know if this actually affects the plot
for (i in length(mat[1,])){
  mat[i, i] = 1
}

# Gets rid of unconnected people
connected <- which(rowSums(mat)!=0)
mat <- mat[connected,connected]
my_friends_info <- my_friends_info[connected,]
network <- graph.adjacency(mat, mode="undirected", weighted=TRUE)

# Keeps the biggest connected cluster
cl <- clusters(network)
gc <- which(cl$membership == 1)
mat <- mat[gc, gc]
my_friends_info <- my_friends_info[gc,]

# The rest is pretty much straight from Pablo's script
# Except note the weighted flag in the adjacency graph call
network <- graph.adjacency(mat, mode="undirected", weighted=TRUE)
fc <- fastgreedy.community(network) ## communities / clusters
set.seed(123)
l <- layout.fruchterman.reingold(network, niter=1000, coolexp=0.5, circular=TRUE) ## layout
d <- data.frame(l); names(d) <- c("x", "y")
d$cluster <- factor(fc$membership)

## now let's add the edges
edgelist <- get.edgelist(network, names=FALSE)
edges <- data.frame(d[edgelist[,1],c("x", "y")], d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")
d$degree <- degree(network)
which.max(degree(network)) ## who do I have more friends in common with?
central.nodes <- lapply(communities(fc), function(x) x[which.max(d$degree[x])])
central.names <- fc$names[unlist(central.nodes)] ## names of central nodes
## within each cluster

d$label <- NA

## let's put it all together, with some final touches on how points look
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2), 
  data=edges, size=0.25, color="white", alpha=1/3) +
  ## note that here I add a border to the points
  geom_point(color="grey20", aes(fill=cluster), shape=21, size=2) +
  scale_fill_discrete(labels=labels) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill="black"),
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
    )

pq