library(igraph)

set.seed(0)
colors = c("darkred","blue4")
vcol = 'gold'
vlab = 'white'

# 5	5	[43, 49]	Exoo 1989b, McKay and Radziszowski 1995
n = 43
# R(5,5)
cl = 5
m = matrix(rbinom(n^2,1,.7),nrow=n)
m[lower.tri(m)] = 0  # It is an undirected graft
diag(m) = 0 # Avoiding self edges
g = graph_from_adjacency_matrix(m, mode='undirected')


plot(g, layout = layout_in_circle(g),
     edge.curved=0, 
     edge.color=colors[1],
     vertex.size=10, 
     vertex.color=vcol,
     vertex.label.color='red',
     vertex.label.cex=1)



m.bar = matrix(rep(0, nrow(m)^2), nrow=nrow(m))
for(i in 1:nrow(m)){for(j in 1:ncol(m)){m.bar[i,j] <- ifelse(m[i,j]==0,1,0)}}
m.bar[lower.tri(m.bar)] = 0  
diag(m.bar) = 0
g.bar = graph_from_adjacency_matrix(m.bar, mode='undirected')

plot(g.bar, 
     layout=layout_in_circle(g.bar), 
     edge.curved=0, 
     edge.color=colors[2],
     vertex.color=vcol,
     vertex.size=10,
     vertex.label.cex=1,
     vertex.label.color="red")


plot(g, layout = layout_in_circle(g),
     edge.curved=0, 
     edge.color=colors[1],
     vertex.size=10, 
     vertex.color=vcol,
     vertex.label.color='red',
     vertex.label.cex=1)
plot(g.bar, 
     layout=layout_in_circle(g.bar), 
     edge.curved=0, 
     edge.width=2,
     edge.color=colors[2],
     vertex.color=vcol,
     vertex.size=10,
     vertex.label.cex=1,
     vertex.label.color="red",
     add=T)


u = union(g,g.bar)

print(paste("Is the graph complete? ", length(E(u))==n*(n-1)/2))

red.clust = cliques(g, min=cl, max=cl)
print(paste("How many red clicks of size", cl, "are there?", length(red.clust),"."))
blue.clust = cliques(g.bar, min=cl, max=cl)
print(paste("How many blue clicks of size", cl, "are there?", length(blue.clust),'.'))

# Largest clique:

print(paste("The maximum red clique size is", clique_num(g)))
print(paste("The maximum blue clique size is", clique_num(g.bar)))
            