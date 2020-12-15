#library(statnet)
library(igraph)

rd_csv <- function(p){
  return(read.csv(p, stringsAsFactors = FALSE))
}

#https://bookdown.org/markhoff/social_network_analysis/your-first-network.html

path <- paste(getwd(), 'github', 'levels', 'comparisons.csv', sep = '/')
#Reading in attributes.
path2 <- paste(getwd(), 'github', 'levels', 'matches.csv', sep = '/')

#Reading in matches.
df <- rd_csv(path2)
#Reading in attributes of metros.
atts <- rd_csv(path)

#Adding regions to atts df.
reg <- c('Midwest', 'South', 'South', 'South', 'South', 'South', 'South', 'West', 'Midwest',
         'Midwest', 'South', 'Northeast', 'Midwest', 'West', 'West', 'South', 'Northeast',
         'South', 'South', 'West', 'Midwest', 'West', 'West', 'South', 'Northeast', 'Northeast',
         'West', 'Northeast', 'West', 'West', 'West', 'West')
length(reg)
atts$region <- reg

#Creating network from edgelist and attributes.
nw <- graph_from_data_frame(df[, c(1, 2)], directed = FALSE, vertices = atts)
plot(nw)
#Edgelist.
# el <- as.matrix(df[, c(1, 2)])
# nw <- graph.edgelist(el, directed = FALSE)
#Looking at names.
#V(nw)$name
#Making long Metro Name Shorter to Plot.
long_names <- unlist(strsplit(V(nw)$name, ', '))
short_names <- long_names[which(match(long_names, long_names) %% 2 != 0)]
shorter_names <- vector(mode = 'character', length = length(short_names))
for (nm in short_names){
  shorter_names[match(nm, short_names)] <- unlist(strsplit(nm, '-'))[1]
}
#Changing long names to short names.
V(nw)$name <- shorter_names
#Setting pval edge attribute.
E(nw)$pval <- df$pval
#Defining colors for regions.
colrs <- rainbow(length(unique(reg)), alpha = .60)[as.factor(atts$region)]
plot(nw, layout = layout.kamada.kawai(nw), vertex.label.cex = .9,
     vertex.label.color = 'black',
     #vertex.color = 'tomato',
     vertex.color = colrs,
     edge.curved = .1,
     edge.width = E(nw)$pval + 1.75,
     vertex.size = log(V(nw)$Median.Total.Compensation))
     ##vertex.size = log(V(nw)$n + 10))
legend('topleft', legend = c('Midwest', 'Northeast', 'South', 'West'),
       col = rainbow(length(unique(reg)), alpha = .60), pch = 19, title = 'Region')
#Taking out san jose and san francisco
nodes_interest <- c('Kansas City', 'Seattle', 'Salt Lake City')
#BFS traverses graph and gets connections from only those nodes of interest above.
sel_nodes <- bfs(nw, root = nodes_interest, unreachable = FALSE)$order
nw_sub <- induced.subgraph(nw, vids = sel_nodes[!is.na(sel_nodes)])
#plot(nw_sub)

#Varied connected graphs within.
#grps <- decompose.graph(nw)

plot(nw_sub, layout = layout.kamada.kawai(nw_sub), vertex.label.cex = .9,
     vertex.label.color = 'black',
     #vertex.color = 'tomato',
     vertex.color = colrs,
     edge.curved = .1,
     edge.width = E(nw_sub)$pval + 1.75)
legend('bottomleft', legend = c('Midwest', 'Northeast', 'South', 'West'),
       col = rainbow(length(unique(reg)), alpha = .60), pch = 19, title = 'Region')


#Looking at edges
E(nw)
#Looking at vertices.
V(nw)

