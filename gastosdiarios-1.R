# Reading data
dt <- read.table("Gastos_Diarios.csv", sep=',', header=TRUE, row.names=1)
names(dt) <- c("comida","roupas")
print(head(dt))

# Plotting scattergram (XY graph)
# http://www.statmethods.net/graphs/scatterplot.html
plot(dt$comida, dt$roupas, main="Comida x Roupas", xlab="Comida", ylab="Roupas")
# http://www.statmethods.net/advgraphs/axes.html
text(dt$comida, dt$roupas, row.names(dt), cex=0.6, pos=4, col="blue") 
print("Graphic XY (comida vs. roupas) is plotted.")

# Prepare data
dts <- na.omit(dt) # listwise deletion of missing
dts <- scale(dts)  # standardize variables 
print(dts)

# Analysing clusters
# ------------------

# Visualizing the Dendrogram
# https://rpubs.com/gaston/dendrograms
hc = hclust(dist(dt)) 			# prepare hierarchical cluster
plot(hc, xlab="pessoas")		# very simple dendrogram

print("Cluster dendrogram is plotted.")

# http://www.statmethods.net/advstats/cluster.html
wss <- (nrow(dts)-1)*sum(apply(dts,2,var))
# Establishing number of clusters solution
# nCenters <- nrow(dts)-1   # (maximum allowed)
nCenters <- 2				# (chosen visually, observing dendrogram image)	

# Partioning by K-means
for (i in 2:nCenters) 
  wss[i] <- sum(kmeans(dts, centers=i)$withinss)
plot(1:nCenters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(dts, nCenters) 
# get cluster means
aggregate(dts,by=list(fit$cluster),FUN=mean)
# append cluster assignment
dt.cluster <- data.frame(dts, fit$cluster) 

# Resulting clusters
print(head(dt.cluster))

# Plotting the reviewed scattergram with clusters determined
# set up the plot region:
par(bg="#fce8e4")
plot(dt$comida, dt$roupas, main="Comida x Roupas - CLUSTERS", xlab="Comida", ylab="Roupas", col.main="navy", pch=19)
text(dt$comida, dt$roupas, dt.cluster$fit.cluster, cex=1.2, pos=2, col="blue", font=4) 
text(dt$comida, dt$roupas, row.names(dt), cex=1.1, pos=4, col="maroon", font=2) 

# http://stackoverflow.com/questions/22463710/filled-rectangle-in-r
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "#fce802", border = "black", density=20, lty=2, angle=90) 
box(which="figure",lty="solid",col="red",bg="yellow")
mtext( "legenda:\nClusters: 1,2\nPessoas: a-e", cex=1, adj=-0.4, line=-25, at=1.05 )

print("XY cluster analysis (comida vs. roupas) is plotted.")

