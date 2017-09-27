#test

data(mtcars)

mtcars
rownames(mtcars)

l = c()
for (i in 1:length(rownames(mtcars))){
  l = c(l, paste('Sample', i, sep = ''))
}
l

rownames(mtcars) = l
rownames(mtcars)

# prepare hierarchical cluster
hc = hclust(dist(mtcars))
# very simple dendrogram
plot(hc)

# labels at the same level
plot(hc, hang = -1)


# tweeking some parameters
op = par(bg = "white")
plot(hc, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", 
     col.axis = "#F38630", lwd = 3, sub = "", hang = -1, axes = FALSE, ylab = 'Distance', xlab = 'Samples')
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 400, 100), side = 2, at = seq(0, 400, 100), line = 1, 
      col = "#A38630", las = 2)

# using dendrogram objects
hcd = as.dendrogram(hc)

labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951")
# cut dendrogram in 4 clusters
clusMember = cutree(hc, 4)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)

plot(clusDendro, main = "Cool Dendrogram", type = "triangle")


plot(clusDendro, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", 
     col.axis = "#F38630", lwd = 3, sub = "", hang = -1, axes = FALSE)





