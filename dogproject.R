# Dogproject
library(ggplot2)

data = read.table("/Users/Liis/Desktop/DOK/K_2018/Multivariate_analysis/Project/dogdata2.csv", header=T, sep=",", row.names=1)
dim(data)

#subdata = data %>% filter(label %in% c('Maltese_dog', 'Doberman', 'golden_retriever', "bosu", "EntleBucher"))

# Classes

ggplot(data) + geom_bar(aes(x=label), fill="darkblue") + theme_bw() + xlab("Breed") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#ggplot(subdata) + geom_bar(aes(x=label), fill="darkblue") + theme_bw() + xlab("Breed") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Values from 0 to 255

summary(data$X1)

# Normalize

scaled = scale(data[,-which(names(data) %in% c("label", "image"))])

# Euclidean distances
# dists = dist(scaled)

dists2 = dist(data[,-which(names(data) %in% c("label", "image"))])

#dists3 = dist(subdata[,-which(names(subdata) %in% c("label"))])

# Hierarchical clustering

hc = hclust(dists) # complete linkage
hc2 = hclust(dists2, method="average")

# hc3 = hclust(dists3, method="single")


plot(hc, labels=data$label, cex=0.5)
plot(hc2, labels=data$label, cex=0.9)

# plot(hc3, labels=subdata$label, cex=0.9)

# PCA

pc = prcomp(data[,-which(names(data) %in% c("label", "image"))])

res = data.frame(pc$x)
res$image = data$image
res$label = data$label

ggplot(res) + geom_point(aes(x=PC1, y=PC2, col=data$label)) + theme_bw() + coord_fixed()


library(magick)
library(grid)
ggplot(res, aes(x=PC1, y=PC2)) + geom_image(aes(image=paste0("/Users/Liis/Desktop/DOK/K_2018/Multivariate_analysis/Project/Resimages2/",image)), size=.05) + theme_bw() + coord_fixed()

# K-means
km <- kmeans(data[,-which(names(data) %in% c("label", "image"))], 4)
table(km$cluster, data$label)

first = km$centers[3,]

first = as.numeric(data[2, -which(names(data) %in% c("label", "image"))])
# every 3rd is from R
# then from G and B
r = matrix(first[seq(1, length(first), by=3)], byrow=T)/255
g = matrix(first[seq(2, length(first), by=3)], byrow=T)/255
b = matrix(first[seq(3, length(first), by=3)], byrow=T)/255

dim(r) = c(128,128)
dim(g) = c(128,128)
dim(b) = c(128,128)

r = t(r)
g = t(g)
b = t(b)
res = matrix(cbind(r,g,b), byrow=F)
dim(res) = c(128,128,3)
grid.raster(res, interpolate=FALSE)

library(jpeg)
im = readJPEG("/Users/Liis/Desktop/DOK/K_2018/Multivariate_analysis/Project/Resimages2/EntleBucher_10.jpg")

grid.raster(im, interpolate=FALSE)
