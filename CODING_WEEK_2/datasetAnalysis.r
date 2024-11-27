library(tidyverse, quietly = TRUE)
library(corrr, quietly = TRUE)
library(testthat, quietly = TRUE)
# library(factoextra, quietly = TRUE)

# Load dataset 4
ds4 <- read.csv("./data/DATA_SET_REFERENCE_4_A.csv", row.names = 1)
ds4 <- ds4[complete.cases(ds4),]

# log-transform 
ds4.lg <- log1p(ds4)
# and have a look
head(ds4.lg)
dim(ds4.lg)

# Perform a PCA on the patients from Earth and Venus without scaling or centering the data
# and store it as "ds4.lg.pca", then store the summary in variable s

# ds4.lg.pca <- 
# s <- 

# your code here
ds4.lg.pca <- prcomp( t(ds4.lg), scale=FALSE, center=FALSE )
s  <- summary(ds4.lg.pca)

print(s)

### BEGIN HIDDEN TEST
test_that("Make sure to assign the summary of the PCA object to the variable s.", {
    expect_equal(class(s), "summary.prcomp")
})

test_that("Make sure to perform the PCA on the patients, not the genes.", {
    expect_equal(nrow(ds4.lg.pca$x), ncol(ds4.lg))
})

test_that("Something went wrong in the PCA or summary step.", {
    expect_equal(s, summary(ds4.lg.pca))
})
### END HIDDEN TEST

# Lets examine the PCA result closer. The scree plot shows us which dimensions 
# carry the majority of the variation.
options(repr.plot.width = 6, repr.plot.height = 4)
plot(ds4.lg.pca,
	xlab = "Dimension",
	main = 'Scree plot')

# cumulative explained variability plot
cp <- cumsum(ds4.lg.pca$sdev^2 / sum(ds4.lg.pca$sdev^2))
plot(cp, 
    xlab = "PC #", 
     ylab = "Amount of explained variance", 
     main = "Cumulative variance plot"
    )
# The vast majority of variance in the dataset (98%) is represented in the first principal component!
col.by.planet <- rep('Earth', ncol(ds4.lg)) 
col.by.planet[grep('Venus', colnames(ds4.lg))] <- 'Venus'

# now lets look at whether the patients cluster somehow
# library(factoextra)
# options(repr.plot.width = 6, repr.plot.height = 6)

# fviz_pca_ind(ds4.lg.pca,
#              axes = c(1, 2),
#              geom = c("point"),
#             col.ind = col.by.planet)

head(ds4.lg)

# We can e.g. run a t-test to determine if Gene1 expression values are significantly different.
# In the first step, create logical indices "venus.idx" and ""earth.idx"" for patients from 
# Venus and Earth, respectively. Then, perform a two-sided t-test for "Gene1" expression values
# between Venus and Earth patients and store it as "gene1.venus.vs.earth"

# venus.idx <- 
# earth.idx <- 
# gene1.venus.vs.earth <-

# your code here
venus.idx <- grep('Venus', colnames(ds4))
earth.idx <- grep('Earth', colnames(ds4))
gene1.venus.vs.earth <- t.test( ds4.lg["Gene1", venus.idx], ds4.lg["Gene1", earth.idx] )


print(gene1.venus.vs.earth)

### BEGIN HIDDEN TEST
test_that("Make sure to select only Venus patients in venus.idx.", {
    expect_equal(venus.idx, grep('Venus', colnames(ds4.lg)))
})

test_that("Make sure to select only Earth patients in venus.idx.", {
    expect_equal(earth.idx, grep('Earth', colnames(ds4.lg)))
})

test_that("Make sure to store the t-test as gene1.venus.vs.earth.", {
    expect_equal(class(gene1.venus.vs.earth), "htest")
})

test_that("Make sure to calculate the two-sided t-test.", {
    expect_equal(gene1.venus.vs.earth$p.value, 
                 t.test(
                     ds4.lg['Gene1',grep('Venus', colnames(ds4.lg))], 
                     ds4.lg['Gene1',grep('Earth', colnames(ds4.lg))]
                 )$p.value)
})
### END HIDDEN TEST

# It is not practical to do this for each gene, so let's write a loop to perform a two-sided t-test for every 
# gene and store the resulting p-value in a vector "p.vals". 
# Alternatively, you can use apply to achieve the same.

# p.vals <- 

# your code here
p.vals <- sapply( 1:nrow(ds4.lg), function(i) t.test(ds4.lg[i,earth.idx], ds4.lg[i, venus.idx]) [c("p.value")] )

# How many genes do we find to be significantly different, assuming a threshold of 0.01?
table(p.vals < 0.01)



### BEGIN HIDDEN TEST

pv <- apply(ds4.lg, 1, 
                 function(x) 
                     t.test(x[earth.idx],x[venus.idx])$p.value
                 )

test_that("Make sure to store all p-values in the vector.", {
    expect_equal(length(p.vals), nrow(ds4.lg))
})

test_that("Make sure to store all p-values in the vector.", {
    expect_equal(length(p.vals), nrow(ds4.lg))
})

# can store p-vals as list or vector. want to be independent from that and only care about the p-values, 
# i.e. the sum of equality tests
test_that("Make sure to calculate a two-sided t-test and store only the p-value.", {
    expect_equal(sum(pv == p.vals), nrow(ds4.lg))
})
### END HIDDEN TEST

# A volcano plot visualizes the observed magnitude of gene expression change relative to the 
# statistical significance of that change assigned to the employed statistical method.

fc.log <- log10(apply(ds4.lg[,venus.idx], 1, mean) / apply(ds4.lg[,earth.idx], 1, mean))
col.fc <- rep('black', nrow(ds4.lg))
col.fc[p.vals < 0.01 & fc.log < 0] <- 'red'
col.fc[p.vals < 0.01 & fc.log > 0] <- 'green'

options(repr.plot.width = 6, repr.plot.height = 6)
plot(fc.log, -log10(unlist(p.vals)), 
     main = 'Volcano plot',
     xlab = 'Mean expression fold-change [log]',
     ylab = 't-Test p-value [-log]',
    col = col.fc,
    pch = 19)
abline(h = -log10(0.01), v = 0)

# A visual representation of the differentially expressed genes is very helpful. 
# Usually a heatmap is used for that purpose.
library(pheatmap)

# Create a logical vector "de.idx" indicating genes with significant p-values from the t-test, using a 
# p-value cutoff of 0.01. We can use the vector "p.vals" from earlier.
# de.idx <- 

# your code here
de.idx <- p.vals < 0.01

options(repr.plot.width = 7, repr.plot.height = 20)
pheatmap(ds4.lg[de.idx,], cluster.cols = FALSE)

### BEGIN HIDDEN TEST

idx <- p.vals < 0.01

test_that("The index has to include all genes.", {
    expect_equal(length(de.idx), nrow(ds4.lg))
})

test_that("Make sure to calculate a two-sided t-test and store only the p-value.", {
    expect_equal(idx, de.idx)
})
### END HIDDEN TEST

# And lastly, lets concentrate on the ten genes with the largest expression increase and decrease between planets.
# Firstly, we need to find the thresholds for the genes with highest and lowest fold-change.
neg.thr <- sort(fc.log)[10]
pos.thr <- sort(fc.log)[length(fc.log)-10]

# We then apply the thresholds to create a logical index for our genes of interest.
idx <- which(fc.log >= pos.thr | fc.log <= neg.thr)

# And lastly visualize them as heatmap.
options(repr.plot.width = 8, repr.plot.height = 5)
pheatmap(ds4.lg[idx,], cluster_cols = FALSE)


