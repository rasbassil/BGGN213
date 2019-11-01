
#Import File
weight <- read.table(file = "bimm143_05_rstats/weight_chart.txt", header = TRUE)

#Line Plot
plot(weight$Age, weight$Weight , type = "o", pch=15, cex=1.5, lwd=2, ylim=c(2,10),
     xlab="Age(months)", ylab="Weight(kg)", main="Baby Weight with Age", col="blue")

#Barplot
mouse <- read.table(file="bimm143_05_rstats/feature_counts.txt", header= TRUE, sep="\t")

#or read.delim function sets header and sep features for tab delimited files
read.delim(file = "bimm143_05_rstats/feature_counts.txt")

#Plot
#barplot(height)
par(mar=c(3,11,3,3))
barplot(height = mouse$Count, horiz= TRUE, names.arg=mouse$Feature, 
        main= "Number of Features in the Mouse GRCm38 Genome", las=1, xlim=c(0,80000))

#Histograms
x <- c(rnorm(10000),rnorm(10000)+4)
hist(x, breaks=150)

#Using Color in Plots
par(mar=c(7,5,6,3))
sex <- read.delim("bimm143_05_rstats/male_female_counts.txt")

#Color Vectors
barplot(sex$Count, ylab="Counts", names.arg=sex$Sample, las=2, col=rainbow(nrow(sex)))

barplot(sex$Count, ylab="Counts", names.arg=sex$Sample, las=2, col=c("blue","red"))

#Color by Value
genes <- read.delim("bimm143_05_rstats/up_down_expression.txt")
nrow(genes)
table(genes$State)
palette(c("blue","grey","red"))
plot(genes$Condition1, genes$Condition2, col=genes$State, xlab="Expression Condition 1",
     ylab="Expression Condition 2")

#Dynamic Use of Color
meth <- read.delim("bimm143_05_rstats/expression_methylation.txt")
nrow(meth)
dcols <- densCols(meth$gene.meth, meth$expression)
plot(meth$gene.meth,meth$expression, col=dcols, pch=20)

inds <- meth$expression > 0
dcols.custom <- densCols(meth$gene.meth[inds], meth$expression[inds], 
                   colramp= colorRampPalette(c("blue", "green", "red", "yellow")))
plot(meth$gene.meth[inds],meth$expression[inds], col=dcols.custom, pch=20)
