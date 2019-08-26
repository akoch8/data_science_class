###
### A brief introduction to
### DATA SCIENCE
###
### Example analysis in R:
### Differential expression analysis of TP53 between clear cell renal cell
### carcinoma and normal cortex.
###
### Alexander Koch
### 2019
###

setwd('~/Documents/postdoc/dataScienceClass/')

library(data.table)

# Load the GEO gene expression data, skipping the annotation rows at the beginning.
data = fread('data/GDS4282.soft', skip=318, data.table=F)
dim(data)
head(data)
hist(as.matrix(data[,-c(1, 2)]), col='orange', border=NA, main='Gene expression histogram')
plot(density(as.matrix(data[,-c(1, 2)])), bty='n', lwd=2, col='orange', main='Gene expression density plot')

# Load the GEO data annotation data from the start of the file.
annotation = fread('data/GDS4282.soft', nrows=318, data.table=F, sep='\n')
annotation = annotation[,1]
length(annotation)
annotation[1]

# Extract the names of the normal and the tumor samples.
tumorSamples = grep('Gene expression profiling of clear-cell renal cell carcinoma', annotation, value=T)
tumorPatients = tumorSamples
tumorPatients = sub('^.+ patient ', '', tumorPatients)
tumorSamples = sub(' = .+$', '', tumorSamples)
tumorSamples = sub('#', '', tumorSamples)
tumor = data.frame(cbind(tumorSamples, tumorPatients), stringsAsFactors=F)
colnames(tumor) = c('tumor_sample', 'patient')
head(tumor)
normalSamples = grep('Gene expression profiling of normal cortex', annotation, value=T)
normalPatients = normalSamples
normalPatients = sub('^.+ patient ', '', normalPatients)
normalSamples = sub(' = .+$', '', normalSamples)
normalSamples = sub('#', '', normalSamples)
normal = data.frame(cbind(normalSamples, normalPatients), stringsAsFactors=F)
colnames(normal) = c('normal_sample', 'patient')
head(normal)

# Check if the normal and tumor patient IDs overlap.
length(normalPatients[!normalPatients %in% tumorPatients])

# Combine the normal and tumor sample names and patient IDs in a single data frame.
samples = merge(normal, tumor)
head(samples)

# Only keep the samples from the expression data that are in our list of normal and
# tumor samples.
data = data[,c('ID_REF', 'IDENTIFIER', samples$normal_sample, samples$tumor_sample)]

# Is there a difference in gene expression between normal and tumor samples for
# the gene TP53?
'TP53' %in% data$IDENTIFIER
data[which(data$IDENTIFIER == 'TP53'),]

# There are two probes for TP53 on the gene expression array, so we will have to
# combine the expression data from both probes in a single measurement. We can do this
# by taking the mean of both measurements.
tp53 = colMeans(data[which(data$IDENTIFIER == 'TP53'),-c(1, 2)])

# Check if the expression data is normally distributed. If it is, we can use the
# t-test to test the difference between normal and tumor samples. If not, we have
# to use a non-parametric test such as the Mann-Whitney test (also known as the
# Wilcoxon rank-sum test).
hist(tp53)
hist(tp53, col='orange', border=NA, xlab='TP53 expression')
plot(density(tp53))
plot(density(tp53), bty='n', lwd=3, col='orange', main='', xlab='TP53 expression')
shapiro.test(tp53)
qqnorm(tp53)
qqnorm(tp53, bty='n', pch=20, col='grey')
qqline(tp53, lwd=2, col='#5555ee')

# It looks like the data are not normally distributed, so we'll use the Wilcoxon
# rank-sum test. Given that each patient provided a normal and a tumor sample, we
# will perform a paired test.
testResult = wilcox.test(tp53[samples$normal_sample], tp53[samples$tumor_sample], paired=T)
testResult

# Plot the data.
textColor = '#2f2f2f'
subColor = '#6f6f6f'
normalColor = '#7fbf7b'
tumorColor = '#af8dc3'
boxplot(tp53[samples$normal_sample], tp53[samples$tumor_sample])
par(mar=c(3, 4, 4, 1))
boxplot(tp53[samples$normal_sample], tp53[samples$tumor_sample], outline=F, boxwex=0.4, frame.plot=F, yaxt='n', xaxt='n', ylim=c(floor(min(tp53)), ceiling(max(tp53))))
points(jitter(rep(1, nrow(samples)), amount=0.15), tp53[samples$normal_sample], pch=19, col=normalColor)
points(jitter(rep(2, nrow(samples)), amount=0.15), tp53[samples$tumor_sample], pch=19, col=tumorColor)
mtext('TP53 expression', side=3, line=2, at=0.5, adj=0, cex=1, col=textColor, font=2)
mtext('TP53 is higher expressed in ccRCC samples than in normal samples', side=3, line=1, at=0.5, adj=0, cex=0.8, col=subColor)
mtext(paste0('(Wilcoxon rank-sum test, p = ', round(testResult$p.value, digits=5), ')'), side=3, line=0, at=0.5, adj=0, cex=0.8, col=subColor)
axis(1, at=c(1, 2), labels=c('Normal', 'Tumor'), lwd=0, col.axis=textColor, font=2)
axis(2, at=seq(floor(min(tp53)), ceiling(max(tp53)), 1), lwd=0.5, col=textColor, col.axis=textColor, las=1)
title(ylab='TP53 expression', col.lab=textColor, font.lab=2)

# Use the png() function to save the plot. Other options are pdf(),
# jpeg(), tiff(), and bmp(), or to save the plot by hand from the
# RStudio Plots window.
png('img/geoExample.png', width=6, height=5, units='in', res=150)
par(mar=c(3, 4, 4, 1))
boxplot(tp53[samples$normal_sample], tp53[samples$tumor_sample], outline=F, boxwex=0.4, frame.plot=F, yaxt='n', xaxt='n', ylim=c(floor(min(tp53)), ceiling(max(tp53))))
points(jitter(rep(1, nrow(samples)), amount=0.15), tp53[samples$normal_sample], pch=19, col=normalColor)
points(jitter(rep(2, nrow(samples)), amount=0.15), tp53[samples$tumor_sample], pch=19, col=tumorColor)
mtext('TP53 expression', side=3, line=2, at=0.5, adj=0, cex=1, col=textColor, font=2)
mtext('TP53 is higher expressed in ccRCC samples than in normal samples', side=3, line=1, at=0.5, adj=0, cex=0.8, col=subColor)
mtext(paste0('(Wilcoxon rank-sum test, p = ', round(testResult$p.value, digits=5), ')'), side=3, line=0, at=0.5, adj=0, cex=0.8, col=subColor)
axis(1, at=c(1, 2), labels=c('Normal', 'Tumor'), lwd=0, col.axis=textColor, font=2)
axis(2, at=seq(floor(min(tp53)), ceiling(max(tp53)), 1), lwd=0.5, col=textColor, col.axis=textColor, las=1)
title(ylab='TP53 expression', col.lab=textColor, font.lab=2)
dev.off()

