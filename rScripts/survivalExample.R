###
### A brief introduction to
### DATA SCIENCE
###
### Example analysis in R:
### Compare the overall survival between the different tumor stages.
###
### Alexander Koch
### 2019
###

setwd('~/Documents/postdoc/dataScienceClass/')

library(data.table)
library(survival)

# Load and process the phenotype and survival data that we downloaded from TCGA.
p = fread('data/TCGA-COAD.GDC_phenotype.tsv', data.table=F)
dim(p)
head(p)
table(p$sample_type.samples)

# Select only the primary tumor samples.
p = p[which(p$sample_type.samples == 'Primary Tumor'),]
dim(p)
table(p$tumor_stage.diagnoses)

p = p[,c('submitter_id', 'tumor_stage.diagnoses')]
colnames(p) = c('patient', 'stage')
p$stage[grepl('iv', p$stage)] = 'stage 4'
p$stage[grepl('iii', p$stage)] = 'stage 3'
p$stage[grepl('ii', p$stage)] = 'stage 2'
p$stage[grepl('i', p$stage)] = 'stage 1'
p$stage[!grepl('stage', p$stage)] = NA

s = fread('data/TCGA-COAD.survival.tsv', data.table=F)
dim(s)
head(s)
s = s[,c('_PATIENT', '_EVENT', '_TIME_TO_EVENT')]
colnames(s) = c('patient', 'event', 'time')

# Combine the selected phenotype and survival data into a single table.
data = merge(p, s)
dim(data)
head(data)

# Create the survival curves for the four different tumor stages.
fit = survfit(Surv(time, event) ~ stage, data=data)
r = survdiff(Surv(time, event) ~ stage, data=data)
pvalue = pchisq(r$chisq, 3, lower.tail=F)
pvalue

stageColors = c('#f6d2a9', '#f19c7c', '#dd686c', '#b13f64')
plot(fit, lty=1, col=stageColors, frame.plot=F, lwd=2, xlab='Years', ylab='Survival', mark=-0x00D7L, xaxt='n', yaxt='n', xlim=c(0, 15 * 365))
axis(1, at=seq(0, 15 * 365, by=5 * 365), labels=seq(0, 15, by=5), col='#2f2f2f', col.axis='#2f2f2f')
axis(2, at=seq(0, 1, by=0.2), col='#2f2f2f', col.axis='#2f2f2f', las=1)
text(15 * 365 / 2, 1, paste0('p = ', sprintf('%3.2e', pvalue)), adj=c(0.5, 0.5), col='#2f2f2f')
legend('topright', c('stage 1', 'stage 2', 'stage 3', 'stage 4'), col=stageColors, lty=1, lwd=2, bty='n', text.col='#2f2f2f')
