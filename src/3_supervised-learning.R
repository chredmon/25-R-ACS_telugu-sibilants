## Analysis 3: Statistical learning
## Charlie Redmon

## packages
library(plyr)
library(caret)
library(lme4)
library(brms)
library(dplyr)
library(bayesplot)
library(tidybayes)
library(ggplot2)
library(emmeans)
library(coda)
library(reshape2)

## directories
ddir <- "../dat/working"

## data
dc <- read.csv(file.path(ddir, "c_measures.csv"))
dv <- read.csv(file.path(ddir, "v_measures.csv"))

d <- merge(dc, dv,
           by=c("Speaker", "Block", "Word", "Rep"))

## data restructuring

# convert frequencies to kHz
fcols <- c("peakF", "mu1", "mu2",
           grep("F\\d", names(d), value=TRUE))

d[, fcols] <- apply(d[, fcols], 2, function(x) x/1000)

d$tiltLF <- d$tiltLF * 1000
d$tiltHF <- d$tiltHF * 1000

## residualise
cols_cv <- c("Speaker", "Position.x", "POA.x", "V2.x",
             "amp", "peakF", "tiltLF",
             "tiltHF", "mu1", "mu2", "mu3", "mu4",
             "v2F2_1", "v2F2_5", "v2F3_1", "v2F3_5")

cols_vc <- c("Speaker", "Position.x", "POA.x", "V1.x", "V2.x",
             "amp", "peakF", "tiltLF",
             "tiltHF", "mu1", "mu2", "mu3", "mu4",
             "v1F2_1", "v1F2_5", "v1F3_1", "v1F3_5",
             "v2F2_1", "v2F2_5", "v2F3_1", "v2F3_5")

mDF_cv <- subset(d, Position.x == "CV")
mDF_vc <- subset(d, Position.x == "VC")

mDF_cv <- mDF_cv[, cols_cv]
names(mDF_cv)[2:4] <- c("Position", "POA", "V2")

mDF_vc <- mDF_vc[, cols_vc]
names(mDF_vc)[2:5] <- c("Position", "POA", "V1", "V2")
mDF_vc$V2 <- ifelse(is.na(mDF_vc$V2), "a", mDF_vc$V2)

for (i in names(mDF_cv)[-c(1:4)]) {

   m <- lm(as.formula(paste0(i, " ~ V2 + Speaker")),
           data=mDF_cv)

    mDF_cv[, i] <- m$residuals

}

for (i in names(mDF_vc)[-c(1:5)]) {

   m <- lm(as.formula(paste0(i, " ~ V1 + V2 + Speaker")),
           data=mDF_vc)

    mDF_vc[, i] <- m$residuals

}


## run SVM
mDF_cv_mat <- mDF_cv[, -c(1, 2, 4)]
mDF_vc_mat <- mDF_vc[, -c(1, 2, 4, 5)]

mDF_cv_mat$POA <- factor(mDF_cv_mat$POA)
mDF_vc_mat$POA <- factor(mDF_vc_mat$POA)

mCV <- train(POA ~ .,
             data=mDF_cv_mat,
             method="svmLinear",
             trControl=trainControl(method="repeatedcv",
                                    number=10,
                                    repeats=5,
                                    classProbs=TRUE))


mDF_cv$alvProb <- predict(mCV, type="prob")[, "alveolar"]
mDF_cv$palProb <- predict(mCV, type="prob")[, "palatal"]
mDF_cv$retProb <- predict(mCV, type="prob")[, "retroflex"]

ddply(mDF_cv, .(POA), summarise,
      mu_alv = round(100 * mean(alvProb), 1),
      mu_pal = round(100 * mean(palProb), 1),
      mu_ret = round(100 * mean(retProb), 1))

ddply(mDF_cv, .(Speaker, POA), summarise,
      mu_alv = round(100 * mean(alvProb), 1),
      mu_pal = round(100 * mean(palProb), 1),
      mu_ret = round(100 * mean(retProb), 1))


mVC <- train(POA ~ .,
             data=mDF_vc_mat,
             method="svmLinear",
             trControl=trainControl(method="repeatedcv",
                                    number=10,
                                    repeats=5,
                                    classProbs=TRUE))


mDF_vc$alvProb <- predict(mVC, type="prob")[, "alveolar"]
mDF_vc$palProb <- predict(mVC, type="prob")[, "palatal"]
mDF_vc$retProb <- predict(mVC, type="prob")[, "retroflex"]

ddply(mDF_vc, .(POA), summarise,
      mu_alv = round(100 * mean(alvProb), 1),
      mu_pal = round(100 * mean(palProb), 1),
      mu_ret = round(100 * mean(retProb), 1))

ddply(mDF_vc, .(Speaker, POA), summarise,
      mu_alv = round(100 * mean(alvProb), 1),
      mu_pal = round(100 * mean(palProb), 1),
      mu_ret = round(100 * mean(retProb), 1))


## accounting for predictions

wordDF <- read.csv(file.path(ddir, "finalStimuli.csv"), header=TRUE)
wordDF <- wordDF[, c("Tx", "SibNB", "Freq", "Degree", "SibComps")]

idcols <- c("Block", "Word", "Rep")
pd_cv <- cbind(mDF_cv, d[d$Position.x == "CV", idcols])
pd_vc <- cbind(mDF_vc, d[d$Position.x == "VC", idcols])

pd_cv <- merge(pd_cv, wordDF, by.x="Word", by.y="Tx", all.x=TRUE)
pd_vc <- merge(pd_vc, wordDF, by.x="Word", by.y="Tx", all.x=TRUE)

pd_cv$pred <- apply(pd_cv[, c("alvProb", "palProb", "retProb")],
                    MARGIN=1,
                    function(x) {
                        if (which.max(x) == 1) {
                            "alveolar"
                        } else if (which.max(x) == 2) {
                            "palatal"
                        } else {
                            "retroflex"
                        }
                    })

pd_vc$pred <- apply(pd_vc[, c("alvProb", "palProb", "retProb")],
                    MARGIN=1,
                    function(x) {
                        if (which.max(x) == 1) {
                            "alveolar"
                        } else if (which.max(x) == 2) {
                            "palatal"
                        } else {
                            "retroflex"
                        }
                    })

pd_cv$acc <- ifelse(pd_cv$POA == pd_cv$pred, 1, 0)
pd_vc$acc <- ifelse(pd_vc$POA == pd_vc$pred, 1, 0)

pd_cv$Freq <- log(pd_cv$Freq, base=10)
pd_vc$Freq <- log(pd_vc$Freq, base=10)


pm_cv <- glm(acc ~ SibNB + Degree + Freq, data=pd_cv,
             family=binomial(link="logit"))
summary(pm_cv)

pm_cv <- glm(acc ~ SibComps + Degree + Freq, data=pd_cv,
             family=binomial(link="logit"))
summary(pm_cv)

pm_vc <- glm(acc ~ SibNB + Degree + Freq, data=pd_vc,
             family=binomial(link="logit"))
summary(pm_vc)

pm_vc <- glm(acc ~ SibComps + Degree + Freq, data=pd_vc,
             family=binomial(link="logit"))
summary(pm_vc)

df <- merge(df, wordDF, by.x="Word", by.y="Tx")

df$POA <- factor(df$POA, levels=c("palatal", "retroflex", "alveolar"))


## Unsupervised Learning Model (Projection-Based Clustering, PBC)
library(ProjectionBasedClustering)

matCV <- as.matrix(mDF_cv_mat[, -1])
projpts <- NeRV(matCV)
bm <- Projection2Bestmatches(projpts)

cl_cv2 <- ProjectionBasedClustering(k=2, matCV,
                                    BestMatches=bm$Bestmatches,
                                    LC=bm$LC,
                                    method="mahalanobis")

cl_cv3 <- ProjectionBasedClustering(k=3, matCV,
                                    BestMatches=bm$Bestmatches,
                                    LC=bm$LC,
                                    method="mahalanobis")

matVC <- as.matrix(mDF_vc_mat[, -1])
projpts_vc <- NeRV(matVC)
bm_vc <- Projection2Bestmatches(projpts_vc)

cl_vc2 <- ProjectionBasedClustering(k=2, matVC,
                                    BestMatches=bm_vc$Bestmatches,
                                    LC=bm_vc$LC,
                                    method="mahalanobis")

cl_vc3 <- ProjectionBasedClustering(k=3, matVC,
                                    BestMatches=bm_vc$Bestmatches,
                                    LC=bm_vc$LC,
                                    method="mahalanobis")



resCV2 <- data.frame(Phone=mDF_cv_mat[, 1],
                     Clust=cl_cv2)
tab <- table(resCV2$Clust, resCV2$Phone)
round(100 * prop.table(tab, margin=1))

resCV3 <- data.frame(Phone=mDF_cv_mat[, 1],
                     Clust=cl_cv3)
tab <- table(resCV3$Clust, resCV3$Phone)
round(100 * prop.table(tab, margin=1))



resVC2 <- data.frame(Phone=mDF_vc_mat[, 1],
                     Clust=l_vc2)
tab <- table(resVC2$Clust, resVC2$Phone)
round(100 * prop.table(tab, margin=1))

resVC3 <- data.frame(Phone=mDF_vc_mat[, 1],
                     Clust=cl_vc3)
tab <- table(resVC3$Clust, resVC3$Phone)
round(100 * prop.table(tab, margin=1))


