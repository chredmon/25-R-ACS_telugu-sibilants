# Analysis 2: Vocalic parameter discriminability
# Charles Redmon

# packages
library(plyr)
library(lme4)
library(brms)
library(dplyr)
library(bayesplot)
library(tidybayes)
library(ggplot2)
library(emmeans)
library(coda)
library(reshape2)

# paths
ddir <- "../dat/working"

fn <- file.path(ddir, "v_measures.csv")

# data
d <- read.csv(fn)

## data restructuring

# convert Hz to kHz
fcols <- names(d)[11:30]

for (i in fcols) {
    d[, i] <- d[, i] / 1000
}


# encode factors
d$Speaker <- factor(d$Speaker)
d$POA <- factor(d$POA)
d$V1 <- factor(d$V1)
d$V2 <- factor(d$V2)

# convert to long format
f2cols <- grep("F2", names(d), value=TRUE)
f3cols <- grep("F3", names(d), value=TRUE)

dF2 <- melt(d,
            id.vars=c("Speaker", "Block", "Position", "Word", "Rep",
                      "POA", "V1", "V2"),
            measure.vars=f2cols,
            variable.name="Time",
            value.name="F2")

dF2$Context <- substr(dF2$Time, 1, 2)
dF2$Time <- as.numeric(substr(dF2$Time, 6, 6))

dF3 <- melt(d,
            id.vars=c("Speaker", "Block", "Position", "Word", "Rep",
                      "POA", "V1", "V2"),
            measure.vars=f3cols,
            variable.name="Time",
            value.name="F3")

dF3$Context <- substr(dF3$Time, 1, 2)
dF3$Time <- as.numeric(substr(dF3$Time, 6, 6))



## MODELING

dF2_vc <- subset(dF2, Position == "VC" & Context == "v1")
dF3_vc <- subset(dF3, Position == "VC" & Context == "v1")
dF2_cv <- subset(dF2, Position == "CV" & Context == "v2")
dF3_cv <- subset(dF3, Position == "CV" & Context == "v2")

mDF_f2_vc <- subset(dF2_vc, V1 == "u")
mDF_f3_vc <- subset(dF3_vc, V1 == "u")
mDF_f2_cv <- subset(dF2_cv, V2 == "u")
mDF_f3_cv <- subset(dF3_cv, V2 == "u")

mF2_vc <- gam(F2 ~ POA +
         s(Time, by=POA, bs="cr", k=3) +
         s(Speaker, bs="re") +
         s(Time, by=interaction(Speaker, POA), bs="re"),
         data=mDF_f2_vc)

summary(mF2_vc)

mF3_vc <- gam(F3 ~ POA +
         s(Time, by=POA, bs="cr", k=3) +
         s(Speaker, bs="re") +
         s(Time, by=interaction(Speaker, POA), bs="re"),
         data=mDF_f3_vc)

summary(mF3_vc)

mF2_cv <- gam(F2 ~ POA +
         s(Time, by=POA, bs="cr", k=3) +
         s(Time, by=interaction(Speaker, POA), bs="re"),
         data=mDF_f2_cv)

summary(mF2_cv)

mF3_cv <- gam(F3 ~ POA +
         s(Time, by=POA, bs="cr", k=3) +
         s(Speaker, bs="re") +
         s(Time, by=interaction(Speaker, POA), bs="re"),
         data=mDF_f3_cv)

summary(mF3_cv)

ndat <- expand.grid(
    Time=seq(1, 5, length.out=50),
    POA=levels(d$POA),
    Speaker=levels(d$Speaker))

ndat_rep <- ndat[rep(1:nrow(ndat), times=5000), ]
rownames(ndat_rep) <- NULL

m_sim_F2vc <- gratia::posterior_samples(mF2_vc,
                n=5000, burnin=1000, n_cores=4, method="mh",
                data=ndat)

m_sim_F3vc <- gratia::posterior_samples(mF3_vc,
                n=5000, burnin=1000, n_cores=4, method="mh",
                data=ndat)

m_sim_F2cv <- gratia::posterior_samples(mF2_cv,
                n=5000, burnin=1000, n_cores=4, method="mh",
                data=ndat)

m_sim_F3cv <- gratia::posterior_samples(mF3_cv,
                n=5000, burnin=1000, n_cores=4, method="mh",
                data=ndat)

post_F2vc <- cbind(ndat_rep, m_sim_F2vc)
post_F3vc <- cbind(ndat_rep, m_sim_F3vc)
post_F2cv <- cbind(ndat_rep, m_sim_F2cv)
post_F3cv <- cbind(ndat_rep, m_sim_F3cv)

names(post_F2vc)[ncol(post_F2vc)] <- "y"
names(post_F3vc)[ncol(post_F3vc)] <- "y"
names(post_F2cv)[ncol(post_F2cv)] <- "y"
names(post_F3cv)[ncol(post_F3cv)] <- "y"

mu_F2vc <- post_F2vc %>% group_by(.draw, POA, Time) %>%
         summarise(yhat=mean(y))

mu_F3vc <- post_F3vc %>% group_by(.draw, POA, Time) %>%
         summarise(yhat=mean(y))

mu_F2cv <- post_F2cv %>% group_by(.draw, POA, Time) %>%
         summarise(yhat=mean(y))

mu_F3cv <- post_F3cv %>% group_by(.draw, POA, Time) %>%
         summarise(yhat=mean(y))

# numerical contrasts
xF2vc <- dcast(mu_F2vc, .draw + Time ~ POA, value.var="yhat")
xF3vc <- dcast(mu_F3vc, .draw + Time ~ POA, value.var="yhat")
xF2cv <- dcast(mu_F2cv, .draw + Time ~ POA, value.var="yhat")
xF3cv <- dcast(mu_F3cv, .draw + Time ~ POA, value.var="yhat")

xF2vc$a_p <- xF2vc$alveolar - xF2vc$palatal
xF2vc$a_r <- xF2vc$alveolar - xF2vc$retroflex
xF2vc$p_r <- xF2vc$palatal - xF2vc$retroflex

xF3vc$a_p <- xF3vc$alveolar - xF3vc$palatal
xF3vc$a_r <- xF3vc$alveolar - xF3vc$retroflex
xF3vc$p_r <- xF3vc$palatal - xF3vc$retroflex

xF2cv$a_p <- xF2cv$alveolar - xF2cv$palatal
xF2cv$a_r <- xF2cv$alveolar - xF2cv$retroflex
xF2cv$p_r <- xF2cv$palatal - xF2cv$retroflex

xF3cv$a_p <- xF3cv$alveolar - xF3cv$palatal
xF3cv$a_r <- xF3cv$alveolar - xF3cv$retroflex
xF3cv$p_r <- xF3cv$palatal - xF3cv$retroflex

xDF <- xF3cv
xDF_summ <- ddply(xDF, .(Time), summarise,
                  x_lwr = quantile(a_p, 0.025),
                  x_upr = quantile(a_p, 0.975))

xDF_summ$x <- ifelse(xDF_summ$x_lwr < 0 &
                         xDF_summ$x_upr < 0, -1,
                         ifelse(xDF_summ$x_lwr > 0 &
                                xDF_summ$x_upr > 0, 1, 0))

xDF_summ <- xDF_summ[!duplicated(xDF_summ$x), ]
xDF_summ




# plot

pDF_F2vc <- ddply(mu_F2vc, .(POA, Time), summarise,
                mu = mean(yhat),
                lwr = quantile(yhat, 0.25),
                upr = quantile(yhat, 0.75))

pDF_F3vc <- ddply(mu_F3vc, .(POA, Time), summarise,
                mu = mean(yhat),
                lwr = quantile(yhat, 0.25),
                upr = quantile(yhat, 0.75))

pDF_F2cv <- ddply(mu_F2cv, .(POA, Time), summarise,
                mu = mean(yhat),
                lwr = quantile(yhat, 0.25),
                upr = quantile(yhat, 0.75))

pDF_F3cv <- ddply(mu_F3cv, .(POA, Time), summarise,
                mu = mean(yhat),
                lwr = quantile(yhat, 0.25),
                upr = quantile(yhat, 0.75))

pDF_F2vc$Position <- "VC"
pDF_F3vc$Position <- "VC"
pDF_F2cv$Position <- "CV"
pDF_F3cv$Position <- "CV"

pDF_F2vc$Formant <- "F2"
pDF_F3vc$Formant <- "F3"
pDF_F2cv$Formant <- "F2"
pDF_F3cv$Formant <- "F3"

pDF <- rbind(pDF_F2vc, pDF_F3vc, pDF_F2cv, pDF_F3cv)

pDF$Position <- factor(pDF$Position, levels=c("VC", "CV"))
pDF$Formant <- factor(pDF$Formant, levels=c("F3", "F2"))

#blankDF <- data.frame(Time=c(1, 1), mu=c(2.5, 3.1),
#                      Position=c("VC", "VC"),
#                      POA=c("retroflex", "retroflex"),
#                      Formant=c("F3", "F3"))

#blankDF$Position <- factor(blankDF$Position, levels=c("VC", "CV"))
#blankDF$Formant <- factor(blankDF$Formant, levels=c("F3", "F2"))

jgreen <- "#389826"
jblue <- "#4063D8"
jred <- "#CB3C33"

p <- ggplot(pDF, aes(x=Time, y=mu, colour=POA)) +
     facet_grid(Formant~Position, scales="free_y") +
     geom_line() +
     geom_ribbon(aes(ymin=lwr, ymax=upr, fill=POA), alpha=0.5) +
#     geom_blank(data=blankDF) +
     scale_colour_manual(values=c(jgreen, jblue, jred)) +
     scale_fill_manual(values=c(jgreen, jblue, jred)) +
     labs(y="Formant Frequency (kHz)", x="Time Point") +
     theme_bw() +
     theme(legend.position=c(0.99, 0.49),
           legend.justification=c(1,1),
           axis.text.x=element_text(colour="black"),
           axis.text.y=element_text(colour="black"),
           strip.text.x=element_text(size=12),
           strip.text.y=element_text(size=12),
           legend.text=element_text(size=12),
           legend.title=element_blank())


ggsave("plots/formants_o.png", p, width=9, height=6, units="in",
       dpi=600)







