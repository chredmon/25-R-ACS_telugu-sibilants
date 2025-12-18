# Analysis 1: Consonantal parameter discriminability
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

# directories (relative to source file)
ddir <- "../dat/working"

# files
cmfile <- file.path(ddir, "c_measures.csv")
vmfile <- file.path(ddir, "v_measures.csv")

# data
dc <- read.csv(cmfile)
dv <- read.csv(vmfile)

## data restructuring

# convert frequencies to kHz
dc$peakF <- dc$peakF / 1000
dc$tiltLF <- dc$tiltLF * 1000
dc$tiltHF <- dc$tiltHF * 1000
dc$mu1 <- dc$mu1 / 1000
dc$mu2 <- dc$mu2 / 1000

# reduce vowels to three categories
dc$VF <- ifelse(dc$V %in% c("e", "i"), "HF",
                ifelse(dc$V %in% c("o", "u"), "HB", "L"))
dc$VF <- factor(dc$VF, levels=c("L", "HF", "HB"))

# code VC as reference level
dc$Position <- factor(dc$Position, levels=c("VC", "CV"))
dv$Position <- factor(dv$Position, levels=c("VC", "CV"))

# ensure Speaker and Place of Articulation (POA) are factors
dc$Speaker <- factor(dc$Speaker)
dv$Speaker <- factor(dv$Speaker)

dc$POA <- factor(dc$POA)
dv$POA <- factor(dv$POA)


## MODELING

### Amplitude

# ML reference
m_amp_ml <- lmer(amp ~ POA*Position + VF +
                 (1 + POA + Position | Speaker),
                 data=dc)

# main Bayesian model
m_amp <- brm(amp ~ POA*Position + VF +
             (1 + POA + Position | Speaker),
             data=dc, chains=2, cores=2, warmup=1000, iter=3000,
             prior=prior("normal(0, 10)", class="b"),
             backend="cmdstanr")

plot(m_amp)
saveRDS(m_amp, "models/model_01_amp.rds")
m_amp <- readRDS("models/model_01_amp.rds")

pairs(emmeans(m_amp, ~ POA | Position, epred=TRUE))
emm <- emmeans(m_amp, ~ POA * Position, epred=TRUE)

post <- tidybayes::gather_emmeans_draws(emm)
post <- post[, c(1, 2, 6)]
names(post)[3] <- "yhat"
post$Param <- "Amplitude (dB)"



### Spectral Peak Frequency

# ML reference
m_peakF_ml <- lmer(peakF ~ POA*Position + VF +
                   (1 + POA + Position | Speaker),
                   data=dc)

# main Bayesian model
m_peakF <- brm(peakF ~ POA*Position + VF + (1 + POA + Position | Speaker),
    data=dc, chains=2, cores=2, warmup=1000, iter=3000,
    prior=prior("normal(0, 10)", class="b"),
    backend="cmdstanr")

plot(m_peakF)
saveRDS(m_peakF, "models/model_01_peakF.rds")
m_peakF <- readRDS("models/model_01_peakF.rds")

pairs(emmeans(m_peakF, ~ POA | Position, epred=TRUE))

emm_peakF <- emmeans(m_peakF, ~ POA * Position, epred=TRUE)
post_peakF <- tidybayes::gather_emmeans_draws(emm_peakF)
post_peakF <- post_peakF[, c(1, 2, 6)]
names(post_peakF)[3] <- "yhat"
post_peakF$Param <- "Peak Frequency (kHz)"
post <- rbind(post, post_peakF)



### Low-Frequency Spectral Tilt

# ML reference
m_tiltLF_ml <- lmer(tiltLF ~ POA*Position + VF +
                    (1 + POA + Position | Speaker),
                    data=dc)

# main Bayesian model
m_tiltLF <- brm(tiltLF ~ POA*Position + VF +
                (1 + POA + Position | Speaker),
                data=dc, chains=2, cores=2, warmup=1000, iter=3000,
                prior=prior("normal(0, 10)", class="b"),
                backend="cmdstanr")

plot(m_tiltLF)
saveRDS(m_tiltLF, "models/model_01_tiltLF.rds")
m_tiltLF <- readRDS("models/model_01_tiltLF.rds")

pairs(emmeans(m_tiltLF, ~ POA | Position, epred=TRUE))

emm_tiltLF <- emmeans(m_tiltLF, ~ POA * Position, epred=TRUE)
post_tiltLF <- tidybayes::gather_emmeans_draws(emm_tiltLF)
post_tiltLF <- post_tiltLF[, c(1, 2, 6)]
names(post_tiltLF)[3] <- "yhat"
post_tiltLF$Param <- "Low-Freq. Tilt (dB/kHz)"
post <- rbind(post, post_tiltLF)


### High-Frequency Spectral Tilt

# ML reference
m_tiltHF_ml <- lmer(tiltHF ~ POA*Position + VF +
                    (1 + POA + Position | Speaker),
                    data=dc)

# main Bayesian model
m_tiltHF <- brm(tiltHF ~ POA*Position + VF +
                (1 + POA + Position | Speaker),
                data=dc, chains=2, cores=2, warmup=1000, iter=3000,
                prior=prior("normal(0, 10)", class="b"),
                backend="cmdstanr")

plot(m_tiltHF)
saveRDS(m_tiltHF, "models/model_01_tiltHF.rds")
m_tiltHF <- readRDS("models/model_01_tiltHF.rds")

pairs(emmeans(m_tiltHF, ~ POA | Position, epred=TRUE))

emm_tiltHF <- emmeans(m_tiltHF, ~ POA * Position, epred=TRUE)
post_tiltHF <- tidybayes::gather_emmeans_draws(emm_tiltHF)
post_tiltHF <- post_tiltHF[, c(1, 2, 6)]
names(post_tiltHF)[3] <- "yhat"
post_tiltHF$Param <- "High-Freq. Tilt (dB/kHz)"
post <- rbind(post, post_tiltHF)


### Spectral Mean

# ML reference
m_mu1_ml <- lmer(mu1 ~ POA*Position + VF +
                 (1 + POA + Position | Speaker),
                 data=dc)

# main Bayesian model
m_mu1 <- brm(mu1 ~ POA*Position + VF +
             (1 + POA + Position | Speaker),
             data=dc, chains=2, cores=2, warmup=1000, iter=3000,
             prior=prior("normal(0, 10)", class="b"),
             backend="cmdstanr")

plot(m_mu1)
saveRDS(m_mu1, "models/model_01_mu1.rds")
m_mu1 <- readRDS("models/model_01_mu1.rds")

pairs(emmeans(m_mu1, ~ POA | Position, epred=TRUE))

emm_mu1 <- emmeans(m_mu1, ~ POA * Position, epred=TRUE)
post_mu1 <- tidybayes::gather_emmeans_draws(emm_mu1)
post_mu1 <- post_mu1[, c(1, 2, 6)]
names(post_mu1)[3] <- "yhat"
post_mu1$Param <- "Spectral Mean (kHz)"
post <- rbind(post, post_mu1)


### Spectral Dispersion

# ML reference
m_mu2_ml <- lmer(mu2 ~ POA*Position + VF +
                 (1 + POA + Position | Speaker),
                 data=dc)

# main Bayesian model
m_mu2 <- brm(mu2 ~ POA*Position + VF +
             (1 + POA + Position | Speaker),
             data=dc, chains=2, cores=2, warmup=1000, iter=3000,
             prior=prior("normal(0, 10)", class="b"),
             backend="cmdstanr")

plot(m_mu2)
saveRDS(m_mu2, "models/model_01_mu2.rds")
m_mu2 <- readRDS("models/model_01_mu2.rds")

pairs(emmeans(m_mu2, ~ POA | Position, epred=TRUE))

emm_mu2 <- emmeans(m_mu2, ~ POA * Position, epred=TRUE)
post_mu2 <- tidybayes::gather_emmeans_draws(emm_mu2)
post_mu2 <- post_mu2[, c(1, 2, 6)]
names(post_mu2)[3] <- "yhat"
post_mu2$Param <- "Spectral Dispersion (kHz)"
post <- rbind(post, post_mu2)


### Spectral Skewness

# ML reference
m_mu3_ml <- lmer(mu3 ~ POA*Position + VF +
                 (1 + POA + Position | Speaker),
                 data=dc)

# main Bayesian model
m_mu3 <- brm(mu3 ~ POA*Position + VF +
             (1 + POA + Position | Speaker),
             data=dc, chains=2, cores=2, warmup=1000, iter=3000,
             prior=prior("normal(0, 10)", class="b"),
             backend="cmdstanr")

plot(m_mu3)
saveRDS(m_mu3, "models/model_01_mu3.rds")
m_mu3 <- readRDS("models/model_01_mu3.rds")

pairs(emmeans(m_mu3, ~ POA | Position, epred=TRUE))

emm_mu3 <- emmeans(m_mu3, ~ POA * Position, epred=TRUE)
post_mu3 <- tidybayes::gather_emmeans_draws(emm_mu3)
post_mu3 <- post_mu3[, c(1, 2, 6)]
names(post_mu3)[3] <- "yhat"
post_mu3$Param <- "Spectral Skewness"
post <- rbind(post, post_mu3)


### Spectral Kurtosis

# ML reference
m_mu4_ml <- lmer(mu4 ~ POA*Position + VF +
                 (1 + POA + Position | Speaker),
                 data=dc)

# main Bayesian model
m_mu4 <- brm(mu4 ~ POA*Position + VF +
             (1 + POA + Position | Speaker),
             data=dc, chains=2, cores=2, warmup=1000, iter=3000,
             prior=prior("normal(0, 10)", class="b"),
             backend="cmdstanr")

plot(m_mu4)
saveRDS(m_mu4, "models/model_01_mu4.rds")
m_mu4 <- readRDS("models/model_01_mu4.rds")

pairs(emmeans(m_mu4, ~ POA | Position, epred=TRUE))

emm_mu4 <- emmeans(m_mu4, ~ POA * Position, epred=TRUE)
post_mu4 <- tidybayes::gather_emmeans_draws(emm_mu4)
post_mu4 <- post_mu4[, c(1, 2, 6)]
names(post_mu4)[3] <- "yhat"
post_mu4$Param <- "Spectral Kurtosis"
post <- rbind(post, post_mu4)


## Plot
post$Param <- factor(post$Param,
                     levels=c("Amplitude (dB)",
                              "Peak Frequency (kHz)",
                              "Low-Freq. Tilt (dB/kHz)",
                              "High-Freq. Tilt (dB/kHz)",
                              "Spectral Mean (kHz)",
                              "Spectral Dispersion (kHz)",
                              "Spectral Skewness",
                              "Spectral Kurtosis"))


post$POA <- mapvalues(post$POA,
                      from=c("alveolar", "palatal", "retroflex"),
                      to=c("alv", "pal", "ret"))

saveRDS(post, "models/posterior_1.rds")
post <- readRDS("models/posterior_1.rds")

jgreen <- "#389826"
jblue <- "#4063D8"
jred <- "#CB3C33"

p <- ggplot(post, aes(x=yhat, colour=POA)) +
     facet_wrap(Param ~ Position, ncol=2, scales="free") +
     geom_density(linewidth=1) +
     scale_colour_manual(values=c(jgreen, jblue, jred)) +
     theme_bw() +
     labs(x="E(y)") +
     theme(legend.position="none",
           axis.title.y=element_blank(),
           panel.grid.major.y=element_blank(),
           panel.grid.minor.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           strip.background=element_blank(),
           strip.text.x=element_blank())

ggsave("plots/posterior_1.png", p, width=4.5, height=9,
       units="in", dpi=300)


post <- ddply(post, .(Param, Position, POA), transform,
              Sample = seq_along(yhat))

post_wide <- post %>%
                pivot_wider(names_from=POA, values_from=yhat)

post_wide$A_P <- post_wide$alv - post_wide$pal
post_wide$A_R <- post_wide$alv - post_wide$ret
post_wide$P_R <- post_wide$pal - post_wide$ret

post_con <- post_wide[, -c(4:6)] %>%
                pivot_longer(cols=c("A_P", "A_R", "P_R"),
                             names_to="Contrast",
                             values_to="Diff")

post_con$Contrast <- paste0(post_con$Position,
                            "_", post_con$Contrast)


post_con$Contrast <- factor(post_con$Contrast,
                            levels=c("VC_A_P", "VC_A_R", "VC_P_R",
                                     "CV_A_P", "CV_A_R", "CV_P_R"))

pd <- ddply(post_con,
            .(Param, Contrast),
            summarise,
            med = median(Diff),
            q975 = quantile(Diff, 0.975),
            q75 = quantile(Diff, 0.75),
            q25 = quantile(Diff, 0.25),
            q025 = quantile(Diff, 0.025))



p <- ggplot(pd, aes(x=Contrast, y=med)) +
     facet_wrap(~Param, ncol=1, scales="free") +
     annotate("text", x=-Inf, y=Inf, label="VCV", size=2.5,
              hjust=-0.2, vjust=1.5) +
     annotate("text", x=Inf, y=Inf, label="CV", size=2.5,
              hjust=1.2, vjust=1.5) +
     geom_hline(yintercept=0, color="gray", linetype="dashed") +
     geom_vline(xintercept=3.5) +
     geom_linerange(aes(ymin=q025, ymax=q975),
                    colour="#6baed6", linewidth=1.5) +
     geom_linerange(aes(ymin=q25, ymax=q75),
                    colour="#2171b5", linewidth=3) +
     scale_y_continuous(expand=expansion(0.3, 0)) +
     scale_x_discrete(labels=c("a-p", "a-r", "p-r",
                               "a-p", "a-r", "p-r")) +
     theme_bw() +
     theme(axis.title.y=element_blank(),
           strip.background=element_blank(),
           strip.text.x=element_blank(),
           panel.grid.major=element_blank(),
           panel.grid.minor=element_blank())

ggsave("plots/posterior-contrasts_1.png", p, width=2.25, height=9,
       units="in", dpi=300)

# Subject-wise contrasts
m <- readRDS("models/model_01_mu4.rds")

subj_hdi <- m %>%
    emmeans(~ POA*Position + Speaker,
            at = list(Speaker = levels(dc$Speaker)),
            epred=TRUE, re_formula=NULL) %>%
    contrast(method="pairwise", by="Speaker") %>%
    gather_emmeans_draws() %>%
    median_hdi()


subset(subj_hdi, contrast == "alveolar VC - palatal VC")

subset(subj_hdi, contrast == "alveolar VC - retroflex VC")

subset(subj_hdi, contrast == "palatal VC - retroflex VC")


subset(subj_hdi, contrast == "alveolar CV - palatal CV")

subset(subj_hdi, contrast == "alveolar CV - retroflex CV")

subset(subj_hdi, contrast == "palatal CV - retroflex CV")





