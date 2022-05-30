# 종이헬리콥터 실험 #

library(FrF2)
set.seed(527)
design=FrF2(16,6, generators=c("ABC","ABD"), randomize = F, default.levels = c(0,1),
            factor.names = c("Wing.Length","Body.Length","Body.Width","Folded.Wings","Taped.Body","Taped.Wings"))


y <- c(3.55, 4.47, 4.42, 7.74, 3.42, 5.14, 4.66, 3.88, 3.68, 3.95, 3.75, 7.02, 3.95, 4.14, 2.84, 4.46)

design <- add.response(design, y)
design

fit <-lm(y ~ .,data=design)
summary(fit)
anova(fit)

MEPlot(design)
IAPlot(design,lwd = 2, cex = 2, cex.xax = 1.2, cex.lab = 1.5)

plot(fit,which=1,add.smooth=FALSE)
plot(fit, which = 2)

res <- rstudent(fit)

#이상치
res[abs(res)>qt(0.957,8)]

#정규성 검정
shapiro.test(res)
hist(res)

#
summary(influence.measures(fit))
