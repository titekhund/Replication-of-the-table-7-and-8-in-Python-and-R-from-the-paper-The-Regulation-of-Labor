
setwd("G:\\My Drive\\econometris_everything\\Teaching econometrics\\2021_22\\jamee_2021\\World Bank project")

## World Bank Project: Replications of Table VII and VIII  

install.packages("sjPlot")
library("sjPlot")
library("stargazer")

## Table VII 

data1 <-  readxl::read_excel("dataset-wb_labor-regulation_v2-one-sheet.xls")

data2<- data.frame(data1$index_labor7a,data1$index_industrial4a, data1$index_socseca,data1$all_indexn_e, data1$all_indexn_c,data1$proc_99b, data1$time_99b,
data1$cost_99b)

colnames(data2)<-c( "Employment laws index", "Collective relations laws index", "Social security laws index", "Court formalism index for the eviction of
a nonpaying tenant", "Court formalism index for the collection of
a bounced check", "Log (number of steps to start a business)", "Log (number of days to start a business)", "Log (cost to start a business/GDP per
capita)") 
  
tab_corr(data2,
         file="tables.doc", na.deletion = c("pairwise"), 
         corr.method = c("pearson"), 
         digits = 5, triangle = "lower", 
         title = "CORRELATIONS BETWEEN REGULATION INDICES")


# VIII 

mod1 <- lm(av_unem_9100~ index_labor7a+ avg_yrsc25_9500, data=data1)

summary(mod1)

mod2 <- lm(av_unem_9100~  index_industrial4a + avg_yrsc25_9500, data=data1)

mod3 <-lm(av_unem_9100~ index_socseca + avg_yrsc25_9500, data=data1)


mod4 <- lm(rat_mal2024 ~ index_labor7a+ avg_yrsc25_9500, data=data1)

mod5 <- lm(rat_mal2024 ~  index_industrial4a + avg_yrsc25_9500, data=data1)

mod6 <-lm(rat_mal2024~ index_socseca + avg_yrsc25_9500, data=data1)



stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
          type="html",
          title = "Partial Replication - Table VIII, REGULATION OF LABOR AND OUTCOMES",
          align=TRUE,
          out="star_linear_2.doc",
          intercept.bottom = F,
          intercept.top = T,
          ci = T, digits=2,
          notes = "This is a caption.",
          model.names = T,
          single.row = T,
          covariate.labels = c("Constant", "Employment laws index", 
                               "Collective relations laws index", 
                               "Social security laws index",
                               "average years of schooling"))











