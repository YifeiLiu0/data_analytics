library(readr)
cr <- read_csv("cr.csv")

### method: DID

## OLS:
# assumption doesn't hold - not a good method
# endogeneity: rainfall is related to index

## IV:
# assumption doesn't hold - not a good method
# potential IVs (i.e. rainfall) are related to productivity directly

## RDD: line 145
# assumption doesn't hold - not a good method
# cutoff isn't exogenous (not quasi-random) because it's related to rainfall and so on

## DID: line 25
# assumption holds
# stability is good
# only use half of the dataset: may cause bias



### DID
library(plm)
library(dplyr)
library(stargazer)
library(huxtable)
library(lfe)
library(ggplot2)


## subset the data for DID
# transform the data into panel data
cr_panel <- pdata.frame(cr, index = c("id", "year"))

# group the data by id
cr_panel <- cr_panel %>% group_by(id)

# delete the group when treatment equals to 1 in 1996-2000
cr_sub <- cr_panel %>% filter(!any(year %in% 1996:2000 & treat == 1))

# divide the groups into two types: treatment and control
cr_sub <- cr_sub %>%
  mutate(type = ifelse(any(year %in% 2001:2005 & treat == 1), 
                       "treat", "control" )) %>%
  relocate(type, .after = treat)  

# Convert 'year' column to numeric
cr_sub$year <- as.numeric(as.character(cr_sub$year))

# add dummy variables
cr_sub$dummy_treat <- as.integer(cr_sub$type == "treat")
cr_sub$dummy_post <- as.integer(cr_sub$year >= 2001)
cr_sub$new_methods <- as.integer(cr_sub$type == "treat" & cr_sub$year >= 2001)


## various specifications of DID
# baseline model
did <- lm(prod ~ new_methods + dummy_treat + dummy_post, 
          data = cr_sub)

# include year and id fixed effects
did_fe <- felm(prod ~ new_methods + factor(id) + factor(year), 
                 data = cr_sub)

did_felm <- felm(prod ~ new_methods 
                 | id + year, 
                 data = cr_sub)
# cluster the standard errors
did_felm_clu <- felm(prod ~ new_methods 
                 | id + year
                 | 0 | id, 
                 data = cr_sub)

# delete rows with NA
cr_sub <- cr_sub[complete.cases(cr_sub), ]

# Include additional control variables
did_control <- felm(prod ~ new_methods 
                    + irrigation + craft_income + crop_div + rainfall_1
                    | id + year, 
                    data = cr_sub)
# cluster the standard errors
did_control_clu <- felm(prod ~ new_methods 
                    + irrigation + craft_income + crop_div + rainfall_1
                    | id + year
                    | 0 | id, 
                    data = cr_sub)

# add time trends
did_tt <- felm(prod ~ new_methods 
               + irrigation + craft_income + crop_div + rainfall_1
               | factor(id) + factor(year) + factor(id): year, 
               data = cr_sub)
# cluster the standard errors
did_tt_cluster <- felm(prod ~ new_methods 
               + irrigation + craft_income + crop_div + rainfall_1
               | factor(id) + factor(year) + factor(id): year
               | 0 | id, 
               data = cr_sub)

# summary
huxreg(
  "Baseline" = did,
  "FE" = did_fe,
  "Proj." = did_felm,
  "Cont." = did_control,
  "Trends" = did_tt,
  "Cluster" = did_cluster,
  coefs = "new_methods",
  statistics = c("N" = "nobs"))

huxreg(
  "Proj." = did_felm,
  "Proj_clu" = did_felm_clu,
  "Cont." = did_control,
  "Cont_clu" = did_control_clu,
  "Trends" = did_tt,
  "Trends_clu" = did_tt_cluster,
  coefs = "new_methods",
  statistics = c("N" = "nobs"))


## assumption test
trends <- aggregate(cr_sub["prod"],
                    by = list("year" = cr_sub$year, "treat" = cr_sub$dummy_treat),
                    FUN = "mean")

trend_treat <- subset(trends, treat==1)
trend_nottreat <- subset(trends, treat==0)

plot(trend_treat$prod ~ trend_treat$year, type="b", col = "red",
  ylim = c(-4, 5), ylab = "Productivity", xlab = "Year")
lines(trend_nottreat$prod ~ trend_nottreat$year,
  type="b", col = "blue")
abline(v=2001)
legend("bottomleft", legend = c("Treat", "Control"),
  col = c("red", "blue"), pch = 1)





### Appendix
### RDD
library(stats)
library(stargazer)
library(readr)


## plot of variables against means_index
min(cr$means_index)
max(cr$means_index)
bin <- seq(from = 0, to = 100)

# productivity against means_index
prod <- tapply(cr$prod, cut(cr$means_index, seq(0, 100)), mean)
plot(bin, prod, 
     main="Mean value of productivity as a function of means_index", 
     xlab="means_index bin", ylab="productivity mean in the bin")

# rainfall_2 against means_index
rainfall_2 <- tapply(cr$rainfall_2, cut(cr$means_index, seq(0, 100)), mean)
plot(bin, rainfall_2, 
     main="Mean value of rainfall_2 as a function of means_index", 
     xlab="means_index bin", ylab="rainfall_2 mean in the bin")


## RDD: bandwidth 5
# recenter means_index to 0
cr$means_rescaled <- cr$means_index - 65
cr$inter <- cr$means_rescaled * cr$treat

# RDD with a bandwidth of 5 (assume CEFs are linear)
bd5 <- subset(cr, means_index>=60 & means_index<70)
reg_5 <- lm(bd5$prod ~ bd5$means_rescaled + bd5$treat + bd5$inter)

# summary
summary(reg_5)
stargazer(reg_5, out = "result_RDD.html")
