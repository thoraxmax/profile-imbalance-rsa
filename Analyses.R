library(lavaan)
library(haven)
library(semTools)
library(dplyr)
library(magrittr)
library(psych)

setwd("/Users/ma8505ce/Desktop/Artiklar/BESSI/Artikel 0 - allt i ett")

order<-c("chisq.scaled","df.scaled","pvalue.scaled",
         "rmsea.scaled", "cfi.scaled","srmr")

alldata_correct <- read_sav("Data/alldata_correct.sav")
df_retest <- read_sav("Data/test_retest.sav")
df <- alldata_correct

################################################
########
########
######## HiPIC
########
########
################################################


mod01 <- '
neuro =~ HiPIC_01_b + HiPIC_06_b + HiPIC_14_b + HiPIC_17_b + HiPIC_22_b + rev_hipic27
extra =~ HiPIC_08_b + HiPIC_10_b + HiPIC_18_b + HiPIC_19_b + HiPIC_23_b + HiPIC_25_b
agree =~ HiPIC_04_b + HiPIC_28_b + rev_hipic05 + rev_hipic11 + rev_hipic16 + rev_hipic21
consc =~ HiPIC_03_b + HiPIC_15_b + HiPIC_26_b + rev_hipic07 + rev_hipic09 + rev_hipic13
open =~ HiPIC_02_b + HiPIC_12_b + HiPIC_20_b + HiPIC_24_b + HiPIC_29_b + HiPIC_30_b
'
fitmod1 <- cfa(mod01, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod1, fit.measures = order)
#  chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#     1787.870       395.000         0.000         0.095         0.794         0.103
summary(fitmod1, std=T)


# Remove reversed rev_hipic27 from neuroticism
# divide agreeableness and conscientiousness into reversed and non-reversed items
mod02 <- '
neuro =~ HiPIC_01_b + HiPIC_06_b + HiPIC_14_b + HiPIC_17_b + HiPIC_22_b
extra =~ HiPIC_08_b + HiPIC_10_b + HiPIC_18_b + HiPIC_19_b + HiPIC_23_b + HiPIC_25_b
agree1 =~ HiPIC_04_b + HiPIC_28_b
agree2 =~ rev_hipic05 + rev_hipic11 + rev_hipic16 + rev_hipic21
consc1 =~ HiPIC_03_b + HiPIC_15_b + HiPIC_26_b
consc2 =~ rev_hipic07 + rev_hipic09 + rev_hipic13
open =~ HiPIC_02_b + HiPIC_12_b + HiPIC_20_b + HiPIC_24_b + HiPIC_29_b + HiPIC_30_b
'
fitmod2 <- cfa(mod02, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod2, fit.measures = order)
#   chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#       1376.277       356.000         0.000         0.086         0.849         0.090 
summary(fitmod2, std=T)

# keep the factors with most adequate loadings, ie agree2 & consc1
mod03 <- '
neuro =~ HiPIC_01_b + HiPIC_06_b + HiPIC_14_b + HiPIC_17_b + HiPIC_22_b
extra =~ HiPIC_08_b + HiPIC_10_b + HiPIC_18_b + HiPIC_19_b + HiPIC_23_b + HiPIC_25_b
agree2 =~ rev_hipic05 + rev_hipic11 + rev_hipic16 + rev_hipic21
consc1 =~ HiPIC_03_b + HiPIC_15_b + HiPIC_26_b
open =~ HiPIC_02_b + HiPIC_12_b + HiPIC_20_b + HiPIC_24_b + HiPIC_29_b + HiPIC_30_b
'
fitmod3 <- cfa(mod03, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod3, fit.measures = order)
#   chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#     1158.778       242.000         0.000         0.098         0.862         0.092 
summary(fitmod3, std=T)

mods_fitmod3 <- modificationindices(fitmod3)
head(mods_fitmod3[order(-mods_fitmod3$mi), ], 10)


#           lhs op        rhs      mi    epc sepc.lv sepc.all sepc.nox
#249      extra =~ HiPIC_30_b 275.485  2.074   0.909    0.909    0.909
#576 HiPIC_12_b ~~ HiPIC_24_b 173.056  0.505   0.505    0.623    0.623
#506 HiPIC_25_b ~~ HiPIC_30_b 151.662  0.466   0.466    0.730    0.730
#573 HiPIC_02_b ~~ HiPIC_29_b 134.961  0.568   0.568    2.369    2.369
#216      neuro =~ HiPIC_19_b 125.183 -0.938  -0.763   -0.763   -0.763
#231      neuro =~ HiPIC_30_b 110.160 -0.503  -0.409   -0.409   -0.409
#432 HiPIC_10_b ~~ HiPIC_18_b  88.836  0.365   0.365    0.727    0.727
#574 HiPIC_02_b ~~ HiPIC_30_b  64.125 -0.475  -0.475   -1.253   -1.253
#316 HiPIC_01_b ~~ HiPIC_19_b  60.606 -0.290  -0.290   -0.892   -0.892
#228      neuro =~ HiPIC_20_b  59.046 -0.344  -0.280   -0.280   -0.280


### Lots of theoretically justified correlated residuals
mod04 <- '
neuro =~ HiPIC_01_b + HiPIC_06_b + HiPIC_14_b + HiPIC_17_b + HiPIC_22_b
extra =~ HiPIC_08_b + HiPIC_10_b + HiPIC_18_b + HiPIC_19_b + HiPIC_23_b + HiPIC_25_b
agree2 =~ rev_hipic05 + rev_hipic11 + rev_hipic16 + rev_hipic21
consc1 =~ HiPIC_03_b + HiPIC_15_b + HiPIC_26_b
open =~ HiPIC_02_b + HiPIC_12_b + HiPIC_20_b + HiPIC_24_b + HiPIC_29_b + HiPIC_30_b

HiPIC_12_b ~~ HiPIC_24_b
HiPIC_25_b ~~ HiPIC_30_b
HiPIC_02_b ~~ HiPIC_29_b
HiPIC_10_b ~~ HiPIC_18_b
HiPIC_01_b ~~ HiPIC_19_b
'
fitmod4 <- cfa(mod04, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod4, fit.measures = order)
# chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#   717.179       237.000         0.000         0.072         0.928         0.075 
summary(fitmod4, std=T)

semTools::reliability(fitmod4)
#             neuro     extra    agree2    consc1      open
#alpha.ord 0.8815393 0.7252085 0.5762436 0.7954198 0.7490198


###### ###### ###### ###### 
###### Test-retest
###### ###### ###### ###### 
df_retest <- df_retest %>%
  mutate(
    neuro    = rowMeans(across(c(HiPIC_01_b, HiPIC_06_b, HiPIC_14_b, HiPIC_17_b, HiPIC_22_b)), na.rm = TRUE),
    extra = rowMeans(across(c(HiPIC_08_b, HiPIC_10_b, HiPIC_18_b, HiPIC_19_b, HiPIC_23_b, HiPIC_25_b)), na.rm = TRUE),
    agree2      = rowMeans(across(c(HiPIC_05_b, HiPIC_11_b, HiPIC_16_b, HiPIC_21_b)), na.rm = TRUE),
    consc1      = rowMeans(across(c(HiPIC_03_b, HiPIC_15_b, HiPIC_26_b)), na.rm = TRUE),
    open      = rowMeans(across(c(HiPIC_02_b, HiPIC_12_b, HiPIC_20_b, HiPIC_24_b, HiPIC_29_b, HiPIC_30_b)), na.rm = TRUE),
    neuro_retest    = rowMeans(across(c(HiPIC_01_retest, HiPIC_06_retest, HiPIC_14_retest, HiPIC_17_retest, HiPIC_22_retest)), na.rm = TRUE),
    extra_retest  = rowMeans(across(c(HiPIC_08_retest, HiPIC_10_retest, HiPIC_18_retest, HiPIC_19_retest, HiPIC_23_retest, HiPIC_25_retest)), na.rm = TRUE),
    agree2_retest       = rowMeans(across(c(HiPIC_05_retest, HiPIC_11_retest, HiPIC_16_retest, HiPIC_21_retest)), na.rm = TRUE),
    consc1_retest       = rowMeans(across(c(HiPIC_03_retest, HiPIC_15_retest, HiPIC_26_retest)), na.rm = TRUE),
    open_retest       = rowMeans(across(c(HiPIC_02_retest, HiPIC_12_retest, HiPIC_20_retest, HiPIC_24_retest, HiPIC_29_retest, HiPIC_30_retest)), na.rm = TRUE)
    )

data_icc <- df_retest[, c("neuro", "neuro_retest")]
ICC(data_icc) # neuroticism
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.84 12  77  77 8.8e-23        0.76        0.90

data_icc <- df_retest[, c("extra", "extra_retest")]
ICC(data_icc) # extraversion
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.86 13  77  77 1.6e-24        0.79        0.91

data_icc <- df_retest[, c("agree2", "agree2_retest")]
ICC(data_icc) # agreeableness
#                         type  ICC  F df1 df2       p lower bound upper bound
# Single_fixed_raters      ICC3 0.097 1.2  77  77 0.2       -0.13        0.31

data_icc <- df_retest[, c("consc1", "consc1_retest")]
ICC(data_icc) # conscientiousness
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.82 9.9  77  77 2.7e-20        0.73        0.88

data_icc <- df_retest[, c("open", "open_retest")]
ICC(data_icc) # openness
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.87 14  77  77 1.2e-25        0.80        0.92










################################################
########
########
######## BESSI
########
########
################################################


## Start with full model with all factors
model_full <- '
self_time =~ bessi_03_b + bessi_30_b + bessi_57_b
self_organiz =~ bessi_05_b + bessi_32_b + bessi_59_b
self_consistent =~ bessi_08_b + bessi_35_b + bessi_62_b + bessi_82_b
self_tasks =~ bessi_11_b + bessi_38_b + bessi_65_b
self_detail =~ bessi_14_b + bessi_41_b + bessi_68_b
self_rules =~ bessi_16_b + bessi_43_b + bessi_70_b
self_decision =~ bessi_24_b + bessi_51_b + bessi_78_b
self_goals =~ bessi_21_b + bessi_48_b + bessi_75_b
self_resp =~ bessi_19_b + bessi_46_b + bessi_73_b

soceng_lead =~ bessi_01_b + bessi_28_b + bessi_55_b + bessi_83_b
soceng_convince =~ bessi_12_b + bessi_39_b + bessi_66_b
soceng_express =~ bessi_15_b + bessi_42_b + bessi_69_b
soceng_contact =~ bessi_22_b + bessi_49_b + bessi_76_b
socengself_energy =~ bessi_06_b + bessi_33_b + bessi_60_b + bessi_84_b

coop_understand =~ bessi_02_b + bessi_29_b + bessi_56_b
coop_trust =~ bessi_07_b + bessi_34_b + bessi_61_b
coop_warmth =~ bessi_13_b + bessi_40_b + bessi_67_b
coop_collab =~ bessi_20_b + bessi_47_b + bessi_74_b
coopself_ethics =~ bessi_25_b + bessi_52_b + bessi_79_b

emotion_worry =~ bessi_04_b + bessi_31_b + bessi_58_b + bessi_85_b
emotion_optimism =~ bessi_10_b + bessi_37_b + bessi_64_b
emotion_anger =~ bessi_18_b + bessi_45_b + bessi_72_b
emotion_selfeff =~ bessi_23_b + bessi_50_b + bessi_77_b
emotionself_impulse =~ bessi_26_b + bessi_53_b + bessi_80_b

comp_selfref =~ bessi_09_b + bessi_36_b + bessi_63_b
comp_adapt =~ bessi_17_b + bessi_44_b + bessi_71_b
comp_indep =~ bessi_27_b + bessi_54_b + bessi_81_b + bessi_86_b'

fitmodel_full <- cfa(model_full, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmodel_full, fit.measures = order)
# chisq.scaled  df.scaled   pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled   srmr
# 5455.146      3218.000         0.000         0.042         0.908         0.896         0.064
summary(fitmodel_full, std=T)

mods_fitmodel_full <- modificationindices(fitmodel_full)
head(mods_fitmodel_full[order(-mods_fitmodel_full$mi), ], 10)
#                   lhs op        rhs      mi   epc sepc.lv sepc.all sepc.nox
#1828         self_resp =~ bessi_71_b 296.032 0.541   0.427    0.427    0.427
#2233 socengself_energy =~ bessi_26_b 294.743 0.463   0.363    0.363    0.363
#1330   self_consistent =~ bessi_71_b 287.991 0.640   0.400    0.400    0.400
#1405        self_tasks =~ bessi_26_b 287.894 0.574   0.412    0.412    0.412
#1168         self_time =~ bessi_81_b 286.691 1.288   0.577    0.577    0.577
#1413        self_tasks =~ bessi_71_b 284.508 0.576   0.414    0.414    0.414
#2241 socengself_energy =~ bessi_71_b 279.428 0.592   0.464    0.464    0.464
#1496       self_detail =~ bessi_71_b 270.626 0.528   0.390    0.390    0.390
#2656   coopself_ethics =~ bessi_71_b 263.547 0.765   0.568    0.568    0.568
#3153      comp_selfref =~ bessi_71_b 258.147 0.706   0.543    0.543    0.543


#Add one modification [self_resp =~ bessi_71_b]: "Anpassa mig till förändring" på ansvarstagande = rimligt
model_full_mod1 <- '
self_time =~ bessi_03_b + bessi_30_b + bessi_57_b
self_organiz =~ bessi_05_b + bessi_32_b + bessi_59_b
self_consistent =~ bessi_08_b + bessi_35_b + bessi_62_b + bessi_82_b
self_tasks =~ bessi_11_b + bessi_38_b + bessi_65_b
self_detail =~ bessi_14_b + bessi_41_b + bessi_68_b
self_rules =~ bessi_16_b + bessi_43_b + bessi_70_b
self_decision =~ bessi_24_b + bessi_51_b + bessi_78_b
self_goals =~ bessi_21_b + bessi_48_b + bessi_75_b
self_resp =~ bessi_19_b + bessi_46_b + bessi_73_b + bessi_71_b

soceng_lead =~ bessi_01_b + bessi_28_b + bessi_55_b + bessi_83_b
soceng_convince =~ bessi_12_b + bessi_39_b + bessi_66_b
soceng_express =~ bessi_15_b + bessi_42_b + bessi_69_b
soceng_contact =~ bessi_22_b + bessi_49_b + bessi_76_b
socengself_energy =~ bessi_06_b + bessi_33_b + bessi_60_b + bessi_84_b

coop_understand =~ bessi_02_b + bessi_29_b + bessi_56_b
coop_trust =~ bessi_07_b + bessi_34_b + bessi_61_b
coop_warmth =~ bessi_13_b + bessi_40_b + bessi_67_b
coop_collab =~ bessi_20_b + bessi_47_b + bessi_74_b
coopself_ethics =~ bessi_25_b + bessi_52_b + bessi_79_b

emotion_worry =~ bessi_04_b + bessi_31_b + bessi_58_b + bessi_85_b
emotion_optimism =~ bessi_10_b + bessi_37_b + bessi_64_b
emotion_anger =~ bessi_18_b + bessi_45_b + bessi_72_b
emotion_selfeff =~ bessi_23_b + bessi_50_b + bessi_77_b
emotionself_impulse =~ bessi_26_b + bessi_53_b + bessi_80_b

comp_selfref =~ bessi_09_b + bessi_36_b + bessi_63_b
comp_adapt =~ bessi_17_b + bessi_44_b + bessi_71_b
comp_indep =~ bessi_27_b + bessi_54_b + bessi_81_b + bessi_86_b'

fitmodel_full_mod1 <- cfa(model_full_mod1, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmodel_full_mod1, fit.measures = order)
# chisq.scaled  df.scaled   pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled   srmr
# 5333.052      3217.000         0.000         0.041         0.913         0.901         0.063
summary(fitmodel_full_mod1, std=T)

mods_fitmodel_full_mod1 <- modificationindices(fitmodel_full_mod1)
head(mods_fitmodel_full_mod1[order(-mods_fitmodel_full_mod1$mi), ], 10)

#                   lhs op        rhs      mi   epc sepc.lv sepc.all sepc.nox
# 2234 socengself_energy =~ bessi_26_b 295.213 0.463   0.363    0.363    0.363
# 1407        self_tasks =~ bessi_26_b 288.274 0.574   0.412    0.412    0.412
# 1169         self_time =~ bessi_81_b 285.266 1.279   0.576    0.576    0.576
# 3260        comp_indep =~ bessi_24_b 254.288 0.624   0.455    0.455    0.455
# 1324   self_consistent =~ bessi_26_b 247.669 0.668   0.418    0.418    0.418
# 1583        self_rules =~ bessi_81_b 243.184 0.597   0.442    0.442    0.442
# 1852       soceng_lead =~ bessi_24_b 238.328 0.451   0.351    0.351    0.351
# 2731     emotion_worry =~ bessi_26_b 230.625 0.373   0.288    0.288    0.288
# 1687        self_goals =~ bessi_24_b 229.163 0.789   0.522    0.522    0.522
# 1739        self_goals =~ bessi_26_b 222.241 0.528   0.349    0.349    0.349

#Add one modification [self_time =~ bessi_81_b]: Få saker gjorda på egen hand på tid = rimligt
model_full_mod2 <- '
self_time =~ bessi_03_b + bessi_30_b + bessi_57_b + bessi_81_b
self_organiz =~ bessi_05_b + bessi_32_b + bessi_59_b
self_consistent =~ bessi_08_b + bessi_35_b + bessi_62_b + bessi_82_b
self_tasks =~ bessi_11_b + bessi_38_b + bessi_65_b
self_detail =~ bessi_14_b + bessi_41_b + bessi_68_b
self_rules =~ bessi_16_b + bessi_43_b + bessi_70_b
self_decision =~ bessi_24_b + bessi_51_b + bessi_78_b
self_goals =~ bessi_21_b + bessi_48_b + bessi_75_b
self_resp =~ bessi_19_b + bessi_46_b + bessi_73_b + bessi_71_b

soceng_lead =~ bessi_01_b + bessi_28_b + bessi_55_b + bessi_83_b
soceng_convince =~ bessi_12_b + bessi_39_b + bessi_66_b
soceng_express =~ bessi_15_b + bessi_42_b + bessi_69_b
soceng_contact =~ bessi_22_b + bessi_49_b + bessi_76_b
socengself_energy =~ bessi_06_b + bessi_33_b + bessi_60_b + bessi_84_b

coop_understand =~ bessi_02_b + bessi_29_b + bessi_56_b
coop_trust =~ bessi_07_b + bessi_34_b + bessi_61_b
coop_warmth =~ bessi_13_b + bessi_40_b + bessi_67_b
coop_collab =~ bessi_20_b + bessi_47_b + bessi_74_b
coopself_ethics =~ bessi_25_b + bessi_52_b + bessi_79_b

emotion_worry =~ bessi_04_b + bessi_31_b + bessi_58_b + bessi_85_b
emotion_optimism =~ bessi_10_b + bessi_37_b + bessi_64_b
emotion_anger =~ bessi_18_b + bessi_45_b + bessi_72_b
emotion_selfeff =~ bessi_23_b + bessi_50_b + bessi_77_b
emotionself_impulse =~ bessi_26_b + bessi_53_b + bessi_80_b

comp_selfref =~ bessi_09_b + bessi_36_b + bessi_63_b
comp_adapt =~ bessi_17_b + bessi_44_b + bessi_71_b
comp_indep =~ bessi_27_b + bessi_54_b + bessi_81_b + bessi_86_b'

fitmodel_full_mod2 <- cfa(model_full_mod2, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmodel_full_mod2, fit.measures = order)
# chisq.scaled  df.scaled   pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled   srmr
# 5211.370      3216.000         0.000         0.040         0.918         0.907         0.062 
summary(fitmodel_full_mod2, std=T)


semTools::reliability(fitmodel_full)

######################################################
###### Narrow model
######################################################

## Do narrower model where "unclear" factors are omitted
model_narrow <- '
self_time =~ bessi_03_b + bessi_30_b + bessi_57_b
self_organiz =~ bessi_05_b + bessi_32_b + bessi_59_b
self_consistent =~ bessi_08_b + bessi_35_b + bessi_62_b + bessi_82_b
self_tasks =~ bessi_11_b + bessi_38_b + bessi_65_b
self_detail =~ bessi_14_b + bessi_41_b + bessi_68_b
self_rules =~ bessi_16_b + bessi_43_b + bessi_70_b
self_decision =~ bessi_24_b + bessi_51_b + bessi_78_b
self_goals =~ bessi_21_b + bessi_48_b + bessi_75_b
self_resp =~ bessi_19_b + bessi_46_b + bessi_73_b

soceng_lead =~ bessi_01_b + bessi_28_b + bessi_55_b + bessi_83_b
soceng_convince =~ bessi_12_b + bessi_39_b + bessi_66_b
soceng_express =~ bessi_15_b + bessi_42_b + bessi_69_b
soceng_contact =~ bessi_22_b + bessi_49_b + bessi_76_b

coop_understand =~ bessi_02_b + bessi_29_b + bessi_56_b
coop_trust =~ bessi_07_b + bessi_34_b + bessi_61_b
coop_warmth =~ bessi_13_b + bessi_40_b + bessi_67_b
coop_collab =~ bessi_20_b + bessi_47_b + bessi_74_b

emotion_worry =~ bessi_04_b + bessi_31_b + bessi_58_b + bessi_85_b
emotion_optimism =~ bessi_10_b + bessi_37_b + bessi_64_b
emotion_anger =~ bessi_18_b + bessi_45_b + bessi_72_b
emotion_selfeff =~ bessi_23_b + bessi_50_b + bessi_77_b'

fitmodel_narrow <- cfa(model_narrow, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmodel_narrow, fit.measures = order)
# chisq.scaled  df.scaled   pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled   srmr
# 3174.185      1869.000         0.000         0.042         0.935         0.926         0.058
summary(fitmodel_narrow, std=T)

#It's ok

## Add second-order structure to narrower model
model_narrow_so <- '
self_time =~ bessi_03_b + bessi_30_b + bessi_57_b
self_organiz =~ bessi_05_b + bessi_32_b + bessi_59_b
self_consistent =~ bessi_08_b + bessi_35_b + bessi_62_b + bessi_82_b
self_tasks =~ bessi_11_b + bessi_38_b + bessi_65_b
self_detail =~ bessi_14_b + bessi_41_b + bessi_68_b
self_rules =~ bessi_16_b + bessi_43_b + bessi_70_b
self_decision =~ bessi_24_b + bessi_51_b + bessi_78_b
self_goals =~ bessi_21_b + bessi_48_b + bessi_75_b
self_resp =~ bessi_19_b + bessi_46_b + bessi_73_b

soceng_lead =~ bessi_01_b + bessi_28_b + bessi_55_b + bessi_83_b
soceng_convince =~ bessi_12_b + bessi_39_b + bessi_66_b
soceng_express =~ bessi_15_b + bessi_42_b + bessi_69_b
soceng_contact =~ bessi_22_b + bessi_49_b + bessi_76_b

coop_understand =~ bessi_02_b + bessi_29_b + bessi_56_b
coop_trust =~ bessi_07_b + bessi_34_b + bessi_61_b
coop_warmth =~ bessi_13_b + bessi_40_b + bessi_67_b
coop_collab =~ bessi_20_b + bessi_47_b + bessi_74_b

emotion_worry =~ bessi_04_b + bessi_31_b + bessi_58_b + bessi_85_b
emotion_optimism =~ bessi_10_b + bessi_37_b + bessi_64_b
emotion_anger =~ bessi_18_b + bessi_45_b + bessi_72_b
emotion_selfeff =~ bessi_23_b + bessi_50_b + bessi_77_b

SELFM=~self_time+self_organiz+self_consistent+self_tasks+self_detail+self_rules+self_decision+self_goals+self_resp
EMRES=~emotion_worry+emotion_optimism+emotion_anger+emotion_selfeff
SOCENG=~soceng_lead+soceng_convince+soceng_express+soceng_contact
COOPER=~coop_understand+coop_trust+coop_warmth+coop_collab'

fitmodel_narrow_so <- cfa(model_narrow_so, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmodel_narrow_so, fit.measures = order)
# chisq.scaled  df.scaled   pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled   srmr
# 4335.489      2052.000         0.000         0.053         0.887         0.882         0.086  
summary(fitmodel_narrow_so, std=T)

mods_fitmodel_narrow_so <- modificationindices(fitmodel_narrow_so)
head(mods_fitmodel_narrow_so[order(-mods_fitmodel_narrow_so$mi), ], 10)
#                lhs op               rhs      mi    epc sepc.lv sepc.all sepc.nox
#4561  soceng_contact ~~          COOPER 327.983  0.084   0.583    0.583    0.583
#1399  soceng_contact =~      bessi_67_b 326.615  0.858   0.689    0.689    0.689
#4616   emotion_anger ~~          SOCENG 294.047 -0.172  -0.403   -0.403   -0.403
#4620 emotion_selfeff ~~          SOCENG 268.715  0.188   0.644    0.644    0.644
#4552  soceng_contact ~~     coop_warmth 237.321  0.185   2.922    2.922    2.922
#2100          SOCENG =~      bessi_67_b 231.887  1.258   0.855    0.855    0.855
#4558  soceng_contact ~~           SELFM 231.085 -0.064  -0.323   -0.323   -0.323
#4508     soceng_lead ~~ soceng_convince 224.361  0.199   0.909    0.909    0.909
#4454      self_rules ~~           EMRES 197.806 -0.123  -0.347   -0.347   -0.347
#4453      self_rules ~~           SELFM 197.782  0.118   0.564    0.564    0.564



items <- df[, c(
  "bessi_03_b", "bessi_30_b", "bessi_57_b",
  "bessi_05_b", "bessi_32_b", "bessi_59_b",
  "bessi_08_b", "bessi_35_b", "bessi_62_b",
  "bessi_82_b", "bessi_11_b", "bessi_38_b",
  "bessi_65_b", "bessi_14_b", "bessi_41_b",
  "bessi_68_b", "bessi_16_b", "bessi_43_b",
  "bessi_70_b", "bessi_24_b", "bessi_51_b",
  "bessi_78_b", "bessi_21_b", "bessi_48_b",
  "bessi_75_b", "bessi_19_b", "bessi_46_b",
  "bessi_73_b"
)]
# Cronbach's alpha
alpha_result <- psych::alpha(items)
# View results
alpha_result #.93 Self-management




items2 <- df[, c(
  "bessi_01_b", "bessi_28_b", "bessi_55_b", "bessi_83_b",
  "bessi_12_b", "bessi_39_b", "bessi_66_b",
  "bessi_15_b", "bessi_42_b", "bessi_69_b",
  "bessi_22_b", "bessi_49_b", "bessi_76_b"
)]
# Cronbach's alpha
alpha_result2 <- psych::alpha(items2)
# View results
alpha_result2 #.89 Social engagement




items3 <- df[, c(
  "bessi_02_b", "bessi_29_b", "bessi_56_b",
  "bessi_07_b", "bessi_34_b", "bessi_61_b",
  "bessi_13_b", "bessi_40_b", "bessi_67_b",
  "bessi_20_b", "bessi_47_b", "bessi_74_b"
)]
# Cronbach's alpha
alpha_result3 <- psych::alpha(items3)
# View results
alpha_result3 #.81 Cooperation



items4 <- df[, c(
  "bessi_04_b", "bessi_31_b", "bessi_58_b", "bessi_85_b",
  "bessi_10_b", "bessi_37_b", "bessi_64_b",
  "bessi_18_b", "bessi_45_b", "bessi_72_b",
  "bessi_23_b", "bessi_50_b", "bessi_77_b"
)]

# Cronbach's alpha
alpha_result4 <- psych::alpha(items4)
# View results
alpha_result4 #.91 Emotional resilience

###### ###### ###### ###### 
###### Test-retest
###### ###### ###### ###### 
df_retest <- df_retest %>%
  mutate(
    self    = rowMeans(across(c(bessi_03_b, bessi_30_b, bessi_57_b, bessi_05_b, bessi_32_b, bessi_59_b, bessi_08_b, bessi_35_b, bessi_62_b, bessi_82_b, bessi_11_b, bessi_38_b, bessi_65_b,bessi_14_b, bessi_41_b, bessi_68_b, bessi_16_b, bessi_43_b, bessi_70_b, bessi_24_b, bessi_51_b, bessi_78_b, bessi_21_b, bessi_48_b, bessi_75_b, bessi_19_b, bessi_46_b, bessi_73_b)), na.rm = TRUE),
    soceng = rowMeans(across(c(bessi_01_b, bessi_28_b, bessi_55_b, bessi_83_b, bessi_12_b, bessi_39_b, bessi_66_b, bessi_15_b, bessi_42_b, bessi_69_b, bessi_22_b, bessi_49_b, bessi_76_b)), na.rm = TRUE),
    coop      = rowMeans(across(c(bessi_02_b, bessi_29_b, bessi_56_b, bessi_07_b, bessi_34_b, bessi_61_b, bessi_13_b, bessi_40_b, bessi_67_b, bessi_20_b, bessi_47_b, bessi_74_b)), na.rm = TRUE),
    emo      = rowMeans(across(c(bessi_04_b, bessi_31_b, bessi_58_b, bessi_85_b, bessi_10_b, bessi_37_b, bessi_64_b, bessi_18_b, bessi_45_b, bessi_72_b, bessi_23_b, bessi_50_b, bessi_77_b)), na.rm = TRUE),
    self_retest    = rowMeans(across(c(bessi_03_retest, bessi_30_retest, bessi_57_retest, bessi_05_retest, bessi_32_retest, bessi_59_retest, bessi_08_retest, bessi_35_retest, bessi_62_retest, bessi_82_retest, bessi_11_retest, bessi_38_retest, bessi_65_retest,bessi_14_retest, bessi_41_retest, bessi_68_retest, bessi_16_retest, bessi_43_retest, bessi_70_retest, bessi_24_retest, bessi_51_retest, bessi_78_retest, bessi_21_retest, bessi_48_retest, bessi_75_retest, bessi_19_retest, bessi_46_retest, bessi_73_retest)), na.rm = TRUE),
    soceng_retest  = rowMeans(across(c(bessi_01_retest, bessi_28_retest, bessi_55_retest, bessi_83_retest, bessi_12_retest, bessi_39_retest, bessi_66_retest, bessi_15_retest, bessi_42_retest, bessi_69_retest, bessi_22_retest, bessi_49_retest, bessi_76_retest)), na.rm = TRUE),
    coop_retest       = rowMeans(across(c(bessi_02_retest, bessi_29_retest, bessi_56_retest, bessi_07_retest, bessi_34_retest, bessi_61_retest, bessi_13_retest, bessi_40_retest, bessi_67_retest, bessi_20_retest, bessi_47_retest, bessi_74_retest)), na.rm = TRUE),
    emo_retest       = rowMeans(across(c(bessi_04_retest, bessi_31_retest, bessi_58_retest, bessi_85_retest, bessi_10_retest, bessi_37_retest, bessi_64_retest, bessi_18_retest, bessi_45_retest, bessi_72_retest, bessi_23_retest, bessi_50_retest, bessi_77_retest)), na.rm = TRUE)
  )


data_icc <- df_retest[, c("self", "self_retest")]
ICC(data_icc) # self-management
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.88 15  77  77 9.7e-27        0.82        0.92

data_icc <- df_retest[, c("soceng", "soceng_retest")]
ICC(data_icc) # social engagement
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.91 20  77  77 1.2e-30        0.86        0.94

data_icc <- df_retest[, c("coop", "coop_retest")]
ICC(data_icc) # cooperation
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.77 7.6  77  77 6.6e-17        0.66        0.85

data_icc <- df_retest[, c("emo", "emo_retest")]
ICC(data_icc) # emotional resilience
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.86 13  77  77 1.2e-24        0.79        0.91







################################################
########
########
######## E-SWAN-G
########
########
################################################

mod_narrow <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b
'

fitmod_narrow <- cfa(mod_narrow, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod_narrow, fit.measures = order)
# chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#      1467.877       436.000         0.000         0.078         0.911         0.074 
summary(fitmod_narrow, std=T)


mods_fitmod_narrow <- modificationindices(fitmod_narrow)
head(mods_fitmod_narrow[order(-mods_fitmod_narrow$mi), ], 10)

#           lhs op        rhs      mi    epc sepc.lv sepc.all sepc.nox
#521   interact =~ ESWAN_25_b 181.673  1.104   0.674    0.674    0.674
#409      drive =~ ESWAN_25_b 135.032  0.692   0.484    0.484    0.484
#461 mangdstrss =~ ESWAN_24_b 120.865 -0.314  -0.259   -0.259   -0.259
#351 strsscntxt =~ ESWAN_25_b 118.148  0.468   0.384    0.384    0.384
#408      drive =~ ESWAN_24_b 116.800 -0.509  -0.356   -0.356   -0.356
#520   interact =~ ESWAN_24_b 112.843 -0.689  -0.420   -0.420   -0.420
#350 strsscntxt =~ ESWAN_24_b 111.990 -0.375  -0.308   -0.308   -0.308
#321    fearsit =~ ESWAN_24_b 102.270 -0.512  -0.329   -0.329   -0.329
#462 mangdstrss =~ ESWAN_25_b  97.739  0.344   0.284    0.284    0.284
#458 mangdstrss =~ ESWAN_14_b  86.802  0.392   0.324    0.324    0.324

#### Add one modification [interact =~ ESWAN_25_b]: "Känna samhörighet med andra" också del av interact, vilket känns logiskt
mod_narrow_m1 <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b
'

fitmod_narrow_m1 <- cfa(mod_narrow_m1, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod_narrow_m1, fit.measures = order)
# chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#.  1327.364       435.000         0.000         0.072         0.923         0.069 
summary(fitmod_narrow_m1, std=T)


mods_fitmod_narrow_m1 <- modificationindices(fitmod_narrow_m1)
head(mods_fitmod_narrow_m1[order(-mods_fitmod_narrow_m1$mi), ], 10)
#524   interact =~ ESWAN_28_b 122.813  0.590   0.359    0.359    0.359
#459 mangdstrss =~ ESWAN_14_b  94.282  0.402   0.332    0.332    0.332
#413      drive =~ ESWAN_28_b  89.673  0.392   0.273    0.273    0.273
#355 strsscntxt =~ ESWAN_28_b  84.977  0.308   0.253    0.253    0.253
#341 strsscntxt =~ ESWAN_14_b  83.890  0.393   0.323    0.323    0.323
#399      drive =~ ESWAN_14_b  82.495  0.531   0.371    0.371    0.371
#966 ESWAN_22_b ~~ ESWAN_23_b  77.221  0.214   0.214    0.774    0.774
#312    fearsit =~ ESWAN_14_b  71.249  0.570   0.366    0.366    0.366
#511   interact =~ ESWAN_14_b  67.658  0.490   0.298    0.298    0.298
#453 mangdstrss =~ ESWAN_08_b  66.363 -0.373  -0.308   -0.308   -0.308


#### Add one modification [interact =~ ESWAN_28_b]: "Dela med mig av mina saker med andra" också del av interact, vilket känns logiskt
mod_narrow_m2 <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b
'

fitmod_narrow_m2 <- cfa(mod_narrow_m2, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod_narrow_m2, fit.measures = order)
# chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled          srmr 
#.    1243.359       434.000         0.000         0.069         0.930         0.065 
summary(fitmod_narrow_m2, std=T) # collabbond acting strange, weakly correlated with all other factors, where the overall pattern are clear correlations across factors

semTools::reliability(fitmod_narrow_m2)

mods_fitmod_narrow_m2 <- modificationindices(fitmod_narrow_m2)
head(mods_fitmod_narrow_m2[order(-mods_fitmod_narrow_m2$mi), ], 10)
#460 mangdstrss =~ ESWAN_14_b 98.389  0.406   0.335    0.335    0.335
#342 strsscntxt =~ ESWAN_14_b 87.857  0.398   0.328    0.328    0.328
#400      drive =~ ESWAN_14_b 87.255  0.538   0.376    0.376    0.376
#966 ESWAN_22_b ~~ ESWAN_23_b 77.034  0.214   0.214    0.773    0.773
#313    fearsit =~ ESWAN_14_b 74.579  0.580   0.372    0.372    0.372
#512   interact =~ ESWAN_14_b 72.740  0.496   0.304    0.304    0.304
#454 mangdstrss =~ ESWAN_08_b 69.792 -0.376  -0.310   -0.310   -0.310
#429 focusstill =~ ESWAN_11_b 54.085  0.522   0.311    0.311    0.311
#336 strsscntxt =~ ESWAN_08_b 53.786 -0.314  -0.258   -0.258   -0.258
#455 mangdstrss =~ ESWAN_09_b 51.859  0.301   0.248    0.248    0.248

semTools::reliability(fitmod_narrow_m2)

##########################################
##########################################
###### Second-order structure
##########################################
##########################################

## Two alternative models based on previous study
mod_so1 <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b
RELLEARN =~ focusstill + collabbond + interact + frust
INTDIST =~ drive + mangdstrss
FEAR =~ fearsit + strsscntxt'

mod_so2 <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b
RELLEARN =~ collabbond + interact
INTDIST =~ drive + mangdstrss + frust + focusstill
FEAR =~ fearsit + strsscntxt'

fitmod_so1 <- cfa(mod_so1, data = df, ordered = T, missing="pairwise")
fitmod_so2 <- cfa(mod_so2, data = df, ordered = T, missing="pairwise")

fitmeasures(fitmod_so1, fit.measures = order)
fitmeasures(fitmod_so2, fit.measures = order)
# chisq.scaled  df.scaled   pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled   srmr
# 1600.984       451.000         0.000         0.081         0.900         0.891         0.082  Model 1
# 1609.536       451.000         0.000         0.081         0.900         0.890         0.082  Model 2

summary(fitmod_so1, std=T)
summary(fitmod_so2, std=T)


#### Free the problematic colabbond from its factor for both models
mod_so3 <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b
RELLEARN =~ focusstill + interact + frust
INTDIST =~ drive + mangdstrss
FEAR =~ fearsit + strsscntxt'

mod_so4 <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b  + ESWAN_28_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b
RELLEARN =~  interact
INTDIST =~ drive + mangdstrss + frust + focusstill
FEAR =~ fearsit + strsscntxt'


fitmod_so3 <- cfa(mod_so3, data = df, ordered = T, missing="pairwise")
fitmod_so4 <- cfa(mod_so4, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod_so3, fit.measures = order)
fitmeasures(fitmod_so4, fit.measures = order)
# chisq.scaled  df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled          srmr 
# 1609.536       451.000         0.000         0.081         0.900         0.890         0.082  Model 3
# 1618.197       450.000         0.000         0.081         0.899         0.889         0.082   Model 4

summary(fitmod_so3, std=T)

# Nothing works great and the latent factors are extremely correlated. Try a bifactor model


mod_bifactor <- '
g =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b +
     ESWAN_04_b + ESWAN_05_b + ESWAN_06_b +
     ESWAN_07_b + ESWAN_08_b + ESWAN_09_b +
     ESWAN_10_b + ESWAN_11_b + ESWAN_12_b +
     ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b +
     ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b +
     ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b + ESWAN_28_b +
     ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b

fearsit    =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust      =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive      =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
collabbond =~ ESWAN_24_b + ESWAN_25_b + ESWAN_26_b + ESWAN_27_b + ESWAN_28_b
interact   =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b

g ~~ 0*fearsit
g ~~ 0*strsscntxt
g ~~ 0*frust
g ~~ 0*drive
g ~~ 0*focusstill
g ~~ 0*mangdstrss
g ~~ 0*collabbond
g ~~ 0*interact
fearsit ~~ 0*strsscntxt
fearsit ~~ 0*frust
fearsit ~~ 0*drive
fearsit ~~ 0*focusstill
fearsit ~~ 0*mangdstrss
fearsit ~~ 0*collabbond
fearsit ~~ 0*interact
strsscntxt ~~ 0*frust
strsscntxt ~~ 0*drive
strsscntxt ~~ 0*focusstill
strsscntxt ~~ 0*mangdstrss
strsscntxt ~~ 0*collabbond
strsscntxt ~~ 0*interact
frust ~~ 0*drive
frust ~~ 0*focusstill
frust ~~ 0*mangdstrss
frust ~~ 0*collabbond
frust ~~ 0*interact
drive ~~ 0*focusstill
drive ~~ 0*mangdstrss
drive ~~ 0*collabbond
drive ~~ 0*interact
focusstill ~~ 0*mangdstrss
focusstill ~~ 0*collabbond
focusstill ~~ 0*interact
mangdstrss ~~ 0*collabbond
mangdstrss ~~ 0*interact
collabbond ~~ 0*interact
'

fit_mod_bifactor <- cfa(mod_bifactor, data = df, ordered = T, missing="pairwise", std.lv    = TRUE)
fitmeasures(fit_mod_bifactor, fit.measures = order)
#chisq.scaled     df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled          srmr 
#  1545.362       430.000         0.000         0.081         0.903         0.889         0.079 
summary(fit_mod_bifactor, std=T)
# Messy


#### Estimate first-order factors and do an exploratory factor analysis
fitmod_narrow_m2 <- cfa(mod_narrow_m2, data = df, ordered = T, missing="pairwise")
fscores <- lavPredict(fitmod_narrow_m2)

fscores <- as.data.frame(fscores)

library(polycor)
library(nFactors)
library(lavaan)
library(semTools)
library(readxl)
library(psych)

corrp <- cor(fscores, use = "pairwise.complete.obs")
corrp
KMO(r=corrp)  # Overall MSA =  0.62
#fearsit strsscntxt      frust      drive focusstill mangdstrss collabbond   interact 
# 0.81       0.67       0.60       0.59       0.51       0.78       0.30       0.53 

nScree(x=corrp,model="factors", cor=T)
# noc naf nparallel nkaiser
# 3   1         3     3

# Extract three factors
print(fa(corrp,3,fm="pa",rotate="promax")$loadings,cut=0)

#Loadings:
#           PA1    PA3    PA2   
#fearsit     0.852 -0.135  0.116
#strsscntxt  0.825  0.235 -0.103
#frust       0.199 -0.247  0.991
#drive       0.247  0.758  0.108
#focusstill  0.024  0.115  0.510
#mangdstrss  0.670  0.189  0.257
#collabbond -0.190  0.333  0.207
#interact    0.251  1.059 -0.314


#### Try this using CFA
mod_efa <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b
EMOTIONS =~ fearsit + strsscntxt + mangdstrss
SOCIAL =~ drive + interact
NP =~ frust + focusstill'

fitmod_efa <- cfa(mod_efa, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod_efa, fit.measures = order)
# chisq.scaled  df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled          srmr 
# 1188.489       367.000         0.000         0.076         0.927         0.920         0.070   EFA-model
summary(fitmod_efa, std=T)




### Good enough, but how much drop in fit compared to only firts-order factors. Contrast to a model with only the first-order factors

mod_efa_fo <- '
fearsit =~ ESWAN_01_b + ESWAN_02_b + ESWAN_03_b
strsscntxt =~ ESWAN_04_b + ESWAN_05_b + ESWAN_06_b
frust =~ ESWAN_07_b + ESWAN_08_b + ESWAN_09_b
drive =~ ESWAN_10_b + ESWAN_11_b + ESWAN_12_b
focusstill =~ ESWAN_13_b + ESWAN_14_b + ESWAN_15_b + ESWAN_16_b
mangdstrss =~ ESWAN_17_b + ESWAN_18_b + ESWAN_19_b + ESWAN_20_b  + ESWAN_21_b + ESWAN_22_b + ESWAN_23_b
interact =~ ESWAN_29_b + ESWAN_30_b + ESWAN_31_b + ESWAN_32_b + ESWAN_25_b + ESWAN_28_b'

fitmod_efa_fo <- cfa(mod_efa_fo, data = df, ordered = T, missing="pairwise")
fitmeasures(fitmod_efa_fo, fit.measures = order)
# chisq.scaled  df.scaled pvalue.scaled  rmsea.scaled    cfi.scaled    tli.scaled          srmr 
# 1059.316       356.000         0.000         0.071         0.938         0.929         0.063   EFA-model
summary(fitmod_efa_fo, std=T)

### Good enough....

items <- df[, c(
  "ESWAN_01_b", "ESWAN_02_b", "ESWAN_03_b", "ESWAN_04_b", "ESWAN_05_b",
  "ESWAN_06_b", "ESWAN_17_b", "ESWAN_18_b", "ESWAN_19_b", "ESWAN_20_b",
  "ESWAN_21_b", "ESWAN_22_b", "ESWAN_23_b")]
alpha_result <- psych::alpha(items)
alpha_result # 0.90, Manage emotions

items2 <- df[, c(
  "ESWAN_07_b", "ESWAN_08_b", "ESWAN_09_b",
  "ESWAN_13_b", "ESWAN_14_b", "ESWAN_15_b", "ESWAN_16_b")]
alpha_result2 <- psych::alpha(items2)
alpha_result2 # 0.78, Self-regulation

items3 <- df[, c(
  "ESWAN_10_b", "ESWAN_11_b", "ESWAN_12_b",
  "ESWAN_29_b", "ESWAN_30_b", "ESWAN_31_b", "ESWAN_32_b",
  "ESWAN_25_b", "ESWAN_28_b")]
alpha_result3 <- psych::alpha(items3)
alpha_result3 # 0.81, social engagement



df_retest <- df_retest %>%
  mutate(
    eswanemo    = rowMeans(across(c(ESWAN_01_b, ESWAN_02_b, ESWAN_03_b, ESWAN_04_b, ESWAN_05_b,ESWAN_06_b, ESWAN_17_b, ESWAN_18_b, ESWAN_19_b, ESWAN_20_b,ESWAN_21_b, ESWAN_22_b, ESWAN_23_b)), na.rm = TRUE),
    eswansoceng = rowMeans(across(c(ESWAN_10_b, ESWAN_11_b, ESWAN_12_b,ESWAN_29_b, ESWAN_30_b, ESWAN_31_b, ESWAN_32_b,ESWAN_25_b, ESWAN_28_b)), na.rm = TRUE),
    eswanselfreg      = rowMeans(across(c(ESWAN_07_b, ESWAN_08_b, ESWAN_09_b,ESWAN_13_b, ESWAN_14_b, ESWAN_15_b, ESWAN_16_b)), na.rm = TRUE),
    eswanemo_retest    = rowMeans(across(c(ESWAN_01_retest, ESWAN_02_retest, ESWAN_03_retest, ESWAN_04_retest, ESWAN_05_retest,ESWAN_06_retest, ESWAN_17_retest, ESWAN_18_retest, ESWAN_19_retest, ESWAN_20_retest,ESWAN_21_retest, ESWAN_22_retest, ESWAN_23_retest)), na.rm = TRUE),
    eswansoceng_retest  = rowMeans(across(c(ESWAN_10_retest, ESWAN_11_retest, ESWAN_12_retest,ESWAN_29_retest, ESWAN_30_retest, ESWAN_31_retest, ESWAN_32_retest,ESWAN_25_retest, ESWAN_28_retest)), na.rm = TRUE),
    eswanselfreg_retest      = rowMeans(across(c(ESWAN_07_retest, ESWAN_08_retest, ESWAN_09_retest,ESWAN_13_retest, ESWAN_14_retest, ESWAN_15_retest, ESWAN_16_retest)), na.rm = TRUE))


data_icc <- df_retest[, c("eswanemo", "eswanemo_retest")]
ICC(data_icc) # manage emotions
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.83 11  77  77 2.2e-21        0.74        0.89

data_icc <- df_retest[, c("eswansoceng", "eswansoceng_retest")]
ICC(data_icc) # social drive
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters      ICC3 0.78 8.0  77  77 1.7e-17        0.67        0.85

data_icc <- df_retest[, c("eswanselfreg", "eswanselfreg_retest")]
ICC(data_icc) # self-regulation
#                         type  ICC  F df1 df2       p lower bound upper bound
#Single_fixed_raters       0.76 7.4  77  77 1.5e-16        0.65        0.84




########################################
#####
##### Create final variables
#####
########################################

 ## Check correlations
df <- df %>%
  mutate(
    rcads_tot    = rowMeans(across(c(RCADS_01_b,RCADS_02_b,RCADS_03_b,RCADS_04_b,RCADS_05_b,RCADS_06_b,RCADS_07_b,RCADS_08_b,RCADS_09_b,RCADS_10_b,
                                 RCADS_11_b,RCADS_12_b,RCADS_13_b,RCADS_14_b,RCADS_15_b,RCADS_16_b,RCADS_17_b,RCADS_18_b,RCADS_19_b,RCADS_20_b,
                                 RCADS_21_b,RCADS_22_b,RCADS_23_b,RCADS_24_b,RCADS_25_b)), na.rm = TRUE),
    hipicopen      = rowMeans(across(c(HiPIC_02_b, HiPIC_12_b, HiPIC_20_b, HiPIC_24_b, HiPIC_29_b, HiPIC_30_b)), na.rm = TRUE),
    hipicconsc      = rowMeans(across(c(HiPIC_03_b, HiPIC_15_b, HiPIC_26_b)), na.rm = TRUE),
    hipicextra = rowMeans(across(c(HiPIC_08_b, HiPIC_10_b, HiPIC_18_b, HiPIC_19_b, HiPIC_23_b, HiPIC_25_b)), na.rm = TRUE),
    hipicneuro    = rowMeans(across(c(HiPIC_01_b, HiPIC_06_b, HiPIC_14_b, HiPIC_17_b, HiPIC_22_b)), na.rm = TRUE),
    bessiself    = rowMeans(across(c(bessi_03_b, bessi_30_b, bessi_57_b, bessi_05_b, bessi_32_b, bessi_59_b, bessi_08_b, bessi_35_b, bessi_62_b, bessi_82_b, bessi_11_b, bessi_38_b, bessi_65_b,bessi_14_b, bessi_41_b, bessi_68_b, bessi_16_b, bessi_43_b, bessi_70_b, bessi_24_b, bessi_51_b, bessi_78_b, bessi_21_b, bessi_48_b, bessi_75_b, bessi_19_b, bessi_46_b, bessi_73_b)), na.rm = TRUE),
    bessisoceng = rowMeans(across(c(bessi_01_b, bessi_28_b, bessi_55_b, bessi_83_b, bessi_12_b, bessi_39_b, bessi_66_b, bessi_15_b, bessi_42_b, bessi_69_b, bessi_22_b, bessi_49_b, bessi_76_b)), na.rm = TRUE),
    bessicoop      = rowMeans(across(c(bessi_02_b, bessi_29_b, bessi_56_b, bessi_07_b, bessi_34_b, bessi_61_b, bessi_13_b, bessi_40_b, bessi_67_b, bessi_20_b, bessi_47_b, bessi_74_b)), na.rm = TRUE),
    bessiemo      = rowMeans(across(c(bessi_04_b, bessi_31_b, bessi_58_b, bessi_85_b, bessi_10_b, bessi_37_b, bessi_64_b, bessi_18_b, bessi_45_b, bessi_72_b, bessi_23_b, bessi_50_b, bessi_77_b)), na.rm = TRUE),
    eswanemo    = rowMeans(across(c(ESWAN_01_b, ESWAN_02_b, ESWAN_03_b, ESWAN_04_b, ESWAN_05_b,ESWAN_06_b, ESWAN_17_b, ESWAN_18_b, ESWAN_19_b, ESWAN_20_b,ESWAN_21_b, ESWAN_22_b, ESWAN_23_b)), na.rm = TRUE),
    eswansocdrive = rowMeans(across(c(ESWAN_10_b, ESWAN_11_b, ESWAN_12_b,ESWAN_29_b, ESWAN_30_b, ESWAN_31_b, ESWAN_32_b,ESWAN_25_b, ESWAN_28_b)), na.rm = TRUE),
    eswanselfreg      = rowMeans(across(c(ESWAN_07_b, ESWAN_08_b, ESWAN_09_b,ESWAN_13_b, ESWAN_14_b, ESWAN_15_b, ESWAN_16_b)), na.rm = TRUE))

corrdf <- df[254:264]

cor(corrdf, use = "pairwise.complete.obs")

write_sav(df, "final.sav")

