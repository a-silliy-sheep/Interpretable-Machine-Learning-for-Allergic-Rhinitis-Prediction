
# package loading-------------------------------------------------------------------------
library(caret)  # Models tuning hyperparameter package
library(Boruta)  # Random forest method of selecting features
library(tidyverse)

library(LogicReg)  #logreg
library(data.table)  #data.frame tranform to data.table（the latter is often used by XGBoost model）
library(MASS)  # LDA
library(class)  #k-nearest neighbors
library(kknn)  #weighted k-nearest neighbors

library(rpart)  # classification and regressionn trees
library(randomForest)  #random forests
library(xgboost)  # gradient boosting

library(kernlab)  #assist with SVM feature selection
library(e1071)  #SVM

library(corrplot)  # matrix of correlation
library(ggplot2)
library(InformationValue)  # poltROC drawing
library(partykit)  #treeplots
library(mice)

library(DMwR2)
library(smotefamily)  # Synthetic Minority Oversampling Techniques ---- Data Synthesis
library(Hmisc)  #label

library(openxlsx)
library(showtext)
library(Cairo) 

library(ROCR)  # relate to ROC
source(file = "./funct_.R")  # There are related custom functions within
load(file = "D:/paperStage1/Data/Closed/2018CCHH_Urumqi/Code_/MachineLearning/rhinitis/New/Initial.RData")


#######The original data collation process involves data security issues and is therefore not shown. 
      ##dfCheck.RData is the data processed by the inclusion exclusion criteria################

load(file = "./dfCheck.RData")  # Data collated but not filled with missing values
dfCheck <- obj_

# First、Data preprocessing==================================================================================================
# Data preprocessing before interpolation----------------------------------------------------------------------------
dfCheck2 <- dfCheck
colnames(dfCheck2)
sum(dfCheck2[, '如果是.孩子大约几岁时.被首次诊断'] %in% c(1))  # See the number of children with AR at age 1

dfCheck2 <- dfCheck2[!dfCheck2[, '如果是.孩子大约几岁时.被首次诊断'] %in% c(1), ]  # Exclusion of children with AR at 1 year of age


dfModel <- dfCheck2[, -c(1:41, 45:49, 52:54, 56:58, 63:77, 82:102, 107:108, 112:121, 125:130, 134:139, 150:153,
                        159:182, 194:198, 204:236, 245:255, 257:264, 281:293, 295, 297, 300:301, 303:305)]  # preliminary screening

colnames(dfModel)
get_compositionDrop(dfModel, threshold_ = 0.985)

dfModel <- dfModel[, -c(5, 11:12)]

table(dfModel[, '如果是.孩子大约几岁时.被首次诊断'], exclude = T)  # Find out how old a child was when diagnosed with AR
prop.table(table(dfModel[, '如果是.孩子大约几岁时.被首次诊断'], exclude = T))  # View Composition Ratio 7.8%
colnames(dfModel)


dfModel <- dfModel %>% dplyr::mutate(
  AR_doctor_moreThan1 = ifelse(dfModel[, '如果是.孩子大约几岁时.被首次诊断'] %in% c(2, 3, 4, 5, 6), 1, 0),
  孩子0_1岁曾出现呼吸困难_发出像哮鸣音 = ifelse(dfModel[, '如果是.孩子在那些年龄段出现过.不满1岁时'] == 1, 1, 0),
  孩子0_1岁在没有感冒_流感的情况下打喷嚏 = ifelse(dfModel[, '如是.孩子在哪些年龄段.不满1岁'] == 1, 1, 0),
  孩子0_1岁有过至少连续6个月反复发作的瘙痒疹子 = ifelse(dfModel[, '如果是.孩子在哪些年龄段发生过'] == 1, 1, 0),
  孩子0_1岁被医生确诊为AD = ifelse(dfModel[, '如果是.大约几岁被诊断为花粉症或过敏性鼻炎'] == 1, 1, 0),
  孩子0_1岁患医生诊断的肺炎 = ifelse(dfModel[, '如果是.孩子在那些年龄段被诊断.不满1岁'] == 1, 1, 0),
  孩子0_1岁曾接受过抗生素治疗 = ifelse(dfModel[, '有.不满1岁'] == 1, 1, 0),
  孩子睡觉房间的卫生清洁频率 = ifelse(孩子睡觉房间的卫生清洁频率 %in% c(3, 4, 5, 6), 3, 孩子睡觉房间的卫生清洁频率),  # 3:Once a week or less
  如果孩子不满1岁接受过抗生素治疗.那么有几次 = ifelse(如果孩子不满1岁接受过抗生素治疗.那么有几次 %in% c(1, 2),
                                  2, 如果孩子不满1岁接受过抗生素治疗.那么有几次),  # 2: 1-2次
  是否为独生子女 = ifelse(孩子是否为独生子女_sub == 0, 1, 0),  # 1：is only child.， 0：no
  haveOlderSblings = ifelse(孩子是否为独生子女_sub == 1, 1, 0),   # 1：have a brother and a sister.， 0：no
  preDeliver_delayDeliver = ifelse(preDeliver_delayDeliver == 1, -1,   # -1: premature
                                   ifelse(preDeliver_delayDeliver == 0, 0,  # normal
                                          ifelse(preDeliver_delayDeliver == 2 , 1, NA))),  # 1:
  age = ifelse(realAgeToQuestionnaire_YMD_byM <= 60, 0, 
               ifelse(realAgeToQuestionnaire_YMD_byM > 60 & realAgeToQuestionnaire_YMD_byM < 72 , 1, 2))
) %>% dplyr::select(-c('孩子在没有感冒.流感的情况下是否打喷嚏等', '孩子是否曾被医生诊断患湿疹或过敏性皮炎',
                       '如果是.大约几岁被诊断为花粉症或过敏性鼻炎', '孩子是否患过肺炎', '您的孩子是否患过耳炎',
                       "孩子是否被医生诊断为患有花粉症或过敏性鼻炎", "如果是.孩子大约几岁时.被首次诊断",
                       "孩子是否曾出现过呼吸困难.发出像哮鸣音", '如果是.孩子在那些年龄段出现过.不满1岁时',
                       '如是.孩子在哪些年龄段.不满1岁', 'height_birth', '孩子通常睡在哪个房间', '如果是.地毯地垫的大小',
                       '孩子是否为独生子女_sub', 'realAgeToQuestionnaire_YMD_byM',
                       '孩子通常睡觉房间地板材料是', '孩子通常睡觉的房间.其墙壁的表面材料为',
                       '孩子是否有过至少连续6个月反复发作的瘙痒疹子', '如果是.孩子在哪些年龄段发生过',
                       '如果是.孩子的肺炎是否经过医生诊断', '如果是.孩子在那些年龄段被诊断.不满1岁',
                       '孩子是否接受过抗生素治疗.没有用过', '有.不满1岁')
                    )

colnames(dfModel)
get_compositionDrop(dfModel)


    # Feature binary, & set ‘don't know’ to NA
for (col in 20:30){ 
  dfModel[, col] <- ifelse(dfModel[, col] == 1, 0,  # 0: no
                           ifelse(dfModel[, col] %in% c(2, 3), 1, NA))  # 1：yes
}



library(scales)


dfModel <- dfModel %>% dplyr::mutate(  # Continued adjustment of variables
  parentIncome = ifelse(dfModel[, '孩子父母年均家庭税前总收入约'] %in% c(1, 2), 1,   #----  1: 小于10W
                        ifelse(dfModel[, '孩子父母年均家庭税前总收入约'] %in% c(3, 4), 2,  #----  2: 10~40W
                               ifelse(dfModel[, '孩子父母年均家庭税前总收入约'] == 5, 3, NA))),  #----  3: 大于40W
  maternalFeeding= ifelse(dfModel[, '孩子进行纯母乳喂养的持续时间'] %in% c(1, 2), 1,   #---- 1:'不到1个月甚至无'
                       ifelse(dfModel[, '孩子进行纯母乳喂养的持续时间']  == 3, 2,  #---- 2: '≥1个月且小于4个月'
                              ifelse(dfModel[, '孩子进行纯母乳喂养的持续时间'] == 4, 3,  #--- 3: '≥4个月且少于6个月'
                                     ifelse(dfModel[, '孩子进行纯母乳喂养的持续时间'] %in% c(5, 6), 4, NA)))),  # -- 4： '6个月及以上'
  momEducation= ifelse(dfModel[, '母亲的最高学历是'] == 1, 1,   #----  1：'低'---- . <= 12 年
                         ifelse(dfModel[, '母亲的最高学历是'] %in% c(2), 2,  #----:2：'中'-----12 < . <= 15
                                ifelse(dfModel[, '母亲的最高学历是'] %in% c(3, 4), 3, NA))),   #---3：'高'---- . >15 硕士及以上
  dadEducation= ifelse(dfModel[, '父亲的最高学历是'] == 1, 1,   #----1：低' ----中专或以下
                       ifelse(dfModel[, '父亲的最高学历是'] %in% c(2), 2,  #----  2：'中'------  大专本科
                              ifelse(dfModel[, '父亲的最高学历是'] %in% c(3, 4), 3, NA))),  #---- 3： '高'----- 硕士及以上
) %>% dplyr::select(-c('孩子父母年均家庭税前总收入约', '孩子进行纯母乳喂养的持续时间',
                       '母亲的最高学历是', '父亲的最高学历是'))

colnames(dfModel)




dfModelOK <- dfModel


get_compositionDrop(dfModelOK)

      # Data verification：
View(cbind(dfModel[rownames(dfModelOK), c("weight_birth", "bmi_birth")],
           dfModelOK[, c("孩子的性别是", "weight_birth", "bmi_birth")]))

colnames(dfModelOK)

dfSave(obj_ = dfModelOK, file_name = 'dfModelOK')  #save


get_compositionDrop(dfModelOK)

dfModelOK <- dfModelOK %>% dplyr::mutate(
  event = AR_doctor_moreThan1,
  nothing = 'None'
) %>% dplyr::select('event', 'nothing', everything())

#     dfSave(dfModelOK, file = 'dfModelOK_仅筛选_未进行填充和特征选择')
load('./dfModelOK_仅筛选_未进行填充和特征选择.RData') # -------------------------------------------Intermediate data loading
dfModelOK <- obj_

# Data Filling & Feature Selection---------------(around 10 hours)---------------------------

str(dfModelOK)
colnames(dfModelOK)

for (col in colnames(dfModelOK)){
  dfModelOK[, col] <- factor(dfModelOK[, col] )
}

get_compositionDrop(dfModelOK)


imp_tog <- mice(dfModelOK, method = 'rf', m=1, maxit = 10, seed=2024)
imp_togDf <- complete(imp_tog, fill = 1)
imp_togDf <-  imp_togDf %>% dplyr::select(AR_doctor_moreThan1, everything()) %>% 
  dplyr::select('event', 'nothing', everything()) %>%
  dplyr::select(!one_of('AR_doctor_moreThan1'))

imp_togDf <- na.omit(imp_togDf)
colnames(imp_togDf)


#     dfSave(imp_togDf, file = 'imp_togDf_已RF填充_且剔除填充后仍缺失样本')
load('./imp_togDf_已RF填充_且剔除填充后仍缺失样本.RData') # ----------------------------------------Intermediate data loading
imp_togDf <- obj_

get_compositionDrop(imp_togDf)

get_CrossTable(df_ = imp_togDf, outcome_ind_ = 1, factor_ind_ = 3:ncol(imp_togDf), output_ = T, file_ = 'featureSelet_卡方.xlsx')

feature1 <- read.xlsx('./featureSelet_卡方.xlsx') %>% 
  dplyr::filter(p.value <0.05) %>% dplyr::select('var')

#     write.table(feature1, file = 'feature1.txt')

feature1 <- read.table('./feature1.txt')   # -----------------------------------------------------Intermediate data loading


# Stratified data
dfStratify <- imp_togDf[, c('孩子的性别是', '孩子的出生方式', 'age')]
       
identical(rownames(imp_togDf), rownames(dfStratify))  # True

dfStratify <- dfStratify %>% 
  dplyr::mutate(
    sex = 孩子的性别是,
    birthWay = 孩子的出生方式
  ) %>% dplyr::select(-c('孩子的性别是', '孩子的出生方式'))

attr(dfStratify$sex, "label") <- "1=男；2=女"
attr(dfStratify$birthWay, "label") <- "1=顺产；2=剖宫产"
attr(dfStratify$age, "label") <- "0 = 2-4岁；1 = 5岁；2 = 6-8岁"


#       dfSave(obj_ = dfStratify, file_name = 'dfStratify')
load('./dfStratify.RData')    # -----------------------------------------------------Intermediate data loading
dfStratify <- obj_

feature1 <- feature1[['var']][-di_which(feature1[['var']],
                                        c('孩子的性别是', '孩子的出生方式', 'age', '鱼类或爬行类.', '其他植物或动物.'))]
get_compositionDrop(imp_togDf[, c('event', feature1)])



feature.dmm <- get_feature(df_ =  imp_togDf[, c('event', feature1)], targ_var = 'event',
                           rand_n_ = 2,  # Boruta() internally randomises 1:rand_n_ random number training (i.e. rand_n_ repetitions per group)
                           rfe_n_ = 20,  #rfe() The number of features of the sub-loop
                           repeat_ = list(all_ = 5, rf_ = 1, lda_ = 5),  # Data repeated ‘repeat_’ times
                           number_ = list(all_ = 5 , rf_ = 1, lda_ = 5),  #Data split into ‘number_’ groups
                           cl_ = 3,   # Number of threads
                           cut_min_ = 30, cut_n_ = 1)  # If the result of each return is still greater than 40/40*0.66 after 
                                                        #de-weighting, then screening is initiated (number of repetitions >= cut_n_)
# Feature selection for smote.df would be highly biased due to the ‘source data based’ philosophy and is not discussed further!  ---once /20min

Features <- list(
  data = imp_togDf,
  formula = "get_feature(df_ =  imp_togDf[, c('event', feature1)], targ_var = 'event',
                           rand_n_ = 2,  # Boruta() internally randomises 1:rand_n_ random number training (i.e. rand_n_ repetitions per group)
                           rfe_n_ = 20,  #rfe() The number of features of the sub-loop
                           repeat_ = list(all_ = 5, rf_ = 1, lda_ = 5),  # Data repeated ‘repeat_’ times
                           number_ = list(all_ = 5 , rf_ = 1, lda_ = 5),  #Data split into ‘number_’ groups
                           cl_ = 3,   # Number of threads
                           cut_min_ = 30, cut_n_ = 1)",
  return = feature.dmm
)

#   save(Features, file = 'Features.RData')
load('./Features.RData')    # -----------------------------------------------------Intermediate data loading


table(Features[["return"]][["orig"]][["Boruta_confi"]])[
  table(Features[["return"]][["orig"]][["Boruta_confi"]])>3]  # >15 per cent of repetitions in non-excluded features

featureIn1 <- names(table(Features[["return"]][["orig"]][["Boruta_confi"]])[  # 'Accepted' of feature extraction
  table(Features[["return"]][["orig"]][["Boruta_confi"]])>3])

featureIn2 <- names(table(Features[["return"]][["orig"]][["Boruta_togth"]])[  # Feature extraction not 'rejected'
  table(Features[["return"]][["orig"]][["Boruta_togth"]])>5])
featureIn2 <- featureIn2[-which(featureIn2 %in% c('如果孩子不满1岁接受过抗生素治疗.那么有几次', '是否为独生子女'))]  #  repeat --240410


# Data segmentation----------------(around 12 hours)-------------------------------------
str(dfModelOK)
dfModelOK2 <- dfModelOK %>% dplyr::select(-c("孩子的性别是", "孩子的出生方式", "age"))  # Exclusion of stratification variables
for (col in colnames(dfModelOK2)){
  dfModelOK2[, col] <- as.factor(dfModelOK2[, col])
}

str(dfModelOK2)
colnames(dfModelOK2)
get_compositionDrop(dfModelOK2)

data.mergeADA1 <- get_mlDataDivide(ml_df_ = dfModelOK2,  # First line ending variable & all factorisation
                               feature_in_ = featureIn1, seed_ = 1,
                               partition_ = list(times_ = 100, p1 = .55, p2 = .15, p3 = .3), 
                               imp_ = list(meth = 'rf', m = 1, maxit = 5),  # Data filling options - separate interpolation of training and test sets (no interpolation)
                               targ_var_ = 'event', 
                               stra_df_ = dfStratify, stra_var_ = colnames(dfStratify),  # Stratified data populated
                               doResample = T, # Class Imbalance Handling
                               SMOTE_ = list(do = F, size = 13),  # Priority is given to ---- after 13-fold amplification 1:1
                               ADASYN_ = list(do = T, beta_ = 0.7, dist = "Overlap"),  # Perform adaptive sampling - HEOM (both numeric/factor-based variables)
                               threhold_ = 0.995)  
#  save(data.mergeADA1, file = 'data.mergeADA1.RData')   ----240412
load('./data.mergeADA1.RData')    # ------------------------------------------------------------------Intermediate data loading


data.mergeADA2 <- get_mlDataDivide(ml_df_ = dfModelOK2,
                                   feature_in_ = featureIn2, seed_ = 1,
                                   partition_ = list(times_ = 100, p1 = .55, p2 = .15, p3 = .3), 
                                   imp_ = list(meth = 'rf', m = 1, maxit = 5), 
                                   targ_var_ = 'event', 
                                   stra_df_ = dfStratify[2], stra_var_ = colnames(dfStratify[2]), 
                                   doResample = T, 
                                   SMOTE_ = list(do = F, size = 13),
                                   ADASYN_ = list(do = T, beta_ = 0.7, dist = "Overlap"), 
                                   threhold_ = 0.995)  # 名存实亡
#  save(data.mergeADA2, file = 'data.mergeADA2.RData')   ----240412
load('.//data.mergeADA2.RData')    # ---------------------------------------------------------------Intermediate data loading


featureIn3 <- c('孩子母亲是否患有.过敏性.鼻炎', '孩子父亲是否患有.过敏性.鼻炎', 'haveOlderSblings',
                '孩子是否有因食物引起的湿疹.荨麻疹.腹泄等', 'dadEducation')
      # Select the 5 best performing features in the ‘coreFeature’.：---------lessFeature
data.mergeADA3 <- get_mlDataDivide(ml_df_ = dfModelOK2,
                                   feature_in_ = featureIn3, seed_ = 1,
                                   partition_ = list(times_ = 100, p1 = .55, p2 = .15, p3 = .3),
                                   imp_ = list(meth = 'rf', m = 1, maxit = 5), 
                                   targ_var_ = 'event', 
                                   stra_df_ = dfStratify[2], stra_var_ = colnames(dfStratify[2]), 
                                   doResample = T, 
                                   SMOTE_ = list(do = F, size = 13),
                                   ADASYN_ = list(do = T, beta_ = 0.7, dist = "Overlap"), 
                                   threhold_ = 0.995) 

#  save(data.mergeADA3, file = 'data.mergeADA3.RData')   ----240819
load('./data.mergeADA3.RData')    # ---------------------------------------------------------------Intermediate data loading


# Data quality control ---- checked the correlation of each variable with the outcome (
      # it is logical that the correlation should not change much):
tmp <- rbind(data.mergeADA1[["nominal_"]][["sample_1"]][["all"]][["imp_1"]][["train0"]],  # pre-oversample
             data.mergeADA1[["nominal_"]][["sample_1"]][["all"]][["imp_1"]][["validation"]],
             data.mergeADA1[["nominal_"]][["sample_1"]][["all"]][["imp_1"]][["test"]])

str(tmp)
colnames(tmp)

tmp <- tmp %>% dplyr::mutate(
  event = event,
  nothing = 'None'
) %>% dplyr::select('event', 'nothing', everything())

get_CrossTable(df_ = tmp, outcome_ind_ = 1, 3:ncol(tmp), output_ = T, file_ = 'tmp.xlsx')  # This function is used for single factor analysis
        #--------not so bad






## Second、model training===================================================================================================


#logistic regression-----------------------------------------(around 15 mins)----------------------------

#     df_test <- data.mergeADA1[["nominal_"]][["sample_1"]][["all"]][["imp_1"]]
#     
#     library(LogicReg)  #logreg
#     
#     lr.fit <- glm(event ~ ., family = binomial,
#                   data = df_test[["train"]])
#     lr.pred.train <- predict(object = lr.fit, newdata = df_test[["train"]],
#                              type = 'response')
#     lr.roc.train <- pROC::roc(ifelse(df_test[["trainY"]] == '1', 1, 0),
#                               as.numeric(lr.pred.train),
#                               direction = c('<'),  # control < case
#     )
#     get_est(pred_ = as.numeric(lr.pred.train),
#             true_ = ifelse(df_test[["trainY"]] == '1', 1, 0),
#             confMt_ = F, plot_ = F)
#     
#     
#     lr.pred.test <- predict(object = lr.fit, newdata = df_test[["test"]],
#                              type = 'response')
#     
#     get_est(pred_ = as.numeric(lr.pred.test),
#             true_ = ifelse(df_test[["testY"]] == '1', 1, 0),
#             confMt_ = F, plot_ = F)


getLR_model <- function(model_dat_, formula_, optParam = NA){
  fit_ <- glm(formula_, family = binomial, data = model_dat_)
  return(fit_)
}

getLR_Pred <- function(model_, pred_X){
  # document: Get the predicted value of this model on new data, return vector (probability)
  # model_: incoming model
  # pred_X: predictor variable
  pred_ <- predict(model_, newdata = pred_X, type='response')
  return(pred_)
}



#   Calculation.
Logreg_ret1 <-  get_AggregateModel(data_iter_ = data.mergeADA1[[1]], stratify_df_ = dfStratify, targ_var_ = 'event',
                                cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                fitFuncs = getLR_model, predFuncs = getLR_Pred,
                                time_sleep_ = 0)

Logreg_ret2 <-  get_AggregateModel(data_iter_ = data.mergeADA2[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                   cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                   fitFuncs = getLR_model, predFuncs = getLR_Pred,
                                   time_sleep_ = 0)

Logreg_ret3 <-  get_AggregateModel(data_iter_ = data.mergeADA3[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                   cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #精度2%
                                   fitFuncs = getLR_model, predFuncs = getLR_Pred,
                                   time_sleep_ = 0)

Sys.sleep(120)


Logreg <- list(featureIn1 = Logreg_ret1, featureIn2 = Logreg_ret2, featureIn3 = Logreg_ret3)


getwd()
#     !!! save(Logreg, file = 'Logreg.RData')   -------240819
load('.//Logreg.RData')      # -------------------------------------------------------------Intermediate data loading



# Support Vector Machine (SVM)---------------------------------------------------(around 5 hours)---------------
library(kernlab)  #assist with SVM feature selection
library(e1071)  
getModelInfo()
modelLookup('svmLinear')

set.seed(4)

getSVM_optParam <- function(Tune.dat_, formula_, fitFuncs, predFuncs){
  
  grid_ <- expand.grid(
    cost= round(seq(.4, 3, length.out = 5), 1),
    gamma = 1# round(seq(.1, 4, length.out = 4), 1)
    )
  
  Ret_ <- list()  
  for (para.dat_p in names(Tune.dat_)){
    para.dat_ <- Tune.dat_[[para.dat_p]]
    X_ <- para.dat_[['X_']]
    Y_ <- para.dat_[['Y_']]
    
    ret_ <- c()
    for (i in 1:nrow(grid_)){
      
      optParam <- list(cost = grid_[i, 'cost'], gamma = grid_[i, 'gamma'])
      
      samp_i <- c(1:nrow(X_))[1:round(nrow(X_)*.7)]
      samp_i2 <- c(1:nrow(X_))[-samp_i]
      
      fit2_ <- fitFuncs(model_dat_ = data.frame(event = Y_[samp_i], X_[samp_i, ]),
                        formula_ = formula_, optParam = optParam) #-------------------model 2
      pred2_.test <- predFuncs(model_ = fit2_, pred_X = X_[samp_i2, ])
      auroc.test2_ <- pROC::roc(ifelse(Y_[samp_i2] == '1', 1, 0),
                                as.numeric(pred2_.test),
                                direction = c('<'),  # control < case
      )
      cat('Looking for optimal Auroc parameters：', i, '/', nrow(grid_), '----', auroc.test2_[["auc"]][1], '\n')
      ret_ <- c(ret_, auroc.test2_[["auc"]][1])
    }
    Ret_[[para.dat_p]] <- ret_
  }
  
  tmp_ <- do.call('cbind', Ret_) %>% as.data.frame()
  write.csv(tmp_, '_tmp_plot.csv')
  tmp_ <- read.csv('_tmp_plot.csv', row.names = 1) 
  tmp_2 <- apply(tmp_[,c('one', 'two', 'thr')], 1, function(x){mean(x)})
  
  opt_i <- which.max(tmp_2) %>% print()
  tmp_2[which.max(tmp_2)] %>% print()
  grid_[which.max(tmp_2), ] %>% print()
  Sys.sleep(20)
  
  return(list(cost = grid_[opt_i, 'cost'], gamma = grid_[opt_i, 'gamma']))
}



getSVM_model <- function(model_dat_, formula_, optParam = NA){
  
  fit_ <- svm(formula_, data = df_strToNum(model_dat_), kernel='linear', probability = T,  # kernel function：linear
                   cost= optParam[['cost']], gamma = optParam[['gamma']])  
  return(fit_)
}

getSVM_Pred <- function(model_, pred_X){
  # document: Get the predicted value of this model on new data, return vector (probability)
  # model_: incoming model
  # pred_X: predictor variable
  pred_ <- predict(model_, newdata= df_strToNum(pred_X))  
  return(pred_)
}



#   calculation:
SVM_ret1 <-  get_AggregateModel(data_iter_ = data.mergeADA1[[1]], stratify_df_ = dfStratify, targ_var_ = 'event',
                                   cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                   fitFuncs = getSVM_model, predFuncs = getSVM_Pred, paramFuncs = getSVM_optParam,
                                   time_sleep_ = 2)

SVM_ret2 <-  get_AggregateModel(data_iter_ = data.mergeADA2[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                   cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                   fitFuncs = getSVM_model, predFuncs = getSVM_Pred, paramFuncs = getSVM_optParam,
                                   time_sleep_ = 2)

SVM_ret3 <-  get_AggregateModel(data_iter_ = data.mergeADA3[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                fitFuncs = getSVM_model, predFuncs = getSVM_Pred, paramFuncs = getSVM_optParam,
                                time_sleep_ = 2)


Sys.sleep(120)


SVM <- list(featureIn1 = SVM_ret1, featureIn2 = SVM_ret2, featureIn3 = SVM_ret3)


getwd()
#     !!! save(SVM, file = 'SVM.RData')   -----------------------240412
load('.//SVM.RData')      # -------------------------------------------------------------Intermediate data loading



# Integration model of bagging ------bootstrap aggregating bootstrap aggregating method (similar|with put back) ---- random forests-----(around 6 hours)--------------------------------------------
library(randomForest)  #random forests
modelLookup('rf')

#     fit_ <- randomForest(event ~ ., data = df_test[["train"]])
#     fit_tune_ <- randomForest(event ~ ., data = df_test[['train']], 
#                               ntree=which.min(fit_$err.rate[,1]))
#     
#     rf.pred.train <- stats::predict(object = fit_tune_,
#                                     newdata = df_test[["train"]], type = 'prob')
#     get_est(pred_ = rf.pred.train[, 2],
#             true_ = ifelse(df_test[["trainY"]] == '1', 1, 0),
#             confMt_ = F, plot_ = F)
#     
#     rf.pred.test <- stats::predict(object = fit_tune_,
#                                     newdata = df_test[["test"]], type = 'prob')
#     get_est(pred_ = rf.pred.test[, 2],
#             true_ = ifelse(df_test[["testY"]] == '1', 1, 0),
#             confMt_ = F, plot_ = F)


getRF_optParam <- function(Tune.dat_, formula_, fitFuncs, predFuncs){
  grid_ <- expand.grid(
    ntree = round(seq(100, 800, length.out = 5), 0),
    mtry = round(seq(1, 7, length.out = 6), 0),
    nodesize = round(seq(4, 15, length.out = 3), 0)
  )
  
  Ret_ <- list() 
  for (para.dat_p in names(Tune.dat_)){
    para.dat_ <- Tune.dat_[[para.dat_p]]
    X_ <- para.dat_[['X_']]
    Y_ <- para.dat_[['Y_']]
      
    ret_ <- c()
    for (i in 1:nrow(grid_)){
      
      
      optParam <- list(ntree = grid_[i, 'ntree'], mtry = grid_[i, 'mtry'], nodesize = grid_[i, 'nodesize'])
      
      samp_i <- c(1:nrow(X_))[1:round(nrow(X_)*.7)]
      samp_i2 <- c(1:nrow(X_))[-samp_i]
      
      fit2_ <- fitFuncs(model_dat_ = data.frame(event = Y_[samp_i], X_[samp_i, ]),
                        formula_ = formula_, optParam = optParam) #-------------------模型2
      pred2_.test <- predFuncs(model_ = fit2_, pred_X = X_[samp_i2, ])
      auroc.test2_ <- pROC::roc(ifelse(Y_[samp_i2] == '1', 1, 0),
                                as.numeric(pred2_.test),
                                direction = c('<'),  # control < case
      )
      cat('Looking for optimal Auroc parameters：', i, '/', nrow(grid_), '----', auroc.test2_[["auc"]][1], '\n')
      ret_ <- c(ret_, auroc.test2_[["auc"]][1])
    }
    Ret_[[para.dat_p]] <- ret_
  }
  
  tmp_ <- do.call('cbind', Ret_) %>% as.data.frame()
  write.csv(tmp_, '_tmp_plot.csv')
  tmp_ <- read.csv('_tmp_plot.csv', row.names = 1) 
  tmp_2 <- apply(tmp_[,c('one', 'two', 'thr')], 1, function(x){mean(x)})
  
  opt_i <- which.max(tmp_2) %>% print()
  tmp_2[which.max(tmp_2)] %>% print()
  grid_[which.max(tmp_2), ] %>% print()
  
  return(list(ntree = grid_[opt_i, 'ntree'], mtry = grid_[opt_i, 'mtry'], nodesize = grid_[opt_i, 'nodesize']))
}





getRF_model <- function(model_dat_, formula_, optParam = NA){
  randomForest(formula_, data = model_dat_)
  fit_tune_ <- randomForest(formula_, data = model_dat_, 
                            ntree = optParam[['ntree']], mtry = optParam[['mtry']], 
                            nodesize = optParam[['nodesize']])
  
  return(fit_tune_)
}

getRF_Pred <- function(model_, pred_X){
  # document: Get the predicted value of this model on new data, return vector (probability)
  # model_: incoming model
  # pred_X: predictor variable
  pred_ <- predict(model_, newdata = pred_X, type = 'prob')
  pred_ <- pred_[, 2]
  return(pred_)
}





#   calculation:
RF_ret1 <-  get_AggregateModel(data_iter_ = data.mergeADA1[[1]], stratify_df_ = dfStratify, targ_var_ = 'event',
                                   cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                   fitFuncs = getRF_model, predFuncs = getRF_Pred, paramFuncs = getRF_optParam,
                                   time_sleep_ = 0)

RF_ret2 <-  get_AggregateModel(data_iter_ = data.mergeADA2[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                   cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                   fitFuncs = getRF_model, predFuncs = getRF_Pred, paramFuncs = getRF_optParam,
                                   time_sleep_ = 0)

RF_ret3 <-  get_AggregateModel(data_iter_ = data.mergeADA3[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                               cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                               fitFuncs = getRF_model, predFuncs = getRF_Pred, paramFuncs = getRF_optParam,
                               time_sleep_ = 0)



Sys.sleep(120)


RF <- list(featureIn1 = RF_ret1, featureIn2 = RF_ret2, featureIn3 = RF_ret3)


getwd()
#     !!! save(RF, file = 'RF.RData')     --------------240413
load('.//RF.RData')      # -------------------------------------------------------------Intermediate data loading





#Boosting Evolution of Integrated Models (like for like | no playback)----XGBoost----(around 18 hours)------------------------------------------
library(xgboost)  # gradient boosting
xgb.info <- getModelInfo('xgbTree')$xgbTree
modelLookup('xgbTree')


getXGB_optParam <- function(Tune.dat_, formula_, fitFuncs, predFuncs){
  grid_ <- expand.grid(
    nrounds = round(seq(150, 1000, length.out = 4), 0),  # Maximum number of iterations (number of trees in the final model)
    max_depth = round(seq(2, 8, length.out = 3), 0),  #Maximum depth of a single tree
    eta = round(seq(.05, .75, length.out = 3), 3),  # Learning rate - contribution of each tree in the final solution，default=0.3
    gamma = round(seq(1, 10, length.out = 3), 2),  #Minimum reduction required when adding a new leaf partition to the tree
    colsample_bytree = round(seq(.3, 1, length.out = 3), 2),  
    min_child_weight = 2,  # The larger the value, the more conservative
    subsample = 0.5#round(seq(.3, .75, length.out = 3), 2)  # Sub-sample data as a proportion of the whole observation，default=1
  )
  
  
  Ret_ <- list() 
  for (para.dat_p in names(Tune.dat_)){
    para.dat_ <- Tune.dat_[[para.dat_p]]
    X_ <- para.dat_[['X_']]
    Y_ <- para.dat_[['Y_']]
    
    ret_ <- c()
    for (i in 1:nrow(grid_)){
      
      optParam <- list(param = list(objective = 'binary:logistic', booster = 'gbtree', eval_metric = 'error',
                                    eta = grid_[i, 'eta'],
                                    max_depth = grid_[i, 'max_depth'],
                                    subsample = grid_[i, 'subsample'],
                                    colsample_bytree = grid_[i, 'colsample_bytree'],
                                    gamma = grid_[i, 'gamma']
      ),
      nrounds = grid_[i, 'nrounds'])
      
      samp_i <- c(1:nrow(X_))[1:round(nrow(X_)*.7)]
      samp_i2 <- c(1:nrow(X_))[-samp_i]
      
      fit2_ <- fitFuncs(model_dat_ = data.frame(event = Y_[samp_i], X_[samp_i, ]),
                        formula_ = formula_, optParam = optParam) #-------------------model2
      pred2_.test <- predFuncs(model_ = fit2_, pred_X = X_[samp_i2, ])
      auroc.test2_ <- pROC::roc(ifelse(Y_[samp_i2] == '1', 1, 0),
                                as.numeric(pred2_.test),
                                direction = c('<'),  # control < case
      )
      cat('Looking for optimal Auroc parameters：', i, '/', nrow(grid_), '----', auroc.test2_[["auc"]][1], '\n')
      ret_ <- c(ret_, auroc.test2_[["auc"]][1])
      
      if (i%%50 == 49){Sys.sleep(5)}
    }
    Ret_[[para.dat_p]] <- ret_
  }
  
  tmp_ <- do.call('cbind', Ret_) %>% as.data.frame()
  write.csv(tmp_, '_tmp_plot.csv')
  tmp_ <- read.csv('_tmp_plot.csv', row.names = 1) 
  tmp_2 <- apply(tmp_[,c('one', 'two', 'thr')], 1, function(x){mean(x)})
  
  opt_i <- which.max(tmp_2) %>% print()
  tmp_2[which.max(tmp_2)] %>% print()
  grid_[which.max(tmp_2), ] %>% print()

  return(list(param = list(objective = 'binary:logistic', booster = 'gbtree', eval_metric = 'error',
                           eta = grid_[opt_i, 'eta'],
                           max_depth = grid_[opt_i, 'max_depth'],
                           subsample = grid_[opt_i, 'subsample'],
                           colsample_bytree = grid_[opt_i, 'colsample_bytree'],
                           gamma = grid_[opt_i, 'gamma']
  ),
  nrounds = grid_[opt_i, 'nrounds']))
}



getXGB_model <- function(model_dat_, formula_, optParam = NA){
  outcome <- as.character(formula_)[2]
  
  train.x_ <- as.matrix(df_strToNum(model_dat_[, -which(colnames(model_dat_) == outcome)]))
  train.y_ <- ifelse(model_dat_[[outcome]] == '0', 0, 1 )
  trainMat_ <- xgb.DMatrix(data = train.x_, 
                           label = train.y_)
  
  fit_ <- xgb.train(params = optParam[['param']], data = trainMat_, nrounds = optParam[['nrounds']])
  return(fit_)
}

getXGB_Pred <- function(model_, pred_X){
  # document: Get the predicted value of the model on the new data, return vector (probability)
  # model_: incoming model
  # pred_X: Predictor variable
  train.x_ <- as.matrix(df_strToNum(pred_X))
  pred_ <- predict(model_, train.x_, type = 'prob')
  return(pred_)
}



#   calculation:
XGB_ret1 <-  get_AggregateModel(data_iter_ = data.mergeADA1[[1]], stratify_df_ = dfStratify, targ_var_ = 'event',
                                cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                fitFuncs = getXGB_model, predFuncs = getXGB_Pred, paramFuncs = getXGB_optParam,
                                time_sleep_ = 2)

XGB_ret2 <-  get_AggregateModel(data_iter_ = data.mergeADA2[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                fitFuncs = getXGB_model, predFuncs = getXGB_Pred, paramFuncs = getXGB_optParam,
                                time_sleep_ = 2)

XGB_ret3 <-  get_AggregateModel(data_iter_ = data.mergeADA3[[1]], stratify_df_ = dfStratify[2], targ_var_ = 'event',
                                cut_ = round(seq(.01, .99, length.out = 49), digits = 2),  #2%
                                fitFuncs = getXGB_model, predFuncs = getXGB_Pred, paramFuncs = getXGB_optParam,
                                time_sleep_ = 2)

Sys.sleep(120)


XGB <- list(featureIn1 = XGB_ret1, featureIn2 = XGB_ret2, featureIn3 = XGB_ret3)


getwd()
#     !!! save(XGB, file = 'XGB.RData') --------------------240412
load('./XGB.RData')      # -------------------------------------------------------------Intermediate data loading


# Model training completed--------------------------------------------------------------------------------------------------------

Final_Ret <- list(
  coreFeatures = list(data = data.mergeADA1, LR = Logreg[["featureIn1"]][[2]],
                      SVM = SVM[["featureIn1"]][[2]], RF = RF[["featureIn1"]][[2]], XGBoost = XGB[["featureIn1"]][[2]]),
  moreFeatures = list(data = data.mergeADA2, LR = Logreg[["featureIn2"]][[2]],
                      SVM = SVM[["featureIn2"]][[2]], RF = RF[["featureIn2"]][[2]], XGBoost = XGB[["featureIn2"]][[2]]),
  lessFeatures = list(data = data.mergeADA3, LR = Logreg[["featureIn3"]][[2]],
                      SVM = SVM[["featureIn3"]][[2]], RF = RF[["featureIn3"]][[2]], XGBoost = XGB[["featureIn3"]][[2]])
)

getwd()
#     !!! save(Final_Ret, file = 'Final_Ret.RData')    ------240820
load('./Final_Ret.RData')      # -------------------------------------------------------------Intermediate data loading


# Third、Summary analysis of model results==============================================================================================


# Aggregate AUROC values：(around 30 mins)

get_nlrPred <- function(dat, pred_X){
  # dat: used to train the model; contains spe and sen columns
  # pred_X: predicted independent variable
  
  library(mgcv)
  nlr <- gam(sen ~ s(spe), data =  dat, family = 'gaussian')
  nlr_sum <- summary(nlr)

  pred <- predict(nlr, newdata = data.frame(spe = pred_X))
  

  pred[pred > 1] <- 1
  pred[1] <- 1
  pred[pred < 0] <- 0
  pred[length(pred)] <- 0
  return(list(pred = pred, R2 = nlr_sum[["r.sq"]]))
}


df_sen_spe <- NULL
df_cutOff <- NULL  # Optimal Decision Threshold for Predictive Models on the Test Set

auroc_v <- c()
auprc_v <- c()
cutOff_validation <- c()  # weightScore when optimal cut-off point on validation set
cutOff_test <- c()
optPoint <- c()  # the optimal cut-off point
sets <- c()
stra <- c()
elem <- c()
model <- c()
featu <- c()
location <- c()

monitor <- NULL
count_n <- c()

for (featu_ in names(Final_Ret)){
  for (model_p in names(Final_Ret[[featu_]])[2:5]){
    for (samp_p in names(Final_Ret[[featu_]][[model_p]])){
      for (stra_p in names(Final_Ret[[featu_]][[model_p]][[samp_p]])){  #分层变量
        if (stra_p != 'all'){
          for (elem_p in names(Final_Ret[[featu_]][[model_p]][[samp_p]][[stra_p]])){
            for (imp_p in names(Final_Ret[[featu_]][[model_p]][[samp_p]][[stra_p]][[elem_p]])){
              
              nam <- paste(featu_, model_p, samp_p, stra_p, elem_p, imp_p, sep = ' * ') %>% print()
              dat_ <- Final_Ret[[featu_]][[model_p]][[samp_p]][[stra_p]][[elem_p]][[imp_p]]
              
              auroc_v <- c(auroc_v, dat_[["auroc"]][["value"]][["train"]], dat_[["auroc"]][["value"]][["test"]])
              auprc_v <- c(auprc_v, dat_[["auprc"]][["value"]][["train"]], dat_[["auprc"]][["value"]][["test"]])
              cutOff_validation <- c(cutOff_validation, rep(dat_[["Cutoff"]][["validation"]][['weightScore']][
                dat_[["Cutoff"]][["validation"]][['optCut']] == dat_[["Cutoff"]][["optPoint"]]], 2))
              
              count_n <- c(count_n, 1)
              
              cutOff_test <- c(cutOff_test, rep( dat_[["Cutoff"]][["testPerf"]][['weightScore']], 2))
              optPoint <- c(optPoint, rep(dat_[["Cutoff"]][["optPoint"]], 2))
              
              df_cutOff_ <- dat_[["Cutoff"]][["testPerf"]][, c('Pos Pred Value', 'Neg Pred Value',
                                                               'Accuracy', 'F1', 'Sensitivity', 'Specificity',
                                                               'weightScore')]
              df_cutOff <- rbind(df_cutOff, df_cutOff_, df_cutOff_)
              
              sets <- c(sets, 'train', 'test')
              stra <- c(stra, stra_p, stra_p)
              elem <- c(elem, elem_p, elem_p)
              model <- c(model, model_p, model_p)
              featu <- c(featu, featu_, featu_)
              location <- c(location, paste(samp_p, imp_p, sep = '__'), paste(samp_p, imp_p, sep = '__'))
              
              pred_X <- c(seq(0, .01, length.out = 200), seq(.01, .99, length.out = 98), seq(.99, 1, length.out = 200))
              
              train.pred <- get_nlrPred(dat = dat_[["auroc"]][["plotData.train"]], pred_X = pred_X)
              test.pred <- get_nlrPred(dat = dat_[["auroc"]][["plotData.test"]], pred_X = pred_X)
              
              df_sen_spe <- rbind(df_sen_spe, data.frame(spe = c(pred_X, pred_X),
                                                         sen = c(as.numeric(train.pred[[1]]), as.numeric(test.pred[[1]])), 
                                                         R2 = c(rep(train.pred[[2]], 498), rep(test.pred[[2]], 498)),
                                                         type = c(rep('train', 498), rep('test', 498)), 
                                                         location = paste(samp_p, imp_p, sep = '__'),
                                                         stra = stra_p, elem = elem_p,
                                                         model = model_p, featu = featu_
              ))
            }
          }
        }else{
          for (imp_p in names(Final_Ret[[featu_]][[model_p]][[samp_p]][[stra_p]])){
            nam <- paste(featu_, model_p, samp_p, stra_p, imp_p, sep = ' * ') %>% print()
            dat_ <- Final_Ret[[featu_]][[model_p]][[samp_p]][[stra_p]][[imp_p]]
            
            auroc_v <- c(auroc_v, dat_[["auroc"]][["value"]][["train"]], dat_[["auroc"]][["value"]][["test"]])
            auprc_v <- c(auprc_v, dat_[["auprc"]][["value"]][["train"]], dat_[["auprc"]][["value"]][["test"]])
            cutOff_validation <- c(cutOff_validation, rep(dat_[["Cutoff"]][["validation"]][['weightScore']][
              dat_[["Cutoff"]][["validation"]][['optCut']] == dat_[["Cutoff"]][["optPoint"]]], 2))
            count_n <- c(count_n, 1)
            
            cutOff_test <- c(cutOff_test, rep(dat_[["Cutoff"]][["testPerf"]][['weightScore']], 2))
            optPoint <- c(optPoint, rep(dat_[["Cutoff"]][["optPoint"]], 2))
            
            df_cutOff_ <- dat_[["Cutoff"]][["testPerf"]][, c('Pos Pred Value', 'Neg Pred Value',
                                                             'Accuracy', 'F1', 'Sensitivity', 'Specificity',
                                                             'weightScore')]
            df_cutOff <- rbind(df_cutOff, df_cutOff_, df_cutOff_)
            
            sets <- c(sets, 'train', 'test')
            stra <- c(stra, 'all', 'all')
            elem <- c(elem, 'all', 'all')
            model <- c(model, model_p, model_p)
            featu <- c(featu, featu_, featu_)
            location <- c(location, paste(samp_p, imp_p, sep = '__'), paste(samp_p, imp_p, sep = '__'))
            
            pred_X <- c(seq(0, .01, length.out = 200), seq(.01, .99, length.out = 98), seq(.99, 1, length.out = 200))  # 498
            train.pred <- get_nlrPred(dat = dat_[["auroc"]][["plotData.train"]], pred_X = pred_X)
            test.pred <- get_nlrPred(dat = dat_[["auroc"]][["plotData.test"]], pred_X = pred_X)
            
            df_sen_spe <- rbind(df_sen_spe, data.frame(spe = c(pred_X, pred_X),
                                                       sen = c(as.numeric(train.pred[[1]]), as.numeric(test.pred[[1]])), 
                                                       R2 = c(rep(train.pred[[2]], 498), rep(test.pred[[2]], 498)),
                                                       type = c(rep('train', 498), rep('test', 498)), 
                                                       location = paste(samp_p, imp_p, sep = '__'),
                                                       stra = 'all', elem = 'all',
                                                       model = model_p, featu = featu_
            ))
            
            #     df_plot.train <- data.frame(list(spe = pred_X, sen = as.numeric(train.pred) ))
            #     get_simpleRocPlot(df_plot = df_plot.train)
            #      
            #     df_plot.test <- data.frame(list(spe = pred_X, sen = as.numeric(test.pred) ))
            #     get_simpleRocPlot(df_plot = df_plot.test)
            #      
            #     Sys.sleep(10)
          }
        }
      }
    }
  }
}



df_ro_prc_cutOff <- data.frame(list(  # Aggregate results
  auroc_v = auroc_v,
  auprc_v = auprc_v,
  cutOff_validation = cutOff_validation,
  cutOff_test = cutOff_test,
  optPoint = optPoint,
  sets = sets,
  stra = stra,
  elem = elem,
  model = model,
  featu = featu,
  location = location,
  df_cutOff
))

rownames(df_ro_prc_cutOff) <- 1:nrow(df_ro_prc_cutOff)

getwd()
#         write.csv(df_ro_prc_cutOff, 'df_ro_prc_cutOff.csv')   ----240820

df_ro_prc_cutOff <- read.csv('./df_ro_prc_cutOff.csv')    # -------------------------------------------------Intermediate data loading


# Summary of model parameters-------------Table 2------------------
model_patam <- list(
  RF = RF[["featureIn1"]][["param"]][["all"]],
  SVM = SVM[["featureIn1"]][["param"]][["all"]],
  XGBoost = XGB[["featureIn1"]][["param"]][["all"]]
)

for (i in names(model_patam)){
  write.table(model_patam[[i]], 'model_params.txt', append = T)
}


#分层-------------------------------------------------------------

dat_featu <- df_ro_prc_cutOff[df_ro_prc_cutOff[['featu']] == 'coreFeatures', ]
dat_set <- dat_featu %>% dplyr::filter(sets == 'test') %>% 
  dplyr::mutate(auroc_adjust = auroc_v - .5)


dat_set[dat_set[['stra']] == 'age', 'elem'] <- ifelse(dat_set[dat_set[['stra']] == 'age', 'elem'] == '0', '2-4', 
                                                      ifelse(dat_set[dat_set[['stra']] == 'age', 'elem'] == '1',
                                                             '5', '6-8'))

dat_set[dat_set[['stra']] == 'birthWay', 'elem'] <- ifelse(dat_set[dat_set[['stra']] == 'birthWay', 'elem'] == '1',
                                                           'eutocia', 'cesarean') 

dat_set[dat_set[['stra']] == 'sex', 'elem'] <- ifelse(dat_set[dat_set[['stra']] == 'sex', 'elem'] == '1',
                                                      'boy', 'girl') 


# Cut-off Value Tabulation - Schedule ----- & Radar Charting-----------------------------------------------------

df_cutOff_tab <- aggregate(x=dat_set[, c('optPoint', 'Pos.Pred.Value', 'Neg.Pred.Value', 'Accuracy', 'F1',
                                         'Sensitivity', 'Specificity', 'weightScore')],
                     by = list(dat_set[['model']], dat_set[['elem']], dat_set[['stra']]),
                     FUN = function(x){c(round(mean(x, na.rm = T), 2))}) %>% as.data.frame()

write.csv(df_cutOff_tab, '_tmp_plot.csv')
df_cutOff_tab <- read.csv('_tmp_plot.csv', row.names = 1)  # The first column is the index

colnames(df_cutOff_tab) <- c('model', 'elem', 'stra', 'optPoint', 'Pos.Pred.Value', 'Neg.Pred.Value', 'Accuracy', 'F1',
                             'Sensitivity', 'Specificity', 'weightScore')


identical(round(get_WeightScore(df_cutOff_tab), 2), df_cutOff_tab$weightScore)  
#     View(cbind(round(get_WeightScore(df_cutOff_tab), 2), df_cutOff_tab$weightScore))
#     View(cbind(get_WeightScore(df_cutOff_tab), df_cutOff_tab$weightScore))
#     
#     View(cbind(round(get_WeightScore(df_cutOff), 2), round(df_cutOff$weightScore, 2)))

df_cutOff_tab$weightScore <- round(get_WeightScore(df_cutOff_tab), 2)
df_cutOff_tab <- df_cutOff_tab %>% dplyr::select(c('stra', 'elem', 'model', everything()))

df_cutOff_tab2 <- df_cutOff_tab %>% dplyr::mutate(
  class = paste0(stra, '__', elem)
) %>% dplyr::select(-c('stra', 'elem', 'weightScore')) %>% dplyr::select(class, everything())

for (i in unique(df_cutOff_tab2[['class']])){
  dat <- df_cutOff_tab2[df_cutOff_tab2$class == i, ] %>%  dplyr::select(-c('optPoint'))
  dat[nrow(dat) + 1, ] <- c('nothing', 'Max', as.numeric(apply(df_cutOff_tab2[, 4:9], MARGIN = 2, FUN = function(x){max(x)})))
  dat[nrow(dat) + 1, ] <- c('nothing', 'Min', as.numeric(apply(df_cutOff_tab2[, 4:9], MARGIN = 2, FUN = function(x){min(x)})))
  rownames(dat) <- dat[, 'model']
  dat <- dat %>% dplyr::select(-c('class', 'model'))
  dat <- df_strToNum(dat[c('Max', 'Min', 'LR', 'SVM', 'RF', 'XGBoost'), ])
  
  color <- c("#20c997", "#228be6", "#b197fc", "#e03131")
  
  Cairo::CairoPDF(file = pastev(c('radar_', i, '.pdf'), sep_ = ''), width = 10.5, height = 7) 
  plot02 <- fmsb::radarchart(dat,title = c("Three Spider charts"), axistype = 2, seg = 6,
                       # axislabels = seq(0, 1, 0.2),
                       pcol = color,
                       pfcol = scales::alpha(color, c(.4, .3, .2, .2)), 
                       plwd = 2, plty = 1,
                       cglcol = "grey", cglty = 1, cglwd = 0.8,  # Customize the grid
                       axislabcol = "grey",   # Customize the axis
                       vlabels = colnames(dat),vlcex = 1,  # Variable labels
  )
  legend( #Adding a Legend
    x = "bottomright", legend = c('LR','SVM','RF', 'XGBoost'), 
    horiz = F, bty = "n", pch = 20 , col = color,
    text.col = "black", cex = 1, pt.cex = 1.
  )
  dev.off()
}

write.xlsx(df_cutOff_tab, 'df_cutOff_tab.xlsx')

# Radar plotting of the performance of each indicator at the cut-off value:
df_cutOff_tab %>% df_cutOff_tab %>% dplyr::mutate(
  
)



# Stratified performance-----------------------Figure 2 & 3-----------------------------------

for (task_p in c('auroc_adjust', 'optPoint', 'cutOff_validation', 'cutOff_test')){
  if (task_p == 'optPoint'){break}
  
  df_plot <- aggregate(x=dat_set[, task_p],
                       by = list(dat_set[['model']], dat_set[['elem']], dat_set[['stra']]),
                       FUN = function(x){c(mean(x, na.rm = T), sd(x, na.rm = T),
                                           sd(x, na.rm = T)/sqrt(length(x)))}) %>% as.data.frame()
  
  write.csv(df_plot, '_tmp_plot.csv')
  df_plot <- read.csv('_tmp_plot.csv', row.names = 1) 
  
  colnames(df_plot) <- c('model', 'elem', 'stra', 'auroc_adjust', 'sd', 'se')
  
  
  df_plot <- df_plot %>% dplyr::mutate(
    stra = factor(stra, levels = c('sex', 'birthWay', 'age', 'all'),
                  labels = c('sex', 'birthWay', 'age', 'all')),
    elem = factor(elem, levels = c('boy', 'girl', 'eutocia', 'cesarean', '2-4', '5', '6-8', 'all'),
                  labels = c('boy', 'girl', 'eutocia', 'cesarean', '2-4', '5', '6-8', 'all')),
    model = factor(model, levels = c('LR', 'SVM', 'RF', 'XGBoost'),
                   labels = c('LR', 'SVM', 'RF', 'XGBoost'))
  )
  
  
  
  par(mar = c(1, 1, 1, 1))
  ggp1 <- ggplot(df_plot, aes(x = elem, weight = auroc_adjust, fill = model)) +
    geom_hline(yintercept = seq(.1, ifelse(task_p == 'auroc_adjust', .3, .5), .1), 
               color = 'gray', linetype = 'dashed') +
    geom_bar(color = "black", width = .7, position = 'dodge') +
    scale_y_continuous(expand = c(0,0)) +
    geom_errorbar(aes(ymin = auroc_adjust - sd, ymax = auroc_adjust + sd), width = 0.25,
                  size = 0.3, position = position_dodge(0.7)) +
    labs(x = NULL, y = ifelse(task_p == 'auroc_adjust', 'AUROC - 0.5', 'weightScore')) +
    #   scale_color_manual(values = c('#ff922b', '#4dabf7', '#12b886', '#fa5252', '#9775fa', 'yellow')) + 
    scale_fill_brewer(palette = "Set3")+
    scale_y_continuous(expand = c(0,0)) +
    facet_grid(. ~ stra, scales = 'free', space = 'free_x') +
    theme_default
  
  print(ggp1)
  
  ggsave(filename = pastev(c(task_p, '_barPlot', '.pdf'), sep_ = ''), plot = ggp1, 
         width = 28, height = 10, units = 'cm')
}



# RF 和 XGBoost 与 LR的AUROC t检验-------------------------------------------


df_compara <- dat_set[dat_set[['sets']] == 'test' & dat_set[['model']] != 'SVM',
                      c('auroc_v', 'stra', 'elem', 'model')]


df_compara_RF <- df_compara[df_compara[['model']] != 'XGBoost', ]
df_compara_RF <- aggregate(x=df_compara_RF[, c('auroc_v')],
                     by = list(df_compara_RF[['model']], df_compara_RF[['stra']], df_compara_RF[['elem']]),
                     FUN = function(x){x}) %>% as.data.frame()

write.csv(df_compara_RF, '_tmp_plot.csv')
df_compara_RF <- read.csv('_tmp_plot.csv', row.names = 1) 

colnames(df_compara_RF) <- c('model', 'stra', 'elem', paste0('v', 1:100))


df_compara_RF_dat <- list()
for (elem_p in unique(paste0(df_compara_RF$elem, '__', df_compara_RF$stra))){
  print(elem_p)
  
  stra_ <- strsplit(elem_p, '__', fixed = T)[[1]][2]
  elem_ <- strsplit(elem_p, '__', fixed = T)[[1]][1]
  logi_ <- df_compara_RF[['stra']] == stra_ & df_compara_RF[['elem']] == elem_
  
  df_compara_RF_dat[[elem_p]] <- data.frame(list(
    LR = as.numeric(df_compara_RF[df_compara_RF[['model']] == 'LR' & logi_, 4:103]) - 0.5, 
    RF = as.numeric(df_compara_RF[df_compara_RF[['model']] == 'RF' & logi_, 4:103]) - 0.5
  ))
  
}

getwd()
Roc_compara <- get_pairedTest(object = df_compara_RF_dat, output = T, filename = 'LR_RF_Roc_compara')



# Plotting the ROC 95% CI----------------------Figure 1----------------------------



for (type_p in c('train', 'test')){
  
  df_95CI <- df_sen_spe[df_sen_spe[['featu']] == 'coreFeatures' & df_sen_spe$type == type_p, ]
  
  df_95CI2 <- aggregate(x=df_95CI[, 'sen'],
                        by = list(df_95CI[['type']], df_95CI[['spe']], df_95CI[['model']], df_95CI[['elem']], df_95CI[['stra']]),
                        FUN = function(x){c(mean(x, na.rm = T), sd(x, na.rm = T),
                                            sd(x, na.rm = T)/sqrt(length(x)))}) %>% as.data.frame()
  
  write.csv(df_95CI2, '_tmp_plot.csv')
  df_95CI2 <- read.csv('_tmp_plot.csv', row.names = 1) 
  
  colnames(df_95CI2) <- c('type', 'spe', 'model', 'elem', 'stra', 'sen_adjust', 'sd', 'se')
  
  df_95CI2[df_95CI2[['stra']] == 'age', 'elem'] <- ifelse(df_95CI2[df_95CI2[['stra']] == 'age', 'elem'] == '0', '2-4', 
                                                          ifelse(df_95CI2[df_95CI2[['stra']] == 'age', 'elem'] == '1',
                                                                 '5', '6-8'))  # "0 = 2-4岁；1 = 5岁；2 = 6-8岁"
  
  df_95CI2[df_95CI2[['stra']] == 'birthWay', 'elem'] <- ifelse(df_95CI2[df_95CI2[['stra']] == 'birthWay', 'elem'] == '1',
                                                               'eutocia', 'cesarean')  # "1=顺产；2=剖宫产"
  
  df_95CI2[df_95CI2[['stra']] == 'sex', 'elem'] <- ifelse(df_95CI2[df_95CI2[['stra']] == 'sex', 'elem'] == '1',
                                                          'boy', 'girl')  # "1=男；2=女"
  
  
  df_95CI2 <- df_95CI2 %>% dplyr::mutate(
    stra = factor(stra, levels = c('sex', 'birthWay', 'age', 'all'),
                  labels = c('sex', 'birthWay', 'age', 'all')),
    elem = factor(elem, levels = c('boy', 'girl', 'eutocia', 'cesarean', '2-4', '5', '6-8', 'all'),
                  labels = c('boy', 'girl', 'eutocia', 'cesarean', '2-4', '5', '6-8', 'all')),
    model = factor(model, levels = c('LR', 'SVM', 'RF', 'XGBoost'),
                   labels = c('LR', 'SVM', 'RF', 'XGBoost')),
    type = factor(type, levels = c('train', 'test'),
                  labels = c('train', 'test'))
  )
  
  
  
  df_95CI2_all <- df_95CI2[df_95CI2[['elem']] == 'all', ]
  df_95CI2_all <- df_95CI2_all %>% dplyr::mutate(spe_1 = round(1-spe, 4))
  
  df_plot <- df_95CI2_all
  
  par(mar = c(2, 2, 2, 2))
  ggp2 <- ggplot(data = df_plot[df_plot$model == 'LR', ], aes(x = spe_1, y = sen_adjust)) +
    geom_ribbon(aes(ymin = sen_adjust - sd, ymax = sen_adjust + sd), alpha = 0.45, fill = '#20c997') +
    geom_ribbon(data = df_plot[df_plot$model == 'SVM', ],
                aes(ymin = sen_adjust - sd, ymax = sen_adjust + sd), alpha = 0.35, fill = '#228be6') +
    geom_ribbon(data = df_plot[df_plot$model == 'RF', ],
                aes(ymin = sen_adjust - sd, ymax = sen_adjust + sd), alpha = 0.3, fill = '#b197fc') +
    geom_ribbon(data = df_plot[df_plot$model == 'XGBoost', ],
                aes(ymin = sen_adjust - sd, ymax = sen_adjust + sd), alpha = 0.3, fill = '#e03131') +
    geom_line(data = df_plot[df_plot$model == 'LR', ], aes(x = spe_1, y = sen_adjust),
              size = 0.45, color = '#20c997', alpha = 1) +
    geom_line(data = df_plot[df_plot$model == 'SVM', ], aes(x = spe_1, y = sen_adjust),
              size = 0.45, color = '#228be6', alpha = 1) +
    geom_line(data = df_plot[df_plot$model == 'RF', ], aes(x = spe_1, y = sen_adjust),
              size = 0.45, color = '#b197fc', alpha = 1) +
    geom_line(data = df_plot[df_plot$model == 'XGBoost', ], aes(x = spe_1, y = sen_adjust),
              size = 0.45, color = '#e03131', alpha = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = .5) +
    xlab('1 - Specificities') + 
    scale_y_continuous(expand=c(0,0),  # 让柱子底部与X轴紧贴
                       breaks = c(.25, .50, .75, 1)) +
    scale_x_continuous(expand=c(.002,0)) +
    ylab('Sensitivities') +
    facet_grid(. ~ type) +
    facet_grid(. ~ type, scales = 'free', space = 'free_x') +
    theme_default
  
  print(ggp2)
  ggsave(filename = pastev(c(type_p, '_ROC', '.pdf'), sep_ = ''), plot = ggp2, 
         width = 14, height = 15, units = 'cm')
}

df_core_test_all <- df_ro_prc_cutOff %>% 
  dplyr::filter(stra == 'all' & featu == 'coreFeatures')  # nrow = 200

df_core_test_all <- aggregate(x=df_core_test_all[, 'auroc_v'],
                              by = list(df_core_test_all[['model']], df_core_test_all[['sets']]),
                              FUN = function(x){c(mean(x, na.rm = T), sd(x, na.rm = T),
                                                  sd(x, na.rm = T)/sqrt(length(x)))}) %>% as.data.frame()

write.csv(df_core_test_all, '_tmp_plot.csv')
df_core_test_all <- read.csv('_tmp_plot.csv', row.names = 1) 

colnames(df_core_test_all) <- c('model', 'sets', 'auroc', 'sd', 'se')

write.xlsx(df_core_test_all, file = 'core_test_all.xlsx')




# Characteristic Importance -----------------------------------------------------------
    #-----age5 and all, Two groups of data
dat_age5 <- rbind(data.mergeADA1[["nominal_"]][["sample_3"]][["age"]][["1"]][["imp_1"]][['train']],
                  data.mergeADA1[["nominal_"]][["sample_3"]][["age"]][["1"]][["imp_1"]][['validation']])
dat_all <- rbind(data.mergeADA1[["nominal_"]][["sample_3"]][["all"]][["imp_1"]][['train']],
                 data.mergeADA1[["nominal_"]][["sample_3"]][["all"]][["imp_1"]][['validation']])

var_standart <- data.frame(list(
  var = c("dadEducation", "haveOlderSblings", "孩子0_1岁曾接受过抗生素治疗",
          "孩子0_1岁有过至少连续6个月反复发作的瘙痒疹子", "孩子0到1岁期间.是否有人抽烟.父亲",
          "孩子0到1岁时.是否在孩子住所杨宠物或植物", "孩子父亲是否患有.过敏性.鼻炎",
          "孩子母亲是否患有.过敏性.鼻炎", "孩子母亲是否患有湿疹.或过敏性皮炎.",
          "孩子是否有因食物引起的湿疹.荨麻疹.腹泄等", "孩子兄妹.若有.是否患有.过敏性.鼻炎", "开花植物.", 
          "母亲怀孕期间期间.是否有人抽烟.父亲", "爷爷.."),
  eng = c("Father’s education", 'Have older sblings', 'Antibiotic therapy during cfy',
          'Successive bouts of rash more \nthan 6 months during cfy', 'Father smoking during cfy', 
          'Feeding pats or growing \nplants during cfy', 'Father with AR', 'Mother with AR', 
          'Mother with AD', 'Child with food allergy', 'Siblings with AR', 
          'Flowers planting during cfy', 'Father smoking during mp',
          'Paternal grandfather smoking\n during mp')
))

  # random forest：

get_varImpPlot <- function(var, value, saveOrNot = F, fileName = 'NULL',
                           xy_ = c(21, 12), unit = 'cm'){
  # var: variable name
  # value: importance value
  # saveOrNot: whether to save or not
  # fileName: the name of the file - no brainer!
  # xy_: c(width, height)
  # unit: centimetre (cm), inch (in)
  
  library(showtext)
  font_add('Arial','/Library/Fonts/Arial.ttf') 
  showtext_auto() 
  
  df_ <- data.frame(list(
    var = var,
    value = value
  ))
  ggp1 <- ggplot(data = df_, aes(x = value, y = reorder(var, value))) +
    geom_point(size = 3, color = '#da77f2') + 
    theme_bw() +
    ylab(NULL) +
    xlab('Importance of variables\n(MeanDecreaseGini)') +
    theme_classic() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = 'grey60', linetype = 'dashed'),
      axis.text.x = element_text(family = 'Arial'
                                 #, face = 'italic'
                                 , colour = 'black'
                                 , size = rel(1.5)  
      ),
      axis.text.y = element_text(family = 'Arial'
                                 #, face = 'italic'
                                 , colour = 'black'
                                 , size = rel(1.5)),
      axis.title = element_text(family = 'Arial'  
                                #, face = 'italic'
                                , colour = 'black'
                                , size = rel(1.5)),
      axis.ticks = element_line(color = "black", size = 0.6,lineend = 2),
      axis.line = element_line(linetype = 1,color = "black",size = 0.8, lineend = 'square')  
    )
  
  print(ggp1)
  if (saveOrNot){
    ggsave(filename = paste(fileName, '.pdf', sep = ''), plot = ggp1, 
           width = xy_[1], height = xy_[2], units = unit)
  }
}


rf_model <- list(
  age5 = getRF_model(model_dat_ = dat_age5,
                        formula_ = as.formula('event ~ .'), optParam = RF_ret1[["param"]][["age_1"]]),
  all = getRF_model(model_dat_ = dat_all,
                       formula_ = as.formula('event ~ .'), optParam = RF_ret1[["param"]][["all"]])
)


varImpPlot(rf_model[['age5']])

for (rf.i in names(rf_model)){
  model_ <- rf_model[[rf.i]]
  
  rf.varImp <- data.frame(
    list(
      var = rownames(model_$importance),
      value = as.numeric(model_$importance[, 'MeanDecreaseGini'])
    )
  )
  
  rf.varImp <- merge(x = var_standart, y = rf.varImp, by = 'var')
  
  rf_top10 <- reorder(rf.varImp$var, rf.varImp$value)[1:10]
  rf.varImp <- rf.varImp[rf.varImp$var %in% rf_top10, ]
  
  get_varImpPlot(var = rf.varImp$eng, value = rf.varImp$value,
                 saveOrNot = T, fileName = paste('rfVarImp_', rf.i, sep = ''), xy_ = c(16, 11.43), unit = 'cm')
}



  # XGBoost：

xgb_model <- list(
  age5 = list(
    model = getXGB_model(model_dat_ = dat_age5,
                         formula_ = as.formula('event ~ .'), optParam = XGB_ret1[["param"]][["age_1"]]),
    pred_X = data.mergeADA1[["nominal_"]][["sample_3"]][["age"]][["1"]][["imp_1"]][['testX']]
  ),
  all = list(
    model = getXGB_model(model_dat_ = dat_all,
                         formula_ = as.formula('event ~ .'), optParam = XGB_ret1[["param"]][["all"]]),
    pred_X = data.mergeADA1[["nominal_"]][["sample_3"]][["all"]][["imp_1"]][['testX']]
  )
)


        # Building SHAP Interpretable Machine Learning



sv_waterfall(xgb.shp, row_id = 2)  # waterfall plot
sv_force(xgb.shp,row_id = 2)  # force plot 

for (xgb.i in names(xgb_model)){  
  model_ <- xgb_model[[xgb.i]][['model']]
  xgb.shp <- shapviz(object = model_, X_pred = as.matrix(
    df_strToNum(xgb_model[[xgb.i]][['pred_X']])))
  
  View(cbind(colnames(xgb.shp$X), var_standart$var[di_which(colnames(xgb.shp$X), var_standart$var)]))
  colnames(xgb.shp$S) <- var_standart$eng[di_which(colnames(xgb.shp$S), var_standart$var)]
  colnames(xgb.shp$X) <- var_standart$eng[di_which(colnames(xgb.shp$X), var_standart$var)]
  
  pdf(pastev(c('shap_', xgb.i, '.pdf'), sep_ = ''), width = 7.5, height = 4.5)  
  
  sv_importance(xgb.shp, kind = "both"
                , alpha = 0.2, bar_width = .7
                , fill = '#8ce99a'
                , bee_width = 0.4
                , max_display = 10
  ) +
    scale_color_gradient(low = '#5c7cfa', high = '#fa5252') + 
    theme_classic() +
    theme_default
  
  dev.off()
}



# Comparison of model performance after adding or subtracting features-------------------------------------------------------


df_variFeature <- df_ro_prc_cutOff %>% 
  dplyr::filter(stra == 'all', elem == 'all', sets == 'test') %>% 
  dplyr::select(auroc_v, featu, model) %>% 
  dplyr::mutate(auroc_v = auroc_v - 0.5)

variFeature_dec_dat <- list()
for (model_p in unique(df_morefeature[['model']])){
  variFeature_dec_dat[[model_p]] <- data.frame(list(
    group = as.factor(c(rep('coreFeatures', 50), rep('moreFeatures', 50), rep('lessFeatures', 50))),
    values = c(as.numeric(df_morefeature[df_morefeature[['featu']] == 'coreFeatures' & 
                                           df_morefeature[['model']] == model_p, 'auroc_v'][1:50]) ,
               as.numeric(df_morefeature[df_morefeature[['featu']] == 'moreFeatures' & 
                                           df_morefeature[['model']] == model_p, 'auroc_v']),
               as.numeric(df_morefeature[df_morefeature[['featu']] == 'lessFeatures' & 
                                           df_morefeature[['model']] == model_p, 'auroc_v']))
  ))
}

        # ################This custom function is used for hypothesis testing and involves many other applications which we will not show#####################################
Roc_variFeatures <- get_hypothesisTesting(object = variFeature_dec_dat,  # Groups are not easy to merge, so the direct input list object ----- type and name are strictly limited, please refer to function
                                     x = NA, y = NA, reverse = F,  # object not NA
                                     ignoreNormal = T,  # small sample size（< 40）
                                     appoximately_normal = F, 
                                     adj_method = 'bonferroni',  
                                     exact_ = T,  
                                     maxGroup = 3,  
                                     filename_ = 'variFeatures_hypothesis_test'
)

df_variFeature2 <- aggregate(x=df_variFeature[, 'auroc_v'],
                     by = list(df_variFeature[['featu']], df_variFeature[['model']]),
                     FUN = function(x){c(mean(x, na.rm = T), sd(x, na.rm = T),
                                         sd(x, na.rm = T)/sqrt(length(x)))}) %>% as.data.frame()

write.csv(df_variFeature2, '_tmp_plot.csv')
df_variFeature2 <- read.csv('_tmp_plot.csv', row.names = 1) 

colnames(df_variFeature2) <- c('featu', 'model', 'auroc_adjust', 'sd', 'se')




df_variFeature2 <- df_variFeature2 %>% dplyr::mutate(
  featu = factor(featu, levels = c('lessFeatures', 'coreFeatures', 'moreFeatures'),
                 labels = c('lessFeatures', 'coreFeatures', 'moreFeatures')),
  model = factor(model, levels = c('LR', 'SVM', 'RF', 'XGBoost'),
                 labels = c('LR', 'SVM', 'RF', 'XGBoost'))
)

par(mar = c(1, 1, 1, 1))
ggp3 <- ggplot(df_variFeature2, aes(x = model, weight = auroc_adjust, fill = featu)) +
  geom_hline(yintercept = seq(.1, .3, .1), color = 'gray', linetype = 'dashed') +
  geom_bar(color = "black", width = .7, position = 'dodge') +
  scale_y_continuous(expand = c(0,0)) +
  geom_errorbar(aes(ymin = auroc_adjust - sd, ymax = auroc_adjust + sd), width = 0.25,
                size = 0.3, position = position_dodge(0.7)) +
  labs(x = NULL, y = 'AUROC - 0.5') +
  #   scale_color_manual(values = c('#ff922b', '#4dabf7', '#12b886', '#fa5252', '#9775fa', 'yellow')) + 
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(expand = c(0,0)) +
  # facet_grid(. ~ stra, scales = 'free', space = 'free_x') +
  theme_default

print(ggp3)

ggsave(filename = pastev(c('variFeature', '_barPlot', '.pdf'), sep_ = ''), plot = ggp3, 
       width = 28, height = 12, units = 'cm')


getwd()
#   save.image("./END_save.RData")    -------240414
load("./END_save.RData")









#  Checkout----------------------------
get_simpleRocPlot <- function(df_plot){
  par(mar = c(2, 2, 2, 2))
  ggp2 <- ggplot(data = df_plot, aes(x = 1-spe, y = sen)) +
    geom_line(size = .7) + 
    scale_color_manual(values = c('#12b886', '#9775fa', '#4dabf7', '#ff922b', 'yellow', '#fa5252')) +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = .7) +
    xlab('1 - Specificities') + 
    scale_y_continuous(expand=c(0,0), 
                       breaks = c(.25, .50, .75, 1)) +
    scale_x_continuous(expand=c(.002,0)) +
    ylab('Sensitivities') +
    # facet_grid(. ~ class) +
    # facet_grid(. ~ class, scales = 'free', space = 'free_x') +
    theme_classic() +
    theme(
      axis.text.x = element_text(family = 'Arial'
                                 #, face = 'italic'
                                 , colour = 'black'
                                 , size = rel(1.5)  
      ),
      axis.text.y = element_text(family = 'Arial'
                                 #, face = 'italic'
                                 , colour = 'black'
                                 , size = rel(1.5)),
      axis.title = element_text(family = 'Arial'
                                #, face = 'italic'
                                , colour = 'black'
                                , size = rel(1.5)),
      axis.ticks = element_line(color = "black", size = 0.6,lineend = 2),  
      axis.line = element_line(linetype = 1,color = "black",size = 0.8, lineend = 'square'), 
      strip.text = element_text(face = 'bold', size = rel(1.15)),
      panel.spacing.x = unit(.4, 'cm'), 
      legend.title = element_text(  
        face = 'italic',
        family = 'Arial',
        colour = 'black',
        size = rel(1.1)
      ),
      legend.text = element_text(
        face = 'italic',
        family = 'Arial',
        colour = 'black',
        size = rel(0.9)
      )
    )
  
  print(ggp2)
}

df_plot <- data.frame(list(
  spe = df_95CI2_all[df_95CI2_all$type == 'test' & df_95CI2_all$model == 'XGBoost', 'spe'],
  sen = df_95CI2_all[df_95CI2_all$type == 'test' & df_95CI2_all$model == 'XGBoost', 'sen_adjust']
))

get_simpleRocPlot(df_plot = df_plot)


get_simpleRocPlot(df_plot = RF_ret1[["sample_1"]][["all"]][["imp_2"]][["auroc"]][["plotData.train"]])

get_simpleRocPlot(df_plot = RF_ret1[["sample_1"]][["all"]][["imp_2"]][["auroc"]][["plotData.test"]])



# # make corrections-------24/08/19

a1 <- caret::confusionMatrix(factor(rep(c('1', '0'), c(5, 10))), factor(rep(c('1', '0'), c(10, 5))), positive = '1')
a0 <- caret::confusionMatrix(factor(rep(c('1', '0'), c(5, 10))), factor(rep(c('1', '0'), c(10, 5))), positive = '0')

a0_outp <- as.data.frame(matrix(c('0', as.vector(c(a0[["overall"]][1:2], a0[["byClass"]]))),
                                nrow = 1, ncol = length(c(a0[["overall"]][1:2], a0[["byClass"]])) + 1,
                                dimnames = list(rownames = 1, colnames = c('responseSeting', names(c(a0[["overall"]][1:2], a0[["byClass"]]))))))


a0_outp <- as.data.frame(matrix(c('0', as.vector(c(a0[["overall"]][1:2], a0[["byClass"]]))), 
                                nrow = 1, ncol = length(c(a0[["overall"]][1:2], a0[["byClass"]])) + 1, 
                                dimnames = list(rownames = 1, colnames = c('responseSeting', names(c(a0[["overall"]][1:2], a0[["byClass"]]))))))

a1_0 <- rbind(a1_outp, a0_outp)

write.xlsx(a1_0, 'confusionMatrix_positive订正.xlsx')



# data output----- for python---------------------------------

get_pyDat <- function(df){
  py_ind <- sample(1:nrow(df), round(0.4*nrow(df), 0), replace = F)
  df2 <- df
  colnames(df2) <- c('event', 'mom_with_AR', 'dad_with_AR', 'haveOlderSblings', 'child with AD', 'dadEducation')
  return(df2[py_ind, ])
}


dat_forPython <- list(
  trian = get_pyDat(data.mergeADA3[["nominal_"]][["sample_3"]][["all"]][["imp_1"]][["train"]]),
  validation = get_pyDat(data.mergeADA3[["nominal_"]][["sample_3"]][["all"]][["imp_1"]][["validation"]]),
  test = get_pyDat(data.mergeADA3[["nominal_"]][["sample_3"]][["all"]][["imp_1"]][["test"]])
  )

write.xlsx(dat_forPython, 'dat_forPython.xlsx')




