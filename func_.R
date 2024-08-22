
# This file contains most of the custom functions needed in Rhinitis_upload.---------------------------------------



# which expansion=========================================

di_which <- function(x,y){  # # x,y are both vectors, the former being the datum;
  # Documentation：Find the position of the y-vector in the x-vector and return the vector; if not, return NA
  y_ret <- c()
  for (i in y){
    if (length(which(x==i)) != 0){
      y_ret <- c(y_ret,which(x==i))
    }else{y_ret <- c(y_ret,NA)}
  }
  return(y_ret)
}


# paste expansion=======================================
pastev <- function(v,sep_){
  # Documentation：String splicing with interval sep_ for vector v
  s <- c()
  for (i in v){
    if (i == v[1]){
      s <- paste(s,i,sep='')
    }else{s <- paste(s,i,sep=sep_)}
  }
  return(s)
}


# Data frame file saved --------------------------------------------------
dfSave <- function(obj_, file_name = 'xxx'){  # Save csv,txt and RData data in the current working path.
  # obj: dataframe object to save to
  # file_name: 
  write.csv(x = obj_, file = paste(file_name, '.csv', sep = ''))
  write.table(x = label(obj_), file = paste(file_name, '.txt', sep = ''))
  save(obj_, file = paste(file_name, '.RData', sep = ''))
}

#Boolean Vector NA Processing--------------------------------------------------
boolVector_na_to_false <- function(v){
  v[is.na(v)] <- FALSE
  return(v)
}


#View the composition ratios (including NA) and return columns with composition ratios below the threshold -----------------------------------
get_compositionDrop <- function(df_, threshold_=0.995){
  # Get the columns of the dichotomous variable dataframe df_ that have a very low composition ratio, showing the calculation process
  # Quick start: the closer threhold_ is to 1, the fewer results are filtered; <<====>> the closer it is to 0, the better the filter is and the more results are filtered.
  sum_ <- dim(df_)[1]
  c_ret=c()
  for (c in colnames(df_)){
    print(c)
    tab_ <- table(df_[,c])
    tab_2 <- table(df_[,c], exclude = NULL)
    print((tab_2/dim(df_)[1])*100)
    if (tab_[1]/sum_>threshold_ | tab_[1]/sum_<1-threshold_){
      c_ret <- c(c_ret, c)
    }
  }
  return(c_ret)
}




#Acquisition of ML data division------------------------------------------------------------------
get_mlDataDivide <- function(ml_df_, feature_in_, partition_ = list(times_ = 3, p1 = .6, p2 = .2, p3 = .2), seed_ = 4, 
                             imp_ = list(meth = rep('rf', ncol(ml_df_)), m = 3, maxit = 10),
                             targ_var_ = 'eczema_doctor', stra_df_ = data.frame(), stra_var_ = colnames(stra_df_),
                             doResample = T, SMOTE_ = list(do = F, size = 2), 
                             ADASYN_ = list(do = T, beta_ = 0.7, dist = "Overlap"), threhold_ = 0.985){
  # document:  
  # parameters:
  # ml_df_ i.e. total data used for ML, non-factorised -------- not feature-qualified
  # features incorporated by the model
  # partition_: times_, i.e. the number of times createDataPartition() is repeated, p, i.e. the proportion of the training set partitioned.
  # imp_: parameter for multiple interpolation
  # ind_: the index matrix returned by createDataPartition(), and its parameter times_ corresponds to the m-value below.
  # targ_var_: the ending feature.
                # return: return 2*m*4 list
                # 2 corresponds to the case where the ending variable is 0/1 or zero/one.
                # m corresponds to different index values
                # 4 i.e. trainX, testX, trainY, testX
  # stra_: hierarchical ----- guarantee: stra_var_ is ‘sex’, ‘nation’, ‘age’
  # smote_orNot_ : whether to perform internal augmentation - i.e. ‘whether to line divide the dataset and augment the training set only’
  # smote_dupSize_ : minority class augmentation times
  # beta: multiple of minority class to majority class after ADASYN_ method sampling (default 0.7x)
  # -------------2024 03 01 Change ------ to set data padding to be done internally
  
  ########I will explain the general framework of the function, the following code is only used to assist in the understanding of the function, and can not be completed run#########
  #step1. Building a data framework
  ret <- list(
    nominal_ = list(),  # The ending variable is 0/1
    zero_one = list()  #The ending variable is zero/one ----XGBoost models need to be
  )
  
  for (c in 1:partition_[['times_']]){  
    ret[[1]][[paste('sample_', c, sep = '')]] <- list()  
    ret[[2]][[paste('sample_', c, sep = '')]] <- list()
    for (stra_ in stra_var_){
      ret[[1]][[paste('sample_', c, sep = '')]][[stra_]] <- list()
      ret[[2]][[paste('sample_', c, sep = '')]][[stra_]] <- list()
      for (elem_ in names(table(stra_df_[[stra_]]))){
        ret[[1]][[paste('sample_', c, sep = '')]][[stra_]][[elem_]] <- list()
        ret[[2]][[paste('sample_', c, sep = '')]][[stra_]][[elem_]] <- list()
      }
    }
  }
  # With the above steps, the framework of the model used can be obtained as follows:
  list(
    nominal_ = list(  # The ending variable is presented as 0/1
      sample_1 = list(  # i.e. the number of random divisions of the data set
        stra_1 = list(  # i.e. sex
          sub_group1 = list(  # i.e. male/female
            imp_1 = list(  # Multiple interpolation fill returns multiple sets of fill data
              train0,  # Data frame format, i.e. training set without upsampling
              train,  # Data frame format, training set after oversampling
              validation,  # Data frame format, no upsampling
              test,  # Data frame format, no upsampling
              trainX,  # Data frame format, predictor variable data for train
              validationX,  # Data frame format，
              testX,  # Data frame format，
              tainY,  # Vector format, outcome variable data for 'train'
              validationY,  # Vector format
              testY,  # Vector format
            ),
            imp_2 = list(...),
            imp_3 = list(...),
            ...
          ),
          sub_group2 = list(...),
          sub_group3 = list(...),
          ...
          
        ),
        stra_2 = list(...),
        stra_3 = list(...),
        ...
      ),
      sample_2 = list(...),
      sample_3 = list(...),
      ...
    ),
    zero_one = list(  
      
    )
  )
  
  # step2. Unlock the above frames in turn by a for() loop and do the following for each sub-item:
      # 1). Segmentation of data
      # 2). Use mice() multiple interpolation to fill in the missing values of the training set and test set, respectively.
      # 3). Split the training set into training set and validation set.
      # 4). Up-sample the training set (including strategies such as SMOTE and ADASYN)
  
  # step3. Returns the partitioned dataset
}




#Random Forest Approach to Cumulative Features-------------------------------------------------------------
Boruta_selectVar <- function(df_, rand_n=5, targ_var = 'eczema_byDoctor'){
  library(Boruta)
  #df_:Factor attribute data
  #rand_n:range of random numbers1:rand_n---- repeat the operation and summarise it
  formula_ <- as.formula(paste(targ_var, ' ~ .'))  
  
  selectVar_1 <- c()
  selectVar_2 <- c() 
  for (i in 1:rand_n){
    set.seed(i)
    feature.selection <- Boruta(formula_, data=df_, doTrace=1)
    table(feature.selection$finalDecision) %>% print()
    selectVar_1 <- c(selectVar_1, getSelectedAttributes(feature.selection))
    selectVar_2 <- c(selectVar_2, getSelectedAttributes(feature.selection, withTentative = TRUE))
    Sys.sleep(10)
  }
  return(list(
    confirmed_ = selectVar_1[!duplicated(selectVar_1)],
    together_ = selectVar_2[!duplicated(selectVar_2)]
  ))
}


#Summary of features obtained for each disease (fragmentation - ‘n_ fold k_’) -------------------------------------------------------
get_feature <- function(df_, targ_var = 'eczema_byDoctor', rand_n_=4, rfe_n_ = 15,
                        repeat_ = list(all_ = 3, rf_ = 3, lda_ = 4),  
                        number_ = list(all_ = 3, rf_ = 5, lda_ = 5),
                        cl_ = 2, cut_min_ = 40, cut_n_ = 2){
  #currently only two methods, rfe yet to be explored only ......
  #df_:source data for feature filtering
  #rand_n_:see Boruta_selectVar()
  #rfe_n_:number of features filtered by rfe() method (per sub-loop)
  #repeat_:number of cross-validation repetitions in rfeControl() (list representation)
  #number_:number of cross-validation folds in rfeControl() (list representation)
  #cl_:number of threads
  #cut_min_:Filtering threshold for features accumulated by multiple repetitions ($togeth_ de-weighted features > 40, $confi > *2/3, rfe() same as $confi_)
  #cut_: filtering criterion for features accumulated from multiple repetitions (number of repetitions >= cut_n_)
  
  library(doSNOW) 
  Boruta_confi_ <- c()
  Boruta_togth_ <- c()
  rfe_rf_ <- c()
  for (n_ in 1:repeat_[['all_']]){ 
    ind_ <- sample(number_[['all_']], nrow(df_), replace=TRUE,
                   prob=rep(1/number_[['all_']], number_[['all_']]))
    for (k_ in 1:number_[['all_']]){ 
      cat(pastev(c(rep('\n', 5), rep('-', 20), n_, '*', k_, rep('-', 30), ':'), sep_ = ''))
      df_split <- df_[ind_ == k_, ]
      
      Boruta_obj_ <- Boruta_selectVar(df_ = df_split,targ_var = targ_var, rand_n=rand_n_)  #100%CPU ~5min/1bit/1000
      Sys.sleep(20)
      
      ml.cl_ <- makeCluster(cl_, type='SOCK')
      registerDoSNOW(ml.cl_) 
      
      rfe.rfeContrl_ <- rfeControl(functions = rfFuncs, method = 'repeatedcv',
                                   repeats = repeat_[['rf_']], number = number_[['rf_']],
                                   verbose = TRUE) 
      
      formula_ <- as.formula(paste(targ_var, ' ~ .'))
      #   rfe.rf_obj_ <- rfe(formula_, data=df_split, #25%CPU ~2min
      #                      sizes = 2^(2:6), rfeControl = rfe.rfeContrl_)
      
      #     rfe.rf_features_ <- as.character(lapply(rfe.rf_obj_$optVariables[1:rfe_n_],
      #                                             FUN = function(x){substr(x, start = 1, stop = nchar(x)-1)}))
      rfe.rf_features_ <- 'None'
      stopCluster(ml.cl_)
      Boruta_confi_ <- c(Boruta_confi_, Boruta_obj_[['confirmed_']])
      Boruta_togth_ <- c(Boruta_togth_, Boruta_obj_[['together_']])
      rfe_rf_ <- c(rfe_rf_, rfe.rf_features_)
      
    }
  }
  orig_ret <- list(
    Boruta_confi = Boruta_confi_,
    Boruta_togth = Boruta_togth_,
    rfe_rf = rfe_rf_
  )
  detach("package:doSNOW", unload = TRUE)
  
  if (length(Boruta_confi_[!duplicated(Boruta_confi_)]) > 2*cut_min_/3){
    cat('Boruta_confi_ Screening Initiation---', cut_min_, '||', cut_n_, '------original data:')
    print(Boruta_confi_[!duplicated(Boruta_confi_)])
    Boruta_confi_ <- names(table(Boruta_confi_))[table(Boruta_confi_) >= cut_n_]
  }
  if (length(Boruta_togth_[!duplicated(Boruta_togth_)]) > cut_min_){
    cat('Boruta_togth_ Screening Initiation---', cut_min_, '||', cut_n_, '------original data:')
    print(Boruta_togth_[!duplicated(Boruta_togth_)])
    Boruta_togth_ <- names(table(Boruta_togth_))[table(Boruta_togth_) >= cut_n_]
  }  
  if (length(rfe_rf_[!duplicated(rfe_rf_)]) > 2*cut_min_/3) {
    cat('rfe_rf_ Screening Initiation---', cut_min_, '||', cut_n_, '------original data:')
    print(rfe_rf_[!duplicated(rfe_rf_)])
    rfe_rf_ <- names(table(rfe_rf_))[table(rfe_rf_) >= cut_n_]
  }
  return(
    list(orig = orig_ret,
         result = list(
           Boruta_confi = Boruta_confi_[!duplicated(Boruta_confi_)],
           Boruta_togth = Boruta_togth_[!duplicated(Boruta_togth_)],
           rfe_rf = rfe_rf_[!duplicated(rfe_rf_)]
         ))
  )
}



# Getting a cut-off value weighted score----------------------------------
get_WeightScore <- function(df_){
  # Document: get the scores under each truncation value
  # df: truncated point performance against validation set performance after training on a small training set, including ‘Accuracy’, ‘Kappa, etc. ...
  weightScore <- df_[['Accuracy']]*.1 + 
    ifelse(is.null(df_[['Pos.Pred.Value']]), df_[['Pos Pred Value']], df_[['Pos.Pred.Value']])*.3 + 
    ifelse(is.null(df_[['Neg.Pred.Value']]), df_[['Neg Pred Value']], df_[['Neg.Pred.Value']])*.1 + 
    df_[['F1']]*.15 + (df_[['Sensitivity']] + df_[['Specificity']] - 1)*.35 
  # reliability：60% (Accuracy + F1 value + YI) ; profit：40% (PPV + NPV)
  return(weightScore)
}




# Obtaining Indicators for Individual Models--------------------------------------------------------------------------
get_UnitModelPerf <- function(formula_, dat_, cut_v_, fitFuncs, predFuncs, optParam = NULL){
  # formula_: expression (response variable ~ predictor1 + predictor2...)
  # dat_: minimum data unit (e.g. data.mergeADA1[[‘nominal_’]][[‘sample_1’]][[‘age’]][[‘0’]]))
  # cut_v_.
  # fitFUncs: function used to train the model ---- considering that some models need to be trained with tuning parameter
  # predFuncs: prediction function (inputs trained prediction model, outputs response values given predictor variables)
  
  ########I will explain the general framework of the function, the following code is only used to assist in the understanding of the function, and can not be completed run#########
  
  # step1. training set is used for model training to find the optimal truncation point on the validation set 
      # 1). Obtain model discriminators (accuracy, sensitivity, specificity, F1 value, etc.) for different truncation values.
      # 2). Obtain the weightScore under different truncation values according to get_WeightScore function, and obtain the optimal truncation value accordingly.
  
  # step2. Integrate the training set and validation set as a training set to train and evaluate the model with optimal parameters.
      # 1). Take the optimal truncation points obtained in step1 and obtain the performance of each metric on the testing set.
      # 2). Obtain the AROC of the model on the testing set.
  
  # step3. Returns individual model evaluation results in the following format:
      list(
        auroc = list(value = list(train = 0.712, test = 0.650), plotData = df(row*2)),
        auprc = list(value = list(train = 0.712, test = 0.650), plotData = df(row*3)),
        Cutoff = list(validation = df, optPoint = 0.4, testPerf = df(1*cols))
      )
}



#Iterate to get all sample cut values-------------------------------------
get_AggregateModel <- function(data_iter_, stratify_df_, targ_var_ = 'event', cut_ = seq(.01, .99, length.out = 49), 
                               fitFuncs, predFuncs, paramFuncs = NULL, time_sleep_ = 2){
  #data_:obj_[['nominal_']] OR obj_[['zero_one_']]
  #stratify_df_: for stratification
  #cut_: division of probabilistic prediction results ----------- train the model only once, cut length(cut_) times
  # fitFUncs: functions used to train the model ---- taking into account that some models need to be tuned for training.
  # predFuncs: prediction function (inputs trained prediction model, outputs response values given predictor variables)
  # paramFuncs: parameterisation for caret to get the optimal parameter for each stratified NO.1 set of data ----- (no parameterisation process by default)
  
  ########I will explain the general framework of the function, the following code is only used to assist in the understanding of the function, and can not be completed run#########
  
  # step1. Extract the data frame built by get_mlDataDivide() function sequentially until we get the training set, validation set, test set, etc. required for individual model training.
      # 1). According to the paramFuncs function, randomly take 3 datasets to obtain the optimal parameters (median/mean) for the validation set, and use them as the optimal parameters for the model in the corresponding hierarchical subgroups.
  
  # step2. Obtain the evaluation results of individual models.
      # 1). Return the evaluation results for each sub-model according to the get_mlDataDivide function
  
  # step3. return the metrics for that model against data_iter_ (keeping the original framework)
}


# Drawing Unified Font Settings-------------------------------------------------------------------------------
theme_default <- theme(
  axis.text.x = element_text(family = 'Arial'
                             #, face = 'italic'
                             , colour = 'black'
                             , angle = 30
                             , vjust = 0.7
                             , size = rel(1.5)),
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
  panel.spacing.x = unit(.2, 'cm'), 
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


