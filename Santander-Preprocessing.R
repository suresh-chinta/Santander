library(xgboost)
library(Matrix)
 
 set.seed(1234)
 
 train <- read.csv("train.csv")
 test  <- read.csv("test.csv")
 
 ##### Removing IDs
 train.id <- train$ID
 train$ID <- NULL
 test.id <- test$ID
 test$ID <- NULL

 ##### Extracting TARGET
 train.y <- train$TARGET
 train$TARGET <- NULL
 trainy = train.y
 
 ##### 0 count per line
 count0 <- function(x) {
     return( sum(x == 0) )
 }
 train$n0 <- apply(train, 1, FUN=count0)
 test$n0 <- apply(test, 1, FUN=count0)
 
 ##### Removing constant features
 cat("\n## Removing the constants features.\n")

## Removing the constants features.
 for (f in names(train)) {
     if (length(unique(train[[f]])) == 1) {
         cat(f, "is constant in train. We delete it.\n")
         train[[f]] <- NULL
         test[[f]] <- NULL
     }
 }
 ##### Removing identical features
 features_pair <- combn(names(train), 2, simplify = F)
 toRemove <- c()
 for(pair in features_pair) {
     f1 <- pair[1]
     f2 <- pair[2]
     
     if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
         if (all(train[[f1]] == train[[f2]])) {
             cat(f1, "and", f2, "are equals.\n")
             toRemove <- c(toRemove, f2)
         }
     }
 }
 
 feature.names <- setdiff(names(train), toRemove)
 train <- train[, feature.names]
 test <- test[, feature.names]

 

 
 #Set flags
  #---------------------------------------------------------------------------------------------
 
 train$num_var13_b = 0
 train$num_var42_b = 0
 train$num_var13_0_b = 0
 train$num_var13_largo_b = 0
 train$num_var13_largo_0_b = 0
 
 train[train$num_var13 != 0, "num_var13_b"] = 1
 train[train$num_var42 != 0, "num_var42_b"] = 1
 train[train$num_var13_0 != 0, "num_var13_0_b"] = 1
 train[train$num_var13_largo != 0, "num_var13_largo_b"] = 1
 train[train$num_var13_largo_0 != 0, "num_var13_largo_0_b"] = 1
 
 test$num_var13_b = 0
 test$num_var42_b = 0
 test$num_var13_0_b = 0
 test$num_var13_largo_b = 0
 test$num_var13_largo_0_b = 0
 
 test[test$num_var13 != 0, "num_var13_b"] = 1
 test[test$num_var42 != 0, "num_var42_b"] = 1
 test[test$num_var13_0 != 0, "num_var13_0_b"] = 1
 test[test$num_var13_largo != 0, "num_var13_largo_b"] = 1
 test[test$num_var13_largo_0 != 0, "num_var13_largo_0_b"] = 1
 
 train$num_var1_0_b = 0
 train$num_var1_b = 0
 train$num_var8_0_b = 0
 train$num_var13_corto_0_b = 0
 train$num_var13_corto_b = 0
 train$num_var24_b = 0
 train$num_var33_b = 0
 train$num_var40_0_b = 0
 train$num_var44_0_b = 0
 
 train[train$num_var1_0 != 0, "num_var1_0_b"] = 1
 train[train$num_var1 != 0, "num_var1_b"] = 1
 train[train$num_var8_0 != 0, "num_var8_0_b"] = 1
 train[train$num_var13_corto_0 != 0, "num_var13_corto_0_b"] = 1
 train[train$num_var13_corto != 0, "num_var13_corto_b"] = 1
 train[train$num_var24 != 0, "num_var24_b"] = 1
 train[train$num_var33 != 0, "num_var33_b"] = 1
 train[train$num_var40_0 != 0, "num_var40_0_b"] = 1
 train[train$num_var44_0 != 0, "num_var44_0_b"] = 1
 
 test$num_var1_0_b = 0
 test$num_var1_b = 0
 test$num_var8_0_b = 0
 test$num_var13_corto_0_b = 0
 test$num_var13_corto_b = 0
 test$num_var24_b = 0
 test$num_var33_b = 0
 test$num_var40_0_b = 0
 test$num_var44_0_b = 0
 
 test[test$num_var1_0 != 0, "num_var1_0_b"] = 1
 test[test$num_var1 != 0, "num_var1_b"] = 1
 test[test$num_var8_0 != 0, "num_var8_0_b"] = 1
 test[test$num_var13_corto_0 != 0, "num_var13_corto_0_b"] = 1
 test[test$num_var13_corto != 0, "num_var13_corto_b"] = 1
 test[test$num_var24 != 0, "num_var24_b"] = 1
 test[test$num_var33 != 0, "num_var33_b"] = 1
 test[test$num_var40_0 != 0, "num_var40_0_b"] = 1
 test[test$num_var44_0 != 0, "num_var44_0_b"] = 1

 train$num_var4_b = 0
 train$num_var4_b[train$num_var4 == 0] = 1

 test$num_var4_b = 0
 test$num_var4_b[test$num_var4 == 0] = 1


 #-------------------------------------------------------------------------------------------
 
 train$num_var13 = as.factor(train$num_var13)
 train$num_var13_0 = as.factor(train$num_var13_0)
 train$num_var42 = as.factor(train$num_var42)
 train$num_var13_largo = as.factor(train$num_var13_largo)
 train$num_var13_largo_0 = as.factor(train$num_var13_largo_0)
 train$num_var4 = as.factor(train$num_var4)
 
 train$num_var1_0 = as.factor(train$num_var1_0)
 train$num_var1 = as.factor(train$num_var1)
 train$num_var8_0 = as.factor(train$num_var8_0)
 train$num_var13_corto_0 = as.factor(train$num_var13_corto_0)
 train$num_var13_corto = as.factor(train$num_var13_corto)
 train$num_var24 = as.factor(train$num_var24)
 train$num_var33 = as.factor(train$num_var33)
 train$num_var40_0 = as.factor(train$num_var40_0)
 train$num_var44_0 = as.factor(train$num_var44_0)
 
 test$num_var1_0 = as.factor(test$num_var1_0)
 test$num_var1 = as.factor(test$num_var1)
 test$num_var8_0 = as.factor(test$num_var8_0)
 test$num_var13_corto_0 = as.factor(test$num_var13_corto_0)
 test$num_var13_corto = as.factor(test$num_var13_corto)
 test$num_var24 = as.factor(test$num_var24)
 test$num_var33 = as.factor(test$num_var33)
 test$num_var40_0 = as.factor(test$num_var40_0)
 test$num_var44_0 = as.factor(test$num_var44_0)
 
 test$num_var13 = as.factor(test$num_var13)
 test$num_var13_0 = as.factor(test$num_var13_0)
 test$num_var13_largo = as.factor(test$num_var13_largo)
 test$num_var13_largo_0 = as.factor(test$num_var13_largo_0)
 test$num_var4 = as.factor(test$num_var4)
 test$num_var42 = as.factor(test$num_var42)
 
 #---------------------
 
 levels(train$num_var13) =  levels(test$num_var13)
 levels(train$num_var13_0) =  levels(test$num_var13_0)
 levels(train$num_var4) =  levels(test$num_var4)
 levels(test$num_var42) =  levels(train$num_var42)
 
 levels(train$num_var13_largo_0) = c("0",  "3",  "6",  "9",  "12", "15","18", "21")
 levels(test$num_var13_largo_0) = c("0",  "3",  "6",  "9",  "12", "15","18", "21")
 
 levels(train$num_var13_largo) = c("0",  "3",  "6",  "9",  "12", "15","18", "21")
 levels(test$num_var13_largo) = c("0",  "3",  "6",  "9",  "12", "15","18", "21")
 
 
 #--------------------------------------------------------------------------------------------------------------------------
 
 ohe_feats = c('num_var13','num_var13_0','num_var42','num_var13_largo','num_var13_largo_0','num_var4', 'num_var44_0', 'num_var40_0', 'num_var33', 'num_var24', 
 'num_var13_corto', 'num_var13_corto_0', 'num_var8_0', 'num_var1', 'num_var1_0')
 
 dummies <- dummyVars(~ num_var13+num_var13_0+num_var42+num_var13_largo+num_var13_largo_0+num_var4+num_var44_0+num_var40_0+num_var33+num_var24+num_var13_corto +num_var13_corto_0+num_var8_0+num_var1+num_var1_0, data = train)

 train_ohe <- as.data.frame(predict(dummies, newdata = train))
 train_combined <- cbind(train[,-c(which(colnames(train) %in% ohe_feats))],train_ohe)
 
 test_ohe <- as.data.frame(predict(dummies, newdata = test))
 test_combined <- cbind(test[,-c(which(colnames(test) %in% ohe_feats))],test_ohe)
 train = train_combined
 test = test_combined
 
 train$var38log = log(1+train$var38)
 test$var38log = log(1+test$var38)
 
 
 train$var38log[train$var38 == 117310.979016494] = 0
 test$var38log[test$var38 == 117310.979016494] = 0
 
 train$var38 = NULL
 test$var38 = NULL
 
 train$delta_imp_reemb_var33_1y3 = NULL
 test$delta_imp_reemb_var33_1y3 = NULL
 
 train$delta_imp_trasp_var33_out_1y3 = NULL
 test$delta_imp_trasp_var33_out_1y3 = NULL
 
 train$delta_num_aport_var33_1y3[train$delta_num_aport_var33_1y3!=0] = 9999999999
 test$delta_num_aport_var33_1y3[test$delta_num_aport_var33_1y3!=0] = 9999999999
 
 train$delta_num_compra_var44_1y3[train$delta_num_compra_var44_1y3!=0] = 9999999999
 test$delta_num_compra_var44_1y3[test$delta_num_compra_var44_1y3!=0] = 9999999999
 
 train$delta_num_venta_var44_1y3[train$delta_num_venta_var44_1y3!=0] = 9999999999
 test$delta_num_venta_var44_1y3[test$delta_num_venta_var44_1y3!=0] = 9999999999
 
 train$delta_imp_amort_var18_1y3 = 0
 test$delta_imp_amort_var18_1y3 = 0
 
 train$delta_imp_amort_var34_1y3 = NULL
 test$delta_imp_amort_var34_1y3 = NULL
 
 #train$delta_imp_aport_var13_1y3
       #delta_imp_aport_var33_1y3
 
 train$delta_imp_aport_var13_1y3[train$delta_imp_aport_var13_1y3!=0] = 9999999999
 test$delta_imp_aport_var13_1y3[test$delta_imp_aport_var13_1y3!=0] = 9999999999
 
 train$delta_imp_aport_var33_1y3[train$delta_imp_aport_var33_1y3!=0] = 9999999999
 test$delta_imp_aport_var33_1y3[test$delta_imp_aport_var33_1y3!=0] = 9999999999
 
 #delta_imp_compra_var44_1y3
 
 train$delta_imp_compra_var44_1y3[train$delta_imp_compra_var44_1y3!=0] = 9999999999
 test$delta_imp_compra_var44_1y3[test$delta_imp_compra_var44_1y3!=0] = 9999999999

 train$num_op_var39_ult1_years  = trunc(train$num_op_var39_ult1/12) + 1
 train$num_op_var39_ult3_years  = trunc(train$num_op_var39_ult3/12) + 1
 train$num_op_var40_ult1_years  = trunc(train$num_op_var40_ult1/12) + 1
 train$num_op_var40_ult3_years  = trunc(train$num_op_var40_ult3/12) + 1
 train$num_op_var41_ult3_years  = trunc(train$num_op_var41_ult3/12) + 1
 train$num_op_var41_ult1_years  = trunc(train$num_op_var41_ult1/12) + 1
 train$num_op_var40_hace3_years = trunc(train$num_op_var40_hace3/12) + 1
 train$num_op_var40_hace2_years = trunc(train$num_op_var40_hace2/12) + 1
 train$num_var22_hace2_years    = trunc(train$num_var22_hace2/12) + 1
 train$num_var22_hace3_years    = trunc(train$num_var22_hace3/12) + 1
 train$num_var37_med_ult2_years = trunc(train$num_var37_med_ult2/12) + 1
 
  test$num_op_var39_ult1_years   = trunc(test$num_op_var39_ult1/12) + 1
  test$num_op_var39_ult3_years   = trunc(test$num_op_var39_ult3/12) + 1
  test$num_op_var40_ult1_years   = trunc(test$num_op_var40_ult1/12) + 1
  test$num_op_var40_ult3_years   = trunc(test$num_op_var40_ult3/12) + 1
  test$num_op_var41_ult3_years   = trunc(test$num_op_var41_ult3/12) + 1
  test$num_op_var41_ult1_years   = trunc(test$num_op_var41_ult1/12) + 1
  test$num_op_var40_hace3_years  = trunc(test$num_op_var40_hace3/12) + 1
  test$num_op_var40_hace2_years  = trunc(test$num_op_var40_hace2/12) + 1
  test$num_var22_hace2_years     = trunc(test$num_var22_hace2/12) + 1
  test$num_var22_hace3_years     = trunc(test$num_var22_hace3/12) + 1
  test$num_var37_med_ult2_years = trunc(test$num_var37_med_ult2/12) + 1


#NEWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW_22ND_WWWWWWWWWWWWWWWWW
#NEWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW_22ND_WWWWWWWWWWWWWWWWW
  train$num_op_var39_ult1_Q  = trunc(train$num_op_var39_ult1/3)
 train$num_op_var39_ult3_Q  = trunc(train$num_op_var39_ult3/3)
 train$num_op_var40_ult1_Q  = trunc(train$num_op_var40_ult1/3)
 train$num_op_var40_ult3_Q  = trunc(train$num_op_var40_ult3/3)
 train$num_op_var41_ult3_Q  = trunc(train$num_op_var41_ult3/3)
 train$num_op_var41_ult1_Q  = trunc(train$num_op_var41_ult1/3)
 train$num_op_var40_hace3_Q = trunc(train$num_op_var40_hace3/3)
 train$num_op_var40_hace2_Q = trunc(train$num_op_var40_hace2/3)
 train$num_var22_hace2_Q    = trunc(train$num_var22_hace2/3)
 train$num_var22_hace3_Q    = trunc(train$num_var22_hace3/3)
 train$num_var37_med_ult2_Q = trunc(train$num_var37_med_ult2/3)
 
  test$num_op_var39_ult1_Q   = trunc(test$num_op_var39_ult1/3)
  test$num_op_var39_ult3_Q   = trunc(test$num_op_var39_ult3/3)
  test$num_op_var40_ult1_Q   = trunc(test$num_op_var40_ult1/3)
  test$num_op_var40_ult3_Q   = trunc(test$num_op_var40_ult3/3)
  test$num_op_var41_ult3_Q   = trunc(test$num_op_var41_ult3/3)
  test$num_op_var41_ult1_Q   = trunc(test$num_op_var41_ult1/3)
  test$num_op_var40_hace3_Q  = trunc(test$num_op_var40_hace3/3)
  test$num_op_var40_hace2_Q  = trunc(test$num_op_var40_hace2/3)
  test$num_var22_hace2_Q     = trunc(test$num_var22_hace2/3)
  test$num_var22_hace3_Q     = trunc(test$num_var22_hace3/3)
  test$num_var37_med_ult2_Q = trunc(test$num_var37_med_ult2/3)
#NEWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW_22ND_WWWWWWWWWWWWWWWWW

 
  #train9 = train 
  train$sal_zero = rowSums(train[,113:137] == 0) 
  train$ind_zero = rowSums(train[,20:67] == 0)  
  train$imp_zero = rowSums(train[,3:19] == 0)  
  train$num_zero = rowSums(train[,68:112] == 0)
  train$num2_zero = rowSums(train[,190:249] == 0)
  train$num2_meses = rowSums(train[,207:217] == 0)
  train$num2_op = rowSums(train[,218:229] == 0)
  train$sal2_zero = rowSums(train[,250:287] == 0)
  train$num_3 = rowSums(train[,68:112] == 3)
  train$num_6 = rowSums(train[,68:112] == 6)
  train$delta_zero = rowSums(train[,139:154] == 0)
  train$delta_neg = rowSums(train[,139:154] < 0)
  train$delta_pos = rowSums(train[,139:154] > 0)
  train$sum_sal = rowSums(train[,113:137]) 
  train$sum_sal2 = rowSums(train[,250:287])
  train$imp_sum = rowSums(train[,3:19])
  train$mean_sal = train$sum_sal/(ncol(train[,113:137])- train$sal_zero + 1)
  train$mean_sal2 = train$sum_sal2/(ncol(train[,250:287])- train$sal2_zero + 1)
  train$mean_imp = train$imp_sum/(ncol(train[,3:19])- train$imp_zero+1)
  train$imp2_zero = rowSums(train[,155:180] == 0)
  train$ind2_zero = rowSums(train[,181:188] == 0) 
  train$imp2_sum = rowSums(train[,155:180])
  train$mean_imp2 = train$imp2_sum/(ncol(train[,155:180])- train$imp2_zero+1)
  train$imp2_trasp_zero = rowSums(train[,172:178] == 0)
  train$imp2_trasp_sum = rowSums(train[,172:178])

  #train$imp_aprot_zero = rowSums(train[,157:162] == 0)
  #train$imp_aprot_sum  = rowSums(train[,157:162])
  #train$imp_remeeb_zero = rowSums(train[,167:170] == 0)
  #train$imp_remeeb_sum  = rowSums(train[,167:170])
  #train$num_aprot_zero = rowSums(train[,190:195] == 0)
  #train$num_aprot_sum  = rowSums(train[,190:195])

  #test9 = test
  test$sal_zero = rowSums(test[,113:137] == 0) 
  test$ind_zero = rowSums(test[,20:67] == 0)  
  test$imp_zero = rowSums(test[,3:19] == 0)  
  test$num_zero = rowSums(test[,68:112] == 0)
  test$num2_zero = rowSums(test[,190:249] == 0)
  test$num2_meses = rowSums(test[,207:217] == 0)
  test$num2_op = rowSums(test[,218:229] == 0)
  test$sal2_zero = rowSums(test[,250:287] == 0)
  test$num_3 = rowSums(test[,68:112] == 3)
  test$num_6 = rowSums(test[,68:112] == 6)
  test$delta_zero = rowSums(test[,139:154] == 0)
  test$delta_neg = rowSums(test[,139:154] < 0)
  test$delta_pos = rowSums(test[,139:154] > 0)
  test$sum_sal = rowSums(test[,113:137]) 
  test$sum_sal2 = rowSums(test[,250:287])
  test$imp_sum = rowSums(test[,3:19])
  test$mean_sal = test$sum_sal/(ncol(test[,113:137])- test$sal_zero + 1)
  test$mean_sal2 = test$sum_sal2/(ncol(test[,250:287])- test$sal2_zero + 1)
  test$mean_imp = test$imp_sum/(ncol(test[,3:19])- test$imp_zero+1)
  test$imp2_zero = rowSums(test[,155:180] == 0)
  test$ind2_zero = rowSums(test[,181:188] == 0) 
  test$imp2_sum = rowSums(test[,155:180])
  test$mean_imp2 = test$imp2_sum/(ncol(test[,155:180])- test$imp2_zero+1)
  test$imp2_trasp_zero = rowSums(test[,172:178] == 0)
  test$imp2_trasp_sum = rowSums(test[,172:178])

#  test$imp_aprot_zero = rowSums(test[,157:162] == 0)
#  test$imp_aprot_sum  = rowSums(test[,157:162])
#  test$imp_remeeb_zero = rowSums(test[,167:170] == 0)
#  test$imp_remeeb_sum  = rowSums(test[,167:170])
#  test$num_aprot_zero = rowSums(test[,190:195] == 0)
#  test$num_aprot_sum  = rowSums(test[,190:195])



  train$var3_2 = 0
  train[train$var3 == 2, "var3_2"] = 1

  test$var3_2 = 0
  test[test$var3 == 2, "var3_2"] = 1

  train$TARGET <- train.y
  trainx <- sparse.model.matrix(TARGET ~ ., data = train)
  dtrain <- xgb.DMatrix(data=trainx, label=train.y)
  watchlist <- list(train=dtrain)

