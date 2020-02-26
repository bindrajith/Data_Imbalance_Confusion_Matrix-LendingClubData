df_loans_100k<-read.csv("/Users/daviddelacruz/Desktop/loans_100k.csv", sep= ",")
##df.rejected<-read.csv("/Users/daviddelacruz/Desktop/rejected_100k.csv", sep= ",")
##df.loans<-read.csv("/Users/daviddelacruz/Desktop/loans.csv", sep= ",")


library(tidyverse)
library(plotly) 
library(caret)
library(corrplot)
library(imputeTS)
library(ade4)
library(data.table)
library(CORElearn)
library(ggcorrplot)
library(ROCR)
library(mRMRe)

str(df_loans_100k)

#Rename some values on target variable
df_loans_100k$loan_status[df_loans_100k$loan_status == "Does not meet the credit policy. Status:Fully Paid"] <- "Fully Paid"
df_loans_100k$loan_status[df_loans_100k$loan_status == "Does not meet the credit policy. Status:Charged Off"] <- "Charged Off"


#Subset target varible
df_loans_100k.2<- subset(df_loans_100k, loan_status == "Fully Paid" | loan_status == "Charged Off" | loan_status == "Default" 
                         | (loan_status == "Current" & out_prncp < 1))

df_loans_100k.2$loan_status[df_loans_100k.2$loan_status == "Current"] <- "Fully Paid"
df_loans_100k.2$loan_status[df_loans_100k.2$loan_status == "Default"] <- "Charged Off"



#Selecting features before loan application. Also dropping addr_state, earliest_cr_line, zip_code
df.loans<- select(df_loans_100k.2,acc_now_delinq, acc_open_past_24mths,all_util,annual_inc,annual_inc_joint,
                  application_type,avg_cur_bal,bc_open_to_buy,chargeoff_within_12_mths,delinq_2yrs,
                  delinq_amnt,dti,dti_joint,emp_length,grade,home_ownership,installment,
                  int_rate,loan_amnt,loan_status,mort_acc,mths_since_last_delinq,mths_since_last_major_derog,
                  mths_since_last_record,mths_since_rcnt_il,
                  mths_since_recent_bc,mths_since_recent_bc_dlq,mths_since_recent_inq,mths_since_recent_revol_delinq,
                  num_accts_ever_120_pd,num_actv_bc_tl,num_actv_rev_tl,num_bc_sats,num_bc_tl,num_il_tl,num_op_rev_tl,
                  num_rev_accts,num_rev_tl_bal_gt_0,num_sats,num_tl_120dpd_2m,num_tl_30dpd,num_tl_90g_dpd_24m,
                  num_tl_op_past_12m,open_acc,open_acc_6m,open_il_12m,open_il_24m,open_act_il,open_rv_12m,open_rv_24m,
                  pct_tl_nvr_dlq,percent_bc_gt_75,pub_rec,pub_rec_bankruptcies,purpose,sub_grade,tax_liens,term,
                  tot_cur_bal,tot_hi_cred_lim,total_acc,total_bal_ex_mort,total_bal_il,total_bc_limit,
                  total_cu_tl,total_il_high_credit_limit,total_rev_hi_lim,verification_status,
                  revol_bal_joint,sec_app_earliest_cr_line,sec_app_inq_last_6mths,sec_app_mort_acc,sec_app_open_acc,
                  sec_app_revol_util,sec_app_open_act_il,sec_app_num_rev_accts,sec_app_chargeoff_within_12_mths,
                  sec_app_collections_12_mths_ex_med,sec_app_mths_since_last_major_derog)
                         
table(df.loans$loan_status)


df.loans$annual_inc_joint[is.na(df.loans$annual_inc_joint)] <- 0
df.loans$dti_joint[is.na(df.loans$dti_joint)] <- 0


df.loans <- mutate(df.loans, annual_inc = ifelse(annual_inc_joint== 0 , annual_inc, annual_inc_joint))
df.loans <- mutate(df.loans, dti = ifelse(dti_joint== 0 , dti, dti_joint))

df.loans <- select(df.loans, -(annual_inc_joint))
df.loans <- select(df.loans, -(dti_joint))



#Add NA values on missing datapoint
df.loans[df.loans == ""] <- NA

# Select columns with variance 0
equal_v <- apply(df.loans, 2, function(x) max(na.omit(x)) == min(na.omit(x)))
index_vr2 <- which(equal_v == T)

# Select columns with more than 20% of missing values
col_NA <- apply(df.loans, 2, function(x) sum(is.na(x))/nrow(df.loans))
row_NA <- apply(df.loans, 1, function(x) sum(is.na(x))/ncol(df.loans))


# Plot missing values in % per features
plot_ly(x = seq(1,ncol(df.loans)), y = col_NA, type = "scatter", mode = "markers", color = "red") %>%
layout(title = "Before deleting features with missing values",
         xaxis = list(title = "Features Index"),
         yaxis = list(title = "Percentage(%)"))

plot_ly(x = seq(1,nrow(df.loans)), y = row_NA, type = "scatter", mode = "markers") %>%
  layout(title = "Observation Missing Values Percentage",
         xaxis = list(title = "Observation Index"),
         yaxis = list(title = "Percentage(%)"))

# Deleting columns with more than 0.4 of missing values
index_mr <- which(col_NA > 0.4)

ggplot(df.loans)+
  geom_bar(mapping = aes(x = loan_status))

table(df.loans$loan_status)

# Dropping columns
df <- df.loans[,-unique(c(index_vr2, index_mr))]

col_NA <- apply(df, 2, function(x) sum(is.na(x))/nrow(df))

# Plot missing values in % per features
plot_ly(x = seq(1,ncol(df)), y = col_NA, type = "scatter", mode = "markers", color = "red") %>%
  layout(title = "After deleting features with missing values",
         xaxis = list(title = "Features Index"),
         yaxis = list(title = "Percentage(%)"))

#No duplicates rows on the df

#Missing values patrons: Feature Analysis 
df.missing <- df[is.na(df$num_accts_ever_120_pd),]

ggplot(df)+
  geom_bar(mapping = aes(x = loan_status))

ggplot(df.missing)+
  geom_bar(mapping = aes(x = loan_status))


df.missing.subset <- subset(df, (!is.na(df[,5]))& (!is.na(df[,21])))

sum(is.na(df.missing.subset))
sum(is.na(df))


#Visualize subset % of missing value per features
col_NA <- apply(df.missing.subset, 2, function(x) sum(is.na(x))/nrow(df.missing.subset))
# Plot missing values in % per features
plot_ly(x = seq(1,ncol(df.missing.subset)), y = col_NA, type = "scatter", mode = "markers", color = "red") %>%
  layout(title = "features with missing values df.missing.subset",
         xaxis = list(title = "Features Index"),
         yaxis = list(title = "Percentage(%)"))

#Visualize subset % of missing value per observation
row_NA <- apply(df.missing.subset, 1, function(x) sum(is.na(x))/ncol(df.missing.subset))
plot_ly(x = seq(1,nrow(df.missing.subset)), y = row_NA, type = "scatter", mode = "markers") %>%
  layout(title = "Observation Missing Values Percentage df.missing.subset",
         xaxis = list(title = "Observation Index"),
         yaxis = list(title = "Percentage(%)"))

df <- df.missing.subset

ggplot(df)+
  geom_bar(mapping = aes(x = loan_status))

table(df$loan_status)

#TRANSFORMING VARIABLES

#Transform to numertic the interest rate
df$int_rate <- as.numeric(gsub('[%]','',df$int_rate))
class(df$int_rate)

# Transforming term
df<-mutate(df, term= as.numeric(str_sub(df$term, end= 3)) )


# Correlation Matrix

#subset Correlation
#df.corr <- select(df, annual_inc, avg_cur_bal, bc_open_to_buy, dti, installment, int_rate, loan_amnt, tot_cur_bal,
#                  tot_hi_cred_lim, total_bal_ex_mort, total_bc_limit, total_rev_hi_lim)

#df.corr <- as.data.frame(apply(df.corr, 2, na.mean))

#res <- cor(df.corr)
#corrplot(res, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45, na.rm=t)


winsorize <-function (x, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x)
  y <- x - med
  sc <- mad(y, center=0) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}

df.outliers <- select(df, annual_inc,dti, installment, 
                      int_rate, loan_amnt, open_acc, total_acc)


df.winsorize <- as.data.frame(apply(df.outliers, 2, winsorize))


#Select features that are not continuous variables
df.no.outliers <-select(df,application_type, emp_length, grade, 
                        home_ownership, purpose, 
                        sub_grade,term, verification_status, acc_now_delinq, acc_open_past_24mths, delinq_amnt,
                        avg_cur_bal, bc_open_to_buy, chargeoff_within_12_mths, delinq_2yrs,
                        mort_acc, mths_since_recent_bc, mths_since_recent_inq, num_accts_ever_120_pd,
                        num_actv_bc_tl, num_actv_rev_tl,num_bc_sats, 
                        num_bc_tl, num_il_tl, num_op_rev_tl, num_rev_accts, num_rev_tl_bal_gt_0, num_sats, 
                        num_tl_120dpd_2m, num_tl_30dpd, num_tl_90g_dpd_24m, num_tl_op_past_12m,
                        pct_tl_nvr_dlq, percent_bc_gt_75, pub_rec_bankruptcies, tax_liens, 
                        tot_cur_bal, tot_hi_cred_lim, total_bal_ex_mort,
                        total_bc_limit, total_il_high_credit_limit, total_rev_hi_lim, pub_rec, pub_rec,loan_status)
                          

df.2<-cbind(df.winsorize,df.no.outliers)


#Recoding Categorical Variables

df.2$grade <- recode_factor(df.2$grade, A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7  )
df.2$grade <- as.numeric(df.2$grade)

df.2$application_type <- recode_factor(df.2$application_type, "Individual" = 1, "Joint App" = 2)
df.2$application_type <- as.numeric(df.2$application_type)

table(df.2$loan_status)


df.2$loan_status <- recode_factor(df.2$loan_status, "Fully Paid" = 0, "Charged Off" = 1)
df.2$loan_status <- as.numeric(df.2$loan_status)
df.2$loan_status[df.2$loan_status== 1] <-0
df.2$loan_status[df.2$loan_status== 2] <-1

table(df.2$loan_status)

df.2$emp_length <- recode_factor(df.2$emp_length, "1 year" = 1, "0+ years" = 2, "2 years" = 3,
                                  "3 years" = 4, "4 years" = 5, "5 years" = 6, "7 years" = 7,
                                  "8 years" = 8, "9 years" = 9, "< 1 year" = 10)
df.2$emp_length <- as.numeric(df.2$emp_length)

df.2$purpose <- recode_factor(df.2$purpose, "car" = 1, "credit_card" = 2, "debt_consolidation" = 3,
                            "educational" = 4, "home_improvement" = 5, "house" = 6, "major_purchase" = 7,
                            "medical" = 8, "moving" = 9, "other" = 10, "renewable_energy" = 11,
                            "small_business" = 12, "vacation" = 13, "wedding" = 14)
df.2$purpose <- as.numeric(df.2$purpose)

df.2$home_ownership <- recode_factor(df.2$home_ownership, "ANY" = 1, "MORTGAGE" = 2, "NONE" = 3,
                                     "OTHER" = 4, "OWN" = 5, "RENT" = 6)
df.2$home_ownership <- as.numeric(df.2$home_ownership)

df.2$verification_status <- recode_factor(df.2$verification_status, "Not Verified Source" = 1,
                                          "Source Verified" = 2, "Verified" = 3)
df.2$verification_status <- as.numeric(df.2$verification_status)

df.2$term[df.2$term== 36] <-1
df.2$term[df.2$term== 60] <-2


df.2$sub_grade <- recode_factor(df.2$sub_grade, "A1" = 1, "A2" = 2, "A3" = 3,
                                "A4" = 4, "A5" = 5, "B1" = 6, "B2" = 7,
                                "B3" = 8, "B4" = 9, "B5" = 10, "C1" = 11,
                               "C2" = 12, "C3" = 13, "C4" = 14, "C5" = 15, "D1" = 16, "D2" = 17,
                                "A4" = 18, "A5" = 19, "B1" = 20, "B2" = 21,
                                "D3" = 22, "D4" = 23, "D5" = 24, "E1" = 25,
                                "E2" = 26, "E3" = 27, "E4" = 28,"E5" = 29, "F1" = 30, "F2" = 31,
                                "A4" = 32, "A5" = 33, "B1" = 34, "B2" = 35,
                                "F3" = 36, "F4" = 37, "F5" = 38, "G1" = 39,
                                "G2" = 40, "G3" = 41, "G4" = 42, "G5" = 42)

df.2$sub_grade <- as.numeric(df.2$sub_grade)


str(df.2)


#Download the data as a  CSV File
#write.csv(df.2, file = "loan_cleaned.csv")

df_all <- df.2



#One-hot-encoding features
ohe_feats = c('term', 'application_type')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(df_all[f])
  df_all[f] = NULL
  df_all = cbind(df_all, df_all_dummy)
}

ohe_feats = c('verification_status', 'emp_length')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(df_all[f])
  df_all[f] = NULL
  df_all = cbind(df_all, df_all_dummy)
}

ohe_feats = c('purpose', 'home_ownership')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(df_all[f])
  df_all[f] = NULL
  df_all = cbind(df_all, df_all_dummy)
}

#Splitting into training and testing
trainIndex <- createDataPartition(df.2$loan_status, p = .6, 
                                  list = FALSE, 
                                  times = 1)

df.train <- df.2[ trainIndex,]
df.test  <- df.2[-trainIndex,]

train_hist <-ggplot(data= df.train)+
  geom_bar(mapping = aes(x = loan_status))

train_hist + ggtitle("Loan Status Train")

test_hist <-   ggplot(data= df.test)+
  geom_bar(mapping = aes(x = loan_status))
  
test_hist + ggtitle("Loan Status Test")


table(df.train$loan_status)

str(df.train)

baseline.model<- subset(df.test, annual_inc >=30000 &  (grade ==1| grade ==2| grade ==3))

table(baseline.model$loan_status)

baseline.model.2<- subset(df.test, annual_inc <30000 & grade != (grade ==1|grade ==2|grade ==3))
table(baseline.model.2$loan_status)

# Impute using the mean of the column
df.train <- as.data.frame(apply(df.train, 2, na.mean))


#FEATURE SELECTION: FILTERS

# run method ReliefF with exponential rank distance


estReliefF <- attrEval(loan_status ~ ., df.train,
                       estimator="ReliefFexpRank", ReliefIterations=30)
print(estReliefF)

plot(estReliefF)

relieff<-as.data.frame(estReliefF)
index_relieff <- rownames(relieff)[which(relieff[,1] > 0.04)][-1]

index_relieff

#GainRatioCost

gainratiocost <- attrEval(loan_status ~ ., df.train,
                          estimator="GainRatioCost", ReliefIterations=30)

print(gainratiocost)

plot(gainratiocost)

gaincost<-as.data.frame(gainratiocost)
index_gaincost <- rownames(gaincost)[which(gaincost[,1] > 0.04)][-1]

index_gaincost

df.train.gaincost<- df.train[,c("loan_status",index_gaincost)]

df.train.relieff<- df.train[,c("loan_status",index_relieff)]


#mRMRe
f_data <- mRMR.data(data = data.frame(df.train))
featureData<-featureData(f_data)
mRMR<-mRMR.classic("mRMRe.Filter",data = f_data, target_indices = 5, 
              feature_count = 12)

solutions(mRMR)

mRMR_features<-as.vector(solutions(mRMR))
#The output of the code gives the indices of the selected 10 features.

df.train.mrmr <- df.train[,c(3,14,1,47,12,46,51)]

#12,
#,46
#23,15,8,45

#CORRELATION ANALYSIS

#Corr for gain cost
corr.gain<-cor(df.train.gaincost)
ggcorrplot(corr.gain)

#Corr for relieff
corr.relieff<-cor(df.train.relieff)
ggcorrplot(corr.relieff)

#Corr for mrmr
corr.mrmr<-cor(df.train.mrmr)
ggcorrplot(corr.mrmr)


df.train.lr <- df.train.relieff


#Logistic Regression
model_glm <- glm(loan_status ~ ., data = df.train.lr, family='binomial')

predicted_glm <- predict(model_glm, df.test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.3,1,0)

table(df.test$loan_status, predicted_glm)

wt <- ifelse(df.train.lr$loan_status == 1, 1/mean(df.train.lr$loan_status == 1),1)

model_glm_wt <- glm(loan_status ~ ., data = df.train.lr, 
                    weights = wt,family='quasibinomial')

predicted_glm_wt <- predict(model_glm_wt, df.test, type='response')
predicted_glm_wt <- ifelse(predicted_glm_wt > 0.5,1,0)

table(df.test$loan_status, predicted_glm_wt)
