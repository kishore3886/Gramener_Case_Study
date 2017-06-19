install.packages("party")
library("party")

#-----Gramener Case Study-----------
#load the data
loan_Data<- read.csv("loan.csv")
#summary of the data
summary(loan_Data)

#---- cleaning the data------------------
#--remove all columns which has NA's-----
#to see all column names
names(loan_Data)
#from the summary of the data we can remove few colmns from data

drops <- c("total_il_high_credit_limit","total_bc_limit","total_bal_ex_mort","tot_hi_cred_lim","tax_liens")
loan_Data<-loan_Data[ , !(names(loan_Data) %in% drops)]
#dropped 6 columns from the data , now there are 105 columns
drops<- c("percent_bc_gt_75","pct_tl_nvr_dlq","num_tl_op_past_12m","num_tl_90g_dpd_24m","num_tl_30dpd",
          "num_tl_120dpd_2m","num_sats","num_rev_tl_bal_gt_0","num_rev_accts","num_op_rev_tl","num_il_tl",
          "num_bc_tl","num_bc_sats","num_actv_rev_tl","num_actv_bc_tl","num_accts_ever_120_pd",
          "mths_since_recent_revol_delinq","mths_since_recent_inq","mths_since_recent_bc_dlq","mths_since_recent_bc",
          "mort_acc","mo_sin_rcnt_tl","mo_sin_rcnt_rev_tl_op","mo_sin_old_rev_tl_op","mo_sin_old_il_acct","delinq_amnt",
          "chargeoff_within_12_mths","bc_util","bc_open_to_buy","avg_cur_bal","acc_open_past_24mths","inq_last_12m")
loan_Data<-loan_Data[ , !(names(loan_Data) %in% drops)]
# dropped few more, now there are 74 cariables
#policy code all 1's so drop
drops<-c("total_cu_tl","inq_fi","total_rev_hi_lim","all_util","max_bal_bc","open_rv_24m","il_util","total_bal_il"
         ,"mths_since_rcnt_il","open_il_24m","open_il_12m","open_il_6m","open_acc_6m","tot_cur_bal","tot_coll_amt"
         ,"acc_now_delinq","verification_status_joint","dti_joint","annual_inc_joint","mths_since_last_major_derog"
         ,"policy_code","collections_12_mths_ex_med")

loan_Data<-loan_Data[ , !(names(loan_Data) %in% drops)]
#all application types are individual, hence we don't need it & initial status all are "f" hence remove it too
drops<-c("open_rv_12m","application_type","initial_list_status","pymnt_plan")
loan_Data<-loan_Data[ , !(names(loan_Data) %in% drops)]
#finally 48 columns left
write.csv(loan_Data,file="cleaned_Data.csv")

#---------treating missing values--------------------
#replace blanks in emp_title with Other
loan_Data$emp_title<-sub("^$","Other",loan_Data$emp_title)
