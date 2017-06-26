install.packages("party")
install.packages("stringr")
install.packages("corrgram")
install.packages("doBy")
install.packages("agricolae")
install.packages("corrplot")


#-----Loading Packages------------------------------------------------------------------------

library ( agricolae)
library(corrgram)
library(plyr)
library("party")
library(ggplot2)
library(stringr)
library(dplyr)
library("doBy")
library("corrplot")

#-----Gramener Case Study---------------------------------------------------------------------

#load the data
loan_Data<- read.csv("loan.csv")
#summary of the data
summary(loan_Data)


#---- Cleaning the data-----------------------------------------------------------------------

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

# dropped few more, now there are 74 variables

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

#---------Treating Missing values--------------------------------------------------------------

#replace blanks in emp_title with "Other"

loan_Data$emp_title<-sub("^$","Other",loan_Data$emp_title)
summary(loan_Data$emp_length)
loan_Copy<-loan_Data

#to check if any dupicates exists in id, member_id columns

length(unique(loan_Data$member_id))
length(loan_Data$member_id)
length(unique(loan_Data$id))
length(loan_Data$id)

#no duplicates

# Adding a derived column for salary which will help us to draw conclusions easily

summary(loan_Copy$annual_inc) 
plot(loan_Copy$annual_inc)
boxplot(loan_Copy$annual_inc)

#these two plots says there are few out liers, for the sake of easy Data analysis we are removing the outliers

class(loan_Copy$annual_inc)
loan_Copy<- subset(loan_Copy,loan_Copy$annual_inc<1000000)
summary(loan_Copy$annual_inc) 
plot(loan_Copy$annual_inc)

#salary to salary slots

loan_Copy$annual_income_slot<- ifelse(loan_Copy$annual_inc<10000,10000,
                                      ifelse(loan_Copy$annual_inc<20000,20000,
                                             ifelse(loan_Copy$annual_inc<30000,30000,
                                                    ifelse(loan_Copy$annual_inc<40000,40000,
                                                           ifelse(loan_Copy$annual_inc<50000,50000,
                                                                  ifelse(loan_Copy$annual_inc<60000,60000,
                                                                         ifelse(loan_Copy$annual_inc<70000,70000,
                                                                                ifelse(loan_Copy$annual_inc<80000,80000,
                                                                                       ifelse(loan_Copy$annual_inc<90000,90000,
                                                                                              ifelse(loan_Copy$annual_inc<100000,100000,
                                                                                                     ifelse(loan_Copy$annual_inc<120000,120000,
                                                                                                            ifelse(loan_Copy$annual_inc<150000,150000,
                                                                                                                   ifelse(loan_Copy$annual_inc<175000,175000,200000)))))))))))))


 
write.csv(loan_Copy,file="cleaned_Data_final.csv")

#to check number od rows and columns of data after cleaning  nrow(loan_Data)

ncol(loan_Data)
nrow(loan_Data)

#convert int_rate to numeric for calculations purposes 

loan_Copy$int_rate<- str_replace_all(loan_Copy$int_rate,'%','')
loan_Copy$int_rate<-as.numeric(loan_Copy$int_rate)


#--------------------------------FILTER THE DATA AS PER LOAN STATUS(SEGMENTING DATA)---------------------------
#

loan_Copy_FullyPaid<- subset(loan_Copy,loan_Copy$loan_status=="Fully Paid")
loan_Copy_ChargedOff<-subset(loan_Copy,loan_Copy$loan_status=="Charged Off")
loan_Copy_Current<-subset(loan_Copy,loan_Copy$loan_status=="Current")

#to confirm no rows are missing

nrow(loan_Copy_ChargedOff)+nrow(loan_Copy_Current)+nrow(loan_Copy_FullyPaid)


#----------------------------------- ANALYSIS STARTS HERE--------------------------------------
#TERM VS INTEREST RATE to check if interest rate is affected by term 

aggregate(loan_Copy, by=list(loan_Copy$int_rate, loan_Copy$term), 
         FUN=mean, na.rm=TRUE)
class(loan_Copy$int_rate)

#Correlation Matrix of all variables.
corrgram(loan_Copy, order=TRUE, lower.panel=panel.shade,
        upper.panel=panel.pie, text.panel=panel.txt,
        main="All variables correlation Matrix")

ggplot(loan_Copy, aes(x = loan_amnt,y = annual_income_slot, col = funded_amnt)) + geom_point(position = "jitter")
ggplot(loan_Copy, aes(x = loan_amnt,y = annual_income_slot, col = funded_amnt, size = total_pymnt)) + geom_point( position = "jitter")
ggplot(loan_Copy, aes(x = funded_amnt,y = funded_amnt_inv, col = annual_income_slot, size = revol_bal)) + geom_point(position = "jitter")
ggplot(loan_Copy, aes(x = funded_amnt,y = funded_amnt_inv, col = annual_inc, size = revol_bal)) + geom_point(position = "jitter")

#based on the result collecting strongly correlated variables:
strong_correl_var <-c("loan_amnt","funded_amnt","funded_amnt_inv","term","emp_title","home_ownership","installment","annual_inc","verification_status","revol_bal","purpose","title","addr_state", "revol_util","total_acc","revol_bal","revol_util","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_late_fee","recoveries","last_pymnt_amnt")
analysis_data<-loan_Copy[ , (names(loan_Data) %in% strong_correl_var)]
#plotting  correlation matrix:
corrgram(analysis_data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Strongly co-related")

corrgram(analysis_data, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Correlation Metrics at view")

#annual income vs funded amount investment

ggplot(loan_Copy, aes(x = annual_income_slot,y = funded_amnt_inv, col = revol_bal)) + geom_smooth()
#this depicts as the annual income increases funded amount inv also increases

ggplot(loan_Copy, aes(x = loan_amnt,y = annual_inc, col = loan_status)) + geom_smooth()
ggplot(loan_Copy, aes(x = funded_amnt,y = annual_inc, col = loan_status)) + geom_smooth()
ggplot(loan_Copy, aes(x = loan_amnt,y = revol_bal, col = loan_status)) + geom_smooth()

#delinq_2yrs
ggplot(loan_Copy, aes(x = loan_amnt,y = delinq_2yrs, col = loan_status)) + geom_smooth()
ggplot(loan_Copy, aes(x = loan_amnt,y = annual_inc, col = loan_status)) + geom_point(position = "jitter")
ggplot(loan_Copy, aes(x = loan_amnt,y = revol_bal, col = loan_status)) + geom_point(position = "jitter")
ggplot(loan_Copy, aes(x = loan_amnt,y = delinq_2yrs, col = loan_status)) + geom_point(position = "jitter")

#analysing various spread of variables for further analysis deriving points :
#barcharts(categorical variables):

ggplot(loan_Copy, aes(x=factor(loan_Copy$term), fill=loan_Copy$funded_amnt)) +geom_bar()
#36 months is greater than double on 60 months for funded amount . this means company prefer short duration loans 
#that can be easily repaid:
ggplot(loan_Copy, aes(x=loan_Copy$term,y =loan_Copy$int_rate)) +geom_point(position = "jitter")
#checking dependency of home ownership on loan amount:
ggplot(loan_Copy, aes(x=loan_Copy$loan_amnt, fill=factor(loan_Copy$home_ownership))) +geom_bar(position = "stack")
#checking dependency of loan status and home ownership , this clearly states that fully paid have utilized all categories of ownership to pay off loans as compared to other two.
ggplot(loan_Copy, aes(x=loan_Copy$loan_status, fill=factor(loan_Copy$home_ownership))) +geom_bar(position = "stack")

#histogram:
ggplot(loan_Copy,aes(x=loan_Copy$funded_amnt, fill=factor(loan_Copy$loan_status)))+ geom_histogram(binwidth = 10000)
h<-hist(loan_Copy_ChargedOff$annual_income_slot,main = "Histogram for Income slots",xlab = "Annual income",right = FALSE)
hist(loan_Copy_Current$annual_income_slot,main = "Histogram for Income slots",xlab = "Annual income")
hist(loan_Copy_FullyPaid $annual_income_slot,main = "Histogram for Income slots",xlab = "Annual income")
g<-ggplot(loan_Copy_FullyPaid$annual_income_slot,aes(class))

#ANNUAL_INC
hist(loan_Copy_ChargedOff$annual_income_slot,main = "Histogram for Income slots charged Off",xlab = "Annual income",right = FALSE)
#annual income 30000 to 80000 are more number in terms of charge-Off
Annual_Inc_filter_Chargerd_off<- subset(loan_Copy_ChargedOff,loan_Copy_ChargedOff$annual_inc>30000 & loan_Copy_ChargedOff$annual_inc<80000)
nrow(Annual_Inc_filter_Chargerd_off)
# 63%

hist(loan_Copy_FullyPaid $annual_income_slot,main = "Histogram for Income slots-Fully Paid",xlab = "Annual income")
Annual_Inc_filter_fullypaid<- subset(loan_Copy_FullyPaid,loan_Copy_FullyPaid$annual_inc>20000 & loan_Copy_FullyPaid$annual_inc<90000)
nrow(Annual_Inc_filter_fullypaid)
# 75%

#as both are almost similar region we can't conclude anything on this variable alone
hist(loan_Copy_Current$annual_income_slot,main = "Histogram for Income slots",xlab = "Annual income")
boxplot(loan_Copy$annual_inc~loan_Copy$loan_status,xlab="loan status",ylab="annual income")
boxplot(loan_Copy$annual_income_slot~loan_Copy$loan_status,xlab="loan status",ylab="annual income")
#Fully paid customers annual income median is slightly higher than charged off candidates. 

#FUNDED AMOUNT
hist(loan_Copy_ChargedOff$funded_amnt,main = "Histogram for funded amounts",xlab = "Funded amount",right = FALSE)
#funded amount between 4000 to 16000 more charged off's exist
Funded_amt_filter_Chargerd_off<- subset(loan_Copy_ChargedOff,loan_Copy_ChargedOff$funded_amnt >4000 & loan_Copy_ChargedOff$funded_amnt<16000)
nrow(Funded_amt_filter_Chargerd_off)

hist(loan_Copy_FullyPaid $funded_amnt,main = "Histogram for funded amounts",xlab = "Funded amount",right = FALSE)
Funded_amt_filter_FullyPaid<- subset(loan_Copy_FullyPaid,loan_Copy_FullyPaid$funded_amnt >4000 & loan_Copy_FullyPaid$funded_amnt<16000)
nrow(Funded_amt_filter_FullyPaid)
#calculate % 21328
boxplot(loan_Copy$funded_amnt~loan_Copy$loan_status,xlab="loan status",ylab="Funded Amount")


#INTEREST RATE
hist(loan_Copy_ChargedOff$int_rate,main = "Histogram for INTEREST RATES",xlab = "Interest Rate",right = FALSE)
#Interest rate  between 11 to 18 more charged off's exist
Interest_Rate_filter_Chargerd_off<- subset(loan_Copy_ChargedOff,loan_Copy_ChargedOff$funded_amnt >4000 & loan_Copy_ChargedOff$funded_amnt<16000)
nrow(Funded_amt_filter_Chargerd_off)
#calculate % 
#Higher the interest rates higher the people chargeoff but we can't conclude this because we could see a mix.This could a catalyst.

hist(loan_Copy_FullyPaid$int_rate,main = "Histogram for INTEREST RATES",xlab = "Interest Rate",right = FALSE)
Interest_Rate_filter_FullyPaid<- subset(loan_Copy_FullyPaid,loan_Copy_FullyPaid$funded_amnt >4000 & loan_Copy_FullyPaid$funded_amnt<16000)
#Interest rate  between 10 to 14 more charged off's exist
#lower the interest rates people are tending to pay
nrow(Funded_amt_filter_FullyPaid)

#Higher the interest rates higher the people chargeoff but we can't conclude this because we could see a mix.This could a catalyst.

hist(loan_Copy_FullyPaid$int_rate,main = "Histogram for INTEREST RATES",xlab = "Interest Rate",right = FALSE)
Interest_Rate_filter_FullyPaid<- subset(loan_Copy_FullyPaid,loan_Copy_FullyPaid$funded_amnt >4000 & loan_Copy_FullyPaid$funded_amnt<16000)
#Interest rate  between 10 to 14 more charged off's exist
#lower the interest rates people are tending to pay
nrow(Funded_amt_filter_FullyPaid)

#INSTALLMENT 
hist(loan_Copy_ChargedOff$installment,main = "Histogram for INSTALLMENT",xlab = "Installment",right = FALSE)
hist(loan_Copy_FullyPaid$installment,main = "Histogram for INSTALLMENT",xlab = "Installment",right = FALSE)
#Installemtnt doesn't effect much. Both cases it is similar we can't decide anything in this


#Mean for categorical variables is meaning less so let's find mode
#a function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#emp title
getmode(loan_Copy_ChargedOff$emp_title )
getmode(loan_Copy_FullyPaid$emp_title)
#in both categories we are getting other as mode hence we can't decide anything on emp title

#verification status
getmode(loan_Copy_ChargedOff$verification_status )
getmode(loan_Copy_FullyPaid$verification_status)
#in both categories we are getting not vrified as mode hence we can't decide anything on verification status

#purpose
getmode(loan_Copy_ChargedOff$purpose )
getmode(loan_Copy_FullyPaid$purpose )
#purpose is leading to debt _consolidation in both cases
#address state
getmode(loan_Copy_ChargedOff$addr_state )
getmode(loan_Copy_FullyPaid$addr_state )
#CA in both cases,we can't conclude anything
#in this case study we can't take any decision using univariate analysis. This is has helped us to get some insights on data

#Function implemented to return mean , meadian and count
#It is returning each annual income slot along with count, mean and median  
summaryBy(loan_amnt~annual_income_slot, data=loan_Copy, FUN=function(x) 
  c(count=length(x), mean=mean(x), median=median(x)))
summaryBy(loan_amnt~annual_income_slot, data=loan_Copy_ChargedOff, FUN=function(x) 
  c(count=length(x), mean=mean(x), median=median(x)))
summaryBy(loan_amnt~annual_income_slot, data=loan_Copy_FullyPaid, FUN=function(x) 
  c(count=length(x), mean=mean(x), median=median(x)))

#----------------------------------------Further ANALYSIS-Correlation-----------------------------------------------
summary(loan_Copy_ChargedOff$annual_inc)
summary(loan_Copy_ChargedOff$annual_income_slot)

#creating seperate frame for loan_amt and income
loan_amt_vs_income <-c("loan_amnt","annual_inc")
loanamt_income<-loan_Copy[ , (names(loan_Data) %in% loan_amt_vs_income)]

#main correlation between the two
corrgram(loanamt_income, order=TRUE, lower.panel=panel.pie,
        upper.panel=panel.pts, text.panel=panel.txt,
        main="Annual income Vs Loan Amount")

correl(loan_Copy$loan_amnt,loan_Copy$annual_inc, method = "pearson" , alternative = "two.sided")
#$stat
#[1] 78.30893

#$rho
#[1] 0.3657807

#$pvalue
#[1] 0
#thus there is a positive correlation between loan amount and income near about 78.03%.

#checkin for correl between revol_bal vs revol_util to give us a scale of risky flambouyant spenders:
revol_bal_vs_revol_util <-c("revol_bal","revol_util")
revol_cal_util<-loan_Copy[ , (names(loan_Data) %in% revol_bal_vs_revol_util)]

#collecting correlations:
correl(loan_Copy$annual_inc,loan_Copy$total_pymnt, method = "pearson" , alternative = "two.sided")
correl(loan_Copy$total_rec_prncp,loan_Copy$total_rec_int, method = "pearson" , alternative = "two.sided")

#stat value turns out to be ~ 70%
#this is an indicator that people with high credit limit tends to spend lavishly, such are risky customers.
boxplot(revol_cal_util$revol_bal ~ revol_cal_util$revol_util)

#Analysing some of the scatterplots:
# annual income vs anual paid :
ggplot(loan_Copy, aes(x=loan_Copy$annual_inc,y=loan_Copy$total_pymnt, col=factor(loan_Copy$loan_status))) +geom_point(shape=2, size=4)
ggplot(loan_Copy, aes(x=loan_Copy$annual_inc,y=loan_Copy$total_pymnt, col=factor(loan_Copy$loan_status))) +geom_point(shape=2, size=4)
ggplot(loan_Copy, aes(x=loan_Copy$last_pymnt_d,y=loan_Copy$total_pymnt, col=factor(loan_Copy$loan_status))) +geom_point(shape=2, size=4)



#----------------------- ANALYSIS BASED ON LOAN STATUS--------------------------------------------------------
nrow(loan_Copy_ChargedOff)
nrow(loan_Copy_Current)
nrow(loan_Copy_FullyPaid)

#Generating correlation for charged-off:
corrgram(loan_Copy_ChargedOff, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Charged off spread over variables")

corrgram(loan_Copy_Current, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Current spread over variables")

corrgram(loan_Copy_FullyPaid, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Fully Paid spread over variables")

# analysis of charged off
ggplot(loan_Copy_ChargedOff, aes(x=annual_income_slot,y=loan_amnt,col = funded_amnt))+geom_smooth()

ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = annual_income_slot, col = funded_amnt)) + geom_point()
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = annual_income_slot, col = funded_amnt, size = total_pymnt)) + geom_point()
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = revol_bal , col = annual_inc)) + geom_point()
ggplot(loan_Copy_ChargedOff, aes(x = funded_amnt,y = funded_amnt_inv, col = annual_income_slot, size = revol_bal)) + geom_point()
ggplot(loan_Copy_ChargedOff, aes(x = funded_amnt,y = funded_amnt_inv, col = annual_inc, size = revol_bal)) + geom_point()

#annual income vs funded amount investment
#this depicts as the annual income increases funded amount inv also increases
ggplot(loan_Copy_ChargedOff, aes(x = annual_income_slot,y = funded_amnt_inv, col = revol_bal)) + geom_smooth()

summaryBy(loan_amnt~annual_inc, data=loan_Copy_ChargedOff, FUN=function(x) 
  c(count=length(x), mean=mean(x), median=median(x)))

ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = annual_inc, col = loan_status)) + geom_smooth()
ggplot(loan_Copy_ChargedOff, aes(x = funded_amnt,y = annual_inc, col = loan_status)) + geom_smooth()
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = revol_bal, col = loan_status)) + geom_smooth()
#delinq_2yrs
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = delinq_2yrs, col = loan_status)) + geom_smooth()
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = annual_inc)) + geom_point(position="jitter")
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = revol_bal)) + geom_point(position="jitter")
ggplot(loan_Copy_ChargedOff, aes(x = loan_amnt,y = delinq_2yrs)) + geom_point(position="jitter")

#Analysis if Complete
# analysis of charged off
ggplot(loan_Copy_ChargedOff, aes(x=annual_income_slot,y=loan_amnt,col = funded_amnt))+geom_smooth()

ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = annual_income_slot, col = funded_amnt)) + geom_point()
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = annual_income_slot, col = funded_amnt, size = total_pymnt)) + geom_point()
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = revol_bal , col = annual_inc)) + geom_point()
ggplot(loan_Copy_FullyPaid, aes(x = funded_amnt,y = funded_amnt_inv, col = annual_income_slot, size = revol_bal)) + geom_point()
ggplot(loan_Copy_FullyPaid, aes(x = funded_amnt,y = funded_amnt_inv, col = annual_inc, size = revol_bal)) + geom_point()

#annual income vs funded amount investment
#this depicts as the annual income increases funded amount inv also increases
ggplot(loan_Copy_FullyPaid, aes(x = annual_income_slot,y = funded_amnt_inv, col = revol_bal)) + geom_smooth()

summaryBy(loan_amnt~annual_inc, data=loan_Copy_FullyPaid, FUN=function(x) 
  c(count=length(x), mean=mean(x), median=median(x)))

ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = annual_inc, col = loan_status)) + geom_smooth()
ggplot(loan_Copy_FullyPaid, aes(x = funded_amnt,y = annual_inc, col = loan_status)) + geom_smooth()
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = revol_bal, col = loan_status)) + geom_smooth()

#delinq_2yrs
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = delinq_2yrs, col = loan_status)) + geom_smooth()
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = annual_inc)) + geom_smooth()
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = revol_bal)) + geom_smooth()
ggplot(loan_Copy_FullyPaid, aes(x = loan_amnt,y = delinq_2yrs)) + geom_smooth()

