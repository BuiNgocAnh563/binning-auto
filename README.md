# binning-auto
auto binning data by r
rm(list = ls())
#### LIBRARY ####
library(readxl)
library(tidyverse)
library(lubridate)
library(stringi)
library(readr)
#library(xlsx)
library(openxlsx)

data=read_excel('./default of credit card clients.xlsx')
head(data)
#Tạo các trường cần thiết:
for (i in c(1:6)) {
  #Tỷ lệ sử dụng hạn mức
  data[[paste0('TLSDHM_',i)]]= data[[paste0('BILL_AMT',i)]]/data[['LIMIT_BAL']]
  
  #Tỷ lệ thanh toán dư nợ:
  data[[paste0('TLTTT_',i)]] = data[[paste0('PAY_AMT',i)]]/data[[paste0('BILL_AMT',i)]]
  
}
#Trung binh su dung the trong ba thang truoc:
data['cc_bal_3m']= rowMeans(data[c('TLSDHM_1','TLSDHM_2','TLSDHM_3')],na.rm=T)
data['cc_bal_6m'] = rowMeans(data[c('TLSDHM_1','TLSDHM_2','TLSDHM_3','TLSDHM_4','TLSDHM_5','TLSDHM_6')],na.rm=T)
data = data %>% select(CIF_NUMBER,LIMIT_BAL:PAY_AMT6,TLSDHM_1:TLSDHM_6,good_bad,cc_bal_3m,cc_bal_6m,good_bad_portfolio)
#check:
d_col_or=c('SEX','EDUCATION','MARRIAGE')
c_col_or=colnames(data)[!(colnames(data) %in% d_col_or)]

head(data[c(1:10)])
#change a level of d_col:
converttonumberfactor <- function(dataset,file){
  for (i in 1:length(d_col_or)) {
    col=d_col_or[i]
    level <- unique(dataset[col]) %>% as.data.frame() %>% mutate(group=row.names(.)) 
    colnames(level) <- c('vari','group')
    level$name <- col
    dataset[[col]]=level[match(dataset[[col]],level[['vari']]),"group"]
    if(i==1){ fileout=level} else {fileout=rbind(fileout,level)}
    wb = createWorkbook()
    addWorksheet(wb, sheetName = 'group_d')
    writeData(wb, sheet='group_d', fileout ,rowNames = TRUE)
    saveWorkbook(wb, file, overwrite=TRUE)
  }
  return(list(data=dataset,d_file=fileout))
}
fileout="./output/d_variable.xlsx"
convert=converttonumberfactor(data,fileout)
raw <- convert$data
head(raw[c(1:10)])
#Convert from integer to numeric
indx <- sapply(select(raw,colnames(raw[1:ncol(raw)])), is.character)
raw[indx] <- lapply(raw[indx], function(x) suppressWarnings(as.numeric(x)))
#Hàm train_test_split chia data thành train và test set theo 'ratio' và giữ nguyên phân bổ các cột trong vector 'stratified_col'
train_test_split = function(dataset, stratified_col, ratio, col_train, col_test, seed){
  set.seed(seed)
  train_set = dataset %>% group_by_(stratified_col) %>% sample_frac(ratio)
  dataset[col_train] = as.numeric(ifelse(dataset[['CIF_NUMBER']] %in% train_set[['CIF_NUMBER']], 1,0))
  dataset[col_test] = as.numeric(ifelse(dataset[[col_train]] == 0, 1,0))
  return(dataset)
}

stratified_col = c("good_bad",d_col_or,c_col_or)
raw = train_test_split(raw, stratified_col , 0.7, "filter_training", "filter_testing", seed = 100)

#THEM CAC TRUONG FILTER
raw[['filter_portfolio']] = ifelse(!is.na(raw[['good_bad']]), 1,0)
raw[['filter_sample']] = ifelse(raw[['good_bad']] >= 0 & raw[['filter_portfolio']], 1, 0)

raw[['filter_training_sample']] = ifelse(raw[['filter_training']]==1 & raw[['filter_sample']]==1, 1,0)
raw[['filter_testing_sample']] = ifelse(raw[['filter_testing']]==1 & raw[['filter_sample']]==1, 1,0)
raw[['filter_inter']] = ifelse(raw[['good_bad']]==-1 & raw[['filter_portfolio']]==1, 1,0)

# TRAINING
training_sample = raw[(raw[['filter_training_sample']] == 1) & (!is.na(raw[['filter_training_sample']])),]

d_file=convert$d_file 
d_file$group=as.numeric(d_file$group)

raw <- raw[!is.na(raw$good_bad),] #Loai_bo_good bad bi NA
head(raw[c(1:4,30:ncol(raw))])
# AUTOBINING
gini_calc= function(gb_data){
  gb_data['0+1'] = gb_data['0'] + gb_data['1'] # Tong GB cho moi tieu chi
  gb_data['per_0+1'] = gb_data['0+1'] / sum(gb_data['0+1']) # Tinh % GB tren tong so GB cua Sample
  gb_data['per_0'] = gb_data['0'] / sum(gb_data['0']) # So Good chia cho tong Good
  gb_data['per_1'] = gb_data['1'] / sum(gb_data['1']) # So Bad chia cho tong Bad
  gb_data['cum_per_1'] = cumsum(gb_data['1']) / sum(gb_data['1']) # Tong luy ke cho Good va Bad
  gb_data['cum_per_1_lag'] = c(0, gb_data[['cum_per_1']][1:nrow(gb_data) - 1]) #Tinh lag cho cum1 (tru 1)
  gb_data['area_slice'] = (gb_data['cum_per_1'] +  gb_data['cum_per_1_lag']) * gb_data['per_0+1'] / 2 #Tinh dien tich phan cat
  gb_data['gini'] = (sum(gb_data['area_slice']) - 0.5) / (0.5 - sum(gb_data['1']) / sum(gb_data['0+1']) / 2)
  return(gb_data)
}

nd_calc = function(gb_data){
  w_mean_1 = weighted.mean(gb_data[['rank']], gb_data[['1']]) # rank = thu tu cot danh tu 1 den n (tuy vao dataset)
  w_mean_0 = weighted.mean(gb_data[['rank']], gb_data[['0']])
  w_std_1 = sqrt(sum(gb_data['1'] * (gb_data['rank'] - w_mean_1)^2) / sum(gb_data['1']))
  w_std_0 = sqrt(sum(gb_data['0'] * (gb_data['rank'] - w_mean_0)^2) / sum(gb_data['0']))
  w_std_all = sqrt((sum(gb_data['1']) * w_std_1^2 + sum(gb_data['0']) * w_std_0^2) / (sum(gb_data['1']) + sum(gb_data['0'])))
  gb_data['ND'] = (w_mean_0 - w_mean_1) / w_std_all
  return(gb_data)
}

iv_calc = function(gb_data){
  gb_data['IV_part'] = log(gb_data['per_0'] / gb_data['per_1']) * (gb_data['per_0'] - gb_data['per_1'])
  gb_data[is.infinite(gb_data[['IV_part']]) | is.na(gb_data['IV_part']),'IV_part'] = 0
  gb_data['IV'] = sum(gb_data['IV_part'])
  return(gb_data)
}

count_na = function(dataset, col){
  count_na_df=data.frame(colSums(is.na(dataset[,col])))
  colnames(count_na_df) = 'na_count'
  count_na_df['na_per']=count_na_df['na_count']/nrow(dataset)
  count_na_df['var']=row.names(count_na_df)
  count_na_df['total'] = nrow(dataset)
  row.names(count_na_df)=NULL
  cn=colnames(count_na_df)
  return(count_na_df[,c('var', 'na_count', 'total', 'na_per')])
}

ttest_calc = function(gb_data){
  # XU LY LOI TIMES DO THIEU COT (1), (-1) HOAC (0) - THAY NA TRONG CAC COT DO BANG GIA TRI 0
  gb_data[["1"]][is.na(gb_data[["1"]])] = 0
  gb_data[["0"]][is.na(gb_data[["0"]])] = 0
  #gb_data[["-1"]][is.na(gb_data[["-1"]])] = 0
  
  hist_good = c()
  
  for (i in 1:nrow(gb_data)){
    hist_good = c(hist_good, rep(gb_data[['rank']][i], gb_data[['0']][i]))
  }
  
  hist_bad = c()
  for (j in 1:nrow(gb_data)){
    hist_bad = c(hist_bad, rep(gb_data[['rank']][j], gb_data[['1']][j]))
  }
  
  gb_data['p_value'] = tryCatch({
    t.test(hist_good,hist_bad)$p.value}, error = function(cond){
      message(cond)
      return (NA)
    })
  return(gb_data)
}

correlation = function (dataset, col, thres=0.6){
  cor = as.data.frame(cor(dataset[,col], use = "pairwise.complete.obs"))
  # print (cor)
  # print (rowSums(abs(cor)>=thres)>=2)
  if (any(rowSums(abs(cor)>=thres)>=2, na.rm=TRUE)){
    return(cor[rowSums(abs(cor)>=thres, na.rm=TRUE)>=2& !is.na(rowSums(abs(cor)>=thres, na.rm=TRUE)), 
               colSums(abs(cor)>=thres, na.rm=TRUE)>=2& !is.na(colSums(abs(cor)>=thres, na.rm=TRUE))])
  } else {
    return (cor)
  }
}

correlation1 = function (dataset, col, thres=0.6){
  if(length(col) == 1){ return (data.frame())}
  cor1 = as.data.frame(cor(dataset[,col], use = "pairwise.complete.obs"))
}

aggre_calc_raw = function(dataset, d_col, c_col, q,  file, filter_col, indicator = 'gini', threshold = 0.1){
  dataset = dataset[dataset[[filter_col]]== 1,]
  col_name_syn = c(d_col, c_col) %>% unlist() %>% as.vector()
  # Create sheet to save na_c and corr
  # For loop run from 1: the end of the dataframe
  # dataset: Datainput
  # d_col: Discrete variable
  # c_col: Continous variable 
  # col_name_syn: Combination Vector of col_name_1 and col_name_2
  # q: Bucket
  # file: file autobins
  # filter_col: sample for autobin
  
  for (i in 1:length(col_name_syn)){
    col = col_name_syn[i]
    
    temp = dataset[!is.na(dataset[col]) ,c(col, 'good_bad')]
    temp1 = dataset[is.na(dataset[col]) ,c(col, 'good_bad')]
    if (!(col %in% c_col)){
      b = sort(unique(dataset[[col]]))
      b = c(min(b) - 1, b)
    }else{
      # print(col)
      b = unique(quantile(dataset[[col]], prob = q, na.rm = TRUE))
    }
   #print(col)
    # print(b)
    gb_data = temp %>% group_by(group = cut(temp[[col]], breaks = b, 
                                            include.lowest = TRUE, na.rm = TRUE), 
                                good_bad) %>% summarise(n = n())
    
    # print(gb_data)
    # break tai b 
    # inlcude.lowest: tinh den gia tri thap nhat, bao gom break
    # Sprear good, bad ra hai cot rieng, fill missing value = 0
    gb_data = gb_data %>% spread(good_bad, n, fill = 0) 
    gb_data['rank'] = as.numeric(rownames(gb_data))
    
    #print(gb_data)
    
    # create gb_data1 for NA
    if (nrow(temp1) > 0) {
      temp1['group'] = 'missing'
      gb_data1 = temp1 %>% group_by(group, good_bad) %>% summarise(n = n())
      gb_data1 = gb_data1 %>% spread(good_bad, n, fill = 0)
      gb_data1['rank'] = NA_integer_
      gb_data = rbind(gb_data, gb_data1)
    }
    
    #print(gb_data)
    gb_data['bad_rate'] = gb_data['1'] / (gb_data['0'] + gb_data['1'])
    gb_data['var'] = col
    cn = colnames(gb_data)
    gb_data_temp = gini_calc(gb_data[!is.na(gb_data['rank']),])
    
    # print(gb_data_temp)
    
    gb_data_temp = nd_calc(gb_data_temp)
    gb_data_temp = iv_calc(gb_data_temp)
    #print(gb_data_temp)
    gb_data_temp = ttest_calc(gb_data_temp)
    gb_data = gb_data %>% left_join(gb_data_temp, by = cn)
    gb_data <- left_join(gb_data,d_file,by=c("rank"="group","var"="name")) %>% rename(group_ori=vari)
    if (i == 1){
      gb_data_all = gb_data
    }else{
      gb_data_all = rbind(gb_data_all, gb_data)
    }
  }
  
  na_c = count_na(dataset, col_name_syn)
  corr = correlation(dataset, col_name_syn)
  #print(corr)
  gb_data_summary = gb_data_all %>% ungroup() %>% select(c(var, gini, ND, IV, p_value)) %>% distinct(var,.keep_all = TRUE)
  
  #COUNT
  count = gb_data_all %>% ungroup() %>% filter(group != "missing") %>% select("1", "0+1", "var") %>% group_by(var) %>% 
    summarise_all(sum) %>% rename( "n_bad"="1","n_total"="0+1" )
  gb_data_summary = gb_data_summary %>% left_join(count, by="var")
  
  
  gb_data_summary['sign'] = ifelse(gb_data_summary[['var']] %in% col_reverse, -1, 1)
  gb_data_summary['d_c'] = ifelse((gb_data_summary[['var']]) %in% d_col , "discrete","continuous")
  gb_data_summary['auto_choose'] = ifelse(((gb_data_summary[[indicator]])*gb_data_summary[['sign']] > threshold) | 
                                            (gb_data_summary[['var']]) %in% d_col , 1,0)
  
  
  #TINH BANG CORRELATION CHO CAC BIEN CHON AUTO
  #print(gb_data_summary,max = 10000)
  col_auto_choose = gb_data_summary[['var']][gb_data_summary[['auto_choose']] == 1 & !is.na(gb_data_summary[['auto_choose']])]
  #print(col_auto_choose)
  corr_auto_choose = correlation(dataset, col_auto_choose)
  
  wb = createWorkbook()
  addWorksheet(wb, sheetName = 'Count_GB')
  writeData(wb, sheet='Count_GB', gb_data_all,rowNames = TRUE)
  addWorksheet(wb, sheetName = 'Summary_GB')
  writeData(wb, sheet='Summary_GB', gb_data_summary,rowNames = TRUE)
  addWorksheet(wb, sheetName = 'na_count')
  writeData(wb, sheet='na_count', na_c,rowNames = TRUE)
  addWorksheet(wb, sheetName = 'Corr')
  writeData(wb, sheet='Corr', corr,rowNames = TRUE)
  addWorksheet(wb, sheetName = 'Corr_auto_choose')
  writeData(wb, sheet='Corr_auto_choose', corr_auto_choose,rowNames = TRUE)
  # writeData(wb, sheet='Corr', corr,rowNames = TRUE)
  saveWorkbook(wb, file, overwrite=TRUE)
  
  return(list(data =dataset, na_count = na_c,correlation= corr, 
              count_gb_review=gb_data_all,count_gb_summary=gb_data_summary))
}
#SET OUT PUT
input_variable = read_xlsx("./input/input_variable.xlsx")

d_col = input_variable[input_variable[['d_c']]== "discrete", "var"][['var']]
c_col = input_variable[input_variable[['d_c']]== "continuous", "var"][['var']]
col_reverse = input_variable[input_variable[['sign']]== -1, "var"][['var']]

# CHO ALL
q = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) # Bucket 10 (input)

file = "./output/Auto_bin.xlsx"
result = aggre_calc_raw(training_sample, d_col, c_col, q, file, "filter_portfolio")
all_name = paste(colnames(result$count_gb_summary)[2:length(colnames(result$count_gb_summary))],"_all", sep = "")
# MANNUAL BINNING
nd_calc = function(gb_data, col){
  w_mean_1 = weighted.mean(gb_data[[col]], gb_data[['1']]) # col la gia tri score dung de tinh toan
  w_mean_0 = weighted.mean(gb_data[[col]], gb_data[['0']])
  w_std_1 = sqrt(sum(gb_data['1'] * (gb_data[col] - w_mean_1)^2) / sum(gb_data['1']))
  w_std_0 = sqrt(sum(gb_data['0'] * (gb_data[col] - w_mean_0)^2) / sum(gb_data['0']))
  w_std_all = sqrt((sum(gb_data['1']) * w_std_1^2 + sum(gb_data['0']) * w_std_0^2) / (sum(gb_data['1']) + sum(gb_data['0'])))
  gb_data['ND'] = (w_mean_0 - w_mean_1) / w_std_all
  return(gb_data)
}

count_na = function(dataset, col, col_filter){
  dataset1 = dataset[dataset[[col_filter]] == 1,]
  count_na_df=data.frame(colSums(is.na(select(dataset1, col))))
  colnames(count_na_df) = 'na_count'
  count_na_df['na_per']=count_na_df['na_count']/nrow(dataset1)
  count_na_df['var']=row.names(count_na_df)
  count_na_df['count_total'] = nrow(dataset1)
  row.names(count_na_df)=NULL
  cn=colnames(count_na_df)
  return(count_na_df[,c(cn[length(cn)],cn[1:length(cn)-1])])
}

ttest_calc = function(gb_data, col){
  # XU LY LOI TIMES DO THIEU COT (1), (-1) HOAC (0) - THAY NA TRONG CAC COT DO BANG GIA TRI 0
  gb_data[["1"]][is.na(gb_data[["1"]])] = 0
  gb_data[["0"]][is.na(gb_data[["0"]])] = 0
  
  hist_good = c()
  for (i in 1:nrow(gb_data)){
    hist_good = c(hist_good, rep(gb_data[[col]][i], gb_data[['0']][i]))
  }
  
  hist_bad = c()
  for (j in 1:nrow(gb_data)){
    hist_bad = c(hist_bad, rep(gb_data[[col]][j], gb_data[['1']][j]))
  }
  
  gb_data['p_value'] = tryCatch({
    t.test(hist_good,hist_bad)$p.value}, error = function(cond){
      message(cond)
      return (NA)
    })
  return(gb_data)
}

correlation = function (dataset, col, thres=0.6){
  cor = as.data.frame(cor(select(dataset,col), use = "pairwise.complete.obs"))
  # print (cor)
  # print (rowSums(abs(cor)>=thres)>=2)
  if (any(rowSums(abs(cor)>=thres)>=2, na.rm=TRUE)){
    return(cor[rowSums(abs(cor)>=thres, na.rm=TRUE)>=2& !is.na(rowSums(abs(cor)>=thres, na.rm=TRUE)), 
               colSums(abs(cor)>=thres, na.rm=TRUE)>=2& !is.na(colSums(abs(cor)>=thres, na.rm=TRUE)) ])
  } else {
    return (cor)
  }
  }
  subset_char= function (string) {
  if (substr(string,1,1) %in% c("[","(")) {
    return (unlist(strsplit(substring(string,2) ,","))[1])
  } else {return (NA)}
}

subset_char_upper = function (string) {
  if (substr(string,1,1) %in% c("[","(")) {
    
    string_sub = unlist(strsplit(substring(string,2) ,","))[2]
    
    # remove the last letter
    string_upper = substr(string_sub,1, nchar(string_sub)-1)
    
    return (string_upper)
  } else {return (NA)}
}
# CODE DE MAPPING DIEM CHO TUNG TIEU CHI DUA VAO FILE PARAMS- CHIA NGUONG BUCKET
mapping_score_chiabk = function (dataset, col,col_bucket,col_score, params){ #col_bucket:Biáº¿n_bk
  
  if (col %in% d_col_or){
    
    dataset[[col_bucket]] = as.character(dataset[[col]])
    dataset[[col_bucket]][is.na(dataset[[col_bucket]])] = "NA"
    
  } else {
    b = sapply(params$gr[params$gr != "NA"], subset_char)
    b = c(as.numeric(b),Inf)
    
    dataset[[col_bucket]] = as.character(cut(dataset[[col]], breaks = b, 
                                             include.lowest = TRUE, na.rm = TRUE))
    dataset[[col_bucket]][is.na(dataset[[col_bucket]])] = "NA"
    
    # LOAI BO WHITESPACE TRONG TEXT, DO INF CO DAU CACH DANG TRUOC
    dataset[[col_bucket]] = gsub(" ", "",dataset[[col_bucket]])
    
  }
  
  dataset = dataset %>% left_join(select(params, c(gr, score)), by=setNames(nm=col_bucket, "gr")) %>% 
    rename_at(vars("score"), ~col_score)
  
  return (dataset)
}

# CODE DE CHAY INDICATOR - COUNT GOOD-BAD, TINH TOAN GINI, ND, IV CHO TUNG TIEU CHI
cal_gb_indicator = function (dataset, col, col_score, col_score_tf, col_score_tf_norm, 
                             col_cal_indi, col_good_bad, col_filter, col_filter_port) {
  
  grouping = rlang::syms (c(col_score, col_good_bad))
  fil_na = rlang::sym(col_score)
  fil_col = rlang::sym(col_filter)
  
  gb_data = dataset %>% filter( !is.na(!!fil_na) & (!!fil_col)== 1) %>% group_by(!!!grouping) %>% summarise(n = n()) %>% ungroup()
  colnames(gb_data)[colnames(gb_data)==col_score] <- 'score'
  
  # break tai b 
  # inlcude.lowest: tinh den gia tri thap nhat, bao gom break
  # Spread good, bad ra hai cot rieng, fill missing value = 0
  
  
  gb_data = gb_data %>% tidyr::spread_(col_good_bad, "n", fill = 0) 
  
  if (!("1" %in% colnames(gb_data))) {gb_data['1'] = 0}
  
  gb_data['bad_rate'] = gb_data['1'] / (gb_data['0'] + gb_data['1'])
  
  gb_data['var'] = col
  
  # CHECK XEM BAD RATE CO GIAM DAN THEO CHIEU DIEM SO KHONG
  rank_score = rank(-gb_data[["score"]])
  rank_br = rank(gb_data[['bad_rate']])
  
  is_badrate_desc = all(rank_score == rank_br)
  
  if (is_badrate_desc){
    gb_data[["score_tf"]] = 1 - (1-0.2)*
      (gb_data$bad_rate- min(gb_data$bad_rate))/(max(gb_data$bad_rate) - min(gb_data$bad_rate))
  } else {
    gb_data[["score_tf"]] = gb_data[["score"]] 
  }
  
  # TINH THEM BIEN SAU KHI TRANSFORM BAD RATE
  
  dataset = dataset %>% left_join(select(gb_data,c(score, score_tf)), by=setNames(nm=col_score, "score"))
  colnames(dataset)[colnames(dataset) == "score_tf"]  <- col_score_tf
  
  mean = mean(dataset[[col_score_tf]][dataset[[col_filter_port]]==1], na.rm=TRUE)
  sd = sd(dataset[[col_score_tf]][dataset[[col_filter_port]]==1], na.rm=TRUE)
  
  dataset[[col_score_tf_norm]] = (dataset[[col_score_tf]]-mean)/sd
  gb_data[["score_tf_norm"]] = (gb_data[["score_tf"]]-mean)/sd
  
  # INDICATOR CALCULATION FOR GB_DATA
  gb_data = gini_calc(gb_data)
  gb_data = nd_calc(gb_data, col_cal_indi)
  gb_data = iv_calc(gb_data)
  gb_data = ttest_calc(gb_data, col_cal_indi)
  
  
  # THEM- test thu
  gb_data_add = gb_data
  gb_data_add['total_g'] = sum(gb_data_add[['0']])
  gb_data_add['total_gb'] = sum(gb_data_add[['0+1']])
  
  gb_data_summary = gb_data_add %>% ungroup() %>% select(c(var, gini, ND, IV, p_value, total_g,total_gb)) %>% distinct()
  
  return (list(data= dataset, gb=gb_data, gb_summary =gb_data_summary ))
  
}

# CODE MAPPING DIEM CHO TAT CA CAC TIEU CHI
aggre_calc_raw_chiabucket = function(dataset, d_col, c_col, parms_bk, file, col_good_bad, 
                                     col_filter, col_filter_port, review_auto) {
  col_name_syn = c(d_col, c_col)
  col_bucket_all = paste(col_name_syn, "_bk",sep="")
  col_score_all = paste(col_name_syn, "_score",sep="")
  col_score_tf_all = paste(col_name_syn, "_score_tf",sep="")
  col_score_tf_norm_all = paste(col_name_syn, "_score_tf_norm",sep="")
  
  for (i in 1:length(col_name_syn)){
    col = col_name_syn[i]
    col_bucket = col_bucket_all[i]
    col_score = col_score_all[i]
    col_score_tf = col_score_tf_all[i]
    col_score_tf_norm = col_score_tf_norm_all[i]
    
    # temp = dataset[c(col, 'good_bad')]
    col_params = parms_bk %>% filter(variable == col)
    
    # print(col)
    # print(col_params)
    dataset = mapping_score_chiabk(dataset, col, col_bucket, col_score, col_params)
    
    #print(col_score)
    #print(dataset[[col_score]])
    # CALCULATE INDICATOR
    cal_indi = cal_gb_indicator(dataset, col, col_score, col_score_tf, col_score_tf_norm, "score_tf_norm", 
                                col_good_bad, col_filter, col_filter_port)
    
    gb_data = cal_indi$gb
    dataset = cal_indi$data
    
    if (i == 1){
      gb_data_all = gb_data
    }else{
      gb_data_all = rbind(gb_data_all, gb_data)
    }
  }
  
  na_c = count_na(dataset, col_score_tf_all, col_filter)
  #  na_c = count_na(dataset, col_score_tf_norm_all, col_filter)
  corr = correlation(dataset, col_score_tf_norm_all)
  gb_data_summary = gb_data_all %>% ungroup() %>% select(c(var, gini, ND, IV, p_value)) %>% distinct()
  
  #COUNT
  count = gb_data_all %>% ungroup() %>% filter(score != "missing") %>% select("1", "0+1", "var") %>% group_by(var) %>% 
    summarise_all(sum) %>% rename( "n_bad"="1","n_total"="0+1" )
  gb_data_summary = gb_data_summary %>% left_join(count, by="var")
  
  # CODE DE GOP FILE AUTOMATION VA FILE CHIA MANUAL LAI
  for (i in 1:length(col_name_syn)){
    col = col_name_syn[i]
    
    # temp = dataset[c(col, 'good_bad')]
    manual_parms_col = parms_bk %>% filter(variable == col)
    auto_col = review_auto %>% filter(var == col) %>% select(-c("0","1","rank","per_0","per_1","cum_per_1","cum_per_1_lag","area_slice","IV_part"))
    
    manual_col = gb_data_all %>% filter(var == col) %>% select(-c("0","1","per_0","per_1","cum_per_1","cum_per_1_lag","area_slice","IV_part",
                                                                  "score_tf","score_tf_norm"
    ))
    
    if (col %in% c_col_or) {
      auto_col[auto_col$group=="missing", "group"] = "NA"
      auto_col['threshold_auto']= as.numeric(sapply(auto_col$group, subset_char_upper))
      
      #TAO THEM -INF CHO AUTO_COL
      auto_col = auto_col %>% ungroup() %>% add_row(threshold_auto=-Inf,.before = 1)
      
      threshold_manual = as.numeric(c(sapply(manual_parms_col$gr[manual_parms_col$gr != "NA"], subset_char),Inf))
      
      auto_col['cut_manual'] = as.character(cut(auto_col[['threshold_auto']],threshold_manual, include.lowest = TRUE))
      
      auto_col[['cut_manual']] = gsub(" ", "",auto_col[['cut_manual']])
      auto_col[['cut_manual']][is.na(auto_col[['cut_manual']])] = 'NA'
      
      auto_col_map = auto_col %>% 
        left_join(select(manual_parms_col, gr, score), by=c("cut_manual"="gr")) %>%
        left_join(manual_col, by="score", suffix=c("_auto","_manual")) %>% select (-c("threshold_auto"))
      
      
    } else {
      auto_col[auto_col$group=="missing", "group"] = "NA"
      auto_col['threshold_auto']= as.numeric(sapply(auto_col$group, subset_char_upper))
      
      auto_col[['cut_manual']] = as.character(auto_col[['threshold_auto']])
      auto_col[['cut_manual']][is.na(auto_col[['cut_manual']])] = 'NA'
      
      auto_col_map = auto_col %>% 
        left_join(select(manual_parms_col, gr, score), by=c("cut_manual"="gr")) %>%
        left_join(manual_col, by="score", suffix=c("_auto","_manual")) %>% select (-c("threshold_auto"))
    } 
    
    if (i == 1){
      all_auto_col_map = data.frame(auto_col_map)
    } else {
      all_auto_col_map = rbind(all_auto_col_map, data.frame(auto_col_map))
    }
  }
  
  wb = createWorkbook()
  addWorksheet(wb, sheetName = 'na_count')
  writeData(wb, sheet='na_count', as.data.frame(na_c),rowNames = TRUE)
  addWorksheet(wb, sheetName = 'Corr')
  writeData(wb, sheet='Corr',as.data.frame(corr),rowNames = TRUE)
  addWorksheet(wb, sheetName = 'count_gb_summary')
  writeData(wb, sheet='count_gb_summary', as.data.frame(gb_data_summary),rowNames = TRUE)
  addWorksheet(wb, sheetName = 'count_gb_review')
  writeData(wb, sheet='count_gb_review', as.data.frame(gb_data_all),rowNames = TRUE)
  
  addWorksheet(wb, sheetName = 'auto_map_manual')
  writeData(wb, sheet='auto_map_manual', as.data.frame(all_auto_col_map),rowNames = TRUE)
  
  saveWorkbook(wb, file, overwrite=TRUE)
  
  return(list(data =dataset, na_count = na_c,correlation= corr, 
              count_gb_review=gb_data_all,count_gb_summary=gb_data_summary, auto_map_manual=all_auto_col_map))
}
#RUN MANUAL
params_chiabucket_loan = read_xlsx('./input/Manual_group.xlsx',sheet = "Manual_FN")

col_choose_loan = read_xlsx('./input/input_col_choose_group.xlsx')

col_choose_loan = col_choose_loan %>% filter(col_choose_loan[['filter']] == 1)

d_col = col_choose_loan[['variable']][col_choose_loan[['d_c']] == "discrete"]
c_col = col_choose_loan[['variable']][col_choose_loan[['d_c']] == "continuous"]
all_col = c(c_col, d_col)

# LAY LAI FILE AUTOMATION TU CODE CHIA AUTOMATION
automatic_loan = select(result$count_gb_review,-group_ori)

path_excel_review = './output/Output_Manual.xlsx'

df_summary_gopbk_loan = aggre_calc_raw_chiabucket(raw, d_col, c_col, params_chiabucket_loan, path_excel_review, 
                                                  "good_bad_portfolio", "filter_training_sample", 'filter_portfolio', automatic_loan)

raw_gopbk_loan = df_summary_gopbk_loan$data
gb_review_loan = df_summary_gopbk_loan$count_gb_review
gb_summary_loan = df_summary_gopbk_loan$count_gb_summary
# CHON LUA MO HINH TOI UU
# TINH TOAN CHO TONG DIEM
cal_total_score = function(dataset, data_indicator, col_choose, col_good_bad, col_filter, filter_port, file, outfile=TRUE, out_data=TRUE,
                           indicator="ND", target=0, weight="ND"){
  data_col_predict = data_indicator[data_indicator[[indicator]] >= target & data_indicator[["var"]] %in% col_choose
                                    , c("var", indicator)]
  data_col_predict['weight_indicator'] = abs(data_col_predict[["ND"]])/sum(abs(data_col_predict[["ND"]]))
  col_predict = data_col_predict[["var"]]
  col_predict_score_tf = paste(col_predict, "_score_tf", sep="")
  col_predict_score_tf_norm = paste(col_predict, "_score_tf_norm", sep="")
  weight_nd = data_col_predict[['weight_indicator']]
  if (length(col_predict) == 0) {
    dataset['percent_not_missing'] = 'NA'
    dataset['sum_score_nd_weight'] = 1} 
  else {
    dataset['percent_not_missing'] = rowSums(t(t(!is.na(dataset[,col_predict_score_tf_norm]))*weight_nd), na.rm=TRUE)
    dataset['sum_score_nd_weight'] = rowSums(t(t(dataset[,col_predict_score_tf_norm])*weight_nd), na.rm=TRUE)/
      rowSums(t(t(!is.na(dataset[,col_predict_score_tf_norm]))*weight_nd), na.rm=TRUE)
  }
  seq_pred = c(0, 0.1, 0.23, 0.4, 0.6, 0.77, 0.9, 1)
  breaks_pred= unique(quantile(dataset[['sum_score_nd_weight']][dataset[[filter_port]]== 1], seq_pred, na.rm=T))
  if(length(breaks_pred)==1){
    breaks_pred = c(0,1)
  }
  dataset['sum_score_nd_weight_bk'] = cut(dataset[['sum_score_nd_weight']], breaks= breaks_pred, 
                                          include.lowest=T, labels=F, na.rm=T)
  cal_indi = cal_gb_indicator(dataset,
                              'sum_score_nd_weight_bk', 
                              'sum_score_nd_weight_bk', 
                              'sum_score_nd_weight_bk_score_tf', 
                              'sum_score_nd_weight_bk_score_tf_norm', "score", col_good_bad, col_filter, filter_port)
  dataset = cal_indi$data
  #TINH TRUNG BINH VA DO LECH CHUAN - CAN FILTER THEO FILTER_PORTFOLIO
  if (length(col_predict) == 0){
    params_mean_sd = data.frame()
  } else {
    
    fil_port = rlang::sym(filter_port)
    
    mean = dataset %>% filter( (!!fil_port)==1) %>% summarise_at(col_predict_score_tf, mean, na.rm=TRUE)
    mean['params'] = 'mean'
    sd = dataset %>% filter( (!!fil_port)==1) %>% summarise_at(col_predict_score_tf, sd, na.rm=TRUE)
    sd['params'] = 'sd'
    params_mean_sd = rbind(mean, sd)
  }
  if (outfile){ 
    wb = createWorkbook()
    addWorksheet(wb, sheetName = 'summary_total_score')
    writeData(wb, sheet='summary_total_score', as.data.frame(cal_indi$gb),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'params_mean_sd')
    writeData(wb, sheet='params_mean_sd',as.data.frame(params_mean_sd),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'params_weight')
    writeData(wb, sheet='params_weight', as.data.frame(data_col_predict),rowNames = TRUE)
    saveWorkbook(wb, file, overwrite=TRUE)
  }
  if (out_data) {
    return (list(data=dataset, gb=cal_indi$gb, gb_summary=cal_indi$gb_summary, mean_sd=params_mean_sd, weight=data_col_predict))
  } else {return (list(gb=cal_indi$gb, gb_summary=cal_indi$gb_summary, mean_sd=params_mean_sd, weight=data_col_predict)) }
}
# TINH CHO TONG DIEM CHO NHỮNGNG BO MAU VA PORTFOLIO KHAC NHAU
cal_total_score_all_port = function (dataset, data_indicator, col_choose, col_gb_sample, col_gb_port, 
                                     col_filter_sample, col_filter_port, filter_port, file, outfile=TRUE, out_data=FALSE,
                                     indicator="ND", target=0, weight="ND") {
  col_tf_norm_all = paste(col_choose, "_score_tf_norm",sep="")
  na_c = count_na(dataset, col_tf_norm_all, filter_port)
  corr = correlation1(dataset, col_tf_norm_all)
  # LAY CACH GOP BIEN CUA NHUNG BIEN CHON VAO MO HINH
  chiabk_chooose = params_chiabucket_loan %>% filter(variable %in% col_choose)
  if (outfile) {wb = createWorkbook()}
  col_filter_all = c(col_filter_sample, col_filter_port)
  for (i in 1:length(col_filter_all)) {
    if (col_filter_all[i] %in% col_filter_sample) {
      cal_score = cal_total_score(dataset, data_indicator, col_choose, col_gb_sample, col_filter_all[i], filter_port, file, outfile=FALSE, out_data=FALSE,
                                  indicator, target, weight)
    } else (
      cal_score = cal_total_score(dataset, data_indicator, col_choose, col_gb_port, col_filter_all[i], filter_port, file, outfile=FALSE, out_data=FALSE,
                                  indicator, target, weight)
    )
    out_score = cal_score$gb_summary
    out_score['portfolio'] = col_filter_all[i]
    # RBIND VAO FILE OUT_SCORE_ALL - CHUA TONG HOP KET QUA TUNG LAN CHAY
    if (i==1){
      out_score_all = out_score
    } else {
      out_score_all = rbind(out_score_all,out_score)
    }
    
    if (outfile) {
      addWorksheet(wb, sheetName = col_filter_all[i])
      writeData(wb, sheet=col_filter_all[i], as.data.frame(cal_score$gb),rowNames = TRUE)
    }
  }
  if (outfile) {
    addWorksheet(wb, sheetName = 'out_score_all_port')
    writeData(wb, sheet='out_score_all_port', as.data.frame(out_score_all),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'mean_sd')
    writeData(wb, sheet='mean_sd', as.data.frame(cal_score$mean_sd),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'weight')
    writeData(wb, sheet='weight', as.data.frame(cal_score$weight),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'chiabk_chooose')
    writeData(wb, sheet='chiabk_chooose', as.data.frame(chiabk_chooose),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'na_count')
    writeData(wb, sheet='na_count', as.data.frame(na_c),rowNames = TRUE)
    addWorksheet(wb, sheetName = 'correlation')
    writeData(wb, sheet='correlation', as.data.frame(corr),rowNames = TRUE)
    saveWorkbook(wb, file, overwrite=TRUE)
  }
  
  return (out_score_all)
}

# CODE CHAY FORWARD STEPWISE
cal_total_score_stepwise = function (dataset,data_indicator, data_choose,col_good_bad, col_filter, filter_port, file, outfile=TRUE,
                                     indicator="ND", target=0, weight="ND"){
  
  data_choose = data_choose %>% filter(filter == 1)
  
  #TAO LIST CUA CAC GROUP BIEN
  group = split(data_choose[["variable"]], data_choose[["group"]])
  
  # CHUYEN THANH VECTOR CHUA CAC BIEN
  group_unlist = unlist(group)
  
  # TAO VECTOR c_choose VA GINI BAN DAU BANG 0
  c_choose = c()
  max_gini = 0
  while_step = 0
  
  continue = 1
  group_remove_unlist = group_unlist 
  
  while (continue!=0 & length(group_remove_unlist)!=0) {
    
    continue = 0
    while_step = while_step+1
    c_choose_prev = c_choose
    
    for (i in 1:length(group_remove_unlist)) {
      
      var = c(c_choose_prev, group_remove_unlist[i])
      
      # print (var)
      # CODE DUOI CO OUTFILE=FALSE nen gia tri file o duoi chi de khai bao, khong tao ra file moi
      f = 'D:/test.xlsx'
      cal_score = cal_total_score(dataset, data_indicator, var, col_good_bad, col_filter, filter_port, f, outfile=FALSE, out_data=FALSE,
                                  indicator, target, weight)
      out_score = cal_score$gb_summary
      
      #print("stepwise")
      #print (cal_score$gb_summary)
      #print (out_score[["gini"]])
      
      out_score['col_choose'] = paste(var, collapse = ',')
      out_score['step'] = while_step
      
      # RBIND VAO FILE OUT_SCORE_ALL - CHUA TONG HOP KET QUA TUNG LAN CHAY
      if (i==1 & while_step== 1){
        out_score_all = out_score
      } else {
        out_score_all = rbind(out_score_all,out_score)
      }
      
      # NEU GINI CUA MO HINH LON HON max_gini, GAN LAI GIA TRI MAX_GINI
      
      if (out_score[["gini"]] > max_gini){
        max_gini = out_score[["gini"]]
        c_choose = var
        continue = 1
      }
    }
    
    #print(while_step)
    #print (continue)
    #print (c_choose)
    
    if (continue == 1){
      
      # LAY RA INDEX CUA CAC GROUP CAN LOAI RA
      if(length(c_choose) == 1) {
        group_ind_remove = which(sapply(group, FUN=function(X) c_choose %in% X))
      } else {
        group_ind_remove = apply(sapply(group, FUN=function(X) c_choose %in% X), which, MARGIN = 1)
      }
      group_remove = group[-group_ind_remove]
      group_remove_unlist = unlist(group_remove)
    }
  }
  
  if (outfile){
    print("print")
    wb = createWorkbook()
    addWorksheet(wb, sheetName = 'out_score_all')
    writeData(wb, sheet='out_score_all', as.data.frame(out_score_all),rowNames = TRUE)
    saveWorkbook(wb, file, overwrite=TRUE)
  }
  
  return (out_score_all)
  
}
setwd('D:/dung duong/Ban Giao/24. Train Model/')
col_choose1 = read.xlsx('./input/input_col_choose_group.xlsx')
#Thay doi col_choose1/2 vao function de tinh toan
f_stepwise = './output/stepwise.xlsx'

df_total_score_train_stepwise = cal_total_score_stepwise(raw_gopbk_loan, gb_summary_loan, col_choose1, 
                                                         "good_bad_portfolio", 'filter_training_sample', 'filter_portfolio', file=f_stepwise, outfile = TRUE)
                                                         print(df_total_score_train_stepwise[order(df_total_score_train_stepwise$gini),] %>% head())
                                                         setwd('D:/dung duong/Ban Giao/24. Train Model/')
# CHON BO TIEU CHI TOI UU SAU KHI DA THUC HIEN BUOC STEPWISE:
col_choose_final_vect1=c('LIMIT_BAL','PAY_0','PAY_AMT1','TLTTT_3')
col_gb_sample = c("filter_training_sample","filter_testing_sample")

col_gb_port = c("filter_portfolio")

file_all = './output/file_all.xlsx'

df_final_model = cal_total_score_all_port(raw_gopbk_loan, gb_summary_loan, col_choose_final_vect1, 
                                          "good_bad", "good_bad_portfolio",
                                          col_gb_sample, col_gb_port,'filter_portfolio', file_all, outfile=TRUE, out_data=TRUE,
                                          indicator="ND", target=0, weight="ND")
print(df_final_model)
