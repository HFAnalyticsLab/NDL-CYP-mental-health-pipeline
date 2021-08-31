#######################################
################ SETUP ################
#######################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,DescTools,lubridate,pbapply,here,rvest,downloader,curl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/CYP MH/England/MHSDS/"

#Create sub-directories if not already there
setwd(rawdatadir)

#Main performance files
main_name <- "Main performance files"
if (main_name %in% list.dirs(path = ".", full.names = FALSE, recursive = FALSE)){
} else {
  dir.create(main_name)
}
#Eating disorders
ed_name <- "Eating disorders files"
if (main_name %in% list.dirs(path = ".", full.names = FALSE, recursive = FALSE)){
} else {
  dir.create(main_name)
}

######################################################
################ SCRAPE LANDING PAGES ################
######################################################

#NHS England Vaccination data website
nhse_link_series <- "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics/"

#Scrape names of pages and clean
monthly_names <- read_html(nhse_link_series) %>%
  html_nodes(xpath="//a[contains(@class, 'cta__button')]") %>%
  html_text() %>%
  as.data.frame() %>%
  rename(.,name=".") %>%
  mutate(.,name=tolower(name)) %>%
  mutate(.,name=str_replace_all(name,"mental health services monthly statistics",""),
         name=str_replace_all(name,"number of children and young people accessing nhs funded community mental health services in england","cyp")) %>%
  mutate(.,name=str_replace_all(name,"-",""),
         name=str_replace_all(name,":",""),
         name=str_replace_all(name,",","")) %>%
  mutate(.,name=trimws(name, "both")) %>%
  mutate(.,index=1:n()) %>% #Find out which links we want to download from here on
  mutate(., first_year=parse_number(name),
         month_name_perf=str_extract(name,"performance(\\s+[^\\s]+){1}"),
         month_name_final=str_extract(name,"final(\\s+[^\\s]+){1}")) %>% 
  mutate(.,month_name=paste(month_name_perf,month_name_final,sep=" ")) %>%
  mutate(.,month_name=str_replace_all(month_name,"NA",""),
         month_name=str_replace_all(month_name,"performance",""),
         month_name=str_replace_all(month_name,"final",""),
         month_name=trimws(month_name, "both")) %>%
  mutate(.,month_year=paste(month_name,first_year,sep=" "),
         wanted=ifelse(month_name!="",1,0)) %>% #Indicator if we want to download this
  select(.,-c("month_name_perf","month_name_final","first_year"))

#Scrape all download links
monthly_links <- read_html(nhse_link_series) %>%
  html_nodes(xpath="//a[contains(@class, 'cta__button')]/@href") %>%
  html_text() %>%
  paste0("https://digital.nhs.uk",.) %>%
  as.data.frame() %>% 
  rename(.,link=".") %>%
  mutate(.,index=1:n())

#Get only the links from subset we want and ann abbreviated month
month_abbv <- data.frame(month_name=c("january","february","march","april","may","june","july","august","september","october","november","december"),
                         month_abbv=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
monthly_names <- left_join(monthly_names,
                                  monthly_links,by="index") %>%
  left_join(.,month_abbv,by="month_name")
rm(monthly_links)

#Filter out unwanted links
monthly_names <- monthly_names %>%
  filter(.,wanted==1)

######################################################
################ SCRAPE MONTHLY PAGE #################
######################################################

#Create function to download monthly series
MHSDS_monthly_series_download <- function(monthyr){
  
  #monthyr <- "june 2021"
  
  #Display series name
  print(monthyr)
  
  #Get right monthly page
  nhse_monthly_link <- monthly_names %>%
    filter(.,month_year==monthyr) %>%
    pull(link)
  
  #Abbreviated month
  month_abbv <- monthly_names %>%
    filter(.,month_year==monthyr) %>%
    pull(month_abbv)
  
  if (length(nhse_monthly_link)>0) {
  
    #Get all csv names
    csv_names <- read_html(nhse_monthly_link) %>%
      html_nodes(xpath="//a[contains(@class, 'nhsd-a-box-link')]/@href") %>%
      html_text() %>%
      as.data.frame() %>% 
      rename(.,link=".") %>% 
      mutate(.,is_csv=str_detect(link, "csv")) %>%
      filter(.,is_csv==TRUE) %>%
      filter(.,str_detect(link,month_abbv)) #Only for 'final' month and not provisional
    
    ### FIRST FILE: Main performance file
    
    #Find link
    patterns_perf <- c(paste0("Data_",month_abbv,"Prf"),
                       paste0("MHSDS%20Data_",month_abbv))
    prf_link <- csv_names %>%
      filter(.,str_detect(link, paste(patterns_perf, collapse = "|"))) %>%
      slice_head(.,n=1) %>%
      pull(link) %>%
      ifelse(length(.)!=0,.,"no link found")
    
    #Download into right folder
    setwd(paste0(rawdatadir,main_name))
    already_there_main <- list.files()
    to_download_main <- prf_link[which(basename(URLdecode(prf_link)) %in% already_there_main==FALSE)]
    
    if(length(to_download_main)==0){
      print("nothing to download")
    } else if (to_download_main!="no link found"){
      for (k in 1:length(to_download_main)){
        curl::curl_download(to_download_main[k], destfile=basename(URLdecode(to_download_main[k])))
      }
    } else {
      print("nothing to download")
    }
    rm(already_there_main,to_download_main,patterns_perf,prf_link)
    
    ### SECOND FILE: Eating disorders
    
    #Find link
    ed_link <- csv_names %>%
      filter(.,str_detect(link,paste0("CYPED_",month_abbv))) %>%
      slice_head(.,n=1) %>%
      pull(link) %>%
      ifelse(length(.)!=0,.,"no link found")
    
    #Download into right folder
    setwd(paste0(rawdatadir,ed_name))
    already_there_ed <- list.files()
    to_download_ed <- ed_link[which(basename(URLdecode(ed_link)) %in% already_there_ed==FALSE)]
    
    if(length(to_download_ed)==0){
      print("nothing to download")
    } else if (to_download_ed!="no link found"){
      for (k in 1:length(to_download_ed)){
        curl::curl_download(to_download_ed[k], destfile=basename(URLdecode(to_download_ed[k])))
      }
    } else {
      print("nothing to download")
    }
    rm(already_there_ed,to_download_ed,ed_link)
    
    #Clean up environment
    rm(month_abbv,nhse_monthly_link,csv_names)
    
  } else {
    print("Monthly series not found")
  }
}

#Test function
MHSDS_monthly_series_download("june 2021")

#Choose months
all_months <- monthly_names %>%
  pull(month_year) %>%
  head(.,n=15)

#Run function
pblapply(all_months,MHSDS_monthly_series_download)

##################################################
################ EXPLORE PRF FILE ################
##################################################

MHSDS_Prf_Apr21 <- fread(paste0(rawdatadir,"/April 2021/MHSDS Data_AprPrf_2021.csv"),
                      header=TRUE, sep=",", check.names=T)

CYP_measures <- MHSDS_Prf_Apr21 %>%
  filter(.,startsWith(MEASURE_ID, "CYP")) %>%
  pull(MEASURE_NAME) %>%
  unique(.)

under18_measures <- MHSDS_Prf_Apr21 %>%
  mutate(MEASURE_NAME=tolower(MEASURE_NAME)) %>% 
  filter(.,str_detect(MEASURE_NAME, "0 to 18|under 16|age 16|age 17|0 to 17|under 18")) %>%
  pull(MEASURE_NAME) %>%
  unique(.)

##################################################
################ EXPLORE ED FILE #################
##################################################

MHSDS_ED_Apr21 <- fread(paste0(rawdatadir,"/April 2021/MHSDS Data_CYPED_AprPrf_2021.csv"),
                     header=TRUE, sep=",", check.names=T)

ed_cyp_measures <- MHSDS_ED_Apr21 %>%
  pull(MEASURE_NAME) %>%
  unique(.)

#####################################################
################ EXPLORE MH ACT FILE ################
#####################################################

# MHSDS_MHS_Apr21 <- fread(paste0(rawdatadir,"/April 2021/MHSDS Data_MHA_AprPrf_2021.csv"),
#                         header=TRUE, sep=",", check.names=T)
# 
# MHSDS_MHS_Apr21 %>%
#   pull(MEASURE_NAME) %>%
#   unique(.)
# 
# MHSDS_MHS_Apr21 %>%
#   pull(BREAKDOWN) %>%
#   unique(.)

#####################################################
################ EXPLORE RESTR FILE #################
#####################################################

# MHSDS_RI_Apr21 <- fread(paste0(rawdatadir,"/April 2021/MHSDS Data_Rstr_New_AprPrf_2021_v2.csv"),
#                          header=TRUE, sep=",", check.names=T)
# 
# MHSDS_RI_Apr21 %>%
#   pull(LEVEL_TWO_DESCRIPTION) %>%
#   unique(.)
# 
# MHSDS_MHS_Apr21 %>%
#   pull(BREAKDOWN) %>%
#   unique(.)

#########################################################
################ EXPLORE PERINATAL FILE #################
#########################################################

# MHSDS_PN_Apr21 <- fread(paste0(rawdatadir,"/April 2021/CYP_Perinatal_April21_Perf.csv"),
#                          header=TRUE, sep=",", check.names=T)