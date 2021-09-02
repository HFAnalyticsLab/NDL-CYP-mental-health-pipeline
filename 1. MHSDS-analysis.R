#######################################
################ TO-DO ################
#######################################

#Test on other computer
#Dashboard where we can select CCG/area for 2 or 3 charts
#One panel for main and another for eating disorders
#Take inspiration from their BI app (https://app.powerbi.com/view?r=eyJrIjoiZDA3ZTE0ZjgtMmI2Ni00ZTI5LThkODctZTg2ZjZjZTk2ZWE5IiwidCI6IjUwZjYwNzFmLWJiZmUtNDAxYS04ODAzLTY3Mzc0OGU2MjllMiIsImMiOjh9&pageName=ReportSection090d617b47d4597458ae)

#######################################
################ SETUP ################
#######################################

#Load packages
library("tidyverse")
library("lubridate")
library("here")

#Clean up the global environment
rm(list = ls())

#Do we want to refresh data?
refresh_data <- "NO"

if(refresh_data=="YES"){
  #Refresh data
  source(here::here("0. Scrape-NHSE-download-and-append.R"))
  #Clean up the global environment
  rm(list = ls())
} else if (refresh_data=="NO"){
}

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/CYP MH/England/MHSDS/"
main_name <- "Main performance files" #Replace with the name of your sub-folder for main performance files
ed_name <- "Eating disorders files" #Replace with the name of your sub-folder for eating disorder files

##################################################
################ EXPLORE PRF FILE ################
##################################################

MHSDS_main_pooled <- fread(paste0(rawdatadir,main_name,"/Pooled/MHSDS_main_pooled.csv"),
                      header=TRUE, sep=",", check.names=T)

#Subset of metrics (to make size more manageable)

CYP_measures <- MHSDS_main_pooled %>%
  filter(.,startsWith(MEASURE_ID, "CYP")) %>%
  pull(MEASURE_NAME) %>%
  unique(.)

under18_measures <- MHSDS_main_pooled %>%
  mutate(MEASURE_NAME=tolower(MEASURE_NAME)) %>% 
  filter(.,str_detect(MEASURE_NAME, "0 to 18|under 16|age 16|age 17|0 to 17|under 18")) %>%
  pull(MEASURE_NAME) %>%
  unique(.)

MHSDS_main_pooled <- MHSDS_main_pooled %>%
  filter(.,MEASURE_NAME %in% c(CYP_measures,under18_measures))

#Correct dates

MHSDS_main_pooled <- MHSDS_main_pooled %>%
  mutate(.,format_date=ifelse(str_detect(REPORTING_PERIOD_START,"/"),"dmy","ymd")) %>% 
  mutate(.,
         start_ymd=ifelse(format_date=="ymd",REPORTING_PERIOD_START,NA),
         start_dmy=ifelse(format_date=="dmy",REPORTING_PERIOD_START,NA),
         end_ymd=ifelse(format_date=="ymd",REPORTING_PERIOD_END,NA),
         end_dmy=ifelse(format_date=="dmy",REPORTING_PERIOD_END,NA)) %>%
  mutate(.,
         start_ymd=lubridate::ymd(start_ymd),
         start_dmy=lubridate::dmy(start_dmy),
         end_ymd=lubridate::ymd(end_ymd),
         end_dmy=lubridate::dmy(end_dmy)) %>%
  mutate(.,start_date=ymd(ifelse(is.na(start_ymd),as.character(start_dmy),
                             as.character(start_ymd))),
         end_date=ymd(ifelse(is.na(end_ymd),as.character(end_dmy),
                               as.character(end_ymd)))) %>% 
  select(.,-c("start_ymd","start_dmy","end_ymd","end_dmy"))

#Chart 1: Number of open referrals

open_referrals_data <- MHSDS_main_pooled %>%
  filter(MEASURE_ID=="CYP32") %>%
  filter(BREAKDOWN=="England") %>%
  select(.,start_date,end_date,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE) %>% 
  mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
  arrange(.,start_date) %>%
  as_tibble()

open_referrals_chart <- open_referrals_data %>%
  ggplot(., aes(x=start_date, y=MEASURE_VALUE, group=PRIMARY_LEVEL_DESCRIPTION)) +
  geom_line(aes(color=PRIMARY_LEVEL_DESCRIPTION),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Number of open referrals CAMHS") +
  theme_ipsum() +
  xlab("Date") +
  ylab("Number of open referrals") +
  labs(col="Breakdown") +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.border = element_blank(),
        legend.text=element_text(size=8),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))
  
ggplotly(open_referrals_chart)

#Chart 2: Number of people in contact with CAMHS

number_contacts_data <- MHSDS_main_pooled %>%
  filter(MEASURE_ID=="CYP01") %>%
  filter(BREAKDOWN=="England") %>%
  select(.,start_date,end_date,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE) %>% 
  mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
  arrange(.,start_date) %>%
  as_tibble()

number_contacts_chart <- number_contacts_data %>%
  ggplot(., aes(x=start_date, y=MEASURE_VALUE, group=PRIMARY_LEVEL_DESCRIPTION)) +
  geom_line(aes(color=PRIMARY_LEVEL_DESCRIPTION),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Number of contacts with CAMHS") +
  theme_ipsum() +
  xlab("Date") +
  ylab("Number of contacts") +
  labs(col="Breakdown") +
  scale_color_brewer(palette = "Set1") +
  theme(panel.border = element_blank(),
        legend.text=element_text(size=8),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(number_contacts_chart)

# +
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.ticks.x=element_blank(),
#         legend.text=element_text(size=8),
#         text = element_text(size = 7),
#         axis.text = element_text(size = 7),
#         axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

##################################################
################ EXPLORE ED FILE #################
##################################################

MHSDS_ED_pooled <- fread(paste0(rawdatadir,ed_name,"/Pooled/MHSDS_ED_pooled.csv"),
                           header=TRUE, sep=",", check.names=T)

#Names of measures
ed_cyp_measures <- MHSDS_ED_pooled %>%
  pull(MEASURE_NAME) %>%
  unique(.)

#Clean up dates
MHSDS_ED_pooled <- MHSDS_ED_pooled %>%
  mutate(.,start_date=lubridate::dmy(REPORTING_PERIOD_START),
         end_date=lubridate::dmy(REPORTING_PERIOD_END))

#Chart 3: Number of people waiting for treatment

number_waiting_data <- MHSDS_ED_pooled %>%
  filter(MEASURE_ID=="ED88") %>%
  filter(BREAKDOWN=="England") %>%
  select(.,start_date,end_date,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE) %>% 
  mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
  arrange(.,start_date) %>%
  as_tibble()

number_waiting_chart <- number_waiting_data %>%
  ggplot(., aes(x=start_date, y=MEASURE_VALUE, group=PRIMARY_LEVEL_DESCRIPTION)) +
  geom_line(aes(color=PRIMARY_LEVEL_DESCRIPTION),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Number of referrals for eating disorders waiting for treatment") +
  theme_ipsum() +
  xlab("Date") +
  ylab("Number of referrals") +
  labs(col="Breakdown") +
  scale_color_brewer(palette = "Set2") +
  theme(panel.border = element_blank(),
        legend.text=element_text(size=8),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(number_waiting_chart)

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