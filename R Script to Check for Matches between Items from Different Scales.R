#-------------------------Description of This Script----------------------------

#This script identifies the most commonly used wording of each item in each
#scale identified in data we are planning to harmonise. This script then saves
#the most commonly used wording of each item of each scale in a list (one list
#per scale). These lists are then used to run functions from the harmonydata
#package to check for matches between items from different scales (i.e., items
#with a high level of semantic similarity/correspondence). Indices of similarity
#between item pairs and identified matches between items are saved in lists,
#matrices or data frames, and are then exported as csv files.

#----------------------Preparation and Loading Packages-------------------------
#Un-comment and run the below lines of code if these packages are not already 
#installed in R:
#install.packages("devtools")
#devtools::install_github("harmonydata/harmony_r")
#install.packages("dplyr")
#install.packages("readxl") 

#Note: Additional packages would be required for data not in xlsx format

#Load the required R libraries/packages:
library(devtools)
library(harmonydata)
library(dplyr)
library(readxl)
harmonydata::set_url()

#----------------------Set Working Drive, Load Data-----------------------------

#Set the working drive/file location R will use to find data files. E.g.:
setwd("~/Data Harmonisation Project/Content Analysis")

#Load the data and save to a dataframe object in the R environment:
dat1 <- read_xlsx("Survey Items.xlsx") #The xlsx file is a list of survey items.

#Add an ID column to the dataframe:
dat1$id<-as.character(seq(1,nrow(dat1)))

#-----------K10: Identify Most Commonly Used Wording, Save in List--------------

#The below code identifies the most commonly used wording of each item of the K10
#in the list of survey items in the dat1 dataframe. This code is useful for 
#identifying the most commonly used wording of an item in instances where 
#multiple surveys/datasets to be harmonised contained the same scale, but the 
#wording of each item in each scale may have varied slightly across surveys).

#Note: Because items from the K6 and K5 are identical or nearly identical to 
#the corresponding items in the K10, the K6 and K5 versions of each K10 item
#will be included when identifying the most common wording of each item:


freqK101i <- dat1 %>%  #Start with dataframe 'dat1'
  filter(Scale == "K10" & `Item Number` == 1) %>%  #Filter rows where Scale is "K10" and Item Number is 1
  group_by(Items) %>%  #Group by the 'Items' column
  summarise(total = n())  #Count the number of rows in each group and store the results in a column called 'total' in an object called freqK101i

#Repeat the above for each item in the scale:
freqK102i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==2) | (Scale=="K6" & `Item Number`==1) | (Scale=="K5" & `Item Number`==1)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK103i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==3)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK104i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==4) | (Scale=="K6" & `Item Number`==2) | (Scale=="K5" & `Item Number`==2)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK105i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==5) | (Scale=="K6" & `Item Number`==3) | (Scale=="K5" & `Item Number`==3)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK106i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==6)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK107i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==7)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK108i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==8) | (Scale=="K6" & `Item Number`==4) | (Scale=="K5" & `Item Number`==4)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK109i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==9) | (Scale=="K6" & `Item Number`==5) | (Scale=="K5" & `Item Number`==5)) %>%
  group_by(Items) %>%
  summarise(total=n())

freqK1010i<- dat1 %>%
  filter((Scale=="K10" & `Item Number`==10) | (Scale=="K6" & `Item Number`==6)) %>%
  group_by(Items) %>%
  summarise(total=n())

#Create a list of the most commonly used wordings of the K10 items:
K10 <- list(
  instrument_name = "K10",
  questions = list(
    list(question_no = "K10_1", question_text="In the past four weeks, about how often did you feel tired out for no good reason?"),
    list(question_no = "K10_2", question_text="In the past four weeks, about how often did you feel nervous?"),
    list(question_no = "K10_3", question_text="In the past four weeks, about how often did you feel so nervous that nothing could calm you down?"),
    list(question_no = "K10_4", question_text="In the past four weeks, about how often did you feel hopeless?"),
    list(question_no = "K10_5", question_text="In the past four weeks, about how often did you feel restless or fidgety?"),
    list(question_no = "K10_6", question_text="In the past four weeks, about how often did you feel so restless you could not sit still?"),
    list(question_no = "K10_7", question_text="In the past four weeks, about how often did you feel depressed?"),
    list(question_no = "K10_8", question_text="In the past four weeks, about how often did you feel that everything was an effort?"),
    list(question_no = "K10_9", question_text="In the past four weeks, about how often did you feel so sad that nothing could cheer you up?"),
    list(question_no = "K10_10", question_text="In the past four weeks, about how often did you feel worthless?")
  )
)

#Alternatively, the below code will create a function to automatically identify 
#the most commonly used wordings of K10 items, including matching items from K6 
#and K5, and then store them in a list:

#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(
      (Scale == "K10" & `Item Number` == item_number) |
        (Scale == "K6" & `Item Number` == ifelse(item_number == 2, 1,  # K10_2 -> K6_1
                                                 ifelse(item_number == 4, 2,  # K10_4 -> K6_2
                                                        ifelse(item_number == 5, 3,  # K10_5 -> K6_3
                                                               ifelse(item_number == 8, 4,  # K10_8 -> K6_4
                                                                      ifelse(item_number == 9, 5,  # K10_9 -> K6_5
                                                                             ifelse(item_number == 10, 6, #K10_10 -> K6_6
                                                                                    NA))))))) |
        (Scale == "K5" & `Item Number` == ifelse(item_number == 2, 1,  # K10_2 -> K5_1
                                                 ifelse(item_number == 4, 2,  # K10_4 -> K5_2
                                                        ifelse(item_number == 5, 3,  # K10_5 -> K5_3
                                                               ifelse(item_number == 8, 4,  # K10_8 -> K5_4
                                                                      ifelse(item_number == 9, 5,  # K10_9 -> K5_5
                                                                             NA))))))) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate a list which will store the most common wording of 
#each item:
K10 <- list(
  instrument_name = "K10",
  questions = lapply(1:10, function(i) {
    list(
      question_no = paste0("K10_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#----------MHI-5: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the wording most commonly used for the 5 items of the 
#SF-36 that make up the MHI-5.
#Note: The MHI-5 is comprised of items 24, 25, 26, 28 and 30 of the SF-36.

freqmhi501i<- dat1 %>%
  filter(Scale=="SF-36" & `Item Number`==24) %>%
  group_by(Items) %>%
  summarise(total=n())

freqmhi502i<- dat1 %>%
  filter(Scale=="SF-36" & `Item Number`==25) %>%
  group_by(Items) %>%
  summarise(total=n())

freqmhi503i<- dat1 %>%
  filter(Scale=="SF-36" & `Item Number`==26) %>%
  group_by(Items) %>%
  summarise(total=n())

freqmhi504i<- dat1 %>%
  filter(Scale=="SF-36" & `Item Number`==28) %>%
  group_by(Items) %>%
  summarise(total=n())

freqmhi505i<- dat1 %>%
  filter(Scale=="SF-36" & `Item Number`==30) %>%
  group_by(Items) %>%
  summarise(total=n())

#Manually storing the most commonly used wordings of the MHI-5 items in a list:

mhi5 <- list(
  instrument_name = "MHI-5",
  questions = list(
    list(question_no = "MHI5_1", question_text="These questions are about how you feel and how things have been with you during the past 4 weeks. For each question, please give the one answer that comes closest to the way you have been feeling. How much of the time during the past 4 weeks: Have you been a nervous person?"),
    list(question_no = "MHI5_2", question_text="These questions are about how you feel and how things have been with you during the past 4 weeks. For each question, please give the one answer that comes closest to the way you have been feeling. How much of the time during the past 4 weeks: Have you felt so down in the dumps that nothing could cheer you up?"),
    list(question_no = "MHI5_3", question_text="These questions are about how you feel and how things have been with you during the past 4 weeks. For each question, please give the one answer that comes closest to the way you have been feeling. How much of the time during the past 4 weeks: Have you felt calm and peaceful?"),
    list(question_no = "MHI5_4", question_text="These questions are about how you feel and how things have been with you during the past 4 weeks. For each question, please give the one answer that comes closest to the way you have been feeling. How much of the time during the past 4 weeks: Have you felt down?"),
    list(question_no = "MHI5_5", question_text="These questions are about how you feel and how things have been with you during the past 4 weeks. For each question, please give the one answer that comes closest to the way you have been feeling. How much of the time during the past 4 weeks: Have you been a happy person?")
  )
)

#The below code will create a function to automatically identify the most 
#commonly used wordings of MHI-5 items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SF-36" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
MHI5 <- list(
  instrument_name = "MHI-5",
  questions = lapply(c(24,25,26,28,30), function(i) {
    list(
      question_no = paste0("MHI-5_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#--------SF-36 MCS: Identify Most Commonly Used Wording, Save in List-----------

#The below will identify the wording most commonly used for the SF-36 items
#that are focused on mental health but excluding items focused on physical
#health.
#Note: Items include 17, 18, 19, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32.

freqsf3617i<- dat1 %>%
  filter(Scale=="SF-36" & `Item Number`==17) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsf3618i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 18) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3619i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 19) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3620i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 20) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3623i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 23) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3624i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 24) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3625i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 25) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3626i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 26) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3627i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 27) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3628i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 28) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3629i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 29) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3630i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 30) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3631i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 31) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf3632i <- dat1 %>%
  filter(Scale == "SF-36" & `Item Number` == 32) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SF-36 mental health items and store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SF-36" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
SF36 <- list(
  instrument_name = "SF-36",
  questions = lapply(c(17,18,19,20,23,24,25,26,27,28,29,30,31,32), function(i) {
    list(
      question_no = paste0("SF36_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#----------SF-12: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each SF-12 item and
#store them in individual objects for inspection to make sure the survey items
#are all coded correctly:

freqsf121i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf122i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf123i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf124i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf125i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf126i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf127i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf128i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf129i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf1210i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf1211i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf1212i <- dat1 %>%
  filter(Scale == "SF-12" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SF-12 items and then store them in a list.
#Note, this code will only do this for the mental health focused items
#from the SF-12.
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SF-12" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
SF12 <- list(
  instrument_name = "SF-12",
  questions = lapply(c(6,7,9,10,11,12), function(i) {
    list(
      question_no = paste0("SF-12_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------SF-8: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each SF-8 item and
#store them in individual objects for inspection to make sure the survey items
#are all coded correctly:

freqsf81i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf82i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf83i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf84i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf85i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf86i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf87i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsf88i <- dat1 %>%
  filter(Scale == "SF-8" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SF-8 items and then store them in a list.
#Note, this code will only do this for the mental health focused items
#from the SF-8.
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SF-8" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
SF8 <- list(
  instrument_name = "SF-8",
  questions = lapply(5:8, function(i) {
    list(
      question_no = paste0("SF-8_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#----------GHQ-12: Identify Most Commonly Used Wording, Save in List------------

#The below will identify the most commonly used wording of each GHQ-12 item and
#store them in individual objects for inspection to make sure the survey items
#are all coded correctly:

freqghq121i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==1) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq122i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==2) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq123i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==3) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq124i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==4) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq125i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==5) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq126i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==6) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq127i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==7) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq128i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==8) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq129i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==9) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq1210i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==10) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq1211i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==11) %>%
  group_by(Items) %>%
  summarise(total=n())

freqghq1212i<- dat1 %>%
  filter(Scale=="GHQ-12" & `Item Number`==12) %>%
  group_by(Items) %>%
  summarise(total=n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of GHQ-12 items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "GHQ-12" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}
#Second create and populate the list which will store the most common items:
GHQ12 <- list(
  instrument_name = "GHQ-12",
  questions = lapply(1:12, function(i) {
    list(
      question_no = paste0("GHQ12_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------GAD7: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each GAD7 item:

freqgad71i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==1) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgad72i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==2) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgad73i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==3) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgad74i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==4) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgad75i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==5) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgad76i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==6) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgad77i<- dat1 %>%
  filter(Scale=="GAD-7" & `Item Number`==7) %>%
  group_by(Items) %>%
  summarise(total=n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of GAD-7 items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "GAD-7" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
GAD7 <- list(
  instrument_name = "GAD-7",
  questions = lapply(1:7, function(i) {
    list(
      question_no = paste0("GAD7_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------PHQ9: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each PHQ-9 item:

freqphq91i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==1) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq92i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==2) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq93i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==3) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq94i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==4) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq95i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==5) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq96i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==6) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq97i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==7) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq98i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==8) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq99i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==9) %>%
  group_by(Items) %>%
  summarise(total=n())

freqphq910i<- dat1 %>%
  filter(Scale=="PHQ-9" & `Item Number`==10) %>%
  group_by(Items) %>%
  summarise(total=n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of PHQ-9 items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "PHQ-9" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
PHQ9 <- list(
  instrument_name = "PHQ-9",
  questions = lapply(1:10, function(i) {
    list(
      question_no = paste0("PHQ-9_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#----------PHQ-9M: Identify Most Commonly Used Wording, Save in List------------

#The below will identify the most commonly used wording of each PHQ-9M 
#(Modified for Teens) item and store them in individual objects for inspection 
#to make sure the survey items are all coded correctly:

freqphq9m1i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m2i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m3i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m4i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m5i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m6i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m7i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m8i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m9i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqphq9m10i <- dat1 %>%
  filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of PHQ-9M items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "PHQ-9 Modified (PHQ-9M)" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
PHQ9M <- list(
  instrument_name = "PHQ-9M",
  questions = lapply(1:10, function(i) {
    list(
      question_no = paste0("PHQ-9M_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------CES-D: Identify Most Commonly Used Wording, Save in List------------

#The below will identify the most commonly used wording of each CES-D item and
#store them in individual objects for inspection to make sure the survey items
#are all coded correctly:

freqcesd1i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==1) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd2i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==2) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd3i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==3) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd4i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==4) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd5i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==5) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd6i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==6) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd7i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==7) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd8i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==8) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd9i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==9) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd10i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==10) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd11i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==11) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd12i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==12) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd13i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==13) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd14i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==14) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd15i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==15) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd16i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==16) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd17i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==17) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd18i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==18) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd19i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==19) %>%
  group_by(Items) %>%
  summarise(total=n())

freqcesd20i<- dat1 %>%
  filter(Scale=="CES-D" & `Item Number`==20) %>%
  group_by(Items) %>%
  summarise(total=n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of CES-D items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "CES-D" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}
#Second create and populate the list which will store the most common items:
CESD <- list(
  instrument_name = "CES-D",
  questions = lapply(1:20, function(i) {
    list(
      question_no = paste0("CESD_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#------------GADS: Identify Most Commonly Used Wording, Save in List------------

#The below will identify the most commonly used wording of each GADS item and
#store them in individual objects:

freqgads1i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==1) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads2i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==2) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads3i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==3) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads4i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==4) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads5i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==5) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads6i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==6) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads7i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==7) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads8i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==8) %>%
  group_by(Items) %>%
  summarise(total=n())

freqgads9i<- dat1 %>%
  filter(Scale=="GADS" & `Item Number`==9) %>%
  group_by(Items) %>%
  summarise(total=n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of GADS items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "GADS" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}
#Second create and populate the list which will store the most common items:
GADS <- list(
  instrument_name = "GADS",
  questions = lapply(1:9, function(i) {
    list(
      question_no = paste0("GADS", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#----------SMFQ: Identify Most Commonly Used Wording, Save in List--------------

#The below will identify the most commonly used wording of each SMFQ 
#item and store them in individual objects:

freqsmfq1i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq2i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq3i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq4i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq5i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq6i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq7i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq8i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq9i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq10i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq11i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq12i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqsmfq13i <- dat1 %>%
  filter(Scale == "SMFQ" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SMFQ items and then store them in a list:
#First create the function:

get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SMFQ" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

# Second create and populate the list which will store the most common items:
SMFQ <- list(
  instrument_name = "SMFQ",
  questions = lapply(1:13, function(i) {
    list(
      question_no = paste0("SMFQ_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#------SDQ - Emotional: Identify Most Commonly Used Wording, Save in List-------

#The below will identify the most commonly used wording of each item from the 
#SDQ Emotional Symptoms subscale and store them in individual objects.

freqsdq1i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==1) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq2i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==2) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq3i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==3) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq4i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==4) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq5i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==5) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq6i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==6) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq7i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==7) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq8i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==8) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq9i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==9) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq10i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==10) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq11i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==11) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq12i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==12) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq13i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==13) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq14i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==14) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq15i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==15) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq16i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==16) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq17i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==17) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq18i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==18) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq19i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==19) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq20i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==20) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq21i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==21) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq22i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==22) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq23i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==23) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq24i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==24) %>%
  group_by(Items) %>%
  summarise(total=n())

freqsdq25i<- dat1 %>%
  filter(Scale=="SDQ" & `Item Number`==25) %>%
  group_by(Items) %>%
  summarise(total=n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SDQ items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SDQ" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}
#Second create and populate the list which will store the most common items.
#Note that the list will only store the most common wordings of items from the
#Emotional Symptoms sub-scale:
SDQ <- list(
  instrument_name = "SDQ",
  questions = lapply(c(3, 8, 13, 16, 24), function(i) {
    list(
      question_no = paste0("SDQ_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#---------GDS-SF: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each GDS-SF item and
#store them in individual objects:

freqgdssf1i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf2i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf3i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf4i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf5i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf6i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf7i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf8i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf9i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf10i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf11i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf12i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf13i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf14i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 14) %>%
  group_by(Items) %>%
  summarise(total = n())

freqgdssf15i <- dat1 %>%
  filter(Scale == "GDS-SF" & `Item Number` == 15) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of GDS-SF items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "GDS-SF" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}
#Second create and populate the list which will store the most common items:
GDSSF <- list(
  instrument_name = "GDS-SF",
  questions = lapply(1:15, function(i) {
    list(
      question_no = paste0("GDSSF_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------DQ5: Identify Most Commonly Used Wording, Save in List--------------

#The below will identify the most commonly used wording of each DQ5 item and
#store them in individual objects:

freqdq51i <- dat1 %>%
  filter(Scale == "DQ5" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdq52i <- dat1 %>%
  filter(Scale == "DQ5" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdq53i <- dat1 %>%
  filter(Scale == "DQ5" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdq54i <- dat1 %>%
  filter(Scale == "DQ5" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdq55i <- dat1 %>%
  filter(Scale == "DQ5" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of DQ5 items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "DQ5" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}
#Second create and populate the list which will store the most common items:
DQ5 <- list(
  instrument_name = "DQ5",
  questions = lapply(1:5, function(i) {
    list(
      question_no = paste0("DQ5_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#---------PedsQL: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each PedsQL item and
#store them in individual objects:

freqpedsql1i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql2i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql3i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql4i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql5i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql6i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql7i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql8i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql9i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql10i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql11i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql12i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql13i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql14i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 14) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql15i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 15) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql16i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 16) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql17i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 17) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql18i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 18) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql19i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 19) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql20i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 20) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql21i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 21) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql22i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 22) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsql23i <- dat1 %>%
  filter(Scale == "PedsQL" & `Item Number` == 23) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of PedsQL items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "PedsQL" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
PedsQL <- list(
  instrument_name = "PedsQL",
  questions = lapply(1:23, function(i) {
    list(
      question_no = paste0("PedsQL_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#--------PedsQL GWS: Identify Most Commonly Used Wording, Save in List----------

#The below will identify the most commonly used wording of each PedsQL GWS item 
#and store them in individual objects:

freqpedsqlgws1i <- dat1 %>%
  filter(Scale == "PedsQL GWS" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsqlgws2i <- dat1 %>%
  filter(Scale == "PedsQL GWS" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsqlgws3i <- dat1 %>%
  filter(Scale == "PedsQL GWS" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsqlgws4i <- dat1 %>%
  filter(Scale == "PedsQL GWS" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsqlgws5i <- dat1 %>%
  filter(Scale == "PedsQL GWS" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqpedsqlgws6i <- dat1 %>%
  filter(Scale == "PedsQL GWS" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of PedsQL GWS items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "PedsQL GWS" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
PedsQLGWS <- list(
  instrument_name = "PedsQL GWS",
  questions = lapply(1:6, function(i) {
    list(
      question_no = paste0("PedsQLGWS_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------DASS: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each DASS item and
#store them in individual objects:

freqdass1i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass2i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass3i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass4i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass5i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass6i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass7i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass8i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass9i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass10i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass11i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass12i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass13i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass14i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 14) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass15i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 15) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass16i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 16) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass17i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 17) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass18i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 18) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass19i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 19) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass20i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 20) %>%
  group_by(Items) %>%
  summarise(total = n())

freqdass21i <- dat1 %>%
  filter(Scale == "DASS" & `Item Number` == 21) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of DASS items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "DASS" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
DASS <- list(
  instrument_name = "DASS",
  questions = lapply(1:21, function(i) {
    list(
      question_no = paste0("DASS_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#---------HBSC-SCL: Identify Most Commonly Used Wording, Save in List-----------

#The below will identify the most commonly used wording of each HBSC-SCL item 
#and store them in individual objects:

freqhbscscl1i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl2i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl3i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl4i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl5i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl6i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl7i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqhbscscl8i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of HBSC-SCL items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "HBSC-SCL" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
HBSCSCL <- list(
  instrument_name = "HBSC-SCL",
  questions = lapply(1:8, function(i) {
    list(
      question_no = paste0("HBSC-SCL_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----SCAS (10-14 yrs): Identify Most Commonly Used Wording, Save in List-------

#The below will identify the most commonly used wording of each SCAS (10-14yrs) 
#item and store them in individual objects:

freqscas101i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas102i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas103i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas104i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas105i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas106i <- dat1 %>%
  filter(Scale == "HBSC-SCL" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SCAS (10-14yrs) items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SCAS - 10-14 years" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
SCAS_10to14years <- list(
  instrument_name = "SCAS - 10-14 years",
  questions = lapply(1:6, function(i) {
    list(
      question_no = paste0("SCAS_10-14years_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----SCAS_15to17years: Identify Most Commonly Used Wording, Save in List-------

#The below will identify the most commonly used wording of each SCAS - 15-17 yrs 
#item and store them in individual objects:

freqscas1517_1i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_2i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_3i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_4i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_5i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_6i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_7i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_8i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_9i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_10i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_11i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqscas1517_12i <- dat1 %>%
  filter(Scale == "SCAS - 15-17 years" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of SCAS - 15-17 years items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "SCAS - 15-17 years" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
SCAS_15to17years <- list(
  instrument_name = "SCAS - 15-17 years",
  questions = lapply(1:12, function(i) {
    list(
      question_no = paste0("SCAS_15to17years_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#----------RCMAS: Identify Most Commonly Used Wording, Save in List-------------

#The below will identify the most commonly used wording of each RCMAS 
#item and store them in individual objects:

freqrcmas1i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas2i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas3i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas4i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas5i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas6i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas7i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas8i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas9i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas10i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas11i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas12i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas13i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas14i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 14) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas15i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 15) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas16i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 16) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas17i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 17) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas18i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 18) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas19i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 19) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas20i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 20) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas21i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 21) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas22i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 22) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas23i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 23) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas24i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 24) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas25i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 25) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas26i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 26) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas27i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 27) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas28i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 28) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas29i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 29) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas30i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 30) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas31i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 31) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas32i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 32) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas33i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 33) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas34i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 34) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas35i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 35) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas36i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 36) %>%
  group_by(Items) %>%
  summarise(total = n())

freqrcmas37i <- dat1 %>%
  filter(Scale == "RCMAS" & `Item Number` == 37) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of RCMAS items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "RCMAS" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
RCMAS <- list(
  instrument_name = "RCMAS",
  questions = lapply(1:37, function(i) {
    list(
      question_no = paste0("RCMAS_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------YSR: Identify Most Commonly Used Wording, Save in List--------------

# The below will identify the most commonly used wording of each YSR 
# item and store them in individual objects:

freqysr1i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr2i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr3i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr4i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr5i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr6i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr7i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr8i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr9i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr10i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr11i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr12i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr13i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr14i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 14) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr15i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 15) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr16i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 16) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr17i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 17) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr18i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 18) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr19i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 19) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr20i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 20) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr21i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 21) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr22i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 22) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr23i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 23) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr24i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 24) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr25i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 25) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr26i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 26) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr27i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 27) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr28i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 28) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr29i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 29) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr30i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 30) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr31i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 31) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr32i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 32) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr33i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 33) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr34i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 34) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr35i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 35) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr36i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 36) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr37i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 37) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr38i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 38) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr39i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 39) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr40i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 40) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr41i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 41) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr42i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 42) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr43i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 43) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr44i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 44) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr45i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 45) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr46i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 46) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr47i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 47) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr48i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 48) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr49i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 49) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr50i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 50) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr51i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 51) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr52i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 52) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr53i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 53) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr54i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 54) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr55i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 55) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr56i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 56) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr57i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 57) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr58i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 58) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr59i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 59) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr60i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 60) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr61i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 61) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr62i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 62) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr63i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 63) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr64i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 64) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr65i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 65) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr66i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 66) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr67i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 67) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr68i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 68) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr69i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 69) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr70i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 70) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr71i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 71) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr72i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 72) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr73i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 73) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr74i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 74) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr75i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 75) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr76i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 76) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr77i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 77) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr78i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 78) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr79i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 79) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr80i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 80) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr81i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 81) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr82i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 82) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr83i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 83) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr84i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 84) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr85i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 85) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr86i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 86) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr87i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 87) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr88i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 88) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr89i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 89) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr90i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 90) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr91i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 91) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr92i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 92) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr93i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 93) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr94i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 94) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr95i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 95) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr96i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 96) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr97i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 97) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr98i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 98) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr99i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 99) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr100i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 100) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr101i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 101) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr102i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 102) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr103i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 103) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr104i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 104) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr105i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 105) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr106i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 106) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr107i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 107) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr108i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 108) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr109i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 109) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr110i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 110) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr111i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 111) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr112i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 112) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr113i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 113) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr114i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 114) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr115i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 115) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr116i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 116) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr117i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 117) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr118i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 118) %>%
  group_by(Items) %>%
  summarise(total = n())

freqysr119i <- dat1 %>%
  filter(Scale == "YSR" & `Item Number` == 119) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of YSR items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "YSR" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
YSR <- list(
  instrument_name = "YSR",
  questions = lapply(1:119, function(i) {
    list(
      question_no = paste0("YSR_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------CHQ: Identify Most Commonly Used Wording, Save in List--------------

#The below will identify the most commonly used wording of each CHQ 
#item and store them in individual objects:

freqchq1i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 1) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq2i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 2) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq3i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 3) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq4i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 4) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq5i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 5) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq6i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 6) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq7i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 7) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq8i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 8) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq9i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 9) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq10i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 10) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq11i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 11) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq12i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 12) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq13i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 13) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq14i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 14) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq15i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 15) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq16i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 16) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq17i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 17) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq18i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 18) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq19i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 19) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq20i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 20) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq21i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 21) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq22i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 22) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq23i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 23) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq24i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 24) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq25i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 25) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq26i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 26) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq27i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 27) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq28i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 28) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq29i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 29) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq30i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 30) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq31i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 31) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq32i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 32) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq33i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 33) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq34i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 34) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq35i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 35) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq36i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 36) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq37i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 37) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq38i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 38) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq39i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 39) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq40i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 40) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq41i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 41) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq42i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 42) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq43i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 43) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq44i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 44) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq45i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 45) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq46i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 46) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq47i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 47) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq48i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 48) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq49i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 49) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq50i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 50) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq51i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 51) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq52i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 52) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq53i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 53) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq54i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 54) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq55i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 55) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq56i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 56) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq57i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 57) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq58i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 58) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq59i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 59) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq60i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 60) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq61i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 61) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq62i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 62) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq63i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 63) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq64i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 64) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq65i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 65) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq66i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 66) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq67i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 67) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq68i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 68) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq69i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 69) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq70i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 70) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq71i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 71) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq72i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 72) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq73i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 73) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq74i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 74) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq75i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 75) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq76i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 76) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq77i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 77) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq78i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 78) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq79i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 79) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq80i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 80) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq81i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 81) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq82i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 82) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq83i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 83) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq84i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 84) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq85i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 85) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq86i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 86) %>%
  group_by(Items) %>%
  summarise(total = n())

freqchq87i <- dat1 %>%
  filter(Scale == "CHQ" & `Item Number` == 87) %>%
  group_by(Items) %>%
  summarise(total = n())

#The below code will create a function to automatically identify the most 
#commonly used wordings of CHQ items and then store them in a list:
#First create the function:
get_most_common_item_text <- function(item_number) {
  dat1 %>%
    filter(Scale == "CHQ" & `Item Number` == item_number) %>%
    group_by(Items) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(Items)
}

#Second create and populate the list which will store the most common items:
CHQ <- list(
  instrument_name = "CHQ",
  questions = lapply(1:87, function(i) {
    list(
      question_no = paste0("CHQ_", i),
      question_text = get_most_common_item_text(i)
    )
  })
)

#-----------------Harmony Tests for Matches Between Scales----------------------
#-----------Harmony Tests for Matches Between SF-12 and SF-36 MCS---------------

#The code below checks for matches between SF-12 mental health focused items and 
#SF-36 mental health focused items.

#Create a list containing the two instruments (SF-12 and SF-36)
instruments_list <- list(SF12, SF36)

#Use the match_instruments function and store results in an object called 'match':
match = match_instruments(instruments_list)

#Initialize a dataframe with the first match from the 'match' object:
df <- data.frame(match$matches[[1]]) 

#Create a loop:
for (x in 1:length(match$matches)) { #Loop over all matched instruments
  df[x, ] = match$matches[[x]] #Fill each row of 'df' with the corresponding match
}

#Update column and row names in the 'df' dataframe:
colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" ") ) #Set column names using the question number and text
rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" ") ) #Set row names using the question number and text

matrix_df <-as.matrix(df) #Convert 'df' to a matrix
logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70) #Create a logical matrix where values are TRUE if > 0.70 or < -0.70
subset_matrix <- matrix_df #Make a copy of the matrix
subset_matrix[!logical_matrix] <- NA #Set values not meeting the logical condition to NA
subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0] #Remove rows and columns with only NA values

#Write the filtered matrix to a CSV file:
write.csv(subset_matrix, "matches_sf12_sf36mcs.csv") 

matching_indices <- which(logical_matrix, arr.ind = TRUE) #Find the indices of the TRUE values in the logical matrix

variable_pairs <- data.frame( #Create a data frame with the matched variable pairs
  Var1 = rownames(matrix_df)[matching_indices[, 1]], #Get row names corresponding to the matching indices
  Var2 = colnames(matrix_df)[matching_indices[, 2]], # Get column names corresponding to the matching indices
  Value = matrix_df[matching_indices] #Get the matrix values corresponding to the matching indices
)

#Remove pairs where Var1 and Var2 are the same:
variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]

#Write the variable pairs to a CSV file to be exported to the workding directory
write.csv(variable_pairs, "variable_pairs_sf12_sf36mcs.csv")

#------------Harmony Tests for Matches Between SF-8 and SF-36 MCS---------------

#Checking for matches between SF-8 mental health focused items and SF-36
#mental health focused items.
instruments_list <- list(SF8, SF36)

match = match_instruments(instruments_list)

df <- data.frame(match$matches[[1]])
for (x in 1:length(match$matches)) {
  df[x, ] = match$matches[[x]]
}
colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" ") )
rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" ") )

matrix_df <-as.matrix(df)
logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
subset_matrix <- matrix_df
subset_matrix[!logical_matrix] <- NA
subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]

write.csv(subset_matrix, "matches_sf8_sf36mcs.csv")

matching_indices <- which(logical_matrix, arr.ind = TRUE)

variable_pairs <- data.frame(
  Var1 = rownames(matrix_df)[matching_indices[, 1]],
  Var2 = colnames(matrix_df)[matching_indices[, 2]],
  Value = matrix_df[matching_indices]
)

variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]

write.csv(variable_pairs, "variable_pairs_sf8_sf36mcs.csv")

#-----------Harmony Tests for Matches Between SMFQ and Other Scales-------------

scales_list <- c("K10", "SF8", "SF12", "SF36", "GHQ12", "GAD7", "PHQ9", "PHQ9M", 
                 "CESD", "GADS","SDQ", "GDSSF", "DQ5", "HBSCSCL", "PedsQL", 
                 "PedsQLGWS", "DASS", "SCAS_10to14years", "SCAS_15to17years", 
                 "RCMAS", "YSR", "CHQ")

#Define output directory
output_directory <- "~/Scoping Review/Content Analysis" 

#Loop through each scale to find matches
for (scale in scales_list) {
  instruments_list <- list(SMFQ, get(scale))  # Use get() to retrieve the actual scale object
  
  # Perform the match
  match = match_instruments(instruments_list)
  
  # Create a data frame from the matches
  df <- data.frame(match$matches[[1]])
  for (x in 1:length(match$matches)) {
    df[x, ] = match$matches[[x]]
  }
  
  # Set column and row names
  colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  
  # Convert to matrix
  matrix_df <- as.matrix(df)
  
  # Create a logical matrix for matches
  logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
  
  # Create a subset matrix with NA for non-matches
  subset_matrix <- matrix_df
  subset_matrix[!logical_matrix] <- NA
  subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]
  
  # Get the scale name for file naming
  scale_name <- as.character(scale)  # Convert scale to character for file naming
  
  # Write matches to CSV
  write.csv(subset_matrix, file.path(output_directory, paste0("matches_smfq_", scale_name, ".csv")), row.names = TRUE)
  
  # Find matching indices
  matching_indices <- which(logical_matrix, arr.ind = TRUE)
  
  # Create a data frame for variable pairs
  variable_pairs <- data.frame(
    Var1 = rownames(matrix_df)[matching_indices[, 1]],
    Var2 = colnames(matrix_df)[matching_indices[, 2]],
    Value = matrix_df[matching_indices]
  )
  
  # Remove self-pairs
  variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]
  
  # Write variable pairs to CSV
  write.csv(variable_pairs, file.path(output_directory, paste0("variable_pairs_smfq_", scale_name, ".csv")), row.names = FALSE)
}

#----------Harmony Tests for Matches Between PedsQL and Other Scales------------

scales_list <- c("K10","SF8", "SF12", "SF36","GHQ12","GAD7","PHQ9","PHQ9M","CESD",
                 "GADS","SMFQ","SDQ","GDSSF","DQ5","HBSCSCL","PedsQLGWS","DASS", 
                 "SCAS_10to14years","SCAS_15to17years","RCMAS","YSR","CHQ")

#Define output directory
output_directory <- "~/Scoping Review/Content Analysis" 

#Loop through each scale to find matches
for (scale in scales_list) {
  instruments_list <- list(PedsQL, get(scale))  # Use get() to retrieve the actual scale object
  
  # Perform the match
  match = match_instruments(instruments_list)
  
  # Create a data frame from the matches
  df <- data.frame(match$matches[[1]])
  for (x in 1:length(match$matches)) {
    df[x, ] = match$matches[[x]]
  }
  
  # Set column and row names
  colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  
  # Convert to matrix
  matrix_df <- as.matrix(df)
  
  # Create a logical matrix for matches
  logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
  
  # Create a subset matrix with NA for non-matches
  subset_matrix <- matrix_df
  subset_matrix[!logical_matrix] <- NA
  subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]
  
  # Get the scale name for file naming
  scale_name <- as.character(scale)  # Convert scale to character for file naming
  
  # Write matches to CSV
  write.csv(subset_matrix, file.path(output_directory, paste0("matches_pedsql_", scale_name, ".csv")), row.names = TRUE)
  
  # Find matching indices
  matching_indices <- which(logical_matrix, arr.ind = TRUE)
  
  # Create a data frame for variable pairs
  variable_pairs <- data.frame(
    Var1 = rownames(matrix_df)[matching_indices[, 1]],
    Var2 = colnames(matrix_df)[matching_indices[, 2]],
    Value = matrix_df[matching_indices]
  )
  
  # Remove self-pairs
  variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]
  
  # Write variable pairs to CSV
  write.csv(variable_pairs, file.path(output_directory, paste0("variable_pairs_pedsql_", scale_name, ".csv")), row.names = FALSE)
}

#--------Harmony Tests for Matches Between PedsQL GWS and Other Scales----------

scales_list <- c("K10","SF8","SF12","SF36","GHQ12","GAD7","PHQ9","PHQ9M","CESD",
                 "GADS","SMFQ","SDQ","GDSSF","DQ5","HBSCSCL","PedsQL","DASS", 
                 "SCAS_10to14years","SCAS_15to17years","RCMAS","YSR","CHQ")

#Define output directory
output_directory <- "~/Scoping Review/Content Analysis" 

#Loop through each scale to find matches
for (scale in scales_list) {
  instruments_list <- list(PedsQLGWS, get(scale))  # Use get() to retrieve the actual scale object
  
  # Perform the match
  match = match_instruments(instruments_list)
  
  # Create a data frame from the matches
  df <- data.frame(match$matches[[1]])
  for (x in 1:length(match$matches)) {
    df[x, ] = match$matches[[x]]
  }
  
  # Set column and row names
  colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  
  # Convert to matrix
  matrix_df <- as.matrix(df)
  
  # Create a logical matrix for matches
  logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
  
  # Create a subset matrix with NA for non-matches
  subset_matrix <- matrix_df
  subset_matrix[!logical_matrix] <- NA
  subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]
  
  # Get the scale name for file naming
  scale_name <- as.character(scale)  # Convert scale to character for file naming
  
  # Write matches to CSV
  write.csv(subset_matrix, file.path(output_directory, paste0("matches_pedsqlgws_", scale_name, ".csv")), row.names = TRUE)
  
  # Find matching indices
  matching_indices <- which(logical_matrix, arr.ind = TRUE)
  
  # Create a data frame for variable pairs
  variable_pairs <- data.frame(
    Var1 = rownames(matrix_df)[matching_indices[, 1]],
    Var2 = colnames(matrix_df)[matching_indices[, 2]],
    Value = matrix_df[matching_indices]
  )
  
  # Remove self-pairs
  variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]
  
  # Write variable pairs to CSV
  write.csv(variable_pairs, file.path(output_directory, paste0("variable_pairs_pedsqlgws_", scale_name, ".csv")), row.names = FALSE)
}

#-----------Harmony Tests for Matches Between SMFQ and Other Scales-------------

scales_list <- c("K10", "SF8", "SF12", "SF36", "GHQ12", "GAD7", "PHQ9", "PHQ9M", "CESD", "GADS",
                 "SDQ", "GDSSF", "DQ5", "HBSCSCL", "PedsQL", "PedsQLGWS", "DASS", 
                 "SCAS_10to14years", "SCAS_15to17years", "RCMAS", "YSR", "CHQ")

#Define output directory
output_directory <- "~/Scoping Review/Content Analysis" 

#Loop through each scale to find matches
for (scale in scales_list) {
  instruments_list <- list(SMFQ, get(scale))  # Use get() to retrieve the actual scale object
  
  # Perform the match
  match = match_instruments(instruments_list)
  
  # Create a data frame from the matches
  df <- data.frame(match$matches[[1]])
  for (x in 1:length(match$matches)) {
    df[x, ] = match$matches[[x]]
  }
  
  # Set column and row names
  colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  
  # Convert to matrix
  matrix_df <- as.matrix(df)
  
  # Create a logical matrix for matches
  logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
  
  # Create a subset matrix with NA for non-matches
  subset_matrix <- matrix_df
  subset_matrix[!logical_matrix] <- NA
  subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]
  
  # Get the scale name for file naming
  scale_name <- as.character(scale)  # Convert scale to character for file naming
  
  # Write matches to CSV
  write.csv(subset_matrix, file.path(output_directory, paste0("matches_smfq_", scale_name, ".csv")), row.names = TRUE)
  
  # Find matching indices
  matching_indices <- which(logical_matrix, arr.ind = TRUE)
  
  # Create a data frame for variable pairs
  variable_pairs <- data.frame(
    Var1 = rownames(matrix_df)[matching_indices[, 1]],
    Var2 = colnames(matrix_df)[matching_indices[, 2]],
    Value = matrix_df[matching_indices]
  )
  
  # Remove self-pairs
  variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]
  
  # Write variable pairs to CSV
  write.csv(variable_pairs, file.path(output_directory, paste0("variable_pairs_smfq_", scale_name, ".csv")), row.names = FALSE)
}

#---------Harmony Tests for Matches Between HBSCSCL and Other Scales------------

scales_list <- c("K10", "SF8", "SF12", "SF36", "GHQ12", "GAD7", "PHQ9", "PHQ9M", "CESD", "GADS", 
                 "SMFQ", "SDQ", "GDSSF", "DQ5", "PedsQL", "PedsQLGWS", "DASS", 
                 "SCAS_10to14years", "SCAS_15to17years", "RCMAS", "YSR", "CHQ")

#Define output directory
output_directory <- "~/Scoping Review/Content Analysis" 

#Loop through each scale to find matches
for (scale in scales_list) {
  instruments_list <- list(HBSCSCL, get(scale))  # Use get() to retrieve the actual scale object
  
  # Perform the match
  match = match_instruments(instruments_list)
  
  # Create a data frame from the matches
  df <- data.frame(match$matches[[1]])
  for (x in 1:length(match$matches)) {
    df[x, ] = match$matches[[x]]
  }
  
  # Set column and row names
  colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" "))
  
  # Convert to matrix
  matrix_df <- as.matrix(df)
  
  # Create a logical matrix for matches
  logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
  
  # Create a subset matrix with NA for non-matches
  subset_matrix <- matrix_df
  subset_matrix[!logical_matrix] <- NA
  subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]
  
  # Get the scale name for file naming
  scale_name <- as.character(scale)  # Convert scale to character for file naming
  
  # Write matches to CSV
  write.csv(subset_matrix, file.path(output_directory, paste0("matches_hbscscl_", scale_name, ".csv")), row.names = TRUE)
  
  # Find matching indices
  matching_indices <- which(logical_matrix, arr.ind = TRUE)
  
  # Create a data frame for variable pairs
  variable_pairs <- data.frame(
    Var1 = rownames(matrix_df)[matching_indices[, 1]],
    Var2 = colnames(matrix_df)[matching_indices[, 2]],
    Value = matrix_df[matching_indices]
  )
  
  # Remove self-pairs
  variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]
  
  # Write variable pairs to CSV
  write.csv(variable_pairs, file.path(output_directory, paste0("variable_pairs_hbscscl_", scale_name, ".csv")), row.names = FALSE)
}

#-------------Harmony Tests for Matches Between K10 and SF-36 MCS---------------

instruments_list <- list(K10, SF36)

match = match_instruments(instruments_list)

df <- data.frame(match$matches[[1]])
for (x in 1:length(match$matches)) {
  df[x, ] = match$matches[[x]]
}
colnames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" ") )
rownames(df) <- lapply(match$questions, function(x) paste(x$question_no,  x$question_text , sep=" ") )

matrix_df <-as.matrix(df)
logical_matrix <- (matrix_df > 0.70) | (matrix_df < -0.70)
subset_matrix <- matrix_df
subset_matrix[!logical_matrix] <- NA
subset_matrix <- subset_matrix[rowSums(!is.na(subset_matrix)) > 0, colSums(!is.na(subset_matrix)) > 0]

write.csv(subset_matrix, "matches_k10_sf36mcs.csv")

matching_indices <- which(logical_matrix, arr.ind = TRUE)

variable_pairs <- data.frame(
  Var1 = rownames(matrix_df)[matching_indices[, 1]],
  Var2 = colnames(matrix_df)[matching_indices[, 2]],
  Value = matrix_df[matching_indices]
)

variable_pairs <- variable_pairs[variable_pairs$Var1 != variable_pairs$Var2, ]

write.csv(variable_pairs, "variable_pairs_k10_sf36mcs.csv")
