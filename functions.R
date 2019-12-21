###################################
#Functions & Constants
###################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,data.table,ggplot2,here,mailR,plotly)

root <- 'C:\\Users\\owenm\\Documents\\'
setwd(root)

options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
##################################
#To do
##################################
#Scrape the data from SSA website
#Include state-level data
#Package with make
#Stick on github
#Make a dashboard

##################################
#Functions
##################################

#Functions we need

#Gender ambiguity
#data <- data %>% group_by(Year, Name) %>% mutate(gap=max(Count) - min(Count))


#Starts with
name_starts_with <- function(name,n) {
  substr(name,1,n)
}

#Ends with
name_ends_with <- function(name,n) {
  substr(name,nchar(name)-n+1,nchar(name))
}

#Year by year change
change <- function(new,old) {
  delta <- (new-old)/old
  return(delta)
}

  #Plotting names
  name.graph <- function(names,year,sex="M") {
    data %>% 
      filter(Name %in% c(names) & Year>=year & Sex==sex) %>% group_by(Year) %>% 
      ungroup() %>% 
      plot_ly(.,x=~Year) %>% 
      add_trace(y=~Count,name="Count",type='scatter',mode="lines")
         
  }


#Is it in the top 50?
  is.top.50 <- function(name,year){
    result <- data %>% filter(Name %in% name & Year==year & Sex=="M") %>% 
      mutate(top50 = ifelse(counter<=50,"Yes","No"))
    print(result)
  
  }

#General query
name_query <- function(minyear,maxyear=2018,minrank,maxrank,endings,prefixes) { 
  data %>% ungroup() %>% 
    mutate(ending = name_ends_with(Name,2),
           prefix = name_starts_with(Name,1)) %>%  
    filter(Year>=minyear & Year <=maxyear
           & Sex=="M" 
           & counter < maxrank & counter>minrank & 
             !(ending %in% endings) &
             (prefix %in% prefixes)) %>% 
    distinct(Name)
}

#File Reader
file_reader <- function(year){
  
  file <- fread(paste0('~\\Names\\Raw Data\\names\\','yob',year,'.txt'),header=FALSE)
  colnames(file) <- c('Name','Sex','Count')
  file <- file %>% mutate( Year = year) 
  
  return(file)
}

#state File Reader
state_file_reader <- function(state){
  
  file <- fread(paste0('~\\Names\\Raw Data\\namesbystate\\',state),header=FALSE)
  colnames(file) <- c('State','Sex','Year','Name','Count')
  
  return(file)
}

 c(
      
                  'Rory',
                  'Cooper','Huck','Alden','Everett',
                  'Adrian','Ellison','Emerson','Cole','Duncan','Garrett',
                  'Graham','Griffin','Kyle','Leo',
                  'Cameron','Wesley','Zachary')

#Constants
sender <- "owenmccart@gmail.com"
recipients <- "cdecker3@gmail.com"
timespan <- seq(1900,2018)
states <- list.files(path = "C:\\Users\\owenm\\Downloads\\namesbystate\\",pattern="TXT")
bad <- c('en','on','ey','ie')
possibles <- c(
                  'Bailey','Silas','Lincoln', 'Colin',
                  'Spencer','Gene','Laird','Rory',
                  'Cooper','Damian','Alden','Everett',
                  'Adrian','Ellison','Joran',
                  'Jeffrey','Emerson','Cole','Duncan','Garrett',
                  'Graham','Graeme','Julian','Griffin','Reid','Conrad',
                  'Tristan','Kyle','Devin','Leo','Theodore',
                  'Cameron','Wesley','Kai','Zachary')

