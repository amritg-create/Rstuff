
#Section 1: Loading in the Data File and R libraries. 

#Reading in the libraries

library("readxl")
library("dplyr")
library("tidyverse")
library("writexl")
library(ggplot2)
library(lubridate)
library(tidyquant)
library(stringr)
library(tidyr)
library(anytime)

#Reading in the data file

data <- read_excel("C:/Users/amrigupt/Downloads/firstquarter2024ChangeEvents.xlsx")

data

#Glimpse of Data for easier viewing

data %>% glimpse()

#Looking at ALL CHANGE EVENTS.

#How many change events have happened in the last three months? Run the below code: 

data %>% 
    select(EVENT_NAME) %>%
    count() #This tells you how many change events there were over the last 3 months. 

#Looking at a count of each type of change event over the last three months in descending order: 

data %>%
    group_by(EVENT_NAME) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    ungroup()

#Looking at a count of each type of change event over the last 3 months grouped by hierarchy:

data %>%
    group_by(EVENT_NAME, CATEGORY_NAME) %>%
    summarize(count = n())%>%
    distinct %>%
    ungroup()


#Conversion of event_occur date to real date column in date format. 

data$EVENT_OCCUR_DATE %>% class()
data$real_date <- lubridate::mdy_hms(data$EVENT_OCCUR_DATE)
data$real_date <- as.Date(data$real_date)

#Count of all change events on a monthly basis:

data$month <- month(data$real_date, label = TRUE)
data %>% select(month, EVENT_NAME) %>%
    group_by(month, EVENT_NAME) %>%
    count()

#Section 2: Relationship Change Events Analysis

#Use the below code to output all the hierarchies that had new_relationship change events in the last 3 months:

data %>% 
    select(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'New_Relationship') %>%
    distinct()

#Use the below code to output a count in descending order of the hierarchies with New_Relationship change events: 

data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'New_Relationship') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

#Use the below code to generate a bar plot with a count for all hierarchies in the last 3 months with New_Relationship change events: 

new_relationship_tbl <- data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'New_Relationship') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

new_relationship_tbl %>% 
    
    select(CATEGORY_NAME, n) %>%
    
    mutate(CATEGORY_NAME = CATEGORY_NAME %>% as_factor() %>% fct_reorder(n))%>%
    
    ggplot(aes(CATEGORY_NAME, n)) + 
    
    geom_col(fill = "orange") +
    coord_flip() +
    geom_text(aes(label = n), vjust = 1.1, color = "black")+
    expand_limits(y=100000) +
    labs(title = "Number of New_Relationship Change Events", size = 2, y = "Number of Events", x = "Hierarchy")

#Use the below code to output all hierarchies that had Inactivate_Relationship change events in the last 3 months:

data %>% 
    select(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Inactivate_Relationship') %>%
    distinct()

#Use the below code to output a count in descending order of the hierarchies with Inactivate_Relationship change events: 

inactivate_relationship_tbl <- data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Inactivate_Relationship') %>%
    tally() %>%
    arrange(desc(n)) %>%
    distinct() %>%
    ungroup()

#Use the below code to generate a bar plot with a count for all hierarchies in the last 3 months with Inactivate_Relationship change events: 

inactivate_relationship_tbl %>% 
    
    select(CATEGORY_NAME, n) %>%
    
    mutate(CATEGORY_NAME = CATEGORY_NAME %>% as_factor() %>% fct_reorder(n))%>%
    
    ggplot(aes(CATEGORY_NAME, n)) + 
    
    geom_col(fill = "orange") +
    coord_flip() +
    geom_text(aes(label = n), vjust = 1.1, color = "black")+
    expand_limits(y=100000) +
    labs(title = "Number of Inactivate_Relationship Change Events", size = 2, y = "Number of Events", x = "Hierarchy")

#Section 3: Node Change Events Analysis

#Use the below code to output all the hierarchies that had new_node change events in the last 3 months:

data %>% 
    select(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'New_Node') %>%
    distinct()

#Use the below code to output a count in descending order of the hierarchies with New_Node change events: 

data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'New_Node') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

#Use the below code for visualization for New_node:

new_node_tbl <- data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'New_Node') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

new_node_tbl %>% 
    
    select(CATEGORY_NAME, n) %>%
    
    mutate(CATEGORY_NAME = CATEGORY_NAME %>% as_factor() %>% fct_reorder(n))%>%
    
    ggplot(aes(CATEGORY_NAME, n)) + 
    
    geom_col(fill = "orange") +
    coord_flip() +
    geom_text(aes(label = n), vjust = 1.1, color = "black")+
    expand_limits(y=100000) +
    labs(title = "Number of New_Node Change Events", size = 2, y = "Number of Events", x = "Hierarchy")

#Use the below code to output all hierarchies that had Inactivate_Node change events in the last 3 months:

data %>% 
    select(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Inactivate_Node') %>%
    distinct()
    
#Use the below code to output a count in descending order of the hierarchies with Inactivate_Node change events: 

data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Inactivate_Node') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

#Generate a visualization for Inactivate_Node events for all hierarchies:

inactivate_node_tbl <- data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Inactivate_Node') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

inactivate_node_tbl %>% 
    
    select(CATEGORY_NAME, n) %>%
    
    mutate(CATEGORY_NAME = CATEGORY_NAME %>% as_factor() %>% fct_reorder(n))%>%
    
    ggplot(aes(CATEGORY_NAME, n)) + 
    
    geom_col(fill = "orange") +
    coord_flip() +
    geom_text(aes(label = n), vjust = 1.1, color = "black")+
    expand_limits(y=100000) +
    labs(title = "Number of Inactivate_Node Change Events", size = 2, y = "Number of Events", x = "Hierarchy")

#Looking at update node statistics:

data %>% 
    select(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Update_Node') %>%
    distinct()

#Use the below code to output a count in descending order of the hierarchies with Inactivate_Node change events: 

data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Update_Node') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

#Generate a visualization for Inactivate_Node events for all hierarchies:

inactivate_node_tbl <- data %>%
    select(CATEGORY_NAME, EVENT_NAME) %>%
    group_by(CATEGORY_NAME, EVENT_NAME) %>%
    filter(EVENT_NAME == 'Inactivate_Node') %>%
    tally() %>%
    arrange(desc(n)) %>%
    ungroup()

inactivate_node_tbl %>% 
    
    select(CATEGORY_NAME, n) %>%
    
    mutate(CATEGORY_NAME = CATEGORY_NAME %>% as_factor() %>% fct_reorder(n))%>%
    
    ggplot(aes(CATEGORY_NAME, n)) + 
    
    geom_col(fill = "orange") +
    coord_flip() +
    geom_text(aes(label = n), vjust = 1.1, color = "black")+
    expand_limits(y=100000) +
    labs(title = "Number of Inactivate_Node Change Events", size = 2, y = "Number of Events", x = "Hierarchy")


#Section 4: Looking at user activity statistics. 

#Create Events User Statistics to see all users who had create related events in the last 3 months:

users <- data %>% 
    select(CREATED_BY) %>%
    filter(CREATED_BY != 'system') %>%
    distinct() %>%
    tally()


#Update Events User Statistics to see all users who had update related events in the last 3 months:

data %>% 
    select(UPDATED_BY) %>%
    filter(UPDATED_BY != 'system') %>%
    distinct() %>%
    tally()






    

