library(tidyverse)
library(dbplyr)
library(dplyr)

responses <- read.csv("multipleChoiceResponses.csv", header = TRUE)
filtered <- responses %>% dplyr::select(WorkToolsSelect,LanguageRecommendationSelect,EmployerIndustry,WorkAlgorithmsSelect)

tools <- filtered %>% mutate(work_tool = str_split(filtered$WorkToolsSelect,",")) %>% unnest(work_tool) 
head(tool_count,6)

tool_count <- tools %>% 
  group_by(work_tool) %>% 
  summarise(each_tool_count = n()) %>%
  arrange(desc(each_tool_count))

#remove the row with empty value
tool_count <- tool_count[tool_count$work_tool!='',]

#Visualize the popularity of all the tools 
ggplot(data=tool_count) +
  geom_bar(aes(x=work_tool, y=each_tool_count), stat = "identity") +
  labs(
    x = "Work Tools",
    y = "Count"
  )


#if work_tool contains R/Python/both/neither on a new preference column 
#use the filtered data because you now want R and python before unnesting
filtered <- filtered[(filtered$WorkToolsSelect!=""),]
filtered <- filtered %>%
  mutate(language_preference = case_when(str_detect(WorkToolsSelect, "R") & !str_detect(WorkToolsSelect, "Python")~"R",
                                         str_detect(WorkToolsSelect, "Python") &! str_detect(WorkToolsSelect, "R")~"Python",
                                         str_detect(WorkToolsSelect, "R|Python")~"both",
                                         TRUE~"neither"
                                         ))

#remove the neither column and find the number of each preferred language
debate_plot <- filtered %>% 
  group_by(language_preference) %>%
  summarise(count = n()) %>%
  filter(language_preference != "neither")

ggplot(debate_plot) + 
  geom_bar(aes(x=language_preference,y=count), stat = 'identity') +
  labs(x= "Language",
       y = "Popularity"
      )

#this returns the count of of each group (grouped by the 2 factors) 
#n() returns the number of rows of the same group 
#row_number() with mutate returns the row number for each group(one group contains 4 element)
recommendations <- filtered  %>% 
  group_by(language_preference,LanguageRecommendationSelect) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  mutate(row = row_number()) %>%
  filter(row <= 4)


#facet_wrap(~Language) will set the facet wrap on top of the plot
recommendations <- recommendations[(recommendations$LanguageRecommendationSelect != ''),]
ggplot(recommendations) +
  geom_bar(aes(x=language_preference, y=count), stat = 'identity') +
  facet_wrap(~LanguageRecommendationSelect)





  






