library(tidyverse)


table.in <- read_csv("data/AggregationApproachTables/AggregationTable_KeyChallenges.csv",
                     locale=locale(encoding="latin1"))

table.in$`Principaux défis`





abd.bm.tab.src <- read_csv("data/SummaryTables_AbundanceBM.csv") %>% select(-Stock)

table.in <-  abd.bm.tab.src %>% dplyr::filter(Aggregate=="Nass",Scenario == "LTAvg",BM == "Smsy") %>% select(-Aggregate,-Scenario,-BM) %>%
  mutate_if(is.numeric,function(x){prettyNum(round(x), big.mark=" ")}) %>%
  dplyr::rename(Étiquette = Label, Moyenne = Mean,	Médiane = Median)

table.in$Étiquette <- str_replace_all(table.in$Étiquette, c( "Sum" = "Somme",  "Agg Fit" ="Ajus Regr", "Sum-Agg" = "Somme - Ajus Regr"))

table.in
