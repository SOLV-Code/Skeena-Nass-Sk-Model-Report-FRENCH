library(tidyverse)


table.in   <-  read_csv("data/SummaryTables_RelAbdMetric_AnnualSummary_Nass.csv") %>%
  mutate_at(c(4,9:11),function(x){format(round(x,2),n.small =2,decimal.mark=",")})

 table.in







abd.bm.tab.src <- read_csv("data/SummaryTables_AbundanceBM.csv") %>% select(-Stock)

table.in <-  abd.bm.tab.src %>% dplyr::filter(Aggregate=="Nass",Scenario == "LTAvg",BM == "Smsy") %>% select(-Aggregate,-Scenario,-BM) %>%
  mutate_if(is.numeric,function(x){prettyNum(round(x), big.mark=" ")}) %>%
  dplyr::rename(Étiquette = Label, Moyenne = Mean,	Médiane = Median)

table.in$Étiquette <- str_replace_all(table.in$Étiquette, c( "Sum" = "Somme",  "Agg Fit" ="Ajus Regr", "Sum-Agg" = "Somme - Ajus Regr"))

table.in
