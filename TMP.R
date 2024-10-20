library(tidyverse)


table.in   <-  read_csv("data/StateSpaceTest/HamazakiAppOutputs_ReportTableSource.csv",
                        locale=locale(encoding="latin1"))  %>%
  dplyr::filter(Stock == "Kwinag") %>% select(-Stock) %>%
  mutate_at(4:6,~ prettyNum(.x,big.mark=" "))

 table.in
str( table.in)






abd.bm.tab.src <- read_csv("data/SummaryTables_AbundanceBM.csv") %>% select(-Stock)

table.in <-  abd.bm.tab.src %>% dplyr::filter(Aggregate=="Nass",Scenario == "LTAvg",BM == "Smsy") %>% select(-Aggregate,-Scenario,-BM) %>%
  mutate_if(is.numeric,function(x){prettyNum(round(x), big.mark=" ")}) %>%
  dplyr::rename(Étiquette = Label, Moyenne = Mean,	Médiane = Median)

table.in$Étiquette <- str_replace_all(table.in$Étiquette, c( "Sum" = "Somme",  "Agg Fit" ="Ajus Regr", "Sum-Agg" = "Somme - Ajus Regr"))

table.in




umsy.bm.tab.src <- read_csv("data/SummaryTables_UMSY.csv")

table.in <-  umsy.bm.tab.src %>% dplyr::filter(Aggregate=="SkeenaWild",Scenario == "LTAvg",BM == "Umsy.c") %>%
  select(-Aggregate,-Scenario,-BM,-Stock) %>%
  mutate_if(is.numeric,function(x){prettyNum(round(x), big.mark=" ")})

table.in[table.in == "NA"] <- "-"

table.in$Label <- gsub("Agg Fit", "Ajus Regr",  table.in$Label)
col.names.use <- c("Étiquette","Moyenne","Médiane","p10","p25","p75","p90")

table.in
