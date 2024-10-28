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







data.frame(
  `Exécution` = c(seq(1,6), "7-24", "25", "26"),
  Description = c(
    "Scénario de référence du MBH, mais incluant le codage de Korman et English (2013) dans la précision de calcul de la fonction de vraisemblance de $\\sigma$",
    "Identique au scénario de référence du MBH, mais sans borne inférieure a priori sur $\\beta$.",
    "Identique au scénario de référence du MBH, mais sans tenir compte des effets annuels communs.",
    "Exécution de modèle non hiérarchique sans effet annuel commun, mais incluant les mêmes informations a priori de $S_\\textrm{max}$ que dans le scénario de référence du MBH.",
    "Identique au scénario de référence du MBH, mais valeurs a priori normales de $S_\\textrm{max}$ au lieu de la valeur a priori log-normale du scénario de référence sur le $\\beta$ de Ricker.",
    "Omettre les données sur le stock-recrutement dans le MBH, un stock à la fois.",
    "Omettre les données sur le stock-recrutement pour les stocks mis en valeur du lac Babine dans le MBH (ruisseau Pinkut et rivière Fulton).",
    "Identique au scénario de référence du MBH, mais omettre les stocks mis en valeur du lac Babine (ruisseau Pinkut et rivière Fulton) et appliquer des valeurs a priori vagues de $S_\\textrm{max}$ aux stocks des rivières Bear, Kitwanga et Sustut."
  ),
  check.names = FALSE
)





c(seq(1,6), "7-24", "25", "26")



library(tidyverse)
tbl.test <- read_csv("data/Reference Tables/Acronyms.csv")#
tbl.test |> kableExtra::kbl()



