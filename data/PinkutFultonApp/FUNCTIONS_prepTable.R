

prepTablePars <- function(stk.df,caption){
# stk.df is the data frame with all the output summary
# generates the table input

table.in <- stk.df %>% dplyr::filter(ReportLabel == "BR" & VarType %in% c("ln.alpha","beta","sigma")) %>%
					mutate(BrYr = "All", Label = "") %>%
					select(ModelType,VarType,BrYr, Label,p10,p25,p50,p75,p90) %>%
	        mutate_if(is.numeric, round,2)


if("AR1" %in% stk.df$ReportLabel){
	
table.in  <- table.in %>% bind_rows(stk.df	%>%
								dplyr::filter(ReportLabel == "AR1" & VarType %in% c("ln.alpha","beta","phi","sigma")) %>%
					mutate(BrYr = "All",Label = "") %>%
					select(ModelType,VarType,BrYr, Label, p10,p25,p50,p75,p90) %>%
	        mutate_if(is.numeric, round,2))

	
}

if("KF" %in% stk.df$ReportLabel){

kf.tmp.lna <- stk.df	%>% dplyr::filter(ReportLabel == "KF" & VarType %in% c("ln.alpha")) 
min.idx <- 		which(kf.tmp.lna$p50 == min(kf.tmp.lna$p50))
max.idx <- 		which(kf.tmp.lna$p50 == max(kf.tmp.lna$p50))
last4.idx <-  which(kf.tmp.lna$Yr %in% sort(max(kf.tmp.lna$Yr)-(0:3)))

kf.tmp.lna <- kf.tmp.lna[c(min.idx,max.idx,last4.idx),] %>%
								mutate(Label = c("Min a", "Max a", rep("Last 4",4))) %>%
								mutate(BrYr = as.character(Yr))


kf.tmp <- stk.df	%>%
					dplyr::filter(ReportLabel == "KF" & VarType %in% c("beta","sigma")) %>%
					mutate(Label = "") %>% mutate(BrYr = "All") %>%
					bind_rows(kf.tmp.lna) %>%
					select(ModelType,VarType,BrYr, Label, p10,p25,p50,p75,p90) %>%
	        mutate_if(is.numeric, round,2) %>%
					mutate(BrYr = as.character(BrYr))
kf.tmp$VarType[duplicated(kf.tmp$VarType)] <- ""
table.in  <- table.in %>% bind_rows(kf.tmp)


}

table.in <- table.in %>% rename(Variable = VarType)
table.in$Variable <- gsub("_",".",table.in$Variable)

table.in$ModelType[duplicated(table.in$ModelType)] <- ""

#print(table.in)


if(dim(table.in)[1] ==3){
table.out <- table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","l","l","l","l","l","l"),
                  caption = caption) %>%
     kableExtra::row_spec(c(3), hline_after = TRUE) %>%
     kableExtra::column_spec(7, bold = TRUE) 

}


if(dim(table.in)[1] > 3){
table.out <- table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","l","l","l","l","l","l"),
                  caption = caption) %>%
     kableExtra::row_spec(c(3,7), hline_after = TRUE) %>%
     kableExtra::column_spec(7, bold = TRUE) %>%
	     kableExtra::row_spec(c(9), extra_latex_after = "\\cmidrule(l){2-9}") %>%
     kableExtra::row_spec(c(11), extra_latex_after = "\\cmidrule(l){3-9}") 
}


return(table.out)

} # end prepTablePars

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------





prepTableMainPriors <- function( stk.df,caption){
# stk.df is the data frame with all the output summary
# generates the table input


table.lna <- stk.df %>% arrange(ReportSeq) %>%
			 mutate(Par = "ln.alpha", 
					Distr = "normal",
					Mean = format(round(p.alpha,2),n.small = 2),
					SD = format(round(sigma_alpha,2),n.small = 2),
					CV = "-",
					Cap = "-") %>%
			select(Par,ReportLabel,ModelType, Distr,Mean, SD,CV,Cap)

table.smax <- stk.df %>% arrange(ReportSeq) %>%
			 mutate(Par = "Smax", 
					Distr = "lognormal",
					Mean = formatC(round(p.beta*10^3),big.mark=","),
					SD = format(round(sigma_beta*10^3),big.mark=",",scientific=FALSE),
					CV = format(round(sigma_beta/p.beta,2),n.small = 2),
					Cap =formatC(round(max.scalar*p.beta*10^3),big.mark=",")) %>%
			select(Par,ReportLabel,ModelType, Distr,Mean, SD,CV,Cap)


table.in <- bind_rows(table.lna,table.smax)


table.in$Par[duplicated(table.in$Par)] <- ""

table.out <- table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","l","l","r","r","r","r"),
                  caption = caption) %>%
	  	  kableExtra::row_spec(dim(table.lna)[1], hline_after = TRUE)
	  
return(table.out)


} # end prepTableMainPriors

#-------------------------------------------------------------------------------





prepTableMainPriorsOLD <- function( stk.df,caption){
# stk.df is the data frame with all the output summary
# generates the table input


priors.sub <- stk.df %>% dplyr::filter(ReportLabel == "BR")
table.in <- data.frame(ModelType = priors.sub$ModelType, Par = c("ln.alpha","Smax"),Distr = c("normal", "lognormal"),
					 Mean = round(c(priors.sub$p.alpha,priors.sub$p.beta*10^3)),
					 SD = round(c(priors.sub$sigma_alpha,priors.sub$sigma_beta*10^3)),
					 Cap = c(NA,round(priors.sub$max.scalar*priors.sub$p.beta*10^3))
					)

if("AR1" %in% stk.df$ReportLabel){
priors.sub <- stk.df %>% dplyr::filter(ReportLabel == "AR1")	
	table.in  <- table.in %>% bind_rows(
							data.frame(ModelType = priors.sub$ModelType, Par = c("ln.alpha","Smax"),
												 Distr = c("normal","lognormal"),
					 Mean = c(round(c(priors.sub$p.alpha,priors.sub$p.beta*10^3))),
					 SD = c(round(c(priors.sub$sigma_alpha,priors.sub$sigma_beta*10^3))),
					 Cap = c(NA,round(priors.sub$max.scalar*priors.sub$p.beta*10^3))
							))

}

if("KF" %in% stk.df$ReportLabel){
priors.sub <- stk.df %>% dplyr::filter(ReportLabel == "KF")	
	table.in  <- table.in %>% bind_rows(
							data.frame(ModelType = priors.sub$ModelType, Par = c("ln.alpha","Smax"),
												 Distr = c("normal","lognormal"),
					 Mean = c(round(c(priors.sub$p.alpha,priors.sub$p.beta*10^3))),
					 SD = c(round(c(priors.sub$sigma_alpha,priors.sub$sigma_beta*10^3))),
					 Cap = c(NA,round(priors.sub$max.scalar*priors.sub$p.beta*10^3))
					 ))

}

table.in$ModelType[duplicated(table.in$ModelType)] <- ""
table.in$Cap[is.na(table.in$Cap)] <- "-"

table.out <- table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r"),
                  caption = caption) %>%
	  kableExtra::row_spec(c(2,4), extra_latex_after = "\\cmidrule(l){1-6}") 
	  
return(table.out)

} # end prepTableMainPriors

#-------------------------------------------------------------------------------

prepTableOtherPriors<- function( stk.df,caption){
# stk.df is the data frame with all the output summary
# generates the table input



priors.sub <- stk.df %>% dplyr::filter(ReportLabel == "BR")
table.in <- data.frame(ModelType = priors.sub$ModelType, Par = c("Rec Precision"),Distr = c("gamma"),
					 Shape = priors.sub$shape.tau_R,
					 Lambda = priors.sub$lambda_tau_R
					)




if("AR1" %in% stk.df$ReportLabel){
priors.sub <- stk.df %>% dplyr::filter(ReportLabel == "AR1")	
	table.in  <- table.in %>% bind_rows(
					data.frame(ModelType = priors.sub$ModelType, 
					Par = c("Rec Precision","Pattern Noise"),
					Distr = c("gamma","gamma"),
					 Shape = c(priors.sub$shape.tau_R,priors.sub$shape.tauw),
					 Lambda = c(priors.sub$lambda_tau_R,priors.sub$lambda_tauw)
					 
					 ))

}

if("KF" %in% stk.df$ReportLabel){
priors.sub <- stk.df %>% dplyr::filter(ReportLabel == "KF")	

table.in  <- table.in %>% bind_rows(
					data.frame(ModelType = priors.sub$ModelType, 
					Par = c("Rec Precision","Prod Step Noise"),
					Distr = c("gamma","gamma"),
					 Shape = c(priors.sub$shape.tau_R,priors.sub$shape.tauw),
					 Lambda = c(priors.sub$lambda_tau_R,priors.sub$lambda_tauw)
					 
					 ))

}

table.in$ModelType[duplicated(table.in$ModelType)] <- ""

table.out <- table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","l","l"),
                  caption = caption) %>%
	  kableExtra::row_spec(c(1,3), extra_latex_after = "\\cmidrule(l){1-5}") 
	  


return(table.out)

} # end prepTableOtherPriors


#-------------------------------------------------------------------------------

minmax.cols <-   function(x,num = 3){


# original version: messes up when there are missing values
#max.idx <- sort(x,decreasing = TRUE, index.return = TRUE)$ix[1:num]
#min.idx <- sort(x,decreasing = FALSE, index.return = TRUE)$ix[1:num]
#x.out <- rep("white",length(x))
#x.out[max.idx] <- "cyan"
#x.out[min.idx] <- "orange"

x.vals <- na.omit(x)

max.vals <- sort(x.vals,decreasing = TRUE)[1:3]
min.vals <- sort(x.vals[x.vals>0],decreasing = FALSE)[1:3]

col.skip <-  length(unique(x.vals)) == 1
max.skip <-  length(unique(max.vals)) < 1
min.skip <-  length(unique(min.vals)) < 1

x.out <- rep("white",length(x))

if(!col.skip & !max.skip){ x.out[x %in% max.vals] <- "cyan"}
if(!col.skip & !min.skip){x.out[x %in% min.vals] <- "orange"}



return(x.out)





}

#-------------------------------------------------------------------------------

prepTableSRData <- function( sr.df,caption){
# sr.df is the data frame with all the output summary
# generates the table input


table.in <- sr.df %>% select(-Stock) #%>% complete(Year) # not doing anything?


full.yrs <- data.frame(Year = min(table.in$Year):max(table.in$Year))

table.in <- left_join(full.yrs,table.in, by="Year")

table.cols <- table.in
table.cols[,1] <- "white"
for(i in 4:dim(table.cols)[2]){ table.cols[,i]	<- minmax.cols(x = table.in[,i], num = 3)}

table.in <- table.in %>%
mutate_at(c(4,5,6,7),function(x){prettyNum(round(x,0),big.mark=",",scientific=F)} ) %>%
#mutate_at(6,function(x){round(x*100,0)} ) %>%
mutate_at(9,function(x){formatC(round(x,2), digits = 1, format = 'f')} ) %>%
mutate_at(10:13,function(x){formatC(round(x*100,1), digits = 1, format = 'f')} ) %>%
mutate(Filter = dplyr::recode(as.character(Filter),"TRUE" = "X","FALSE"="")) %>%
mutate(Infill = dplyr::recode(as.character(Infill),"TRUE" = "X","FALSE"="")) %>%
dplyr::rename(F = Filter, I = Infill) 

table.in [is.na(table.in )] <- ""
table.in [table.in  == "NA"] <- ""


table.out <- table.in  %>%     
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","r","r","r","r","r","r","r","r","r","r"),
                  caption = caption) %>%
	kableExtra::row_spec(which(table.in$Year %in% seq(1940,2020,by=10))-1, extra_latex_after = "\\cmidrule(l){1-13}") %>%
  kableExtra::column_spec(4, background =  table.cols[,4]) %>%	
  kableExtra::column_spec(5, background =  table.cols[,5]) %>%	
  kableExtra::column_spec(6, background =  table.cols[,6]) %>%	
  kableExtra::column_spec(7, background =  table.cols[,7]) %>%
  kableExtra::column_spec(8, background =  table.cols[,8]) %>%	
  kableExtra::column_spec(9, background =  table.cols[,9]) %>%	
  kableExtra::column_spec(10, background =  table.cols[,10]) %>%	
  kableExtra::column_spec(11, background =  table.cols[,11]) %>%
  kableExtra::column_spec(12, background =  table.cols[,12]) %>%
  kableExtra::column_spec(13, background =  table.cols[,13])
  
return(table.out)

} # end prepTableSRData




#---------------------------------------------------------------------------
prepTableSRDataSummary <- function( sr.df,caption){

table.in <- sr.df 

#print(names(table.in))
if("Filter" %in% names(table.in)){table.in <- table.in %>% select(-Filter, -Infill) }
#print(names(table.in))

probs.vec <- c(0,0.1,0.25,0.5,0.75,0.9,1)
probs.labels <- c("Min", "p10","p25","Median","p75","p90","Max")

all.yr.df <- bind_cols(Years = "All" , 
											 Label = c("n",probs.labels)   , 
											 bind_rows(
											 	summarize_all(table.in %>% select( -Stock, - Year), function(x){as.character(sum(!is.na(x)))}),
											 	summarize_all(table.in %>% select( -Stock, - Year), quantile,probs=probs.vec,na.rm=TRUE) %>%
													mutate_at(c(1,2,3,4),function(x){prettyNum(round(x,0),big.mark=",",scientific=F)} ) %>%
													mutate_at(5,function(x){as.character(round(x*100,0))} ) %>%
													mutate_at(6,function(x){formatC(round(x,2), digits = 1, format = 'f')}) %>%
													mutate_at(7,function(x){formatC(round(x,1), digits = 1, format = 'f')}) %>%
													mutate_at(8:10,function(x){formatC(round(x*100,1), digits = 1, format = 'f')} )
											 											 )
								) 

recent.df <- bind_cols(Years = "2000+" , 
											 Label = c("n",probs.labels)   , 
											 bind_rows(summarize_all(table.in %>% dplyr::filter(Year >= 2000) %>% select( -Stock, - Year), 
											 												function(x){as.character(sum(!is.na(x)))}),
											 summarize_all(table.in %>% dplyr::filter(Year >= 2000) %>% select( -Stock, - Year),
																			quantile,probs=probs.vec,na.rm=TRUE) %>%
													mutate_at(c(1,2,3,4),function(x){prettyNum(round(x,0),big.mark=",",scientific=F)} ) %>%
													mutate_at(5,function(x){as.character(round(x*100,0))} ) %>%
													mutate_at(6,function(x){formatC(round(x,2), digits = 1, format = 'f')}) %>%
													mutate_at(7,function(x){formatC(round(x,1), digits = 1, format = 'f')}) %>%
													mutate_at(8:10,function(x){formatC(round(x*100,1), digits = 1, format = 'f')} )
											 											 )
								) 

table.in <- bind_rows(all.yr.df, recent.df)

table.in$Years[duplicated(table.in$Years)] <- ""
											 
 table.in 	%>%     
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","r","r","r","r","r","r","r","r","r","r","r"),
                  caption = caption) %>%
		kableExtra::row_spec(c(1,5,9,13), bold=TRUE) %>%
	  kableExtra::row_spec(c(8), hline_after = TRUE) %>%
	kableExtra::row_spec(c(1,4,5,9,12,13), extra_latex_after = "\\cmidrule(l){2-12}") 


} #prepTableSRDataSummary





prepTableModelList <- function( specs.df,caption){



table.in <- stk.specs.table %>%
               select(ReportLabel,ModelType,ReportDescription)    %>%
               dplyr::rename(Label = ReportLabel,Type = ModelType,Description = ReportDescription) #%>%
               #bind_rows(data.frame(Label = c("HBMbc","HBM26"), Type = "Ricker",
			   #Description = c("))

table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","l"),
                  caption = caption)  %>%
     kableExtra::row_spec(c(1:(dim(table.in)[1]-1)), hline_after = TRUE) %>%
     kableExtra::column_spec(3, width="35em") 


}




prepTableModelSpecs <- function( specs.df,caption){



table.in <- stk.specs.table %>%
               select(ReportLabel,ModelType,n.chains,n.samples,n.burnin,n.thin,Kept)    %>%
               dplyr::rename(Label = ReportLabel, Type = ModelType,
                             Burnin = n.burnin, Chains = n.chains,Thin = n.thin,Samples = n.samples) %>%
               mutate_at(c(4,5,7),function(x){x/1000}) %>%
               mutate(Kept = round(Kept,1)) #%>%
              # bind_rows(data.frame(Label = c("HBM1","HBM2")))

table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r","r"),
                  caption = caption) 
				  
				  }
				  
				  
			
			
			
prepTableStockNotes <- function( notes.df,caption){


table.in <- notes.df %>% select(Category, Notes)

table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r","r"),
                  caption = caption) %>%
         column_spec(1, width = "9em") %>%
         column_spec(2, width = "35em") %>%
         row_spec(1:(dim(table.in)[1]-1),hline_after = TRUE)

}




prepTableConvergence <- function( fits.df,caption){


table.in <- fits.df %>%
                  mutate(SRObs = paste0(NumSRObs," (",FirstBY,"-",LastBY,")")) %>%   
                  mutate(DIC = round(DIC,2),MaxRhat = round(max.Rhat,3), Deviance = round(med.deviance,2), Contr = round(SpnContr,1) ) %>%
               dplyr::rename(Label = ReportLabel) %>%
                select(Label,SRObs,Contr,DIC, MaxRhat, Deviance) %>%
               bind_rows(data.frame(Label = c("HBM1","HBM2")))

table.in %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
#   mutate_all(function(x){gsub("\\\\#","\#", x)}) %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r","r"),
                  caption = caption) #%>%
				  
				  
  }
  
  
  
prepTablePostFits <- function( fits.df,caption){
 
  table.in <- fits.df %>%
                 mutate(Num = ln.alpha.num, MaxCV =  format(round(ln.alpha.max.cv, digits=2), nsmall = 2) ,
                        Neff.a = paste0(ln.alpha.med.n.eff," (", ln.alpha.min.n.eff,"-",ln.alpha.max.n.eff,")"),
                        CV.b = format(round(beta.cv, digits=2), nsmall = 2), Neff.b = beta.n.eff	,
                        CV.sigma = format(round(sigma.cv, digits=2), nsmall = 2), Neff.sigma = sigma.n.eff)	
                         

table.in$Neff.a[table.in$ln.alpha.num == 1] <- table.in$ln.alpha.min.n.eff[table.in$ln.alpha.num == 1]


table.in  <- table.in  %>% select(ReportLabel, Num,MaxCV,Neff.a,CV.b,Neff.b,CV.sigma,Neff.sigma) %>%   
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) %>%
               bind_rows(data.frame(ReportLabel = c("HBM1","HBM2")))

names(table.in ) <- c("Label", "Num","MaxCV","Neff","CV","Neff","CV","Neff") 


table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r","r","r","l"),
                  caption = "(ref:PostFitSummaryMeziadin)") %>%
   add_header_above(c(" ", "ln(alpha)" = 3, "beta" = 2,"sigma" =2),bold = T)
     #kableExtra::row_spec(c(3), hline_after = TRUE) %>%
     #kableExtra::column_spec(7, bold = TRUE) 
	 
	 
}




prepTableCap <- function( cap.df,caption){

table.in <-  cap.df %>% 
   select(ReportLabel,median, mean,cv , n.eff, p2.5, p25, p75 ,  p97.5) %>%
   mutate_at(c(2,3,5:9),function(x){prettyNum(round(x),big.mark=",")}) %>%
   mutate(cv = format(round(cv, digits=2), nsmall = 2))
   
table.in  <- table.in  %>%
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) #%>%
               #bind_rows(data.frame(ReportLabel = c("HBM1","HBM2")))

names(table.in ) <- c("Label", "Median","Mean","CV","Neff","2.5","25","75","97.5") 


table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","r","r","r","r","r","r","r","r"),
                  caption = caption) %>%
   add_header_above(c(rep(" ",5), "Percentiles" = 4),bold = T)
     #kableExtra::row_spec(c(3), hline_after = TRUE) %>%
     #kableExtra::column_spec(7, bold = TRUE) 
}



prepTableProd1 <- function( prod.df,caption){



table.in <-   prod.df %>% 
   select(ReportLabel,Year,median, mean,cv , n.eff, p2.5, p25, p75 ,  p97.5) %>%
   mutate_at(c(2:4,6:9)+1,function(x){format(round(x, digits=2), nsmall = 2)}) 
   
table.in  <- table.in  %>%
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)})  #%>%
              # bind_rows(data.frame(ReportLabel = c("HBM1","HBM2")))

names(table.in ) <- c("Label", "BrYr", "Median","Mean","CV","Neff","2.5","25","75","97.5") 
table.in$BrYr[is.na(table.in$BrYr)] <- " "



table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r","r","r","r","r","r"),
                  caption = caption) %>%
   add_header_above(c(rep(" ",6), "Percentiles" = 4),bold = T)
     #kableExtra::row_spec(c(3), hline_after = TRUE) %>%
     #kableExtra::column_spec(7, bold = TRUE) 
	 
	}
	
	
	

prepTableModelSelect <- function( model.sel.df ,caption){




table.in <-   model.sel.df %>% 
   select(Scenario,ModelType, ReportLabel, Sample, KFBrYr, ProdAdjPercentile) 
			
  col.names.use <- c("Scenario","ModelType","Model","Sample","BrYr", "Adj")

table.in$KFBrYr[is.na(table.in$KFBrYr)] <- " "


dupl.idx <- duplicated(table.in$Scenario)

table.in$Scenario[dupl.idx] <- ""
table.in[is.na(table.in)] <- ""


table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,
   					 align = c("l","l","l","r","r","r"), col.names = col.names.use,
                  caption = caption) %>%
   #add_header_above(c(rep(" ",6), "Percentiles" = 4),bold = T)
     kableExtra::row_spec(which(!dupl.idx)[-1]-1, hline_after= TRUE)
     #kableExtra::column_spec(7, bold = TRUE) 
	 
 
	 
	}
		
	
	
	
	
	
prepTableProd2 <- function( prod.rps.df,caption, spn.version = "At1k"){

		
	
table.in <- prod.rps.df %>%  dplyr::filter(SpnLabel == spn.version) %>%
   select(ReportLabel,Year, Median, Mean, p10, p25, p75 ,  p90) %>%
   mutate_at(c(2:7)+1,function(x){format(round(x, digits=1), nsmall = 1)}) 
   
table.in  <- table.in  %>%
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)})  %>%
               bind_rows(data.frame(ReportLabel = c("HBM1","HBM2")))

names(table.in ) <- c("Label", "BrYr", "Median","Mean","10","25","75","90") 
table.in$BrYr[is.na(table.in$BrYr)] <- " "

table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","l","r","r","r","r","r","r"),
                  caption = "(ref:ProdSummary2Meziadin)") %>%
   add_header_above(c(rep(" ",3), "Percentiles" = 4),bold = T)
     #kableExtra::row_spec(c(3), hline_after = TRUE) %>%
     #kableExtra::column_spec(7, bold = TRUE) 
	 
	 
	}
	
	


prepTableBM <- function( bm.df ,caption){


	
	
bm.df <- bm.df %>% select(Variable,Mean,Median,CV,n,percNA,p10,p25,p75,p90)

beta.df <- bm.df %>% dplyr::filter(Variable == "beta") %>% 
            mutate_at(c(2:3,7:10), function(x){formatC(x, format = "e", digits = 2)} ) %>%
            mutate(CV = format(round(CV,2),nsmall = 2))

otherpars.df <- data.frame(Variable = c("sigma","phi","ln.alpha","ln.alpha.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate_at(c(2:4,7:10), function(x){format(round(x,2),nsmall = 2)} ) 

abdbm.df <- data.frame(Variable = c("Smax","Seq","Smsy","Sgen","Seq.c","Smsy.c","Sgen.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate(CV = format(round(CV,2),nsmall = 2)) %>%
               mutate_at(c(2:3,7:10), function(x){prettyNum(round(x),big.mark = ",")} ) 
umsy.df <- data.frame(Variable = c("Umsy","Umsy.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate_at(c(2:4,7:10), function(x){format(round(x,2),nsmall = 2)} ) 
ratio.df <- data.frame(Variable = c("SgenRatio","SgenRatio.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate_at(c(2:4,7:10), function(x){format(round(x,2),nsmall = 2)} ) 


table.in <-    bind_rows(beta.df,otherpars.df,abdbm.df,umsy.df,ratio.df)
   
table.in  <- table.in  %>%
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) 



table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","r","r","r","r","r","r","r","r","r"),
                  caption = caption) %>%
   add_header_above(c(rep(" ",6), "Percentiles" = 4),bold = T) %>%
     kableExtra::row_spec(c(5,6,9,12,14), hline_after = TRUE) #%>%
     #kableExtra::column_spec(7, bold = TRUE) 

}



prepTableBM.f <- function( bm.df ,caption){


	
	
bm.df <- bm.df %>% select(Variable,Mean,Median,CV,n,percNA,p10,p25,p75,p90)

beta.df <- bm.df %>% dplyr::filter(Variable == "beta") %>% 
            mutate_at(c(2:3,7:10), function(x){formatC(x, format = "e", digits = 2)} ) %>%
            mutate(CV = format(round(CV,2),nsmall = 2))

otherpars.df <- data.frame(Variable = c("sigma","phi","ln.alpha","ln.alpha.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate_at(c(2:4,7:10), function(x){format(round(x,2),nsmall = 2)} ) 

abdbm.df <- data.frame(Variable = c("Smax","Seq","Smsy","Sgen","Seq.c","Smsy.c","Sgen.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate(CV = format(round(CV,2),nsmall = 2)) %>%
               mutate_at(c(2:3,7:10), function(x){prettyNum(round(x),big.mark = " ")} ) 
umsy.df <- data.frame(Variable = c("Umsy","Umsy.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate_at(c(2:4,7:10), function(x){format(round(x,2),nsmall = 2)} ) 
ratio.df <- data.frame(Variable = c("SgenRatio","SgenRatio.c")) %>%
                left_join(bm.df,by="Variable") %>%
                mutate_at(c(2:4,7:10), function(x){format(round(x,2),nsmall = 2)} ) 


table.in <-    bind_rows(beta.df,otherpars.df,abdbm.df,umsy.df,ratio.df)
   
table.in  <- table.in  %>%
   mutate_all(function(x){gsub("&", "\\\\&", x)}) %>% 
   mutate_all(function(x){gsub("%", "\\\\%", x)}) %>%
   mutate_all(function(x){gsub("\\\\n","\n", x)}) 

col.names.use <- c("Variable","Moyenne","Médiane","CV","n","percNA","p10","p25","p75","p90")

table.in  %>%
   csas_table(format = "latex", escape = FALSE, font_size = 10,align = c("l","r","r","r","r","r","r","r","r","r"),
                  caption = caption, col.names=col.names.use) %>%
   add_header_above(c(rep(" ",6), "Centiles" = 4),bold = T) %>%
     kableExtra::row_spec(c(5,6,9,12,14), hline_after = TRUE) #%>%
     #kableExtra::column_spec(7, bold = TRUE) 

}
