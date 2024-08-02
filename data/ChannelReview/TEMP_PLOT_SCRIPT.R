library(tidyverse)


pot.fec.df <- read_csv("REPORT/data/ChannelReview/Pot_fec_by_channel.csv")
pot.fec.df


ylim.use <- range(pot.fec.df$calc_fec,na.rm=TRUE)
ylim.use
xlim.use <- range(pretty(pot.fec.df$brood_year))

pinkut.df <- pot.fec.df %>% dplyr::filter(project == "Pinkut") %>%
							arrange(brood_year)
fulton.df <- pot.fec.df %>% dplyr::filter(project == "Fulton") %>%
	arrange(brood_year)




png(filename = "REPORT/data/ChannelReview/PotentialFecundity.png",
		width = 480*4.5, height = 480*2.5, units = "px", pointsize = 14*3.5, bg = "white",  res = NA)



par(mfrow = c(1,2))

plot(fulton.df$brood_year,fulton.df$calc_fec,bty="n", type="o",
		 ylim = ylim.use, xlim = xlim.use,xlab= "Brood Year", ylab = "Potential Fecundity",
		 col="darkblue", pch=19, main="Fulton",col.main="darkblue",las=1)



plot(pinkut.df$brood_year,pinkut.df$calc_fec,bty="n", type="o",
		 ylim = ylim.use, xlim = xlim.use,xlab= "Brood Year", ylab = "Potential Fecundity",
		 col="darkblue", pch=19, main="Pinkut",col.main="darkblue",las =1)

dev.off()
