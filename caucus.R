caucus <- data.frame(
  State=c("IA", "IA", "NH", "NH", "NV", "SC", "Super Tuesday", "Super Tuesday", "NV", "SC"), 
  Date=ymd(c("2016/02/01", "2016/02/01", "2016/02/09", "2016/02/09", "2016/02/20", "2016/02/20", "2016/03/01", "2016/03/01", "2016/02/23", "2016/02/27")),
  Party=c("R","D", "R","D", "D", "R", "D", "R", "D", "R")
)

caucuslayer <- function(ymax, party) {
  subcaucus <- subset(caucus, Party==party)
  list(
    geom_segment(aes(x=Date, xend=Date), y = 2, yend=ymax,
                 colour="grey75", data=subcaucus),
    geom_text(aes(x=Date,  label=State), y = 0, size=2.25,
              colour="grey75", data=subcaucus)
  )
}