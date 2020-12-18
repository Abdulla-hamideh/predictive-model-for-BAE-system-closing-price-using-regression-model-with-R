predict_BAE <- function(lse, newdata){
  
  lse$STJ <- lse$STJ + 10
  lse$RB <- lse$RB + 10
  lse$MIN <- lse$MIN +10
  lse$PRU <- lse$PRU + 10
  lse$CCH <- lse$CCH + 10
  lse$EXPN <- lse$EXPN + 10
  lse$VOD <- lse$VOD + 10 
  lse$GVC <- lse$GVC + 10
  lse$AHT <- lse$AHT +10
  lse$CPG <- lse$CPG + 10
  lse$CCL <- lse$CCL + 10
  lse$RR <- lse$RR + 10
  lse$DLG <- lse$DLG + 10
  lse$TUI <- lse$TUI + 10
  lse$LLOY <- lse$LLOY + 10
  lse$RMV <- lse$RMV + 10 
  lse$SSE <- lse$SSE + 10 
  lse$SDR <- lse$SDR + 10
  lse$SMT <- lse$SMT + 10
  lse$EZJ <- lse$EZJ + 10
  lse$NMC <- lse$NMC + 10
  lse$BATS <- lse$BATS +10 
  lse$SPX <- lse$SPX + 10
  lse$TSCO <- lse$TSCO + 10
  lse$CNA <- lse$CNA + 10
  lse$RTO <- lse$RTO + 10
  lse$ANTO <- lse$ANTO + 10
  newdata$STJ <- newdata$STJ + 10
  newdata$RB <- newdata$RB + 10
  newdata$MIN <- newdata$MIN +10
  newdata$PRU <- newdata$PRU + 10
  newdata$CCH <- newdata$CCH + 10
  newdata$EXPN <- newdata$EXPN + 10
  newdata$VOD <- newdata$VOD + 10 
  newdata$GVC <- newdata$GVC + 10
  newdata$AHT <- newdata$AHT +10
  newdata$CPG <- newdata$CPG + 10
  newdata$CCL <- newdata$CCL + 10
  newdata$RR <- newdata$RR + 10 
  newdata$DLG <- newdata$DLG + 10
  newdata$TUI <- newdata$TUI + 10
  newdata$LLOY <- newdata$LLOY + 10
  newdata$RMV <- newdata$RMV + 10 
  newdata$SSE <- newdata$SSE + 10
  newdata$SDR <- newdata$SDR + 10
  newdata$SMT <- newdata$SMT + 10
  newdata$EZJ <- newdata$EZJ + 10
  newdata$NMC <- newdata$NMC + 10
  newdata$BATS <- newdata$BATS +10 
  newdata$SPX <- newdata$SPX + 10
  newdata$TSCO <- newdata$TSCO + 10
  newdata$CNA <- newdata$CNA + 10
  newdata$RTO <- newdata$RTO + 10
  newdata$ANTO <- newdata$ANTO + 10

  lse$CNA <- lse$CNA^3
  lse$MIN2 <- lse$MIN^2
  lse$SSE <- lse$SSE^3
  lse$STJ <- log(lse$STJ)
  lse$GVC <- log(lse$GVC)
  lse$PRU <- log(lse$PRU)
  lse$SMT <- log(lse$SMT)
  lse$EZJ <- log(lse$EZJ)
  newdata$CNA <- newdata$CNA^3
  newdata$MIN2 <- newdata$MIN^2
  newdata$SSE <- newdata$SSE^3
  newdata$STJ <- log(newdata$STJ)
  newdata$GVC <- log(newdata$GVC)
  newdata$PRU <- log(newdata$PRU)
  newdata$SMT <- log(newdata$SMT)
  newdata$EZJ <- log(newdata$EZJ)
  # this is the part that fits your linear model
  BAE.lm <- lm(BA~ STJ + DLG  + VOD  + GVC + CPG + 
                  SPX + AHT  + RB + PRU + SMT + 
                  EZJ, data = lse)
  # this is the part that produces predictions using your linear model
  predictions <- predict(BAE.lm, newdata = newdata)
  return(predictions)
}

