library(tidyverse)
library(ggplot2)

#####------------------------------------------------------#
##### Read and merge data ####
#####------------------------------------------------------#

popvote_df <- read_csv("popvote_1948-2016.csv") # update here
economy_df <- read_csv("econ.csv") # update here
poll_df    <- read_csv("pollavg_1968-2016.csv") # update here

dat <- popvote_df %>% 
    full_join(poll_df %>% 
                  filter(days_until_election == 45) %>% 
                  group_by(year,party) %>% 
                  summarise(avg_support=mean(avg_support))) %>% 
    left_join(economy_df %>% 
                  filter(quarter == 2))

#####------------------------------------------------------#
#####  Proposed models ####
#####------------------------------------------------------#

## option 1: fundamentals-only model

dat_econ <- unique(dat[!is.na(dat$GDP_growth_qt),])
dat_econ_inc <- dat_econ[dat_econ$incumbent_party_winner,]
dat_econ_chl <- dat_econ[!dat_econ$incumbent_part_winner,]
mod_econ_inc <- lm(pv ~ GDP_growth_qt, data = dat_econ_inc)
mod_econ_chl <- lm(pv ~ GDP_growth_qt, data = dat_econ_chl)

## option 2: adjusted polls-only model
dat_poll <- dat[!is.na(dat$avg_support),]
dat_poll_inc <- dat_poll[dat_poll$incumbent_party,]
dat_poll_chl <- dat_poll[!dat_poll$incumbent_party,]
mod_poll_inc <- lm(pv ~ avg_support, data = dat_poll_inc)
mod_poll_chl <- lm(pv ~ avg_support, data = dat_poll_chl)

## option 3: adjusted polls + fundamentals model
dat_plus <- dat[!is.na(dat$avg_support) & !is.na(dat$GDP_growth_qt),]
dat_plus_inc <- dat_plus[dat_plus$incumbent_party,]
dat_plus_chl <- dat_plus[!dat_plus$incumbent_party,]
mod_plus_inc <- lm(pv ~ avg_support + GDP_growth_qt, data = dat_plus_inc)
mod_plus_chl <- lm(pv ~ avg_support + GDP_growth_qt, data = dat_plus_chl)

#####------------------------------------------------------#
#####  Model selection: In-sample evaluation ####
#####------------------------------------------------------#
 
## interpret models
summary(mod_econ_inc)
summary(mod_econ_chl)

summary(mod_poll_inc)
summary(mod_poll_chl)

summary(mod_plus_inc)
summary(mod_plus_chl)

## in-sample fit
mean(abs(mod_econ_inc$residuals))
mean(abs(mod_econ_chl$residuals))

mean(abs(mod_poll_inc$residuals))
mean(abs(mod_poll_chl$residuals))

mean(abs(mod_plus_inc$residuals))
mean(abs(mod_plus_chl$residuals))

par(mfrow=c(3,2))
{
    plot(mod_econ_inc$fitted.values, dat_econ_inc$pv,
         main="fundamentals (incumbent)", xlab="predicted", ylab="true", 
         cex.lab=2, cex.main=2, type='n',xlim=c(40,65),ylim=c(40,65))
    text(mod_econ_inc$fitted.values, dat_econ_inc$pv, dat_econ_inc$year)
    abline(a=0, b=1, lty=2)
    
    plot(mod_econ_chl$fitted.values, dat_econ_chl$pv,
         main="fundamentals (challenger)", xlab="predicted", ylab="true", 
         cex.lab=2, cex.main=2, type='n',xlim=c(40,55),ylim=c(40,55))
    text(mod_econ_chl$fitted.values, dat_econ_chl$pv, dat_econ_chl$year)
    abline(a=0, b=1, lty=2)
    
    plot(mod_poll_inc$fitted.values, dat_poll_inc$pv,
         main="polls (incumbent)", xlab="predicted", ylab="true", 
         cex.lab=2, cex.main=2, type='n',xlim=c(40,55),ylim=c(40,55))
    text(mod_poll_inc$fitted.values, dat_poll_inc$pv, dat_poll_inc$year)
    abline(a=0, b=1, lty=2)
    
    plot(mod_poll_chl$fitted.values, dat_poll_chl$pv,
         main="polls (challenger)", xlab="predicted", ylab="true", 
         cex.lab=2, cex.main=2, type='n',xlim=c(40,55),ylim=c(40,55))
    text(mod_poll_chl$fitted.values, dat_poll_chl$pv, dat_poll_chl$year)
    abline(a=0, b=1, lty=2)
    
    plot(mod_plus_inc$fitted.values, dat_plus_inc$pv,
         main="plus (incumbent)", xlab="predicted", ylab="true", 
         cex.lab=2, cex.main=2, type='n',xlim=c(40,55),ylim=c(40,55))
    text(mod_plus_inc$fitted.values, dat_plus_inc$pv, dat_plus_inc$year)
    abline(a=0, b=1, lty=2)
    
    plot(mod_plus_chl$fitted.values, dat_plus_chl$pv,
         main="plus (challenger)", xlab="predicted", ylab="true", 
         cex.lab=2, cex.main=2, type='n',xlim=c(40,55),ylim=c(40,55))
    text(mod_plus_chl$fitted.values, dat_plus_chl$pv, dat_plus_chl$year)
    abline(a=0, b=1, lty=2)
}

#####------------------------------------------------------#
#####  Model selection: Out-of-sample evaluation ####
#####------------------------------------------------------#

all_years <- seq(from=1948, to=2016, by=4)
outsamp_dflist <- lapply(all_years, function(year){
 
  true_inc <- unique(dat$pv[dat$year == year & dat$incumbent_party_winner])
  true_chl <- unique(dat$pv[dat$year == year & !dat$incumbent_party_winner])

  ##fundamental model out-of-sample prediction
  mod_econ_inc_ <- lm(pv ~ GDP_growth_qt, data = dat_econ_inc[dat_econ_inc$year != year,])
  mod_econ_chl_ <- lm(pv ~ GDP_growth_qt, data = dat_econ_chl[dat_econ_chl$year != year,])
  pred_econ_inc <- predict(mod_econ_inc_, dat_econ_inc[dat_econ_inc$year == year,])
  pred_econ_chl <- predict(mod_econ_chl_, dat_econ_chl[dat_econ_chl$year == year,])

  if (year >= 1980) {
    ##poll model out-of-sample prediction
    mod_poll_inc_ <- lm(pv ~ avg_support, data = dat_poll_inc[dat_poll_inc$year != year,])
    mod_poll_chl_ <- lm(pv ~ avg_support, data = dat_poll_chl[dat_poll_chl$year != year,])
    pred_poll_inc <- predict(mod_poll_inc_, dat_poll_inc[dat_poll_inc$year == year,])
    pred_poll_chl <- predict(mod_poll_chl_, dat_poll_chl[dat_poll_chl$year == year,])


    ##plus model out-of-sample prediction
    mod_plus_inc_ <- lm(pv ~ GDP_growth_qt + avg_support, data = dat_plus_inc[dat_poll_inc$year != year,])
    mod_plus_chl_ <- lm(pv ~ GDP_growth_qt + avg_support, data = dat_plus_chl[dat_poll_chl$year != year,])
    pred_plus_inc <- predict(mod_plus_inc_, dat_plus_inc[dat_plus_inc$year == year,])
    pred_plus_chl <- predict(mod_plus_chl_, dat_plus_chl[dat_plus_chl$year == year,])
  } else {
    pred_poll_inc <- pred_poll_chl <- pred_plus_inc <- pred_plus_chl <- NA
  }
  
  cbind.data.frame(year,
        econ_margin_error = (pred_econ_inc-pred_econ_chl) - (true_inc-true_chl),
        poll_margin_error = (pred_poll_inc-pred_poll_chl) - (true_inc-true_chl),
        plus_margin_error = (pred_plus_inc-pred_plus_chl) - (true_inc-true_chl),
        econ_winner_correct = (pred_econ_inc > pred_econ_chl) == (true_inc > true_chl),
        poll_winner_correct = (pred_poll_inc > pred_poll_chl) == (true_inc > true_chl),
        plus_winner_correct = (pred_plus_inc > pred_plus_chl) == (true_inc > true_chl)
  )
})
outsamp_df <- do.call(rbind, outsamp_dflist)
colMeans(abs(outsamp_df[2:4]), na.rm=T)
colMeans(outsamp_df[5:7], na.rm=T) ### classification accuracy

outsamp_df[,c("year","econ_winner_correct","poll_winner_correct","plus_winner_correct")]

#####------------------------------------------------------#
#####  Predicting 2020 ####
#####------------------------------------------------------#
dat_2020_inc <- data.frame(GDP_growth_qt = -9.49, avg_support = __)
dat_2020_chl <- data.frame(GDP_growth_qt = -9.49, avg_support = __)

## point predictions
predict(mod_plus_inc, newdata = dat_2020_inc)
predict(mod_plus_chl, newdata = dat_2020_chl)

## prediction intervals
(pred_plus_inc <- predict(mod_plus_inc, dat_2020_inc, 
                          interval = "prediction", level=0.95))
(pred_plus_chl <- predict(mod_plus_chl, dat_2020_chl, 
                          interval = "prediction", level=0.95))

(pred_poll_inc <- predict(mod_poll_inc, dat_2020_inc, 
                          interval = "prediction", level=0.95))
(pred_poll_chl <- predict(mod_poll_chl, dat_2020_chl, 
                          interval = "prediction", level=0.95))

(pred_econ_inc <- predict(mod_econ_inc, dat_2020_inc, 
                          interval = "prediction", level=0.95))
(pred_econ_chl <- predict(mod_econ_chl, dat_2020_chl, 
                          interval = "prediction", level=0.95))

pred_df <- rbind.data.frame(
  data.frame(pred_plus_inc, model="plus", candidate="Trump"),
  data.frame(pred_plus_chl, model="plus", candidate="Biden"),
  data.frame(pred_poll_inc, model="polls", candidate="Trump"),
  data.frame(pred_poll_chl, model="polls", candidate="Biden"),
  data.frame(pred_econ_inc, model="fundamentals", candidate="Trump"),
  data.frame(pred_econ_chl, model="fundamentals", candidate="Biden")
)
ggplot(pred_df, 
       aes(x=candidate, y=fit, ymin=lwr, ymax=upr, color=model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  theme_bw()

#####------------------------------------------------------#
#####  New option: Weighted Ensemble ####
#####------------------------------------------------------#

## Ensemble 1: polls and fundamentals matter equally as much
pwt <- 0.5; ewt <- 0.5;
pwt*predict(mod_poll_inc, dat_2020_inc) + ewt*predict(mod_econ_inc, dat_2020_inc)
pwt*predict(mod_poll_chl, dat_2020_chl) + ewt*predict(mod_econ_chl, dat_2020_chl)

## Ensemble 2: polls matter closer to November 
## (what Nate Silver thinks) # my var = days until election?
days_left <- 42
pwt <- 1/sqrt(days_left); ewt <- 1-(1/sqrt(days_left));
plot(100:1, 1/sqrt(100:1), xlim=c(100, 0), type="l", 
     ylab="poll model weight", xlab="days til elxn")

pwt*predict(mod_poll_inc, dat_2020_inc) + ewt*predict(mod_econ_inc, dat_2020_inc) # adjusted poll
pwt*predict(mod_poll_chl, dat_2020_chl) + ewt*predict(mod_econ_chl, dat_2020_chl) # adjusted poll

pwt*dat_2020_inc$avg_support + ewt*predict(mod_econ_inc, dat_2020_inc) # raw poll
pwt*dat_2020_chl$avg_support + ewt*predict(mod_econ_chl, dat_2020_chl) # raw poll

## Ensemble 3: fundamentals matter closer to November 
## (what Gelman & King (1993) think)
days_left <- 42
pwt <- 1-(1/sqrt(days_left)); ewt <- 1/sqrt(days_left);
pwt*predict(mod_poll_inc, dat_2020_inc) + ewt*predict(mod_econ_inc, dat_2020_inc)
pwt*predict(mod_poll_chl, dat_2020_chl) + ewt*predict(mod_econ_chl, dat_2020_chl)

## Ensemble 4: weight models based on their historical performance
pwt <- colMeans(outsamp_df,na.rm=T)["poll_winner_correct"]; 
ewt <- colMeans(outsamp_df, na.rm=T)["econ_winner_correct"];
(pwt/(pwt+ewt))*predict(mod_poll_inc, dat_2020_inc) + (ewt/(pwt+ewt))*predict(mod_econ_inc, dat_2020_inc)
(pwt/(pwt+ewt))*predict(mod_poll_chl, dat_2020_chl) + (ewt/(pwt+ewt))*predict(mod_econ_chl, dat_2020_chl)

#####------------------------------------------------------#
#####  Example (extra): Real-time 2020 Poll Averages ####
#####------------------------------------------------------#
{
    # poll_2022_df <- read_csv(538_generic_ballot_averages_2018-2022.csv) # SUBSET TO ONLY 2022 CYCLE
    # alternatively, pull directly from website
    # poll_2020_url <- "https://projects.fivethirtyeight.com/2020-general-data/..."
    # poll_2020_df <- read_csv(poll_2020_url)
    
    elxnday_2020 <- as.Date("11/3/2020", "%m/%d/%Y")
    dnc_2020 <- as.Date("8/20/2020", "%m/%d/%Y")
    rnc_2020 <- as.Date("8/27/2020", "%m/%d/%Y")
    
    colnames(poll_2020_df) <- c("year","state","poll_date","candidate_name","avg_support","avg_support_adj")
    
    poll_2020_df <- poll_2020_df %>%
        mutate(party = case_when(candidate_name == "" ~ "republican",
                                 candidate_name == "" ~ "democrat"),
               poll_date = as.Date(poll_date, "%m/%d/%Y"),
               days_left = round(difftime(elxnday_2020, poll_date, unit="days")),
               weeks_left = round(difftime(elxnday_2020, poll_date, unit="weeks")),
               before_convention = case_when(poll_date < dnc_2020 & party == "democrat" ~ TRUE,
                                             poll_date < rnc_2020 & party == "republican" ~ TRUE,
                                             TRUE ~ FALSE)) %>%
        filter(!is.na(party)) %>%
        filter(state == "National")
}