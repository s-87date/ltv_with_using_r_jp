library(BTYD)
library(dplyr)
library(foreach)

#==== Parate/NBD , BG/NBD, Gamma-Gamma====
## prep
# load data, and tidy up
cdnowElog <- system.file("data/cdnowElog.csv", package="BTYD")
elog <- dc.ReadLines(cdnowElog, cust.idx = 2, date.idx = 3, sales.idx = 5); glimpse(elog)
elog$date <- as.Date(elog$date, "%Y%m%d")
# only one tran per cust per day w/ the total sum of their spending for the day
elog <- dc.MergeTransactionsOnSameDate(elog)
# divide for validation
end.of.cal.period <- as.Date("1997-09-30")
elog.cal <- elog[which(elog$date <= end.of.cal.period),]
# filterd event log w/o/ init log, and each cust summary
split.data <- dc.SplitUpElogForRepeatTrans(elog.cal)
clean.elog <- split.data$repeat.trans.elog
# make customer-by-time matrix
freq.cbt <- dc.CreateFreqCBT(clean.elog)
# create a customer-by-time matrix using all transactions, and then merge the filtered CBT with this total CBT
tot.cbt <- dc.CreateFreqCBT(elog)
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)
# 
birth.periods <- split.data$cust.data$birth.per
last.dates <- split.data$cust.data$last.date 
cal.cbs.dates <- data.frame(birth.periods, last.dates, end.of.cal.period)
ave.spend <- elog.cal %>% ##spend
    dplyr::group_by(cust) %>%
    dplyr::summarise(ave.spend=mean(sales)) %>%
    .$ave.spend
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates, per="week") %>%
    as.data.frame() %>%
    dplyr::mutate(m.x=ave.spend)
cal.cbs %>% head

## pram est
# palate / nbd
(pnbd.params <- pnbd.EstimateParameters(cal.cbs))
(bgnbd.params <- bgnbd.EstimateParameters(cal.cbs))
# gamma - gamma
# There will be many warnings due to the zeroes that are
# included in the data above. To avoid them, use the following:
# (see example for spend.LL)
# We will let the spend function use default starting parameters
(spend.params <- spend.EstimateParameters(cal.cbs$m.x, cal.cbs$x))

## individual level est
# the number of repeat transactions a newly acquired customer will make in a time period of one year
# use 52 weeks to represent one year
one.year <- 52
# expected num of trans by all cust
# pnbd.Expectation(pnbd.params, t=one.year) 
# bgnbd.Expectation(bgnbd.params, t=one.year)
ltv.result <- foreach(i=1:5, .combine="rbind") %do% {
    # the number of transactions we expect a customer to make in the holdout period
    pnbd.expected.trans.num <- pnbd.ConditionalExpectedTransactions(pnbd.params, T.star=one.year*i, cal.cbs$x, cal.cbs$t.x, cal.cbs$T.cal)
    bgnbd.expected.trans.num <- bgnbd.ConditionalExpectedTransactions(bgnbd.params, T.star=one.year*i, cal.cbs$x, cal.cbs$t.x, cal.cbs$T.cal)
    # the probability that a customer is still alive at the end of the calibration period
    pnbd.prob.alive <- pnbd.PAlive(pnbd.params, cal.cbs$x, cal.cbs$t.x, cal.cbs$T.cal)
    bgnbd.prob.alive <- bgnbd.PAlive(bgnbd.params, cal.cbs$x, cal.cbs$t.x, cal.cbs$T.cal)
    # the expected transaction value for a customer
    expected.trans.val <- spend.expected.value(spend.params, cal.cbs$m.x, cal.cbs$x)
    # calc ltv
    pnbd.forecast.ltv <- pnbd.expected.trans.num * pnbd.prob.alive * expected.trans.val
    bgnbd.forecast.ltv <- bgnbd.expected.trans.num * bgnbd.prob.alive * expected.trans.val

    # result data.frame
    # cust, cal.cbs, year, model, trans.num, prob.alive, ltv
    pnbd.res <- bind_cols(
        data.frame(cust=dimnames(tot.cbt)$cust, year=i, model="pnbd"),
        cal.cbs,
        trans.num=pnbd.expected.trans.num,
        prob.alive=pnbd.prob.alive,
        forecast.ltv=pnbd.forecast.ltv
    )
    bgnbd.res <- bind_cols(
        data.frame(cust=dimnames(tot.cbt)$cust, year=i, model="pnbd"),
        cal.cbs,
        trans.num=bgnbd.expected.trans.num,
        prob.alive=bgnbd.prob.alive,
        forecast.ltv=bgnbd.forecast.ltv
    )
    res <- bind_rows(pnbd.res, bgnbd.res)
    return(res)
}


