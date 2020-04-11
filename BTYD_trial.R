library(BTYD)
library(dplyr)
library(foreach)
library(ggplot2)
devtools::source_gist("2a1bb0133ff568cbe28d", 
                      filename = "geom_flat_violin.R")

#==== Parate/NBD , BG/NBD, Gamma-Gamma ====
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

## param est
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
        trans.val=expected.trans.val,
        forecast.ltv=pnbd.forecast.ltv
    )
    bgnbd.res <- bind_cols(
        data.frame(cust=dimnames(tot.cbt)$cust, year=i, model="bgnbd"),
        cal.cbs,
        trans.num=bgnbd.expected.trans.num,
        prob.alive=bgnbd.prob.alive,
        trans.val=expected.trans.val,
        forecast.ltv=bgnbd.forecast.ltv
    )
    res <- bind_rows(pnbd.res, bgnbd.res)
    return(res)
}

## viz res
# tile
# x.axis = x, y.axis = t.x, fill = trans.num or prob.alive
ggplot(data=ltv.result, aes(x=x, y=t.x, fill=trans.num)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("blue", "red")) + 
    ggtitle("frequency;x, recency;t.x, and expected transactions in 1 year")
ggplot(data=ltv.result, aes(x=x, y=t.x, fill=prob.alive)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("blue", "red")) + 
    ggtitle("frequency;x, recency;t.x, and prob to alive in 1 year")

# tile
# x.axis = m.x, y.axis = m.x, fill = expected.trans.val
ggplot(data=ltv.result, aes(x=x, y=round(m.x), fill=trans.val)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("blue", "red")) + 
    ggtitle("frequency;x, monetary;m.x, and expected transactions in 1 year")

ggplot(data=ltv.result, aes(x=round(log(x),1), y=round(log(m.x),1), fill=trans.val)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("blue", "red")) + 
    ggtitle("frequency;log(x), monetary;log(m.x), and expected transactions in 1 year")

# violin
# x.axis = year, y.axis = trans.num or prob.alive

# tile
# x.axis = x, y.axis = t.x, fill = pnbd's  - bgnbd's
