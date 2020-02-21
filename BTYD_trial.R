library(BTYD)
library(dplyr)

cdnowElog <- system.file("data/cdnowElog.csv", package="BTYD")
elog <- dc.ReadLines(cdnowElog, cust.idx = 2, date.idx = 3, sales.idx = 5)
elog$date <- as.Date(elog$date, "%Y%m%d")

## clean up
# only one tran per cust per day w/ the total sum of their spending for the day
elog <- dc.MergeTransactionsOnSameDate(elog)

# divide for validation
end.of.cal.period <- as.Date("1997-09-30")
elog.cal <- elog[which(elog$date <= end.of.cal.period),]

# filterd event log w/o/ init log, and each cust summary
split.data <- dc.SplitUpElogForRepeatTrans(elog.cal)
clean.elog <- split.data$repeat.trans.elog

## 
# make customer-by-time matrix
freq.cbt <- dc.CreateFreqCBT(clean.elog)



