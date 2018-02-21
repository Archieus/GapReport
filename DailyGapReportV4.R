library(jsonlite)
library(quantmod)
library(data.table)
library(timeDate)

Sys.setenv(TZ = "EST5EDT")

# AlphaVantage API Key: 4FS12H08TP68SJV2

####Identify Business Days (excludes NYSE Holidays and Weekends)###
Holidays <- as.Date(holidayNYSE())
Calndr <- timeSequence("2018-01-01", to = Sys.Date(), by = "day")
business.days <- Calndr[isBizday(Calndr, Holidays)]
daterange <- tail(business.days,3)

getQuoteEH <- function(ticks){
  qRoot <- "https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,regularMarketChangePercent,regularMarketPrice,regularMarketChange,regularMarketTime&formatted=false&symbols="
  z <- fromJSON(paste(qRoot, paste(ticks, collapse=","), sep=""))
  tryCatch(fromJSON(paste(qRoot, paste(ticks, collapse=","), sep="")),error=function(e) {remove(ticks)})
  z <- z$quoteResponse$result[,c("symbol", "regularMarketTime", "regularMarketPrice", "regularMarketChange", "regularMarketChangePercent")]
  row.names(z) <- z$symbol
  z$symbol <- NULL
  names(z) <- c("Time", "Price", "Change", "Percent Change")
  z$Time <- as.POSIXct(z$Time, origin = '1970-01-01 00:00:00')
  return(z)
  
}

##Download Quotes for Up Gaps
UpGaps <- read.table("F:/GapData/Input Data/UpGaps.txt", sep = ",", header = TRUE)
UpGaps.tr = t(UpGaps)

UpGap.data <- t(as.data.frame(sapply(UpGaps.tr, function(x) getQuoteEH(x))))

##Download Quotes for Down Gaps
DownGaps <- read.table('F:/GapData/Input Data/DownGaps.txt', sep = ",", header = TRUE)
DownGaps.tr = t(DownGaps)

DownGap.data <- t(as.data.frame(sapply(DownGaps.tr, function(x) getQuote(x))))

##Download S&P and Russell 2000 data for Structured Products
Indexes <- c('^GSPC', '^RUT')
suppressWarnings(getSymbols(Indexes, src = 'yahoo', from = (Sys.Date()-2615)))
RUT.df <- as.data.frame(RUT)
names(RUT.df) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adj-Close')
SP500.df <- as.data.frame(GSPC)
names(SP500.df) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adj-Close')

#### BEGIN LOW CLOSE DATA DOWNLOAD ####
#######################################
LowClose <- read.csv('F:/GapData/Input Data/LowClose.txt', sep = "," , header = TRUE)
LowClose.tr <- t(LowClose)
##Download data using getSymbols quantmod
LOWSC <- new.env() # create new enviroment for data to be loaded to

today <- substr(date(), start = 1, stop =3) #Pulls Day of the week from the date() function

sapply(LowClose.tr, function(x){
  try(
    suppressWarnings(getSymbols(
      x, src = 'google',
      from= as.Date(daterange[1]),
      env=LOWSC)),
    silent=TRUE)
})

Close.data <- eapply(LOWSC, Cl)
Close.df <- data.frame(Close.data)
PctChg <- na.omit(ROC(Close.df,1, "discrete"))*100
Close.df <- rbind(last(Close.df),PctChg)

#### LOAD PORTFOLIO DATA ####
#Accounts <- read.csv("F:/Gap Data/Input Data/Position Export.csv", sep = ",", header = TRUE)

Accounts <- read.csv("F:/VBATradeMacros/Position Export.csv", sep = ",", header = TRUE)

#### GATHER LOW CLOSE DATA ####
Lownames <- gsub( ".Close",  "", names(Close.df), fixed = TRUE)
names(Close.df) <- Lownames
LowQuotes <- as.data.frame(t(Close.df))
LowQuotes <- setNames(cbind(rownames(LowQuotes), LowQuotes, row.names = NULL), c("Symbol", "Close", "PctChg"))

#### GENERATE LOW CLOSE REPORT ####
LowRaw <- merge(Accounts, LowQuotes, by = "Symbol", all = FALSE)
LowRawExCA <- LowRaw[which(LowRaw$Asset.Class.Code != "C"),]
LowReport <- LowRawExCA[,c(1,7,3,2,8,20,21)]

#### GATHER UP- and DOWN-GAP DATA ####
UPdata <- as.data.frame(cbind(rownames(UpGap.data), UpGap.data, row.names = NULL))
names(UPdata) <- c("Symbol", "Time", "Last"," Change", "PctChg")

UPGapRaw <- merge(Accounts, UPdata, by = "Symbol", all = FALSE)
UPGapExCA <- UPGapRaw[which(UPGapRaw$Asset.Class.Code != "C"),]
UPGapReport <- UPGapExCA[,c(1,7,3,2,21:23)]

### Coerce Data Frame to single Characters as there are columns that are actually Lists ###
UPGapRep.df <- data.frame(lapply(UPGapReport, as.character), stringsAsFactors=FALSE)

#### GATHER UP- and DOWN-GAP DATA ####
DNdata <- as.data.frame(cbind(rownames(DownGap.data), DownGap.data, row.names = NULL))
names(DNdata) <- c("Symbol", "Time", "Last", "Change", "PctChg")

DNGapRaw <- merge(Accounts, DNdata, by = "Symbol", all = FALSE)
DNGapExCA <- DNGapRaw[which(DNGapRaw$Asset.Class.Code != "C"),]
DNGapReport <- as.data.frame(DNGapExCA[,c(1,7,3,2,21:23)])

### Coerce Data Frame to single Characters as there are columns that are actually Lists ###
DNGapRep.df <- data.frame(lapply(DNGapReport, as.character), stringsAsFactors=FALSE)

#### WRITE DATA TO CSV FILES
write.csv(RUT.df, file = 'F:/Matt-Data/RUT.csv', row.names = TRUE)
write.csv(SP500.df, file = 'F:/Matt-Data/SP500.csv', row.names = TRUE)
#write.csv(UST.df, file  = 'F:/Tameka-Data/LongTsy.csv', row.names = TRUE)

#write.csv(UpGap.data, file = 'Upfinal.csv')
#write.csv(DownGap.data, file = 'Downfinal.csv')
#write.csv(t(Close.df), file = "LowScreen.csv")

write.csv(UPGapRep.df, file = "UpGapReport.csv")
write.csv(DNGapRep.df, file = "DNGapReport.csv")
write.csv(LowReport, file = "LowCloseReport.csv")

###AUDIT FUNCTION### See how many symbols were omitted from the download
print(length(ls(LOWSC)))
print(length(LowClose.tr))
