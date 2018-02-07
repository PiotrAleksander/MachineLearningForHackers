library(tm)
library(ggplot2)
library(lubridate)

data.path<-"../SpamClassifier/data/"
easyham.path<-paste(data.path, "easy_ham/", sep="") #dane ze SpamAssassins

msg.full <- function(path) {
  con <- file(path, open="rt")
  msg <- readLines(con)
  close(con)
  return(msg)
}

get.from <- function(msg.vec) {
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from != "" & from != " ")]
  return(from[grepl("@", from)][1])
}

get.msg <- function(msg.vec) {
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse="\n"))
}

get.subject <- function(msg.vec) {
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0) {
    return(strsplit(subj, "Subject: ")[[1]][2])
  }
  else {
    return("")
  }
}

get.date <- function(msg.vec) {
  date.grep <- grepl("^Date: ", msg.vec)
  date.grepl <- which(date.grep == TRUE)
  date <- msg.vec[date.grepl[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}
parse.email <- function(path) {
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs, function(p) parse.email(paste(easyham.path, p, sep="")))

ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.Email", "Subject", "Message", "Path")

date.converter <- function(dates, pattern1, pattern2) {
  pattern1.convert <- parse_date_time(dates, orders = pattern1, locale = "us") #wydaje się, że działa poprawnie, jednak w książce użyty jest po prostu strptime, który pobiera locale systemowe i na polskim komputerze zwróci same NA dla danych ze SpamAssassins
  pattern2.convert <- parse_date_time(dates, orders = pattern2, locale = "us")
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"
allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.Email <- tolower(allparse.df$From.Email)
priority.df <- allparse.df[with(allparse.df, order(Date)),]
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)),]
