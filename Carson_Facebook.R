# Facebook Connection
library(devtools)
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(stringr)
install_github("pablobarbera/Rfacebook/Rfacebook") # from GitHub
install.packages("urltools")

library(Rfacebook)
library(urltools)

getDomainAndTime <- function (dt)
{
  dt$domain <- domain(dt$link)
  dt$domain <- sub('www.','',dt$domain)
  dt$domain <- as.factor(dt$domain)
  dt$created_time <- as.POSIXct(strptime(dt$created_time, format="%Y-%m-%dT%H:%M:%S%z", tz="UTC"))
  dt$campaign <- param_get(dt$link,c('utm_campaign'))
  return(dt)
}

getSource <- function(dt)
{
  dt$link_source <- as.character(NA)
  dt[domain=="ijr.com"]$link_source <- "Independent Journal Review (IJR)"
  dt[domain=="ijreview.com"]$link_source <- "Independent Journal Review (IJR)"
  dt[domain=="injo.com"]$link_source <- "Independent Journal Review (IJR)"
  dt[domain=="opinion.injo.com"]$link_source <- "Independent Journal Review (IJR)"
  dt[domain=="journal.ijreview.com"]$link_source <- "Independent Journal Review (IJR)"
  dt$link_source <- as.factor(dt$link_source)
  return(dt)
}

tok <- fbOAuth(1111111111111111, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') # Enter your Facebook OAuth credentials

carson.page <- as.data.table(getPage("realbencarson", tok, n = 5000, since='2016/01/01', until='2016/12/31'))
carson.page.all <- as.data.table(getPage("realbencarson", tok, n = 5000, since='2016/01/01', until='2017/02/24'))
setkey(carson.page, id)

carson.page <- getDomainAndTime(carson.page)

# Categorize link source for IJR links
carson.page <- getSource(carson.page)

# Make a list of events
carson.events <- data.table(event=c(
  "Carson Ends Campaign",
  "Carson Endorses Trump",
  "Trump Wins Primary",
  "Trump Wins Election"),date=c(
  as.POSIXct(strptime("2016-03-04 00:00:00", "%Y-%m-%d %H:%M:%S")),
  as.POSIXct(strptime("2016-03-11 00:00:00", "%Y-%m-%d %H:%M:%S")),
  as.POSIXct(strptime("2016-07-19 00:00:00", "%Y-%m-%d %H:%M:%S")),
  as.POSIXct(strptime("2016-11-09 00:00:00", "%Y-%m-%d %H:%M:%S"))
))

# Let's take a look at the percentage of posts that originate from different domains
carson.page.by.domain <- carson.page[is.na(domain)==FALSE & domain!='facebook.com',.(.N),by=domain][order(-N)]
carson.page.by.domain$domain <- factor(page.by.domain$domain, levels = page.by.domain$domain)
g <- ggplot(data=carson.page.by.domain[1:10], aes(domain, N, fill=N))
g <- g + geom_bar(stat='identity')
g <- g + ggtitle('Top Domains')
g <- g + labs(title='Top 10 External Websites Linked To By realbencarson Facebook, 2016',
              x='', y='Number of Posts')
g <- g + theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)),
               axis.text.x = element_text(angle = 60, hjust = 1, size = rel(1.5)),
               legend.position='None')
g <- g + scale_y_continuous(breaks = seq(0, 
                                         ceiling(max(carson.page.by.domain$N)/25)*25,
                                         by = 25))
g

# Let's see how pervasive IJR stories from Dr. Carson are
dat <- melt(carson.page[link_source=="Independent Journal Review (IJR)"], 
            id.vars=c("id","created_time","campaign"), 
            measure.vars=c("likes_count","comments_count","shares_count"))
dat[is.na(dat$campaign)]$campaign <- 'None'
carson.page$cum_likes_count <- cumsum(carson.page$likes_count)
carson.page$cum_comments_count <- cumsum(carson.page$comments_count)
carson.page$cum_shares_count <- cumsum(carson.page$shares_count)
dat.cum <- melt(carson.page[link_source=="Independent Journal Review (IJR)"], 
            id.vars=c("id","created_time"), 
            measure.vars=c("cum_likes_count","cum_comments_count","cum_shares_count"))

dat$variable <- revalue(dat$variable, c("likes_count"="Likes", "comments_count"="Comments", "shares_count"="Shares"))
g <- ggplot()
g <- g + geom_point(data=dat, aes(x=created_time, y=value, color=campaign, shape=campaign))
g <- g + facet_grid(variable ~ ., scales="free")
g <- g + geom_vline(aes(xintercept = as.numeric(date)), data=carson.events, colour="red", linetype = "longdash")
g <- g + geom_text(aes(x=date, y=0, label=event), data=carson.events, angle=90, hjust = "left")
g <- g + scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(x="Date", y="Count")
g

# Cumulative Data
dat.cum$Post <- revalue(dat.cum$variable, c("cum_likes_count"="Likes", "cum_comments_count"="Comments", "cum_shares_count"="Shares"))
g <- ggplot()
g <- g + geom_area(data=dat.cum, aes(x=created_time, y=value, fill=Post))
g <- g + geom_point(data=carson.page[link_source=="Independent Journal Review (IJR)"], 
                    aes(x=created_time,
                        y=cum_likes_count+cum_shares_count+cum_comments_count),
                        #shape=campaign,
                        shape=3, 
                        alpha=0.5)
g <- g + geom_vline(aes(xintercept = as.numeric(date)), data=carson.events, colour="red", linetype = "longdash")
g <- g + geom_text(aes(x=date, y=0, label=event), data=carson.events, angle=90, hjust = "left")
g <- g + scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
g <- g + scale_y_continuous(labels = comma)
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + labs(x="Date", y="Count")
g
