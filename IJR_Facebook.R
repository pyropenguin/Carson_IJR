# Analysis of Independent Journal Review
library(devtools)
library(data.table)
library(Rfacebook)
library(urltools)


############# Compile Data from Scratch #############
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

ijr.page <- as.data.table(getPage("injoreview", tok, n = 5000, since='2016/01/01', until='2016/12/31'))
setkey(ijr.page, id)

ijr.page <- getDomainAndTime(ijr.page)
ijr.page <- getSource(ijr.page)

# Extract author from each IJR post
ijr.page$author <- as.character(NA)
for (postIdx in 1:nrow(ijr.page)) # DEBUG post <- ijr.page[1]
{
  post <- ijr.page[postIdx]
  if ((!is.na(post$link_source)) & 
      (post$link_source == "Independent Journal Review (IJR)"))
  {
    url <- post$link
    html <- getURL(url, followlocation = TRUE)
    doc <- htmlParse(html, asText=TRUE)
    author <- xpathSApply(doc, "//a[@class='opt360-author-name']", xmlValue)
    print(author)
    ijr.page[postIdx]$author <- author
  }
}
ijr.page$author <- as.factor(ijr.page$author)
save(ijr.page, file="ijr.page.dat")
################################################

############# Use Precompiled Data #############
load(file="ijr.page.dat")

