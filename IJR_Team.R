# IJR Team
library(RCurl)
library(XML)
library(stringr)
library(data.table)

# download IJR team html
html <- getURL("https://about.ijr.com/team/", followlocation = TRUE)
doc = htmlParse(html, asText=TRUE)
h1vals <- xpathSApply(doc, "//h1", xmlValue) # First H1 is page title, omit [1]
h2vals <- xpathSApply(doc, "//h2", xmlValue)

Team.IJR <- data.table(name=str_trim(h1vals[2:length(h1vals)]), title=str_trim(h2vals))
setkey(Team.IJR, name)
Team.IJR <- Team.IJR[!duplicated(Team.IJR)] # Remove duplicates

# download IMGE team html (front page)
html <- getURL("https://imge.com", followlocation = TRUE)
doc = htmlParse(html, asText=TRUE)
divVals <- xpathSApply(doc, "//div[@class='col-xs-8 col-lg-9 text']", xmlValue)

Team.IMGE.FrontPage <- data.table(name=str_trim(divVals), title="Not Listed")
setkey(Team.IMGE.FrontPage, name)
Team.IMGE.FrontPage <- Team.IMGE.FrontPage[!duplicated(Team.IMGE.FrontPage)] # Remove duplicates

# download IMGE team html (unlinked team page)
html <- getURL("https://imge.com/meet-the-team/", followlocation = TRUE)
doc = htmlParse(html, asText=TRUE)
aVals <- xpathSApply(doc, "//a[@class='u-url']", xmlValue) # Last 3 used elsewhere, omit 83:85
h5Vals <- xpathSApply(doc, "//h5", xmlValue)

Team.IMGE <- data.table(name=str_trim(aVals[1:(length(aVals)-3)]), title=str_trim(h5Vals))
setkey(Team.IMGE, name)
Team.IMGE <- Team.IMGE[!duplicated(Team.IMGE)] # Remove duplicates

# grab IJR article authors
load(file="ijr.page.dat")
Team.IJR.Authors <- data.table(name=str_trim(unique(ijr.page$author)), title="Author")
Team.IJR.Authors <- Team.IJR.Authors[!is.na(name)] # Remove NAs
setkey(Team.IJR.Authors, name)

# Merge it all together
Team <- merge(Team.IMGE, Team.IMGE.FrontPage, all=TRUE)
Team <- merge(Team, Team.IJR, all=TRUE)
Team <- merge(Team, Team.IJR.Authors, all=TRUE)
names(Team) <- c('name', 'Title.IMGE', 'Title.IMGE.FrontPage', 'Title.IJR', 'Title.IJR.Authors')
