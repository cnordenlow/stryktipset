#strykTipset <- read_html('https://spela.svenskaspel.se/stryktipset/')
# https://spela.svenskaspel.se/stryktipset/1_4633
# https://spela.svenskaspel.se/stryktipset/



# 
# strykTipsetLank = 'https://spela.svenskaspel.se/stryktipset/'
# strykTipsetId = '1_4633'
# strykTipsetUrl = paste(strykTipsetLank, strykTipsetLank, sep = '')
# strykTipset <- read_html(strykTipsetUrl)

# test <- html_nodes(strykTipset, '.statistics-box')
# strykTipset
# head(test)
# summary(test)
# test2 <- html_nodes(strykTipset, '.statistics-wrapper')
# test2
# test[1:2]
# test3 <- html_nodes(strykTipset, '.table .bold')
# test3 <- html_nodes(strykTipset, '.table')

# test[1:2]
# trim(test)
# testText <- html_text(test, trim = TRUE)
# testText
# 
# test[101:117]



# install.packages('RSelenium')
# Selenium ar paketet som anvands for att styra browsern fran R
library(rvest)
library(dplyr)
library(RSelenium)
library(stringr)
# checkForServer()
# RSelenium::rsDriver()

# Oppnar firefox (fick det inte att funka med chrome/nat annat)
rD <- rsDriver(port = 4446L, browser = 'firefox')
remDr <- rD$client
#remDr$close()
#rD$server$stop()
remDr$open()

# Vet inte om de sista siffrorna spelar nagan roll, jag tror man kan ta bort dem
# Man maste dock klicka acceptera cookies sjalv i nulaget (detta gar sakert att fixa med selenium)
# Sa att den klickar
tmp_url <- 'https://spela.svenskaspel.se/stryktipset/1_4633'
remDr$navigate(tmp_url)
# Vantar i 5 sek pga att man ska hinna klicka pa acceptera cookies, samt att den ska ladda oddsen
Sys.sleep(5)
# Sparar ner hemsidan inkl. odds
pageSourceMain <- remDr$getPageSource()
pageSourceMatches <- list()


# det har var ett forsok att fa ner extra info om matcherna
# dvs svenska folket osv
# matchLoop <- c(1:13)
# for (val in matchLoop) {
#   tempLinkSource <- paste('/html/head/script[', val, ']', sep = '')
#   test <- html_nodes(strykTipset, xpath = tempLinkSource)
#   extraURL <- paste(tmp_url, '/', matchLoop, sep = '')
#   remDr$close()
#   remDr$open()
#   remDr$navigate(extraURL)
#   Sys.sleep(5)
#   pageSourceMatches['val'] <- remDr$getPageSource
# }
# remDr$navigate(extraURL)
# 
# remDr$close()
# rD$server$stop()


strykTipset <- read_html(pageSourceMain[[1]])
# plockar ut matcherna
matcher <- html_nodes(strykTipset, '.js-match.match-header')
# plockar ut oddsen
odds <- html_nodes(strykTipset, '.statistics-box')
# av nagan anledning var detta ratt rader, kommer inte ihag varfor
odds[10:20]
# gor om till lasbart
testMatcher <- html_text(matcher, trim = TRUE)
testOdds <- html_text(odds, trim = TRUE)
testOdds

# vid det har laget har man oddsen och matcherna i matriser tror jag


# kommer inte ihag vad detta var
# testIgen <- html_nodes(strykTipset, '.block.align-center')
# tempLinkSource <- paste('/html/head/script[', 1, ']', sep = '')
# tempLinkSource2 <- html_nodes(strykTipset, xpath = tempLinkSource)
# 
# tempLinkSource3 <- as(tempLinkSource2, "character")
# tempLinkSource4 <- str_extract_all(tempLinkSource2[[1]], "\\[a-z]+(?= &amp;g)")
# pageName=stryktipset%3A1_4633%3A1-arsenal-sheffu
# tempLinkSource3 <- html_text(tempLinkSource2, trim = TRUE)
