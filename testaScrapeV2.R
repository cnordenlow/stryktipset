# Ej klart, mer avancerat forsok
# Kanske battre dock
# install.packages('RSelenium')
library(rvest)
library(dplyr)
library(tidyr)
library(RSelenium)
library(stringr)

xpathOmsattning = '/html/body/div[2]/div/div/div[2]/div[2]/div/div/div[2]/div/div[2]/div[1]/div/div/div[1]/div/div/div/div[2]/div[2]'
xpathJackpot = ''

# vinstfordelning
# 13 ratt = 40% + ev. jackpot, chans = 1:1 594 323
# 12 ratt = 15%, chans = 1:61 320
# 11 ratt = 12%, chans = 1:5 110
# 10 ratt = 25%, chans = 1:697
# Garantifond
# Ensam 13 ratt = 10 miljoner



rm(list=setdiff(ls(), c('strykTipset', 'downloadStryktipset')))

strykTipset <- downloadStryktipset('1_4636')

# Om man vill spara ner resultatet har jag kort sa har
# Sa kan man kommentera bort raden ovanfor nar man testar, sa man inte behover ladda ner varje gang
#saveRDS(strykTipset, file = 'stryket.rds')
#strykTipset <- readRDS(file = 'stryket.rds')

tempMatches <- strykTipset %>% html_nodes('.js-match.match-header') %>%
  html_text(trim = TRUE)
tempOdds <- strykTipset %>% html_nodes('.statistics-box') %>%
  html_text(trim = TRUE) %>% matrix(ncol = 3, byrow = TRUE) %>%
  data.frame() %>% rename('1' = X1, X = X2, '2' = X3)
tempOmsattning <- strykTipset %>%
  html_nodes(xpath = xpathOmsattning) %>%
  html_text()
tempOmsattning %>% str_replace_all('[a-zA-Z]', '') %>%
  str_replace_all('[\u0000-\u007F]', '')

odds <- slice(tempOdds,  row_number = seq(1,26,2))
svenskaFolket <- slice(tempOdds, row_number = seq(2,26,2))
lag <- tempMatches %>% data.frame() %>%
  separate(col = '.', into = c('Lag1', 'Lag2'), sep = ' - ')






# downloadStryktipset
# output: pageSourceMain = page source of stryktipset
# input: idString = the last part of the link (e.g. 1_4636)
#        browserName = name of your browser (e.g. chrome)
#        portName = what port you want to use
downloadStryktipset <- function(idString, browserName = 'firefox', portName = 4446L) {
  rD <- rsDriver(port = portName, browser = browserName)
  
  # start the remote client
  remDr <- rD$client
  remDr$open()
  
  
  # define the url
  strykTipsetUrl <- 'https://spela.svenskaspel.se/stryktipset/' %>%
    paste(idString, sep = '')
  
  # navigate to the url
  remDr$navigate(strykTipsetUrl)
  
  # wait until load
  Sys.sleep(5)
  
  # get the page source
  pageSourceMain <- remDr$getPageSource()
  
  # close servers
  remDr$close()
  rD$server$stop()
  
  pageSource <- read_html(pageSourceMain[[1]])
  return(pageSource)
}
