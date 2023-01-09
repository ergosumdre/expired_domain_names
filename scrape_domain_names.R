get_expired_domains <- function(email, password, keyword){
  library(RSelenium)
  library(dplyr)
  library(httr)
  library(rvest)
  library(stringr)
  library(readr)
  
  
  
  ### IF there is an error while bring up rsDriver, delete the file: LICENSE.<something> located here:
  ### /Users/dre/Library/Application Support/binman_chromedriver/mac64/109.0.5414.25/LICENSE.chromedriver
  ### 
  ### 
  login_page <- "https://justdropped.com/next.cgi?file=login.nrml"
  url <- login_page
  rD <- rsDriver(browser="firefox", port=8010L, verbose=F)
  
  
  
  remDr <- rD[["client"]]
  remDr$navigate(url = url)
  # Sys.sleep(5) # give the page time to fully load
  html <- remDr$getPageSource()[[1]]
  
  
  ### LOGIN INFO ####
  login_page <- remDr$getPageSource()[[1]]
  remDr$findElement(using = 'name', 'emaillogin')$sendKeysToElement(list(email))
  remDr$findElement(using = 'name', 'passcode')$sendKeysToElement(list(password))
  submit_button <- remDr$findElement(using = "css selector", "tr:nth-child(6) input")
  submit_button$clickElement()
  
  
  
  # SEARCH FOR EXPIRED DOMAINS 
  query <- keyword
  remDr$findElement(using = 'name', 'searchcontains1')$sendKeysToElement(list(query)) # domain contains
  remDr$executeScript("window.scrollBy(0,10000);") # scroll to bottom of page
  remDr$findElement(using = "xpath", 
                    "/html/body/center[4]/div[2]/form/center/table/tbody/tr/td[3]/table/tbody/tr/td/table/tbody/tr[12]/td/input")$clickElement() # click search button
  
  Sys.sleep(15)
  
  
  
  
  # get domain names
  page_list <- remDr$getPageSource()[[1]]
  parse_domains <- function(x){
    expired_domains <- read_html(x) %>% # parse HTML
      html_nodes("b") %>% # type of table/class and title of table/class
      rvest::html_text()
    expired_domains <- expired_domains[which(grepl("com", expired_domains) == TRUE)] # filter on .coms
    return(expired_domains)
  }
  
  
  
  
  ### total_page logic. Gets total number of domains, then divide by num displayed (50)
  num_of_results <- str_locate(page_list, "Total Domains in List: ") 
  num_of_results <- stringr::str_sub(page_list, start = num_of_results[1], end = num_of_results[2] + 20) %>% parse_number()
  
  
  if(num_of_results < 50){
    total_pages <- 1
  } else{
    total_pages <- round(num_of_results / 50 )
    }
  html_pages <- list()
  domains <- list()
  
  ## Currently breaks if total pages is 1. 
  if(total_pages > 1){
    for (i in 1:total_pages) {
      print(paste0("Page: ", i, " of ", total_pages))
      html_pages[[i]] <- remDr$getPageSource()[[1]]
      domains[[i]] <- parse_domains(html_pages[[i]])
      remDr$executeScript("window.scrollBy(0,10000);")
      if(i != total_pages){
        next_page <- remDr$findElement(using = "link text", "Next")
        next_page$clickElement()
        Sys.sleep(2)
      }else{
        print("Finished")
      }
    }
    
  }else{
    print(total_pages)
    html_pages[[i]] <- remDr$getPageSource()[[1]]
    domains[[i]] <- parse_domains(html_pages[[i]])
  }
  
  
  domains <- unlist(domains)
  domains <- data.frame(domain_names = domains)
  domains <- domains[-which(str_detect(domains$domain_names, "@") == TRUE),]
  domains <- data.frame(domain_names = domains)
  domains <- unique(domains)
  return(domains)
  
  
  
}

# use
domains_expired_domains <- get_expired_domains(email_address, password, keyword)




