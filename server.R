# This function aims to help commidities purchaser to ensure what kind of 
# item is valuable and worth for replenishment.
#
# Returns :
#   A table depends on which serach word users typed.

library(shiny)
library(RGA)
library(data.table)
library(rvest)
library(plyr)
library(stringr)

shinyServer(function(input, output) {
    
  authorize()
  
  observeEvent(input$Start, {
    hide("Start")
  })
  
  observeEvent(input$nextone, {
    hide("nextone")
  })
  
  datafile <- eventReactive(input$Start, {
    alter <- get_ga(profileId = "91337451", start.date = input$querydate[1],
           end.date = input$querydate[2], metrics = "ga:uniquePurchases,
           ga:productDetailViews, ga:productAddsToCart",
           dimensions = "ga:productName") %>% setDT
    alter[, productName := 
            str_replace_all(productName, "(\\[.*?\\])|\\【.*?\\】", "") %>%
            trimws]
    alter <- alter[, .(uniquePurchases = sum(uniquePurchases),
                       productDetailViews = sum(productDetailViews),
                       productAddsToCart = sum(productAddsToCart)), productName]
    alter[, `:=`(buyToDetailRate = uniquePurchases / productDetailViews * 100,
                 cartToDetailRate = productAddsToCart / productDetailViews
                 * 100)]
    alter[is.nan(buyToDetailRate) | is.nan(cartToDetailRate),
          `:=`(buyToDetailRate = 0, cartToDetailRate = 0)] %>% return
    
  })
  
  AllProduct <- eventReactive(input$Start, {
    querylist1 <- paste0("http://www.bonnyread.com.tw/products?page=",
                        1 : input$AllProductPage)
    TempQurey1 <- lapply(querylist1, read_html)
    AllProductName <- lapply(TempQurey1, function(k) {
      k %>% html_nodes(xpath = "//div[@class='title text-primary-color']") %>%
        html_text(trim = TRUE)
    }) %>% unlist %>% str_replace_all(., "(\\[.*?\\])|\\【.*?\\】", "") %>%
      trimws
    AllProductPrice <- lapply(TempQurey1, function(k) {
      k %>% html_nodes(xpath =
            "//div[@class='global-primary dark-primary price '] |
            //div[@class='price-sale price']") %>%
        html_text(trim = TRUE) %>% gsub("NT$", "", ., fixed = TRUE) %>%
        gsub(",", "", ., fixed = TRUE)
    }) %>% unlist %>% as.integer
    data.table(productName = AllProductName,
               AllProductPrice = AllProductPrice) %>% return
  })
  
  LatestProductName <- eventReactive(input$Start, {
    querylist2 <- paste0(
      "http://www.bonnyread.com.tw/categories/newarrival?page=",
      1 : input$LatestProductPage)
      lapply(querylist2, function(k) { k %>% read_html %>%
          html_nodes(xpath = "//div[@class='title text-primary-color']") %>%
          html_text(trim = TRUE)}) %>% unlist %>% 
        str_replace_all(., "(\\[.*?\\])|\\【.*?\\】", "") %>% trimws
  })
  
  CompletedFile <- eventReactive(input$Start, {
    retrieve <- datafile()
    tmpfile <- retrieve[AllProduct(), on = "productName", nomatch = 0] 
    tmpfile <- tmpfile[!productName %in% LatestProductName()]
    
    tmpfile[grepl(c("戒指", "對戒", "戒組", "尾戒", "關節戒", "連指戒",
                     "情侶戒", "三件戒", "開口戒") %>% paste(collapse = "|"),
                   productName) & (! grepl("盒", productName)),
             category := "戒指"]
    tmpfile[grepl(c("耳環", "耳針", "耳扣", "耳夾", "耳骨環") %>%
                     paste(collapse = "|"), productName),
             category := "耳環"]
    tmpfile[grepl(c("項鍊", "鎖骨鍊", "頸鍊", "頸圈") %>%
                     paste(collapse = "|"), productName),
             category := "項鍊"]
    tmpfile[grepl(c("手鍊", "手環", "手鐲") %>% paste(collapse = "|"),
                   productName), category := "手鍊"]
    tmpfile[grepl(c("髮飾", "髮帶", "髮圈", "髮夾", "髮箍") %>%
                     paste(collapse = "|"), productName),
             category := "髮飾"]
    tmpfile[grepl("手錶", productName), category := "手錶"]
    tmpfile[grepl("刺青貼紙", productName),
             category := "刺青貼紙"]
    tmpfile[grepl("腳鍊", productName), category := "腳鍊"]
    tmpfile[grepl("眼鏡|墨鏡", productName) & (! grepl("盒", productName)),
             category := "眼鏡"]
    tmpfile[is.na(category), category := "其它"]
    
    tmpfile[, `:=`(standardize1 = log10(uniquePurchases),
                   standardize2 = log10(buyToDetailRate),
                   standardize3 = log10(cartToDetailRate))]
    CleanOutList <- tmpfile[buyToDetailRate == 0 | cartToDetailRate == 0,
                            productName]
    tmpfile <- tmpfile[! (buyToDetailRate == 0 | cartToDetailRate == 0)]
    tmpfile[, PriceRange := findInterval(AllProductPrice, c(
      quantile(min(AllProductPrice) : max(AllProductPrice), 0.33),
      quantile(min(AllProductPrice) : max(AllProductPrice), 0.66))), category]
    tmpfile[, PriceRange := mapvalues(PriceRange, c(0, 1, 2),
                                      c("Low Price", "Moderate Price",
                                        "High Price"))]
    tmpfile[, `:=`(standardize1 = 
                     (standardize1 - mean(standardize1)) / sd(standardize1),
                   standardize2 = 
                     (standardize2 - mean(standardize2)) / sd(standardize2),
                   standardize3 = 
                     (standardize3 - mean(standardize3)) / sd(standardize3)),
            .(category, PriceRange)]
    tmpfile[, result := standardize1 + standardize2 * 0.7 + standardize3 * 0.3]
    tmpfile[, result := findInterval(result, c(0, 1.5, 2, 3))]
    tmpfile[, result := mapvalues(result, 0 : 4, c("Rough", "unrecommendable",
                                                   "Controversial", 
                                                  "Preferable", "Exceptional"))]
    tmpfile[category == "其它" | is.na(result), result := "無判斷"]
    tmpfile[, Potential := ifelse(standardize2 > 2 | standardize3 >2,
                     "Potential Challenger", " NoBody")] %>% return
  })
  
  FilteringNameFile <- reactive({
    tmpfile2 <- CompletedFile()
    if (input$CheckingName != "") {
      if (grepl(input$CheckingName, tmpfile2$productName) %>% any) {
        tmpfile2 <- tmpfile2[grepl(input$CheckingName, productName),
                  .(productName, result, Potential)] %>% return          
      } else {
        data.table(productName = input$CheckingName, Message = "未知資訊")
      }
    } else {
      tmpfile2[, .(productName, result, Potential)] %>% return
    }
  })
  
  output$pikachu <- renderDataTable({
    FilteringNameFile()
  })

})
