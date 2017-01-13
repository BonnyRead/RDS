

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),

  # Application title
  titlePanel("Replenishment Decision System"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", "
             #loadmessage {
                 position: fixed;
                 top: 0px;
                 left: 0px;
                 width: 100%;
                 padding: 5px 0px 5px 0px;
                 text-align: center;
                 font-weight: bold;
                 font-size: 100%;
                 color: #FFFFFF;
                 background-color: #289dd7;
                 z-index: 105;
                 }
                 "),
      conditionalPanel(condition = "input.nextone == 0",
      h3("Hi,歡迎使用存貨'輔助'系統,該系統用於給予使用者補貨時的參考依據
               ,切勿完全依賴該系統給予的資訊."),
      h4("該系統結果分為五級, Rough為最差勁
          的評比接著是Unrecommendable, 上述兩者皆為不建議後續的進貨除非有
          特殊原因,Controversial為中間具有些許爭議的商品,使用者可依其經驗
          或系統外之資訊對其下判斷, Preferable為推薦補貨商品而Exceptional
          為最為優秀的一類產品,應在任何情況下都繼續採用. 雜類產品因為其產
          品組成的關係,將不會在這裡對其做評價,若為其他類產品將會導出
          '無判斷'的結果."),
      h4("新功能 : 加入了尋找最具潛力的品項,在Potential這個欄位具有Potential 
         Challenger的商品將是較具可能性的商品,若輔以有效的廣告投遞或許能有效提
         升商品的銷售與評價")),
      actionButton("nextone", "NextPage", width = "100%"),
      conditionalPanel(condition = "input.nextone != 0", 
      dateRangeInput("querydate", "資料日期", start = Sys.Date() - 90,
                      end = Sys.Date(), max = Sys.Date()),
      numericInput("AllProductPage", "產品數量總頁次", value = 100),
      numericInput("LatestProductPage", "新品數量總頁次", value = 10),
      textInput("CheckingName", "檢查產品名稱"),
      actionButton("Start", "Launch", width = "100%")),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("執行中...請稍候", id="loadmessage"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("pikachu")
    )
  )
))
