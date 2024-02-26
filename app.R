library(dplyr)
library(ggplot2)
library(glue)
library(sass)
library(shiny)
library(shiny.fluent)
library(shiny.router)
library(tidyr)
library(purrr)
library(htmltools)
library(reactable)
library(reactablefmtr)
library(scales)
library(sparkline)
library(htmlwidgets)



makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

makeScript <- function(js) {
  htmltools::htmlDependency(
    name = "TagPickerExample",
    version = "0", # Not used.
    src = c(href = ""), # Not used.
    head = paste0("<script>", js, "</script>")
  )
}

filters <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    horizontal = F,
    tokens = list(childrenGap = 10),
    TextField.shinyInput("slider", defaultValue = "100000000", label = "Minimum Revenue", description = "in millions", deferredValidationTime = 100),
    TextField.shinyInput("sliderNI", defaultValue = "100000000", label = "Minimum Net Income", description = "in millions", deferredValidationTime = 100),
    TextField.shinyInput("slider_increaseR", defaultValue = "0.1", label = "YoY Revenue Growth Threshold", description = "e.g. 10% is 0.1", deferredValidationTime = 100),
    TextField.shinyInput("slider_increaseNI", defaultValue = "0.1", label = "YoY Net Income Growth Threshold", description = "e.g. 35% is 0.35", deferredValidationTime = 100),
  )
)

header <- tagList(
  img(src = "logo_sdd.png", class = "logo"),
  div(Text(variant = "xLarge", "U.S. Stock Screener"), class = "title"),
  CommandBar(
    items = list(
      CommandBarItem("New", "Add", subitems = list(
        CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:gerard@solucionsdedades.cat"),
        CommandBarItem("Calendar event", "Calendar", key = "calendarEvent", href = "https://calendly.com/cienciadedades/primer-contacte")
      ))
    ),
    style = list(width = "100%"))
  )

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'About', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
      list(name = 'CienciaDeDades.cat', url = 'http://cienciadedades.cat', key = 'cienciadedades', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by CienciaDeDades.cat", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at gerard@solucionsdedades.cat"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}

analysis_page <- makePage(
  "U.S. Stock Screener",
  "",
  div(
    Stack(
      tokens = list(childrenGap = 10),
      span(Text(variant = "xLarge", "Easily monitor more than >6,000 companies listed in the NASDAQ and NYSE")),
      span(Text(variant = "small", "Updated daily to never miss any new piece of business intelligence")),
      span(Text(variant = "xLarge", ""))
      ),
    Stack(
      horizontal = T,
      tokens = list(childrenGap = 10),
      makeCard(Text(variant = "xLarge","Filters"), filters, size = 3, style = "max-height: 400px"),
      makeCard(Text(variant = "xLarge","Greatest Momentum"), reactableOutput("table2"), size = 9)
      ),
    Stack(
      makeCard(Text(variant = "xLarge", "Latest data"), reactableOutput("table"), size = 12)
      )
    )
  )

card1 <- makeCard(
  "About me",
 Stack(
    Text("I developed ",Link(href="https://cran.r-project.org/web/packages/tidyedgar/index.html", "'tidyedgar', an R package (found in CRAN)"),
         ", to retrieve financial data from the US SEC's EDGAR. This shiny app showcases its potential."),
    Text("I began my professional career as a private equity analyst, so for me, this represents coming full circle. I hope you find it useful.")
  )
)


home_page <- makePage(
  "A U.S. stock screener built using R!",
  "Leveraging shiny.fluent and shiny.react",
  div(card1)
)

router <- router_ui(
  route("/", analysis_page ),
  route("other", home_page)
)



ui <- fluentPage(
  layout(router),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ))




server <- function(input, output, session) {
  router_server()
  
  table_df <- readRDS(paste0("summary_latest_", format(Sys.time(), tz = "UTC", "%Y-%m-%d"),".rds"))
  
  
  filtered_table <- reactive({
    
    df <- table_df %>%
      dplyr::select(-data.cik, -uom) %>%
      dplyr::mutate(
        revenue = revenue / 1e6,
        OperatingIncomeLoss = OperatingIncomeLoss / 1e6,
        net_income = net_income / 1e6,
        OperatingMargin = round(OperatingIncomeLoss / revenue, 4),
        change_R = pmin(pmax(change_R, -99.99), 99.99),
        change_NI = pmin(pmax(change_NI, -99.99), 99.99)
            ) %>%
      dplyr::filter(if(input$slider_increaseNI != "") (change_NI >= as.numeric(input$slider_increaseNI)) else TRUE) %>%
      dplyr::filter(if(input$slider_increaseR != "" ) (change_R >= as.numeric(input$slider_increaseR)) else TRUE) %>%
      dplyr::filter(if(input$slider != "" ) (revenue >= as.numeric(input$slider)) else TRUE) %>%
      dplyr::filter(if(input$sliderNI != "" ) (net_income >= as.numeric(input$sliderNI)) else TRUE)
    
    df
  })
  
  filtered_table_top <- reactive({
    dfa <- table_df %>%
      dplyr::select(-data.cik, -uom, -OperatingIncomeLoss, -change_OI, -av_oi) %>%
      dplyr::filter(net_income > 1e6,
                    change_R > 0.6,
                    year == 2023) %>%
      dplyr::mutate(revenue = revenue / 1e6,
                    net_income = net_income / 1e6,
                    change_R = pmin(pmax(change_R, -99.99), 99.99),
                    change_NI = pmin(pmax(change_NI, -99.99), 99.99)) %>%
      dplyr::arrange(desc(revenue)) %>%
      dplyr::slice_head(n = 20)
    
    dfa
  })
  
  
  
  
  output$table <- renderReactable({
    av_rev <- filtered_table() %>%
      dplyr::filter(!is.nan(change_R) & !is.infinite(change_R)) %>%
      dplyr::mutate(
        change_R = pmin(pmax(change_R, -30), 30),
      ) %>%
      dplyr::summarise(means = mean(change_R, na.rm = TRUE))
    
    av_ni <- filtered_table() %>%
      dplyr::filter(!is.nan(change_NI) & !is.infinite(change_NI)) %>%
      dplyr::mutate(
        change_NI = pmin(pmax(change_NI, -3), 6)
      ) %>%
      dplyr::summarise(means = mean(change_NI, na.rm = TRUE))
    
      reactable(filtered_table() %>% select(-weights),
              theme = nytimes(),
              highlight = TRUE,
              wrap = T,
              resizable = T,
              filterable = T,
              defaultPageSize = 20,
              defaultColDef = colDef(align= "center", footerStyle = list(fontWeight = "bold")),
              defaultSorted = list(revenue = "desc"),
              columns = list(
                data.entityName = colDef(name = "Company name", footer = "Total \n(nº companies)"),
                revenue = colDef(name = "Revenue (in Million)", format = reactable::colFormat(currency = "USD",
                                                                             separators = TRUE),
                             footer = function(values) paste0("$", formatC(sum(values, na.rm=T), format="f", digits=2, big.mark=","), "  (", formatC(sum(!is.na(values)), format="f", digits=0, big.mark=","), ")")),
                OperatingIncomeLoss = colDef(name = "Operating Income (in Million)", format = reactable::colFormat(currency = "USD",
                                                                                              separators = TRUE),
                                 footer = function(values) paste0("$", formatC(sum(values, na.rm=T), format="f", digits=2, big.mark=","), "  (", formatC(sum(!is.na(values)), format="f", digits=0, big.mark=","), ")")),
                
                net_income = colDef(name = "Net Income (in Million)", format = reactable::colFormat(currency = "USD",
                                                                             separators = TRUE),
                                       footer = function(values) paste0("$", formatC(sum(values, na.rm=T), format="f", digits=2, big.mark=","), "  (", formatC(sum(!is.na(values)), format="f", digits=0, big.mark=","), ")")),
                OperatingMargin = colDef(name = "Operating Margin",
                                  format = colFormat(percent = TRUE, digits = 1),
                                  maxWidth = 75),
                change_R = colDef(name = "Last year",
                                  format = colFormat(percent = TRUE, digits = 1),
                                  maxWidth = 75,
                                  style = JS("function(rowInfo) {
      const value = rowInfo.values['change_R']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }"),
                footer = function() paste0(scales::percent(as.numeric(av_rev$means), accuracy = 0.1))),
                change_NI = colDef(name = "Last year",
                                   maxWidth = 75,
                                   format = colFormat(percent = TRUE, digits = 1),
                                     style = JS("function(rowInfo) {
      const value = rowInfo.values['change_NI']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }"),
                                   footer = function() paste0(scales::percent(as.numeric(av_ni$means), accuracy = 0.1))),
                change_OI = colDef(name = "Last year",
                                   maxWidth = 75,
                                   format = colFormat(percent = TRUE, digits = 1),
                                   style = JS("function(rowInfo) {
      const value = rowInfo.values['change_OI']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }")),
                
                av_rev = colDef(name = "Last 3 years",
                                  format = colFormat(percent = TRUE, digits = 1),
                                  maxWidth = 75,
                                  style = JS("function(rowInfo) {
      const value = rowInfo.values['av_rev']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }")),
                av_ni = colDef(name = "Last 3 years",
                                   maxWidth = 75,
                                   format = colFormat(percent = TRUE, digits = 1),
                                   style = JS("function(rowInfo) {
      const value = rowInfo.values['av_ni']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }")),
                av_oi = colDef(name = "Last 3 years",
                               maxWidth = 75,
                               format = colFormat(percent = TRUE, digits = 1),
                               style = JS("function(rowInfo) {
      const value = rowInfo.values['av_oi']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }")),
                year = colDef(name = "Financial Year", maxWidth = 65),
                data.end = colDef(name = "Last End Date", maxWidth = 85, format = colFormat(date = TRUE, locales = "en-GB"))#,
        ),
        columnGroups = list(
          colGroup(name = "Revenue Change (YoY)", columns = c("change_R", "av_rev")),
          colGroup(name = "Operating Income Change (YoY)", columns = c("change_OI", "av_oi")),
          colGroup(name = "Net Income Change (YoY)", columns = c("change_NI", "av_ni"))
        ))
  })
  
  output$table2 <- renderReactable({
    
    reactable(filtered_table_top() %>% select(-av_rev, -av_ni),
              theme = nytimes(),
              highlight = TRUE,
              wrap = T,
              resizable = T,
              filterable = T,
              defaultPageSize = 5,
              defaultColDef = colDef(align= "center", footerStyle = list(fontWeight = "bold")),
              defaultSorted = list(revenue = "desc"),
              columns = list(
                data.entityName = colDef(name = "Company name", footer = "Total \n(nº companies)"),
                revenue = colDef(name = "Revenue (in Million)", format = reactable::colFormat(currency = "USD",
                                                                                              separators = TRUE),
                                 footer = function(values) paste0("$", formatC(sum(values, na.rm=T), format="f", digits=2, big.mark=","), "  (", formatC(sum(!is.na(values)), format="f", digits=0, big.mark=","), ")")),
                net_income = colDef(name = "Net Income (in Million)", format = reactable::colFormat(currency = "USD",
                                                                                                       separators = TRUE),
                                       footer = function(values) paste0("$", formatC(sum(values, na.rm=T), format="f", digits=2, big.mark=","), "  (", formatC(sum(!is.na(values)), format="f", digits=0, big.mark=","), ")")),
                change_R = colDef(name = "Last year",
                                  format = colFormat(percent = TRUE, digits = 1),
                                  maxWidth = 75,
                                  style = JS("function(rowInfo) {
      const value = rowInfo.values['change_R']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }")),
                change_NI = colDef(name = "Last year",
                                   maxWidth = 75,
                                   format = colFormat(percent = TRUE, digits = 1),
                                   style = JS("function(rowInfo) {
      const value = rowInfo.values['change_NI']
      let color
      if (value > 0) {
        color = '#008000'
      } else if (value < 0) {
        color = '#e00000'
      } else {
        color = '#777'
      }
      return { color: color, fontWeight: 'bold' }
    }")),
                year = colDef(name = "Financial Year", maxWidth = 65),
                data.end = colDef(name = "Last End Date", maxWidth = 85, format = colFormat(date = TRUE, locales = "en-GB")),
                weights = colDef(name = "Revenue evolution", minWidth = 100, cell = function(value, index) {
                  sparkline::sparkline(filtered_table_top()$weights[[index]], width = 80, height = 30)
                })
              ),
              columnGroups = list(
                colGroup(name = "Revenue Change (YoY)", columns = c("change_R")),
                colGroup(name = "Net Income Change (YoY)", columns = c("change_NI"))
              ))
  })
  
  
}

shinyApp(ui, server)