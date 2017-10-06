library(shiny)
library(data.table)
library(purrr)
library(splines)

source('polyfit2.R')

comma = function(x) {
  format(x, big.mark=',', scientific= FALSE)
}

load('data/cities50k.RData') # max(POP) > 50000
load('data/states.RData')

city00.16['Louisville/Jefferson County, KY', City:= 'Louisville, KY']

citySummary = city00.16[, .(
  State = STATE[1], Region = REGION[1],
  `2016 Population` = comma(POP[.N]),
  `Max Population` = comma(POP[which.max(POP)]),
  Rank = sizeRank[.N], `Region Rank` = sizeRank_region[.N]
), by='City'][
  order(Rank)]


ui = navbarPage(
  title = 'Population Growth Trajectories of U.S. Cities (Census)',
  footer = 'Data: https://www.census.gov/programs-surveys/popest.html',
  
  tabPanel(
    title = 'Main',
    
    tags$head(
      tags$script(src='https://d3js.org/d3.v4.min.js'),
      includeCSS('CityTrajd3.css'),
      includeScript('CityTrajd3.js')
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId='Region', label='Select Census region:',
                    choices=c('Northeast','Midwest','South','West')),
        
        checkboxGroupInput(inputId='States', label='Select states:',
                           choices=sort(c('DC', state.abb)),
                           selected=states$Northeast, inline=TRUE),
        
        numericInput(inputId='MinPop', label='Population larger than:',
                     value=2.5e5, min=5e4, max=999999999, step=1e4),
        
        numericInput(inputId='MaxPop', label='Population smaller than:',
                     value=999999999, min=5e4, max=999999999, step=1e4),
        
        selectInput(inputId='BaseYear', label='Select base year:',
                    choices=2000:2014, selected=2000),
        
        selectInput(inputId='ClipAtBase', label='Clip data at base year:',
                    choices=c(TRUE,FALSE)),
        
        selectInput(inputId='CurveFitMethod', label='Curve fitting method:',
                    choices=c(#'B-spline'='d3.curveBasis',
                      'Natural cubic spline',
                      'Polynomial (k = 5)',
                      #'Polynomial2 (match slopes at endpoints)'='Polynomial2',
                      'None')),
        
        width = 3
      ),
      
      mainPanel(
        div(id='d3io', class='d3io'),
        
        width = 9
      )
    )
  ),
  
  tabPanel(
    title = 'Table',
    fluidRow(
      column(2),
      column(8, DT::dataTableOutput(outputId='Table1'),
             style='font-size:85%'),
      column(2)
    )
  ),
  
  # Additional navbarPage() options
  fluid = TRUE, inverse = TRUE, position = 'fixed-top',
  windowTitle = 'shinyApp'
)

server = function (input, output, session) {
  
  observe({
    updateCheckboxGroupInput(
      session, 'States', selected=states[[input$Region]])
  })
  
  data= reactive({
    # filtering
    city00.16[city00.16[, .I[max(POP) >= input$MinPop & max(POP) <= input$MaxPop],
                        by='City']$V1][
                          STATE %in% input$States, .(City, YEAR, POP_Adj)]
  })
  
  json_data = reactive({
    dataC = copy(data())
    BaseYear = as.integer(input$BaseYear)
    ClipAtBase = as.logical(input$ClipAtBase)
    
    if (ClipAtBase) dataC = dataC[YEAR >= BaseYear]
    
    dataC[, POP_Idx:= 100 * POP_Adj / POP_Adj[YEAR==BaseYear],
          by='City']
    
    if (nrow(dataC) > 0 && !grepl('d3|None', input$CurveFitMethod)) {
      xgrid = seq(if (ClipAtBase) BaseYear else 2000, 2016, .1)
      
      if (input$CurveFitMethod=='Natural cubic spline') {
        xgridL = xgrid[xgrid <= BaseYear]
        xgridU = xgrid[xgrid >= BaseYear]
        
        fit = rbind(
          if (!ClipAtBase & BaseYear > 2000) dataC[YEAR <= BaseYear, .(
            YEAR = xgridL,
            POP_Idx = rev(predict(
              lm(I(rev(POP_Idx - 100)) ~ 0 + ns(
                YEAR - BaseYear, df=max(1, ceiling(.N/2)))
              ), newdata=data.frame(YEAR=xgridL)
            ) + 100
            )),
            by='City'],
          
          dataC[YEAR >= BaseYear, .(
            YEAR = xgridU,
            POP_Idx = predict(
              lm(I(POP_Idx - 100) ~ 0 + ns(
                YEAR - BaseYear, df=max(2, ceiling(.N/2)))
              ), newdata=data.frame(YEAR=xgridU)
            ) + 100
          ),
          by='City']
        )
        
      } else if (input$CurveFitMethod=='Polynomial (k = 5)') {
        
        fit = dataC[, .(
          YEAR = xgrid,
          POP_Idx = predict(
            lm(I(POP_Idx - 100) ~ 0 + poly(YEAR - BaseYear, 5, raw=TRUE)),
            newdata=data.frame(YEAR=xgrid)
          ) + 100
        ),
        by='City']
        
      } else if (input$CurveFitMethod=='Polynomial2') {
        
        fit = dataC[, .(
          YEAR = xgrid,
          POP_Idx = predict(
            polyfit2(YEAR - BaseYear, POP_Idx - 100, k=5, lambda=7),
            newX = xgrid - BaseYear) + 100
        ),
        by='City']
      }
      
      dataC = dataC[, .(YEAR=as.double(YEAR), City)][
        fit, on=.(YEAR, City)]
    }
    
    dataC %>%
      split(by='City') %>%
      map(function(d) c(CITY=d$City[1], as.list(d[, .(YEAR, POP_Idx)]))) %>%
      unname() %>%
      list()
    
  })
  
  output$Table1 = DT::renderDataTable({
    DT::datatable(
      citySummary, options=list(dom='ft', paging=FALSE, ordering=FALSE)
    )
  })
  
  observe({session$sendCustomMessage('jsondata', json_data())})
  
  observe({session$sendCustomMessage('CurveFitMethod',
                                     list(input$CurveFitMethod))})
  
}

shinyApp(ui=ui, server=server)
