library(shiny)
library(rhandsontable)
library(plotly)



fluidPage(
  titlePanel("CPPI Advanced Analysis"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput('file1', 'Upload Assets Prices',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 fileInput('file2', 'Upload Risk-Free Rate (%)',
                           accept = c('text/csv',
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),
                 numericInput("eonia", "Risk-Free Rate (%)", 0, step = 0.1),
                 dateRangeInput("date", "Date Range"),
                 br(),
                 downloadButton('report.pdf', "Generate report"),
                 br(),
                 br(),
                 checkboxInput("reset", strong("Reset")),
                 checkboxInput("rrs", strong("Risk Regime Shift")),
                 conditionalPanel(
                   condition= "input.rrs == true",
                   strong("Cushion value when:"),
                   sliderInput("cushion.threshold1", h5("Dynamic -> Balanced"), 
                               min = 0, max =10, value = 6.7, step = 0.1, ad),
                   sliderInput("cushion.threshold2", h5("Balanced -> Cautious"), 
                               min = 0, max =10, value = 3.3, step = 0.1)),
                 br(),
                 selectInput("prot.type", "Protection Type", c("NAV Max" = "prot.nav.max",
                                                               "Last Month NAV" = "l.m.nav",
                                                               "Initial NAV" = "ini.nav")),
                 numericInput("prot", "Capital Protected (%)", 90, min=0, max=100),
                 numericInput("fees", "Fees (%)", 1.3, min=0, max=100, step=0.1),
                 numericInput("expo.cut", "Cut Margin (%)", 0.3, min=0, max=10, step=0.05),
                 br(),
                 checkboxInput("curhedge", strong("Currency Hedging")),
                 conditionalPanel(
                   condition= "input.curhedge == true",
                   selectInput("ptfcur", label = "Portfolio Currency", 
                               choices = list("EUR" = 1, 
                                              "USD" = 2, 
                                              "JPY" = 3, 
                                              "GBP" = 4,
                                              "HKD" = 5,
                                              "SGD" = 6,
                                              "TWD" = 7), 
                               selected = 1),
                   br(),
                   
                   numericInput('eur.rate', "Currency hedging cost in EUR", 0, min = -100, max = 100, step = 0.1),
                   fileInput('eur.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),
                   
                   numericInput('usd.rate', "Currency hedging cost in USD", 0, min = -100, max = 100, step = 0.1),
                   fileInput('usd.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),
                   

                   numericInput('jpy.rate', "Currency hedging cost in JPY", 0, min = -100, max = 100, step = 0.1),
                   fileInput('jpy.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),
                   
                   numericInput('gbp.rate', "Currency hedging cost in GBP", 0, min = -100, max = 100, step = 0.1),
                   fileInput('gbp.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),

                   numericInput('hkd.rate', "Currency hedging cost in HKD", 0, min = -100, max = 100, step = 0.1),
                   fileInput('hkd.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),

                   numericInput('sgd.rate', "Currency hedging cost in SGD", 0, min = -100, max = 100, step = 0.1),
                   fileInput('sgd.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),
    
                   numericInput('twd.rate', "Currency hedging cost in TWD", 0, min = -100, max = 100, step = 0.1),
                   fileInput('twd.rate.histo', '',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv'))
                   
                   
                   ),
                 tags$hr()
                 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Assets Prices", tableOutput('prices')),
        tabPanel("Assets Features", 
                 sliderInput('datefeat', "", 
                             min = as.Date("2011/01/01","%Y/%m/%d"), 
                             max = as.Date("2015/02/02","%Y/%m/%d"), 
                             value = c(as.Date("2011/01/01","%Y/%m/%d"),
                                       as.Date("2015/02/02","%Y/%m/%d"))),
                splitLayout( style = "height:400px",
                             cellWidths = 600,
                             plotOutput('asset.ret'),
                             plotOutput('asset.sd')),
                splitLayout( cellWidths = 600,   
                             plotOutput('asset.sharpe'),
                             plotOutput('asset.var.cvar'))
                             ),
        tabPanel("Weights & Parameter of Loss",
                 fixedRow(column(2, rHandsontableOutput('asset.cur.table')),
                   column(7, rHandsontableOutput('alloc.table'),
                                  br(),
                                  actionButton('go', 'Start Back Test')),
                          column(3, br(),
                                 h4("Optimisation"),
                                 br(),
                                 actionButton('go4', 'Start Optimisation'),
                                 conditionalPanel(
                                   condition="input.go4%2==true", br(),
                                   sliderInput("alloc.risk", "Risk of the allocation ",
                                               min =0, max =10, value = 5, step = 0.1)))),
                 fixedRow(column(9, plotOutput('cor.mat')), 
                          column(3, sliderInput('datecor', "", 
                                                 min = as.Date("2011/01/01","%Y/%m/%d"), 
                                                 max = as.Date("2015/02/02","%Y/%m/%d"), 
                                                 value = c(as.Date("2011/01/01","%Y/%m/%d"),
                                                           as.Date("2015/02/02","%Y/%m/%d"))))),
       br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                 plotlyOutput('eff.alloc.plot')
        ),
        tabPanel("Back Test Results", 
                 tabsetPanel(
                   tabPanel("Graphs",
                            plotOutput('ptf.plot'),
                            plotOutput('drawdown.plot'),
                            plotOutput('cushion'),
                            plotOutput('risk.contirb')),
                   tabPanel("Risk Metrics", 
                            fixedRow(column(6, tableOutput('risk.metrics')),
                                     conditionalPanel(condition= "input.rrs == true",
                                                      column(6, h3("Risk Regime Shift"),
                                                             tableOutput('rrs.results')))),
                            plotOutput('histo')))),
        tabPanel("Monte Carlo Simulations", 
                 tabsetPanel(
                   tabPanel("Historical Data",
                            fixedRow(column(6, rHandsontableOutput('mc.para.table'),
                                            br(),
                                            actionButton('go2', 'GO!')),
                                     column(6, tableOutput('mc.results'))),
                            br(),
                            plotOutput('mc.plot'),
                            br(),
                            tableOutput('mc.proba'),
                            br(),
                            tableOutput('mc.dec')),
                   tabPanel("Empirically",
                            fixedRow(column(5, rHandsontableOutput('emp.para.table'),
                                            br(),
                                            rHandsontableOutput('eff.front.table'),
                                            br(),
                                            actionButton('go3', 'GO!')),
                                     column(4, numericInput("pl.vol","Volatility / Parameter of loss", 
                                                            2, step=0.1)),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     column(4, numericInput("risk.budget.max",
                                                            "Risk Budget Max", 
                                                            4, step=0.1)),
                                     column(6, tableOutput('sharpe.ratio')),
                                     column(7, plotOutput('front.eff.plot'))),
                            tableOutput('emp.results'),
                            plotOutput('emp.ptf.nav.plot'),
                            plotOutput('emp.cushion.plot'),
                            tableOutput('emp.ptf.nav.dec'),
                            tableOutput('emp.cushion.dec'),
                            tableOutput('emp.prob.mon')
                            
                            
                   ))
        )
      )
      
    )
    
  )
  
)


