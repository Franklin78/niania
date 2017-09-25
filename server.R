library(shiny)
library(ISOweek)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(rhandsontable)
library(plotly)
library(ROI)
require(ROI.plugin.quadprog)
library(quantmod)
library(gridExtra)
library(grid)
library(ggplot2)

library(forecast)
library(scales)
library(reshape2)
library(ggcorrplot)
library(plotrix)


function(input, output, session) {
  
var<-reactiveValues(cur.name=NULL,
                    asset.name=NULL,
                    asset.ret=NULL,
                    dt=NULL,
                    alloc=NULL,
                    eonia=NULL,
                    bt.para=NULL,
                    bt.dt=NULL, 
                    bt.ptf.nav=NULL, 
                    bt.nav.prot=NULL, 
                    bt.basket.nav=NULL, 
                    bt.basket.nav.gross=NULL,
                    bt.ptf.dd=NULL,
                    bt.basket.dd=NULL,
                    bt.expo=NULL,
                    bt.cushion=NULL,
                    bt.risk.metrics=NULL,
                    mc.para=NULL,
                    mc.basket.ret=NULL,
                    mc.percentil=NULL,
                    mc.ptf.nav.dec=NULL,
                    mc.cushion.dec=NULL,
                    mc.prob=NULL,
                    mc.result=NULL)

  ################ Prices
  output$prices <- renderTable({
    
    var$cur.name<-c("EUR", "USD", "JPY", "GBP", "HKD", "SGD", "TWD")
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    asset.price<-read.csv(inFile$datapath, sep=';')
    
    asset.price[,1]<-as.Date(asset.price[,1],format="%d/%m/%Y")
    
    date<-format.Date(asset.price[,1],format="%Y/%m/%d")
    row.names(asset.price)<-date
    
    date.min<-first(date)
    date.max<-last(date)
    
    asset.price<-subset(asset.price, date>=date.min&date<=date.max)
    n=nrow(asset.price)-1
    asset.price<-asset.price[,-1]

    asset.name<-colnames(asset.price)

    updateDateRangeInput(session, "date",
                          label = "Date Range",
                          start = date.min,
                          end = date[n],
                          min = date.min,
                          max = date.max)

    updateSliderInput(session, "datefeat", 
                      min =  as.Date(date.min), 
                      max = as.Date(date.max), 
                      value = c(as.Date(date.min), as.Date(date.max)))

    updateSliderInput(session, "datecor", 
                      min =  as.Date(date.min), 
                      max = as.Date(date.max), 
                      value = c(as.Date(date.min), as.Date(date.max)))


    var$asset.name<-asset.name

    asset.price

    }, include.rownames=T)
  
  ################ Assets features
  observeEvent(c(input$datefeat), {
    
    nd=252
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    asset.price<-read.csv(inFile$datapath, sep=';')
    for(j in (1:ncol(asset.price))) 
    {if (j==1)
    {asset.price[,j]=as.character(asset.price[,j])
    asset.price[,j]=as.Date(asset.price[,j],format="%d/%m/%Y") }
      else {asset.price[,j]=as.numeric(asset.price[,j])}} 
    
    date<-asset.price[,1]
    row.names(asset.price)<-date
    date.min<-as.Date(input$datefeat[1])
    date.max<-as.Date(input$datefeat[2])
    asset.price<-subset(asset.price, date>=date.min&date<=date.max)
    
    dt<-asset.price[-1,1]
    asset.price<-asset.price[,-1]
    n=nrow(asset.price)-1
    n.asset=ncol(asset.price)
    asset.ret<-CalculateReturns(asset.price)
    asset.ret<-asset.ret[-1,]
    asset.name<-colnames(asset.ret)
    
    
        # Asset Ret
    asset.ret.yr<-matrix(NA, nr=1, nc=n.asset)
    colnames(asset.ret.yr)<-asset.name
    for (j in 1:n.asset)
    {asset.ret.yr[,j] = (prod(1+asset.ret[,j])^(nd/n)-1)*100
    }

    output$asset.ret<-renderPlot({
      labels<-asset.name
      
     mp<- barplot(asset.ret.yr, 
              axes = FALSE, axisnames = FALSE,
              col = 'navy', 
              main = "Annualized Returns")
      
      text(mp, par("usr")[3], 
           labels = labels, 
           srt = 30, 
           adj = c(1.1,1.1), 
           xpd = TRUE, cex=.75)
      axis(2)
      
    },
    width = 600,
    height = 350)
      
    # Asset sd
    asset.sd.yr<-matrix(NA, nr=1, nc=n.asset)
    colnames(asset.sd.yr)<-asset.name
    for (j in 1:n.asset)
    {asset.sd.yr[,j] = sd(asset.ret[,j])*sqrt(nd)*100
    }
    
    output$asset.sd<-renderPlot({
      labels<-asset.name
      
      mp<- barplot(asset.sd.yr, 
                   axes = FALSE, axisnames = FALSE,
                   col = 'firebrick3', 
                   main = "Annualized Volatility")
      
      text(mp, par("usr")[3], 
           labels = labels, 
           srt = 30, 
           adj = c(1.1,1.1), 
           xpd = TRUE, cex=.75)
      axis(2)
      
    },
    width = 600,
    height = 350)

    
    # Asset sharpe
    asset.sharpe.yr<-matrix(NA, nr=1, nc=n.asset)
    colnames(asset.sd.yr)<-asset.name
    for (j in 1:n.asset)
    {asset.sharpe.yr[,j] = asset.ret.yr[,j] / asset.sd.yr[,j]*100
    }
    
    output$asset.sharpe<-renderPlot({
      labels<-asset.name
      
      mp<- barplot(asset.sharpe.yr, 
                   axes = FALSE, axisnames = FALSE,
                   col = 'gold', 
                   main = "Sharpe ratio")
      
      text(mp, par("usr")[3], 
           labels = labels, 
           srt = 30, 
           adj = c(1.1,1.1), 
           xpd = TRUE, cex=.75)
      axis(2)
      
    },
    width = 600,
    height = 350)
    
    
    # Asset VaR CVaR
    asset.var.cvar<-matrix(NA, nr=2, nc=n.asset)
    colnames(asset.var.cvar)<-asset.name
    row.names(asset.var.cvar)<-c("VaR 95", "CVaR 95")

    for (j in 1:n.asset)
    {asset.var.cvar[1,j] = VaR(asset.ret[,j], p=0.95, method = "historical", clean = "none")
    asset.var.cvar[2,j] = ETL(asset.ret[,j], p=0.95, method = "historical", clean = "none")
    }
    
    output$asset.var.cvar<-renderPlot({
      a<-asset.name
      b<-rep("", length(asset.name))
      labels<-c(a,b)[ order( c(seq_along(a), seq_along(b)))] 
      
      mp<- barplot(asset.var.cvar, 
                   beside=T,
                   legend = rownames(asset.var.cvar),
                   axes = FALSE, axisnames = FALSE,
                   col = c('navy', 'gold'),
                   main = "VaR and CVaR daily")
      
      text(mp, par("usr")[3], 
           labels = labels, 
           srt = 30, 
           adj = c(1.1,1.1), 
           xpd = TRUE, cex=.75)
      axis(2)
      
    },
    width = 600,
    height = 350)
    
    

    
    
    
    
    asset.ret.yr
    
    
    
  })
  
  #_______________________________________________________________________
  
  
  ################ Weights Table
  observeEvent(c(input$rrs), {
   
  output$alloc.table<-renderRHandsontable({

     asset.name<-var$asset.name
     n.row.alloc.table=length(asset.name)+1
     
     if (input$rrs==T)
       { alloc<-as.data.frame(matrix(0, nr=n.row.alloc.table, nc=3))
      row.names(alloc)<-c(asset.name,"Parameter of Loss")
      colnames(alloc)<-c("Cautious (%)", "Balanced (%)", "Dynamic (%)")
      rhandsontable(alloc, 
                    rowHeaderWidth = 250, 
                    colWidths = 100 )}
     
     else
       { alloc<-as.data.frame(matrix(0, nr=n.row.alloc.table, nc=1))
     row.names(alloc)<-c(asset.name,"Parameter of Loss")
     colnames(alloc)<-c("Weights (%)")
     rhandsontable(alloc, 
                   rowHeaderWidth = 250, 
                   colWidths = 100)}
     })
  })
  
  
  observeEvent(input$curhedge, {
    if (input$curhedge == T)
      {output$asset.cur.table<-renderRHandsontable({
   rhandsontable(data.frame(Currency = rep("USD",length(var$asset.name))), 
                 rowHeaders = FALSE)%>%
          hot_col(col = "Currency", type = 'dropdown', 
                  source = var$cur.name)
  
        })
    }
    else {return(NULL)}
    })
    
  
  ################################################

  observeEvent(c(input$date[1],
                 input$date[2],
                 input$asset.cur.table,
                 input$ptfcur,
                 input$usd.rate,
                 input$eur.rate,
                 input$jpy.rate,
                 input$gbp.rate,
                 input$hkd.rate,
                 input$sgd.rate,
                 input$twd.rate),
               { 
   nd = 252              
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  asset.price<-read.csv(inFile$datapath, sep=';')
  for(j in (1:ncol(asset.price))) 
  {if (j==1)
  {asset.price[,j]=as.character(asset.price[,j])
  asset.price[,j]=as.Date(asset.price[,j],format="%d/%m/%Y") }
    else {asset.price[,j]=as.numeric(asset.price[,j])}} 
  
  date<-asset.price[,1]
  row.names(asset.price)<-date
  date.min<-as.Date(input$date[1])
  date.max<-as.Date(input$date[2])
  asset.price<-subset(asset.price, date>=date.min&date<=date.max)
  
  dt<-asset.price[-1,1]
  asset.price<-asset.price[,-1]
  n=nrow(asset.price)-1
  n.asset=ncol(asset.price)
  asset.ret<-CalculateReturns(asset.price)
  asset.ret<-asset.ret[-1,]
  asset.name<-colnames(asset.ret)
  
  var$dt<-as.Date(dt)
 
  if (input$curhedge==F)
  {var$asset.ret<-asset.ret}
  
  else {
    one.mth.fwd<-c()
    df<-as.matrix(hot_to_r(input$asset.cur.table))
    asset.cur<-df
    cur.name<-var$cur.name

    #EUR
    eur.rate.file<-input$eur.rate.histo
    if (is.null(eur.rate.file))
    {eur.fwd<-as.matrix(rep(input$eur.rate,n))}
    else
    {eur.rate.histo<-read.csv(eur.rate.file$datapath, sep = ";")
    eur.rate.histo[,1] = as.character(eur.rate.histo[,1])
    eur.rate.histo[,1] = as.Date(eur.rate.histo[,1], format = "%d/%m/%Y")
    eur.rate.histo[,2] = as.numeric(eur.rate.histo[,2])
    eur.rate.histo<-subset(eur.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    eur.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==eur.rate.histo[(i+k), 1])
    {eur.fwd<-rbind(eur.fwd, eur.rate.histo[(i+k), ])}
      else {k = k+1
      if (dt[i]==eur.rate.histo[(i+k),1])
      {eur.fwd<-rbind(eur.fwd, eur.rate.histo[(i+k),])}
      else {k= k+1
      eur.fwd<-rbind(eur.fwd, eur.rate.histo[(i+k),])}
      }
    }
    eur.fwd<-eur.fwd[,-1]
    }
    
    
    #USD
    usd.rate.file<-input$usd.rate.histo
    if (is.null(usd.rate.file))
    {usd.fwd<-as.matrix(rep(input$usd.rate,n))}
    else
    {usd.rate.histo<-read.csv(usd.rate.file$datapath, sep = ";")
    usd.rate.histo[,1] = as.character(usd.rate.histo[,1])
    usd.rate.histo[,1] = as.Date(usd.rate.histo[,1], format = "%d/%m/%Y")
    usd.rate.histo[,2] = as.numeric(usd.rate.histo[,2])
    usd.rate.histo<-subset(usd.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    usd.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==usd.rate.histo[(i+k), 1])
    {usd.fwd<-rbind(usd.fwd, usd.rate.histo[(i+k), ])}
    else {k = k+1
    if (dt[i]==usd.rate.histo[(i+k),1])
    {usd.fwd<-rbind(usd.fwd, usd.rate.histo[(i+k),])}
    else {k= k+1
    usd.fwd<-rbind(usd.fwd, usd.rate.histo[(i+k),])}
      }
    }
    usd.fwd<-usd.fwd[,-1]
    }

    
    
    #JPY
    jpy.rate.file<-input$jpy.rate.histo
    if (is.null(jpy.rate.file))
    {jpy.fwd<-as.matrix(rep(input$jpy.rate,n))}
    else
    {jpy.rate.histo<-read.csv(jpy.rate.file$datapath, sep = ";")
    jpy.rate.histo[,1] = as.character(jpy.rate.histo[,1])
    jpy.rate.histo[,1] = as.Date(jpy.rate.histo[,1], format = "%d/%m/%Y")
    jpy.rate.histo[,2] = as.numeric(jpy.rate.histo[,2])
    jpy.rate.histo<-subset(jpy.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    jpy.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==jpy.rate.histo[(i+k), 1])
    {jpy.fwd<-rbind(jpy.fwd, jpy.rate.histo[(i+k), ])}
      else {k = k+1
      if (dt[i]==jpy.rate.histo[(i+k),1])
      {jpy.fwd<-rbind(jpy.fwd, jpy.rate.histo[(i+k),])}
      else {k= k+1
      jpy.fwd<-rbind(jpy.fwd, jpy.rate.histo[(i+k),])}
      }
    }
    jpy.fwd<-jpy.fwd[,-1]
    }
    
    
    #GBP
    gbp.rate.file<-input$gbp.rate.histo
    if (is.null(gbp.rate.file))
    {gbp.fwd<-as.matrix(rep(input$gbp.rate,n))}
    else
    {gbp.rate.histo<-read.csv(gbp.rate.file$datapath, sep = ";")
    gbp.rate.histo[,1] = as.character(gbp.rate.histo[,1])
    gbp.rate.histo[,1] = as.Date(gbp.rate.histo[,1], format = "%d/%m/%Y")
    gbp.rate.histo[,2] = as.numeric(gbp.rate.histo[,2])
    gbp.rate.histo<-subset(gbp.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    gbp.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==gbp.rate.histo[(i+k), 1])
    {gbp.fwd<-rbind(gbp.fwd, gbp.rate.histo[(i+k), ])}
      else {k = k+1
      if (dt[i]==gbp.rate.histo[(i+k),1])
      {gbp.fwd<-rbind(gbp.fwd, gbp.rate.histo[(i+k),])}
      else {k= k+1
      gbp.fwd<-rbind(gbp.fwd, gbp.rate.histo[(i+k),])}
      }
    }
    gbp.fwd<-gbp.fwd[,-1]
    }
    

    #HKD
    hkd.rate.file<-input$hkd.rate.histo
    if (is.null(hkd.rate.file))
    {hkd.fwd<-as.matrix(rep(input$hkd.rate,n))}
    else
    {hkd.rate.histo<-read.csv(hkd.rate.file$datapath, sep = ";")
    hkd.rate.histo[,1] = as.character(hkd.rate.histo[,1])
    hkd.rate.histo[,1] = as.Date(hkd.rate.histo[,1], format = "%d/%m/%Y")
    hkd.rate.histo[,2] = as.numeric(hkd.rate.histo[,2])
    hkd.rate.histo<-subset(hkd.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    hkd.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==hkd.rate.histo[(i+k), 1])
    {hkd.fwd<-rbind(hkd.fwd, hkd.rate.histo[(i+k), ])}
      else {k = k+1
      if (dt[i]==hkd.rate.histo[(i+k),1])
      {hkd.fwd<-rbind(hkd.fwd, hkd.rate.histo[(i+k),])}
      else {k= k+1
      hkd.fwd<-rbind(hkd.fwd, hkd.rate.histo[(i+k),])}
      }
    }
    hkd.fwd<-hkd.fwd[,-1]
    }

    #SGD
    sgd.rate.file<-input$sgd.rate.histo
    if (is.null(sgd.rate.file))
    {sgd.fwd<-as.matrix(rep(input$sgd.rate,n))}
    else
    {sgd.rate.histo<-read.csv(sgd.rate.file$datapath, sep = ";")
    sgd.rate.histo[,1] = as.character(sgd.rate.histo[,1])
    sgd.rate.histo[,1] = as.Date(sgd.rate.histo[,1], format = "%d/%m/%Y")
    sgd.rate.histo[,2] = as.numeric(sgd.rate.histo[,2])
    sgd.rate.histo<-subset(sgd.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    sgd.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==sgd.rate.histo[(i+k), 1])
    {sgd.fwd<-rbind(sgd.fwd, sgd.rate.histo[(i+k), ])}
      else {k = k+1
      if (dt[i]==sgd.rate.histo[(i+k),1])
      {sgd.fwd<-rbind(sgd.fwd, sgd.rate.histo[(i+k),])}
      else {k= k+1
      sgd.fwd<-rbind(sgd.fwd, sgd.rate.histo[(i+k),])}
      }
    }
    sgd.fwd<-sgd.fwd[,-1]
    }
    

    #TWD
    twd.rate.file<-input$twd.rate.histo
    if (is.null(twd.rate.file))
    {twd.fwd<-as.matrix(rep(input$twd.rate,n))}
    else
    {twd.rate.histo<-read.csv(twd.rate.file$datapath, sep = ";")
    twd.rate.histo[,1] = as.character(twd.rate.histo[,1])
    twd.rate.histo[,1] = as.Date(twd.rate.histo[,1], format = "%d/%m/%Y")
    twd.rate.histo[,2] = as.numeric(twd.rate.histo[,2])
    twd.rate.histo<-subset(twd.rate.histo, Date>=as.Date(first(dt))
                           &Date<=as.Date(last(dt)))
    k = 0
    twd.fwd<-c()
    for (i in 1:n)
    {if (dt[i]==twd.rate.histo[(i+k), 1])
    {twd.fwd<-rbind(twd.fwd, twd.rate.histo[(i+k), ])}
      else {k = k+1
      if (dt[i]==twd.rate.histo[(i+k),1])
      {twd.fwd<-rbind(twd.fwd, twd.rate.histo[(i+k),])}
      else {k= k+1
      twd.fwd<-rbind(twd.fwd, twd.rate.histo[(i+k),])}
      }
    }
    twd.fwd<-twd.fwd[,-1]
    }
    
    one.mth.fwd<-cbind(eur.fwd,
                       usd.fwd,
                       jpy.fwd,
                       gbp.fwd,
                       hkd.fwd,
                       sgd.fwd,
                       twd.fwd)
    
    colnames(one.mth.fwd)<-cur.name
    one.mth.fwd<-as.data.frame(one.mth.fwd)
    
    ptf.cur = cur.name[as.numeric(input$ptfcur)]
    ptfcur.rate = one.mth.fwd[ptf.cur]
    
    hedge.cost<-c()
    asset.ret.hedge<-c()
    for (j in 1:n.asset)
    {hedge.cost<-cbind(hedge.cost, as.matrix(one.mth.fwd[asset.cur[j]]))
    asset.ret.hedge<-cbind(asset.ret.hedge, asset.ret[,j] + hedge.cost[,j]/100/nd)}

    asset.ret.hedge<-as.matrix(asset.ret.hedge)
    rownames(asset.ret.hedge)<-rownames(asset.ret)
    colnames(asset.ret.hedge)<-asset.name
    var$asset.ret<-asset.ret.hedge

    }
 

  })
  
  ################ Correlation Matrix
observeEvent(c(input$datecor, 
             input$asset.cur.table,
             input$ptfcur,
             input$usd.rate,
             input$eur.rate,
             input$jpy.rate,
             input$gbp.rate,
             input$hkd.rate,
             input$sgd.rate,
             input$twd.rate), {
    output$cor.mat<-renderPlot({
      asset.ret<-as.data.frame(var$asset.ret)
      date<-row.names(asset.ret)
      date.min=input$datecor[1]
      date.max=input$datecor[2]
 
     asset.ret.sub<- subset(asset.ret, date>=date.min & date<=date.max)

      cor.mat<-cor(asset.ret.sub)
      
      ggcorrplot(cor.mat, hc.order = T,  insig = "blank", lab=T)
    
    
  }, 
 
  
  width = 800,
  height = 800

  )
})
 
  
  
  

   ################ Optimisation
   
  observeEvent(input$go4, {

     progress <- shiny::Progress$new()
     on.exit(progress$close())
       
     progress$set(message = "Making plot", value = 0) 
       
       if (input$go4==T)
        {
         
     asset.ret<-isolate(var$asset.ret)
     asset.name<-var$asset.name
     n=nrow(asset.ret)
     n.asset=ncol(asset.ret)
     mean.asset.ret<-colMeans(asset.ret)
     vcv.mat<-cov(asset.ret)

     #
     port<-portfolio.spec((assets=asset.name))
     
     port <- add.constraint(portfolio=port, type="long_only")
     
     port <- add.constraint(port, type="leverage", min_sum=0.99, max_sum=1.01)
    
     #
      rportfolios<-random_portfolios(port, permutations = 5000, 
                                    rp_method="sample", 
                                    eliminate=T)
     
      min.var.port<-add.objective(port, type = "risk", name = "var")
      
      min.var.opt<-optimize.portfolio(asset.ret, min.var.port, 
                                     optmize_method = "ROI", 
                                     rp = rportfolios)
      
      min.ret<-min.var.opt$weights%*%mean.asset.ret
      
     
      max.ret.port<-add.objective(port, type = "return", name = "mean")
      
      max.ret.opt<-optimize.portfolio(asset.ret, max.ret.port, 
                                     optimize_method = "random", rp = rportfolios)
      
      max.ret<-max.ret.opt$weights%*%mean.asset.ret

      #
      vec<-seq(min.ret, max.ret, length.out = 100)
      
      eff.front<-data.frame(eff.ret = rep(NA, length(vec)),
                            eff.risk = rep(NA, length(vec)),
                            eff.sharpe = rep(NA, length(vec)))
      eff.weights<-matrix(nr = length(vec), nc = n.asset)
      colnames(eff.weights)<-asset.name
      
      for(i in 1:length(vec)){
        eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
        eff.port <- add.objective(eff.port, type = "risk", name = "var")
        eff.port <- optimize.portfolio(asset.ret, eff.port, optimize_method = "ROI")
        eff.front$eff.risk[i] <- sqrt(t(eff.port$weights) %*% vcv.mat %*% eff.port$weights)
        eff.front$eff.ret[i] <- eff.port$weights %*% mean.asset.ret
        eff.front$eff.sharpe[i] <- eff.front$eff.ret[i]/eff.front$eff.risk[i] 
        eff.weights[i,] = eff.port$weights
        print(paste(round(i/length(vec) * 100, 0), "% done..."))
        progress$inc(length(vec), detail = paste("Doing part", i)) }
      
      
      eff.weights<-na.omit(eff.weights)
      eff.front<-na.omit(eff.front)
    
      front.weight.cumul<-c()
      front.weight.cumul<-eff.weights[,1]
      for (j in 2:n.asset)
      {front.weight.cumul<-cbind(front.weight.cumul,
                                 rowSums(eff.weights[,1:j]))}
      row.names(front.weight.cumul)<-eff.front[, 2]
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "Portfolio Allocation (%)")
      
      output$eff.alloc.plot<-renderPlotly({
        p<-plot_ly()
        
        for (j in 1:n.asset)
        {p<-add_trace(p, x=sqrt(eff.front[,2])*100, 
                      y=front.weight.cumul[,j]*100,
                      type="scatter",
                      mode="lines",
                      fill='tonexty',
                      yaxis = "y2",
                      name=asset.name[j],
                      yaxis=seq(0,1,0.1))}
        
        p<-add_lines(p, x=sqrt(eff.front[,2])*100, 
                     y=eff.front[, 1]*100*250,
                     type="scatter",
                     mode="lines",
                     name="Efficient Frontier",
                     line = list(color = 'rgb(205, 12, 24)', width = 4))
        p<-layout(p, title = "Efficient Allocations", 
                  yaxis2 = ay,
                  yaxis = list (title = "Annualized Returns (%)"),
                  xaxis = list(title="Volatility (%)"))
        p})
      
      risk.min <- round(sqrt(eff.front[1,2])*100,2)
      risk.max <- round(sqrt(eff.front[length(vec),2])*100,2)
      risk.mean = mean(c(risk.min,risk.max))
      
      updateSliderInput(session, "alloc.risk", value = risk.mean,
                        min = risk.min, max = risk.max, 
                        step = round((risk.max-risk.min)/99,2))
      
      
      output$alloc.table<-renderRHandsontable({
        
        risk.level<-(input$alloc.risk-risk.min)/(risk.max-risk.min)*99+1

        x<-t(c(eff.weights[risk.level,]*100, 0))
        nr.alloc=n.asset+1
        alloc<-as.data.frame(matrix(x, nr=nr.alloc, nc=1))
        row.names(alloc)<-c(asset.name,"Parameter of Loss")
        colnames(alloc)<-c("Weights (%)")
        rhandsontable(alloc, 
                      rowHeaderWidth = 210, 
                      colWidths = 100)})


      }

     })
     

   
  ################ Content of allocation matrix storage
   alloc.storage<-eventReactive(input$go,
                                {w.pl<-input$alloc.table})
   
  ################ Back Test Results

   output$ptf.plot <- renderPlot({ 
   
     w.pl <- hot_to_r(alloc.storage())

     n.asset=nrow(w.pl)-1
     
     t1<- ttheme_default(base_size = 5,
                         core= list(bg_params = list(fill=c(rep(c("grey95", "grey90"),
                                                length.out=n.asset),
                                            "skyblue"))))
     var$alloc<-tableGrob(w.pl,  
                          theme = t1)
   
  
    para.loss.table<-as.matrix(w.pl[nrow(w.pl),])
    asset.weight.table<-as.matrix(w.pl[-nrow(w.pl),])/100
  
    prot=isolate(input$prot/100)
    fees=isolate(input$fees/100)
    expo.cut=isolate(input$expo.cut/100)
    reset=isolate(input$reset)
    rrs=isolate(input$rrs)
    prot.type=isolate(input$prot.type)

    dt<-var$dt

    asset.ret<-as.matrix(var$asset.ret)
    n=nrow(asset.ret)

   inFile<-input$file2
    if (is.null(inFile))
    {EONIA<-as.matrix(rep(input$eonia/100, n)) }
    
   if (!is.null(inFile)){
      EONIA.upload<-read.csv(inFile$datapath, sep=';')
    for(i in (1:ncol(EONIA.upload))) 
    {if (i==1) {EONIA.upload[,i]=as.character(EONIA.upload[,i])
    EONIA.upload[,i]=as.Date(EONIA.upload[,i],format="%d/%m/%Y")}
      else {EONIA.upload[,i]=as.numeric(EONIA.upload[,i])}}
    
    EONIA.upload=subset(EONIA.upload,Date>=as.Date(first(dt))&Date<=as.Date(last(dt)))
    
    k=0
    EONIA<-c()
    for(i in 1:n)
    {if(dt[i]==EONIA.upload[(i+k),1])
      {EONIA<-rbind(EONIA,EONIA.upload[(i+k),])}
      else
      {k=k+1
      if(dt[i]==EONIA.upload[(i+k),1])
      {EONIA<-rbind(EONIA,EONIA.upload[(i+k),])}
      else{k=k+1
      EONIA<-rbind(EONIA,EONIA.upload[(i+k),])}}}
    remove(k, EONIA.upload)
    EONIA<-EONIA[,2]
    EONIA<-data.frame(EONIA/100)}
   
   var$eonia<-EONIA

    date.start.year<-c()
    date.end.year<-c()
    for (i in (1:length(dt)))
    {if (i==1) {date.start.year[i]=min(dt)}
      else { {if (i==length(dt)) {date.end.year<-c(date.end.year,max(dt))}
        else {if (format(dt[i],"%Y")>format(dt[i-1],"%Y"))
        {date.end.year<-c(date.end.year,dt[i-1])
        date.start.year<-c(date.start.year,dt[i])
        date.end.year<-as.Date(date.end.year)
        date.start.year<-as.Date(date.start.year)}}}}}
   
    date.start.month<-c()
    date.end.month<-c()
    for (i in (1:length(dt)))
    {if (i==1) {date.start.month[i]=min(dt)}
      else { {if (i==length(dt)) {date.end.month<-c(date.end.month,max(dt))}
        else {if (format(dt[i],"%m")>format(dt[i-1],"%m"))
        {date.end.month<-c(date.end.month, dt[i-1])
        date.start.month<-c(date.start.month, dt[i])
        date.end.month<-as.Date(date.end.month)
        date.start.month<-as.Date(date.start.month)}}}}}

    nd=252
    n.rs=0
    n.dr=0
    n.br=0
    n.cr=0
 
    
    asset.weight.start<-asset.weight.table[,ncol(asset.weight.table)]
    para.loss.start<-para.loss.table[,ncol(para.loss.table)]

    
    M0<-matrix(NA, nrow=n, ncol=1)
    
    
    expo<-M0
    nd.expo=1
    
    ptf.ret<-M0
    ptf.nav<-M0
    ptf.pl<-M0
    ptf.navmax<-M0
    ptf.dd<-M0
    ptf.navmax.dd<-M0
    ptf.l.m.nav<-M0
    
    nav.prot<-M0
    cushion<-M0
    cushion.conso<-M0
    
    basket.ret<-M0
    basket.nav<-M0
    basket.nav.gross<-M0
    basket.navmax<-M0
    basket.dd<-M0
    basket.pl<-M0
    
    if (rrs==T)
    {cushion.threshold<-c(input$cushion.threshold1, input$cushion.threshold2)/100}
    
    progress <- Progress$new(session, min=1, max=n)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress')
    
    
    for (i in 1:n)
      
    {progress$set(value = i)
      
      if (i==1){ basket.pl[i]=para.loss.start
    basket.ret[i]=asset.ret[i,]%*%as.matrix(asset.weight.start)
    basket.nav[i]=100
    basket.nav.gross[i]=basket.nav[i]
    basket.navmax[i]=basket.nav[i]
    basket.dd[i]=(basket.nav[i]/basket.navmax[i]-1)*100            
    expo[i]=1
    ptf.ret[i]=expo[i]*basket.ret[i]
    ptf.nav[i]=expo[i]*basket.nav[i]
    ptf.navmax[i]=ptf.nav[i]
    ptf.navmax.dd[i]=ptf.nav[i]
    ptf.l.m.nav[i]=ptf.nav[i]
    nav.prot[i]=prot*ptf.navmax[i]
    cushion[i]=(ptf.nav[i]-nav.prot[i])/100-0.005
    ptf.pl[i]=expo[i]*basket.pl[i]/100 
    cushion.conso[i]=ptf.pl[i]/cushion[i]
    ptf.dd[i]=(ptf.nav[i]/ptf.navmax[i]-1)*100}
      
    if (i>1) { if (cushion[i-1]>=basket.pl[i-1]/100) { expo[i]=1 }
    else { if (cushion[i-1]<=expo.cut) {expo[i]=0 }
          else { expo[i]=cushion[i-1]*100/basket.pl[i-1] } 
          if (expo[i]<=0) { expo[i]=0 }}
      if (expo[i]==1){nd.expo=nd.expo+1}

      if( rrs==T)  
      {if (i<=5) 
      {asset.weight<-asset.weight.start
      para.loss<-para.loss.start}
        else {XX<-c()
        if(ISOweek(dt[i])!=ISOweek(dt[i-1]))    
        {for (j in 1:5)
        { if(ISOweek(dt[i-j])==ISOweek(dt[i-1]))
        {XX=rbind(XX, cushion[i-j])
        if (mean(XX)>=cushion.threshold[1])
        { asset.weight<-as.matrix(asset.weight.table[,3])
        para.loss<-para.loss.table[3]}
        if (mean(XX)<cushion.threshold[1] & mean(XX)>=cushion.threshold[2])
        { asset.weight<-as.matrix(asset.weight.table[,2])
        para.loss<-para.loss.table[2]}
        if (mean(XX)<cushion.threshold[2])
        { asset.weight<-as.matrix(asset.weight.table[,1])
        para.loss<-para.loss.table[1]}}}}}} 

      else {asset.weight<-asset.weight.start
      para.loss<-para.loss.start}
      
      basket.pl[i]=para.loss
      
      if (basket.pl[i-1]!=basket.pl[i]){n.rs=n.rs+1}
    if(rrs==T)
      {if (basket.pl[i]==para.loss.table[3]){n.dr=n.dr+1}
      if (basket.pl[i]==para.loss.table[2]){n.br=n.br+1}
      if (basket.pl[i]==para.loss.table[1]){n.cr=n.cr+1}}
      

      basket.ret[i]=asset.ret[i,]%*%as.matrix(asset.weight)
      
      basket.nav.gross[i]=basket.nav.gross[i-1]*(1+basket.ret[i])
      basket.nav[i]=basket.nav[i-1]*(1+basket.ret[i]-fees/nd)
      
      ptf.ret[i]=expo[i]*basket.ret[i]+(1-expo[i])*EONIA[i,1]/nd
      
      if (ptf.nav[i-1]>nav.prot[i-1])
      {ptf.nav[i]=ptf.nav[i-1]*(1+ptf.ret[i]-fees/nd)}
      else {if (ptf.nav[i-1]<nav.prot[i-1]){ptf.nav[i]=ptf.nav[i-1]}
        else {ptf.nav[i]=nav.prot[i-1]}}
      
      for (j in 1:length(date.end.month))
      {if (dt[i]==date.end.month[j])
        { ptf.l.m.nav[i]=ptf.nav[i] }
      if (dt[i]>=date.start.month[j]&&dt[i]<date.end.month[j]) 
        {ptf.l.m.nav[i]=ptf.l.m.nav[i-1]}
      }
      
      if (reset==F)
      {ptf.navmax[i]=max(ptf.nav[i], ptf.navmax[i-1])
       ptf.dd[i]=(ptf.nav[i]/ptf.navmax[i]-1)*100
       if (prot.type=="prot.nav.max")
       {nav.prot[i]=prot*ptf.navmax[i]}
       if (prot.type=="l.m.nav")
       {nav.prot[i]=prot*ptf.l.m.nav[i]}
       if (prot.type=="ini.nav")
       {nav.prot[i]=prot*ptf.nav[1]}
       cushion[i]=(ptf.nav[i]-nav.prot[i])/ptf.nav[i]-fees/4}
      
      
      else {for (j in (1:length(date.start.year)))
      { if (dt[i]==date.start.year[j]) 
        { ptf.navmax[i]=ptf.nav[i]
          nd.yr=0 }
        if (dt[i]>date.start.year[j]&dt[i]<=date.end.year[j])
        { ptf.navmax[i]=max(ptf.navmax[i-1], ptf.nav[i])
        nd.yr = date.end.year[j]-dt[i]}}
        
        ptf.navmax.dd[i]=max(ptf.nav[i], ptf.navmax.dd[i-1])
        ptf.dd[i]=(ptf.nav[i]/ptf.navmax.dd[i]-1)*100 
        if (prot.type==c("prot.nav.max"))
        {nav.prot[i]=prot*ptf.navmax[i]}
        if (prot.type==c("l.m.nav"))
        {nav.prot[i]=prot*ptf.l.m.nav[i]}
        if (prot.type==c("ini.nav"))
        {for (j in (1:length(date.start.year)))
        {if (dt[i]==date.start.year[j])
        {nav.prot[i]=prot*ptf.nav[i]}
         if (dt[i]>date.start.year[j]&dt[i]<=date.end.year[j]) 
            {nav.prot[i]=nav.prot[i-1]}}}
        cushion[i]=(ptf.nav[i]-nav.prot[i])/ptf.nav[i]-fees*nd.yr/nd
        
        }
      
     
      
      

      ptf.pl[i]=expo[i]*basket.pl[i]/100 
      
      cushion.conso[i]=ptf.pl[i]/cushion[i]
      
      basket.navmax[i]=max(basket.navmax[i-1],basket.nav[i])   
      
      basket.dd[i]=(basket.nav[i]/basket.navmax[i]-1)*100
    
      
      }
      
   
       
       }

     
    if (n<nd)
    {ptf.ret.daily=prod(1+ptf.ret)^(1/n)-1
    basket.ret.daily=prod(1+basket.ret)^(1/n)-1  
    ptf.ret.yearly=((1+ptf.ret.daily)^nd-1)*100  
    basket.ret.yearly=((1+basket.ret.daily)^nd-1)*100}
    else {ptf.ret.yearly=(ptf.nav[n]-100)*nd/n
    basket.ret.yearly=(basket.nav[n]-100)*nd/n}
    
    ptf.ddmax=min(ptf.dd)
    ptf.t.ddmax=first(which(ptf.dd==ptf.ddmax))
    ptf.ddmax.date=dt[ptf.t.ddmax]
    ptf.recov.date1=format.Date(dt[last(which(subset(ptf.dd, dt<ptf.ddmax.date)==0))],format="%d/%m/%Y")
    ptf.recov.date2=format.Date(dt[first(which(subset(ptf.dd, dt>ptf.ddmax.date)==0))+ptf.t.ddmax], format="%d/%m/%Y")
    
    
    ptf.recov=dt[first(which(subset(ptf.dd, dt>ptf.ddmax.date)==0))+ptf.t.ddmax]-
      dt[last(which(subset(ptf.dd, dt<ptf.ddmax.date)==0))]
    if (is.na(ptf.recov)){ptf.recov<-c("-")}  

    basket.ddmax=min(basket.dd)
    basket.t.ddmax=first(which(basket.dd==basket.ddmax))
    basket.ddmax.date=dt[which(basket.dd==basket.ddmax)]
    basket.recov.date1=format.Date(dt[last(which(subset(basket.dd, dt<basket.ddmax.date)==0))],format="%d/%m/%Y")
    basket.recov.date2=format.Date(dt[first(which(subset(basket.dd, dt>basket.ddmax.date)==0))+basket.t.ddmax], format="%d/%m/%Y")
  
    basket.recov=dt[first(which(subset(basket.dd, dt>basket.ddmax.date)==0))+basket.t.ddmax]-
      dt[last(which(subset(basket.dd, dt<basket.ddmax.date)==0))]
    if (is.na(basket.recov)){basket.recov<-c("-")}
    
    ptf.vol=sd(ptf.ret[,1])*sqrt(nd)
    basket.vol=sd(basket.ret[,1])*sqrt(nd)
    
    ptf.var=VaR(ptf.ret[,1], p=0.95, method = "historical", clean = "none")
    basket.var=VaR(basket.ret[,1], p=0.95, method = "historical", clean = "none")
    
    ptf.cvar=ETL(ptf.ret[,1], p=0.95, method = "historical", clean = "none")
    basket.cvar=ETL(basket.ret[,1], p=0.95, method = "historical", clean = "none")
    
    
    mone.date=format.Date(dt[first(which(expo==0))],format="%d/%m/%Y")
    if(is.na(mone.date)){mone.date<-c("-") }
      
    gap=ptf.nav[n]-nav.prot[n]
    
    percent.expo=nd.expo/n
    
    ######### Graphs
    
    if(reset==T){rst<-c("with")}
    else {rst<-c("without")}
    if(rrs==T){rrs.<-c("with")}
    else {rrs.<-c("without")}
    
    
    plot(ptf.nav[,1]~dt,
         type="l", col="blue4", lwd=3,
         ylim=c(min(nav.prot, basket.nav),max(ptf.nav[,1], basket.nav.gross[,1])),
         xlab="", ylab="", 
         main=paste("NAV of the Fund,", rst, "reset and", rrs., "risk regime shift"))
    
    lines(nav.prot[,1]~dt,type="l", col="red3",lwd=2)
    lines(basket.nav[,1]~dt,type="l",col="deepskyblue", lwd=1)
    lines(basket.nav.gross[,1]~dt,type="l",col="deepskyblue", lwd=1, lty = 4)
    
    legend(x="topleft", 
           legend=c("Fund NAV", "Net Basket NAV","Gross Basket NAV", "Protection"), 
           bty = "n", col=c("blue4","deepskyblue","deepskyblue","red3"), 
           cex=1, lwd = c(3), lty=c(1,1,4,1) ) 
    
   
    
   output$drawdown.plot<-renderPlot({ plot(ptf.dd[,1]~dt,
         type='l',col="blue4", lwd=3,
         ylim=c(min(ptf.dd[,1], basket.dd[,1]),0),
         xlab="", ylab="",
         main="Drawdown of the Fund and Basket")
    lines(basket.dd[,1]~dt,type="l",col="deepskyblue", lwd=1)
    legend("bottomleft",
           legend=c("Fund", "Basket"),
           bty = "n", col=c("blue4", "deepskyblue"), cex=1, lwd=c(3))})
   

      
      output$cushion<-renderPlot({
        data<-data.frame(dt, cushion[,1], expo[,1],2)
        par(mar = c(5,5,2,5))
        with(data, plot(dt, cushion[,1], type="l", col="red3", lwd=2,
                        main="Risk budget and exposition",
                     xlab="",ylab=c("Risk budget"), ylim=c(-0.005,max(cushion[,1]))))
        par(new = T)
        with(data, plot(dt, expo[,1], type='l', col="olivedrab",lwd=2,
                        axes=F, xlab=NA, ylab=NA))
        axis(side = 4)
        mtext(side = 4, line = 3, 'Exposition')
        legend("bottomleft",
               legend=c("Risk budget", "Exposition"),
               bty = "n", col=c("red3", "olivedrab"), cex=1, lwd=c(3)) })
    
      ######### Risk Metrics
      
      Parameters<-c("Performance over the period",
                   "Annualized return",
                   "Annualized volatility",
                   "Sharpe Ratio",
                   "Max drawdown",
                   "Date of max drawdown",
                   "Time of recovery (days)",
                   "Percentage of time fully exposed",
                   "VaR daily (95%)",
                   "CVaR daily (95%)",
                   "Date of monetarisation")
  
      if(gap<0){Parameters<-c(Parameters, "Gap")}
      
      Fund<-c(paste(round(ptf.nav[n]-100,3), "%"),
                   paste(round(ptf.ret.yearly,3), "%"),
                   paste(round(ptf.vol*100,3), "%"),
              round(ptf.ret.yearly/(ptf.vol*100),2),
              round(ptf.ddmax,2),
              format.Date(ptf.ddmax.date,format="%d/%m/%Y"),
              ptf.recov,
              paste(round(percent.expo*100,2),"%"),
              paste(round(ptf.var*100,3), "%"),
              paste(round(ptf.cvar*100,3), "%"),
              mone.date )

      if(gap<0){Fund<-c(Fund, round(gap,2))}
      
      Basket<-c(paste(round(basket.nav[n]-100,3), "%"),
                     paste(round(basket.ret.yearly,3), "%"),
                     paste(round(basket.vol*100,3), "%"),
                     round(basket.ret.yearly/(basket.vol*100),2),
                     round(basket.ddmax,2),
                     format.Date(basket.ddmax.date,format="%d/%m/%Y"),
                     basket.recov,
                     "-",
                     paste(round(basket.var*100,3), "%"),
                     paste(round(basket.cvar*100,3), "%"),
                     "-")
      
      if(gap<0){Basket<-c(Basket, "-")}
      
      risk.metrics.table<-as.data.frame(cbind(Parameters, Fund, Basket))
     
       output$risk.metrics<-renderTable({risk.metrics.table})
    
      output$rrs.results<-renderTable({
        Parameters<-c("Number of regime shifts",
                          "Proportion of time with the dynamic regime",
                          "Proportion of time with the balanced regime",
                          "Proportion of time with the cautious regime")
        
        Values<-c(n.rs,
                  paste(round(n.dr/n*100,2),"%"),
                  paste(round(n.br/n*100,2),"%"),
                  paste(round(n.cr/n*100,2),"%"))
                           
        rrs.results.table<-as.data.frame(cbind(Parameters, Values))
        
      }) 
       
       
       output$histo<-renderPlot({
      hist(ptf.ret[,1], breaks=n/10, col="blue4",
           main="Distribution of fund returns",
           ylab="Frequence",xlab="", probability = F)})

   
      var$bt.dt<-dt 
      var$bt.ptf.nav<-ptf.nav
      var$bt.nav.prot<-nav.prot
      var$bt.basket.nav<-basket.nav
      var$bt.basket.nav.gross<-basket.nav.gross
      var$bt.ptf.dd<-ptf.dd
      var$bt.basket.dd<-basket.dd
      var$bt.expo<-expo
      var$bt.cushion<-cushion
      
      colours<-matrix("white", nr=nrow(risk.metrics.table), nc=ncol(risk.metrics.table))
      t1<- ttheme_default(base_size = 8,
                          core = list(bg_params = list(fill=colours)))
      var$bt.risk.metrics<-tableGrob(risk.metrics.table, 
                                     rows=NULL,  
                                     theme = t1)
      
      
#
      if (is.constant(EONIA[,1])==T){rfr<-paste(round(EONIA[1,1]*100,2), "%")}
      else {rfr<-c("historical")}
      
      if (reset == T){rst<-c("with")}
      else {rst<-c("without")}
      
      if (prot.type==c("prot.nav.max")) {prt.tp<-c("NAV max")}
      if (prot.type==c("l.m.nav")) {prt.tp<-c("Last month NAV")}
      if (prot.type==c("ini.nav")) {prt.tp<-c("Initial NAV")}
      
      prot.val<-paste(prot*100, "%")
      
      fees.val<-paste(fees*100, "%")
      
      cut.margin<-paste(expo.cut*100, "%")

      bt.para<-cbind(rfr,
                     rst,
                     prt.tp,
                     prot.val,
                     fees.val,
                     cut.margin)
      colnames(bt.para)<-c("Risk-free rate",
                           "Reset",
                           "Protection type",
                           "Protection",
                           "Fees",
                           "Cut margin")
     
      colours<-matrix("white", nr=nrow(bt.para), nc=ncol(bt.para))
      t2<- ttheme_default(base_size = 8,
                          core = list(bg_params = list(fill=colours)))
      var$bt.para<-tableGrob(bt.para,  
                           theme = t2)
      
   # PTF volatility
      ptf.vol=as.numeric(sqrt(t(asset.weight.start)%*%cov(asset.ret)%*%asset.weight.start))
      
   # Asset marginal contribution to volatility
      asset.mcr<-cov(asset.ret)%*%asset.weight.start/ptf.vol

   # Asset contribution to volatility  
      asset.cr<-asset.weight.start*asset.mcr
      
   # Asset percent contribution to volatility  
      asset.pcr<-round(asset.cr/ptf.vol*100,0)

    # Asset contribution to performance
      asset.ret.yr<-c()
      asset.cp<-c()
      for (i in 1:ncol(asset.ret))
      {asset.ret.yr<-rbind(asset.ret.yr, prod(1+asset.ret[,i])) }
      
      
      asset.cp<-asset.weight.start*asset.ret.yr
      basket.ret.gross<-asset.weight.start%*%t(asset.ret)
      basket.ret.gross.yr<-prod(1+basket.ret.gross[1,])
      asset.pcp<-asset.cp*100/basket.ret.gross.yr
      
      asset.pcrp<-cbind(asset.pcp, asset.pcr)
      colnames(asset.pcrp)<-c("Contribution to performance",
                              "Contribution to volatility")
      
      
      output$risk.contirb<-renderPlot({
        par(mar=c(5,10,4,2)+0.1,mgp=c(5,1,0))
       mp<-barplot(t(asset.pcrp), 
                   beside = T,
                   legend = colnames(asset.pcrp), 
                   col = c('navy', 'firebrick3'),
                   horiz=T,
                   cex.names=1,
                   las=2)
              },
        width = 1000,
        height = 500)
      
      
      
       })
   
   
   #================================================
   #           Monte Carlo configuration
   #================================================
   
   output$mc.para.table<-renderRHandsontable({
     mc.para<-as.data.frame(matrix(c(100,252), nr=2, nc=1))
     row.names(mc.para)<-c("Number of observations", "Number of simulated days")
     colnames(mc.para)<-c("Values")
     rhandsontable(mc.para,
                   rowHeaderWidth = 200, 
                   colWidths = 100)
      })
   
   ################ Content of Monte Carlo table storage
   
   mc.para.storage<-eventReactive(input$go2,
                                  {mc.para<-input$mc.para.table})
   

   ################ Monte Carlo simulation

     output$mc.plot <- renderPlot({ 
       input$go2
       
       withProgress(message = 'Making plot', value = 0, { 
       
       mc.para <- hot_to_r(mc.para.storage())
     
      
      n.obs=mc.para[1,1]
      n.days=mc.para[2,1]
      
      w.pl <- alloc.storage()
      if (!is.null(input$alloc.table)) 
        w.pl <- hot_to_r(input$alloc.table)

      para.loss.table<-as.matrix(w.pl[nrow(w.pl),])
      asset.weight.table<-as.matrix(w.pl[-nrow(w.pl),])/100

      prot=isolate(input$prot/100)
      fees= isolate(input$fees/100)
      expo.cut=isolate(input$expo.cut/100)
      reset=isolate(input$reset)
      rrs=isolate(input$rrs)
      prot.type=isolate(input$prot.type)

      dt<-var$dt
      asset.ret<-as.matrix(var$asset.ret)
      n=nrow(asset.ret)
      
      basket.return<-matrix(NA, nr=n, nc=ncol(asset.weight.table))
      for (j in 1:ncol(basket.return))
      {basket.return[,j]<-asset.ret%*%as.matrix(asset.weight.table[,j])}

      
      
      EONIA<-var$eonia
      
      
      
      #####
      
      M0<-matrix(NA, nr=n.days, nc=n.obs)

      expo<-M0
      
      ptf.ret<-M0
      ptf.nav<-M0
      ptf.pl<-M0
      ptf.navmax<-M0
      ptf.dd<-M0
      ptf.l.m.nav<-M0
      
      nav.prot<-M0
      cushion<-M0
      cushion.conso<-M0
      cushion.week<-c()
      basket.ret<-M0 ###!!!!!!! basket.ret est different de basket.return
      basket.nav<-M0
      basket.pl<-M0
      basket.navmax<-M0
      basket.dd<-M0
      
      nd=252
      
      epsilon=log(n.obs)/log(10)
      
      M1<-matrix(NA, nrow=1, ncol=n.obs)
      
      ptf.ddmax<-M1
      ptf.vol<-M1
      basket.vol<-M1
      ptf.ret.yr<-M1
      perc.expo<-M1
     
      de.expo=0
      
      gap<-c()
 
      if (rrs==T)
      {cushion.threshold<-c(input$cushion.threshold1, input$cushion.threshold2)/100}
      
      for (k in 1:n.obs)
      {n.expo=0
      
        EONIA.mc<-matrix(NA, nr=n.days, nc=1)
        
        pdf_EONIA<-density(EONIA,
                           bw = "nrd0", 
                           adjust = 1,
                           kernel = c("gaussian"),
                           weights = NULL, window = kernel, width,
                           give.Rkern = FALSE,
                           n = n, cut =20, na.rm = FALSE)
        
        EONIA.mc <- approx(cumsum(pdf_EONIA$y)/sum(pdf_EONIA$y), 
                           pdf_EONIA$x,
                           runif(n.days))$y
        
        basket.ret.mc<-matrix(NA, nr=n.days, nc=ncol(asset.weight.table))
        for (j in 1:ncol(basket.return))
        {  pdf_basket.ret<-density(basket.return[,j],
                                   bw = "nrd0", 
                                   adjust = 1,
                                   kernel = c("gaussian"),
                                   weights = NULL, window = kernel, width,
                                   give.Rkern = FALSE,
                                   n = n, cut =20, na.rm = FALSE)
        basket.ret.mc[,j] <- approx(cumsum( pdf_basket.ret$y)/sum( pdf_basket.ret$y), 
                                    pdf_basket.ret$x,
                                    runif(n.days))$y
        }
        
        
        basket.ret.start<-basket.ret.mc[,ncol(basket.ret.mc)]
        asset.weight.start<-asset.weight.table[,ncol(asset.weight.table)]
        para.loss.start<-para.loss.table[,ncol(para.loss.table)]

        
        for (i in 1:n.days)
        {if (i==1)
        { basket.pl[i,k]=para.loss.start 
        basket.ret[i,k]=basket.ret.start[i]
        basket.nav[i,k]=100
        basket.navmax[i,k]=basket.nav[i,k]
        expo[i,k]=1
        ptf.ret[i,k]=expo[i,k]*basket.ret[i,k]
        ptf.nav[i,k]=100
        ptf.navmax[i,k]=ptf.nav[i,k]
        ptf.l.m.nav[i,k]=ptf.nav[i,k]
        nav.prot[i,k]=ptf.navmax[i,k]*prot
        cushion[i,k]=(ptf.nav[i,k]-nav.prot[i,k])/100-0.005
        ptf.pl[i,k]=expo[i,k]*basket.pl[i,k]/100 
        cushion.conso[i,k]=ptf.pl[i,k]/cushion[i,k]
        basket.dd[i,k]=(basket.nav[i,k]/basket.navmax[i,k]-1)*100
        ptf.dd[i,k]=(ptf.nav[i,k]/ptf.navmax[i,k]-1)*100
        cushion.week<-rbind(cushion.week, cushion[i,k]) }
          
          if (i>1)
          {if (cushion[i-1,k]>=basket.pl[i-1,k]/100) { expo[i,k]=1 }
            else { if (cushion[i-1,k]<=expo.cut) { expo[i,k]=0 }
              else { expo[i,k]=cushion[i-1,k]*100/basket.pl[i-1,k] } 
              if (expo[i,k]<=0) { expo[i,k]=0 } }
            
            
            if (rrs==T)
            {if (i%%5==0){cushion.mean=mean(cushion.week)
            cushion.week<-c()}
              if (i<=5){basket.pl[i,k]=para.loss.start
              basket.ret[i,k]=basket.ret.start[i]}
              else {if (cushion.mean>=cushion.threshold[2])
              {basket.pl[i,k]=para.loss.table[3]
              basket.ret[i,k]=basket.ret.mc[i,3]}
                if (cushion.mean>=cushion.threshold[1]&cushion.mean<cushion.threshold[2])
                { basket.pl[i,k]=para.loss.table[2]
                basket.ret[i,k]=basket.ret.mc[i,2]}
                if (cushion.mean<cushion.threshold[1])
                { basket.pl[i,k]=para.loss.table[1]
                basket.ret[i,k]=basket.ret.mc[i,1]} }
            }
            
            else {basket.pl[i,k]=para.loss.table[1]
            basket.ret[i,k]=basket.ret.mc[i]}
            
            basket.nav[i,k]=basket.nav[i-1,k]*(1+basket.ret[i,k]-fees/nd)
            
            ptf.ret[i,k]=expo[i,k]*basket.ret[i,k]+(1-expo[i,k])*EONIA.mc[i]/nd
            
            if (ptf.nav[i-1,k]>nav.prot[i-1,k])
            {ptf.nav[i,k]=ptf.nav[i-1,k]*(1+ptf.ret[i,k]-fees/nd)}
            else {if (ptf.nav[i-1,k]<nav.prot[i-1,k]){ptf.nav[i,k]=ptf.nav[i-1,k]}
              else {ptf.nav[i,k]=nav.prot[i-1,k]}}

            if (i%%(nd/12)==0){ptf.l.m.nav[i,k]=ptf.nav[i,k]}
            else {ptf.l.m.nav[i,k]=ptf.l.m.nav[i-1,k]}
            
            if (prot.type=="l.m.nav")
            { nav.prot[i,k] = prot*ptf.l.m.nav[i,k]}
            
            
            if (reset==F)
            {ptf.navmax[i,k]=max(ptf.nav[i], ptf.navmax[i-1])
            if (prot.type=="prot.nav.max")
            {nav.prot[i,k] = prot*ptf.navmax[i,k]}
            if (prot.type=="ini.nav")
            {nav.prot[i,k] = nav.prot[i-1,k]}
            cushion[i,k]=(ptf.nav[i,k]-nav.prot[i,k])/ptf.nav[i,k]-fees/4}  
            
            if (reset==T)
            {n.yr=1
              if (i%%nd==0)
            {n.yr=n.yr+1
             ptf.navmax[i,k]=ptf.nav[i,k] 
            if (prot.type=="ini.nav")
            {nav.prot[i,k] = prot*ptf.nav[i,k]} }
              else{
                ptf.navmax[i,k]=max(ptf.nav[i], ptf.navmax[i-1])
              if (prot.type=="ini.nav")
              {nav.prot[i,k] = nav.prot[i-1,k]}}
              
              if (prot.type=="prot.nav.max")
              {nav.prot[i,k] = prot*ptf.navmax[i,k]}  
              
            cushion[i,k]=(ptf.nav[i,k]-nav.prot[i,k])/ptf.nav[i,k]-fees/nd*(n.yr*nd-i)  
              
            }
              
            

            cushion.week<-rbind(cushion.week, cushion[i,k])
            
            ptf.pl[i,k]=expo[i,k]*basket.pl[i,k]/100 
            
            cushion.conso[i,k]=ptf.pl[i,k]/cushion[i,k]
            
            basket.navmax[i,k]=max(basket.navmax[i-1,k],basket.nav[i,k], na.rm=TRUE)   
            
            basket.dd[i,k]=(basket.nav[i,k]/basket.navmax[i,k]-1)*100
            
            ptf.dd[i,k]=(ptf.nav[i,k]/ptf.navmax[i,k]-1)*100 
            
            if (expo[i,k]==1){n.expo = n.expo + 1}
          
            }
          
        }
        
        ptf.ddmax[,k]=min(ptf.dd[,k])      
        ptf.vol[,k]= sd(ptf.ret[,k])*sqrt(nd)*100
        basket.vol[,k]=sd(basket.ret)*sqrt(nd)*100
        
        if (prod(expo[,k])<1) {de.expo=de.expo+1}
        
        if (ptf.nav[n.days,k]<nav.prot[n.days,k])
        {gap<-rbind(gap, ptf.nav[n.days,k]-nav.prot[n.days,k])}
      
        ptf.ret.yr[,k] = (ptf.nav[n.days,k] - ptf.nav[1])/(n.days/nd)
        
        perc.expo[,k] = n.expo/n.days*100

        incProgress(10, detail = paste(round(k/n.obs*100,2), "%"))        }
      

      n.mone=length(which(expo[n.days,]==0))
      n.gap=length(which(ptf.nav[n.days,]<nav.prot[n.days,]))
      
      prob.mone=paste(round(n.mone/n.obs*100, epsilon),"%")
      prob.gap=paste(round(n.gap/n.obs*100, epsilon),"%")
      
      if(is.null(gap))
      {gap.mean=0}
      else {gap.mean=mean(gap)}
      
      ptf.nav.mean=paste(round(mean(ptf.nav[n.days,]),3), "(+ / - ",
                         round(1.64*sd(ptf.nav[n.days,])/sqrt(n.obs),3),"%)")
      
      ptf.nav.median=paste(round(median(ptf.nav[n.days,]),3), "(+ / - ",
                           round(1.64*sd(ptf.nav[n.days,])/sqrt(n.obs),3),"%)")
      
      ptf.ret.yr.mean=paste(round(mean(ptf.ret.yr),3), "(+ / - ",
                            round(1.64*sd(ptf.ret.yr)/sqrt(n.obs),3),"%)")

      ptf.vol.mean=paste(round(mean(ptf.vol),3),"%", "(+ / - ",
                         round(1.64*sd(ptf.nav[n.days,])/(sqrt(2*n.obs)),3),"%)")
      
      ptf.sharpe = round(mean(ptf.ret.yr)/mean(ptf.vol), 2)
      
      proba.de.expo=paste(round(de.expo/n.obs*100,3),"%")
      
      perc.expo.mean=paste(round(mean(perc.expo),2),"%")
      
      
      Parameters<-c(paste("Mean of final NAV of the fund"),
                          "Median of final NAV of the fund",
                          "Mean of Annualized Return of the fund",
                          "Mean of Annualized Volatility of the fund",
                          "Mean Sharpe",
                          "Average of time fully exposed",
                          "Probability of trigering CPPI mecanism",
                          "Probability of Monetarisation",
                          "Probability of Gap")
 
      Values<-c(ptf.nav.mean,
                ptf.nav.median,
                ptf.ret.yr.mean,
                ptf.vol.mean,
                ptf.sharpe,
                perc.expo.mean,
                proba.de.expo,
                prob.mone,
                prob.gap)
      
      if (gap.mean != 0)
        {Parameters<-c(Parameters,"Mean Gap")
        Values<-c(Values, paste(round(gap.mean,2), "%", "(+ / - ",
                                round(1.64*sd(gap)/sqrt(n.obs),3),"%)"))}
      
       mc.results.table<-cbind(Parameters, Values)
      
      output$mc.results<-renderTable(mc.results.table)
      
      cl<-rainbow(n.obs)
      plot(ptf.nav[,1], 
           type="l", col="white",
           xlab="Number of days", ylab="NAV", 
           main=paste("Monte Carlo Simulations over", n.days, "days"),
           ylim = c(min(ptf.nav),max(ptf.nav)))
      for (k in 2:n.obs)
      {lines(ptf.nav[,k],type="l",col=cl[k])}

      percentil<-seq(10, 90, 10)/100
      var$mc.percentil<-percentil
      M2<-matrix(NA, nr=n.days, nc=length(percentil))
      ptf.nav.dec<-M2
      cushion.dec<-M2
      for (j in 1:length(percentil))
      {for (i in 1:n.days)
      {ptf.nav.dec[i,j]<-quantile(ptf.nav[i,], prob = percentil [j])
      cushion.dec[i,j]<-quantile(cushion[i,], prob = percentil[j])}}
      
      var$mc.ptf.nav.dec<-ptf.nav.dec
      var$mc.cushion.dec<-cushion.dec
      
      n.yr=trunc(n.days/nd)
      
      proba.cppi.yr<-c()
      proba.mone.yr<-c()
      proba.gap.yr<-c()
      
      mc.proba.results<-c()
      
      n.cppi=0
      n.mone=0
      n.gap=0
      
      for (y in 0:(n.yr-1))
      {inf<-1
       sup<-(y+1)*nd
       expo.yr<-expo[inf:sup,]

        
        for (k in 1:n.obs)
        {if (prod(expo.yr[,k])<1)
        {n.cppi=n.cppi+1}
        if (expo[sup,k]==0)
        {n.mone=n.mone+1}
          if (ptf.nav[sup,k]<nav.prot[sup,k])
        {n.gap=n.gap+1}
          
          }
         proba.cppi.yr<-cbind(proba.cppi.yr, paste(round(n.cppi/n.obs*100,3),"%"))
          n.cppi=0
          
         proba.mone.yr<-cbind(proba.mone.yr, paste(round(n.mone/n.obs*100,3),"%"))
          n.mone=0
          
         proba.gap.yr<-cbind(proba.gap.yr, paste(round(n.gap/n.obs*100,3),"%"))
          n.gap=0
          
        }
      
      proba.cppi.yr<-cbind(proba.cppi.yr, proba.de.expo)
      
      proba.mone.yr<-cbind(proba.mone.yr, prob.mone)
      
      proba.gap.yr<-cbind(proba.gap.yr, prob.gap)
      
      mc.proba.results.para<-c("Probability of trigering CPPI mecanism",
                               "Probability of Monetarisation",
                               "Probablility of Gap")
      
      mc.proba.results<-rbind(proba.cppi.yr, 
                              proba.mone.yr,
                              proba.gap.yr)
      mc.proba.results<-cbind(mc.proba.results.para,
                              mc.proba.results)
      
      colnames(mc.proba.results)<-cbind(c("Parameters"),
                                        t(paste(seq(1:n.yr), "yr")),
                                        paste(n.days, "days"))
      
      
      output$mc.proba<-renderTable({mc.proba.results})
      
      colours<-matrix("white", nr=nrow(mc.proba.results), nc=ncol(mc.proba.results))
      t3<-ttheme_default(base_size = 8,
                         core = list(bg_params = list(fill=colours)))
      var$mc.prob<-tableGrob(as.data.frame(mc.proba.results),
                             theme=t3,
                             rows = NULL)
      
      
      
      #####################################===============================
      
      #####################################===============================
      
      #####################################===============================
      
      #####################################===============================
      
      #####################################=============================== 
      
      percentil<-seq(10, 90, 10)/100
  
      ptf.nav.dec<-c()
      ptf.ret.yr.dec<-c()
      ptf.vol.dec<-c()
      ptf.sharpe.dec<-c()
      ptf.ddmax.dec<-c()
      perc.expo.dec<-c()
      
      for (j in 1:length(percentil))
      {ptf.nav.dec<-cbind(ptf.nav.dec,quantile(ptf.nav[n.days,], prob = percentil [j]))
      ptf.ret.yr.dec<-cbind(ptf.ret.yr.dec,quantile(ptf.ret.yr, prob = percentil [j]))
      ptf.vol.dec<-cbind(ptf.vol.dec,quantile(ptf.vol, prob = percentil [j]))
      ptf.ddmax.dec<-cbind(ptf.ddmax.dec,quantile(ptf.ddmax, prob = percentil [j]))
      perc.expo.dec<-cbind(perc.expo.dec,quantile(perc.expo, prob = percentil [j]))
      }
      
      
      ptf.nav.dec<-cbind(mean(ptf.nav[n.days,]), ptf.nav.dec)
      ptf.ret.yr.dec<-cbind(mean(ptf.ret.yr), ptf.ret.yr.dec)
      ptf.vol.dec<-cbind(mean(ptf.vol), ptf.vol.dec)
      ptf.ddmax.dec<-cbind(mean(ptf.ddmax), ptf.ddmax.dec)
      perc.expo.dec<-cbind(mean(perc.expo), perc.expo.dec)
   
      ptf.sharpe.dec<-round(ptf.ret.yr.dec/ptf.vol.dec,2 )
      
      
      mc.dec.result.para<-rbind("Final NAV of the fund",
                                "Annualized return",
                                "Annualized volatility",
                                "Sharpe",
                                "Max drawown",
                                "Percentage of time fully exposed")
      
      mc.dec.result.val<-round(rbind(ptf.nav.dec,
                           ptf.ret.yr.dec,
                           ptf.vol.dec,
                           ptf.sharpe.dec,
                           ptf.ddmax.dec),2)
      mc.dec.result.val<-rbind(mc.dec.result.val,
                               paste(round(perc.expo.dec,2),"%"))

      mc.dec.result<-cbind(mc.dec.result.para,mc.dec.result.val)
      
      colnames(mc.dec.result)<-c("Parameters",
                                 "Mean",
                                 paste(seq(10, 40, 10), "%"),
                                 "Median",
                                 paste(seq(60, 90, 10), "%"))
      
     output$mc.dec<-renderTable({mc.dec.result})
    
     colours<-matrix("white", nr=nrow(mc.dec.result), nc=ncol(mc.dec.result))
     colours[,2]<-"skyblue"
     colours[,7]<-"skyblue"
     
     t4<-ttheme_default(base_size = 8,
                        core = list(bg_params = list(fill=colours)))
     
     var$mc.result<-tableGrob(mc.dec.result,
                              theme=t4,
                              rows = NULL)
 
     
     colours<-matrix("white", nr=nrow(mc.para), nc=ncol(mc.para))
     t5<-ttheme_default(base_size = 10,
                        core = list(bg_params = list(fill=colours)))
     var$mc.para<-tableGrob(mc.para, cols="Monte Carlo Parameters",
                            theme=t5)
     
     pdf_basket.ret<-density(basket.return[,ncol(basket.return)],
                             bw = "nrd0", 
                             adjust = 1,
                             kernel = c("gaussian"),
                             weights = NULL, window = kernel, width,
                             give.Rkern = FALSE,
                             n = n, cut =20, na.rm = FALSE)
     
     var$mc.basket.ret <-as.matrix(approx(cumsum(pdf_basket.ret$y)/sum(pdf_basket.ret$y),
                            pdf_basket.ret$x,
                            runif(n))$y)
     
     
     
     

     
     
     })
       
       
       })
   

   
     
     
     
     ############ Empiracal Frontier
     
     
   
     output$emp.para.table<-renderRHandsontable({
      
      
       
        emp.para<-as.data.frame(matrix(c(100,400,10), nr=3, nc=1))
       row.names(emp.para)<-c("Number of observations", 
                             "Number of simulated days",
                             "Number of points on the eff. frontier")
       colnames(emp.para)<-c("Values")
       rhandsontable(emp.para,
                     rowHeaderWidth = 250, 
                     colWidths = 130)
     })

   
     
     emp.eff.front.table.ini<-reactive({
     
       emp.para<-input$emp.para.table
     if (!is.null(input$emp.para.table)) 
       emp.para <- hot_to_r(input$emp.para.table)
     
     n.pts.eff.front=emp.para[3,1]
     
     x <- as.numeric(seq(1, n.pts.eff.front))  
     
     y <- x^(1/3)
     
     data.frame("Excess Return" = y, 
                Volatility = x, 
                Sharpe =1:n.pts.eff.front, 
                stringsAsFactors = F)
   })
   
   
   emp.para.sharpe<-reactive({
     
     emp.para<-input$emp.para.table
     if (!is.null(input$emp.para.table)) 
       emp.para <- hot_to_r(input$emp.para.table)
     
     n.pts.eff.front=emp.para[3,1]
 
       if(is.null(input$eff.front.table)){return(emp.eff.front.table.ini())}
       else if(!identical(emp.eff.front.table.ini(), input$eff.front.table)){
         # hot.to.df function will convert your updated table into the dataframe
         mytable <- as.data.frame(hot_to_r(input$eff.front.table))
         # here the second column is a function of the first and it will be multipled by 100 given the values in the first column
         mytable <- mytable[1:n.pts.eff.front,]
         
         # Add some test cases
         mytable[,1][is.na(mytable[,1])] <-is.na(mytable[,1])
         mytable[,2][is.na(mytable[,2])] <-is.na(mytable[,1])
         mytable[,3] <- mytable[,1]/mytable[,2]
         mytable
       }
  
     
     
   })
   
   output$eff.front.table<-renderRHandsontable({rhandsontable(emp.para.sharpe(),
                                                              colWidths = 110)})
   
     ########################################
     
     
     emp.para.storage<-eventReactive(input$go3,
                                     {emp.para<-input$emp.para.table})
     
     eff.front.storage<-eventReactive(input$go3,
                                      {eff.front<-input$eff.front.table})
   
     ########################################
      
     
     output$emp.ptf.nav.plot<-renderPlot({
       
        withProgress(message = 'Making plot', value = 0, { 
       
        emp.para <- hot_to_r(emp.para.storage())

         n.obs=emp.para[1,1]
         n.days=emp.para[2,1]
         n.pts.eff.front=emp.para[3,1]
         
        eff.front <- hot_to_r(eff.front.storage())

     ret.pts<-as.matrix(eff.front[,1])/100
     exc.ret.max=last(ret.pts)
     vol.pts<-as.matrix(eff.front[,2])/100
     vol.max.pts=last(vol.pts)
     vol.min.pts=first(vol.pts)

     d.poly.inter=n.pts.eff.front
     poly.inter=coef(lm(ret.pts ~ poly(vol.pts, d.poly.inter, raw=T)))
     coef.poly<-matrix(NA, nr=1, nc=(d.poly.inter+1))
     for(i in 1:(d.poly.inter+1))
     {if (is.na(poly.inter[i])) {coef.poly[i]=0}
       else {coef.poly[i]=poly.inter[i]}}
     
     f.poly.inter<-function(x)
     {temp<-c()
     for (i in 1:(d.poly.inter+1))
     { temp<-cbind(temp, coef.poly[i]*x^(i-1))}
     rowSums(temp)
     }

 expo.cut<-isolate(input$expo.cut/100)
 prot<-isolate(input$prot/100)
 fees<-isolate(input$fees/100)
 pl.vol<-isolate(input$pl.vol)
 EONIA<-isolate(input$eonia/100)
 reset<-isolate(input$reset)
 prot.type=isolate(input$prot.type)
 risk.budget.max=isolate(input$risk.budget.max/100)

 
 nd = 252
 
 M0<-matrix(NA, nr=n.days, nc=n.obs)
 
 ptf.nav<-M0
 ptf.nav.max<-M0
 ptf.ret<-M0
 nav.prot<-M0
 cushion<-M0
 ptf.l.m.nav<-M0
 
 vol<-M0
 excess.ret<-M0
 
 M1<-matrix(NA, nr=1, nc=n.obs)
 sum.col.vol<-M1
 vol.min<-M1
 vol.max<-M1
 ptf.vol<-M1

  for (k in 1:n.obs)
 {for (i in 1:n.days)
 {if (i == 1)
 { ptf.nav[i,k]=100
 ptf.nav.max[i,k]=ptf.nav[i,k]
 ptf.l.m.nav[i,k]=ptf.nav[i,k]
 nav.prot[i,k]=prot*ptf.nav.max[i,k]
 cushion[i,k]=(ptf.nav[i,k]-nav.prot[i,k])/ptf.nav[i,k]
 vol[i,k]=risk.budget.max
 excess.ret[i,k]=0
 ptf.ret[i,k]=0 }
   
   if (i>1)
   { vol[i,k]=min(cushion[i-1,k]/pl.vol, risk.budget.max)
   excess.ret[i,k]=f.poly.inter(vol[i,k])
   
   if (cushion[i-1,k]>=expo.cut)
   {ptf.ret[i,k]=(EONIA-fees)/nd + rnorm(1, mean = excess.ret[i,k]/nd, sd = vol[i,k]/sqrt(nd))}
   else { ptf.ret[i,k]=0 }
   
   ptf.nav[i,k]=ptf.nav[i-1,k]*(1+ptf.ret[i,k])
   
   if (i%%(nd/12)==0){ ptf.l.m.nav[i,k]=ptf.nav[i,k] }
   else { ptf.l.m.nav[i,k]=ptf.l.m.nav[i-1,k] }
   
   if (reset==F)
   { ptf.nav.max[i,k]=max(ptf.nav[i,k], ptf.nav.max[i-1,k]) }
   
   if (reset==T)
   { if (i%%nd==0)
   { ptf.nav.max[i,k]=ptf.nav[i,k] }
     else 
     { ptf.nav.max[i,k]=max(ptf.nav[i,k], ptf.nav.max[i-1,k]) }}
   
   if (prot.type=="prot.nav.max")
   {nav.prot[i,k]=prot*ptf.nav.max[i,k]}
   
   if (prot.type=="l.m.nav")
   {nav.prot[i,k]=prot*ptf.l.m.nav[i,k]}
   
   cushion[i,k]=(ptf.nav[i,k]-nav.prot[i,k])/ptf.nav[i,k]
   
   }
 }
   sum.col.vol[,k]=sum(vol[,k])
   vol.min[,k]=min(vol[,k])
   vol.max[,k]=max(vol[,k])
   ptf.vol[,k]=sd(ptf.ret[,k])*sqrt(nd)
   incProgress(10, detail = paste(round(k/n.obs*100,2), "%"))
   }
 
 
 percentil<-seq(10, 90, 10)/100
 M2<-matrix(NA, nr=n.days, nc=length(percentil))
 ptf.nav.dec<-M2
 cushion.dec<-M2
 for (j in 1:length(percentil))
 {for (i in 1:n.days)
 {ptf.nav.dec[i,j]<-quantile(ptf.nav[i,], prob = percentil [j])
 cushion.dec[i,j]<-quantile(cushion[i,], prob = percentil[j])}}
 
 cl<-rainbow(length(percentil))
 plot(ptf.nav.dec[,1], 
      type="l", col=cl[1],
      xlab="Number of days", ylab="NAV", 
      main=paste("Portfolio NAV by decile"),
      ylim = c(min(ptf.nav.dec),max(ptf.nav.dec)),
      lwd = 3)
 if (n.obs>1)
 {for (k in 2:length(percentil))
 {lines(ptf.nav.dec[,k],type="l",col=cl[k], lwd = 3)}}
 
 
 output$emp.cushion.plot<-renderPlot({
   cl<-rainbow(length(percentil))
   plot(cushion.dec[,1], 
        type="l", col=cl[1],
        xlab="Number of days", ylab="Cushion", 
        main=paste("Cushion consumption by decile"),
        ylim = c(min(cushion.dec),max(cushion.dec)),
        lwd = 3)
   if (n.obs>1)
   {for (k in 2:length(percentil))
   {lines(cushion.dec[,k],type="l",col=cl[k], lwd = 3)}}  
   
 })
 
 
 if(n.days<nd/12)
 {ptf.nav.dec.table<-as.matrix(paste(round(ptf.nav.dec[n.days,]-100, 2), "%"))
 colnames(ptf.nav.dec.table)<-c(paste("Returns over", n.days, "days"))
 
 cushion.dec.table<-as.matrix(paste(round(100-cushion.dec[n.days,]/cushion[1,1]*100, 2), "%"))
 colnames(cushion.dec)<-c(paste("Cushion consumption over", n.days, "days"))
 
 }
 
 if(n.days>=nd/12 & n.days<nd/4)
 {ptf.nav.dec.table<-as.matrix(cbind( paste(round(ptf.nav.dec[nd/12,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[n.days,]-100, 2), "%")))
 colnames(ptf.nav.dec.table)<-c("Returns over 1 month", 
                                paste(n.days, "days"))
 
 cushion.dec.table<-as.matrix(cbind( paste(round(100-cushion.dec[nd/12,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[n.days,]/cushion[1,1]*100, 2), "%")))
 colnames(cushion.dec.table)<-c("Cushion consumption over 1 month", 
                                paste(n.days, "days"))
 }
 
 if(n.days>=nd/4 & n.days<nd/2)
 {ptf.nav.dec.table<-as.matrix(cbind( paste(round(ptf.nav.dec[nd/12,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[nd/4,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[n.days,]-100, 2), "%")))
 colnames(ptf.nav.dec.table)<-c("Returns over 1 month",
                                "3 months",
                                paste(n.days, "days"))
 
 cushion.dec.table<-as.matrix(cbind( paste(round(100-cushion.dec[nd/12,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[nd/4,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[n.days,]/cushion[1,1]*100, 2), "%")))
 colnames(cushion.dec.table)<-c("Cushion consumption over 1 month",
                                "3 months",
                                paste(n.days, "days"))
 } 
 
 if(n.days>=nd/2 & n.days<nd)
 {ptf.nav.dec.table<-as.matrix(cbind( paste(round(ptf.nav.dec[nd/12,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[nd/4,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[nd/2,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[n.days,]-100, 2), "%")))
 colnames(ptf.nav.dec.table)<-c("Returns over 1 month",
                                "3 months",
                                "6 months",
                                paste(n.days, "days"))
 
 cushion.dec.table<-as.matrix(cbind( paste(round(100-cushion.dec[nd/12,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[nd/4,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[nd/2,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[n.days,]/cushion[1,1]*100, 2), "%")))
 colnames(cushion.dec.table)<-c("Cushion consumption over 1 month",
                                "3 months",
                                "6 months",
                                paste(n.days, "days"))
 }
 
 if(n.days>=nd & n.days<2*nd)
 {ptf.nav.dec.table<-as.matrix(cbind( paste(round(ptf.nav.dec[nd/12,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[nd/4,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[nd/2,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[nd-1,]-100, 2), "%"),
                                      paste(round(ptf.nav.dec[n.days,]-100, 2), "%")))
 colnames(ptf.nav.dec.table)<-c("Returns over 1 month",
                                "3 months",
                                "6 months",
                                "1 year",
                                paste(n.days, "days"))
 
 cushion.dec.table<-as.matrix(cbind( paste(round(100-cushion.dec[nd/12,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[nd/4,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[nd/2,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[nd-1,]/cushion[1,1]*100, 2), "%"),
                                     paste(round(100-cushion.dec[n.days,]/cushion[1,1]*100, 2), "%")))
 colnames(cushion.dec.table)<-c("Cushion consumption over 1 month",
                                "3 months",
                                "6 months",
                                "1 year",
                                paste(n.days, "days"))
 } 
 
 
 
 if(n.days>=2*nd)
 { temp1<-cbind( paste(round(ptf.nav.dec[nd/12,]-100, 2), "%"),
                 paste(round(ptf.nav.dec[nd/4,]-100, 2), "%"),
                 paste(round(ptf.nav.dec[nd/2,]-100, 2), "%"))
 
 temp2<-cbind( paste(round(100-cushion.dec[nd/12,]/cushion[1,1]*100, 2), "%"),
               paste(round(100-cushion.dec[nd/4,]/cushion[1,1]*100, 2), "%"),
               paste(round(100-cushion.dec[nd/2,]/cushion[1,1]*100, 2), "%"))
 
 for (i in 2:n.days)
 {  if ( i%%nd==0)
 {temp1<-cbind(temp1, paste(round(ptf.nav.dec[i,]-100, 2), "%"))
 if (reset==T)
 {l=i-nd+1
 temp2<-cbind(temp2, paste(round(100-cushion.dec[i-1,]/cushion[l,1]*100, 2), "%"))}
 else {temp2<-cbind(temp2, paste(round(100-cushion.dec[i-1,]/cushion[1,1]*100, 2), "%"))}
 }
   if (i==n.days)
   { temp1<- cbind(temp1, paste(round(ptf.nav.dec[i,]-100, 2), "%"))
   if (reset==T)
   {l=i-i%%nd+1
   temp2<- cbind(temp2, paste(100-signif(round(cushion.dec[i,]/cushion[l,1]*100, 2),2), "%"))}    
   else {temp2<- cbind(temp2, paste(100-signif(round(cushion.dec[i,]/cushion[1,1]*100, 2),2), "%"))}    
   }
 }
 
 ptf.nav.dec.table<-as.matrix(temp1)
 colnames(ptf.nav.dec.table)<-c("Returns over 1 month",
                                "3 months",
                                "6 months",
                                "1 year",
                                paste(seq(2, trunc(n.days/nd)), "years"),
                                paste(n.days, "days"))
 
 cushion.dec.table<-as.matrix(temp2)
 colnames(cushion.dec.table)<-c("Cushion consumption over 1 month",
                                "3 months",
                                "6 months",
                                "1 year",
                                paste(seq(2, trunc(n.days/nd)), "years"),
                                paste(n.days, "days"))
 
 }
 
 output$emp.ptf.nav.dec<-renderTable({ptf.nav.dec.table}) 
 output$emp.cushion.dec<-renderTable({cushion.dec.table})


 mean.vol<-seq(mean(vol.min), mean(vol.max), 0.001)
 mean.ret<-f.poly.inter(mean.vol)
 
 
 output$front.eff.plot<-renderPlot({
   plot(ret.pts~vol.pts,
        main="Efficient Frontier", 
        xlab="volatility", 
        ylab="excess return",
        pch=19,
        col="deepskyblue",
        lwd = 3)
   lines(f.poly.inter(vol.pts)~vol.pts, lty = 4, lwd = 3, col='blue4')
   lines(mean.ret~mean.vol, type = 'l', lwd = 2, col ='red') })
 
 n.cppi=length(which(sum.col.vol<(risk.budget.max*n.days))) 
 n.mone=length(which(cushion[n.days,]<expo.cut)) 
 n.gap=length(which(ptf.nav[n.days,]<nav.prot[n.days,]))  
 
 epsilon=log(n.obs)/log(10)
 proba.cppi=paste(round(n.cppi/n.obs*100,3),"%")
 prob.mone=paste(round(n.mone/n.obs*100, epsilon),"%")
 prob.gap=paste(round(n.gap/n.obs*100, epsilon),"%")
 
 
 ptf.nav.mean=paste(round(mean(ptf.nav[n.days,]),3), "(+ / - ",
                    round(1.64*sd(ptf.nav[n.days,])/sqrt(n.obs),3),"%)")
 
 ptf.nav.median=paste(round(median(ptf.nav[n.days,]),3), "(+ / - ",
                      round(1.64*sd(ptf.nav[n.days,])/sqrt(n.obs),3),"%)")
 
 ptf.vol.mean=paste(round(mean(ptf.vol)*100,3),"%", "(+ / - ",
                    round(1.64*sd(ptf.nav[n.days,])/(sqrt(2*n.obs)),3),"%)")
 

 Parameters<-c(paste("Mean of final NAV of the portfolio"),
               "Median of final NAV of the portfolio",
               "Mean of Annualized Volatility of the Portfolio",
               "Probability of trigering CPPI mecanism",
               "Probability of Monetarisation",
               "Probability of Gap")
 
 Values<-c(ptf.nav.mean,
           ptf.nav.median,
           ptf.vol.mean,
           proba.cppi,
           prob.mone,
           prob.gap)
 
 
 
 emp.results<-cbind(Parameters, Values)
 
 output$emp.results<-renderTable({emp.results})

   })
   
     })
     
     
   output$report.pdf = downloadHandler(
   
     filename = function() {"plots.pdf"},
     
     content = function(file) {
       pdf(file, onefile = TRUE, width=12, height=8)
   data1<-data.frame(date = var$bt.dt, 
                     ptf.nav = var$bt.ptf.nav,
                     nav.prot = var$bt.nav.prot,
                     basket.nav = var$bt.basket.nav,
                     basket.nav.gross = var$bt.basket.nav.gross,
                     basket.dd = var$bt.basket.dd,
                     ptf.dd = var$bt.ptf.dd,
                     expo = var$bt.expo,
                     cushion = var$bt.cushion)
   
   
  
    bt.nav.plot <- ggplot(data1, aes( x = date)) + 
     geom_line(aes(y = basket.nav, colour = "basket")) +
     geom_line(aes(y = ptf.nav, colour = "fund")) +
     geom_line(aes(y = nav.prot, colour = "protected nav")) +
     geom_line(aes(y = basket.nav.gross, colour = "gross basket"), linetype = "dotdash")+
     xlab("") +
     ylab("") +
     scale_colour_manual("",
                         breaks = c("fund", "protected nav", "basket", "gross basket" ),
                         values = c("fund" ="blue4", 
                                    "protected nav"="red3", 
                                    "basket"="deepskyblue", 
                                    "gross basket"="deepskyblue")) +
      guides(colour=guide_legend(override.aes=list(linetype=c(1,1,1,2))))  +
     ggtitle("NAV") + 
     theme(plot.title = element_text(size = 2)) +
     theme_bw()
     #''''''''''''''''''''''''''''''''''''''''''''''
     bt.dd.plot <-ggplot(data1, aes(x = date)) + 
       geom_line(aes(y = basket.dd, colour = "basket")) +
       geom_line(aes(y = ptf.dd, colour = "fund")) +
       xlab("") +
       ylab("") +
       scale_colour_manual("",
                           breaks = c("fund", "basket"),
                           values = c("fund" ="blue4", 
                                      "basket"="deepskyblue")) +
       ggtitle("Drawdown") + 
       theme(plot.title = element_text(size = 2)) +
       theme_bw()
     
     #''''''''''''''''''''''''''''''''''''''''''''''
     bt.expo.plot <-ggplot(data1, aes(x = date)) + 
       geom_line(aes(y = cushion*(100-input$prot), colour = "cushion")) +
       geom_line(aes(y = expo, colour = "expo")) +
       scale_y_continuous(sec.axis = sec_axis(~.*(100-input$prot)/100, name = "Risk Budget" )) +
       xlab("") +
       ylab("Expo") +
       scale_colour_manual("",
                           breaks = c("expo", "cushion"),
                           values = c("expo" ="olivedrab", 
                                      "cushion"="red3")) +
       ggtitle("Risk budget and exposition") + 
       theme(plot.title = element_text(size = 2)) +
      theme_bw()
     
     #''''''''''''''''''''''''''''''''''''''''''''''
     lay1<-rbind(c(1,2,2,2,2,2),
                 c(1,2,2,2,2,2),
                 c(1,3,3,3,3,3),
                 c(1,3,3,3,3,3),
                 c(1,4,4,5,5,5),
                 c(1,4,4,5,5,5),
                 c(1,4,4,6,6,6))
     grid.arrange(var$alloc, 
                  bt.nav.plot, 
                  bt.dd.plot, 
                  var$bt.risk.metrics,
                  bt.expo.plot,
                  var$bt.para,
                  layout_matrix = lay1)
     #''''''''''''''''''''''''''''''''''''''''''''''
     if (is.null(var$mc.basket.ret)==F)
     {
     data2<-data.frame(mc.basket.ret=var$mc.basket.ret)

      mc.basket.ret.hist<-ggplot(data2, aes(x=mc.basket.ret*100, fill =..x..)) +
       geom_histogram(binwidth=0.03) +
       scale_y_continuous(labels = percent_format()) +
       scale_fill_gradient("Return (%)", low = "red", high = "blue") +
       xlab("") +
       ylab("") +
       ggtitle("Distribution of basket returns") +
       theme_bw()

     #''''''''''''''''''''''''''''''''''''''''''''''
     mc.ptf.nav.dec<-data.frame(cbind(seq(1:nrow(var$mc.ptf.nav.dec)), 
                                      var$mc.ptf.nav.dec))
       
     for (j in 1:ncol(mc.ptf.nav.dec))
       {if (j==1){colnames(mc.ptf.nav.dec)[j]<-c("day")}
         else {colnames(mc.ptf.nav.dec)[j]<-paste(var$mc.percentil[j-1]*100, "%")}}
     
       mc.ptf.nav.dec.plot<-ggplot(melt(mc.ptf.nav.dec, id="day"),
                                   aes(x=day, y= value, color =variable)) +
         geom_line()+
         xlab("Period of simulation in days") +
         ylab("") +
         ggtitle("NAV of Simulated Fund by quantile") +
         scale_color_manual("Quantile", values= rainbow(ncol(mc.ptf.nav.dec))) +
         theme_bw() 
         
     #''''''''''''''''''''''''''''''''''''''''''''''
       lay2<-rbind(c(NA,NA,NA,NA,NA,NA,NA,NA),
                   c(1,1,1,NA,NA,NA,NA,NA),
                   c(NA,NA,NA,NA,NA,NA,NA,NA),
                   c(2,2,2,2,3,3,3,3),
                   c(2,2,2,2,3,3,3,3),
                   c(2,2,2,2,3,3,3,3),
                   c(2,2,2,2,3,3,3,3),
                   c(2,2,2,2,3,3,3,3),
                   c(2,2,2,2,3,3,3,3),
                   c(2,2,2,2,3,3,3,3),
                   c(NA,NA,NA,NA,NA,NA,NA,NA),
                   c(NA,4,4,4,4,4,4,NA),
                   c(NA,4,4,4,4,4,4,NA),
                   c(NA,4,4,4,4,4,4,NA),
                   c(NA,NA,NA,NA,NA,NA,NA,NA),
                   c(NA,5,5,5,5,5,5,NA),
                   c(NA,5,5,5,5,5,5,NA),
                   c(NA,5,5,5,5,5,5,NA)
                   )
       
       
        grid.arrange(var$mc.para,
                  mc.basket.ret.hist,
                  mc.ptf.nav.dec.plot,
                  var$mc.result,
                  var$mc.prob,
                  layout_matrix = lay2)}
       
      dev.off()
     }
       )
   

     
   
   
   
   
   }

