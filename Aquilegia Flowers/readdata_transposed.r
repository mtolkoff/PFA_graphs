library(scales)
library(ggplot2)
library(RColorBrewer)
nfac = 2
ntaxa = 30
ndata = 12

#Read in data and set working directory. Make sure you use the correct
#directory.
setwd('~/PFA_graphs/Aquilegia Flowers')
workingData <-
  read.table('AquilegiaFac2.log',
             colClasses = 'numeric',
             header = TRUE)






#gets median for each loading matrix minus burnin
getMedian <- function(load, Y, name, i, j)
{
  print(name)
  load[i, j] = median(Y[[which(names(Y) == name)]][(length(Y$posterior) / 10):length(Y$posterior)])
  print(load[i, j])
  return(load)
}

#Measures percentage
getSigInfo <- function(opacity, Y, name, i, j) {
  #percentage of draws > 0
  pct = length(which(Y[[which(names(Y) == name)]][(length(Y$posterior) / 10):length(Y$posterior)] > 0)) / (length(Y$posterior) * 9 / 10)
  #if median of Lij > 0 use that percentage
  if (pct > .5) {
    opacity[i, j] = pct
  }
  #otherwise use 1 - pct
  else{
    opacity[i, j] = 1 - pct
  }
  if (opacity[i, j] > 1)
    opacity[i, j] = 1
  return(opacity)
}


#fill loadings matrix with median values
getLoadMedians <- function(Y, nfac, ndata, sideways) {
  load = matrix(data = 0,
                nrow = ndata,
                ncol = nfac)
  for (i in 1:ndata)
  {
    for (j in 1:nfac)
    {
      if (nfac == 1) {
        load = getMedian(load, Y, paste("l", i, sep = ""), i, j)
      }
      else if (i == 1 && j == 1)
      {
        load = getMedian(load, Y, "l1", i, j)
      }
      else if (j <= i)
      {
        load = getMedian(load, Y, paste("l", i, j, sep = ""), i, j)
      }
      else{
        
      }
    }
  }
  return(load)
}

#get measure of significance
getOpacity <- function(Y, nfac, ndata, sideways) {
  opacity = matrix(data = 0,
                   nrow = ndata,
                   ncol = nfac)
  for (i in 1:ndata)
  {
    for (j in 1:nfac)
    {
      if (nfac == 1) {
        opacity = getSigInfo(opacity, Y, paste("l", i, sep = ""), i, j)
      }
      #recorded differently for 1-1
      else if (i == 1 && j == 1)
      {
        opacity = getSigInfo(opacity, Y, "l1", i, j)
      }
      else if (j <= i)
      {
        opacity = getSigInfo(opacity, Y, paste("l", i, j, sep = ""), i, j)
      }
      else{
        
      }
      
    }
  }
  return(opacity)
}





#fixes multimodal problem. See Tolkoff et al. 2017 for details.
postProcess <- function(workingData, nfac, ndata)
{
  crossCount = matrix(data = 0,
                      nrow = nfac,
                      ncol = ndata)
  size = length(workingData[[which(names(workingData) == "l1")]])
  lowSize = as.integer(.1 * size)
  for (i in 1:nfac) {
    for (j in i:ndata) {
      print(paste(i, j, sep = " "))
      if (i == 1 && j == 1) {
        name = "l1"
      }
      else{
        name = paste("l", j, i, sep = "")
      }
      for (l in size:lowSize) {
        if (workingData[[which(names(workingData) == name)]][l] * workingData[[which(names(workingData) == name)]][l - 1] < 0) {
          crossCount[i, j] = crossCount[i, j] + 1
        }
      }
    }
  }
  for (i in 1:nfac) {
    for (j in i:ndata) {
      if (j == i) {
        index = j
        min = crossCount[i, j]
      }
      if (crossCount[i, j] < min)
      {
        index = j
        min = crossCount[i, j]
      }
    }
    for (k in 1:size) {
      print(k)
      if (index == 1) {
        temp3 = workingData[[which(names(workingData) == "l1")]]
      }
      else{
        temp3 = workingData[[which(names(workingData) == paste("l", index, i, sep = ""))]]
      }
      if (temp3[k] < 0) {
        for (j in 1:ndata) {
          if (j >= i) {
            if (i == 1 && j == 1) {
              name = "l1"
            }
            else{
              name = paste("l", j, i, sep = "")
            }
            temp4 = workingData[[which(names(workingData) == name)]][k]
            workingData[[which(names(workingData) == name)]][k] = -temp4
          }
        }
      }
    }
  }
  return(workingData)
}


#Median of the loadings of the posterior output
load = getLoadMedians(workingData, nfac, ndata, sideways)

#Measure of uncertainty
opacity <- getOpacity(workingData, nfac, ndata, sideways)
print(opacity)
#Adjust the measure of uncertainty to appear more pronounced in the graph
#This is approrpiately reflected in the legend.
opacity <- opacity ^ 8

#Covariate names
loadNames = c(
  "Orientation",
  "Spur length",
  "Blade length",
  "Sepal length",
  "Spur chroma",
  "Spur hue",
  "Spur brightness",
  "Blade chroma",
  "Blade hue",
  "Blade brightness",
  "Pollinator type",
  "Anthocyanins"
)
#Discrete variables may be treated differently since their size is entirely
#dependent on the prior set before analysis
discrete = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

#Incorporate name and discrete flag into one variable
loadArray = array(data = 0, dim = c(ndata, nfac + 2))
for (i in 1:ndata) {
  for (j in 1:(nfac + 2))
  {
    if (j == 1)
      loadArray[i, j] = loadNames[i]
    if (j == 2)
      loadArray[i, j] = discrete[i]
    if (j > 2) {
      if (j - 2 <= i)
        loadArray[i, j] = load[i, j - 2]
      else
        loadArray[i, j] = 1234567
    }
  }
}


#builds the graph
LoadMatrixBuild <-
  function(load, opacity, center, showZero, dotscale) {
    myPal <- brewer.pal(11, "RdBu")
    grey = brewer.pal(9, "Greys")[2]
    color1 = rgb(.513, .73, .441)
    color2 = rgb(.471, .109, .527)
    black = brewer.pal(9, "Greys")[7]
    
    #Number of covariates per line before splitting into different lines.
    nsplit = 12
    
    nrow = ndata / nsplit
    nrow = ceiling(nrow)
    
    
    #different options for how I want to display the graph
    if (showZero) {
      zeroLine = "black"
      zeroFill = "grey"
    }
    else{
      zeroLine = grey
      zeroFill = grey
    }
    
    nload = length(load[, 1])
    nfac = length(load[1, ]) - 2
    
    #prevents segfault error
    if (nsplit > nload) {
      nsplit = nload
    }
    
    #initializes plot and creates boxes
    par(pty = "s")
    yb = c()
    yt = c()
    xtemp = rep(c(nsplit - rep(1:(nsplit), (nrow - 1)) + .5, nsplit:(nsplit - ndata + (nrow - 1) * nsplit + 1) - .5), nfac)
    print(xtemp)
    xr = xtemp + .5
    xl = xtemp - .5
    plot(
      0,
      0,
      type = "n",
      ylim = c(-4, 22),
      xlim = c(-4, 18),
      axes = FALSE,
      ylab = "",
      xlab = "",
      asp = 1
    )
    
    for (i in 1:nrow) {
      if (i * nsplit - nload < 0) {
        yb = c(yb,
               rep(.5 , nsplit) + center + (nfac + 2) * (i - 1))
        yt = c(yt,
               rep(1.5 , nsplit) + center + (nfac + 2) * (i - 1))
      }
      else{
        yb = c(yb,
               rep(.5 , nload - nsplit * (i - 1)) + center + (nfac + 2) * (i - 1))
        yt = c(yt,
               rep(1.5 , nload - nsplit * (i - 1)) + center + (nfac + 2) * (i - 1))
      }
    }
    ybtemp = yb
    yttemp = yt
    if (nfac != 1)
    {
      for (i in 1:(nfac - 1)) {
        yb = c(yb, ybtemp + (i))
        yt = c(yt, yttemp + (i))
      }
    }
    yb = rev(yb)
    yt = rev(yt)
    xl = rev(xl)
    xr = rev(xr)
    if (length(which(load == 1234567)) != 0) {
      rect(xl[-(which(load == 1234567) - 2 * ndata)] + 3, yb[-(which(load == 1234567) -
                                                                 2 * ndata)], xr[-(which(load == 1234567) - 2 * ndata)] + 3, yt[-(which(load ==
                                                                                                                                          1234567) - 2 * ndata)], col = grey, lwd = 1.5)
      rect(xl[(which(load == 1234567)) - 2 * ndata] + 3, yb[(which(load ==
                                                                     1234567) - 2 * ndata)], xr[(which(load == 1234567) - 2 * ndata)] + 3, yt[(which(load ==
                                                                                                                                                       1234567) - 2 * ndata)], col = black, lwd = 1.5)
    }
    else{
      rect(xl, yb + 3, xr, yt + 3, col = grey, lwd = 1.5)
    }

    print(yb)
    print(length(yb))
    
    #Inputs loadings values
    colorMat = matrix(data = 0,
                      nrow = nload,
                      ncol = nfac)
    lineMat = matrix(data = "black",
                     nrow = nload,
                     ncol = nfac)
    for (i in 1:nload)
    {
      for (j in 1:nfac)
      {
        if (load[i, j + 2] == 1234567) {
          colorMat[i, j] = black
          lineMat[i, j] = black
          load[i, j + 2] = 0
        }
        else if (as.numeric(load[i, j + 2]) < 0 &&
                 as.numeric(load[i, 2]) == 0) {
          colorMat[i, j] = alpha(color1, opacity[i, j])
          lineMat[i, j] = alpha('black', opacity[i, j])
        }
        else if (as.numeric(load[i, j + 2]) < 0 &&
                 as.numeric(load[i, 2]) == 1) {
          colorMat[i, j] = alpha(color1, opacity[i, j])
          lineMat[i, j] = alpha('black', opacity[i, j])
        }
        else if (as.numeric(load[i, j + 2]) > 0 &&
                 as.numeric(load[i, 2]) == 0) {
          colorMat[i, j] = alpha(color2, opacity[i, j])
          lineMat[i, j] = alpha('black', opacity[i, j])
        }
        else if (as.numeric(load[i, j + 2]) > 0 &&
                 as.numeric(load[i, 2]) == 1)
        {
          colorMat[i, j] = alpha(color2, opacity[i, j])
          lineMat[i, j] = alpha('black', opacity[i, j])
        }
        else{
          colorMat[i, j] = zeroFill
          lineMat[i, j] = zeroLine
        }
        if (as.numeric(load[i, j + 2]) < 0) {
          load[i, j + 2] = -as.numeric(load[i, j + 2])
        }
        
        
      }
    }
    
    factorList = c()
    for (i in 1:nfac) {
      factorList = c(factorList, paste("Loading", i, sep = " "))
    }
    xtemp = rep(c(nsplit - rep(1:(nsplit), (nrow - 1)) + .5, nsplit:(nsplit -
                                                                       ndata + (nrow - 1) * nsplit + 1) - .5), nfac)
    print(length(xtemp))
    print(xtemp)
    
    #creates labels and names
    symbols(
      x = rev(xtemp) + 3,
      y = (yb + .5) ,
      circles = as.numeric(load[, 3:(nfac + 2)]) * dotscale,
      bg = colorMat,
      add = TRUE,
      inches = FALSE,
      fg = lineMat
    )

    for (i in 1:nrow)
    {
      text(
        x = 3.2,
        y = 1:nfac  + center + (nfac + 2) * (i - 1),
        labels = rev(factorList),
        cex = .75,
        pos = 2
      )
    }
      text(
      x = -4,
      y = center + (nfac + 1) * nrow / 2,
      labels = "Phylogenetic factor \nanalysis",
      cex = .75
    )
    
    j = -1
    for (i in c(seq(-1, -.7, .1), 0, seq(.7, 1, .1))) {
        if (i < 0) {
        rect(
          16,
          center + (nfac + 1) * nrow / 2 + 2 * j - .25 + 1.5,
          16.5,
          center + (nfac + 1) * nrow / 2 + 2 * j + .25 + 1.5,
          col = grey
        )
        rect(
          16,
          center + (nfac + 1) * nrow / 2 + 2 * j - .25 + 1.5,
          16.5,
          center + (nfac + 1) * nrow / 2 + 2 * j + .25 + 1.5,
          col = alpha(color1, i ^ 8)
        )
        text(
          x = 17,
          y = center + (nfac + 1) * nrow / 2 + 2 * j + 1.5,
          labels = i,
          cex = .375
        )
      }
      if (i > 0) {
        rect(
          16,
          center + (nfac + 1) * nrow / 2 + 2 * j - .25 + 1.5,
          16.5,
          center + (nfac + 1) * nrow / 2 + 2 * j + .25 + 1.5,
          col = grey
        )
        rect(
          16,
          center + (nfac + 1) * nrow / 2 + 2 * j - .25 + 1.5,
          16.5,
          center + (nfac + 1) * nrow / 2 + 2 * j + .25 + 1.5,
          col = alpha(color2, i ^ 8)
        )
        text(
          x = 17,
          y = center + (nfac + 1) * nrow / 2 + 2 * j + 1.5,
          labels = i,
          cex = .375
        )
      }
      if (i == 0) {
        rect(
          16,
          center + (nfac + 1) * nrow / 2 + 2 * j - .25 + 1.5,
          16.5,
          center + (nfac + 1) * nrow / 2 + 2 * j + .25 + 1.5,
          col = grey
        )
        text(
          x = 17,
          y = center + (nfac + 1) * nrow / 2 + 2 * j + 1.5,
          labels = i,
          cex = .375
        )
      }
      
      
      j  = j + .25
    }
  }



orderedArray = loadArray[c(1, 10, 7, 4, 3, 11, 6, 2, 9, 8, 5, 12),]
orderedOpacity = opacity[c(1, 10, 7, 4, 3, 11, 6, 2, 9, 8, 5, 12),]
showZero = FALSE
#resize dots to fit on graph
dotscale = 1 / 3.7 #Aquileguia
pdf(file = "FacPlot.pdf")
LoadMatrixBuild(orderedArray, orderedOpacity, 12.5, showZero, dotscale)
dev.off()

