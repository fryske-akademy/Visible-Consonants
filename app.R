library(shiny)
library(shinyBS)
library(shinyjs)
library(stats)
library(tidyr)
library(PBSmapping)
library(formattable)
library(ggplot2)
library(ggrepel)
library(readxl)
library(WriteXLS)
library(pracma)
library(plyr)
library(grid)
library(svglite)
library(Cairo)
library(tikzDevice)
library(shinybusy)

# splitstackshape, DT, psych

# sudo apt-get install libcairo2-dev
# sudo apt-get install libxt-dev

################################################################################

Scale <- function(h,replyScale,Ref)
{
  if (replyScale==" none")
  {
    s <- h
  }

  if (replyScale==" Hz")
  {
    s <- h
  }
  
  if (replyScale==" bark I")
  {
    s = 7*log(h/650+sqrt(1+(h/650)^2))
  }
  
  if (replyScale==" bark II")
  {
    s = 13 * atan(0.00076*h) + 3.5 * atan((h/7500)^2)
  }
  
  if (replyScale==" bark III")
  {
    s <- (26.81 * (h/(1960+h))) - 0.53
    
    s[which(s< 2  )] <- s[which(s< 2  )] + (0.15 * (2-s[which(s< 2  )]     ))
    s[which(s>20.1)] <- s[which(s>20.1)] + (0.22 * (  s[which(s>20.1)]-20.1))
  }
  
  if (replyScale==" ERB I")
  {
    s <- 16.7 * log10(1 + (h/165.4))
  }
  
  if (replyScale==" ERB II")
  {
    s <- 11.17 * log((h+312) / (h+14675)) + 43
  }
  
  if (replyScale==" ERB III")
  {
    s <- 21.4 * log10((0.00437*h)+1)
  }
  
  if (replyScale==" ln")
  {
    s <- log(h)
  }
  
  if (replyScale==" mel I")
  {
    s <- (1000/log10(2)) * log10((h/1000) + 1)
  }
  
  if (replyScale==" mel II")
  {
    s <-1127 * log(1 + (h/700))
  }

  if (replyScale==" ST")
  {
    s <- 12 * log2(h/Ref)
  }

  return(s)
}

vowelScale <- function(vowelTab, replyScale, Ref)
{
  if (is.null(vowelTab))
    return(NULL)
  
  vT <- vowelTab
  
  indices <- grep("^[<|>][f|F]", colnames(vT))

  for (i in indices)
  {
    vT[,i] <- Scale(vT[,i],replyScale, Ref)
  }
  
  return(vT)
}

consonantScale <- function(consonantTab, replyScale, Ref, varList)
{
  if (is.null(consonantTab))
    return(NULL)

  vT <- consonantTab

  for (i in 1:length(varList))
  {
    indexVar <- grep(paste0("^", varList[i], "$"), colnames(vT))
    vT[,indexVar] <- Scale(vT[,indexVar], replyScale, Ref)
  }

  return(vT)
}

################################################################################

consonantNormD <- function(consonantTab,replyNormal,replyVariable)
{
  if ((is.null(consonantTab)) || (length(replyNormal)==0))
    return(NULL)

  indexConsonant <- grep("^consonant$", colnames(consonantTab))
  indexVar       <- which(colnames(consonantTab)==replyVariable)

   SpeaKer <- unique(consonantTab[,1])
  nSpeaKer <- length(SpeaKer)

   VoWel   <- unique(consonantTab[,indexConsonant])
  nVoWel   <- length(VoWel)

  if (replyNormal=="")
  {
    return(consonantTab)
  }
  else

  if (grepl("Lobanov", replyNormal) & (nVoWel==1))
  {
    vT    <- data.frame()
    vTAg  <- data.frame(consonantTab[,1],consonantTab[,indexVar])

    for (q in (1:nSpeaKer))
    {
      vTAgSub <- subset(vTAg, vTAg[,1]==SpeaKer[q])

      meanD <- mean(vTAgSub[,2])
        sdD <-   sd(vTAgSub[,2])

      vTsub <- subset(consonantTab, consonantTab[,1]==SpeaKer[q])
      vTsub$orig <- vTsub[,indexVar]
      vTsub[,indexVar] <- (vTsub[,indexVar]-meanD)/sdD

      vT <- rbind(vT,vTsub)
    }
    
    if (grepl("rescaled", replyNormal))
    {
      lmmod <- lm(vT$orig~vT[,indexVar])
      vT[,indexVar] <- as.numeric(predict(lmmod))
    }
    
    return(vT)
  }
  else

  if (grepl("Lobanov", replyNormal) & (nVoWel> 1))
  {
    vT    <- data.frame()
    vTAg  <- aggregate(consonantTab[,indexVar]~consonantTab[,1]+consonantTab[,indexConsonant], FUN=mean)

    for (q in (1:nSpeaKer))
    {
      vTAgSub <- subset(vTAg, vTAg[,1]==SpeaKer[q])

      meanD <- mean(vTAgSub[,3])
        sdD <-   sd(vTAgSub[,3])

      vTsub <- subset(consonantTab, consonantTab[,1]==SpeaKer[q])
      vTsub$orig <- vTsub[,indexVar]
      vTsub[,indexVar] <- (vTsub[,indexVar]-meanD)/sdD

      vT <- rbind(vT,vTsub)
    }
    
    if (grepl("rescaled", replyNormal))
    {
      lmmod <- lm(vT$orig~vT[,indexVar])
      vT[,indexVar] <- as.numeric(predict(lmmod))
    }
    
    return(vT)
  }
  else
    return(NULL)
}

################################################################################

optionsScale <- function(variable)
{
  options <- c("Hz" = " Hz",
               "bark: Schroeder et al. (1979)" = " bark I",
               "bark: Zwicker & Terhardt (1980)" = " bark II",
               "bark: Traunmüller (1990)" = " bark III",
               "ERB: Greenwood (1961)" = " ERB I",
               "ERB: Moore & Glasberg (1983)" = " ERB II",
               "ERB: Glasberg & Moore (1990)" = " ERB III",
               "ln" = " ln",
               "mel: Fant (1968)" = " mel I",
               "mel: O'Shaughnessy (1987)" = " mel II",
               "ST" = " ST")
  
  return(options)
}

optionsNormal <- function()
{
  options <- c("None"                    = ""                 ,
               "Lobanov (1971)"          = " Lobanov"         ,
               "Lobanov (1971) rescaled" = " Lobanov rescaled")
}

################################################################################

ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/styles.css"), tags$head(includeHTML("GA.html")),

  img(src = 'FA1.png', height = 39, align = "right", style = 'margin-top: 16px; margin-right: 15px;'),
  titlePanel(title = HTML("<div class='title'>Visible Consonants<div>"), windowTitle = "Visible Consonants"),

  tags$head(
    tags$link(rel="icon", href="FA2.png"),

    tags$meta(charset="UTF-8"),
    tags$meta(name   ="description", content="Visible Consonants is a web app for the analysis of acoustic consonant measurements. The app is an useful instrument for research in phonetics, sociolinguistics, dialectology, forensic linguistics, and speech-language pathology."),
  ),

  navbarPage
  (
    title=NULL, id = "navBar", collapsible = TRUE,

    tabPanel
    (
      title = "Load file",
      value = "load_file",

      fluidPage
      (
        style = "border: 1px solid silver; padding: 6px; min-height: 690px;",

        fluidPage
        (
          fileInput('consonantFile', 'Upload xlsx file', accept = c(".xlsx"), width="40%")
        ),

        fluidPage
        (
          style = "font-size: 90%; white-space: nowrap;",
          align = "center",
          DT::dataTableOutput('consonantRound')
        )
      )
    ),

    tabPanel
    (
      title = "1D plot",
      value = "1d plot",

      splitLayout
      (
        style = "border: 1px solid silver;",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          textInput("title2", "Plot title", "", width="100%"),
          
          splitLayout
          (
            cellWidths = c("50%", "49%"),
            uiOutput('selVariable2'),
            uiOutput('selGraph2')
          ),
          
          splitLayout
          (
            cellWidths = c("50%", "49%"),
            uiOutput('selScale2'),
            uiOutput('selNormal2'),
          ),
          
          uiOutput('selRef2'),
          
          splitLayout
          (
            cellWidths = c("70%", "29%"),

            radioButtons("selError2", "Size of confidence intervals:", c("0%","90%","95%","99%"), selected = "95%", inline = TRUE),
            radioButtons("selMeasure2", "Use:", c("SD","SE"), selected = "SE", inline = TRUE)
          ),

          splitLayout
          (
            uiOutput('selXaxis2'),
            uiOutput('selLine2'),
            uiOutput('selPlot2')
          ),

          splitLayout
          (
            uiOutput('catXaxis2'),
            uiOutput('catLine2'),
            uiOutput('catPlot2')
          ),

          checkboxGroupInput("selGeon2", "Options:", c("average", "rotate x-axis labels"), inline=TRUE)
        ),

        column
        (
          width = 12,

          uiOutput("Graph2"),

          column
          (
            width = 11,

            splitLayout
            (
              uiOutput('selFormat2a'),
              downloadButton('download2a', 'Table'),

              uiOutput('selSize2b'),
              uiOutput('selFont2b'),
              uiOutput('selPoint2b'),
              uiOutput('selFormat2b'),
              downloadButton('download2b', 'Graph')
            )
          )
        )
      )
    ),

    tabPanel
    (
      title = "2D plot",
      value = "2d plot",

      splitLayout
      (
        style = "border: 1px solid silver;",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          textInput("title3", "Plot title", "", width="100%"),

          splitLayout
          (
            cellWidths = c("50%", "49%"),
            uiOutput('selScaleX3'),
            uiOutput('selScaleY3')
          ),

          uiOutput('selRef3'),

          splitLayout
          (
            cellWidths = c("50%", "49%"),
            uiOutput('selNormalX3'),
            uiOutput('selNormalY3')
          ),

          splitLayout
          (
            uiOutput('selColor3'),
            uiOutput('selShape3'),
            uiOutput('selPlot3')
          ),

          splitLayout
          (
            uiOutput('catColor3'),
            uiOutput('catShape3'),
            uiOutput('catPlot3')
          ),

          uiOutput('selGeon3'),
          uiOutput('selPars' ),

          splitLayout
          (
            cellWidths = c("28%", "26%", "45%"),
            checkboxInput("grayscale3", "grayscale"         , FALSE),
            checkboxInput("average3"  , "average"           , FALSE)
          )
        ),

        column
        (
          width=12,

          splitLayout
          (
            cellWidths = c("85%", "15%"),

            uiOutput("Graph3"),

            column
            (
              width=12,

              br(),br(),

              uiOutput('selAxisX3'),
              uiOutput('selAxisY3'),

              checkboxInput("selManual", "min/max", FALSE),
              uiOutput('selVar1min'),
              uiOutput('selVar1max'),
              uiOutput('selVar2min'),
              uiOutput('selVar2max')

            )
          ),

          column
          (
            width = 11,

            splitLayout
            (
              uiOutput('selFormat3a'),
              downloadButton('download3a', 'Table'),

              uiOutput('selSize3b'),
              uiOutput('selFont3b'),
              uiOutput('selPoint3b'),
              uiOutput('selFormat3b'),
              downloadButton('download3b', 'Graph')
            )
          )
        )
      )
    ),

    tabPanel
    (
      title = "Contours",
      value = "contours",

      splitLayout
      (
        style = "border: 1px solid silver;",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          textInput("title0", "Plot title", "", width="100%"),
          uiOutput('selVowel0'),
          uiOutput('selScale0'),
          uiOutput('selRef0'),

          splitLayout
          (
            cellWidths = c("70%", "29%"),

            radioButtons("selError0", "Size of confidence intervals:", c("0%","90%","95%","99%"), selected = "0%", inline = TRUE),
            radioButtons("selMeasure0", "Use:", c("SD","SE"), selected = "SE", inline = TRUE)
          ),

          splitLayout
          (
            uiOutput('selVar0'),
            uiOutput('selLine0'),
            uiOutput('selPlot0')
          ),

          splitLayout
          (
            uiOutput('catXaxis0'),
            uiOutput('catLine0'),
            uiOutput('catPlot0')
          ),

          checkboxGroupInput("selGeon0", "Options:", c("average", "points", "smooth"), selected="smooth", inline=TRUE)
        ),

        column
        (
          width = 12,

          uiOutput("Graph0"),

          column
          (
            width = 11,

            splitLayout
            (
              uiOutput('selFormat0a'),
              downloadButton('download0a', 'Table'),

              uiOutput('selSize0b'),
              uiOutput('selFont0b'),
              uiOutput('selPoint0b'),
              uiOutput('selFormat0b'),
              downloadButton('download0b', 'Graph')
            )
          )
        )
      )
    ),

    tabPanel
    (
      title = "Temporal",
      value = "temporal",
      
      splitLayout
      (
        style = "border: 1px solid silver;",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),
        
        column
        (
          width=12,
          
          textInput("title1", "Plot title", "", width="100%"),
          
          splitLayout
          (
            cellWidths = c("50%", "49%"),
            uiOutput('selVariable1'),
            uiOutput('selSequence1'),
          ),
          
          uiOutput('selNormal1'),
          
          splitLayout
          (
            uiOutput('selXaxis1'),
            uiOutput('selPlot1')
          ),
          
          splitLayout
          (
            uiOutput('catXaxis1'),
            uiOutput('catPlot1')
          ),
          
          checkboxGroupInput("selGeon1", "Options:", c("average"), inline=TRUE)
        ),
        
        column
        (
          width = 12,
          
          uiOutput("Graph1"),
          
          column
          (
            width = 11,
            
            splitLayout
            (
              uiOutput('selFormat1a'),
              downloadButton('download1a', 'Table'),
              
              uiOutput('selSize1b'),
              uiOutput('selFont1b'),
              uiOutput('selPoint1b'),
              uiOutput('selFormat1b'),
              downloadButton('download1b', 'Graph')
            )
          )
        )
      )
    ),
    
    navbarMenu("More",

    tabPanel
    (
      title = "Help",
      value = "help",

      fluidPage
      (
        style = "border: 1px solid silver;",

        br(),
        h5(strong("About")),
        p("Visible Consonants is a web app for the analysis of acoustic consonant measurements. The app is an useful instrument for research in phonetics, sociolinguistics, dialectology, forensic linguistics, and speech-language pathology. The following people were involved in the development of Visible Consonants: Wilbert Heeringa (implementation), Hans Van de Velde (project manager), Vincent van Heuven (advice). Visible Consonants is still under development. Comments are welcome and can be sent to", img(src = 'email.png', height = 20, align = "center"),"."),
        br(),
        h5(strong("System requirements")),
        p("Visible Consonants runs best on a computer with a monitor with a minimum resolution of 1370 x 870 (width x height). The use of Mozilla Firefox as a web browser is to be preferred."),
        br(),
        h5(strong("Format")),
        p("The input file should be a spreadsheet that is created in Excel or LibreOffice. It should be saved as an Excel 2007/2010/2013 XML file, i.e. with extension '.xlsx'. The spreadsheet should include the following variables (shown in red):"),
        br(),

        tags$div(tags$ul
        (
          tags$li(tags$span(HTML("<span style='font-variant: small-caps; color:blue'>General</span>"),div(tags$ul(
            tags$li(tags$span(HTML("<span style='color:crimson'>speaker</span>")           ,p("Contains the speaker labels. This column is obligatory."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>consonant</span>")         ,p("Contains the consonant labels. Multiple pronunciations of the same consonant per speaker are possible. In case you want to use IPA characters, enter them as Unicode characters. In order to find Unicode IPA characters, use the online", tags$a(href="http://westonruter.github.io/ipa-chart/keyboard/", "IPA Chart Keyboard", target="_blank"), "of Weston Ruter. This column is obligatory."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>sequence</span>")          ,p("Indicates the context in which a consonant appears. Should be VC, VCV or CV. This column is obligatory.")))
          )))),
          
          tags$li(tags$span(HTML("<span style='font-variant: small-caps; color:blue'>Sociolinguistic</span>"),div(tags$ul(
            tags$li(tags$span(HTML("<span style='color:crimson'>...</span>")               ,p("An arbitrary number of columns representing categorical variables such as location, language, gender, age group, etc. may follow, but is not obligatory. See to it that each categorical variable has an unique set of different values. Prevent the use of numbers, rather use meaningful codes. For example, rather then using codes '1' and '2' for a variable 'age group' use 'old' and 'young' or 'o' and 'y'.")))
          )))),
          
          tags$li(tags$span(HTML("<span style='font-variant: small-caps; color:blue'>Consonant</span>"),div(tags$ul(
            tags$li(tags$span(HTML("<span style='color:crimson'>duration</span>")          ,p("Durations of the consonants. The measurements may be either in seconds or milliseconds. This column is obligatory."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>...</span>")               ,p("Any number of numerical variables such as amplitude, intensity, etc."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>time var1 var2 ...</span>"),p("A set of variables that occurs multiple times. The variable 'time' gives the time point within the consonant interval in seconds or milliseconds, i.e. it is assumed that the consonant interval starts at 0 (milli)seconds. The variables should be measured at the time given in the column 'time'. The program assumes that spectral variables, if any, are measured in Hertz and not normalized. A set may be repeated as ",em("many times"), " as the user wishes, but should occur at least two times. For each repetition the same column names should be used.")))
          ))))
        )),

        br(),

        p("In order to visualize formant transitions of vowels preceding a consonant, the following variables should be added:"),
        br(),
        
        tags$div(tags$ul
        (
          tags$li(tags$span(HTML("<span style='font-variant: small-caps; color:blue'>Vowel before consonant</span>"),div(tags$ul(
            tags$li(tags$span(HTML("<span style='color:crimson'>&lt;vowel</span>")         ,p("The label of the vowel that precedes the consonant. In case you want to use IPA characters, enter them as Unicode characters. This column is obligatory."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>&lt;duration</span>")      ,p("The measurements may be either in seconds or milliseconds. This column is obligatory but may be kept empty."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>...</span>")               ,p("Any number of numerical variables. They should be given for the consonant as well."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>&lt;time &lt;f0 &lt;F1 &lt;F2 &lt;F3</span>"),p(HTML("A set of five columns should follow multiple times: '&lt;time', '&lt;f0', '&lt;F1', '&lt;F2' and '&lt;F3'. The variable '&lt;time' gives the time point within the vowel interval in seconds or milliseconds, i.e. it is assumed that the vowel interval starts at 0 (milli)seconds. The f0, F1, F2 and F3 should be measured at the time given in the column '&lt;time'. The program assumes that they are measured in Hertz and not normalized. The set of five columns may be repeated as ",em("many times"), " as the user wishes, but should occur at least two times. For each repetition the same column names should be used. A set should always include all five columns, but the columns '&lt;f0' and '&lt;F3' may be kept empty."))))
          ))))
        )),

        br(),
        p("In order to visualize formant transitions of vowels following a consonant, the following variables should be added:"),
        br(),
        
        tags$div(tags$ul
        (
          tags$li(tags$span(HTML("<span style='font-variant: small-caps; color:blue'>Vowel after consonant</span>"),div(tags$ul(
            tags$li(tags$span(HTML("<span style='color:crimson'>&gt;vowel</span>")         ,p("The label of the vowel that precedes the consonant. In case you want to use IPA characters, enter them as Unicode characters. This column is obligatory."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>&gt;duration</span>")      ,p("The measurements may be either in seconds or milliseconds. This column is obligatory but may be kept empty."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>...</span>")               ,p("Any number of numerical variables. They should be given for the consonant as well."))),
            tags$li(tags$span(HTML("<span style='color:crimson'>&gt;time &gt;f0 &gt;F1 &gt;F2 &gt;F3</span>"),p(HTML("A set of five columns should follow multiple times: '&gt;time', '&gt;f0', '&gt;F1', '&gt;F2' and '&gt;F3'. The variable '&gt;time' gives the time point within the vowel interval in seconds or milliseconds, i.e. it is assumed that the vowel interval starts at 0 (milli)seconds. The f0, F1, F2 and F3 should be measured at the time given in the column '&gt;time'. The program assumes that they are measured in Hertz and not normalized. The set of five columns may be repeated as ",em("many times"), " as the user wishes, but should occur at least two times. For each repetition the same column names should be used. A set should always include all five columns, but the columns '&gt;f0' and '&gt;F3' may be kept empty."))))
          ))))
        )),

        br(),
        p("The structure of  the input file can be visualized schematically as follows:"),
        br(),
        div(img(src = 'struct.png', height=330), style="margin-left: 32px;"),
        br(), br(),

        h5(strong("Graphs")),
        p("Graphs can be saved in six formats: JPG, PNG, SVG, EPS, PDF and TEX. TEX files are created with TikZ. When using this format, it is assumed that XeLaTeX is installed. Generating a TikZ may take a long time. When including a TikZ file in a LaTeX document, you need to use a font that supports the IPA Unicode characters, for example: 'Doulos SIL', 'Charis SIL' or 'Linux Libertine O'. You also need to adjust the left margin and the scaling of the graph. The LaTeX document should be compiled with", code("xelatex"), ". Example of a LaTeX file in which a TikZ file is included:"),
        br(),

        code(style="margin-left: 36px;", "\\documentclass{minimal}"),
        br(), br(),
        code(style="margin-left: 36px;", "\\usepackage{tikz}"),
        br(),
        code(style="margin-left: 36px;", "\\usepackage{fontspec}"),
        br(),
        code(style="margin-left: 36px;", "\\setmainfont{Linux Libertine O}"),
        br(), br(),
        code(style="margin-left: 36px;", "\\begin{document}"),
        br(),
        code(style="margin-left: 36px;", "{\\hspace*{-3cm}\\scalebox{0.8}{\\input{formantPlot.TEX}}}"),
        br(),
        code(style="margin-left: 36px;", "\\end{document}"),
        br(), br(), br(),
        
        h5(strong("Implementation")),
        p("This program is implemented as a Shiny app. Shiny was developed by RStudio. This app uses the following R packages:"),
        br(),

        tags$div(tags$ul
        (
          tags$li(tags$span(HTML("<span style='color:blue'>base</span>"),p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shiny</span>"),p("Winston Chang, Joe Cheng, J.J. Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.0. https://CRAN.R-project.org/package=shiny"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyBS</span>"),p("Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61. https://CRAN.R-project.org/package=shinyBS"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyjs</span>"), p("Dean Attali (2018). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 1.0. https://CRAN.R-project.org/package=shinyjs"))),
          tags$li(tags$span(HTML("<span style='color:blue'>tidyr</span>"),p("Hadley Wickham and Lionel Henry (2019). tidyr: Tidy Messy Data. R package version 1.0.0. https://CRAN.R-project.org/package=tidyr"))),
          tags$li(tags$span(HTML("<span style='color:blue'>PBSmapping</span>"),p("Jon T. Schnute, Nicholas Boers and Rowan Haigh (2019). PBSmapping: Mapping Fisheries Data and Spatial Analysis Tools. R package version 2.72.1. https://CRAN.R-project.org/package=PBSmapping"))),
          tags$li(tags$span(HTML("<span style='color:blue'>formattable</span>"),p("Kun Ren and Kenton Russell (2016). formattable: Create 'Formattable' Data Structures. R package version 0.2.0.1. https://CRAN.R-project.org/package=formattable"))),
          tags$li(tags$span(HTML("<span style='color:blue'>ggplot2</span>"),p("H. Wickham (2009). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. http://ggplot2.org"))),
          tags$li(tags$span(HTML("<span style='color:blue'>ggrepel</span>"),p("Kamil Slowikowski (2017). ggrepel: Repulsive Text and Label Geoms for 'ggplot2'. R package version 0.7.0. https://CRAN.R-project.org/package=ggrepel"))),
          tags$li(tags$span(HTML("<span style='color:blue'>readxl</span>"),p("Hadley Wickham and Jennifer Bryan (2017). readxl: Read Excel Files. R package version 1.0.0. https://CRAN.R-project.org/package=readxl"))),
          tags$li(tags$span(HTML("<span style='color:blue'>WriteXLS</span>"),p("Marc Schwartz and various authors. (2015). WriteXLS: Cross-Platform Perl Based R Function to Create Excel 2003 (XLS) and Excel 2007 (XLSX) Files. R package version 4.0.0. https://CRAN.R-project.org/package=WriteXLS"))),
          tags$li(tags$span(HTML("<span style='color:blue'>DT</span>"),p("Yihui Xie (2016). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.2. https://CRAN.R-project.org/package=DT"))),
          tags$li(tags$span(HTML("<span style='color:blue'>pracma</span>"),p("Hans Werner Borchers (2017). pracma: Practical Numerical Math Functions. R package version 1.9.9. https://CRAN.R-project.org/package=pracma"))),
          tags$li(tags$span(HTML("<span style='color:blue'>plyr</span>"),p("Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. http://www.jstatsoft.org/v40/i01/"))),
          tags$li(tags$span(HTML("<span style='color:blue'>svglite</span>"),p("Hadley Wickham, Lionel Henry, T Jake Luciani, Matthieu Decorde and Vaudor Lise (2016). svglite: An 'SVG' Graphics Device. R package version 1.2.0. https://CRAN.R-project.org/package=svglite"))),
          tags$li(tags$span(HTML("<span style='color:blue'>Cairo</span>"),p("Simon Urbanek and Jeffrey Horner (2015). Cairo: R graphics device using cairo graphics library for creating high-quality bitmap (PNG, JPEG, TIFF),  vector (PDF, SVG, PostScript) and display (X11 and Win32) output. R package version 1.5-9. https://CRAN.R-project.org/package=Cairo"))),
          tags$li(tags$span(HTML("<span style='color:blue'>tikzDevice</span>"),p("Charlie Sharpsteen and Cameron Bracken (2020). tikzDevice: R Graphics Output in LaTeX Format. R package version 0.12.3.1. https://CRAN.R-project.org/package=tikzDevice"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinybusy</span>"),p("Fanny Meyer and Victor Perrier (2020). shinybusy: Busy Indicator for 'Shiny' Applications. R package version 0.2.2. https://CRAN.R-project.org/package=shinybusy")))
        )),

        br(),
        h5(strong("How to cite this app")),
        p(HTML("Heeringa, Wilbert & Van de Velde, Hans (2024). Visible Consonants [computer program]. Retrieved 30 January 2024 from <span style='font-family: \"Lucida Console\", \"Menlo\", \"Monaco\", \"Courier\", monospace;'>https://www.visibleconsonants.org/</span>.")),
        br()
      ),

      br()
    ),

    tabPanel
    (
      title = "Disclaimer",
      value = "disclaimer",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: 690px;",

        br(),
        h5(strong("Privacy")),
        p("This app uses cookies that are used to collect data. By using this site you agree to these cookies being set. Google Analytics is used in order to track and report website traffic. See: ", a("How Google uses data when you use our partners' sites or apps", href = "https://www.google.com/policies/privacy/partners/", target = "_blank"),"."),
        br(),
        h5(strong("Liability")),
        p("This app is provided 'as is' without warranty of any kind, either express or implied, including, but not limited to, the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Fryske Akademy makes no warranty that: 1) the app will meet your requirements, 2) the app will be uninterrupted, timely, secure or error-free, 3) the results that may be obtained from the use of the app will be effective, accurate or reliable, 4) the quality of the app will meet your expectations, 5) any errors in the app will be corrected."),
        br(),
        p("The app and its documentation could include technical or other mistakes, inaccuracies or typographical errors. The Fryske Akademy may make changes to the app or documentation made available on its web site. The app and its documentation may be out of date, and the Fryske Akademy makes no commitment to update such materials."),
        br(),
        p("The Fryske Akademy assumes no responsibility for errors or ommissions in the app or documentation available from its web site."),
        br(),
        p("In no event shall the Fryske Akademy be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Fryske Akademy has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software."),
        br(),
        p("The use of the app is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from the Fryske Akademy shall create any warranty for the software."),
        br(),
        h5(strong("Other")),
        p("The disclaimer may be changed from time to time."),
        br()
      )
    ))
  ),
  
  tags$td(textOutput("heartbeat"))
)

################################################################################

server <- function(input, output, session)
{
  observeEvent(input$navBar,
  {
    if (getUrlHash() == paste0("#", input$navBar)) return()
    updateQueryString(paste0("#", input$navBar), mode = "push")
  })

  observeEvent(getUrlHash(),
  {
    Hash <- getUrlHash()
    if (Hash == paste0("#", input$navBar)) return()
    Hash <- gsub("#", "", Hash)
    updateNavbarPage(session, "navBar", selected=Hash)
  })

  output$heartbeat <- renderText(
  {
    invalidateLater(5000)
    Sys.time()
  })

  ##############################################################################

  consonantFile <- reactive(
  {
    inFile <- input$consonantFile

    if (is.null(inFile))
      return(NULL)

    file.rename(inFile$datapath, paste0(inFile$datapath,".xlsx"))
    return(as.data.frame(read_excel(paste0(inFile$datapath,".xlsx"), sheet=1, .name_repair = "minimal")))
  })

  checkVar <- function(varName, varIndex, checkEmpty)
  {
    indexVar <- grep(paste0("^", varName, "$"), tolower(colnames(consonantFile())))
    
    if (!is.element(tolower(varName), tolower(colnames(consonantFile()))))
      Message <- paste0("Column '", varName, "' not found.")
    else
      
    if ((varIndex!=0) && (tolower(colnames(consonantFile())[varIndex])!=tolower(varName)))
      Message <- paste0("'", varName, "' found in the wrong column.")
    else
        
    if (checkEmpty && (sum(is.na(consonantFile()[,indexVar]))==nrow(consonantFile())))
      Message <- paste0("Column '", varName, "' is empty.")
    else
      Message <- "OK"          
    
    return(Message)
  }

  checkVars <- reactive(
  {
    if (is.null(consonantFile()))
      return(NULL)

    m <- checkVar("speaker"  , 1, T)
    if (m!="OK") return(m)

    m <- checkVar("consonant", 2, T)
    if (m!="OK") return(m)
    
    m <- checkVar("sequence" , 3, T)
    if (m!="OK") return(m)
    
    m <- checkVar("duration" , 0, T)
    if (m!="OK") return(m)
    
    return("OK")
  })

  Round <- function(x)
  {
    return(trunc(x+0.5))
  }

  consonantRound <- reactive(
  {
    if ((is.null(consonantFile())) || !is.element("duration", colnames(consonantFile())))
      return(NULL)

    # check
    if (checkVars()!="OK")
    {
      showNotification(checkVars(), type = "error", duration = 10)
      return(NULL)
    }

    vT <- consonantFile()

    first <- which(colnames(vT)=="duration")
    last  <- ncol(consonantFile())
    
    for (i in (first:last))
    {
      if ((colnames(vT)[i]!="<vowel") & (colnames(vT)[i]!=">vowel"))
      {
        if (is.character(vT[,i]))
        {
          vT[,i] <- as.numeric(vT[,i])
        }
        
        if (sum(is.na(vT[,i]))<nrow(vT))
        {
          if (max(vT[,i], na.rm = TRUE)<=1)
          {
            vT[,i] <- round(((Round(vT[,i]*1000))/1000),3)
          }
          else
          {
            vT[,i] <- Round(vT[,i])
          }
        }
      }
    }

    return(vT)
  })

  output$consonantRound <- DT::renderDataTable(expr = consonantRound(), options = list(scrollX = TRUE))

  consonantTab <- reactive(
  {
    if (is.null(consonantFile()) || (checkVars()!="OK"))
      return(NULL)

    vT <- consonantFile()

    indexDuration <- grep("^duration$", tolower(colnames(vT)))

    if (indexDuration > 3)
    {
      cnames <- colnames(vT)
      vT <- data.frame(vT[,1], vT[,3:(indexDuration-1)], vT[,2], vT[,indexDuration:ncol(vT)])
      cnames <- c(cnames[1],cnames[3:(indexDuration-1)],cnames[2],cnames[indexDuration:ncol(vT)])
      colnames(vT) <- cnames
    }
    else {}

    indexConsonant <- grep("^consonant$", tolower(trimws(colnames(vT), "r")))

    for (k in 1:ncol(vT))
    {
      if (grepl("^<F1",toupper(colnames(vT)[k])) |
          grepl("^<F2",toupper(colnames(vT)[k])) |
          grepl("^<F3",toupper(colnames(vT)[k])) |
          grepl("^>F1",toupper(colnames(vT)[k])) |
          grepl("^>F2",toupper(colnames(vT)[k])) |
          grepl("^>F3",toupper(colnames(vT)[k])))
      {
        colnames(vT)[k] <- toupper(colnames(vT)[k])
      }
      else
      {
        colnames(vT)[k] <- tolower(trimws(colnames(vT)[k], "r"))
      }
    }

    for (i in ((indexConsonant+1):(ncol(vT))))
    {
      if ((colnames(vT)[i]!="<vowel") & (colnames(vT)[i]!=">vowel"))
      {
        if (is.character(vT[,i]))
        {
          vT[,i] <- as.numeric(vT[,i])
        }
          
        if (sum(is.na(vT[,i]))==nrow(vT))
        {
          vT[,i] <- 0
        }
      }

      if (grepl("time", tolower(colnames(vT)[i])))
        vT[,i] <- vT[,i] * 1000
      else

      if (grepl("durat", tolower(colnames(vT)[i])))
        vT[,i] <- vT[,i] * 1000
      else
        
      if ((grepl("^ampl", tolower(colnames(vT)[i]))) | (grepl("^<ampl", tolower(colnames(vT)[i]))) | (grepl("^>ampl", tolower(colnames(vT)[i]))))
        vT[,i] <- vT[,i] * 1000000
      else {}
    }

    if (is.element("<vowel", colnames(vT)))
      vT$`<vowel`[is.na(vT$`<vowel`)] <- "∅"
    
    if (is.element(">vowel", colnames(vT)))
      vT$`>vowel`[is.na(vT$`>vowel`)] <- "∅"

    return(vT)
  })

  ##############################################################################

  consonantScale2 <- reactive(
  {
    if ((length(input$replyRef2)==0) || is.na(input$replyRef2))
      Ref <- 50
    else
      Ref <- input$replyRef2
      
    return(consonantScale(consonantTab(),input$replyScale2,Ref,input$replyVariable2))
  })
  
  consonantNorm2 <- reactive(
  {
    if (!all(input$replyVariable2 %in% colnames(consonantScale2())))
      return(NULL)
    
    return(consonantNormD(consonantScale2(),input$replyNormal2,input$replyVariable2))
  })

  fuseCols <- function(vT,replyValue)
  {
    columns <- ""

    if (length(replyValue)>0)
    {
      for (i in (1:length(replyValue)))
      {
        indexValue <- grep(paste0("^",as.character(replyValue)[i],"$"), colnames(vT))

        if (i==1)
          columns <- paste0(columns,vT[,indexValue])
        else
          columns <- paste (columns,vT[,indexValue])
      }
    }

    return(columns)
  }

  consonantSub2 <- reactive(
  {
    if (is.null(consonantNorm2()) || (nrow(consonantNorm2())==0) || (length(input$catXaxis2)==0))
      return(NULL)

    vT <- consonantNorm2()

    indexConsonant <- grep("^consonant$", colnames(vT))
    indexVar       <- which(colnames(vT)==input$replyVariable2)

    vT <- subset(vT, !is.na(vT[,indexVar]))
    
    vT$variable <- vT[,indexVar]
    
    vT$indexXaxis <- fuseCols(vT,input$replyXaxis2)
    vT$indexLine  <- fuseCols(vT,input$replyLine2)
    vT$indexPlot  <- fuseCols(vT,input$replyPlot2)

    if (input$selError2=="0%")
      z <- 0
    if (input$selError2=="90%")
      z <- 1.645
    if (input$selError2=="95%")
      z <- 1.96
    if (input$selError2=="99%")
      z <- 2.575

    vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis2))

    if (nrow(vT)==0)
      return(NULL)

    if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)==0))
    {
      if (is.element("average",input$selGeon2))
      {
        vT <- data.frame(indexXaxis=vT$indexXaxis, speaker=vT$speaker, consonant=vT$consonant, variable=vT$variable)
        vT <- aggregate(variable ~ indexXaxis + speaker + consonant, data=vT, FUN=mean)
        vT <- aggregate(variable ~ indexXaxis +           consonant, data=vT, FUN=mean)
      }

      ag    <- aggregate(variable ~ indexXaxis, data=vT, FUN=mean)
      ag$sd <- aggregate(variable ~ indexXaxis, data=vT, FUN=sd)[,2]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(variable ~ indexXaxis, data=vT, FUN=length)[,2]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure2=="SD")
      {
        ag$ll <- ag[,2] - z * ag$sd
        ag$ul <- ag[,2] + z * ag$sd
      }
      if (input$selMeasure2=="SE")
      {
        ag$ll <- ag[,2] - z * ag$se
        ag$ul <- ag[,2] + z * ag$se
      }

      ag <- ag[order(ag[,2]),]
      ag[,1] <- factor(ag[,1], levels=ag[,1])

      colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
      colnames(ag)[2] <- "variable"

      return(ag)
    }
    else

    if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)>0))
    {
      vT <- subset(vT, is.element(vT$indexPlot,input$catPlot2))

      if (nrow(vT)==0)
        return(data.frame())

      if (is.element("average",input$selGeon2))
      {
        vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, speaker=vT$speaker, consonant=vT$consonant, variable=vT$variable)
        vT <- aggregate(variable ~ indexXaxis + indexPlot + speaker + consonant, data=vT, FUN=mean)
        vT <- aggregate(variable ~ indexXaxis + indexPlot +           consonant, data=vT, FUN=mean)
      }

      ag    <- aggregate(variable ~ indexXaxis + indexPlot, data=vT, FUN=mean)
      ag$sd <- aggregate(variable ~ indexXaxis + indexPlot, data=vT, FUN=sd)[,3]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(variable ~ indexXaxis + indexPlot, data=vT, FUN=length)[,3]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure2=="SD")
      {
        ag$ll <- ag[,3] - z * ag$sd
        ag$ul <- ag[,3] + z * ag$sd
      }
      if (input$selMeasure2=="SE")
      {
        ag$ll <- ag[,3] - z * ag$se
        ag$ul <- ag[,3] + z * ag$se
      }

      ag <- ag[order(ag[,3]),]
      xx <- unique(ag[,1])

      ag0 <- data.frame()

      for (q in (1:length(xx)))
      {
        ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
      }

      ag <- ag0
      ag[,1] <- factor(ag[,1], levels=xx)
      ag[,2] <- as.character(ag[,2])

      ag <- ag[order(ag[,2]),]

      colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
      colnames(ag)[2] <- paste(input$replyPlot2 , collapse = " ")
      colnames(ag)[3] <- "variable"

      colnames(ag) <- make.unique(names(ag))

      return(ag)
    }
    else

    if (((length(input$catLine2)>0) & (length(input$catLine2)<=14)) && (length(input$catPlot2)==0))
    {
      vT <- subset(vT, is.element(vT$indexLine,input$catLine2))

      if (nrow(vT)==0)
        return(data.frame())

      if (is.element("average",input$selGeon2))
      {
        vT <- data.frame(indexXaxis=vT$indexXaxis, indexLine=vT$indexLine, speaker=vT$speaker, consonant=vT$consonant, variable=vT$variable)
        vT <- aggregate(variable ~ indexXaxis + indexLine + speaker + consonant, data=vT, FUN=mean)
        vT <- aggregate(variable ~ indexXaxis + indexLine +           consonant, data=vT, FUN=mean)
      }

      ag    <- aggregate(variable ~ indexXaxis + indexLine, data=vT, FUN=mean)
      ag$sd <- aggregate(variable ~ indexXaxis + indexLine, data=vT, FUN=sd)[,3]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(variable ~ indexXaxis + indexLine, data=vT, FUN=length)[,3]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure2=="SD")
      {
        ag$ll <- ag[,3] - z * ag$sd
        ag$ul <- ag[,3] + z * ag$sd
      }
      if (input$selMeasure2=="SE")
      {
        ag$ll <- ag[,3] - z * ag$se
        ag$ul <- ag[,3] + z * ag$se
      }

      ag <- ag[order(ag[,3]),]
      xx <- unique(ag[,1])

      ag0 <- data.frame()

      for (q in (1:length(xx)))
      {
        ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
      }

      ag <- ag0
      ag[,1] <- factor(ag[,1], levels=xx)
      ag[,2] <- as.character(ag[,2])

      colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
      colnames(ag)[2] <- paste(input$replyLine2 , collapse = " ")
      colnames(ag)[3] <- "variable"

      colnames(ag) <- make.unique(names(ag))

      return(ag)
    }
    else

    if (((length(input$catLine2)>0) & (length(input$catLine2)<=14))  && (length(input$catPlot2)>0))
    {
      vT <- subset(vT, is.element(vT$indexLine,input$catLine2) & is.element(vT$indexPlot,input$catPlot2))

      if (nrow(vT)==0)
        return(data.frame())

      if (is.element("average",input$selGeon2))
      {
        vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, indexLine=vT$indexLine, speaker=vT$speaker, consonant=vT$consonant, variable=vT$variable)
        vT <- aggregate(variable ~ indexXaxis + indexPlot + indexLine + speaker + consonant, data=vT, FUN=mean)
        vT <- aggregate(variable ~ indexXaxis + indexPlot + indexLine +           consonant, data=vT, FUN=mean)
      }

      ag    <- aggregate(variable ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, data=vT, FUN=mean)
      ag$sd <- aggregate(variable ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, data=vT, FUN=sd)[,4]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(variable ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, data=vT, FUN=length)[,4]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure2=="SD")
      {
        ag$ll <- ag[,4] - z * ag$sd
        ag$ul <- ag[,4] + z * ag$sd
      }
      if (input$selMeasure2=="SE")
      {
        ag$ll <- ag[,4] - z * ag$se
        ag$ul <- ag[,4] + z * ag$se
      }

      ag <- ag[order(ag[,4]),]
      xx <- unique(ag[,1])

      ag0 <- data.frame()

      for (q in (1:length(xx)))
      {
        ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
      }

      ag <- ag0
      ag[,1] <- factor(ag[,1], levels=xx)
      ag[,2] <- as.character(ag[,2])
      ag[,3] <- as.character(ag[,3])

      ag <- ag[order(ag[,3]),]

      colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
      colnames(ag)[2] <- paste(input$replyLine2 , collapse = " ")
      colnames(ag)[3] <- paste(input$replyPlot2 , collapse = " ")
      colnames(ag)[4] <- "variable"

      colnames(ag) <- make.unique(names(ag))

      return(ag)
    }
    else
      return(data.frame())
  })

  varList <- function(temporal)
  {
    req(consonantTab())
    
    colNames <- colnames(consonantTab())
    
    first <- which(colNames=="duration")
    last  <- ncol(consonantTab())

    varNames <- as.data.frame(table(colNames[first:last]))
    varNames <- subset(varNames, (Var1!="<vowel") & (Var1!=">vowel") &
                                 (Var1!="<time" ) & (Var1!=">time" ) & (Var1!="time"))

    if (!temporal)
    {
      v1 <- subset(varNames, (Freq==1) & (!grepl("^[<|>]", Var1)))$Var1
      v2 <- subset(varNames, (Freq==1) & ( grepl("^[<|>]", Var1)))$Var1
      return(c(v1,v2))
    }
    else
      return(subset(varNames, (Freq> 1))$Var1)
  }

  output$selVariable2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    selectInput('replyVariable2', 'Select variable:', varList(F), selected = varList(F)[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selGraph2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)
      
    options <- c("Dot plot","Bar chart")
    selectInput('replyGraph2', 'Select graph type:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selScale2 <- renderUI(
  {
    req(input$replyVariable2)
    
    spectral <- c("spectral_peak", "center_of_gravity", "standard_deviation")
    
    if (is.element(input$replyVariable2, spectral))
      options <- optionsScale()
    else
      options <- c("None" = " none")

    selectInput('replyScale2', 'Scale:', options, selected = options[1], selectize=FALSE, multiple=FALSE)
  })
  
  output$selRef2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)
      
    if ((length(input$replyScale2)>0) && (input$replyScale2==" ST"))
      numericInput('replyRef2', 'Reference frequency:', value=50, min=1, step=1, width = "100%")
    else
      return(NULL)
  })
  
  output$selNormal2 <- renderUI(
  {
    selectInput('replyNormal2', 'Normalization:', optionsNormal(), selected = optionsNormal()[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selXaxis2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if (is.element("<vowel", colnames(consonantTab())))
      options <- c(options, "<vowel")
    
    if (is.element(">vowel", colnames(consonantTab())))
      options <- c(options, ">vowel")
    
    selectInput('replyXaxis2', 'Variable x-axis:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$catXaxis2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if (length(input$replyXaxis2)>0)
      options <- unique(fuseCols(consonantTab(),input$replyXaxis2))
    else
      options <- NULL

    selectInput('catXaxis2', 'Sel. categories:', options, multiple=TRUE, selectize = FALSE, width="100%")
  })

  output$selLine2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if (is.element("<vowel", colnames(consonantTab())))
      options <- c(options, "<vowel")
    
    if (is.element(">vowel", colnames(consonantTab())))
      options <- c(options, ">vowel")

    selectInput('replyLine2', 'Color variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$catLine2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if (length(input$replyLine2)>0)
      options <- unique(fuseCols(consonantTab(),input$replyLine2))
    else
      options <- NULL

    selectInput('catLine2', 'Select colors:', options, multiple=TRUE, selectize = FALSE, width="100%")
  })

  output$selPlot2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if (is.element("<vowel", colnames(consonantTab())))
      options <- c(options, "<vowel")
    
    if (is.element(">vowel", colnames(consonantTab())))
      options <- c(options, ">vowel")

    selectInput('replyPlot2', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$catPlot2 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if (length(input$replyPlot2)>0)
      options <- unique(fuseCols(consonantTab(),input$replyPlot2))
    else
      options <- NULL

    selectInput('catPlot2', 'Select panels:', options, multiple=TRUE, selectize = FALSE, width="100%")
  })

  scaleLab <- function(replyScale)
  {
    if (replyScale==" none")
      return("Hz")
    
    if (replyScale==" Hz")
      return("Hz")
    
    if (replyScale==" bark I")
      return("bark")
    
    if (replyScale==" bark II")
      return("bark")
    
    if (replyScale==" bark III")
      return("bark")
    
    if (replyScale==" ERB I")
      return("ERB")
    
    if (replyScale==" ERB II")
      return("ERB")
    
    if (replyScale==" ERB III")
      return("ERB")
    
    if (replyScale==" ln")
      return("ln")
    
    if (replyScale==" mel I")
      return("mel")
    
    if (replyScale==" mel II")
      return("mel")
    
    if (replyScale==" ST")
      return("ST")
  }
  
  axisLab <- function(axis,replyScale,replyNorm)
  {
    axis <- tolower(axis)

    if  (replyScale!=" none")
      Scale <- scaleLab(replyScale)
    else

    if  (grepl("durat", axis))
      Scale <- "ms"
    else

    if  (grepl("^ampl", axis))
      Scale <- "μPa"
    else

    if ((grepl("^inte", axis)) | (grepl("_inte", axis)))
      Scale <- "dB"
    else
      Scale <- ""

    if ((Scale!="") & (replyNorm==""))
      return(paste0(" (", Scale, ")"))
    else

    if ((Scale!="") & (replyNorm!=""))
      return(paste0(" (", Scale, " ", replyNorm, ")"))
    else

    if ((Scale=="") & (replyNorm!=""))
      return(paste0(" (", replyNorm, ")"))
    else
      return("")
  }
  
  plotGraph2 <- function()
  {
    if (is.null(consonantSub2()) || (nrow(consonantSub2())==0))
      return(NULL)

    if (input$selError2=="0%")
      w <- 0
    if (input$selError2=="90%")
      w <- 0.4
    if (input$selError2=="95%")
      w <- 0.4
    if (input$selError2=="99%")
      w <- 0.4

    if (is.element("rotate x-axis labels",input$selGeon2))
      Angle = 90
    else
      Angle = 0
    
    varName <- gsub("_", " ", input$replyVariable2)
    varName <- gsub("<", "" , varName)
    varName <- gsub(">", "" , varName)
    
    if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)==0))
    {
      if (input$replyGraph2=="Dot plot")
      {
        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,2], group=1)) +
              geom_point(colour="indianred2", size=3) +
              geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    aspect.ratio   =0.67)
      }
      else

      if (input$replyGraph2=="Bar chart")
      {
        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,2])) +
              geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    aspect.ratio   =0.67)
      }
    }
    else

    if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)>0))
    {
      if (input$replyGraph2=="Dot plot")
      {
        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,3], group=1)) +
              geom_point(colour="indianred2", size=3) +
              geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              facet_wrap(~consonantSub2()[,2]) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    aspect.ratio   =0.67)
      }

      else

      if (input$replyGraph2=="Bar chart")
      {
        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,3])) +
              geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              facet_wrap(~consonantSub2()[,2]) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    aspect.ratio   =0.67)
      }
      else {}
    }
    else

    if (((length(input$catLine2)>0) & (length(input$catLine2)<=14)) && (length(input$catPlot2)==0))
    {
      if (input$replyGraph2=="Dot plot")
      {
        pd <- position_dodge(0.7)

        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,3], group=consonantSub2()[,2], color=consonantSub2()[,2])) +
              geom_point(size=3, position=pd) +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              scale_colour_discrete(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    legend.key.size=unit(1.5, 'points'),
                    aspect.ratio   =0.67)
      }
      else

      if (input$replyGraph2=="Bar chart")
      {
        pd <- position_dodge(0.9)

        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,3], fill=consonantSub2()[,2])) +
              geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              scale_fill_hue(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    legend.key.size=unit(1.5, 'lines'),
                    aspect.ratio   =0.67)
      }
      else {}

      if (input$replyXaxis2==input$replyLine2)
        gp <- gp + theme(axis.title.x=element_blank(),
                         axis.text.x =element_blank(),
                         axis.ticks.x=element_blank())
      else {}
    }
    else

    if (((length(input$catLine2)>0) & (length(input$catLine2)<=14))  && (length(input$catPlot2)>0))
    {
      if (input$replyGraph2=="Dot plot")
      {
        pd <- position_dodge(0.5)

        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,4], group=consonantSub2()[,2], color=consonantSub2()[,2])) +
              geom_point(size=3, position=pd) +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              scale_colour_discrete(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
              facet_wrap(~consonantSub2()[,3]) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    legend.key.size=unit(1.5, 'points'),
                    aspect.ratio   =0.67)
      }
      else

      if (input$replyGraph2=="Bar chart")
      {
        pd <- position_dodge(0.9)

        gp <- ggplot(data=consonantSub2(), aes(x=consonantSub2()[,1], y=consonantSub2()[,4], fill=consonantSub2()[,2])) +
              geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
              geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
              ggtitle(input$title2) +
              xlab(paste(input$replyXaxis2, collapse = " ")) + ylab(paste0(varName, axisLab(input$replyVariable2,input$replyScale2,input$replyNormal2))) +
              scale_fill_hue(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
              facet_wrap(~consonantSub2()[,3]) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                    axis.text.x    =element_text(angle=Angle),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    legend.key.size=unit(1.5, 'lines'),
                    aspect.ratio   =0.67)
      }
      else {}

      if (input$replyXaxis2==input$replyLine2)
        gp <- gp + theme(axis.title.x=element_blank(),
                         axis.text.x =element_blank(),
                         axis.ticks.x=element_blank())
      else {}
    }
    else {}

    return(graphics::plot(gp))
  }

  res2 <- function()
  {
    if (length(input$replySize2b)==0)
      return( 72)
    
    if (input$replySize2b=="tiny"  )
      return( 54)
    if (input$replySize2b=="small" )
      return( 72)
    if (input$replySize2b=="normal")
      return( 90)
    if (input$replySize2b=="large" )
      return(108)
    if (input$replySize2b=="huge"  )
      return(144)
  }

  observeEvent(input$replySize2b,
  {
    output$graph2 <- renderPlot(height = 550, width = 700, res = res2(),
    {
      if (length(input$catXaxis2)>0)
      {
        plotGraph2()
      }
    })
  })

  output$Graph2 <- renderUI(
  {
    plotOutput("graph2", height="627px")
  })

  output$selFormat2a <- renderUI(
  {
    options <- c("txt","xlsx")
    selectInput('replyFormat2a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  fileName2a <- function()
  {
    return(paste0("1DTable.",input$replyFormat2a))
  }

  output$download2a <- downloadHandler(filename = fileName2a, content = function(file)
  {
    if (length(input$catXaxis2)>0)
    {
      vT <- consonantSub2()

      colnames(vT)[which(colnames(vT)=="sd")] <- "standard deviation"
      colnames(vT)[which(colnames(vT)=="se")] <- "standard error"
      colnames(vT)[which(colnames(vT)=="n" )] <- "number of observations"
      colnames(vT)[which(colnames(vT)=="ll")] <- "lower limit"
      colnames(vT)[which(colnames(vT)=="ul")] <- "upper limit"
    }
    else
      vT <- data.frame()

    if (input$replyFormat2a=="txt")
    {
      utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    }
    else

    if (input$replyFormat2a=="xlsx")
    {
      WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
    }
    else {}
  })

  output$selSize2b <- renderUI(
  {
    options <- c("tiny", "small", "normal", "large", "huge")
    selectInput('replySize2b', label=NULL, options, selected = options[3], selectize=FALSE, multiple=FALSE)
  })

  output$selFont2b <- renderUI(
  {
    options <- c("FreeMono", "Latin Modern Mono", "FreeSans", "Latin Modern Sans", "TeX Gyre Bonum", "TeX Gyre Schola", "Latin Modern Roman", "TeX Gyre Pagella", "FreeSerif")
    selectInput('replyFont2b', label=NULL, options, selected = "FreeSans", selectize=FALSE, multiple=FALSE)
  })

  output$selPoint2b <- renderUI(
  {
    options <- c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48)
    selectInput('replyPoint2b', label=NULL, options, selected = 18, selectize=FALSE, multiple=FALSE)
  })

  output$selFormat2b <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat2b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  fileName2b <- function()
  {
    return(paste0("1DPlot.",input$replyFormat2b))
  }

  output$download2b <- downloadHandler(filename = fileName2b, content = function(file)
  {
    grDevices::pdf(NULL)

    scale  <- 72/res2()
    width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
    
    if ((length(input$catXaxis2)>0) && (nrow(consonantSub2())>0))
      plot <- plotGraph2()
    else
      plot <- ggplot()+theme_bw()

    show_modal_spinner()
    
    if (input$replyFormat2b=="JPG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
    else
    if (input$replyFormat2b=="PNG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
    else
    if (input$replyFormat2b=="SVG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
    else
    if (input$replyFormat2b=="EPS")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
    else
    if (input$replyFormat2b=="PDF")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
    else    
    if (input$replyFormat2b=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}

    grDevices::graphics.off()
    
    remove_modal_spinner()
  })

  ##############################################################################

  consonantScale3 <- reactive(
  {
    if ((length(input$replyRefX3)==0) || is.na(input$replyRefX3))
      RefX <- 50
    else
      RefX <- input$replyRefX3

    if ((length(input$replyRefY3)==0) || is.na(input$replyRefY3))
      RefY <- 50
    else
      RefY <- input$replyRefY3

    vT <- consonantTab()
    vT <- consonantScale(vT, input$replyScaleX3, RefX, input$axisX)
    vT <- consonantScale(vT, input$replyScaleY3, RefY, input$axisY)

    return(vT)
  })

  consonantNorm3 <- reactive(
  {
    if ((length(input$replyNormalX3)==0) || (length(input$replyNormalY3)==0))
      return(NULL)

    if (!all(c(input$axisX,input$axisY) %in% colnames(consonantScale3())))
      return(NULL)
    
    vT  <- consonantScale3()
    vT  <- consonantNormD(vT, input$replyNormalX3, input$axisX)
    vT  <- consonantNormD(vT, input$replyNormalY3, input$axisY)

    return(vT)
  })

  consonantSub3 <- reactive(
  {
    if ((is.null(consonantNorm3())) || (nrow(consonantNorm3())==0))
      return(NULL)

    vT <- consonantNorm3()

    vT$indexColor <- fuseCols(consonantNorm3(),input$replyColor3)
    vT$indexShape <- fuseCols(consonantNorm3(),input$replyShape3)
    vT$indexPlot  <- fuseCols(consonantNorm3(),input$replyPlot3)

    indexConsonant <- grep("^consonant$", colnames(consonantNorm3()))

    ### check begin

    if (length(vT$indexColor)==0)
      return(NULL)
    else

    if (length(vT$indexShape)==0)
      return(NULL)
    else

    if (length(vT$indexPlot)==0)
      return(NULL)
    else {}

    ### check end

    if (length(input$catColor3)>0)
    {
      vT1 <- data.frame()

      for (q in (1:length(input$catColor3)))
      {
        vT1 <- rbind(vT1, subset(vT, indexColor==input$catColor3[q]))
      }
    }
    else
    {
      vT1 <- vT
    }

    if (length(input$catShape3)>0)
    {
      vT2 <- data.frame()

      for (q in (1:length(input$catShape3)))
      {
        vT2 <- rbind(vT2, subset(vT1, indexShape==input$catShape3[q]))
      }
    }
    else
    {
      vT2 <- vT1
    }

    if (length(input$catPlot3)>0)
    {
      vT3 <- data.frame()

      for (q in (1:length(input$catPlot3)))
      {
        vT3 <- rbind(vT3, subset(vT2, indexPlot==input$catPlot3[q]))
      }
    }
    else
    {
      vT3 <- vT2
    }

    vT <- vT3

    ###

    if (nrow(vT)>0)
    {
      vT0 <- data.frame()

      indexVar1 <- grep(paste0("^", input$axisX, "$"), colnames(consonantNorm3()))
      indexVar2 <- grep(paste0("^", input$axisY, "$"), colnames(consonantNorm3()))

      if (length(input$catColor3)>0)
        Color <- vT$indexColor
      else
        Color <- rep("none",nrow(vT))

      if (length(input$catShape3)>0)
        Shape <- vT$indexShape
      else
        Shape <- rep("none",nrow(vT))

      if (length(input$catPlot3)>0)
        Plot  <- vT$indexPlot
      else
        Plot  <- rep("none",nrow(vT))

      vT <- subset(vT, !is.na(vT[,indexVar1]))
      vT <- subset(vT, !is.na(vT[,indexVar2]))
      
      Xaxis <- vT[,indexVar1]
      Yaxis <- vT[,indexVar2]

      vT0 <- rbind(vT0, data.frame(speaker   = vT$speaker  ,
                                   consonant = vT$consonant,
                                   color     = Color       ,
                                   shape     = Shape       ,
                                   plot      = Plot        ,
                                   index     = rownames(vT),
                                   X         = Xaxis       ,
                                   Y         = Yaxis       ))

      if (input$average3)
        vT0 <- aggregate(cbind(X,Y)~speaker+consonant+color+shape+plot, data=vT0, FUN=mean)

      vT0$speaker <- NULL

      vT <- vT0
    }
    else {}

    ###

    if ((nrow(vT)>0) & (input$average3))
    {
      vT <- aggregate(cbind(X,Y)~consonant+color+shape+plot, data=vT, FUN=mean)

      no <- nrow(aggregate(cbind(X,Y)~consonant+color+shape+plot, data=vT, FUN=mean))
      vT$index <- seq(1:no)
    }

    ###

    if (nrow(vT)>0)
    {
      vT$consonant <- factor(vT$consonant)
      vT$color     <- factor(vT$color)
      vT$shape     <- factor(vT$shape)
      vT$plot      <- factor(vT$plot)
      vT$index     <- factor(vT$index)

    # utils::write.table(vT, "vT.csv", sep = "\t", row.names = FALSE)
      return(vT)
    }
    else
    {
      return(data.frame())
    }
  })

  output$selScaleX3 <- renderUI(
  {
    req(input$axisX)
    
    spectral <- c("spectral_peak", "center_of_gravity", "standard_deviation")
    
    if (is.element(input$axisX, spectral))
      options <- optionsScale()
    else
      options <- c("None" = " none")
    
    selectInput('replyScaleX3', 'Scale x-axis:', options, selected = options[1], selectize=FALSE, multiple=FALSE)
  })

  output$selScaleY3 <- renderUI(
  {
    req(input$axisY)
      
    spectral <- c("spectral_peak", "center_of_gravity", "standard_deviation")
      
    if (is.element(input$axisY, spectral))
      options <- optionsScale()
    else
      options <- c("None" = " none")
      
    selectInput('replyScaleY3', 'Scale y-axis:', options, selected = options[1], selectize=FALSE, multiple=FALSE)
  })
  
  output$selRef3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)
    
    if (((length(input$replyScaleX3)>0) && (input$replyScaleX3==" ST")) &
        ((length(input$replyScaleY3)>0) && (input$replyScaleY3==" ST")))
    {
      splitLayout(
        cellWidths = c("50%", "49%"),
        numericInput('replyRefX3', 'Reference frequency:', value=50, min=1, step=1, width = "100%"),
        numericInput('replyRefY3', 'Reference frequency:', value=50, min=1, step=1, width = "100%")
      )
    }
    else
      
    if ((length(input$replyScaleX3)>0) && (input$replyScaleX3==" ST"))
      numericInput('replyRefX3', 'Reference frequency:', value=50, min=1, step=1, width = "100%")
    else  
      
    if ((length(input$replyScaleY3)>0) && (input$replyScaleY3==" ST"))
      numericInput('replyRefY3', 'Reference frequency:', value=50, min=1, step=1, width = "100%")
    else
      return(NULL) 
  })

  output$selNormalX3 <- renderUI(
  {
    if (is.null(consonantTab()) || length(input$replyScaleX3)==0)
      return(NULL)

    selectInput('replyNormalX3', 'Norm. x-axis:', optionsNormal(), selected = optionsNormal()[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selNormalY3 <- renderUI(
  {
    if (is.null(consonantTab()) || length(input$replyScaleY3)==0)
      return(NULL)
      
    selectInput('replyNormalY3', 'Norm. y-axis:', optionsNormal(), selected = optionsNormal()[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selAxisX3 <- renderUI(
  {
    req(consonantTab())

    index <- grep("gravity", tolower(varList(F)))
    
    if (length(index) == 0)
    {
      if (length(varList(F)) >= 1)
        index <- 1
      else
        return(NULL)
    }
    
    selectInput('axisX', "x-axis", choices=varList(F), selected=varList(F)[index], selectize=FALSE, width = "100%")
  })
  
  output$selAxisY3 <- renderUI(
  {
    req(consonantTab())

    index <- grep("deviation", tolower(varList(F)))
    
    if (length(index) == 0)
    {
      if (length(varList(F)) >= 2)
        index <- 2
      else
        
      if (length(varList(F)) >= 1)
        index <- 1
      else
        return(NULL)          
    }

    selectInput('axisY', "y-axis", choices=varList(F), selected=varList(F)[index], selectize=FALSE, width = "100%")
  })

  output$selVar1min <- renderUI(
  {
    if ((length(input$selManual)>0) && (input$selManual==TRUE))
    {
      numericInput('replyXmin', 'min. x', value=NULL, step=10, width = "100%")
    }
  })

  output$selVar1max <- renderUI(
  {
    if ((length(input$selManual)>0) && (input$selManual==TRUE))
    {
      numericInput('replyXmax', 'max. x', value=NULL, step=10, width = "100%")
    }
  })

  output$selVar2min <- renderUI(
  {
    if ((length(input$selManual)>0) && (input$selManual==TRUE))
    {
      numericInput('replyYmin', 'min. y', value=NULL, step=10, width = "100%")
    }
  })

  output$selVar2max <- renderUI(
  {
    if ((length(input$selManual)>0) && (input$selManual==TRUE))
    {
      numericInput('replyYmax', 'max. y', value=NULL, step=10, width = "100%")
    }
  })

  output$selColor3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if (is.element("<vowel", colnames(consonantTab())))
      options <- c(options, "<vowel")
    
    if (is.element(">vowel", colnames(consonantTab())))
      options <- c(options, ">vowel")

    selectInput('replyColor3', 'Color variable:', options, selected=options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
  })

  output$catColor3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if (length(input$replyColor3)>0)
      options <- unique(fuseCols(consonantTab(),input$replyColor3))
    else
      options <- NULL

    selectInput('catColor3', 'Select colors:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
  })

  output$selShape3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)
    
    if (((length(input$geon2)>0) && input$geon2) |
        ((length(input$geon3)>0) && input$geon3) |
        ((length(input$geon4)>0) && input$geon4) |
        ((length(input$geon5)>0) && input$geon5))
      options <- c()
    else
    {
      indexConsonant <- grep("^consonant$", colnames(consonantTab()))
      options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))
      
      if (is.element("<vowel", colnames(consonantTab())))
        options <- c(options, "<vowel")
    
      if (is.element(">vowel", colnames(consonantTab())))
        options <- c(options, ">vowel")
    }

    selectInput('replyShape3', 'Shape variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
  })

  output$catShape3 <- renderUI(
  {
    if  (is.null(consonantTab()))
      return(NULL)

    if (length(input$replyShape3)>0)
    {
      if (input$geon2 | input$geon3 | input$geon4 | input$geon5)
        options <- NULL
      else
        options <- unique(fuseCols(consonantTab(),input$replyShape3))
    }
    else
      options <- NULL

    selectInput('catShape3', 'Select shapes:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
  })

  output$selPlot3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if (is.element("<vowel", colnames(consonantTab())))
      options <- c(options, "<vowel")
    
    if (is.element(">vowel", colnames(consonantTab())))
      options <- c(options, ">vowel")

    selectInput('replyPlot3', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
  })

  output$catPlot3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if (length(input$replyPlot3)>0)
      options <- unique(fuseCols(consonantTab(),input$replyPlot3))
    else
      options <- NULL

    selectInput('catPlot3', 'Select panels:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
  })

  output$selGeon3 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    tagList(splitLayout
    (
      cellWidths = c("19%", "17%", "15%", "21%", "19%"),

      checkboxInput("geon1", "labels" , value = FALSE),
      checkboxInput("geon2", "cent."  , value = FALSE),
      checkboxInput("geon3", "hull"   , value = FALSE),
      checkboxInput("geon4", "spokes" , value = FALSE),
      checkboxInput("geon5", "ellipse", value = FALSE)
    ))
  })

  output$selPars <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if ((length(input$geon5)>0) && input$geon5)
      numericInput('replyLevel', 'Confidence level:', value=0.95, step=0.01, width = "100%")
    else
      return(NULL)
  })

  numColor <- function()
  {
    if ((length(input$replyColor3)>0) && (length(input$catColor3)>0))
      return(length(input$catColor3))
    else
      return(0)
  }

  numShape <- function()
  {
    if ((length(input$replyShape3)>0) && (length(input$catShape3)>0))
      return(length(input$catShape3))
    else
      return(0)
  }

  numAll <- function()
  {
    return(numColor()+numShape())
  }

  colPalette <- function(n,grayscale)
  {
    if (!grayscale)
    {
      labColors  <- c("#c87e66","#b58437","#988a00","#709000","#27942e","#00965c","#009482","#008ea3","#0081bd","#386acc","#8d46c8","#b315b1","#bd0088","#b61a51")
      labPalette <- grDevices::colorRampPalette(labColors, space = "Lab")

      if (n==1)
        return(labColors[c(10)])

      if (n==2)
        return(labColors[c(8,14)])

      if (n==3)
        return(labColors[c(5,10,13)])

      if (n==4)
        return(labColors[c(3,7,11,14)])

      if (n==5)
        return(labColors[c(3,5,9,11,14)])

      if (n==6)
        return(labColors[c(2,5,7,10,12,14)])

      if (n==7)
        return(labColors[c(2,4,5,8,10,12,14)])

      if (n==8)
        return(labColors[c(1,3,5,7,9,11,12,14)])

      if (n==9)
        return(labColors[c(1,3,4,6,8,10,11,12,14)])

      if (n==10)
        return(labColors[c(1,3,4,5,7,9,10,11,12,14)])

      if (n==11)
        return(labColors[c(1,2,4,5,6,7,9,10,11,12,14)])

      if (n==12)
        return(labColors[c(1,2,3,4,5,7,8,9,11,12,13,14)])

      if (n==13)
        return(labColors[c(1,2,3,4,5,6,7,8,10,11,12,13,14)])

      if (n==14)
        return(labColors[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)])

      if (n>=15)
        return(labPalette(n))
    }
    else
    {
      return(grDevices::gray(0:(n-1)/n))
    }
  }

  colPalette3 <- function(n)
  {
    return(colPalette(n,input$grayscale3))
  }

  shpPalette <- function()
  {
    return(c(19,1,17,2,15,0,18,5,3,4,8))
  }

  plotGraph3 <- function()
  {
    if (is.null(consonantSub3()) || (nrow(consonantSub3())==0))
      return(NULL)

    Xaxis <- gsub("_", " ", input$axisX)
    Xaxis <- gsub("<", "" , Xaxis)
    Xaxis <- gsub(">", "" , Xaxis)

    Yaxis <- gsub("_", " ", input$axisY)
    Yaxis <- gsub(">", "" , Yaxis)
    Yaxis <- gsub("<", "" , Yaxis)

    if ((!(input$geon2 | input$geon3 | input$geon4 | input$geon5)))
    {
      vT <- consonantSub3()

      if ((numColor()>0) & (numShape()>0) & (numShape()<=11))
      {
        Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color, shape=shape)) +
          scale_shape_manual(values=shpPalette())
        
        if (input$geon1)
          Basis <- Basis + geom_point(size=2.5) + geom_text_repel(position="identity", aes(label=consonant), hjust=0.5, vjust=0.5, family=input$replyFont3b, size=5, alpha=1.0, max.overlaps=100)
        else        
          Basis <- Basis + geom_point(size=2.5)
      }
      else
        
      if  (numColor()>0)
      {
        Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color))
        
        if (input$geon1)
          Basis <- Basis + geom_text(position="identity", aes(label=consonant), hjust=0.5, vjust=0.5, family=input$replyFont3b, size=5, alpha=1.0)
        else        
          Basis <- Basis + geom_point(size=2.5)
      }
      else
        
      if ((numShape()>0) & (numShape()<=11))
      {
        Basis <- ggplot(data=vT, aes(x=X, y=Y, shape=shape)) +
          scale_shape_manual(values=shpPalette())

        if (input$geon1)
          Basis <- Basis + geom_point(size=2.5, colour=colPalette3(1)) + geom_text_repel(position="identity", aes(label=consonant), hjust=0.5, vjust=0.5, family=input$replyFont3b, size=5, alpha=1.0, max.overlaps=100)
        else        
          Basis <- Basis + geom_point(size=2.5, colour=colPalette3(1))
      }
      else
      {
        Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color))
        
        if (input$geon1)
          Basis <- Basis + geom_text(position="identity", aes(label=consonant), hjust=0.5, vjust=0.5, family=input$replyFont3b, size=5, alpha=1.0)
        else        
          Basis <- Basis + geom_point(size=2.5)
      }

      if (!input$geon1)
        Basis <- Basis + labs(colour=paste(input$replyColor3, collapse = " "), shape=paste(input$replyShape3, collapse = " "))
      else
        Basis <- Basis + guides(colour="none") + labs(shape=paste(input$replyShape3, collapse = " "))

      if ((length(input$selManual)>0) && (input$selManual==TRUE))
      {
        scaleX <- scale_x_continuous(name=paste0(Xaxis, axisLab(input$axisX,input$replyScaleX3,input$replyNormalX3)), position="bottom", limits = c(input$replyXmin, input$replyXmax))
        scaleY <- scale_y_continuous(name=paste0(Yaxis, axisLab(input$axisY,input$replyScaleY3,input$replyNormalY3)), position="left"  , limits = c(input$replyYmin, input$replyYmax))
      }
      else
      {
        scaleX <- scale_x_continuous(name=paste0(Xaxis, axisLab(input$axisX,input$replyScaleX3,input$replyNormalX3)), position="bottom")
        scaleY <- scale_y_continuous(name=paste0(Yaxis, axisLab(input$axisY,input$replyScaleY3,input$replyNormalY3)), position="left"  )
      }

      if (length(input$catPlot3)>0)
      {
        Title <- ggtitle(input$title3)
        Facet <- facet_wrap(~plot)
      }
      else
      {
        Title <- ggtitle(input$title3)
        Facet <- facet_null()
      }

      if ((numAll()>0) & (numAll()<=18))
        Legend <- theme(legend.position="right")
      else
      if ((numColor()>0) & (numColor()<=18))
        Legend <- guides(shape="none")
      else
      if ((numShape()>0) & (numShape()<=11))
        Legend <- guides(color="none")
      else
        Legend <- theme(legend.position="none")

      graphics::plot(Basis + scaleX + scaleY + Title + Facet +
                     scale_color_manual(values=colPalette3(length(unique(vT$color)))) +
                     theme_bw() +
                     theme(text           =element_text(size=as.numeric(input$replyPoint3b), family=input$replyFont3b),
                           plot.title     =element_text(face="bold", hjust = 0.5),
                           legend.key.size=unit(1.5,'lines'),
                           aspect.ratio   =1) +
                     Legend)
    }
    else

    if ((input$geon2 | input$geon3 | input$geon4 | input$geon5))
    {
      vT <- consonantSub3()

      centers <- aggregate(cbind(X,Y)~color+plot, data=vT, FUN=mean)

      vT <- vT[order(vT$plot, vT$index),]
      vT$index <- paste0(vT$time,vT$index)

      Basis <- ggplot(data = vT, aes(x=X, y=Y, fill=color, color=color))
      Fill  <- geom_blank()

      if (input$geon1)
        Points <- geom_text(position="identity", aes(label=consonant), hjust=0.5, vjust=0.5, family=input$replyFont3b, size=5, alpha=0.3)
      else
        Points <- geom_blank()

      if (input$geon2)
      {
        if (input$geon4)
          Centers <- geom_text(data=centers, position="identity", aes(label=color), hjust=0.5, vjust=0.5, family=input$replyFont3b, size= 7, alpha=1.0, color="black")
        else
          Centers <- geom_text(data=centers, position="identity", aes(label=color), hjust=0.5, vjust=0.5, family=input$replyFont3b, size=10, alpha=1.0)

        Legend <- theme(legend.position="none")
      }
      else
      {
        Centers <- geom_blank()
        Legend <- theme(legend.position="right")
      }

      if (input$geon3)
      {
        chulls <- ddply(vT, .(color,plot), function(df) df[grDevices::chull(df$X, df$Y), ])

        if ((length(unique(vT$color))==1) | (as.character(input$replyColor3)[1]=="consonant"))
        {
          Hull <- geom_polygon(data=chulls, aes(x=X, y=Y, group=color, fill=color), alpha=0.1)
          Fill <- scale_fill_manual(values=colPalette3(length(unique(vT$color))))
        }
        else

        if (length(unique(vT$color))> 1)
        {
          Hull <- geom_polygon(data=chulls, aes(x=X, y=Y, group=color, fill=color), alpha=0  )
          Fill <- scale_fill_manual(values=rep("white", length(unique(vT$color))))
        }
        else {}
      }
      else
      {
        Hull <- geom_blank()
      }

      if (input$geon4)
      {
        vT0 <- vT
        for (i in (1:nrow(vT0)))
        {
          centersSub <- subset(centers, (centers$color==vT0$color[i]) & (centers$plot==vT0$plot[i]))

          vT0$X[i] <- centersSub$X
          vT0$Y[i] <- centersSub$Y
        }

        vT0 <- rbind(vT,vT0)
        vT0 <- vT0[order(vT0$plot, vT0$index),]

        Spokes <- geom_path(data=vT0, aes(group = index), arrow = arrow(ends = "last", length = unit(0, "inches")), size=1.0, alpha=0.3)
      }
      else
      {
        Spokes <- geom_blank()
      }

      if ((input$geon5) & (length(input$replyLevel)>0))
      {
        if ((input$geon1) | (input$geon3))
          Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel)
        else
        {
          if ((length(unique(vT$color))==1) | (as.character(input$replyColor3)[1]=="consonant"))
          {
            Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel, geom="polygon", alpha=0.3)
            Fill <- scale_fill_manual(values=colPalette3(length(unique(vT$color))))
          }
          else

          if (length(unique(vT$color))> 1)
          {
            Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel, geom="polygon", alpha=0  )
            Fill <- scale_fill_manual(values=rep("white", length(unique(vT$color))))
          }
          else {}
        }
      }
      else
      {
        Ellipse <- geom_blank()
      }
      
      if ((length(input$selManual)>0) && (input$selManual==TRUE))
      {
        scaleX <- scale_x_continuous(name=paste0(Xaxis, axisLab(input$axisX,input$replyScaleX3,input$replyNormalX3)), position="bottom", limits = c(input$replyXmin, input$replyXmax))
        scaleY <- scale_y_continuous(name=paste0(Yaxis, axisLab(input$axisY,input$replyScaleY3,input$replyNormalY3)), position="left"  , limits = c(input$replyYmin, input$replyYmax))
      }
      else
      {
        scaleX <- scale_x_continuous(name=paste0(Xaxis, axisLab(input$axisX,input$replyScaleX3,input$replyNormalX3)), position="bottom")
        scaleY <- scale_y_continuous(name=paste0(Yaxis, axisLab(input$axisY,input$replyScaleY3,input$replyNormalY3)), position="left"  )
      }

      if (length(input$catPlot3)>0)
      {
        Title <- ggtitle(input$title3)
        Facet <- facet_wrap(~plot)
      }
      else
      {
        Title <- ggtitle(input$title3)
        Facet <- facet_null()
      }

      if ((numColor()==0) | (numColor()>18))
      {
        Legend <- theme(legend.position="none")
      }

      graphics::plot(Basis + Points + Hull + Spokes + Ellipse + Centers + scaleX + scaleY + Title + Facet +
                     scale_color_manual(values=colPalette3(length(unique(vT$color)))) + Fill +
                     labs(colour=paste(input$replyColor3, collapse = " "), fill=paste(input$replyColor3, collapse = " ")) +
                     theme_bw() +
                     theme(text           =element_text(size=as.numeric(input$replyPoint3b), family=input$replyFont3b),
                           plot.title     =element_text(face="bold", hjust = 0.5),
                           legend.key.size=unit(1.5,'lines'),
                           aspect.ratio   =1) +
                     Legend)
    }
    else {}
  }

  res3 <- function()
  {
    if (length(input$replySize3b)==0)
      return( 72)
    
    if (input$replySize3b=="tiny"  )
      return( 54)
    if (input$replySize3b=="small" )
      return( 72)
    if (input$replySize3b=="normal")
      return( 90)
    if (input$replySize3b=="large" )
      return(108)
    if (input$replySize3b=="huge"  )
      return(144)
  }

  observeEvent(input$replySize3b,
  {
    output$graph3 <- renderPlot(height = 550, width = 700, res = res3(),
    {
      plotGraph3()
    })
  })

  output$Graph3 <- renderUI(
  {
    plotOutput("graph3", height="627px")
  })

  output$selFormat3a <- renderUI(
  {
    options <- c("txt","xlsx")
    selectInput('replyFormat3a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  fileName3a <- function()
  {
    return(paste0("2DTable.",input$replyFormat3a))
  }

  output$download3a <- downloadHandler(filename = fileName3a, content = function(file)
  {
    vT <- consonantSub3()

    colnames(vT)[which(colnames(vT)=="X")] <- input$axisX
    colnames(vT)[which(colnames(vT)=="Y")] <- input$axisY

    if (input$replyFormat3a=="txt")
    {
      utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    }
    else

    if (input$replyFormat3a=="xlsx")
    {
      WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
    }
    else {}
  })

  output$selSize3b <- renderUI(
  {
    options <- c("tiny", "small", "normal", "large", "huge")
    selectInput('replySize3b', label=NULL, options, selected = options[3], selectize=FALSE, multiple=FALSE)
  })

  output$selFont3b <- renderUI(
  {
    options <- c("FreeMono", "Latin Modern Mono", "FreeSans", "Latin Modern Sans", "TeX Gyre Bonum", "TeX Gyre Schola", "Latin Modern Roman", "TeX Gyre Pagella", "FreeSerif")
    selectInput('replyFont3b', label=NULL, options, selected = "FreeSans", selectize=FALSE, multiple=FALSE)
  })

  output$selPoint3b <- renderUI(
  {
    options <- c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48)
    selectInput('replyPoint3b', label=NULL, options, selected = 18, selectize=FALSE, multiple=FALSE)
  })

  output$selFormat3b <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat3b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  fileName3b <- function()
  {
    return(paste0("2DPlot.",input$replyFormat3b))
  }

  output$download3b <- downloadHandler(filename = fileName3b, content = function(file)
  {
    grDevices::pdf(NULL)

    scale  <- 72/res1()
    width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
    
    if (nrow(consonantSub3())>0)
      plot <- plotGraph3()
    else
      plot <- ggplot()+theme_bw()

    show_modal_spinner()
    
    if (input$replyFormat3b=="JPG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
    else
    if (input$replyFormat3b=="PNG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
    else
    if (input$replyFormat3b=="SVG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
    else
    if (input$replyFormat3b=="EPS")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
    else
    if (input$replyFormat3b=="PDF")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
    else    
    if (input$replyFormat3b=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}

    grDevices::graphics.off()
    
    remove_modal_spinner()
  })

  ##############################################################################

  vowelScale0 <- reactive(
  {
    if ((length(input$replyRef0)==0) || is.na(input$replyRef0))
      Ref <- 50
    else
      Ref <- input$replyRef0

    return(vowelScale(consonantTab(),input$replyScale0,Ref))
  })

  durationIndex <- reactive(
  {
    req(input$replyVowel0)
      
    if (input$replyVowel0=="preceding vowel")
      return(grep("^<duration", colnames(consonantTab())))
    if (input$replyVowel0=="consonant")
      return(grep("^duration", colnames(consonantTab())))
    if (input$replyVowel0=="following vowel")
      return(grep("^>duration", colnames(consonantTab())))
  })

  timeIndices <- reactive(
  {
    req(input$replyVowel0)
    
    if (input$replyVowel0=="preceding vowel")
      return(grep("^<time", colnames(consonantTab())))
    if (input$replyVowel0=="consonant")
      return(grep("^time", colnames(consonantTab())))
    if (input$replyVowel0=="following vowel")
      return(grep("^>time", colnames(consonantTab())))
  })

  getTimeCode <- reactive(
  {
    req(durationIndex())
    req(timeIndices  ())
    
    percentages <-FALSE

    if (sum(is.na(consonantTab()[,durationIndex()]))!=nrow(consonantTab()))
    {
      meanDuration <- mean(consonantTab()[,durationIndex()], na.rm = T)

      if (mean(consonantTab()[,timeIndices()[length(timeIndices())]], na.rm = T) <= meanDuration)
      {
        if (meanDuration==0)
        {
          meanDuration <- 0.000001
        }
        
        timeLabel <- c()
        timeCode  <- c()

        for (i in (1:length(timeIndices())))
        {
          timeLabel[i] <- (mean(consonantTab()[,timeIndices()[i]], na.rm = T)/meanDuration) * 100
          timeCode [i] <- i
        }

        names(timeCode) <- as.character(round(timeLabel))
        
        percentages <-TRUE
      }
    }

    if (percentages==FALSE)
    {
      timeCode <- seq(from=1, to=length(timeIndices()), by=1)
      names(timeCode) <- as.character(timeCode)
    }

    return(timeCode)
  })

  vowelSub0 <- reactive(
  {
    if (is.null(vowelScale0()) || (nrow(vowelScale0())==0) || (length(input$catXaxis0)==0))
      return(NULL)

    vT <- vowelScale0()

    vT$indexPlot <- fuseCols(vowelScale0(),input$replyPlot0)
    vT$indexLine <- fuseCols(vowelScale0(),input$replyLine0)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    indexVar       <- grep(paste0("^", tolower(input$replyVar0), "$"), tolower(colnames(consonantTab())))
    indexVar       <- indexVar[as.numeric(input$catXaxis0)]
    timeLabs       <- as.numeric(names(getTimeCode()))[as.numeric(input$catXaxis0)]
    
    if (input$selError0=="0%")
      z <- 0
    if (input$selError0=="90%")
      z <- 1.645
    if (input$selError0=="95%")
      z <- 1.96
    if (input$selError0=="99%")
      z <- 2.575

    if ((length(input$catPlot0)==0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
    {
      x <- c()
      y <- c()
      s <- c()
      v <- c()

      for (i in (1:length(indexVar)))
      {
        x <- as.numeric(as.character(c(x,rep(timeLabs[i],nrow(vT)))))
        y <- c(y,vT[,indexVar[i]])
        s <- c(s,as.character(vT$speaker))
        v <- c(v,as.character(vT$consonant))
      }

      vT0 <- data.frame(x,s,v,y)

      if (is.element("average",input$selGeon0))
      {
        vT0 <- aggregate(y~x+s+v, data=vT0, FUN=mean)
        vT0 <- aggregate(y~x+  v, data=vT0, FUN=mean)
      }

      if (length(which(is.na(vT0$y)))==nrow(vT0))
        return(data.frame())
      
      ag    <- aggregate(y~x, data=vT0, FUN=mean)
      ag$sd <- aggregate(y~x, data=vT0, FUN=sd)[,2]
      ag$n  <- aggregate(y~x, data=vT0, FUN=length)[,2]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure0=="SD")
      {
        ag$ll <- ag[,2] - z * ag$sd
        ag$ul <- ag[,2] + z * ag$sd
      }
      if (input$selMeasure0=="SE")
      {
        ag$ll <- ag[,2] - z * ag$se
        ag$ul <- ag[,2] + z * ag$se
      }

      colnames(ag)[1] <- "time"
      colnames(ag)[2] <- input$replyVar0

      return(ag)
    }
    else

    if ((length(input$catPlot0)>0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
    {
      vT <- subset(vT, is.element(vT$indexPlot,input$catPlot0))
      vT$indexPlot <- as.character(vT$indexPlot)

      if (nrow(vT)==0)
        return(data.frame())

      x <- c()
      y <- c()
      p <- c()
      s <- c()
      v <- c()

      for (i in (1:length(indexVar)))
      {
        x <- as.numeric(as.character(c(x,rep(timeLabs[i],nrow(vT)))))
        y <- c(y,vT[,indexVar[i]])
        p <- c(p,vT$indexPlot)
        s <- c(s,as.character(vT$speaker))
        v <- c(v,as.character(vT$consonant))
      }

      vT0 <- data.frame(x,p,s,v,y)

      if (is.element("average",input$selGeon0))
      {
        vT0 <- aggregate(y~x+p+s+v, data=vT0, FUN=mean)
        vT0 <- aggregate(y~x+p+  v, data=vT0, FUN=mean)
      }

      if (length(which(is.na(vT0$y)))==nrow(vT0))
        return(data.frame())
      
      ag    <- aggregate(y~x+p, data=vT0, FUN=mean)
      ag$sd <- aggregate(y~x+p, data=vT0, FUN=sd)[,3]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(y~x+p, data=vT0, FUN=length)[,3]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure0=="SD")
      {
        ag$ll <- ag[,3] - z * ag$sd
        ag$ul <- ag[,3] + z * ag$sd
      }
      if (input$selMeasure0=="SE")
      {
        ag$ll <- ag[,3] - z * ag$se
        ag$ul <- ag[,3] + z * ag$se
      }

      ag <- ag[order(ag[,2]),]

      colnames(ag)[1] <- "time"
      colnames(ag)[2] <- paste(input$replyPlot0, collapse = " ")
      colnames(ag)[3] <- input$replyVar0

      return(ag)
    }
    else

    if ((length(input$catPlot0)==0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
    {
      vT <- subset(vT, is.element(vT$indexLine,input$catLine0))
      vT$indexLine <- as.character(vT$indexLine)

      if (nrow(vT)==0)
        return(data.frame())

      x <- c()
      y <- c()
      l <- c()
      s <- c()
      v <- c()

      for (i in (1:length(indexVar)))
      {
        x <- as.numeric(as.character(c(x,rep(timeLabs[i],nrow(vT)))))
        y <- c(y,vT[,indexVar[i]])
        l <- c(l,vT$indexLine)
        s <- c(s,as.character(vT$speaker))
        v <- c(v,as.character(vT$consonant))
      }

      vT0 <- data.frame(x,l,s,v,y)

      if (is.element("average",input$selGeon0))
      {
        vT0 <- aggregate(y~x+l+s+v, data=vT0, FUN=mean)
        vT0 <- aggregate(y~x+l+  v, data=vT0, FUN=mean)
      }

      if (length(which(is.na(vT0$y)))==nrow(vT0))
        return(data.frame())
      
      ag    <- aggregate(y~x+l, data=vT0, FUN=mean)
      ag$sd <- aggregate(y~x+l, data=vT0, FUN=sd)[,3]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(y~x+l, data=vT0, FUN=length)[,3]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure0=="SD")
      {
        ag$ll <- ag[,3] - z * ag$sd
        ag$ul <- ag[,3] + z * ag$sd
      }
      if (input$selMeasure0=="SE")
      {
        ag$ll <- ag[,3] - z * ag$se
        ag$ul <- ag[,3] + z * ag$se
      }

      ag <- ag[order(ag[,2]),]

      colnames(ag)[1] <- "time"
      colnames(ag)[2] <- paste(input$replyLine0, collapse = " ")
      colnames(ag)[3] <- input$replyVar0

      return(ag)
    }
    else

    if ((length(input$catPlot0)>0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
    {
      vT <- subset(vT, is.element(vT$indexPlot,input$catPlot0) & is.element(vT$indexLine,input$catLine0))
      vT$indexPlot <- as.character(vT$indexPlot)
      vT$indexLine <- as.character(vT$indexLine)

      if (nrow(vT)==0)
        return(data.frame())

      x <- c()
      y <- c()
      p <- c()
      l <- c()
      s <- c()
      v <- c()

      for (i in (1:length(indexVar)))
      {
        x <- as.numeric(as.character(c(x,rep(timeLabs[i],nrow(vT)))))
        y <- c(y,vT[,indexVar[i]])
        p <- c(p,vT$indexPlot)
        l <- c(l,vT$indexLine)
        s <- c(s,as.character(vT$speaker))
        v <- c(v,as.character(vT$consonant))
      }

      vT0 <- data.frame(x,p,l,s,v,y)

      if (is.element("average",input$selGeon0))
      {
        vT0 <- aggregate(y~x+p+l+s+v, data=vT0, FUN=mean)
        vT0 <- aggregate(y~x+p+l+  v, data=vT0, FUN=mean)
      }

      if (length(which(is.na(vT0$y)))==nrow(vT0))
        return(data.frame())
      
      ag    <- aggregate(y~x+p+l, data=vT0, FUN=mean)
      ag$sd <- aggregate(y~x+p+l, data=vT0, FUN=sd)[,4]
      ag$sd[is.na(ag$sd)] <- 0
      ag$n  <- aggregate(y~x+p+l, data=vT0, FUN=length)[,4]
      ag$se <- ag$sd / sqrt(ag$n)

      if (input$selMeasure0=="SD")
      {
        ag$ll <- ag[,4] - z * ag$sd
        ag$ul <- ag[,4] + z * ag$sd
      }
      if (input$selMeasure0=="SE")
      {
        ag$ll <- ag[,4] - z * ag$se
        ag$ul <- ag[,4] + z * ag$se
      }

      ag <- ag[order(ag[,2]),]

      colnames(ag)[1] <- "time"
      colnames(ag)[2] <- paste(input$replyPlot0, collapse = " ")
      colnames(ag)[3] <- paste(input$replyLine0, collapse = " ")
      colnames(ag)[4] <- input$replyVar0

      return(ag)
    }
    else
      return(data.frame())
  })

  output$selVowel0 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    options <- c()

    vars <- varList(T)
    
    vars1 <- (grep("^[<]"            , vars, value = T))
    vars2 <- (grep("^[a-z, A-Z, 0-9]", vars, value = T))
    vars3 <- (grep("^[>]"            , vars, value = T))
    
    if (length(vars1) > 0)
      options <- c(options, "preceding vowel")

    if (length(vars2) > 0)
      options <- c(options, "consonant")
    
    if (length(vars3) > 0)
      options <- c(options, "following vowel")

    selectInput('replyVowel0', 'Select segment:', options, selected = options[2], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selScale0 <- renderUI(
  {
    selectInput('replyScale0', 'Scale:', optionsScale(), selected = optionsScale()[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selRef0 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    if ((length(input$replyScale0)>0) && (input$replyScale0==" ST"))
      numericInput('replyRef0', 'Reference frequency:', value=50, min=1, step=1, width = "100%")
    else
      return(NULL)
  })

  output$selVar0 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    req(input$replyVowel0)
    
    vars <- varList(T)
    
    if (input$replyVowel0=="preceding vowel")
      options <- (grep("^[<]"            , vars, value = T))
      
    if (input$replyVowel0=="consonant")
      options <- (grep("^[a-z, A-Z, 0-9]", vars, value = T)) 
      
    if (input$replyVowel0=="following vowel")
      options <- (grep("^[>]"            , vars, value = T)) 
    
    selectInput('replyVar0', 'Variable:', options, selected = options[1], multiple=FALSE, selectize=FALSE, width="100%", size=4)
  })

  output$catXaxis0 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)

    selectInput('catXaxis0', 'Select points:', getTimeCode(), multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$selLine0 <- renderUI(
  {
    if ((is.null(consonantTab())) || (length(input$replyVowel0)==0))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if ((input$replyVowel0=="preceding vowel") && (is.element("<vowel", colnames(consonantTab()))))
      options <- c(options, "<vowel")
    
    if ((input$replyVowel0=="following vowel") && (is.element(">vowel", colnames(consonantTab()))))
      options <- c(options, ">vowel")

    selectInput('replyLine0', 'Color variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$catLine0 <- renderUI(
  {
    if ((is.null(consonantTab())) || (length(input$replyVowel0)==0))
      return(NULL)

    vT <- data.frame()
    
    if ((input$replyVowel0=="preceding vowel") && (is.element("<vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(), `<vowel`!="∅")
    
    if  (input$replyVowel0=="consonant")
      vT <- consonantTab()
    
    if ((input$replyVowel0=="following vowel") && (is.element(">vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(), `>vowel`!="∅")

    if ((length(input$replyLine0)>0) && (nrow(vT) > 0))
      options <- unique(fuseCols(vT,input$replyLine0))
    else
      options <- NULL

    selectInput('catLine0', 'Select colors:', options, multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$selPlot0 <- renderUI(
  {
    if ((is.null(consonantTab())) || (length(input$replyVowel0)==0))
      return(NULL)

    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))

    if ((input$replyVowel0=="preceding vowel") && (is.element("<vowel", colnames(consonantTab()))))
      options <- c(options, "<vowel")
    
    if ((input$replyVowel0=="following vowel") && (is.element(">vowel", colnames(consonantTab()))))
      options <- c(options, ">vowel")
    
    selectInput('replyPlot0', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$catPlot0 <- renderUI(
  {
    if ((is.null(consonantTab())) || (length(input$replyVowel0)==0))
      return(NULL)

    vT <- data.frame()
    
    if ((input$replyVowel0=="preceding vowel") && (is.element("<vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(), `<vowel`!="∅")

    if  (input$replyVowel0=="consonant")
      vT <- consonantTab()
    
    if ((input$replyVowel0=="following vowel") && (is.element(">vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(), `>vowel`!="∅")
    
    if ((length(input$replyPlot0)>0) && (nrow(vT) > 0))
      options <- unique(fuseCols(vT,input$replyPlot0))
    else
      options <- NULL

    selectInput('catPlot0', 'Select panels:', options, multiple=TRUE, selectize=FALSE, width="100%")
  })

  plotGraph0 <- function()
  {
    if (is.null(vowelSub0()) || (nrow(vowelSub0())==0))
      return(NULL)

    varName <- gsub("_", " ", input$replyVar0)
    varName <- gsub("<", "" , varName)
    varName <- gsub(">", "" , varName)
    
    if ((length(input$catPlot0)==0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
    {
      vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,2], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

      if (!is.element("smooth", input$selGeon0))
        vS <- vT
      else
      {
        vS <- data.frame(spline(vT$x, vT$y, n=nrow(vT)*10))
        vS$ll <- spline(vT$x, vT$ll, n=nrow(vT)*10)$y
        vS$ul <- spline(vT$x, vT$ul, n=nrow(vT)*10)$y
      }

      if (is.element("points", input$selGeon0))
        Geom_Point <- geom_point(colour="indianred2", size=3)
      else
        Geom_Point <- geom_point(colour="indianred2", size=0)

      graphics::plot(ggplot(data=vT, aes(x, y, group=1)) +
                     geom_line(data=vS, colour="indianred2", size=1) +
                     Geom_Point +
                     geom_ribbon(data=vS, aes(ymin=ll, ymax=ul), alpha=0.2) +
                     ggtitle(input$title0) +
                     scale_x_continuous(breaks = unique(vT$x)) +
                     xlab("relative duration") + ylab(paste0(varName, axisLab(input$replyVar0,input$replyScale0,""))) +
                     theme_bw() +
                     theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                           plot.title     =element_text(face="bold", hjust = 0.5),
                           aspect.ratio   =0.67))
    }
    else

    if ((length(input$catPlot0)>0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
    {
      vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,3], p=vowelSub0()[,2], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

      if (!is.element("smooth", input$selGeon0))
        vS <- vT
      else
      {
        panels <- unique(vT$p)

        vS <- data.frame()
        for (i in 1:length(panels))
        {
          vSsub <- subset(vT, p==panels[i])
          vSspl <- data.frame(spline(vSsub$x, vSsub$y, n=nrow(vSsub)*10), p=panels[i])
          vSspl$ll <- spline(vSsub$x, vSsub$ll, n=nrow(vSsub)*10)$y
          vSspl$ul <- spline(vSsub$x, vSsub$ul, n=nrow(vSsub)*10)$y
          vS <- rbind(vS,vSspl)
        }

        vT <- vT[with(vT, order(x, p)), ]
        vS <- vS[with(vS, order(x, p)), ]
      }

      if (is.element("points", input$selGeon0))
        Geom_Point <- geom_point(colour="indianred2", size=3)
      else
        Geom_Point <- geom_point(colour="indianred2", size=0)

      graphics::plot(ggplot(data=vT, aes(x, y, group=1)) +
                     geom_line(data=vS, colour="indianred2", size=1) +
                     Geom_Point +
                     geom_ribbon(data=vS, aes(ymin=ll, ymax=ul), alpha=0.2) +
                     ggtitle(input$title0) +
                     scale_x_continuous(breaks = unique(vT$x)) +
                     xlab("relative duration") + ylab(paste0(varName, axisLab(input$replyVar0,input$replyScale0,""))) +
                     facet_wrap(vars(p)) +
                     theme_bw() +
                     theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                           plot.title     =element_text(face="bold", hjust = 0.5),
                           aspect.ratio   =0.67))
    }
    else

    if ((length(input$catPlot0)==0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
    {
      vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,3], l=vowelSub0()[,2], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

      if (!is.element("smooth", input$selGeon0))
        vS <- vT
      else
      {
        lines <- unique(vT$l)

        vS <- data.frame()
        for (i in 1:length(lines))
        {
          vSsub <- subset(vT, l==lines[i])
          vSspl <- data.frame(spline(vSsub$x, vSsub$y, n=nrow(vSsub)*10), l=lines[i])
          vSspl$ll <- spline(vSsub$x, vSsub$ll, n=nrow(vSsub)*10)$y
          vSspl$ul <- spline(vSsub$x, vSsub$ul, n=nrow(vSsub)*10)$y
          vS <- rbind(vS,vSspl)
        }

        vT <- vT[with(vT, order(x, l)), ]
        vS <- vS[with(vS, order(x, l)), ]
      }

      if (is.element("points", input$selGeon0))
        Geom_Point <- geom_point(size=3)
      else
        Geom_Point <- geom_point(colour="indianred2", size=0)

      graphics::plot(ggplot(data=vT, aes(x, y, group=l, color=l)) +
                     geom_line(data=vS, size=1) +
                     Geom_Point +
                     geom_ribbon(data=vS, aes(x=x, ymin=ll, ymax=ul, fill = l), alpha=0.2, colour=NA) +
                     ggtitle(input$title0) +
                     scale_x_continuous(breaks = unique(vT$x)) +
                     xlab("relative duration") + ylab(paste0(varName, axisLab(input$replyVar0,input$replyScale0,""))) +
                     scale_colour_discrete(name=paste0(paste(input$replyLine0,collapse = " "),"\n")) +
                     theme_bw() +
                     theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                           plot.title     =element_text(face="bold", hjust = 0.5),
                           legend.key.size=unit(1.5, 'lines'),
                           aspect.ratio   =0.67) +
                     guides(fill="none"))
    }
    else

    if ((length(input$catPlot0)>0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
    {
      vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,4], p=vowelSub0()[,2], l=vowelSub0()[,3], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

      if (!is.element("smooth", input$selGeon0))
        vS <- vT
      else
      {
        panels <- unique(vT$p)
         lines <- unique(vT$l)

        vS <- data.frame()
        for (i in 1:length(panels))
        {
          for (j in 1:length(lines))
          {
            vSsub <- subset(vT, (p==panels[i]) & (l==lines[j]))

            if (nrow(vSsub)>0)
            {
              vSspl <- data.frame(spline(vSsub$x, vSsub$y, n=nrow(vSsub)*10), p=panels[i], l=lines[j])
              vSspl$ll <- spline(vSsub$x, vSsub$ll, n=nrow(vSsub)*10)$y
              vSspl$ul <- spline(vSsub$x, vSsub$ul, n=nrow(vSsub)*10)$y
              vS <- rbind(vS,vSspl)
            }
          }
        }

        vT <- vT[with(vT, order(x, p, l)), ]
        vS <- vS[with(vS, order(x, p, l)), ]
      }

      if (is.element("points", input$selGeon0))
        Geom_Point <- geom_point(size=3)
      else
        Geom_Point <- geom_point(size=0)

      graphics::plot(ggplot(data=vT, aes(x, y, group=l, color=l)) +
                     geom_line(data=vS, size=1) +
                     Geom_Point +
                     geom_ribbon(data=vS, aes(x=x, ymin=ll, ymax=ul, fill = l), alpha=0.2, colour=NA) +
                     ggtitle(input$title0) +
                     scale_x_continuous(breaks = unique(vT$x)) +
                     xlab("relative duration") + ylab(paste0(varName, axisLab(input$replyVar0,input$replyScale0,""))) +
                     scale_colour_discrete(name=paste0(paste(input$replyLine0, collapse = " "),"\n")) +
                     facet_wrap(vars(p)) +
                     theme_bw() +
                     theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                           plot.title     =element_text(face="bold", hjust = 0.5),
                           legend.key.size=unit(1.5, 'lines'),
                           aspect.ratio   =0.67)+
                     guides(fill="none"))
    }
    else {}
  }

  res0 <- function()
  {
    if (length(input$replySize0b)==0)
      return( 72)
    
    if (input$replySize0b=="tiny"  )
      return( 54)
    if (input$replySize0b=="small" )
      return( 72)
    if (input$replySize0b=="normal")
      return( 90)
    if (input$replySize0b=="large" )
      return(108)
    if (input$replySize0b=="huge"  )
      return(144)
  }

  observeEvent(input$replySize0b,
  {
    output$graph0 <- renderPlot(height = 550, width = 700, res = res0(),
    {
      if (length(input$catXaxis0)>0)
      {
        plotGraph0()
      }
    })
  })

  output$Graph0 <- renderUI(
  {
    plotOutput("graph0", height="627px")
  })

  output$selFormat0a <- renderUI(
  {
    options <- c("txt","xlsx")
    selectInput('replyFormat0a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  fileName0a <- function()
  {
    return(paste0("transitionTable.",input$replyFormat0a))
  }

  output$download0a <- downloadHandler(filename = fileName0a, content = function(file)
  {
    if (length(input$catXaxis0)>0)
    {
      vT <- vowelSub0()

      colnames(vT)[which(colnames(vT)=="sd")] <- "standard deviation"
      colnames(vT)[which(colnames(vT)=="se")] <- "standard error"
      colnames(vT)[which(colnames(vT)=="n" )] <- "number of observations"
      colnames(vT)[which(colnames(vT)=="ll")] <- "lower limit"
      colnames(vT)[which(colnames(vT)=="ul")] <- "upper limit"
    }
    else
      vT <- data.frame()

    if (input$replyFormat0a=="txt")
    {
      utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    }
    else

    if (input$replyFormat0a=="xlsx")
    {
      WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
    }
    else {}
  })

  output$selSize0b <- renderUI(
  {
    options <- c("tiny", "small", "normal", "large", "huge")
    selectInput('replySize0b', label=NULL, options, selected = options[3], selectize=FALSE, multiple=FALSE)
  })

  output$selFont0b <- renderUI(
  {
    options <- c("FreeMono", "Latin Modern Mono", "FreeSans", "Latin Modern Sans", "TeX Gyre Bonum", "TeX Gyre Schola", "Latin Modern Roman", "TeX Gyre Pagella", "FreeSerif")
    selectInput('replyFont0b', label=NULL, options, selected = "FreeSans", selectize=FALSE, multiple=FALSE)
  })

  output$selPoint0b <- renderUI(
  {
    options <- c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48)
    selectInput('replyPoint0b', label=NULL, options, selected = 18, selectize=FALSE, multiple=FALSE)
  })

  output$selFormat0b <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat0b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  fileName0b <- function()
  {
    return(paste0("contoursPlot.",input$replyFormat0b))
  }

  output$download0b <- downloadHandler(filename = fileName0b, content = function(file)
  {
    grDevices::pdf(NULL)

    scale  <- 72/res0()
    width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
    
    if ((length(input$catXaxis0)>0) && (nrow(vowelSub0())>0))
      plot <- plotGraph0()
    else
      plot <- ggplot()+theme_bw()
    
    show_modal_spinner()
    
    if (input$replyFormat0b=="JPG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
    else
    if (input$replyFormat0b=="PNG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
    else
    if (input$replyFormat0b=="SVG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
    else
    if (input$replyFormat0b=="EPS")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
    else
    if (input$replyFormat0b=="PDF")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
    else    
    if (input$replyFormat0b=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}

    grDevices::graphics.off()
    
    remove_modal_spinner()
  })
  
  ##############################################################################

  consonantNorm1 <- reactive(
  {
    req(input$replySequence1)
    
    vT <- subset(consonantTab(),sequence==input$replySequence1)
    vT <- consonantNormD(vT,input$replyNormal1, input$replyVariable1)
    
    if (input$replySequence1=="VC")
    {
      vT <- consonantNormD(vT,input$replyNormal1, paste0("<", input$replyVariable1))
      vT[,colnames(vT)==paste0(">", input$replyVariable1)] <- 0
    }
    
    if (input$replySequence1=="VCV")
    {
      vT <- consonantNormD(vT,input$replyNormal1, paste0("<", input$replyVariable1))
      vT <- consonantNormD(vT,input$replyNormal1, paste0(">", input$replyVariable1))
    }

    if (input$replySequence1=="CV")
    {
      vT[,colnames(vT)==paste0("<", input$replyVariable1)] <- 0
      vT <- consonantNormD(vT,input$replyNormal1, paste0(">", input$replyVariable1))
    }
    
    return(vT)
  })
  
  consonantSub1 <- reactive(
  {
    if (is.null(consonantNorm1()) || (nrow(consonantNorm1())==0) || (length(input$catXaxis1)==0))
      return(NULL)
      
    vT <- consonantNorm1()
    
    vT$indexXaxis <- fuseCols(consonantNorm1(),input$replyXaxis1)

    if (length(input$catPlot1)>0)
      vT$indexPlot <- fuseCols(consonantNorm1(),input$replyPlot1)
    else
      vT$indexPlot <- "none"

    vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis1))

    if (nrow(vT)==0)
      return(data.frame())
    
    if (length(input$catPlot1)>0)
    {
      vT <- subset(vT, is.element(vT$indexPlot,input$catPlot1))
      
      if (nrow(vT)==0)
        return(data.frame())
    }

    var1 <- paste0("<", input$replyVariable1)
    if (var1 %in% colnames(vT))
      names(vT)[names(vT) == var1] <- "dur1"
    else
      vT$dur1 <- 0
    
    var2 <-             input$replyVariable1
    if (var2 %in% colnames(vT))
      names(vT)[names(vT) == var2] <- "dur2"
    else
      vT$dur2 <- 0
    
    var3 <- paste0(">", input$replyVariable1)
    if (var3 %in% colnames(vT))
      names(vT)[names(vT) == var3] <- "dur3"
    else
      vT$dur3 <- 0

    if (is.element("average",input$selGeon1))
    {
      vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, speaker=vT$speaker, consonant=vT$consonant, dur1=vT$dur1, dur2=vT$dur2, dur3=vT$dur3)
      vT <- aggregate(cbind(dur1,dur2,dur3) ~ indexXaxis + indexPlot + speaker + consonant, data=vT, FUN=mean)
      vT <- aggregate(cbind(dur1,dur2,dur3) ~ indexXaxis + indexPlot +           consonant, data=vT, FUN=mean)
    }
    
    ag <- aggregate(cbind(dur1,dur2,dur3) ~ indexXaxis + indexPlot, data=vT, FUN=mean)
    colnames(ag)[1] <- paste(input$replyXaxis1, collapse = " ")
    colnames(ag)[2] <- paste(input$replyPlot1 , collapse = " ")

    ag$total <- ag$dur1 + (0.5 * ag$dur2)
    maxTotal <- max(ag$total)

    vT <- data.frame()
      
    for (i in 1:nrow(ag))
    {
      vT <- rbind(vT, data.frame(
        Xaxis       = ag[i,1],
        Plot        = ag[i,2],
        duration    = ag$dur1[i],
        duration0   = ag$dur2[i],
        position    = "before"
      ))

      vT <- rbind(vT, data.frame(
        Xaxis       = ag[i,1],
        Plot        = ag[i,2],
        duration    = ag$dur2[i],
        duration0   = ag$dur2[i],
        position    = "target"
      ))
        
      vT <- rbind(vT, data.frame(
        Xaxis       = ag[i,1],
        Plot        = ag[i,2],
        duration    = ag$dur3[i],
        duration0   = ag$dur2[i],
        position    = "after"
      ))
        
      vT <- rbind(vT, data.frame(
        Xaxis       = ag[i,1],
        Plot        = ag[i,2],
        duration    = maxTotal - ag$total[i],
        duration0   = ag$dur2[i],
        position    = "filler"
      ))
    }

    vT$position <- factor(vT$position, levels = c("after", "target", "before", "filler"))

    return(vT)
  })

  listVar <- function()
  {
    req(consonantTab())
    req(input$replySequence1)
    
    colNames <- colnames(consonantTab())
    
    colNames <- grep("^[<|>]", colNames, value = T)
    varNames <- as.data.frame(table(colNames))
    
    varNames <- subset(varNames, Freq==1)
    varNames <- subset(varNames, (colNames!="<vowel") & (colNames!=">vowel"))
    
    varNames <- as.character(varNames$colNames)
    
    if (input$replySequence1=="VC")
    {
      varNames <- varNames[grep("^<", varNames)]
      varNames <- gsub("<", "", varNames)
    }
    
    if (input$replySequence1=="VCV")
    {
      varNames <- gsub("<|>", "", varNames)
      varNames <- as.data.frame(table(varNames))
      varNames <- subset(varNames, Freq==2)
      varNames <- as.character(varNames$varNames)
    }
    
    if (input$replySequence1=="CV")
    {
      varNames <- varNames[grep("^>", varNames)]
      varNames <- gsub(">", "", varNames)
    }
  
    varNames <- intersect(varNames, varList(F))

    return(varNames)
  }
  
  output$selVariable1 <- renderUI(
  {
    if (is.null(consonantTab()))
      return(NULL)
    
    selectInput('replyVariable1', 'Select variable:', listVar(), selected = listVar()[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selSequence1 <- renderUI(
  {
    options <- c()  
    
    if (is.element("VC" , consonantTab()$sequence))
      options <- c(options, "VC" )
    if (is.element("VCV", consonantTab()$sequence))
      options <- c(options, "VCV")
    if (is.element( "CV", consonantTab()$sequence))
      options <- c(options,  "CV")
    
    selectInput('replySequence1', 'Choose sequence:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
  })

  output$selNormal1 <- renderUI(
  {
    options <- optionsNormal()[c(1,3)]
    
    selectInput('replyNormal1', 'Normalization:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
  })
  
  output$selXaxis1 <- renderUI(
  {
    if ((is.null(consonantTab())) | (length(input$replySequence1)==0))
      return(NULL)
      
    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))
    options <- options[options!="sequence"]

    if ((input$replySequence1=="VC" ) && (is.element("<vowel", colnames(consonantTab()))))
      options <- c(options, "<vowel")
    
    if ((input$replySequence1=="VCV") && (is.element("<vowel", colnames(consonantTab()))))
      options <- c(options, "<vowel")

    if ((input$replySequence1=="VCV") && (is.element(">vowel", colnames(consonantTab()))))
      options <- c(options, ">vowel")

    if ((input$replySequence1== "CV") && (is.element(">vowel", colnames(consonantTab()))))
      options <- c(options, ">vowel")

    selectInput('replyXaxis1', 'Variable x-axis:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })

  output$catXaxis1 <- renderUI(
  {
    if ((is.null(consonantTab())) | (length(input$replySequence1)==0))
      return(NULL)

    vT <- data.frame()
    
    if ((input$replySequence1=="VC" ) && (is.element("<vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `<vowel`!="∅")

    if ((input$replySequence1=="VCV") && (is.element("<vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `<vowel`!="∅")

    if ((input$replySequence1=="VCV") && (is.element(">vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `>vowel`!="∅")
    
    if ((input$replySequence1== "CV") && (is.element(">vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `>vowel`!="∅")
    
    if (length(input$replyXaxis1)>0)
      options <- unique(fuseCols(vT,input$replyXaxis1))
    else
      options <- NULL
      
    selectInput('catXaxis1', 'Sel. categories:', options, multiple=TRUE, selectize = FALSE, width="100%")
  })
  
  output$selPlot1 <- renderUI(
  {
    if ((is.null(consonantTab())) | (length(input$replySequence1)==0))
      return(NULL)
      
    indexConsonant <- grep("^consonant$", colnames(consonantTab()))
    options <- c(colnames(consonantTab()[indexConsonant]),colnames(consonantTab()[1:(indexConsonant-1)]))
    options <- options[options!="sequence"]

    if ((input$replySequence1=="VC" ) && (is.element("<vowel", colnames(consonantTab()))))
      options <- c(options, "<vowel")
    
    if ((input$replySequence1=="VCV") && (is.element("<vowel", colnames(consonantTab()))))
      options <- c(options, "<vowel")
    
    if ((input$replySequence1=="VCV") && (is.element(">vowel", colnames(consonantTab()))))
      options <- c(options, ">vowel")

    if ((input$replySequence1== "CV") && (is.element(">vowel", colnames(consonantTab()))))
      options <- c(options, ">vowel")

    selectInput('replyPlot1', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
  })
  
  output$catPlot1 <- renderUI(
  {
    if ((is.null(consonantTab())) | (length(input$replySequence1)==0))
      return(NULL)

    vT <- data.frame()
    
    if ((input$replySequence1=="VC" ) && (is.element("<vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `<vowel`!="∅")
    
    if ((input$replySequence1=="VCV") && (is.element("<vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `<vowel`!="∅")
    
    if ((input$replySequence1=="VCV") && (is.element(">vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `>vowel`!="∅")
    
    if ((input$replySequence1== "CV") && (is.element(">vowel", colnames(consonantTab()))))
      vT <- subset(consonantTab(),  `>vowel`!="∅")
    
    if (length(input$replyPlot1)>0)
      options <- unique(fuseCols(vT,input$replyPlot1))
    else
      options <- NULL

    selectInput('catPlot1', 'Select panels:', options, multiple=TRUE, selectize = FALSE, width="100%")
  })
  
  plotGraph1 <- function()
  {
    if (is.null(consonantSub1()) || (nrow(consonantSub1())==0))
      return(NULL)

    varName <- gsub("_", " ", input$replyVariable1)
    varName <- gsub("<", "" , varName)
    varName <- gsub(">", "" , varName)
    
    if (input$replySequence1=="VC")
    {
      vT <- subset(consonantSub1(), position!="after")
      vT$position <- factor(vT$position, levels = c("target", "before", "filler"))
      indices <- c(2,3,4)
    }
    else
      
    if (input$replySequence1=="CV")
    {
      vT <- subset(consonantSub1(), position!="before")
      vT$position <- factor(vT$position, levels = c("after", "target", "filler"))
      indices <- c(1,2,4)
    }
    else
    {
      vT <- consonantSub1()
      indices <- c(1,2,3,4)
    }

    if (length(input$catPlot1)==0)
    {
      graphics::plot(ggplot(vT, aes(x=reorder(Xaxis, duration0), y=duration, fill=position)) + 
                     geom_bar(position="stack", stat="identity", width = .7) +
                     geom_text(aes(label=ifelse(((position!="filler") & (duration!=0)), paste0(sprintf("%.0f", duration),""), "")),
                               position=position_stack(vjust=0.5), 
                               colour=ifelse(vT$position=="target", "white", "black"),
                               size = 3) +
                     scale_fill_manual(values = c("#619cff", "#555555", "#619cff", "white")[indices],
                                       name   = "element\n", 
                                       labels = c('vowel', 'consonant', 'vowel', '')[indices]) +
                     ggtitle(input$title1) +
                     xlab("") + ylab(paste0(varName, axisLab(input$replyVariable1," none",input$replyNormal1))) +
                     coord_flip() +
                     theme_bw() +
                     theme(text         = element_text(size=as.numeric(input$replyPoint1b), family=input$replyFont1b),
                           axis.text.x  = element_blank(),
                           axis.ticks.x = element_blank(),
                           plot.title   = element_text(face="bold", hjust = 0.5),
                           aspect.ratio =0.9))
    }
    else

    if (length(input$catPlot1)>0 )
    {
      graphics::plot(ggplot(vT, aes(x=reorder(Xaxis, duration0), y=duration, fill=position)) + 
                     geom_bar(position="stack", stat="identity", width = .7) +
                     geom_text(aes(label=ifelse(((position!="filler") & (duration!=0)), paste0(sprintf("%.0f", duration),""), "")),
                               position=position_stack(vjust=0.5), 
                               colour=ifelse(vT$position=="target", "white", "black"),
                               size = 3) +
                     scale_fill_manual(values = c("#619cff", "#555555", "#619cff", "white")[indices],
                                       name   = "element\n", 
                                       labels = c('vowel', 'consonant', 'vowel', '')[indices]) +
                     ggtitle(input$title1) +
                     xlab("") + ylab(paste0(varName, axisLab(input$replyVariable1," none",input$replyNormal1))) +
                     coord_flip() +
                     facet_wrap(vars(Plot)) +
                     theme_bw() +
                     theme(text         = element_text(size=as.numeric(input$replyPoint1b), family=input$replyFont1b),
                           axis.text.x  = element_blank(),
                           axis.ticks.x = element_blank(),
                           plot.title   = element_text(face="bold", hjust = 0.5),
                           aspect.ratio =0.9))
    }
    else {}
  }

  res1 <- function()
  {
    if (length(input$replySize1b)==0)
      return( 72)
    
    if (input$replySize1b=="tiny"  )
      return( 54)
    if (input$replySize1b=="small" )
      return( 72)
    if (input$replySize1b=="normal")
      return( 90)
    if (input$replySize1b=="large" )
      return(108)
    if (input$replySize1b=="huge"  )
      return(144)
  }

  observeEvent(input$replySize1b,
  {
    output$graph1 <- renderPlot(height = 550, width = 700, res = res1(),
    {
      if (length(input$catXaxis1)>0)
      {
        plotGraph1()
      }
    })
  })

  output$Graph1 <- renderUI(
  {
    plotOutput("graph1", height="627px")
  })

  output$selFormat1a <- renderUI(
  {
    options <- c("txt","xlsx")
    selectInput('replyFormat1a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  fileName1a <- function()
  {
    return(paste0("temporalTable.",input$replyFormat1a))
  }

  output$download1a <- downloadHandler(filename = fileName1a, content = function(file)
  {
    if (length(input$catXaxis1)>0)
      vT <- consonantSub1()
    else
      vT <- data.frame()

    if (input$replyFormat1a=="txt")
    {
      utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    }
    else

    if (input$replyFormat1a=="xlsx")
    {
      WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
    }
    else {}
  })

  output$selSize1b <- renderUI(
  {
    options <- c("tiny", "small", "normal", "large", "huge")
    selectInput('replySize1b', label=NULL, options, selected = options[3], selectize=FALSE, multiple=FALSE)
  })

  output$selFont1b <- renderUI(
  {
    options <- c("FreeMono", "Latin Modern Mono", "FreeSans", "Latin Modern Sans", "TeX Gyre Bonum", "TeX Gyre Schola", "Latin Modern Roman", "TeX Gyre Pagella", "FreeSerif")
    selectInput('replyFont1b', label=NULL, options, selected = "FreeSans", selectize=FALSE, multiple=FALSE)
  })

  output$selPoint1b <- renderUI(
  {
    options <- c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48)
    selectInput('replyPoint1b', label=NULL, options, selected = 18, selectize=FALSE, multiple=FALSE)
  })

  output$selFormat1b <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat1b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  fileName1b <- function()
  {
    return(paste0("temporalPlot.",input$replyFormat1b))
  }

  output$download1b <- downloadHandler(filename = fileName1b, content = function(file)
  {
    grDevices::pdf(NULL)

    scale  <- 72/res1()
    width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
    
    if ((length(input$catXaxis1)>0) && (nrow(consonantSub1())>0))
      plot <- plotGraph1()
    else
      plot <- ggplot()+theme_bw()
    
    show_modal_spinner()
    
    if (input$replyFormat1b=="JPG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
    else
    if (input$replyFormat1b=="PNG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
    else
    if (input$replyFormat1b=="SVG")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
    else
    if (input$replyFormat1b=="EPS")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
    else
    if (input$replyFormat1b=="PDF")
      ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
    else    
    if (input$replyFormat1b=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}

    grDevices::graphics.off()
    
    remove_modal_spinner()
  })
}

################################################################################

options(shiny.sanitize.errors = TRUE)
options(shiny.usecairo=FALSE)
options(shiny.maxRequestSize=20*1024^2)

shinyApp(ui = ui, server = server)

################################################################################
