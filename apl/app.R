# Panggil paket yang diperlukan ----
library(shiny)
library(gridExtra)
library(tidyverse)

# Buat UI ----

ui <-shinyUI(fluidPage(
  
  titlePanel("Teorema Limit Pusat untuk Proposi",
             windowTitle = "TLP untuk Proporsi"),
  
  sidebarPanel(
    wellPanel(
      sliderInput("n", 
                  "Ukuran sampel:", 
                  value = 200,
                  min = 2, 
                  max = 1000),
      br(),
      
      sliderInput("p", 
                  "Proporsi populasi:", 
                  value = .5,
                  step = .01,
                  min = 0, 
                  max = 1),
      br(),
      
      sliderInput("k", 
                  "Banyak sampel:", 
                  value = 100,
                  min = 10, 
                  max = 1000)
    ),
    
    helpText(a(href="https://github.com/ydkristanto/apl-tlp-prop",
               target="_blank", "Lihat kode sumber"))
    
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Distribusi Populasi",
                         br(),
                         plotOutput("pop.dist", height = "450px")),
                tabPanel("Beberapa Sampel",
                         br(),
                         br(),
                         plotOutput("sample.dist"),
                         br(),
                         div(h3(textOutput("num.samples")),
                             align ="center")),
                tabPanel("Distribusi Sampling",
                         plotOutput("sampling.dist"),
                         div(textOutput("plot.descr"),
                             align = "center"),
                         br(),
                         fluidRow(column(8, br(), br(),
                                         div(textOutput("CLT.descr"),
                                             align = "justify"), br()),
                                  column(4, br(),
                                         plotOutput("pop.dist1",
                                                    height = "200px"))))
    )
  )
))

# Buat fungsi peladen ----
seed = as.numeric(Sys.time())

server <- shinyServer(function(input, output) {
  
  rand_draw = function(n, p) 
  {
    vals = NULL
    vals = do.call(rbinom, list(n = n, size = 1, prob = p))      
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)  
  
  parent = reactive({
    n = 1e5
    return(rep_rand_draw(input$n, input$p))
  })
  
  samples = reactive({
    pop = parent()
    n = input$n
    k = input$k
    return(replicate(k, sample(pop, n, replace = TRUE)))
  })
  
  ## plot 1 ----
  output$pop.dist = renderPlot({
    popsize = 1000
    counts = data.frame(number = c("0","1"),
                        freq = c(popsize * (1 - input$p),
                                 popsize * input$p) / popsize)
    ggplot(counts, aes(x = number, y = freq, fill = factor(number))) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Frekuensi Relatif",
           title = paste0("Distribusi populasi: p = ", input$p),
           size = 14, face = "bold") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(base_size = 16) +
      theme(legend.position = "none")
  })
  
  ## plot 2 ----
  output$sample.dist = renderPlot({ 
    
    x = samples()
    
    plot <- list()
    
    for(i in 1:8){
      df <- tibble(obs = x[,i])
      counts <- df %>% count(obs)
      
      
      plot[[i]] <- ggplot(counts, aes(x = obs, y = n, fill = factor(obs))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, 1.2 * max(counts$n))) +
        scale_x_discrete(limits = c(0, 1)) +
        scale_fill_brewer(palette = "Dark2") +
        theme_bw(base_size = 12) +
        theme(legend.position = "none") +  
        labs(x = "",  y = "Frekuensi",
             title = paste("Sampel",i), size = 14, face = "bold")
      
      mean_samp = round(mean(x[,i]),2)
      
      sd_samp = round(sd(x[,i]),2)
      
      y_pos = max(counts$n) + 0.07 * max(counts$n)
      
      
      # #added if statement to check if count 1 or count 2 are NA. this check
      # #eliminated the error messages in the app
      
      if(!is.na(counts$n[1]) & !is.na(counts$n[2])) {
        if(counts$n[1] > counts$n[2]) {
          plot[[i]] <- plot[[i]] +
            annotate("text", x = 1, y = y_pos,
                     label = paste("p_topi =",
                                   bquote(.(mean_samp))),
                     color ="black", size = 3) 
        }
        else {
          plot[[i]] <- plot[[i]] +
            annotate("text", x = 0, y = y_pos,
                     label = paste("p_topi =" ,
                                   bquote(.(mean_samp))),
                     color = "black", size = 3) 
        }}
      else {
        plot[[i]] <- plot[[i]] +
          annotate("text", x = 0.5, y = y_pos,
                   label = paste("p_topi =" , bquote(.(mean_samp))),
                   color = "black", size = 3)
      }
    }
    grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]],
                 plot[[5]], plot[[6]], plot[[7]], plot[[8]],
                 ncol = 4)
  })
  
  # text
  output$num.samples = renderText({
    k = input$k
    paste0("... dan seterusnya sampel sampel ke-", k,".")
  })
  
  # plot 3
  output$pop.dist1 = renderPlot({
    popsize = 1000
    counts = data.frame(number = c("0","1"),
                        freq = c(popsize * (1 - input$p),
                                 popsize * input$p) / popsize)
    
    
    ggplot(counts, aes(x = number, y = freq, fill = factor(number))) +
      geom_bar(stat = "identity") +
      labs(x= "",  y = "Relative Frequency",
           title = paste0("Population distribution: p = ", input$p),
           size = 8, face="bold") +
      scale_y_continuous(limits= c(0, 1)) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(base_size = 8) +
      theme(legend.position = "none")
  })
  
  
  output$sampling.dist = renderPlot({
    n = input$n
    p = input$p
    k = input$k
    pop = parent()
    ndist = tibble(means = colMeans(samples()))
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot = FALSE)
    
    m_samp = round(mean(ndist$means), 2)
    sd_samp = round(sd(ndist$means), 2)
    sd_teor = sqrt(p * (1 - p) / n)
    
    x_range = max(ndist$means) - min(ndist$means)
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = if_else(m_samp > 0, min(ndist$means) + 0.1*x_range, max(ndist$means) - 0.1*x_range)
    
    # minor change in the way the title is displayed
    
    ggplot(ndist, aes(x = ndist$means)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 20, color ="white") +
      stat_density(geom = "line", size = 1) +
      labs(title = paste("Distribusi sampling proporsi*:"),
           x = "Proporsi sampel",
           y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("rerata p_topi","=", bquote(.(m_samp)),"\n", "SD p_topi ", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_bw(base_size = 17) 
  })
  
  # text
  output$plot.descr = renderText({
    n = input$n
    p = input$p
    k = input$k
    
    paste("*Distribusi proporsi ", k, 
          " sampel acak, masing-masing\nmemuat ", n, 
          " observasi dari populasi", sep = "")
    
  })
  
  # text
  output$CLT.descr = renderText({
    
    n = input$n ; p = input$p ; q = 1-p
    
    pop = parent()
    m_pop =  p
    
    n = input$n
    se=round(sqrt(p*(1-p)/n),4)
    
    paste("Berdasarkan Teorema Limit Pusat (TLP), distribusi sampling proporsi mendekati normal. Rerata distribusi sampling tersebut kurang lebih p (", m_pop, ") dan galat bakunya (simpangan baku proporsi sampel) bernilai kurang lebih sama dengan akar kuadrat peluang sukses (p) dikali dengan peluang gagal (1 - p) dibagi dengan ukuran sampel (sqrt(", p, "*", q,
          "/",n, ") =", se,").", sep = "")
    
  })
  
  
})

# Membuat objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)