#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(pacman)
# p_load(shiny,plotly,leaflet,shinythemes,knitr,rgdal,tidyverse,feather,readxl)
library(shiny)
library(plotly)
library(leaflet)
library(shinythemes)
library(knitr)
library(rgdal)
library(tidyverse)
library(feather)
library(readxl)

ui <- fluidPage(theme = shinytheme("flatly"),
                
                tags$head(tags$style(HTML('* {font-family: Calibri, sans-serif !important;}'))),
                titlePanel(title = div(h2("The Impact of Nashville's Growth on C&D Debris",
                                          img(src = "Zero Waste Logo - Metro.png", align="right", height=150, width=200)),
                                                        h4("Visualizing historical building permits and construction and demolition debris with projections for future trends, 2015 - 2026",
                                                           h5("Data Collected June 16, 2021")))
                                       ),
                
                tabsetPanel(
                  tabPanel("Nashville Map", 
                           fluidRow(
                             column(3,
                                    style="top:20px",
                                    sidebarPanel(style="width: 300px; position:left; left: 20px; right:0px; top: 20px; height: 450px",
                                                 sliderInput("map_year", "Year: ", value = 2022, min = 2022, max = 2026, sep="", animate=TRUE),
                                                 selectInput("map_comm_v_res", "Commercial or Residential:", c("All", "Commercial", "Residential")),
                                                 selectInput("map_project_type", "Construction or Demolition:", c("All", "Construction", "Demolition")),
                                                 selectInput("map_cat", "Category: ", c("Total Debris", "Num. Debris-Generating Permits", "Ave. Cost per Permit")),
                                                 checkboxInput("map_check", "Auto-scale color",value = FALSE))
                             ),
                             column(9, style="top:20px; right:20px; height:600px; width:950px", leafletOutput("nashmap")),
                           )
                  ),
                  tabPanel("C&D Debris",
                           fluidRow(
                             column(2,
                                    style="top:20px",
                                    sidebarPanel(style="width: 200px; position:left; left: 20px; right:20px; top: 20px;",
                                                 selectInput("comm_v_res", "Commercial or Residential:", c("All", "Commercial", "Residential")),
                                                 selectInput("project_type", "Construction or Demolition:", c("All", "Construction", "Demolition")),
                                                 selectInput("cost_range", "Cost Ranges: ", c("All","0-50k", "50k-500k", "500k-3M", "3M-20M", ">20M")),
                                                 checkboxInput("recycling", "Show recycling",value = FALSE))),
                             column(5, style="top:20px", plotlyOutput("debris_plot")),
                             column(5, style="top:20px", plotlyOutput("hover_debris"))
                           )
                  ),
                  tabPanel("Debris-Generating Permits",
                           fluidRow(
                             column(2,
                                    style="top:20px",
                                    sidebarPanel(style="width: 200px; position:left; left: 20px; right:20px; top: 20px;",
                                                 selectInput("permits_time", "Time Period:", c("Year", "Quarter")),
                                                 selectInput("permits_comm_v_res", "Commercial or Residential:", c("All", "Commercial", "Residential")),
                                                 selectInput("permits_project_type", "Construction or Demolition:", c("All", "Construction", "Demolition")))),
                             column(5, style="top:20px", plotlyOutput("permit_plot")),
                             column(5, style="top:20px", plotlyOutput("hover_permits"))
                           )
                  )#,
                  # tabPanel("Methodology",
                  #          uiOutput('markdown'))
                )
)

#------------------------------------------------------------------------------#
# set up global plotting aesthetics
# aes_1 <- list("color" = "#18BC9C", "fillcolor" = "rgba(24,188,156,.25)")
aes_1 <- list("color" = "#738D51", "fillcolor" = "rgba(128,128,128,.25)")
aes_2 <- list("color" = "#4A6BBD", "fillcolor" = "rgba(74,107,189,.25)")
# colors for subtypes - first letter indicates comm_v_res; second indicates project_type (const or dem)
color_cc <- 'rgba(128,0,0,1)'
color_cd <- 'rgba(255,128,128,1)'
color_ca <- 'rgba(255,0,0,1)'
color_rc <- 'rgba(0,102,153,1)'
color_rd <- 'rgba(102,204,255,1)'
color_ra <- 'rgba(25,64,255,1)'
color_ac <- 'rgba(64,0,77,1)'
color_ad <- 'rgba(234,128,255,1)'
subtype_colors <- list("Commercial Construction" = color_cc,
                       "Commercial Demolition" = color_cd,
                       "Commercial All" = color_ca,
                       "Residential Construction" = color_rc,
                       "Residential Demolition" = color_rd,
                       "Residential All" = color_ra,
                       "All Construction" = color_ac,
                       "All Demolition" = color_ad
                       )
fs1 <- 18
fs2 <- 14

#------------------------------------------------------------------------------#
plot_npermit_forecast <- function(forecast, time = "fiscal_quarter"){
  #' plot_npermit_forecast
  #' 
  #' Generate plot of forecast of npermits vs time
  #'
  #' @param forecast tibble from "shiny-data/best_forecast.feather" with user-selected filters
  #' @param time user selected unit of time (fiscal quarter or fiscal year)
  #'
  #' @return figure
  
  ts <- rlang::sym(time)
  ts2 <- as.formula(sprintf("~%s", time))
  
  min_npermits <- 0
  max_npermits <- switch(time, "fiscal_year" = 16000, "fiscal_quarter" = 5000)
  xlabel <- switch(time, "fiscal_year" = "Fiscal Year", "fiscal_quarter" = "Fiscal Quarter")
  
  fig <- plot_ly(width = 600, type = "scatter", mode="lines")
  
  forecast <- forecast %>%
    filter(fy<2027) %>%
    group_by(!!ts) %>%
    summarise(ntot = sum(forecast, na.rm = TRUE), 
              err1 = sqrt(sum(half_iqr^2, na.rm = TRUE)),
              err2 = sqrt(sum(half_i90^2, na.rm = TRUE))) %>%
    ungroup()
  
  # a bit hacky to get the tick labels right
  if(time=="fiscal_year"){
    mask <- c(TRUE)
  }
  else{
    mask <- c(TRUE, FALSE, FALSE, FALSE)
  }
  tickvals <- forecast %>% 
    select(!!ts) %>% 
    unique()
  tickvals <- simplify2array(tickvals)
  tickvals <- tickvals[mask]
  
  fig <- fig %>% 
    add_lines(data = forecast, x = ts2, y = ~ntot, line = list(color = aes_1$color, width=3), 
              name = "Num. Permits") %>%
    add_ribbons(data = forecast, x = ts2, ymax = ~ntot+err1, ymin = ~ntot-err1, color = NA, 
                fillcolor = aes_1$fillcolor, name = "50% Confidence", showlegend=FALSE) %>%
    add_ribbons(data = forecast, x = ts2, ymax = ~ntot+err2, ymin = ~ntot-err2, color = NA, 
                fillcolor = aes_1$fillcolor, name = "90% Confidence", showlegend=FALSE) %>%
    add_ribbons(data = forecast, x = ts2, ymax = ~ntot+1, ymin = ~ntot-1,
                fillcolor = aes_1$fillcolor, color = NA,
                showlegend = TRUE, name = "50, 90% Confidence", visible = "legendonly") %>% 
    # add_segments(x = "FY 21 - Q3",xend = "FY 21 - Q3", y = 1000, yend = 3500) %>%
    layout(
      title = "Total Debris-Generating Permits",
      titlefont = list(size = fs1),
      yaxis = list(title = "Num. Debris-Generating Permits", range = c(min_npermits, max_npermits),
                   titlefont = list(size = fs1), tickfont = list(size = fs2)),
      xaxis = list(title = "", tickangle = -45, tickvals = tickvals, 
                   titlefont = list(size = fs1), tickfont = list(size = fs2)),
      legend = list(x=0.0, y=1.0, font = list(size = fs2))
    )
}

#------------------------------------------------------------------------------#
plot_permit_bar <- function(df, subtype_cols, time, selected_time, title, ylabel, yval, 
                           yrange, nround = 0){
  #' plot_permit_bar
  #' 
  #' Generate bar plot of npermits forecast for subtypes for a user-selected year.
  #'
  #' @param df tibble from "shiny-data/best_forecast.feather"
  #' @param subtype_cols list of columns in tibble defining subtypes
  #' @param time fiscal year or fiscal quarter
  #' @param selected_time user-selected (by clicking) year
  #' @param title title of plot
  #' @param ylabel ylabel of plot
  #' @param yval y-variable name
  #' @param yrange array with ymin and ymax
  #' @param nround number of decimals to which round the number of permits above each bar
  #'
  #' @return figure

  ts <- rlang::sym(time)
  yval <- rlang::sym(yval)
  
  # if clicked portion of plot is invalid, we want an empty plot
  if(!is.null(selected_time)){
    ndata <- df %>% 
      filter(!!ts == selected_time) %>% 
      nrow()
    if(ndata==0){
      selected_time <- NULL
    }
  }
  
  # make empty plot
  if(is.null(selected_time)){
    summ_data <- df %>% 
      mutate(across(all_of(subtype_cols), ~str_to_sentence(.))) %>% 
      unite(subtype, all_of(subtype_cols), sep = " \n") %>% 
      group_by(subtype) %>% 
      summarise(y = 0) %>% 
      ungroup()
    
    fig <- plot_ly(data = summ_data, x = ~subtype, y = ~y , type = "bar",
                   width = 500) %>%
      layout(
        title = title,
        titlefont = list(size = fs1),
        yaxis = list(title = ylabel, range = yrange, titlefont = list(size = fs1), 
                     tickfont = list(size = fs2)),
        xaxis = list(title = "", tickfont = list(size = fs2))
      )
  }
  else{ # make actual plot
    summ_data <- df %>% 
      filter(!!ts == selected_time) %>% 
      mutate(across(all_of(subtype_cols), ~str_to_sentence(.))) %>% 
      unite(subtype, all_of(subtype_cols), sep = "\n") %>% 
      group_by(subtype) %>% 
      summarise(ytmp = sum(!!yval, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(subtype) %>% 
      summarise(y = round(mean(ytmp), nround)) %>% 
      ungroup() %>% 
      arrange(subtype)

    title <- sprintf("%s in %s", title, selected_time)
    sub_colors = c(color_cc, color_cd, color_rc, color_rd)
    
    fig <- plot_ly(data = summ_data, x = ~subtype, y = ~y, type = "bar",
                   width = 500, text = ~y, textposition = 'outside', showlegend = FALSE,
                   marker = list(color = sub_colors)) %>% 
      layout(
        title = title,
        titlefont = list(size = fs1),
        yaxis = list(title = ylabel, range = yrange, titlefont = list(size = fs1), 
                     tickfont = list(size = fs2)),
        xaxis = list(title = "", tickfont = list(size = fs2))
      )
    
  }
  return(fig)
}

#------------------------------------------------------------------------------#
plot_debris_bar <- function(df, debris_proj, subtype_cols, rmspe, selected_time, title, 
                              ylabel, yval, yrange, nround = 0){
  #' plot_debris_bar
  #' 
  #' Generate bar plot of debris forecast for subtypes for a user-selected year.
  #'
  #' @param df tibble from "shiny-data/syn_permit_summary.feather", passed through `adjust_permit_summ`, with user-selected filters applied
  #' @param debris_proj tibble from "shiny-data/debris_projections.feather"
  #' @param subtype_cols list of columns in tibble defining subtypes
  #' @param rmspe root-mean-square percent error
  #' @param selected_time user-selected (by clicking) year
  #' @param title title of plot
  #' @param ylabel ylabel of plot
  #' @param yval y-variable name
  #' @param yrange array with ymin and ymax
  #' @param nround number of decimals to which round the tonnage values above each bar
  #'
  #' @return figure
  
  # if clicked portion of plot is invalid, we want an empty plot
  if(!is.null(selected_time)){
    ndata <- debris_proj %>%
      filter(cy == selected_time) %>%
      nrow()
    if(ndata==0){
      selected_time <- NULL
    }
  }

  # make empty plot
  if(is.null(selected_time)){
    summ_data <- df %>%
      mutate(across(all_of(subtype_cols), ~str_to_sentence(.))) %>%
      unite(subtype, all_of(subtype_cols), sep = " \n") %>%
      group_by(subtype) %>%
      summarise(y = 0) %>%
      ungroup()

    fig <- plot_ly(data = summ_data, x = ~subtype, y = ~y , type = "bar",
                   width = 500) %>%
      layout(
        title = title,
        titlefont = list(size = fs1),
        yaxis = list(title = ylabel, range = yrange, titlefont = list(size = fs1), 
                     tickfont = list(size = fs2)),
        xaxis = list(title = "", tickfont = list(size = fs2))
      )
  }
  else{
    tot_deb <- debris_proj %>% 
      filter(cy==selected_time)
    
    yr_lab <- tot_deb$year_label[1]
    tot_deb <- tot_deb$fit[1]

    summ_data <- df %>% 
      mutate(across(all_of(subtype_cols), ~str_to_sentence(.))) %>% 
      unite(subtype, all_of(subtype_cols), sep = "\n") %>% 
      mutate(y = round(tot_deb*frac_debris,nround)) %>% 
      arrange(subtype)
    
    title <- sprintf("%s in %s", title, yr_lab)
    sub_colors = c(color_cc, color_cd, color_rc, color_rd)
    
    fig <- plot_ly(data = summ_data, x = ~subtype, y = ~y, type = "bar",
                   width = 500, text = ~y, textposition = 'outside', showlegend = FALSE,
                   marker = list(color = sub_colors)) %>% 
      layout(
        title = title,
        titlefont = list(size = fs1),
        yaxis = list(title = ylabel, range = yrange, titlefont = list(size = fs1), 
                     tickfont = list(size = fs2)),
        xaxis = list(title = "", tickfont = list(size = fs2))
      )
  }
  return(fig)
}
#------------------------------------------------------------------------------#
# make bins based on array values
bins_from_array <- function(arr, nbins){
  arr_range <- range(arr)
  bmax <- arr_range[2]
  bmin <- arr_range[1]
  bins <- seq(from = bmin, to = bmax, length.out = nbins)
  return(bins)
}

#------------------------------------------------------------------------------#
# get custom bins for use in map
get_map_bins <- function(map_cat){
  if(map_cat=="total_debris"){
    bins <- seq(from = 0, to = 70000, length.out = 7)
  }
  else if(map_cat=="npermits"){
    bins <- seq(from = 0, to = 1000, length.out = 11)
  }
  else{
    bins <- seq(from = 0, to = 3600000, length.out = 9)
  }
  return(bins)
  
}

#------------------------------------------------------------------------------#
plot_total_debris_fit <- function(syn_permits_summ, debris_proj, rmspe, user_inputs, recycling){
  #' plot_total_debris_fit
  #' 
  #' Generate debris vs. year plot using exponential fit
  #'
  #' @param syn_permits_summ tibble from "shiny-data/syn_permit_summary.feather", passed through `adjust_permit_summ`
  #' @param debris_proj tibble from "shiny-data/debris_projections.feather"
  #' @param rmspe root-mean-square percent error for debris forecast
  #' @param user_inputs list containing user-defined selections
  #' @param recycling boolean indicating whether to plot recycling values
  #'
  #' @return figure

  # set up limits for plotting
  min_debris <- 0
  max_debris <- 700000
  xlabel <- "Calendar Year"
  
  cvr <- user_inputs[1] 
  ptype <- user_inputs[2]
  ccat <- user_inputs[3]
  
  # check if we're plotting total and a subset or just total
  if(all(c(cvr, ptype, ccat)=="All")){
    multi_lines <- FALSE
  }
  else{
    multi_lines <- TRUE
  }
  
  # plot historical + projection as one line; historical values only available on yearly basis
  fig <- plot_ly(width = 600, type = "scatter", mode="lines") %>% 
    add_lines(data = debris_proj, x = ~cy, y = ~fit, name = "Debris Projection", 
              line = list(color = aes_1$color, width = 3)) %>% 
    add_ribbons(data = debris_proj, x = ~cy , ymin = ~lwr, ymax = ~upr,
                fillcolor = aes_1$fillcolor, color = NA,
                showlegend = TRUE, name = "95% Confidence") %>% 
    add_markers(data = debris_proj, x = ~cy, y = ~total_debris, name = "Historical Values", 
                marker = list(color = "rgba(255,102,25,1)",size=8))
  
  # add second line for user-specified subtype
  if(multi_lines){
    filt_summ <- syn_permits_summ %>%
      filter(comm_v_res==cvr) %>%
      filter(project_type==ptype) %>%
      filter(cost_cat==ccat)
    
    fdebris <- filt_summ$frac_debris[1]
    
    filt_summ <- debris_proj %>% 
      mutate(sel_debris = fit*fdebris) %>% 
      mutate(sel_sd = sel_debris*rmspe) %>% 
      mutate(sel_lwr = sel_debris-2*sel_sd) %>% 
      mutate(sel_upr = sel_debris+2*sel_sd)

    color <- subtype_colors[str_c(cvr,ptype,sep=" ")][[1]]
    
    # get fillcolor with lower alpha
    fillcolor <- str_c(str_sub(color,1,nchar(color)-2),"0.25)",sep="")

    fig <- fig %>%
      add_lines(data = filt_summ, name = "Selected Projection", x = ~cy,
                y = ~sel_debris, line = list(color = color, width = 3)) %>%
      add_ribbons(data = filt_summ, x = ~cy, ymin = ~sel_lwr, ymax = ~sel_upr,
                  showlegend = FALSE, fillcolor = fillcolor, color = NA,
                  name = "95% Confidence") 
  }
  if(recycling){
    fig <- fig %>%
      add_markers(data = debris_proj, x = ~cy, y = ~recycling, name = "Recycling",
                  marker = list(color = "rgba(0,179,0,1)",size=8))
  }

  m <- debris_proj %>% 
    filter(cy==2020)
  a <- list(
    x = m$cy-0.2,
    y = m$total_debris,
    text = "COVID; Mar. '20 Tornado",
    font = list(size = fs2),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    textangle = 0,
    ax = -100,
    ay = 0,
    arrowcolor = "black",
    arrowsize = 2,
    arrowwidth = 1,
    arrowhead = 1
  )
  
  fig <- fig %>%
    layout(
      annotations = a,
      title = "Total Debris Generated",
      titlefont = list(size = fs1),
      yaxis = list(title = "Total Debris (Tons)", range = c(min_debris, max_debris),
                   titlefont = list(size = fs1), tickfont = list(size = fs2)),
      xaxis = list(title = xlabel, tickangle = 0, titlefont = list(size = fs1), tickfont = 
                     list(size = fs2)),
      legend = list(x=0.0, y=0.95, font = list(size = fs2))
    )

  return(fig)
}

#------------------------------------------------------------------------------#
adjust_permit_summ <- function(syn_permits_orig){
  #' adjust_permit_summ
  #'
  #' Adjust synthetic permits summary data to get fractional debris predictions
  #' for various subtypes.
  #' 
  #' The file "shiny-data/syn_permit_summary.feather" (output in `45`) contains
  #' yearly predictions for the different subcategories of interest. These 
  #' subcategories correspond to the selectable filters on the C&D Debris tab.
  #' However, these predictions were made using a poor-performing permit-level
  #' model. We here calculate the fractional debris (e.g. comm const / total) 
  #' for each subtype and each FY. This will be scaled by the total debris 
  #' prediction from an exponential fit.
  #'
  #' @param syn_permits_summ tibble from "shiny-data/syn_permit_summary.feather"
  #'
  #' @return adjusted tibble containing fractional debris

  # filter out cols and get cy from fy
  new_summ <- syn_permits_orig %>% 
    filter(!is.na(fiscal_year)) %>% 
    select(!starts_with("q")) %>% 
    select(!fiscal_quarter) %>% 
    mutate(cy = as.numeric(str_c("20",substr(fiscal_year,4,5),sep=""))-0.5)
  
  # get df containing global debris for each year
  all_summ <- new_summ %>% 
    filter(comm_v_res=="All"&project_type=="All"&cost_cat=="All"&!is.na(fiscal_year)) %>% 
    mutate(all_debris = total_debris) %>% 
    select(fiscal_year,all_debris)
  
  # use join to get fractional debris for subtypes
  new_summ %>% 
    left_join(all_summ, by = c("fiscal_year"="fiscal_year")) %>% 
    mutate(frac_debris = total_debris/all_debris) %>% 
    group_by(comm_v_res,project_type,cost_cat) %>% 
    summarise(frac_debris = mean(frac_debris)) %>% 
    ungroup()
}

#------------------------------------------------------------------------------#
adjust_map_df <- function(df_nashmap, syn_permits_orig, debris_proj){
  #' adjust_map_df
  #' 
  #' Adjust map summary data, scaling fractional debris predictions for each 
  #' subtype to the overall, better predictions from fit to historical data.
  #' 
  #' The concept here is the same as in `adjust_permit_summ`. See the
  #' description of that function for more details.
  #'
  #' @param df_nashmap Tibble from "shiny-data/map_summary.feather"
  #' @param syn_permits_orig Tibble from "shiny-data/syn_permit_summary.feather"
  #' @param debris_proj Tibble from "shiny-data/debris_projections.feather"
  #'
  #' @return Tibble containing adjusted summary data for map.

  # get old debris totals in map (overall, not subtype)
  old_totals <- syn_permits_orig %>% 
    filter(!is.na(fiscal_year)) %>% 
    select(!starts_with("q")) %>% 
    select(!fiscal_quarter) %>% 
    mutate(fy = as.numeric(str_c("20",substr(fiscal_year,4,5),sep=""))) %>% 
    filter(comm_v_res=="All"&project_type=="All"&cost_cat=="All"&!is.na(fiscal_year)) %>% 
    select(fy,total_debris)
  
  # get multiplicative factor for each year based on fit from debris_proj
  mult_fact <- debris_proj %>% 
    filter(is.na(total_debris)) %>% 
    mutate(fy = cy+0.5) %>% 
    select(fy,fit) %>% 
    left_join(old_totals,by = "fy") %>% 
    mutate(new_over_old = fit/total_debris) %>% 
    select(fy,new_over_old)
  
  # get adjusted total_debris values for each subtype in map df
  df_nashmap %>% 
    left_join(mult_fact, by="fy") %>% 
    mutate(total_debris = total_debris*new_over_old) %>% 
    mutate(total_debris_mean = total_debris_mean*new_over_old)
}

#------------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  # Load forecast and permits
  forecast <- read_feather("shiny-data/best_forecast.feather")
  syn_permits_orig <- read_feather("shiny-data/syn_permit_summary.feather")
  df_nashmap_orig <- read_feather("shiny-data/map_summary.feather")
  
  # adjust syn_permits to get fractional values averaged over years
  syn_permits_summ <- adjust_permit_summ(syn_permits_orig)
  
  # Load projections for debris from historical fit (made in 44-2)
  debris_proj <- read_feather("shiny-data/debris_projections.feather")
  
  # Extract root-mean-square-percentage error from training
  rmspe <- debris_proj$rmspe[1]
  
  # adjust debris values in map for each subtype to scale to debris_proj
  df_nashmap <- adjust_map_df(df_nashmap_orig, syn_permits_orig, debris_proj)
  
  # prepare map
  setwd("shiny-data/shape")
  district <- readOGR(dsn = ".",
                      layer = "nashville-tn-council-districts",
                      verbose = FALSE)
  district_latlon <- spTransform(district, CRS("+proj=longlat +datum=WGS84"))
  
  #----------------------------------------------------------------------------#
  # Nashville map
  output$nashmap <- renderLeaflet({
    
    # user filters
    map_permits <- df_nashmap %>%
      filter(fy == input$map_year) %>% 
      filter(project_type == input$map_project_type) %>% 
      filter(comm_v_res == input$map_comm_v_res) %>% 
      mutate(cost_rank = dense_rank(desc(const_cost_mean)), npermits_rank = dense_rank(desc(npermits)), 
             debris_rank = dense_rank(desc(total_debris)), ave_debris_rank = dense_rank(desc(total_debris_mean)))
    
    map_cat <- switch(input$map_cat, 
                      "Total Debris" = "total_debris", 
                      "Num. Debris-Generating Permits" = "npermits", 
                      "Ave. Cost per Permit" = "const_cost_mean")
    legend_title <- switch(map_cat, 
                           "total_debris" = "Total C&D Debris (tons)", 
                           "npermits" = "Num. Debris-Generating Permits", 
                           "const_cost_mean" = "Ave. Cost per Permit ($)")

    
    # Get colors and bins based on whether user selects auto-scaling
    color_var <- simplify2array(map_permits[,map_cat])
    if(input$map_check){
      nbins <- 11
      bins <- bins_from_array(color_var, nbins = nbins)
    }
    else{
      bins <- get_map_bins(map_cat)
      nbins <- length(bins)
    }
    lbins <- bins[1:nbins-1]
    ubins <- bins[2:nbins]
    cbins <- lbins+(ubins-lbins)/2.0

    bin_labels <- str_c(as.character(round(lbins,0)), as.character(round(ubins,0)), sep = " - ")
    ton_pal <- colorNumeric("Reds", domain = c(bins[1],bins[nbins]), reverse=FALSE)
    
    d_latlon <- district_latlon[order(match(district_latlon$DistrictID, map_permits$council_dist)),]
    
    labels <- paste("<p><b> Council District ", map_permits$council_dist, " FY", input$map_year%%2000, " Forecast </b></p>",
                    "<p>", "Total Debris (tons): <b>",  signif(map_permits$total_debris, 3) , "</b>  ", map_permits$debris_rank,  "/35 </p>",
                    "<p>", "Total Debris-Generating Permits: <b>",  round(map_permits$npermits, 0) ,  "</b>   ", map_permits$npermits_rank,  "/35 </p>",
                    "<p>", "Ave. Cost per Permit: <b>$",  signif(map_permits$const_cost_mean, 3) ,  "</b>   ", map_permits$cost_rank,  "/35 </p>",
                    "<p>", "Ave. Debris per Permit (tons): <b>",  signif(map_permits$total_debris_mean, 3) ,  "</b>   ", map_permits$ave_debris_rank,  "/35 </p>",
                    sep="")
    
    leaflet(height="100%",width="100%") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder, group = "Voyager") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addLayersControl(baseGroups = c("Terrain","Voyager","Satellite","Toner Lite")) %>%
      #  setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>%
      # addMiniMap(
      #   toggleDisplay = FALSE,
      #   tiles = providers$Stamen.TonerLite
      # ) %>%
      addPolygons(data = d_latlon, weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 1, fillColor = ton_pal(color_var),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = lapply(labels, HTML), labelOptions = labelOptions(offset=c(0,0), direction="right")) %>%
      # popup = lapply(labels, HTML), popupOptions = popupOptions(autoPan = FALSE, direction="right",keepInView = TRUE)) %>% 
      # addLabelOnlyMarkers(data = centers,
      #                     lng = ~x, lat = ~y, label = ~region,
      #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
      addLegend(position = "bottomleft", title = legend_title, pal=ton_pal, values=bins, opacity=1)
    
    
  })
  
  #----------------------------------------------------------------------------#
  # Plot debris vs. time (from syn_permits)
  output$debris_plot <- renderPlotly({
    
    user_inputs <- c(input$comm_v_res, input$project_type, input$cost_range)
    recycling <- input$recycling

    debris_plot <- plot_total_debris_fit(syn_permits_summ, debris_proj, rmspe, 
                                         user_inputs, recycling)
    debris_plot
  })

  #----------------------------------------------------------------------------#
  # Debris bar plot
  output$hover_debris <- renderPlotly({
    selected_time <- event_data("plotly_click")$x
    title <- "Debris Generated by Permit Type"
    ylabel <- "Total Debris (Tons)"
    yval <- "total_debris"
    
    time <- "fiscal_year"
    ymax <- 400000
    yrange <- c(0, ymax)
    
    subtype_cols = c("comm_v_res", "project_type")
    syn_permits_hover <- syn_permits_summ %>% 
      filter(comm_v_res!="All") %>% 
      filter(project_type!="All") %>% 
      filter(cost_cat=="All")

    hover_debris_plot <- plot_debris_bar(syn_permits_hover, debris_proj, 
                                           subtype_cols, rmspe, selected_time, 
                                           title, ylabel, yval, yrange, 
                                           nround = 0)
    return(hover_debris_plot)
  })
  
  #----------------------------------------------------------------------------#
  # Plot number of permits vs time (from forecast)
  output$permit_plot <- renderPlotly({
    filtered_forecast <- tibble(forecast)
    if (input$permits_comm_v_res != "All"){
      filtered_forecast <- filtered_forecast %>%
        filter(comm_v_res == tolower(input$permits_comm_v_res))
    }
    if (input$permits_project_type != "All"){
      filtered_forecast <- filtered_forecast %>%
        filter(project_type == tolower(input$permits_project_type))
    }
    
    time <- switch(input$permits_time, "Year" = "fiscal_year", "Quarter" = "fiscal_quarter")
    permit_plot <- plot_npermit_forecast(filtered_forecast, time)
    permit_plot
  })
  
  #----------------------------------------------------------------------------#
  # Permits bar plot
  output$hover_permits <- renderPlotly({
    selected_time <- event_data("plotly_click")$x
    title <- "Debris-Generating Permits by Permit Type"
    ylabel <- "Average Number of Permits"
    yval <- "forecast"
    
    time <- switch(input$permits_time, "Year" = "fiscal_year", "Quarter" = "fiscal_quarter")
    ymax <- switch(input$permits_time, "Year" = 10000, "Quarter" = 2500)
    yrange <- c(0, ymax)
    
    subtype_cols = c("comm_v_res", "project_type")
    hover_permits_plot <- plot_permit_bar(forecast, subtype_cols, time, 
                                         selected_time, title, ylabel, yval, 
                                         yrange, nround = 0)
    return(hover_permits_plot)
  })
  
  #----------------------------------------------------------------------------#
  output$permit_thin_line <- renderUI({
    div(HTML("<hr>"))
  })

  #----------------------------------------------------------------------------#
  # Markdown explanation
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML(knit('../../data-sources.md', quiet = TRUE)))
  # })
}


shinyApp(ui = ui, server = server)
