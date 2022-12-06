library(shiny)
library(Seurat)
library(waiter)
library(shinyjs)
library(ggplot2)
library(DT)
library(shinycustomloader)
library(gridExtra)
library(grid)

source("helper.R")
source("global.R")

# Define UI for IPF Datasets Application ----
ui <- fluidPage(
  
  # Colour for overall background
  tags$style(HTML("body {
        background-color: #FFF5EE;}"
                  )),
  
  # Title and Title colour
  titlePanel(h1("IPF Dataset Explorer",style="font-size:40px;color:#D2691E;font-weight: bold;"),windowTitle = "IPF Dataset Explorer"),
  
  # Colour Sidebar Panel
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #FAEBD7; font-size:15px; color:black;
        }

        body, label, input, button, select { 
          font-family: "Arial";color:black;
        }')
  )),
  
  # Colour for individual tabs
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #DEB887;  color:black; font-size:15px}
    .tabbable > .nav > li > a[data-value='Dimension Reduction'] {background-color: #FFF8DC; color:black; font-size:15px}
    .tabbable > .nav > li > a[data-value='Differential Markers'] {background-color:#FFEBCD; color:black; font-size:15px}
    .tabbable > .nav > li > a[data-value='Gene Expression Visualization'] {background-color: #FFE4C4; color:black; font-size:15px}
    .tabbable > .nav > li > a[data-value='Dataset Comparison'] {background-color: #FFE4C4; color:black; font-size:15px}
    .tabbable > .nav > li[class=active]    > a {background-color: #CD853F; color:black;font-weight: bold; font-size:15px}"
  )),
  
  sidebarLayout(position = "left",
                # Sidebar panel for inputs 
                sidebarPanel(id = "sidebar", width = 3,
                             br(),
                             # Conditional Sidebar Panel:
                             # https://stackoverflow.com/questions/44540683/tabs-with-different-sidebars
                             ###################################### PANEL 1 ######################################
                             conditionalPanel(condition = "input.tabs1==1",
                                              selectInput("dataset1", 
                                                          label = "Dataset", 
                                                          choices = c("Kaminski", "Banovich" , "Krasnow"),
                                                          selected = "Kaminski"),
                                              textInput("plot1_title", 
                                                        label = "Plot Title", 
                                                        value = "Dim Plot"),
                                              selectInput("dim_red1", 
                                                          label = "Dimension Reduction:", 
                                                          choices = c("tSNE"="tsne", "UMAP"="umap", "PCA"="pca"), 
                                                          selected = "UMAP"),
                                              selectInput("grp_var1", 
                                                          label = "Grouping By:", 
                                                          choices = c("Condition", "Cell Type"), 
                                                          selected = "Cell Type"),
                                              radioButtons("legend1", 
                                                           label = "Legends:",
                                                           c("Yes", "No"), inline = T),
                                              radioButtons("label1", 
                                                           label = "Labels:",
                                                           c("Yes", "No"), inline = T), 
                                              sliderInput("labelSize1", 
                                                          label = "Label Size:",
                                                          min = 1, max = 10, value = 1),
                                              sliderInput("ptSize1", 
                                                          label = "Point Size:",
                                                          min = 0.1, max = 1, value = 0.2)),
                             
                             ###################################### PANEL 2 ######################################
                             conditionalPanel(condition = "input.tabs1==2",
                                              selectInput("dataset2", 
                                                          label = "Dataset", 
                                                          choices = c("Kaminski", "Banovich" , "Krasnow"),
                                                          selected = "Kaminski"),
                                              selectInput("grp_var2", 
                                                          label = "Grouping By:", 
                                                          choices = c("Seurat Clusters", "Condition", "Cell Type"), 
                                                          selected = "Seurat Clusters")),
                             
                             ###################################### PANEL 3 ######################################
                             conditionalPanel(condition = "input.tabs1==3",
                                              selectInput("dataset3", 
                                                          label = "Dataset", 
                                                          choices = c("Kaminski", "Banovich" , "Krasnow"),
                                                          selected = "Kaminski"),
                                              selectInput("plot_type", 
                                                          label = "Plot Type", 
                                                          choices = c("Feature Plot" = "feat_plt", "Violin Plot" = "vln_plt" , "Dot Plot" = "dot_plt", "Ridge Plot" = "ridge_plt", "Heatmap" = "heatmap"),
                                                          selected = "Feature Plot"),
                                              # conditionalPanel(condition = "input.plot_type == 'feat_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Visualization Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'vln_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Violin Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'dot_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Dot Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'ridge_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Ridge Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'heatmap'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Heatmap")),
                                              
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt' || input.plot_type == 'vln_plt' || input.plot_type == 'dot_plt' || input.plot_type == 'ridge_plt' || input.plot_type == 'heatmap'",
                                                               selectInput("genes", 
                                                                           label = "Select Genes:", 
                                                                           choices = NULL,
                                                                           selected = NULL,
                                                                           multiple = T,
                                                                           selectize = TRUE)),
                                              
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt'",
                                                               selectInput("dim_red2", 
                                                                           label = "Dimension Reduction:", 
                                                                           choices = c("tSNE"="tsne", "UMAP"="umap", "PCA"="pca"), 
                                                                           selected = "UMAP")),
                                              
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt' || input.plot_type == 'vln_plt' || input.plot_type == 'dot_plt' || input.plot_type == 'ridge_plt' || input.plot_type == 'heatmap'",
                                                               selectInput("grp_var3", 
                                                                           label = "Grouping By:", 
                                                                           choices = c("Condition", "Cell Type", "Seurat Clusters"), 
                                                                           selected = "Cell Type")),
                                              
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt' || input.plot_type == 'vln_plt' || input.plot_type == 'dot_plt'",
                                                               radioButtons("split2", 
                                                                            label = "Split by Condition",
                                                                            c("Yes", "No"), inline = T)), 
                                              
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt'",
                                                               radioButtons("label2", 
                                                                            label = "Labels:",
                                                                            c("Yes", "No"), inline = T), 
                                                               sliderInput("labelSize2", 
                                                                           label = "Label Size:",
                                                                           min = 1, max = 10, value = 3)),
                                              
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt' || input.plot_type == 'vln_plt' || input.plot_type == 'ridge_plt'",
                                                               radioButtons("legend2", 
                                                                            label = "Add Legend:",
                                                                            c("Yes", "No"), inline = T)),
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt'",
                                                               sliderInput("ptSize2", 
                                                                           label = "Point Size:",
                                                                           min = 0.1, max = 1, value = 0.5)),
                                              conditionalPanel(condition = "input.plot_type == 'feat_plt' || input.plot_type == 'vln_plt' || input.plot_type == 'dot_plt' || input.plot_type == 'ridge_plt' || input.plot_type == 'heatmap'",
                                                               sliderInput("plot2_width", 
                                                                           label = "Width:",
                                                                           min = 500, max = 5000, value = 700),
                                                               sliderInput("plot2_height", 
                                                                           label = "Height:",
                                                                           min = 500, max = 5000, value = 700))),
                                             
                             ###################################### PANEL 4 ######################################
                             conditionalPanel(condition = "input.tabs1==4",
                                              selectInput("dataset4", 
                                                          label = "Dataset", 
                                                          choices = c("Kaminski", "Banovich" , "Krasnow"),
                                                          selected = "Kaminski",
                                                          multiple = T),
                                              selectInput("plot_type2", 
                                                          label = "Plot Type", 
                                                          choices = c("Feature Plot" = "feat_plt", "Violin Plot" = "vln_plt" , "Dot Plot" = "dot_plt", "Ridge Plot" = "ridge_plt", "Heatmap" = "heatmap"),
                                                          selected = "Feature Plot"),
                                              # conditionalPanel(condition = "input.plot_type == 'feat_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Visualization Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'vln_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Violin Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'dot_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Dot Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'ridge_plt'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Ridge Plot")),
                                              # conditionalPanel(condition = "input.plot_type == 'heatmap'",
                                              #                  textInput("plot2_title", 
                                              #                            label = "Plot Title", 
                                              #                            value = "Heatmap")),
                                              
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt' || input.plot_type2 == 'vln_plt' || input.plot_type2 == 'dot_plt' || input.plot_type2 == 'ridge_plt' || input.plot_type2 == 'heatmap'",
                                                               selectInput("genes2", 
                                                                           label = "Select Genes:", 
                                                                           choices = gene_list2,
                                                                           selected =  gene_list2[1],
                                                                           multiple = T,
                                                                           selectize = TRUE)),
                                              
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt'",
                                                               selectInput("dim_red3", 
                                                                           label = "Dimension Reduction:", 
                                                                           choices = c("tSNE"="tsne", "UMAP"="umap", "PCA"="pca"), 
                                                                           selected = "UMAP")),
                                              
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt' || input.plot_type2 == 'vln_plt' || input.plot_type2 == 'dot_plt' || input.plot_type2 == 'ridge_plt' || input.plot_type2 == 'heatmap'",
                                                               selectInput("grp_var4", 
                                                                           label = "Grouping By:", 
                                                                           choices = c("Condition", "Cell Type", "Seurat Clusters"), 
                                                                           selected = "Cell Type")),
                                              
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt' || input.plot_type2 == 'vln_plt' || input.plot_type2 == 'dot_plt'",
                                                               radioButtons("split3", 
                                                                            label = "Split by Condition",
                                                                            c("Yes", "No"), inline = T)), 
                                              
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt'",
                                                               radioButtons("label3", 
                                                                            label = "Labels:",
                                                                            c("Yes", "No"), inline = T), 
                                                               sliderInput("labelSize3", 
                                                                           label = "Label Size:",
                                                                           min = 1, max = 10, value = 3)),
                                              
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt' || input.plot_type2 == 'vln_plt' || input.plot_type2 == 'ridge_plt'",
                                                               radioButtons("legend3", 
                                                                            label = "Add Legend:",
                                                                            c("Yes", "No"), inline = T)),
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt'",
                                                               sliderInput("ptSize3", 
                                                                           label = "Point Size:",
                                                                           min = 0.1, max = 1, value = 0.5)),
                                              conditionalPanel(condition = "input.plot_type2 == 'feat_plt' || input.plot_type2 == 'vln_plt' || input.plot_type2 == 'dot_plt' || input.plot_type2 == 'ridge_plt' || input.plot_type2 == 'heatmap'",
                                                               sliderInput("plot3_width", 
                                                                           label = "Width:",
                                                                           min = 500, max = 5000, value = 700),
                                                               sliderInput("plot3_height", 
                                                                           label = "Height:",
                                                                           min = 500, max = 5000, value = 700)))
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Tabset w/ plot, summary, and table ----
                  tabsetPanel(id = "tabs1",
                              tabPanel("Dimension Reduction", value =1, br(),
                                       column(width = 11, align="center",
                                              # https://stackoverflow.com/questions/47261269/downloadhander-save-plot-for-basic-plot-in-shiny
                                              downloadButton("savePlot1", label = "Save Plot"),
                                              HTML("<br>"),
                                              withLoader(plotOutput("plot1"), type="image", loader="LungBreathing1.gif", proxy.height = 600))),
                                              #plotOutput("plot1"))),
                              
                              tabPanel("Differential Markers", value = 2, br(),
                                       column(width = 12, align="center", 
                                              downloadButton("saveTable1",label = "Save Table"), 
                                              HTML("<br>"),
                                              #withLoader(DT::dataTableOutput("table1"), type = "image", loader = "LoadingGIF2.gif"))),
                                              DT::dataTableOutput("table1"))),
                              
                              tabPanel("Gene Expression Visualization", value = 3, br(),
                                       column(width = 11, align="center",
                                              downloadButton("savePlot2", label = "Save Plot"),
                                              HTML("<br>"),
                                              #downloadButton("savePlot3", label = "Save Multiple Plots"),
                                              withLoader(plotOutput("plot2"), type="image", loader="LungBreathing2.gif", proxy.height = 600))),
                                              #plotOutput("plot2")))
                              
                              tabPanel("Dataset Comparison", value = 4, br(),
                                       column(width = 11, align="center",
                                              downloadButton("savePlot3", label = "Save Plot"),
                                              HTML("<br>"),
                                              #downloadButton("savePlot3", label = "Save Multiple Plots"),
                                              withLoader(plotOutput("plot3"), type="image", loader="LungBreathing2.gif", proxy.height = 600)))
                                              #plotOutput("plot2")))
                              )
                  )
                )
  )

