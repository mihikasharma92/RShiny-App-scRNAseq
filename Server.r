# Define server logic for random distribution app ----
server <- function(input, output, session){

  # Load required Robj dataset1s 
  # load("/Users/sha6hg/Desktop/IPF_ShinyApp/GSE135893_v2_IPF.Robj")
  # load("/Users/sha6hg/Desktop/IPF_ShinyApp/GSE136831_v3_IPF_NoMultiplet.Robj")
  # Krasnow_int <- readRDS("/Users/sha6hg/Desktop/IPF_ShinyApp/Krasnow_Integrated.rds")
  
############################################################################################################
                                              #### TAB 1 ####
  seuratObj1 <- reactive({
    if(input$dataset1 == "Kaminski"){seuratObj1 = GSE136831_v3_IPF}
    else if(input$dataset1 == "Banovich"){seuratObj1 = GSE135893_v2_IPF}
    else if(input$dataset1 == "Krasnow"){seuratObj1 = Krasnow_int}
    })
  
  plot1_title <- reactive({
    plot1_title <- input$plot1_title
    })
  
  groupby_var1 <- reactive({
    # Kaminski
    if(input$dataset1 == "Kaminski"){
      if(input$grp_var1 == "Condition"){
        groupby_var1 = "Disease_Identity"}
      else if(input$grp_var1 == "Cell Type"){
        groupby_var1 = "Manuscript_Identity"}
    }
    # Banovich
    else if(input$dataset1 == "Banovich"){ 
      if(input$grp_var1 == "Condition"){
        groupby_var1 = "Diagnosis"}
      else if(input$grp_var1 == "Cell Type"){
        groupby_var1 = "celltype"}
    }
    # Krasnow
    else if(input$dataset1 == "Krasnow"){
      if(input$grp_var1 == "Condition"){
        groupby_var1 = "region"}
      else if(input$grp_var1 == "Cell Type"){
        groupby_var1 = "free_annotation"}
    }
  })
  
  dim_red1 <- reactive({
    if(input$dim_red1 == "pca"){dim_red1 = "pca"}
    else if(input$dim_red1 == "tsne"){dim_red1 = "tsne"}
    else if(input$dim_red1 == "umap"){dim_red1 = "umap"}
  })
  
  legend1 <- reactive({
    if(input$legend1 == "Yes"){legend1 = "Yes"}
    else if(input$legend1 == "No"){legend1 = "No"}
  })
  
  label1 <- reactive({
    if(input$label1 == "Yes"){label1 = TRUE}
    else if(input$label1 == "No"){label1 = FALSE}
  })
  
  labelSize1 <- reactive({
    labelSize1 <- input$labelSize1
  })
  
  ptSize1 <- reactive({
    ptSize1 <- input$ptSize1
  })
  
  plot1 <- reactive({
    seuratObj1 <- seuratObj1()
    groupby_var1 <- groupby_var1()
    dim_red1 <- dim_red1()
    legend1 <- legend1()
    label1 <- label1()
    labelSize1 <- labelSize1()
    ptSize1 <- ptSize1()
    plot1_title <- plot1_title()
    
    if(legend1 == "Yes")
    {DimPlot(seuratObj1, group.by = groupby_var1, label = label1, label.size = labelSize1, reduction = dim_red1, repel = T, pt.size = ptSize1) + 
        guides(color = guide_legend(override.aes = list(size=4), ncol=1)) + ggtitle(plot1_title)}
    
    else if(legend1 == "No")
    {DimPlot(seuratObj1, group.by = groupby_var1, label = label1, label.size = labelSize1, reduction = dim_red1, repel = T, pt.size = ptSize1) +
        ggtitle(plot1_title) + NoLegend()}
  })
  
  output$plot1 <- renderPlot({
    print(plot1())
  },height = 700, width = 850)
  
  output$savePlot1 <- downloadHandler(
    filename = "DimPlot.png",
    content = function(file) {
      png(file, units = "in", height = 8, width = 9, res = 600)
      print(plot1())
      dev.off()
    })    
############################################################################################################ 
  
############################################################################################################
                                                  #### TAB 2 ####
  dataset2 <- reactive({
    if(input$dataset2 == "Kaminski"){dataset2 = "Kaminski"}
    else if(input$dataset2 == "Banovich"){dataset2 = "Banovich"}
    else if(input$dataset2 == "Krasnow"){dataset2 = "Krasnow"}
  })
  
  table1 <- reactive({
    # Kaminski
    if(input$dataset2 == "Kaminski") {
      if(input$grp_var2 == "Condition"){
        file_table1 = "data/GSE136831_DEGperCondition.txt"
        }
      else if(input$grp_var2 == "Cell Type"){
        file_table1 = "data/GSE136831_CellType.txt"
        }
    }
    # Banovich
    if(input$dataset2 == "Banovich") {
      if(input$grp_var2 == "Condition"){
        file_table1 = "data/GSE135893_CellType.txt"
        }
      else if(input$grp_var2 == "Cell Type"){
        file_table1 = "data/GSE135893_CellType.txt"
        }
    }
    # Krasnow
    if(input$dataset2 == "Krasnow") {
      if(input$grp_var2 == "Condition"){
        file_table1 = "data/GSE135893_CellType.txt"
        }
      else if(input$grp_var2 == "Cell Type"){
        file_table1 = "data/GSE135893_CellType.txt"
        }
    }
    as.data.frame(read.table(file = file_table1, sep="\t", header=T))
  })
  
  output$table1 <- DT::renderDataTable({
    table1 <- table1()
    DT::datatable(table1, filter = 'top', rownames = FALSE)
  })
  
  output$saveTable1 <- downloadHandler(
    filename = "DEG_Table.txt",
    content = function(file) {
      table1 <- table1()
      write.table(table1, file = file, quote = F, col.names = T, row.names = F, sep = "\t")
    }) 
############################################################################################################

############################################################################################################
                                          #### TAB 3 ####
# Feature Plot  
  seuratObj3 <- reactive({
    if(input$dataset3 == "Kaminski"){seuratObj1 = GSE136831_v3_IPF}
    else if(input$dataset3 == "Banovich"){seuratObj1 = GSE135893_v2_IPF}
    else if(input$dataset3 == "Krasnow"){seuratObj1 = Krasnow_int}
  })
  
  plot_type <- reactive({
    plot_type <- input$plot_type
  })
  
  # plot2_title <- reactive({
  #   plot2_title <- input$plot2_title
  # })
  
  genes_list <- reactive({
    obj <- seuratObj3()
    genes_list <- unique(rownames(obj))
  })
  
 observe({
    updateSelectizeInput(session, "genes", 
                         label = "Select Genes:", 
                         choices = genes_list(), server=T)
  })
  
 groupby_var3 <- reactive({
   # Kaminski
   if(input$dataset3 == "Kaminski"){
     if(input$grp_var3 == "Condition"){
       groupby_var3 = "Disease_Identity"}
     else if(input$grp_var3 == "Cell Type")
     {groupby_var3 = "Manuscript_Identity"}
   }
   # Banovich
   else if(input$dataset3 == "Banovich"){ 
     if(input$grp_var3 == "Condition"){
       groupby_var3 = "Diagnosis"}
     else if(input$grp_var3 == "Cell Type"){
       groupby_var3 = "celltype"}
   }
   # Krasnow
   else if(input$dataset3 == "Krasnow"){
     if(input$grp_var3 == "Condition")
     {groupby_var2 = "region"}
     else if(input$grp_var3 == "Cell Type"){
       groupby_var3 = "free_annotation"}
   }
 })
  
  dim_red2 <- reactive({
    if(input$dim_red2 == "pca"){dim_red2 = "pca"}
    else if(input$dim_red2 == "tsne"){dim_red2 = "tsne"}
    else if(input$dim_red2 == "umap"){dim_red2 = "umap"}
  })
  
  label2 <- reactive({
    if(input$label2 == "Yes"){label2 = TRUE}
    else if(input$label2 == "No"){label2 = FALSE}
  })
  
  split_yn2 <- reactive({
    if(input$split2 == "Yes"){split_yn2 = "Yes"}
    else if(input$split2 == "No"){split_yn2 = "No"}
  })
  
  legend_yn2 <- reactive({
    if(input$legend2 == "Yes"){legend_yn2 = "Yes"}
    else if(input$legend2 == "No"){legend_yn2 = "No"}
  })
  
  split_var2 <- reactive({
    if(input$dataset3 == "Kaminski"){split_var2 = "Disease_Identity"}
    else if(input$dataset3 == "Banovich"){split_var2 = "Diagnosis"}
    else if(input$dataset3 == "Krasnow"){split_var2 = "region"}
  })
  
  labelSize2 <- reactive({
    labelSize2 <- input$labelSize2
  })
  
  ptSize2 <- reactive({
    ptSize2 <- input$ptSize2
  })
  
  plot2_width <- reactive({
    plot2_width <- input$plot2_width
  })
  
  plot2_height <- reactive({
    plot2_height <- input$plot2_height
  })
  
  plot2 <- reactive({
    seuratObj3 <- seuratObj3()
    groupby_var3 <- groupby_var3()
    dim_red2 <- dim_red2()
    label2 <- label2()
    split_yn2 <- split_yn2()
    legend_yn2 <- legend_yn2()
    split_var2 <- split_var2()
    labelSize2 <- labelSize2()
    ptSize2 <- ptSize2()
    # plot2_title <- plot2_title()
    plot_type <- plot_type()
    
    Idents(seuratObj3) <- groupby_var3
    DefaultAssay(seuratObj3) <- "RNA"
    
    if(plot_type == "feat_plt"){
      if(split_yn2 == "Yes"){
        FeaturePlot(seuratObj3, features = input$genes, label = label2, label.size = labelSize2, reduction = dim_red2, 
                    pt.size = ptSize2, repel = T, raster = F, split.by = split_var2, order = T) 
        # + ggtitle(plot2_title)
      }
      else if(split_yn2 == "No"){
        FeaturePlot(seuratObj3, features = input$genes, label = label2, label.size = labelSize2, reduction = dim_red2, 
                    pt.size = ptSize2, repel = T, raster = F, order = T) 
        # + ggtitle(plot2_title)
      }
    }
    
    else if(plot_type == "vln_plt"){
      if(split_yn2 == "Yes" && legend_yn2 == "Yes"){
        VlnPlot(seuratObj3, features = input$genes, pt.size = ptSize2, split.by = split_var2, group.by = groupby_var3) + 
        guides(color = guide_legend(override.aes = list(size=4), ncol=1)) 
        # + ggtitle(plot2_title)
      }
      else if(split_yn2 == "Yes" && legend_yn2 == "No"){
        VlnPlot(seuratObj3, features = input$genes, pt.size = ptSize2, split.by = split_var2, group.by = groupby_var3) +
         NoLegend()
        # + ggtitle(plot2_title)
      }
      else if(split_yn2 == "No" && legend_yn2 == "Yes"){
        VlnPlot(seuratObj3, features = input$genes, pt.size = ptSize2, group.by = groupby_var3) +
         guides(color = guide_legend(override.aes = list(size=4), ncol=1)) 
        # + ggtitle(plot2_title)
      }
      else if(split_yn2 == "No" && legend_yn2 == "No"){
        VlnPlot(seuratObj3, features = input$genes, pt.size = ptSize2, group.by = groupby_var3) +
        NoLegend() 
        # + ggtitle(plot2_title) 
      }
      
    }
    
    else if(plot_type == "dot_plt"){
      if(split_yn2 == "Yes"){
        DotPlot(seuratObj3, features = input$genes, split.by = split_var2, group.by = groupby_var3) 
        # + ggtitle(plot2_title)
      }
      else if(split_yn2 == "No"){
        DotPlot(seuratObj3, features = input$genes, group.by = groupby_var3) 
        # + ggtitle(plot2_title)
      }
    }
    
    else if(plot_type == "ridge_plt"){
      if(legend_yn2 == "No"){
        RidgePlot(seuratObj3, features = input$genes, group.by = groupby_var3) + NoLegend()
        # + ggtitle(plot2_title)
      }
      else if(legend_yn2 == "Yes"){
        RidgePlot(seuratObj3, features = input$genes, group.by = groupby_var3) + 
          guides(color = guide_legend(override.aes = list(size=4), ncol=1)) 
        # + ggtitle(plot2_title)
      }
    }
    
    else if(plot_type == "heatmap"){
      DefaultAssay(seuratObj3) <- "RNA"
      DoHeatmap(seuratObj3, features = input$genes, group.by = groupby_var3) 
      # + ggtitle(plot2_title)
    }
    
  })
  
  output$plot2 <- renderPlot({
    plot2_width <- plot2_width()
    plot2_height <- plot2_height()
    print(plot2())
  },height = plot2_height, width = plot2_width)
  
  output$savePlot2 <- downloadHandler(
    filename = "FeaturePlot.png",
    content = function(file) {
      png(file, units = "in", height = 8, width = 8, res = 600)
      print(plot2())
      dev.off()
    })

############################################################################################################
  
############################################################################################################
                                                  #### TAB 4 ####
  # Feature Plot  
  seuratObj_list <- reactive({
    seuratObj_list <- input$dataset4
  })
  
  plot_type2 <- reactive({
    plot_type2 <- input$plot_type2
  })
  
  # plot2_title <- reactive({
  #   plot2_title <- input$plot2_title
  # })
  
  genes_list2 <- reactive({
    genes_list2 <- input$genes_list2
  })
  
  # groupby_var4 <- reactive({
  #   # Kaminski
  #   if(input$dataset4 == "Kaminski"){
  #     if(input$grp_var4 == "Condition"){
  #       groupby_var4 = "Disease_Identity"}
  #     else if(input$grp_var4 == "Cell Type")
  #     {groupby_var4 = "Manuscript_Identity"}
  #   }
  #   # Banovich
  #   else if(input$dataset4 == "Banovich"){ 
  #     if(input$grp_var4 == "Condition"){
  #       groupby_var4 = "Diagnosis"}
  #     else if(input$grp_var4 == "Cell Type"){
  #       groupby_var4 = "celltype"}
  #   }
  #   # Krasnow
  #   else if(input$dataset4 == "Krasnow"){
  #     if(input$grp_var4 == "Condition")
  #     {groupby_var3 = "region"}
  #     else if(input$grp_var4 == "Cell Type"){
  #       groupby_var4 = "free_annotation"}
  #   }
  # })
  
  dim_red3 <- reactive({
    if(input$dim_red3 == "pca"){dim_red3 = "pca"}
    else if(input$dim_red3 == "tsne"){dim_red3 = "tsne"}
    else if(input$dim_red3 == "umap"){dim_red3 = "umap"}
  })
  
  label3 <- reactive({
    if(input$label3 == "Yes"){label3 = TRUE}
    else if(input$label3 == "No"){label3 = FALSE}
  })
  
  split_yn3 <- reactive({
    if(input$split3 == "Yes"){split_yn3 = "Yes"}
    else if(input$split3 == "No"){split_yn3 = "No"}
  })
  
  legend_yn3 <- reactive({
    if(input$legend3 == "Yes"){legend_yn3 = "Yes"}
    else if(input$legend3 == "No"){legend_yn3 = "No"}
  })
  
  # split_var3 <- reactive({
  #   if(input$dataset4 == "Kaminski"){split_var3 = "Disease_Identity"}
  #   else if(input$dataset4 == "Banovich"){split_var3 = "Diagnosis"}
  #   else if(input$dataset4 == "Krasnow"){split_var3 = "region"}
  # })
  
  labelSize3 <- reactive({
    labelSize3 <- input$labelSize3
  })
  
  ptSize3 <- reactive({
    ptSize3 <- input$ptSize3
  })
  
  plot3_width <- reactive({
    plot3_width <- input$plot3_width
  })
  
  plot3_height <- reactive({
    plot3_height <- input$plot3_height
  })
  
  plot3 <- reactive({
    seuratObj_list <- seuratObj_list()
    #groupby_var4 <- groupby_var4()
    dim_red3 <- dim_red3()
    label3 <- label3()
    split_yn3 <- split_yn3()
    legend_yn3 <- legend_yn3()
    #split_var3 <- split_var3()
    labelSize3 <- labelSize3()
    ptSize3 <- ptSize3()
    # plot2_title <- plot2_title()
    plot_type2 <- plot_type2()
    
    plot_list = list()
    
    for(seuratObj4_key in seuratObj_list){
      
      if(seuratObj4_key == "Kaminski"){
        if(input$grp_var4 == "Condition"){
          groupby_var4 = "Disease_Identity"}
        else if(input$grp_var4 == "Cell Type")
        {groupby_var4 = "Manuscript_Identity"}
      }
      # Banovich
      else if(seuratObj4_key == "Banovich"){ 
        if(input$grp_var4 == "Condition"){
          groupby_var4 = "Diagnosis"}
        else if(input$grp_var4 == "Cell Type"){
          groupby_var4 = "celltype"}
      }
      # Krasnow
      else if(seuratObj4_key == "Krasnow"){
        if(input$grp_var4 == "Condition")
        {groupby_var3 = "region"}
        else if(input$grp_var4 == "Cell Type"){
          groupby_var4 = "free_annotation"}
      }
      
      if(seuratObj4_key == "Kaminski"){split_var3 = "Disease_Identity"}
      else if(seuratObj4_key == "Banovich"){split_var3 = "Diagnosis"}
      else if(seuratObj4_key == "Krasnow"){split_var3 = "region"}
      
      seuratObj4 <- get(Object_list[[seuratObj4_key]])
      
      print(seuratObj4_key)
      print(seuratObj4)
    
      Idents(seuratObj4) <- groupby_var4
      DefaultAssay(seuratObj4) <- "RNA"
      
      if(plot_type2 == "feat_plt"){
        if(split_yn3 == "Yes"){
          p1 <- FeaturePlot(seuratObj4, features = input$genes2, label = label3, label.size = labelSize3, reduction = dim_red3, 
                      pt.size = ptSize3, repel = T, raster = F, split.by = split_var3, order = T) 
          # + ggtitle(plot2_title)
        }
        else if(split_yn3 == "No"){
          p1 <- FeaturePlot(seuratObj4, features = input$genes2, label = label3, label.size = labelSize3, reduction = dim_red3, 
                      pt.size = ptSize3, repel = T, raster = F, order = T) 
          # + ggtitle(plot2_title)
        }
      }
      
      else if(plot_type2 == "vln_plt"){
        if(split_yn3 == "Yes" && legend_yn3 == "Yes"){
          p1 <- VlnPlot(seuratObj4, features = input$genes2, pt.size = ptSize3, split.by = split_var3, group.by = groupby_var4) + 
            guides(color = guide_legend(override.aes = list(size=4), ncol=1)) 
          # + ggtitle(plot2_title)
        }
        else if(split_yn3 == "Yes" && legend_yn3 == "No"){
          p1 <- VlnPlot(seuratObj4, features = input$genes2, pt.size = ptSize3, split.by = split_var3, group.by = groupby_var4) +
            NoLegend()
          # + ggtitle(plot2_title)
        }
        else if(split_yn3 == "No" && legend_yn3 == "Yes"){
          p1 <- VlnPlot(seuratObj4, features = input$genes2, pt.size = ptSize3, group.by = groupby_var4) +
            guides(color = guide_legend(override.aes = list(size=4), ncol=1)) 
          # + ggtitle(plot2_title)
        }
        else if(split_yn3 == "No" && legend_yn3 == "No"){
          p1 <- VlnPlot(seuratObj4, features = input$genes2, pt.size = ptSize3, group.by = groupby_var4) +
            NoLegend() 
          # + ggtitle(plot2_title) 
        }
        
      }
      
      else if(plot_type2 == "dot_plt"){
        if(split_yn3 == "Yes"){
          p1 <- DotPlot(seuratObj4, features = input$genes2, split.by = split_var3, group.by = groupby_var4) 
          # + ggtitle(plot2_title)
        }
        else if(split_yn3 == "No"){
          p1 <- DotPlot(seuratObj4, features = input$genes2, group.by = groupby_var3) 
          # + ggtitle(plot2_title)
        }
      }
      
      else if(plot_type2 == "ridge_plt"){
        if(legend_yn3 == "No"){
          p1 <- RidgePlot(seuratObj4, features = input$genes2, group.by = groupby_var4) + NoLegend()
          # + ggtitle(plot2_title)
        }
        else if(legend_yn3 == "Yes"){
          p1 <- RidgePlot(seuratObj4, features = input$genes2, group.by = groupby_var4) + 
            guides(color = guide_legend(override.aes = list(size=4), ncol=1)) 
          # + ggtitle(plot2_title)
        }
      }
      
      else if(plot_type2 == "heatmap"){
        DefaultAssay(seuratObj4) <- "RNA"
        p1 <- DoHeatmap(seuratObj4, features = input$genes2, group.by = groupby_var4) 
        # + ggtitle(plot2_title)
      }
      
      print(p1)
      plot_list[[seuratObj4_key]] <- p1
    }
    
    plot3_height <- plot3_height()
    plot3_width <- plot3_width()
    
    grid.arrange(grobs = plot_list, ncol = 2)
    print(grid.arrange(grobs = plot_list, ncol = length(seuratObj_list)))
    
  })
  
  output$plot3 <- renderPlot({
    print(plot3())
    plot3_height <- plot3_height()
    plot3_width <- plot3_width()
  },height = plot3_height, width = plot3_width, execOnResize = T)
  
  output$savePlot3 <- downloadHandler(
    filename = "Comparison_Plots.png",
    content = function(file) {
      png(file, units = "in", height = 8, width = 8, res = 600)
      print(plot3())
      dev.off()
    })
  
############################################################################################################
  
}
