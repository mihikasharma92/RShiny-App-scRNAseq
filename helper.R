library(Seurat)


load("/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE135893_v2_IPF.Robj")
load("/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_v3_IPF_NoMultiplet.Robj")
Krasnow_int <- readRDS("/Users/sha6hg/Desktop/IPF_ShinyApp/data/Krasnow_Integrated.rds")

# Kam_Celltype <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_CellType.txt", sep = "\t", header = T, row.names = F))
# Kam_Cond <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# Kam_CelltypeCond <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# Kam_Cluster <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# 
# Bano_Celltype <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# Bano_Cond <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# Bano_CelltypeCond <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# Bano_Cluster <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# 
# Kras_Celltype <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))
# Kras_Cluster <- as.data.frame(read.table(file = "/Users/sha6hg/Desktop/IPF_ShinyApp/data/GSE136831_DEGperCondition.txt", sep = "\t", header = T, row.names = F))

gene_list2 <- Reduce(intersect, list(rownames(GSE135893_v2_IPF),rownames(GSE136831_v3_IPF),rownames(Krasnow_int)))