library(tidyverse)
library(MDAtoolkits)
library(ShinyWGCNA)
library(tidymass)
library(WGCNA)
library(treeio)
library(ggtree)
library(ggdendro)
library(plotly)
library(crayon)
library(progressr)
library(ggprism)
library(patchwork)
library(circlize)
library(ComplexHeatmap)
load("~/GeekSpace/拟靶课题/F-box_code/03.progress/02.normalized/obj.serrf.rds")
trait <- read.delim("~/GeekSpace/拟靶课题/F-box_code/03.progress/WGCNA/category2.txt")
expmat <- 
  object.serrf %>% 
  activate_mass_dataset("sample_info") %>% 
  filter(class == "Subject") %>% 
  extract_expression_data() %>% 
  rownames_to_column("ID")

oneStepNetwork <- function(
    exp.mat,
    lazy_modle = TRUE,save_fig = T,F.path = "./",
    datatype = 'peak area',RcCutoff = 1,samplePerc = 0.3,method = 'raw',
    GeneNumCut = 0, cutmethod = "MAD",
    rscut = 0.8,
    minModuleSize = 30,mergeCutHeight = 0.25,maxBlocksize = 999999  
) {
  ##> setting 
  if(isTRUE(save_fig)) {
    if(str_detect(F.path,pattern = "/$")){
      out_path = paste0(F.path,"WGCNA_out/")
    } else {
      out_path = paste0(F.path,"/WGCNA_out/")
    }
    dir.create(paste0(out_path),showWarnings = F,recursive = T)
    message("Your result will export at: ",out_path)
  }
  msg_yes = green$bold$italic;
  msg_no = red$bold$italic;
  msg_warning = yellow$bold$italic;
  if(isTRUE(lazy_modle)){
    message(msg_warning("The WGCNA is running under lazy_modle. \nNote that under lazy_modle, the entire WGCNA process will run with default parameters. \nThis means there is a chance of overlooking issues such as sample heterogeneity and the appropriateness of the power value."))
  }
  message(msg_yes("==========================================\n"))
  message(msg_yes("Step1. Gene or Protein Expression / Metabolites Accumulation Matrix Filtring \n"))
  message(msg_yes("==========================================\n"))
  Step1 <- getdatExpr(rawdata = exp.mat,RcCutoff = RcCutoff,samplePerc = samplePerc,datatype = datatype,method = method)
  Step2 <- getdatExpr2(datExpr = Step1,GeneNumCut = GeneNumCut,cutmethod = cutmethod)
  Step3 <- getsampleTree(datExpr = Step2)
  if(isTRUE(lazy_modle)) {
    Step4 = Step2
    outlier <- NA
  } else {
    ggdendrogram(Step3$sampleTree) %>% ggplotly() %>% print()
    cat("Enter Sample_ID as a vector (e.g., c('x','b','a','d')):\n")
    outlier <- scan(what = character(), n = 0, quiet = TRUE) 
    if(length(outlier) > 0) {
      Step1 <- Step1 %>% select(-all_of(outlier))
      Step2 <- getdatExpr2(datExpr = Step1,GeneNumCut = GeneNumCut,cutmethod = cutmethod)
      msg_yes(paste(outlier, "removed from raw dataset!\n"))
      Step3 <- getsampleTree(datExpr = Step2)
      ggdendrogram(Step3$sampleTree) %>% 
        ggplotly()  %>%
        print()
      Step4 = Step2
    } else {
      Step4 <- Step2
      message(msg_warning("All samples reserved for next steps! Be careful of outliers!"))
    }
  }
  if(isTRUE(save_fig)) {
    pdf(file = paste0(out_path,"SampleCluster.pdf"),width = 30,height = 6)
    ggdendrogram(Step3$sampleTree) %>% print()
    dev.off()
    write.tree(phy = Step3$tree,file = paste0(out_path,"SampleCluster.nwk"))
  }
  message(msg_yes("==========================================\n"))
  message(msg_yes("Step2. Power Value Selection and Scale-Free Network Verification  \n"))
  message(msg_yes("==========================================\n"))
  Step5 <- getpower(datExpr = Step4,rscut = rscut)
  if (isTRUE(lazy_modle)) {
    power_rec <- Step5$power
  } else {
    Step5$plot %>% print
    message(msg_yes(paste0("Default SFT cutoff: ",rscut)))
    message(msg_yes(paste0("The recommended power: ",Step5$power)))
    power_rec <- readline("Choose power: ") %>% as.numeric()
    if(power_rec =="") {
      power_rec = Step5$power
    }
  }
  
  if(isTRUE(save_fig)) {
    pdf(file = paste0(out_path,"PowerSelection.pdf"),width = 11,height = 6)
    Step5$plot %>% print()
    dev.off()
  }
  
  Step6 = powertest(power.test = power_rec,datExpr = Step4,nGenes = ncol(Step4))
  Step6 %>% print()
  if(isTRUE(save_fig)) {
    pdf(file = paste0(out_path,"ScaleFreeCheck.pdf"),width = 11,height = 6)
    Step6 %>% print()
    dev.off()
  }
  message(msg_yes("==========================================\n"))
  message(msg_yes("Step3. Construction network  \n"))
  message(msg_yes("==========================================\n"))
  if(maxBlocksize == 999999){
    maxBlocksize = ncol(Step4)
  }
  Step7 = getnetwork(datExpr = Step4,power = power_rec,minModuleSize = minModuleSize,mergeCutHeight = mergeCutHeight,maxBlocksize = maxBlocksize )
  plotDendroAndColors(Step7$net$dendrograms[[1]], Step7$moduleColors[Step7$net$blockGenes[[1]]],
                      "Module colors",
                      dendroLabels = FALSE, hang = 0.03,
                      addGuide = TRUE, guideHang = 0.05)
  if(isTRUE(save_fig)) {
    pdf(file = paste0(out_path,"ClusterDendrogram.pdf"),width = 10,height = 6)
    plotDendroAndColors(Step7$net$dendrograms[[1]], Step7$moduleColors[Step7$net$blockGenes[[1]]],
                        "Module colors",
                        dendroLabels = FALSE, hang = 0.03,
                        addGuide = TRUE, guideHang = 0.05)
    dev.off()
  }
  out = list(
    expmat = Step4,
    power = power_rec,
    net = Step7,
    outlier = outlier
  )
  return(out)
}


oneStepWGCNA <- function(
    exp.mat,
    lazy_modle = TRUE, save_fig = T, F.path = "./", iterative = T, grey_cut = 10,
    datatype = 'peak area', RcCutoff = 1, samplePerc = 0.3, method = 'raw',
    GeneNumCut = 0, cutmethod = "MAD",
    rscut = 0.8,
    minModuleSize = 30, mergeCutHeight = 0.25, maxBlocksize = 999999,
    r_cutoff = 0.8,hub_model = "GS+MM",
    pheofile
) {
  ##> 01. Parameters 
  msg_yes <- green$bold$italic;
  msg_no <- red$bold$italic;
  msg_warning <- yellow$bold$italic;
  ##> 02. Set default parameters
  expmat_new <- NULL
  grey_num <- NA
  round_x <- 0
  tmp_network <- NULL
  ##> 03. Output file path
  if(isTRUE(save_fig)) {
    if(str_detect(F.path,pattern = "/$")){
      out_path = paste0(F.path,"WGCNA_out/")
    } else {
      out_path = paste0(F.path,"/WGCNA_out/")
    }
    dir.create(paste0(out_path),showWarnings = F,recursive = T)
    message("Your result will export at: ",out_path)
  }
  ##> 04. run wgcna
  ##> 4.1 iterative
  if(isTRUE(iterative)) {
    while (is.na(grey_num) || grey_num > grey_cut ) {
      Sys.sleep(time = 2)
      round_x <- round_x + 1
      message(msg_yes(paste0("iterative WGCNA round ", round_x)))
      if (is.null(expmat_new)) {
        expmat_iteract <- exp.mat
      } else {
        expmat_iteract <- expmat_new
      }
      ##> constraction network
      tmp_network <- suppressMessages(
        oneStepNetwork(
          exp.mat = expmat_iteract,
          lazy_modle = lazy_modle,
          save_fig = save_fig,
          F.path = paste0(F.path, "round_", round_x),
          method = method,
          datatype = datatype,
          RcCutoff = RcCutoff,
          samplePerc = samplePerc,
          GeneNumCut = GeneNumCut,
          cutmethod = cutmethod,
          rscut = rscut,
          minModuleSize = minModuleSize,
          mergeCutHeight = mergeCutHeight,
          maxBlocksize = maxBlocksize
        )
      )
      ##> check unclassified network
      if("grey" %in% (tmp_network$net$Gene2module %>% pull(Module))) {
        grey_num <- tmp_network$net$Gene2module %>%
          group_by(Module) %>%
          summarise(n = n()) %>%
          filter(Module == "grey") %>%
          pull(n)
      } else {
        grey_num  = 0
      }
      ##> filter genes
      GID_rest <- data.frame(GID = tmp_network$net$Gene2module %>%
                               filter(Module != "grey") %>%
                               pull(GID))
      ##> expression matrix 
      colnames(exp.mat)[1] <- "GID"
      if(round_x == 1) {
        exp.mat2 <- exp.mat
      } else {
        exp.mat2 <- expmat_new
      }
      ##> generate expmat matrix
      expmat_new <- GID_rest %>% left_join(exp.mat2, by = "GID")
      if(length(tmp_network$outlier) > 0) {
        expmat_new <- expmat_new %>% select(-all_of(tmp_network$outlier))
      }
      message(msg_warning(paste0("Grey module num:", grey_num)))
    }
  }
  ##> 4.2 tmp_network
  tmp_network <- suppressMessages(
    oneStepNetwork(
      exp.mat = exp.mat,
      lazy_modle = lazy_modle,
      save_fig = save_fig,
      F.path = paste0(F.path),
      method = method,
      datatype = datatype,
      RcCutoff = RcCutoff,
      samplePerc = samplePerc,
      GeneNumCut = GeneNumCut,
      cutmethod = cutmethod,
      rscut = rscut,
      minModuleSize = minModuleSize,
      mergeCutHeight = mergeCutHeight,
      maxBlocksize = maxBlocksize
    )
  )
  
  ##> 05. Run module trait
  #return(tmp_network)
  message(msg_yes("==========================================\n"))
  message(msg_yes("Module trait  \n"))
  message(msg_yes("==========================================\n"))
  ##> 5.1 parameters
  MEs_col = tmp_network$net$MEs_col
  moduleColors = tmp_network$net$moduleColors
  Gene2Module = tmp_network$net$Gene2module
  datExpr = tmp_network$expmat
  nSamples = nrow(datExpr)
  nGenes = ncol(datExpr)
  corType = "pearson"
  power = tmp_network$power
  ##> 5.2 generate phenotype file
  if (ncol(pheofile) == 2) {
    phenotype <- 
    pheofile %>% 
      setNames(c("ID","Class")) %>% 
      mutate(num = 1) %>% 
      pivot_wider(names_from = Class,values_from = num,values_fill = 0) %>% 
      column_to_rownames("ID")
  } else {
    phenotype <- 
      pheofile %>% 
      rename("ID" = colnames(.)[1]) %>% 
      column_to_rownames("ID")
  }
  ##> 5.3 fix phenotype file
  phenotype = phenotype[match(rownames(tmp_network$expmat),rownames(phenotype)),]
  ##> 6.0 Generate trait-module heatmap
  res_kme <- getKME(datExpr = datExpr,moduleColors = moduleColors,
                    MEs_col = MEs_col)
  res_MM <- getMM(datExpr = datExpr,MEs_col = MEs_col,nSamples = nSamples,corType = corType)
  res_GS <- getMt(phenotype = phenotype,nSamples = nSamples,moduleColors = moduleColors,datExpr = datExpr)
  modTraitCor = res_GS$modTraitCor
  mod_color = gsub(pattern = "^..",replacement = "",rownames(modTraitCor))
  mod_color_anno = setNames(mod_color,rownames(modTraitCor))
  
  Left_anno = rowAnnotation(
    Module = rownames(modTraitCor),
    col = list(
      Module = mod_color_anno
    ),
    show_legend = F,
    show_annotation_name = F
  )
  ht =
    Heatmap(
      matrix = modTraitCor,
      cluster_rows = F, cluster_columns = F,
      left_annotation = Left_anno,
      cell_fun = function(j,i,x,y,width,height,fill) {
        grid.text(sprintf(res_GS$textMatrix[i,j]),x,y,gp = gpar(fontsize = 12))
      },
      row_names_side = "left",
      column_names_rot = 90,
      heatmap_legend_param = list(
        at = c(-1,-0.5,0,0.5, 1),
        labels = c("-1","-0.5", "0","0.5", "1"),
        title = "",
        legend_height = unit(9, "cm"),
        title_position = "lefttop-rot"
      ),
      rect_gp = gpar(col = "black", lwd = 1.2),
      column_title = "Module-trait relationships",
      column_title_gp = gpar(fontsize = 15, fontface = "bold"),
      col = colorRamp2(c(-1, 0, 1), c("blue","white","red"))
    )
  draw(ht) %>% print()
  if(isTRUE(save_fig)) {
    pdf(file = paste0(out_path,"Module_trait.pdf"),width = 10,height = 10)
    draw(ht) %>% print()%>% print()
    dev.off()
  }
  ##> 7.0 output MM vs GS
  ##> 7.1 filter sig module-trait
  res_GS.r_long <- 
  res_GS$modTraitCor %>%
    as.data.frame() %>% 
    rownames_to_column("MODULE") %>% 
    pivot_longer(!MODULE,names_to = "Cluster",values_to = "r") 
  res_GS.p_long <- 
    res_GS$modTraitP %>%
    as.data.frame() %>% 
    rownames_to_column("MODULE") %>% 
    pivot_longer(!MODULE,names_to = "Cluster",values_to = "p") 
  res_GS_sig <- left_join(res_GS.r_long,res_GS.p_long) %>% 
    filter(p < 0.05 & r > r_cutoff)
  r.max <- res_GS.r_long %>% pull(r) %>% max
  if(ncol(res_GS_sig) == 0) {
    message(msg_no("No significant module-trait relationships detected under r cutoff: " r_cutoff,"\n",
                   "The max r value is: "),r.max)
  } else {
    purrr::map2(.x = res_GS_sig %>% pull(MODULE),.y = res_GS_sig %>% pull(Cluster),.f = function(.x,.y){
      mod <- .x %>% str_remove("ME")
      pdf(file = paste0(out_path,mod,"_modHeatEig.pdf"),width = 10,height = 5)
      moduleheatmap(datExpr = datExpr,MEs = MEs_col,which.module = mod,moduleColors = moduleColors) %>% print
      dev.off()
      pdf(file = paste0(out_path,mod,"-",.y,"_verboseplot.pdf"),width = 10,height = 5)
      getverboseplot(datExpr = datExpr,module = mod,pheno = .y,MEs = MEs_col,
                     traitData = phenotype,moduleColors = moduleColors,
                     geneModuleMembership = res_MM$MM,nSamples = nSamples) %>% print()
      dev.off()
    })
    KeyModule_Trait <- purrr::map2_dfr(.x,.y,.f = function(.x,.y) {
      mod <- .x %>% str_remove("ME")
      gsvsmm <-
        hubgenes(datExpr = datExpr,
                 mdl = mod,
                 power = power,
                 trt = .y,
                 KME = res_kme,
                 GS.cut = 0,
                 kME.cut = 0,
                 datTrait = phenotype,
                 g2m = Gene2Module
        )
      out <- 
        gsvsmm$hub3 %>% 
        mutate(hubGene.GS_MM = case_when(
          abs_MM > 0.8 & abs_GS > 0.75  ~ "*",
          abs_MM > 0.85 & abs_GS > 0.8  ~ "**",
          abs_MM > 0.9 & abs_GS > 0.8  ~ "**",
          abs_MM > 0.9 & abs_GS > 0.9  ~ "***",
          TRUE ~ ""
        )) %>% 
        group_by(Module) %>% 
        mutate(
          hubGene.MM = case_when(
            abs_MM > 
          )
        )
    })
    writexl::write_xlsx(KeyModule_Trait,path = paste0(out_path,"KeyModule-trait-GSMM.xlsx"))
  }
  
}

xx = oneStepWGCNA(exp.mat = expmat,iterative = F,
                  lazy_modle = T,F.path = "~/tmp/",
                  pheofile = trait,rscut = 0.89,grey_cut = 1)

xx = oneStepNetwork(exp.mat = expmat ,lazy_modle = T,F.path = "~/tmp/")
gery_num = 
  data.frame(mc = xx$moduleColors) %>% 
  group_by(mc) %>% 
  summarise(n= n()) %>% filter(mc == "grey") %>% pull(n)


xx$net$MEs_col
