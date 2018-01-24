sortByAnnotation <-function(numMat,maf, anno, annoOrder = NULL){
  anno[,1] = as.character(anno[,1])
  anno[,1] = ifelse(test = is.na(anno[,1]), yes = "NA", no = anno[,1]) #NAs are notorious; converting them to characters
  anno.spl = split(anno, as.factor(as.character(anno[,1]))) #sorting only first annotation
  anno.spl.sort = lapply(X = anno.spl, function(x){
    numMat[,colnames(numMat)[colnames(numMat) %in% rownames(x)], drop = FALSE]
  })
  
  anno.spl.sort = anno.spl.sort[names(sort(unlist(lapply(anno.spl.sort, ncol)), decreasing = TRUE))] #sort list according to number of elemnts in each classification
  
  if(!is.null(annoOrder)){
    annoSplOrder = names(anno.spl.sort)
    
    if(length(annoOrder[annoOrder %in% annoSplOrder]) == 0){
      message("Values in provided annotation order ", paste(annoOrder, collapse = ", ")," does not match values in clinical features. Here are the available features..")
      print(annoSplOrder)
      stop()
    }
    annoOrder = annoOrder[annoOrder %in% annoSplOrder]
    
    anno.spl.sort = anno.spl.sort[annoOrder]
    
    if(length(annoSplOrder[!annoSplOrder %in% annoOrder]) > 0){
      warning("Following levels are missing from the provided annotation order: ", paste(annoSplOrder[!annoSplOrder %in% annoOrder], collapse = ", "))
    }
  }
  
  numMat.sorted = c()
  for(i in 1:length(anno.spl.sort)){
    numMat.sorted  = cbind(numMat.sorted, anno.spl.sort[[i]])
  }
  
  return(numMat.sorted)
}

createOncoMatrix = function(m, g = NULL, chatty = TRUE){
  
  if(is.null(g)){
    stop("Please provde atleast two genes!")
  }
  
  maf = subsetMaf(maf = m, genes = g, includeSyn = FALSE)
  
  oncomat = data.table::dcast(data = maf[,.(Hugo_Symbol, Variant_Classification, Tumor_Sample_Barcode)], formula = Hugo_Symbol ~ Tumor_Sample_Barcode,
                              fun.aggregate = function(x){
                                x = unique(as.character(x))
                                xad = x[x %in% c('Amp', 'Del')]
                                xvc = x[!x %in% c('Amp', 'Del')]
                                
                                if(length(xvc)>0){
                                  xvc = ifelse(test = length(xvc) > 1, yes = 'Multi_Hit', no = xvc)
                                }
                                
                                x = ifelse(test = length(xad) > 0, yes = paste(xad, xvc, sep = ';'), no = xvc)
                                x = gsub(pattern = ';$', replacement = '', x = x)
                                x = gsub(pattern = '^;', replacement = '', x = x)
                                return(x)
                              } , value.var = 'Variant_Classification', fill = '', drop = FALSE)
  
  #convert to matrix
  data.table::setDF(oncomat)
  rownames(oncomat) = oncomat$Hugo_Symbol
  oncomat = as.matrix(oncomat[,-1, drop = FALSE])
  
  variant.classes = as.character(unique(maf[,Variant_Classification]))
  variant.classes = c('',variant.classes, 'Multi_Hit')
  names(variant.classes) = 0:(length(variant.classes)-1)
  
  #Complex variant classes will be assigned a single integer.
  vc.onc = unique(unlist(apply(oncomat, 2, unique)))
  vc.onc = vc.onc[!vc.onc %in% names(variant.classes)]
  names(vc.onc) = rep(as.character(as.numeric(names(variant.classes)[length(variant.classes)])+1), length(vc.onc))
  variant.classes2 = c(variant.classes, vc.onc)
  
  oncomat.copy <- oncomat
  #Make a numeric coded matrix
  for(i in 1:length(variant.classes2)){
    oncomat[oncomat == variant.classes2[i]] = names(variant.classes2)[i]
  }
  
  #If maf has only one gene
  if(nrow(oncomat) == 1){
    mdf  = t(matrix(as.numeric(oncomat)))
    rownames(mdf) = rownames(oncomat)
    colnames(mdf) = colnames(oncomat)
    return(list(oncoMatrix = oncomat.copy, numericMatrix = mdf, vc = variant.classes))
  }
  
  #convert from character to numeric
  mdf = as.matrix(apply(oncomat, 2, function(x) as.numeric(as.character(x))))
  rownames(mdf) = rownames(oncomat.copy)
  
  
  #If MAF file contains a single sample, simple sorting is enuf.
  if(ncol(mdf) == 1){
    mdf = as.matrix(mdf[order(mdf, decreasing = TRUE),])
    colnames(mdf) = sampleId
    
    oncomat.copy = as.matrix(oncomat.copy[rownames(mdf),])
    colnames(oncomat.copy) = sampleId
    
    return(list(oncoMatrix = oncomat.copy, numericMatrix = mdf, vc = variant.classes))
  } else{
    #Sort by rows as well columns if >1 samples present in MAF
    #Add total variants per gene
    mdf = cbind(mdf, variants = apply(mdf, 1, function(x) {
      length(x[x != "0"])
    }))
    #Sort by total variants
    mdf = mdf[order(mdf[, ncol(mdf)], decreasing = TRUE), ]
    #colnames(mdf) = gsub(pattern = "^X", replacement = "", colnames(mdf))
    nMut = mdf[, ncol(mdf)]
    
    mdf = mdf[, -ncol(mdf)]
    
    mdf.temp.copy = mdf #temp copy of original unsorted numeric coded matrix
    
    mdf[mdf != 0] = 1 #replacing all non-zero integers with 1 improves sorting (& grouping)
    tmdf = t(mdf) #transposematrix
    mdf = t(tmdf[do.call(order, c(as.list(as.data.frame(tmdf)), decreasing = TRUE)), ]) #sort
    
    mdf.temp.copy = mdf.temp.copy[rownames(mdf),] #organise original matrix into sorted matrix
    mdf.temp.copy = mdf.temp.copy[,colnames(mdf)]
    mdf = mdf.temp.copy
    
    #organise original character matrix into sorted matrix
    oncomat.copy <- oncomat.copy[,colnames(mdf)]
    oncomat.copy <- oncomat.copy[rownames(mdf),]
    
    return(list(oncoMatrix = oncomat.copy, numericMatrix = mdf, vc = variant.classes))
  }
}

my_oncoplot<-function (maf, top = 20, genes = NULL, mutsig = NULL, mutsigQval = 0.1, 
                       drawRowBar = TRUE, drawColBar = TRUE, clinicalFeatures = NULL, 
                       annotationDat = NULL, annotationColor = NULL, genesToIgnore = NULL, 
                       showTumorSampleBarcodes = FALSE, removeNonMutated = TRUE, 
                       colors = NULL, sortByMutation = FALSE, sortByAnnotation = FALSE, 
                       annotationOrder = NULL, keepGeneOrder = FALSE, GeneOrderSort = TRUE, 
                       writeMatrix = FALSE, fontSize = 10, SampleNamefontSize = 10, 
                       titleFontSize = 15, legendFontSize = 12) 
{
  set.seed(seed = 1024)
  if (!is.null(genes)) {
    om = createOncoMatrix(m = maf, g = genes)
    numMat = om$numericMatrix
    mat_origin = om$oncoMatrix
  } else if (!is.null(mutsig)) {
    if (as.logical(length(grep(pattern = "gz$", x = mutsig, 
                               fixed = FALSE)))) {
      if (Sys.info()[["sysname"]] == "Windows") {
        mutsigResults.gz = gzfile(description = mutsig, 
                                  open = "r")
        suppressWarnings(ms <- data.table::as.data.table(read.csv(file = mutsigResults.gz, 
                                                                  header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
                                                                  comment.char = "#")))
        close(mutsigResults.gz)
      } else {
        ms = suppressWarnings(data.table::fread(input = paste("zcat <", 
                                                              mutsig), sep = "\t", stringsAsFactors = FALSE, 
                                                verbose = FALSE, data.table = TRUE, showProgress = TRUE, 
                                                header = TRUE))
      }
    } else {
      ms = data.table::fread(input = mutsig, sep = "\t", 
                             stringsAsFactors = FALSE, header = TRUE)
    }
    ms$q = as.numeric(gsub(pattern = "^<", replacement = "", 
                           x = as.character(ms$q)))
    ms[, `:=`(FDR, -log10(as.numeric(as.character(q))))]
    mach.epsi = .Machine$double.eps
    ms$q = ifelse(test = ms$q == 0, yes = mach.epsi, no = ms$q)
    ms.smg = ms[q < mutsigQval]
    genes = as.character(ms[q < mutsigQval, gene])
    om = createOncoMatrix(m = maf, g = genes)
    numMat = om$numericMatrix
    mat_origin = om$oncoMatrix
    if (length(genes[!genes %in% rownames(numMat)]) > 0) {
      message("Following genes from MutSig results are not available in MAF:")
      print(genes[!genes %in% rownames(numMat)])
      message("Ignoring them.")
      genes = genes[ms.smg.genes %in% rownames(numMat)]
      ms.smg = ms.smg[gene %in% ms.smg.genes]
    }
    ms.smg = ms.smg[, .(gene, FDR)]
    ms.smg = data.frame(row.names = ms.smg$gene, FDR = ms.smg$FDR)
  } else {
    genes = getGeneSummary(x = maf)[1:top, Hugo_Symbol]
    om = createOncoMatrix(m = maf, g = genes)
    numMat = om$numericMatrix
    mat_origin = om$oncoMatrix
  }
  if (!is.null(genesToIgnore)) {
    numMat = numMat[!rownames(numMat) %in% genesToIgnore, 
                    ]
    mat_origin = mat_origin[!rownames(mat_origin) %in% genesToIgnore, 
                            ]
  }
  if (ncol(numMat) < 2) {
    stop("Cannot create oncoplot for single sample. Minimum two sample required ! ")
  }
  if (nrow(numMat) < 2) {
    stop("Cannot create oncoplot for single gene. Minimum two genes required ! ")
  }
  totSamps = as.numeric(maf@summary[3, summary])
  if (!is.null(clinicalFeatures)) {
    if (!is.null(annotationDat)) {
      data.table::setDF(annotationDat)
      if (length(clinicalFeatures[!clinicalFeatures %in% 
                                  colnames(annotationDat)]) > 0) {
        message("Following columns are missing from provided annotation data. Ignoring them..")
        print(clinicalFeatures[!clinicalFeatures %in% 
                                 colnames(annotationDat)])
        clinicalFeatures = clinicalFeatures[clinicalFeatures %in% 
                                              colnames(annotationDat)]
        if (length(clinicalFeatures) == 0) {
          stop("Zero annotaions to add! Make sure at-least one of the provided clinicalFeatures are present in annotationDat")
        }
      }
      annotation = annotationDat[, c("Tumor_Sample_Barcode", 
                                     clinicalFeatures)]
      annotation = data.frame(row.names = annotation$Tumor_Sample_Barcode, 
                              annotation[, clinicalFeatures, drop = FALSE])
    }
    else {
      annotationDat = getClinicalData(x = maf)
      if (length(clinicalFeatures[!clinicalFeatures %in% 
                                  colnames(annotationDat)]) > 0) {
        message("Following columns are missing from annotation slot of MAF. Ignoring them..")
        print(clinicalFeatures[!clinicalFeatures %in% 
                                 colnames(annotationDat)])
        clinicalFeatures = clinicalFeatures[clinicalFeatures %in% 
                                              colnames(annotationDat)]
        if (length(clinicalFeatures) == 0) {
          message("Make sure at-least one of the values from provided clinicalFeatures are present in annotation slot of MAF. Here are available annotaions from MAF..")
          print(colnames(getClinicalData(maf)))
          stop("Zero annotaions to add! You can also provide custom annotations via annotationDat argument.")
        }
      }
      annotation = data.frame(row.names = annotationDat$Tumor_Sample_Barcode, 
                              annotationDat[, clinicalFeatures, with = FALSE])
    }
  }
  if (length(genes[!genes %in% rownames(numMat)]) > 0) {
    message("Following genes from provided gene list are missing from MAF:")
    print(genes[!genes %in% rownames(numMat)])
    message("Ignoring them.")
    genes = genes[genes %in% rownames(numMat)]
    if (length(genes) < 1) {
      stop("Only one gene left! At-least two genes required for plotting.")
    }
  }
  if (sortByMutation) {
    numMat = sortByMutation(numMat = numMat, maf = maf)
  }
  if (sortByAnnotation) {
    if (is.null(clinicalFeatures)) {
      stop("Use argument `annotationCol` to provide at-least one of the annotations.")
    }
    numMat = sortByAnnotation(numMat = numMat, maf = maf, 
                              annotation, annoOrder = annotationOrder)
  }
  mat = mat_origin[rownames(numMat), , drop = FALSE]
  mat = mat[, colnames(numMat), drop = FALSE]
  if (removeNonMutated) {
    numMat = numMat[rownames(mat), , drop = FALSE]
    numMat = numMat[, colnames(mat), drop = FALSE]
    tsb = colnames(numMat)
    tsb.exclude = colnames(numMat[, colSums(numMat) == 0, 
                                  drop = FALSE])
    tsb.include = tsb[!tsb %in% tsb.exclude]
    mat = mat[, tsb.include, drop = FALSE]
  }
  if (keepGeneOrder) {
    if (GeneOrderSort) {
      numMat = sortByGeneOrder(m = numMat, g = genes)
    }
    else {
      numMat = numMat[genes, , drop = FALSE]
    }
    mat = mat_origin[rownames(numMat), , drop = FALSE]
    mat = mat[, colnames(numMat), drop = FALSE]
  }
  if (!is.null(clinicalFeatures)) {
    annotation = annotation[rownames(annotation) %in% colnames(mat), 
                            , drop = FALSE]
    annotation = annotation[colnames(mat), , drop = FALSE]
  }
  if (writeMatrix) {
    write.table(mat, "onco_matrix.txt", sep = "\t", quote = FALSE)
  }
  mat[mat == ""] = "xxx"
  if (removeNonMutated) {
    altStat = paste0("Altered in ", ncol(mat), " (", round(ncol(mat)/totSamps, 
                                                           digits = 4) * 100, "%) of ", totSamps, " samples.")
  }
  else {
    mutSamples = length(unique(unlist(genesToBarcodes(maf = maf, 
                                                      genes = rownames(mat), justNames = TRUE))))
    altStat = paste0("Altered in ", mutSamples, " (", round(mutSamples/totSamps, 
                                                            digits = 4) * 100, "%) of ", totSamps, " samples.")
  }
  if (is.null(colors)) {
    col = c(RColorBrewer::brewer.pal(12, name = "Paired"), 
            RColorBrewer::brewer.pal(11, name = "Spectral")[1:3], 
            "black", "violet", "royalblue")
    names(col) = names = c("Nonstop_Mutation", "Frame_Shift_Del", 
                           "IGR", "Missense_Mutation", "Silent", "Nonsense_Mutation", 
                           "RNA", "Splice_Site", "Intron", "Frame_Shift_Ins", 
                           "Nonstop_Mutation", "In_Frame_Del", "ITD", "In_Frame_Ins", 
                           "Translation_Start_Site", "Multi_Hit", "Amp", "Del")
  }
  else {
    col = colors
  }
  bg = "#CCCCCC"
  col = c(col, xxx = bg)
  variant.classes = unique(unlist(as.list(apply(mat_origin, 
                                                2, unique))))
  variant.classes = unique(unlist(strsplit(x = variant.classes, 
                                           split = ";", fixed = TRUE)))
  variant.classes = variant.classes[!variant.classes %in% 
                                      c("xxx")]
  type_col = structure(col[variant.classes], names = names(col[variant.classes]))
  type_col = type_col[!is.na(type_col)]
  type_name = structure(variant.classes, names = variant.classes)
  variant.classes = variant.classes[!variant.classes %in% 
                                      c("Amp", "Del")]
  vc.mat = unique(unlist(as.list(apply(mat, 2, unique))))
  vc.mat = unique(unlist(strsplit(x = vc.mat, split = ";", 
                                  fixed = TRUE)))
  vc.mat = vc.mat[!vc.mat %in% c("xxx")]
  vc.type_name = structure(vc.mat, names = vc.mat)
  vc.type_col = structure(col[vc.mat], names = names(col[vc.mat]))
  vc.type_col = vc.type_col[!is.na(vc.type_col)]
  if (!is.null(clinicalFeatures)) {
    if (!is.null(annotationColor)) {
      bot.anno = ComplexHeatmap::HeatmapAnnotation(df = annotation, 
                                                   col = annotationColor)
    }
    else {
      bot.anno = ComplexHeatmap::HeatmapAnnotation(annotation)
    }
  }
  anno_pct = function(index) {
    n = length(index)
    pct = apply(numMat[rev(index), ], 1, function(x) length(x[x != 
                                                                0]))/as.numeric(maf@summary[3, summary]) * 100
    pct = paste0(round(pct), "%")
    grid::pushViewport(viewport(xscale = c(0, 1), yscale = c(0.5, 
                                                             n + 0.5)))
    grid::grid.text(pct, x = 1, y = seq_along(index), default.units = "native", 
                    just = "right", gp = grid::gpar(fontsize = fontSize))
    grid::upViewport()
  }
  ha_pct = ComplexHeatmap::HeatmapAnnotation(pct = anno_pct, 
                                             width = grid::grobWidth(grid::textGrob("100%", gp = grid::gpar(fontsize = 10))), 
                                             which = "row")
  if (!is.null(mutsig)) {
    anno_row_bar = function(index) {
      n = length(index)
      max_count = max(as.numeric(as.character(ms.smg$FDR)))
      tb = rev(as.list(ms.smg$FDR))
      grid::pushViewport(grid::viewport(xscale = c(0, 
                                                   max_count * 1.1), yscale = c(0.5, n + 0.5)))
      for (i in seq_along(tb)) {
        if (length(tb[[i]])) {
          x = cumsum(tb[[i]])
          grid::grid.rect(x = x, i, width = tb[[i]], 
                          height = 0.8, default.units = "native", 
                          just = "right", gp = grid::gpar(col = NA, 
                                                          fill = "gray70"))
        }
      }
      breaks = grid::grid.pretty(c(0, max_count))
      grid::grid.xaxis(at = breaks, label = breaks, main = FALSE, 
                       gp = grid::gpar(fontsize = 10))
      grid::upViewport()
    }
  }
  else {
    anno_row_bar = function(index) {
      n = length(index)
      tb = list()
      for (i in nrow(mat):1) {
        x = mat[i, ]
        x = x[x != ""]
        x = x[x != "xxx"]
        x = unlist(strsplit(x, ";"))
        x = sort(x)
        tb[[i]] = table(x)
      }
      tb = rev(tb)
      max_count = max(sapply(tb, sum))
      grid::pushViewport(grid::viewport(xscale = c(0, 
                                                   max_count * 1.1), yscale = c(0.5, n + 0.5)))
      for (i in seq_along(tb)) {
        if (length(tb[[i]])) {
          x = cumsum(tb[[i]])
          grid::grid.rect(x = x, i, width = tb[[i]], 
                          height = 0.8, default.units = "native", 
                          just = "right", gp = grid::gpar(col = NA, 
                                                          fill = type_col[names(tb[[i]])]))
        }
      }
      breaks = grid::grid.pretty(c(0, max_count))
      grid::grid.xaxis(at = breaks, label = breaks, main = FALSE, 
                       gp = grid::gpar(fontsize = 10))
      grid::upViewport()
    }
  }
  ha_row_bar = ComplexHeatmap::HeatmapAnnotation(row_bar = anno_row_bar, 
                                                 width = grid::unit(4, "cm"), which = "row")
  anno_column_bar = function(index) {
    n = length(index)
    ss = getSampleSummary(x = maf)
    tb = ss[Tumor_Sample_Barcode %in% colnames(mat)]
    tb = tb[, colnames(tb)[!colnames(tb) %in% c("total", 
                                                "Amp", "Del", "CNV_total")], with = FALSE]
    tb = split(tb, as.factor(as.character(tb$Tumor_Sample_Barcode)))
    tb = lapply(X = tb, function(x) unlist(x)[-1])
    tb = tb[colnames(mat)]
    max_count = max(sapply(tb, sum))
    grid::pushViewport(grid::viewport(yscale = c(0, max_count * 
                                                   1.1), xscale = c(0.5, n + 0.5)))
    for (i in seq_along(tb)) {
      if (length(tb[[i]])) {
        y = cumsum(tb[[i]])
        grid::grid.rect(i, y, height = tb[[i]], width = 0.8, 
                        default.units = "native", just = "top", gp = grid::gpar(col = NA, 
                                                                                fill = type_col[names(tb[[i]])]))
      }
    }
    breaks = grid::grid.pretty(c(0, max_count))
    grid::grid.yaxis(at = breaks, label = breaks, gp = grid::gpar(fontsize = 10))
    grid::upViewport()
  }
  ha_column_bar = ComplexHeatmap::HeatmapAnnotation(column_bar = anno_column_bar, 
                                                    which = "column")
  add_oncoprint = function(type, x, y, width, height) {
    grid::grid.rect(x, y, width - unit(0.5, "mm"), height - 
                      grid::unit(1, "mm"), gp = grid::gpar(col = NA, fill = bg))
    for (i in 1:length(variant.classes)) {
      if (any(type %in% variant.classes[i])) {
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - grid::unit(1, "mm"), gp = grid::gpar(col = NA, 
                                                                      fill = type_col[variant.classes[i]]))
      }
      else if (any(type %in% "Amp")) {
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - grid::unit(1, "mm"), gp = grid::gpar(col = NA, 
                                                                      fill = bg))
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - unit(15, "mm"), gp = grid::gpar(col = NA, 
                                                                 fill = type_col["Amp"]))
      }
      else if (any(type %in% "Del")) {
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - grid::unit(1, "mm"), gp = grid::gpar(col = NA, 
                                                                      fill = bg))
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - grid::unit(15, "mm"), gp = grid::gpar(col = NA, 
                                                                       fill = type_col["Del"]))
      }
    }
  }
  add_oncoprint2 = function(type, x, y, width, height) {
    for (i in 1:length(variant.classes)) {
      if (any(type %in% variant.classes[i])) {
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - grid::unit(1, "mm"), gp = grid::gpar(col = NA, 
                                                                      fill = type_col[variant.classes[i]]))
      }
      else if (any(type %in% "Amp")) {
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - unit(15, "mm"), gp = grid::gpar(col = NA, 
                                                                 fill = type_col["Amp"]))
      }
      else if (any(type %in% "Del")) {
        grid::grid.rect(x, y, width - unit(0.5, "mm"), 
                        height - grid::unit(15, "mm"), gp = grid::gpar(col = NA, 
                                                                       fill = type_col["Del"]))
      }
    }
  }
  celFun = function(j, i, x, y, width, height, fill) {
    type = mat[i, j]
    if (type != "xxx") {
      typeList = sort(unlist(strsplit(x = as.character(type), 
                                      split = ";")), decreasing = TRUE)
      if (length(typeList) > 1) {
        for (i in 1:length(typeList)) {
          add_oncoprint2(typeList[i], x, y, width, height)
        }
      }
      else {
        for (i in 1:length(typeList)) {
          add_oncoprint(typeList[i], x, y, width, height)
        }
      }
    }
    else {
      add_oncoprint(type, x, y, width, height)
    }
  }
  if (drawColBar) {
    if (is.null(clinicalFeatures)) {
      ht = ComplexHeatmap::Heatmap(mat, na_col = bg, rect_gp = grid::gpar(type = "none"), 
                                   cell_fun = celFun, row_names_gp = grid::gpar(fontsize = fontSize), 
                                   show_column_names = showTumorSampleBarcodes, 
                                   show_heatmap_legend = FALSE, top_annotation = ha_column_bar, 
                                   top_annotation_height = grid::unit(2, "cm"), 
                                   column_title_gp = grid::gpar(fontsize = titleFontSize, 
                                                          fontface = "bold"), column_title = altStat)
    }
    else {
      ht = ComplexHeatmap::Heatmap(mat, na_col = bg, rect_gp = grid::gpar(type = "none"), 
                                   cell_fun = celFun, row_names_gp = grid::gpar(fontsize = fontSize), 
                                   show_column_names = showTumorSampleBarcodes, 
                                   show_heatmap_legend = FALSE, top_annotation = ha_column_bar, 
                                   top_annotation_height = grid::unit(2, "cm"), 
                                   bottom_annotation = bot.anno, column_title_gp = grid::gpar(fontsize = titleFontSize, 
                                                                                        fontface = "bold"), column_title = altStat)
    }
  }
  else {
    if (is.null(clinicalFeatures)) {
      ht = ComplexHeatmap::Heatmap(mat, na_col = bg, rect_gp = grid::gpar(type = "none"), 
                                   cell_fun = celFun, row_names_gp = grid::gpar(fontsize = fontSize), 
                                   show_column_names = showTumorSampleBarcodes, 
                                   show_heatmap_legend = FALSE, column_title_gp = grid::gpar(fontsize = titleFontSize, 
                                                                                       fontface = "bold"), column_title = altStat)
    }
    else {
      ht = ComplexHeatmap::Heatmap(mat, na_col = bg, rect_gp = grid::gpar(type = "none"), 
                                   cell_fun = celFun, row_names_gp = grid::gpar(fontsize = fontSize), 
                                   show_column_names = showTumorSampleBarcodes, 
                                   show_heatmap_legend = FALSE, bottom_annotation = bot.anno, 
                                   column_title_gp = grid::gpar(fontsize = titleFontSize, 
                                                          fontface = "bold"), column_title = altStat)
    }
  }
  ha_pct = ComplexHeatmap::HeatmapAnnotation(pct = anno_pct, 
                                             width = grid::grobWidth(grid::textGrob("100%", gp = grid::gpar(fontsize = 10))), 
                                             which = "row")
  ht_list = ha_pct + ht
  if (drawRowBar) {
    ht_list = ht_list + ha_row_bar
  }
  legend = grid::legendGrob(labels = vc.type_name[names(vc.type_col)], 
                            pch = 15, gp = grid::gpar(col = vc.type_col, fontsize = legendFontSize), 
                            nrow = 3)
  suppressWarnings(ComplexHeatmap::draw(ht_list, newpage = FALSE, 
                                        annotation_legend_side = "bottom", annotation_legend_list = list(legend)))
}
