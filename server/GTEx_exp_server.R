# sourced by 'server.R'
# save as 'GTEx_exp_server.R'
# server elements 'GTEx_exp' sub tab of 'GTEx' tab


# load GTEx expression data ---------------------------------------------------------

gtex_expr <- readr::read_rds(file.path(config$database, "GTEx", "expression", "gtex_gene_tmp_annotation_phenotype_v7.rds.gz"))

#  get tissue type --------------------------------------------------------

GTEx_tissue_type <- callModule(GTExTissueType, "tissues")
output$selected_tissues <- renderText(GTEx_tissue_type ())



#######reset tissue type selection when click.
observeEvent(input$GTEx_tissue_reset, {GTEx_tissue_type<-callModule(resetGTExTissueType,"GTEx_exp")})
observeEvent(input$GTEx_normal_Tissue, heatmap_gsva_4_geneset(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set))
# analysis core ----------------------------

#####get gene expression profiles in GTEx dataset######

get_gene_exp_profile <- function(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set,filter_gene = 0){
	gtex_expr <- gtex_expr[gtex.expr$SMTS %in% tissue_set,]
	if (filter_gene){		
		gtex_expr %>% 
			dplyr::mutate(
				expr = purrr::map(
					.x = expr,
					.f = function(.x) {
						.x %>% 
							dplyr::filter( symbol %in% gene_set)
					}
				)
			) -> gtex_gene_list_expr
	}else{
		gtex_gene_list_expr <- gtex_expr
	}
	return(gtex_gene_list_expr)
}

#####calculate and draw GSVA profiles for gene set in selected tissues in GTEx dataset######

heatmap_gsva_4_geneset <- function(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set){
####heatmap#############
        get_gene_exp_profile(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set, filter_gene = 1) %>%
                dplyr::mutate(
                        Mean = purrr::map(
                                .x = expr,
                                .f = function(.x) {rowMeans(.x[,-c(1,2)])}
                        )
                ) -> gtex_gene_list_expr.mean
        gene_n <- length(gene_set)
        display_matrix <- data.frame(round(matrix(unlist(gtex_gene_list_expr.mean$Mean),nrow = gene_n),2))

        colnames(display_matrix) <- gtex_gene_list_expr.mean$SMTS
        display_matrix$GeneName <- gtex_gene_list_expr.mean$expr[[1]]$symbol
        display_matrix %>% tidyr::gather(Tissue, RPKM, -GeneName) -> hm_4_p
        callModule(heatmap_GTEX_Plot, "GTEx_exp", data=hm_4_p)
########gsva############
	gtex_expr <- get_gene_exp_profile(gene_set = gene_set, gtex_expr = gtex_expr, tissue_set = tissue_set,filter_gene = 0)
  
	gene_set.lst <- list(atg_lys = gene_set)
	fn_gsva <- function(.y, gene_set = gene_set){
		.y %>% 
			tidyr::drop_na() %>% 
				dplyr::select( -ensembl_gene_id) -> .d
  
		.d_mat <- as.matrix(.d[,-1])
		rownames(.d_mat) <- .d$symbol
  
		.es_dif <- gsva(.d_mat, gset.idx.list = gene_set.lst, method = "gsva", mx.diff = TRUE, verbose = FALSE, parallel.sz = 1)
  
		.es_dif %>% 
			as.data.frame() %>% 
				tibble::as_tibble() %>% 
					tibble::add_column(set = "atg_lys", .before = 1) -> .d_es
	}

	gtex_expr %>% 
		dplyr::mutate(
			gsva = purrr::map(
				.x = expr,
				.f = function(.x) {
					fn_gsva(.x, gene_set = gene_set)
				}
			)
		) -> gtex_expr_gsva

	gtex_expr_gsva %>% 
		dplyr::select(SMTS, gsva) %>% 
			dplyr::mutate(
				gsva = purrr::map(
					.x = gsva,
					.f = function(.x) {
						.x %>% 
							dplyr::select(-set) %>% 
								tidyr::gather(key = barcode, value = gsva)
					}
				)
			) %>% tidyr::unnest() -> plot_ready
	callModule(box_GTEx_GSVA_Plot, "GTEx_gsva", data=plot_ready)
}


