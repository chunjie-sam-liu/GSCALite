
# load mirna target data --------------------------------------------------
source(file.path(config$wd, "functions", "tcga_mirna_function.R"))

# generate mirna result out ui -------------------------------------------------------
output$ui_mirna_result <- shiny::renderUI({
  fn_mirna_result(selected_analysis$mirna)
})
# start analysis ----------------------------------------------------------

mirna_analysis <- eventReactive(
  status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # get gene set mirna data -------------------------------------------------
    if (status$analysis == TRUE) {
      if (selected_analysis$mirna == TRUE) {
        print(selected_analysis$mirna)
        load_data_mirna()
        # as.character(gene_set$match) -> gene_set$match
        mirna2target %>%
          dplyr::filter(symbol %in% gene_set$match) %>%
          tidyr::unnest() %>%
          dplyr::select(mirna, symbol) -> gene_list_mirmna2target_d3
        if(nrow(gene_list_mirmna2target_d3)>0){
          # prepare data for network  -----------------------------------------------
          # 1. networkD3 style ----
          gene_list_mirmna2target_d3 %>%
            as.data.frame() %>%
            t() %>%
            as.vector() -> igraph_ready
          
          # get node size
          igraph_ready %>%
            table() %>%
            as.data.frame() %>%
            dplyr::mutate(Freq = ifelse(Freq > 20, 20, Freq)) -> mirna_node_size
          names(mirna_node_size) <- c("name", "size")
          mirna_node_size$name <- mirna_node_size$name %>% as.character()
          # igraph generate networkD3 graph object
          mirna_a <- make_graph(igraph_ready)
          mirna_b <- cluster_walktrap(mirna_a)
          mirna_c <- membership(mirna_b)
          mirna_d <- networkD3::igraph_to_networkD3(mirna_a, group = mirna_c)
          
          # add size we got befor
          mirna_d$nodes <- mirna_d$nodes %>%
            dplyr::left_join(mirna_node_size, by = "name")
          
          print("prepare data for networkD3 done!")
          
          # output ------------------------------------------------------------------
          mirna_d3 <- function(){
            networkD3::forceNetwork(
            Links = mirna_d$links, Nodes = mirna_d$nodes,
            Source = "source", Target = "target",
            NodeID = "name", Nodesize = "size", Group = "group", fontSize = 15, radiusCalculation = "Math.sqrt(d.nodesize)+6", zoom = TRUE,
            fontFamily = "cursive", opacityNoHover = 0.7
          )
          }
          output$mirna_net1 <- renderForceNetwork({
            status$analysis
            mirna_d3()
            # print("draw networkD3 done!")
          })
          
          # rbokeh::widget2png(mirna_d3(), "sankey.png")
          
          
          # 2. visNetwork style ----
          # get data for visnetwork, cor control edge width
          mirna2target %>%
            dplyr::filter(symbol %in% gene_set$match) %>%
            tidyr::unnest() %>%
            dplyr::mutate(cor = abs(cor - 1)) %>%
            dplyr::arrange(mirna) %>%
            dplyr::select(mirna, symbol, cor) -> gene_list_mirmna2target_vis
          
          # node size get, gene frequency control node size
          gene_list_mirmna2target_vis %>%
            dplyr::select(-cor) %>%
            as.data.frame() %>%
            t() %>%
            as.vector() -> get_node_freq_vis
          
          get_node_freq_vis %>%
            table() %>%
            as.data.frame() %>%
            dplyr::mutate(Freq = ifelse(Freq > 10, 10, Freq)) -> mirna_node_size_vis
          names(mirna_node_size_vis) <- c("name", "size")
          mirna_node_size_vis$name <- as.character(mirna_node_size_vis$name)
          
          # node size conbine into...
          gene_list_mirmna2target_vis %>%
            dplyr::select(-cor) %>%
            tidyr::gather() %>%
            unique() %>%
            dplyr::rename(name = value) %>%
            dplyr::inner_join(mirna_node_size_vis, by = "name") -> mirna_vis_ready
          
          # get nodes object for network.
          
          mirna_vis_ready %>%
            dplyr::filter(key == "mirna") %>%
            nrow() -> mirna_mirna_num
          mirna_vis_ready %>%
            dplyr::filter(key == "symbol") %>%
            nrow() -> mirna_gene_num
          c(mirna_vis_ready %>%
              dplyr::filter(key == "mirna") %>%
              dplyr::pull(name), mirna_vis_ready %>%
              dplyr::filter(key == "symbol") %>%
              dplyr::pull(name)) -> mirna_node_name
          
          mirna_nodes <- data.frame(
            id = mirna_node_name,
            label = mirna_node_name, # add labels on nodes
            group = c(rep("miRNA", mirna_mirna_num), rep("Gene", mirna_gene_num)), # add groups on nodes
            value = mirna_vis_ready$size, # size adding value
            scaling = list(min = 1, max = 10),
            shape = c(rep("database", mirna_mirna_num), rep("circle", mirna_gene_num)), # control shape of nodes
            # color = c(rep("orange",mirna_mirna_num),rep("purple",mirna_gene_num))# color
            title = paste0("<p>", mirna_node_name, "</p>"),
            stringsAsFactors = FALSE
          )
          
          
          mirna_edges <- data.frame(from = gene_list_mirmna2target_vis$mirna, to = gene_list_mirmna2target_vis$symbol, value = gene_list_mirmna2target_vis$cor)
          
          # network generate
          mirna_vis <- function(){
            visNetwork::visNetwork(mirna_nodes, mirna_edges, width = "100%") %>%
              visNetwork::visEdges(color = "darkorange") %>%
              visNetwork::visEdges(smooth = TRUE) %>%
              visNetwork::visGroups(groupname = "miRNA", color = "lightpink", shape = "database") %>%
              visNetwork::visGroups(groupname = "Gene", color = "gold", shape = "circle") %>%
              visNetwork::visLegend() %>%
              visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = TRUE) %>% # Highlight nearest & Select by node id
              visNetwork::visInteraction(navigationButtons = TRUE) %>%
              visNetwork::visEdges(arrows = "to") %>%
              visNetwork::visExport( type = "pdf", name = "miRNA_visNet",
                                     float = "right", style = NULL, loadDependencies = TRUE) 
          }
          output$mirna_net2 <- visNetwork::renderVisNetwork({
            status$analysis
            mirna_vis()
          })
          
        }else{
          .msg <- c("NOTICE: No significant (click to see help page above) miRNA-gene regulation relationship for your selected genes.")
          shinyBS::createAlert(
            session = session, anchorId = "mirna-no_gene_set", title = "Oops",
            content = .msg, style = "warning", append = FALSE
          )
        }
      }
    }
  }
)


observe(mirna_analysis())