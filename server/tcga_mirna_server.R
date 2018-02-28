
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
          dplyr::filter(symbol %in% gene_set$match) -> genelist_mirna
        
        # estimate the mirna data is not NULL -------------
        if(nrow(genelist_mirna)>0){
          # prepare data for network  ----
          genelist_mirna %>%
            tidyr::unnest() %>%
            dplyr::select(mirna, symbol) -> gene_list_mirmna2target_d3
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
          output$`mirna_net1-downloadNetwork`<- downloadHandler(
            filename = function(){ paste("networkD3",Sys.Date(),Sys.time(),".html")},
            content = function(file){
              mirna_d3() %>% htmlwidgets::saveWidget(file)
            }
          )
          # rbokeh::widget2png(mirna_d3(), "sankey.png")
          
          
          # 2. visNetwork style ----
          # get data for visnetwork, cor control edge width
          genelist_mirna %>%
            tidyr::unnest() %>%
            dplyr::mutate(cor = abs(cor)) %>%
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
            value = mirna_vis_ready$size, # size adding value
            label = mirna_node_name, # add labels on nodes
            group = c(rep("miRNA", mirna_mirna_num), rep("Gene", mirna_gene_num)), # add groups on nodes
            shape = c(rep("dot", mirna_mirna_num), rep("dot", mirna_gene_num)), # control shape of nodes
            # color = c(rep("orange",mirna_mirna_num),rep("purple",mirna_gene_num))# color
            title = paste0("<p>", mirna_node_name, "</p>"),
            stringsAsFactors = FALSE
          )
          
          
          mirna_edges <- data.frame(from = gene_list_mirmna2target_vis$mirna, to = gene_list_mirmna2target_vis$symbol, value = gene_list_mirmna2target_vis$cor)
          
          # network generate
          mirna_vis <- function(nodes,edges){
            visNetwork::visNetwork(nodes, edges, scaling = list(min = 1, max = 10), width = "1000px", height = "1000px") %>%
              # visNetwork::visEdges(color = "darkorange") %>%
              visNetwork::visEdges(smooth = TRUE) %>%
              # visNetwork::visGroups(groupname = "miRNA", color = "lightpink", shape = "database") %>%
              # visNetwork::visGroups(groupname = "Gene", color = "gold", shape = "circle") %>%
              visNetwork::visLegend() %>%
              visNetwork::visOptions(highlightNearest = TRUE) %>%
              # visNetwork::visInteraction(navigationButtons = TRUE) %>%
              visNetwork::visEdges(arrows = "to") 
          }
          output$mirna_net2 <- visNetwork::renderVisNetwork({
            status$analysis
            mirna_vis(mirna_nodes, mirna_edges) %>%
              visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = TRUE) # Highlight nearest & Select by node id
          })
          
          # get position info
          observeEvent(input$`mirna_net2-store_position`, {
            print(input$`mirna_net2-store_position`)
            visNetwork::visNetworkProxy("mirna_net2") %>% visNetwork::visGetPositions()
          })
          
          # format positions
          n2_nodes_positions <- reactive({
            positions <- input$network_positions
            if(!is.null(positions)){
              nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
              nodes_positions$id <- names(positions)
              nodes_positions
            } else {
              NULL
            }
          })
          print(head(n2_nodes_positions()))
          # get download file
          output$`mirna_net2-downloadNetwork`<- downloadHandler(
            filename = function() {
              paste('visNetwork-', Sys.Date(), '.html', sep='')
            },
            content = function(con) {
              print(con)
              nodes_positions <- n2_nodes_positions()
              if(!is.null(nodes_positions)){
                nodes_save <- merge(mirna_nodes, nodes_positions, by = "id", all = T)
              } else  {
                nodes_save <- mirna_nodes
              }
              
              mirna_vis(nodes_save,mirna_edges) %>%
                visNetwork::visExport( type = "pdf", name = "miRNA_visNet",
                                       float = "right", style = NULL, loadDependencies = TRUE) %>%
                visNetwork::visPhysics(enabled = FALSE) %>%
                visNetwork::visSave(con)
              # visNetwork(nodes = nodes_save, edges = edges, height = "800px") %>%
              #   visOptions(highlightNearest = TRUE) %>% visExport() %>%
              #   visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% 
              #   #visSave(file=file)
              #   htmlwidgets::saveWidget(con)
            }
          )
          .msg <- c("NOTICE: miRNA-gene depends on all 33 cancer types, so the selection change of cancer types will not change the result.")
          shinyBS::createAlert(
            session = session, anchorId = "mirna-no_gene_set", title = "Information",
            content = .msg, style = "info", append = FALSE
          )
        }else{
          output$mirna_net2 <- visNetwork::renderVisNetwork({NULL})
          output$mirna_net1 <- renderForceNetwork({NULL})
          .msg <- c("NOTICE: No significant (click to see help page above) miRNA-gene regulation relationship for your selected genes. Please try more genes.")
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