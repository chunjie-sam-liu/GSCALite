
# load mirna target data --------------------------------------------------

mirna2target <- readr::read_rds(file.path(config$database,"TCGA","mirna","miRNA2gene.all.nest.rds.gz"))

# get gene set mirna data -------------------------------------------------

mirna2target %>%
  dplyr::inner_join(gene_list,by="symbol") %>%
  tidyr::unnest() %>%
  dplyr::select(mirna,symbol)->gene_list_mirmna2target

# prepare data for network  -----------------------------------------------
# 1. networkD3 style ----
gene_list_mirmna2target %>% as.data.frame() %>% t() %>% as.vector() ->igraph_ready

# get node size
igraph_ready %>% 
  table() %>% 
  as.data.frame() %>%
  dplyr::mutate(Freq=ifelse(Freq>20,20,Freq)) ->node_size
names(node_size) <-c("name","size")

# igraph generate networkD3 graph object
a <- make_graph(igraph_ready)
b <- cluster_walktrap(a)
c <- membership(b)
d <- networkD3::igraph_to_networkD3(a, group = c)

# add size we got befor
d$nodes <-d$nodes %>%
  dplyr::left_join(node_size,by="name")

print("prepare data for networkD3 done!")
# output ------------------------------------------------------------------

output$mirna_net1 <- renderForceNetwork({
  networkD3::forceNetwork(Links = d$links, Nodes = d$nodes, 
                          Source = 'source', Target = 'target', 
                          NodeID = 'name', Nodesize = 'size',Group = 'group',fontSize = 15,radiusCalculation = "Math.sqrt(d.nodesize)+6",zoom=TRUE,
                          fontFamily = "cursive", opacityNoHover = 0.1)
})


print("draw networkD3 done!")

# 2. visNetwork style ----
gene_list_mirmna2target %>%
  tidyr::gather() %>%
  unique() %>%
  dplyr::rename(name=value) %>%
  dplyr::inner_join(node_size,by="name") ->mirna_vis_ready

mirna_vis_ready %>%
  dplyr::filter(key=="mirna") %>%
  nrow() ->mirna_mirna_num
mirna_vis_ready %>%
  dplyr::filter(key=="symbol") %>%
  nrow() ->mirna_gene_num

nodes <- data.frame(id = c(mirna_vis_ready %>% dplyr::filter(key=="mirna") %>% dplyr::pull(name),mirna_vis_ready %>% dplyr::filter(key=="symbol") %>% dplyr::pull(name)),
                    label = c(mirna_vis_ready %>% dplyr::filter(key=="mirna") %>% dplyr::pull(name),mirna_vis_ready %>% dplyr::filter(key=="symbol") %>% dplyr::pull(name)), # add labels on nodes
                    group = c(rep("miRNA",mirna_mirna_num), rep("Gene",mirna_gene_num)), # add groups on nodes 
                    value = mirna_vis_ready$size,# size adding value
                    shape = c(rep("circle",mirna_mirna_num),rep("database",mirna_gene_num))#, control shape of nodes
                    # color = c(rep("orange",mirna_mirna_num),rep("purple",mirna_gene_num))# color
                    )   

edges<- data.frame(from = gene_list_mirmna2target$mirna, to = gene_list_mirmna2target$symbol)
output$mirna_net2 <- visNetwork::renderVisNetwork({
  visNetwork::visNetwork(nodes, edges, width = "100%") %>% visNetwork::visEdges(color = "grey") %>% visNetwork::visEdges(smooth = TRUE) %>% visNetwork::visGroups(groupname = "miRNA", color = "lightblue",shape="circle") %>%  visNetwork::visGroups(groupname = "Gene", color = "red",shape="database") %>%  visNetwork::visLegend() %>%visNetwork::visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE) %>% visNetwork:: visInteraction(navigationButtons = TRUE) 
})
