# TrumpTownNetworkGraph
Graphing done in MySQL and R

# Trump_Town_American _Ent_Inst_Net_Graph
# https://www.jessesadler.com/post/network-analysis-with-r/

R.Version()  # "R version 3.5.2 (2018-12-20)"
getwd()

#if(!require(installr)) {
  install.packages("installr"); 
 # require(installr)}
#http://www.hcbravo.org/networks-across-scales/misc/tidygraph.nb.html

#updateR()

#The Packages
library(tidyverse)
library(tidygraph)
library(tidygraph)
library(ggraph)
library(colorspace)

#The Data
#AEI<-read.csv("American_Enterprise_Inst.csv")
#head(AEI)
 
Heritage<- read.csv("Heritage_Foundation.csv") 
view(Heritage)

Kirkland<- read.csv("Kirkland&Ellis.csv")
view(Kirkland)

AEI<- read.csv("AEI.csv")
view(AEI)

WHS<- read.csv("white_house_sources.csv")
view(WHS)

#Now we will networkd graph Heritage

###########
#NODE LIST
###########

#We need to add a column as our "Source", i.e. The American Enterprise Institute.
Heritage<- add_column(Heritage_Foundation,source= "Heritage")
View(Heritage)

sources<- Heritage%>%
  distinct(source)%>%
        rename(label = source)
View(sources)

destinations<- Heritage%>%
  distinct(agency_name)%>%
  rename(label = agency_name)
View(destinations)


nodes <- full_join(sources,destinations, by = "label")

nodes<- nodes%>%
    rowid_to_column("id")
View(nodes)

###########
#EDGE LIST
###########

per_route<- Heritage%>%
  group_by(source,agency_name)%>%
  summarise(weight= n()) %>%
  ungroup()
per_route


edges<- per_route%>%
    left_join(nodes, by = c("source" = "label"))%>%
    rename(from = id)
View(edges)

edges<- edges%>%
  left_join(nodes, by = c("agency_name" = "label"))%>%
  rename(to = id)
View(edges)

############
#TIDYGRAPH
###########

routes_tidy<- tbl_graph(nodes = nodes,edges = edges, directed = TRUE)


routes_tidy%>%
  activate(edges)%>%
  arrange(desc(weight))
 routes_tidy
 
 ggraph(routes_tidy)+
    geom_edge_link()+
    geom_node_point()+
   theme_graph()
#
#ggraph(routes_tidy,layout = "grapshot")+
  ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point()+
  geom_edge_link(aes(width = weight) , alpha= 0.8)+
  scale_edge_width(range = c(0.2, 3))+
  geom_node_text(aes(label=label), repel = TRUE)+
  labs(edge_width = "AEI")+
  theme_graph()
   #  color edges
  #ggraph(routes_tidy, layout = "graphopt") + 
    geom_node_point()+
    geom_edge_link(aes(width = weight,color =factor(nodes)) , alpha= 0.8)+
    scale_edge_width(range = c(0.2, 3))+
    geom_node_text(aes(label=label), repel = TRUE)+
    labs(edge_width = "AEI")+
    theme_graph()
    
################################
    # Network Kirkland
##############################
  
    
    #We need to add a column as our "Source", i.e. The American Enterprise Institute.
   Kirkland<- add_column(Kirkland,source= "Kirkland")
    View(Kirkland)
    
    sources<- Kirkland%>%
      distinct(source)%>%
      rename(label = source)
    View(sources)
    
    destinations<- Kirkland%>%
      distinct(agency_name)%>%
      rename(label = agency_name)
    View(destinations)
    
    
    nodes <- full_join(sources,destinations, by = "label")
    
    nodes<- nodes%>%
      rowid_to_column("id")
    View(nodes)
    
    ###########
    #EDGE LIST
    ###########
    
    per_route<- Kirkland%>%
      group_by(source,agency_name)%>%
      summarise(weight= n()) %>%
      ungroup()
    per_route
    
    
    edges<- per_route%>%
      left_join(nodes, by = c("source" = "label"))%>%
      rename(from = id)
    View(edges)
    
    edges<- edges%>%
      left_join(nodes, by = c("agency_name" = "label"))%>%
      rename(to = id)
    View(edges)
    
    ############
    #TIDYGRAPH
    ###########
    
    routes_tidy<- tbl_graph(nodes = nodes,edges = edges, directed = TRUE)
    
    
    routes_tidy%>%
      activate(edges)%>%
      arrange(desc(weight))
    routes_tidy
    
    ggraph(routes_tidy)+
      geom_edge_link()+
      geom_node_point()+
      theme_graph()
    #
    #ggraph(routes_tidy,layout = "grapshot")+
    ggraph(routes_tidy, layout = "graphopt") + 
      geom_node_point()+
      geom_edge_link(aes(width = weight) , alpha= 0.8)+
      scale_edge_width(range = c(0.2, 3))+
      geom_node_text(aes(label=label), repel = TRUE)+
      labs(edge_width = "Kirkland")+
      theme_graph()
    #  color edges
    #ggraph(routes_tidy, layout = "graphopt") + 
   # geom_node_point()+
      geom_edge_link(aes(width = weight,color =factor(nodes)) , alpha= 0.8)+
      scale_edge_width(range = c(0.2, 3))+
      geom_node_text(aes(label=label), repel = TRUE)+
      labs(edge_width = "AEI")+
      theme_graph()
      
  ########################
      # NETWORK AEI
  ###########################
      
      AEI<- add_column(AEI,source= "AEI")
      View(AEI)
      
      sources<- AEI%>%
        distinct(source)%>%
        rename(label = source)
      View(sources)
      
      destinations<- AEI%>%
        distinct(agency_name)%>%
        rename(label = agency_name)
      View(destinations)
      
      
      nodes <- full_join(sources,destinations, by = "label")
      
      nodes<- nodes%>%
        rowid_to_column("id")
      View(nodes)
      
      ###########
      #EDGE LIST
      ###########
      
      per_route<- AEI%>%
        group_by(source,agency_name)%>%
        summarise(weight= n()) %>%
        ungroup()
      per_route
      
      
      edges<- per_route%>%
        left_join(nodes, by = c("source" = "label"))%>%
        rename(from = id)
      View(edges)
      
      edges<- edges%>%
        left_join(nodes, by = c("agency_name" = "label"))%>%
        rename(to = id)
      View(edges)
      
      ############
      #TIDYGRAPH
      ###########
      
      routes_tidy<- tbl_graph(nodes = nodes,edges = edges, directed = TRUE)
      
      
      routes_tidy%>%
        activate(edges)%>%
        arrange(desc(weight))
      routes_tidy
      
      ggraph(routes_tidy)+
        geom_edge_link()+
        geom_node_point()+
        theme_graph()
      #
      #ggraph(routes_tidy,layout = "grapshot")+
      ggraph(routes_tidy, layout = "graphopt") + 
        geom_node_point()+
        geom_edge_link(aes(width = weight) , alpha= 0.8)+
        scale_edge_width(range = c(0.2, 3))+
        geom_node_text(aes(label=label), repel = TRUE)+
        labs(edge_width = "Kirkland")+
        theme_graph()
    
    
    
    
    
  
  #########################
  ##########################
  #Now lets reverse things: Where did WH staffers come from?
  getwd()
  WH_Firms<- read.csv("Firms_gone_to_WH.csv")
  head(WH_Firms)
  #
  #We need to add a column as our "Source", i.e. The American Enterprise Institute.
  WH_Firms<- add_column(WH_Firms,source = "source")
  view(WH_Firms)
  

  
  sources<- WH_Firms%>%
    distinct(source)%>%
    rename(label = source)
  View(sources)
  

  
  destinations<- WH_Firms%>%
    distinct(organization_name)%>%
    rename(label = organization_name)
  View(destinations)
  str(AEI)
  
  sources
  destinations
  nodes <- full_join(sources,destinations, by = "label")
  
  nodes<- nodes%>%
    rowid_to_column("id")
  View(nodes)
  
  ###########
  #EDGE LIST
  ###########
  
  per_route<- WH_Firms%>%
    group_by(source, organization_name)%>%
    summarise(weight= n()) %>%
    ungroup()
  per_route
  
  
  edges<- per_route%>%
    left_join(nodes, by = c("source" = "label"))%>%
    rename(from = id)
  View(edges)
  
  edges<- edges%>%
    left_join(nodes, by = c("agency_name" = "label"))%>%
    rename(to = id)
  View(edges)
  
  edges
  
  
  ############
  #TIDYGRAPH
  ###########
  
  routes_tidy<- tbl_graph(nodes = nodes,edges = edges, directed = TRUE)
  
  routes_tidy
  
  routes_tidy%>%
    activate(edges)%>%
    arrange(desc(weight))
  routes_tidy
  
  ggraph(routes_tidy)+
    geom_edge_link()+
    geom_node_point()+
    theme_graph()
  #
 
  ggraph(routes_tidy, layout = "graphopt") + 
    geom_node_point()+
    geom_edge_link(aes(width = weight) , alpha= 0.8)+
    scale_edge_width(range = c(0.2, 3))+
    geom_node_text(aes(label=label), repel = TRUE)+
    labs(edge_width = "WH_Firms")+
    theme_graph()
  ###################################
  #   Now we will reduce it to just the biggest firms
  ######################################
  
  ############
  df<- WH_Firms
  dim(df) #433 x 3
  view(df)
  
  df_10<-df%>%
      group_by(organization_name)%>%
      top_n(n=10)
  df_10
  dim(df_10)
  
  df_10.1<-df%>%
     count(organization_name)%>%
      arrange(desc(n))%>%
      group_by(organization_name)
  df_10.1
  view(df_10.1)
  
 ################
  # Now lets network graph  those comapnies that sent the most to the WH
  #######
  #We need to add a column as our "Source", i.e. The American Enterprise Institute.
 WH_top_10<- add_column(df_10.1,source = "source")
view(WH_top_10)  
#
sources<- WH_top_10%>%
  distinct(source)%>%
  rename(label = source)
View(sources)

destinations<- WH_top_10%>%
  distinct(organization_name)%>%
  rename(label = organization_name)
View(destinations)

sources
destinations
nodes <- full_join(sources,destinations, by = "label")

nodes<- nodes%>%
  rowid_to_column("id")
View(nodes)

###########
#EDGE LIST
###########

per_route<- WH_top_10%>%
  group_by(source, organization_name)%>%
  summarise(weight= n()) %>%
  ungroup()
per_route

edges<- per_route%>%
  left_join(nodes, by = c("source" = "label"))%>%
  rename(from = id)
View(edges)

############
#TIDYGRAPH
###########

routes_tidy<- tbl_graph(nodes = nodes,edges = edges, directed = TRUE)

routes_tidy%>%
  activate(edges)%>%
  arrange(desc(weight))
routes_tidy

ggraph(routes_tidy)+
  geom_edge_link()+
  geom_node_point()+
  theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point()+
  geom_edge_link(aes(width = weight) , alpha= 0.8)+
  scale_edge_width(range = c(0.2, 3))+
  geom_node_text(aes(label=label), repel = TRUE)+
  labs(edge_width = "WH_Firms")+
  theme_graph()
######################
#TURN THE TELESCOPE AROUND
#group by and count
WHS
Big_10_sources<- WHS%>%
    count(organization_name)%>%
    arrange(desc(n))%>%
    group_by(organization_name)
  Big_10_sources
view(Big_10_sources)



