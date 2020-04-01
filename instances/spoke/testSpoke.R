library(neo4jService)
library(RUnit)
library(RCyjs)
#------------------------------------------------------------------------------------------------------------------------
if(!exists("ns"))
   ns <- neo4jService("localhost", 7474, user="neo4j", password="spoke@isb")

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_nodeAndEdgeCounts()
   test_nodeAndEdgeTables()
   test_getGraphDataFrames()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   message(sprintf("--- test_constructor"))

   checkEquals(is(ns), "neo4jService")
}
#------------------------------------------------------------------------------------------------------------------------
test_nodeAndEdgeCounts <- function()
{
   message(sprintf("--- test_nodeAndEdgeCounts"))

   checkTrue(neo4jService::getNodeCount(ns) > 2000000)
   checkTrue(neo4jService::getEdgeCount(ns) > 6000000)

   expectedNodeLabels <- c(":Anatomy", ":BiologicalProcess", ":CellularComponent", ":Compound",
                           ":Disease", ":Food", ":Gene", ":MolecularFunction",
                           ":Pathway", ":PharmacologicClass", ":Protein", ":SideEffect",
                           ":Symptom")

   expectedEdgeTypes <-  c("AFFECTS_CamG", "ASSOCIATES_DaG", "BINDS_CbP", "CAUSES_CcSE",
                           "CONTAINS_AcA", "CONTAINS_DcD", "CONTAINS_FcCM", "CONTRAINDICATES_CcD",
                           "COVARIES_GcG", "DOWNREGULATES_AdG", "DOWNREGULATES_CdG", "DOWNREGULATES_DdG",
                           "EXPRESSES_AeG", "INCLUDES_PCiC", "INTERACTS_CiP", "INTERACTS_GiG",
                           "INTERACTS_PiP", "ISA_AiA", "ISA_DiD", "LOCALIZES_DlA",
                           "PALLIATES_CpD", "PARTICIPATES_GpBP", "PARTICIPATES_GpCC", "PARTICIPATES_GpMF",
                           "PARTICIPATES_GpPW", "PARTOF_ApA", "PRESENTS_DpS", "REGULATES_GrG",
                           "RESEMBLES_CrC", "RESEMBLES_DrD", "TRANSLATEDFROM_PtG", "TREATS_CtD",
                           "UPREGULATES_AuG", "UPREGULATES_CuG", "UPREGULATES_DuG")

   checkEquals(getNodeLabels(ns), expectedNodeLabels)
   checkEquals(getEdgeTypes(ns), expectedEdgeTypes)

   tbl.nodeDist <- getNodeLabelDistribution(ns)
   checkEquals(dim(tbl.nodeDist), c(length(expectedLabels), 2))
   checkEquals(sum(tbl.nodeDist$count), neo4jService::getNodeCount(ns))

   tbl.edgeDist <- getEdgeTypeDistribution(ns)
   checkEquals(dim(tbl.edgeDist), c(length(expectedEdgeTypes), 2))
        # apparently the edge distributions count edges in both directions
   actual.vs.expected <- sum(tbl.edgeDist$count)/(2 * neo4jService::getEdgeCount(ns))
   checkEqualsNumeric(actual.vs.expected, 1.0, tol=1e-3)

} # test_nodeAndEdgeCounts
#------------------------------------------------------------------------------------------------------------------------
# ROR2 and GREB1 are mentioned prominently in paper about platinum sensitive & resistant ovarian tumors
# https://www.ncbi.nlm.nih.gov/pubmed/30056367
explore_ROR2_GREB1 <- function()
{
   query <- "match(n{name:'ROR2'})-[r]->(m{source:'Entrez Gene'}) return n,r,m"
   x <- query(ns, query)
   tbls <- getNodeAndEdgeTables(ns, query)

   tbl.nodes <- tbls$nodes
   tbl.edges <- tbls$edges

   stopifnot(all(c("startNode", "endNode", "type")  %in% colnames(tbl.edges)))

   colnames(tbl.edges)[grep("startNode", colnames(tbl.edges))] <- "source"
   colnames(tbl.edges)[grep("endNode", colnames(tbl.edges))] <- "target"
   colnames(tbl.edges)[grep("type", colnames(tbl.edges))] <- "interaction"
   tbl.nodes$label <- "tmp"
   gene.rows <- which(tbl.nodes$value == "Gene")
   tbl.nodes$label[gene.rows] <- tbl.nodes$name[gene.rows]

   g.json <- toJSON(dataFramesToJSON(tbl.edges, tbl.nodes))
   if(!exists("rcy"))
       rcy <- RCyjs(title="ROR2")

   deleteGraph(rcy)
   addGraph(rcy, g.json)
   layout(rcy, "cola")
   loadStyleFile(rcy, "style.json")
   checkEquals(RCyjs::getNodeCount(rcy), 3)
   checkEquals(RCyjs::getEdgeCount(rcy), 2)

   from <- "Gene{name: 'ROR2'}"
   to   <- "Gene{name: 'GREB1'}"

   sp <- shortestPath(ns, from, to)
   checkEquals(sp$name, c("ROR2", "Ectoderm Differentiation", "GREB1"))

   from <- "Gene{name: 'ROR2'}"
   to   <- "Gene{name: 'GREB1'}"

   f.up <- "~/github/nooa/explorations/pathway-enrichment/staff-upRegulated.tsv"
   up.5 <- head(read.table(f.up, sep="\t", as.is=TRUE, header=TRUE))$Gene.Name

   f.dn <- "~/github/nooa/explorations/pathway-enrichment/staff-downRegulated.tsv"
   dn.5 <- head(read.table(f.dn, sep="\t", as.is=TRUE, header=TRUE))$Gene.Name

   for(i in 1:6){
       for(j in 1:6){
          sourceNode <- sprintf("Gene{name: '%s'}", up.5[i])
          targetNode <- sprintf("Gene{name: '%s'}", dn.5[j])
          printf("====== %s -> %s", sourceNode, targetNode)
          sp <- shortestPath(ns, sourceNode, targetNode)
          print(sp)
          }}

   one result, for example
      # sp query: MATCH (source:Gene{name: 'LGSN'}), (destination:Gene{name: 'BNC1'}) CALL algo.shortestPath.stream(source, destination, NULL) YIELD nodeId, cost RETURN algo.getNodeById(nodeId)
      #   license identifier chromosome      name                                            description           source                                   url mesh_list
      # 1 CC0 1.0      51557          6      LGSN lengsin, lens protein with glutamine synthetase domain      Entrez Gene http://identifiers.org/ncbigene/51557      NULL
      # 2    <NA>   DOID:305       <NA> carcinoma                                                   <NA> Disease Ontology                                  <NA>   D009375
      # 3    <NA>   DOID:305       <NA> carcinoma                                                   <NA> Disease Ontology                                  <NA>   D002277
      # 4 CC0 1.0        646         15      BNC1                                           basonuclin 1      Entrez Gene   http://identifiers.org/ncbigene/646      NULL



} # explore_ROR2
#------------------------------------------------------------------------------------------------------------------------
test_getGraphDataFrames <- function()
{
    message(sprintf("--- test_getGraphDataFrames"))

    query <- "match (n{name:'Philip Seymour Hoffman'})-[r]->(m) return n, r, m"
    tbls <- getNodeAndEdgeTables(ns, query)
    checkEquals(names(tbls), c("nodes", "edges"))

    tbl.edges <- tbls$edges
    tbl.nodes <- tbls$nodes

    colnames(tbl.edges)[grep("startNode", colnames(tbl.edges))] <- "source"
    colnames(tbl.edges)[grep("endNode", colnames(tbl.edges))] <- "target"
    colnames(tbl.edges)[grep("type", colnames(tbl.edges))] <- "interaction"
    tbl.nodes$label <- "tmp"
    movie.rows <- which(tbl.nodes$value == "Movie")
    person.rows <- which(tbl.nodes$value == "Person")
    tbl.nodes$label[movie.rows] <- tbl.nodes$title[movie.rows]
    tbl.nodes$label[person.rows] <- tbl.nodes$name[person.rows]

    x <- fromJSON(dataFramesToJSON(tbl.edges, tbl.nodes))
    tbl.e <- x$elements$edges
    tbl.n <- x$elements$nodes

    checkEquals(as.character(tbl.e[1,1])[1:4], c("164-(ACTED_IN)-175", "164", "175", "ACTED_IN"))
    checkEquals(as.character(tbl.e[2,1])[1:4], c("164-(ACTED_IN)-163", "164", "163", "ACTED_IN"))

      # make sure that 164 is Hoffman, 176 and 163 are movies

    checkEquals(as.character(tbl.n[2,1])[1:4], c("164", "Person", "1967", "Philip Seymour Hoffman"))
    checkEquals(as.character(tbl.n[1,1])[c(1,2,8)], c("163", "Movie", "Twister"))
    checkEquals(as.character(tbl.n[3,1])[c(1,2,8)], c("175", "Movie", "Charlie Wilson's War"))

} # test_getGraphDataFrames
#------------------------------------------------------------------------------------------------------------------------
startRCy <- function()
{
   rcy <- RCyjs()

   query <- "match (n{name:'Philip Seymour Hoffman'})-[r]->(m) return n, r, m"
   tbls <- getNodeAndEdgeTables(ns, query)

   tbl.edges <- tbls$edges
   colnames(tbl.edges)[grep("startNode", colnames(tbl.edges))] <- "source"
   colnames(tbl.edges)[grep("endNode", colnames(tbl.edges))] <- "target"
   colnames(tbl.edges)[grep("type", colnames(tbl.edges))] <- "interaction"
   tbl.nodes <- tbls$nodes
   tbl.nodes$label <- "tmp"
   movie.rows <- which(tbl.nodes$value == "Movie")
   person.rows <- which(tbl.nodes$value == "Person")
   tbl.nodes$label[movie.rows] <- tbl.nodes$title[movie.rows]
   tbl.nodes$label[person.rows] <- tbl.nodes$name[person.rows]

   g.json <- toJSON(dataFramesToJSON(tbl.edges, tbl.nodes))
   deleteGraph(rcy)
   addGraph(rcy, g.json)
   layout(rcy, "cola")
   loadStyleFile(rcy, "style.json")
   checkEquals(RCyjs::getNodeCount(rcy), 3)
   checkEquals(RCyjs::getEdgeCount(rcy), 2)

} # startCyjs
#------------------------------------------------------------------------------------------------------------------------
test_neighborhood <- function()
{
   message(sprintf("--- test_neighborhood"))

   query <- 'match (n {name: "LUC7L3"})-[r]->(m) return n, r, m'
   x <- query(ns, query)
   x.tbl <- lapply(x, as.data.frame)

} # test_neighborhood
#------------------------------------------------------------------------------------------------------------------------
test_shortestPath <- function()
{
   message(sprintf("--- test_shortestPath"))
   labels <- getNodeLabels(ns)
     #  [1] ":Anatomy"            ":BiologicalProcess"  ":CellularComponent"  ":Compound"
     #  [5] ":Disease"            ":Food"               ":Gene"               ":MolecularFunction"
     #  [9] ":Pathway"            ":PharmacologicClass" ":Protein"            ":SideEffect"
     # [13] ":Symptom"

     # a trivial case to start: just one hop from n{name: 'LUC7L3'} to m{identifier: 'GO:0008380'}, "RNA splicing"
     # found 1 node: query(ns, "match (n {name:'LUC7L3'}) return n")
     # found 1 node: query(ns, "match (n {identifier:'GO:0008380'}) return n")

     # query(ns, "match (n:Gene{name:'LUC7L3'}) return n")
     #
   from <- "LUC7L3"
   to   <- "GO:0008380"
   s <- paste(sprintf("MATCH (source:Gene {id: '%s'}), (destination:BiologicalProcess {identifer: '%s'})", from, to),
              "CALL algo.shortestPath.stream(source, destination) YIELD nodeId",
              "RETURN algo.getNodeById(nodeId).id AS node")
   query(ns, s)

} # test_shortestPath
#------------------------------------------------------------------------------------------------------------------------
# streamToDataFrames <- function(queryResult)
# {
#     browser()
#     stopifnot(all(c("nodes", "relationships", "properties", "data") %in% names(queryResult)))
#     node.count <- queryResult$nodes[1,1]
#     edge.count <- queryResult$relationships[1,1]
#     prop.count <- queryResult$properties[1,1]
#     data.raw <- queryResult$data[1,1]
#     lines.raw <- strsplit(data.raw, "\n")[[1]]
#     lines.tokens <- strsplit(lines.raw, ",")
#
#     column.names.raw <- lines.tokens[[1]]
#     column.names <- unlist(lapply(column.names.raw, function(s) gsub("\"", "", s)))
#
#     unlist(lapply(lines.tokens[[2]], function(s) gsub("\"", "", s)))
#     #tbl.nodes <- data.frame("",
#
#     xyz <- 99
#
# } # streamToDataFrames
# #------------------------------------------------------------------------------------------------------------------------
# test_streamToDataFrames <- function()
# {
#    message(sprintf("--- test_streamToDataFrames"))
#
#      # https://neo4j-rstats.github.io/user-guide/retrieve.html
#
#    qs <- paste('MATCH (tom:Person {name:"Tom Hanks"})-[a:ACTED_IN]->(m)<-[:ACTED_IN]-(coActors)',
#                'RETURN m AS acted,coActors.name')
#
#    res <- call_neo4j(ns@state$db, query, type = "graph")
#    unnest_nodes(res$nodes)
#
#
#
#
#
#    q <- paste0("MATCH (person:Person)-[role:PLAYED]->(movie:Movie) ",
#                "WITH collect(DISTINCT person) AS people, collect(DISTINCT movie) AS movies, ",
#                "collect(role) AS roleRels ",
#                "CALL apoc.export.csv.data(people + movies, roleRels, null, {stream: true}) ",
#                "YIELD file, nodes, relationships, properties, data ",
#                "RETURN file, nodes, relationships, properties, data")
#    x <- query(ns, q)
#    #load("streamQueryResults.RData")
#
#    q <- paste0("MATCH (n)-[r]->(m) ",
#                "WITH collect(DISTINCT n) AS a, collect(DISTINCT m) AS b, ",
#                "collect(r) AS edges ",
#                "CALL apoc.export.csv.data(a + b, edges, null, {stream: true}) ",
#                "YIELD file,  nodes, relationships, properties, data ",
#                "RETURN file, nodes, relationships, properties, data")
#    x <- query(ns, q)
#
#    q <- paste0("MATCH (n)-[r]->(m) ",
#                "WITH collect(DISTINCT n) AS a, collect(DISTINCT m) AS b, ",
#                "collect(r) AS edges ",
#                "RETURN a, edges, b, properties, data")
#    query(ns, q)
#
#
#    q <- paste('MATCH (people:Person)-[relatedTo]-(:Movie {title: "Cloud Atlas"}) ',
#               'RETURN people.name, Type(relatedTo), relatedTo')
#   x <- call_neo4j(q, ns@state$db, type = "graph")
#   unnest_graph(x)
#
#
#    tbls <- streamToDataFrames(x)
#
# } # test_streamToDataFrames
# #------------------------------------------------------------------------------------------------------------------------
if(!interactive())
   runTests()
