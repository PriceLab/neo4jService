library(neo4jService)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
if(!exists("ns"))
   ns <- neo4jService("localhost", 7474, user="neo4j", password="spoke@isb")

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_runCypherFile()
   test_nodeAndEdgeTables()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   message(sprintf("--- test_constructor"))

   checkEquals(is(ns), "neo4jService")
   checkTrue(nodeCount(ns) > 2000000)
   checkTrue(edgeCount(ns) > 6000000)

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_nodeAndEdgeTables <- function()
{
   message(sprintf("--- test_nodeAndEdgeTables"))

   tbl.nodes <- getNodeTable(ns)
   tbl.edges <- getEdgeTable(ns)

   checkEquals(dim(tbl.nodes), c(4, 6))
   checkEquals(dim(tbl.edges), c(3, 4))

} # test_nodeAndEdgeTables
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
if(!interactive())
   runTests()
