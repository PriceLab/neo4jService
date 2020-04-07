library(neo4jService)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
if(!exists("ns"))
   ns <- neo4jService("localhost", 7477, user="neo4j", password="hoopa")

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_nodeAndEdgeCounts()
   #test_nodeAndEdgeTables()

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

   checkEquals(getNodeCount(ns), 1035)
   checkEquals(getEdgeCount(ns), 3139)

   expectedLabels <- c(":Category", ":Customer", ":Order", ":Product", ":Supplier")
   expectedTypes <- c("ORDERS", "PART_OF", "PURCHASED", "SUPPLIES")

   checkEquals(getNodeLabels(ns), expectedLabels)
   checkEquals(getEdgeTypes(ns), expectedTypes)

   tbl.nodeDist <- getNodeLabelDistribution(ns)
   checkEquals(dim(tbl.nodeDist), c(length(expectedLabels), 2))
   checkEquals(tbl.nodeDist$count, c(8, 91, 830, 77, 29))

   tbl.edgeDist <- getEdgeTypeDistribution(ns)
   checkEquals(dim(tbl.edgeDist), c(length(expectedTypes), 2))
   checkEquals(tbl.edgeDist$count, c(4310, 154, 1660, 154))

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_nodeAndEdgeTables <- function()
{
   message(sprintf("--- test_nodeAndEdgeTables"))

   query <- "match (n:Customer{contactName:'Ana Trujillo'})-[r]->(m) return n,r,m"

   tbls <- getNodeAndEdgeTables(ns, query)
   checkEquals(names(tbls), c("nodes", "edges"))

   tbl.edges <- tbls$edges
   tbl.nodes <- tbls$nodes

   checkEquals(dim(tbl.edges), c(4, 5))
   checkEquals(dim(tbl.nodes), c(5, 27))

} # test_nodeAndEdgeTables
#------------------------------------------------------------------------------------------------------------------------
test_triangles <- function()
{
   message(sprintf("--- test_triangles"))

   # these queries work:
   #   query(ns, "match (m:Customer)-[r:PURCHASED]-(n:Order) return m,n,type(r) limit 3")
   #   query(ns, "match (m:Order)-[r:ORDERS]-(n:Product) return m,n,type(r) limit 3")
   #
   # but this does not:

   #s <- paste("CALL algo.triangle.stream('Customer','PURCHASED') YIELD nodeA, nodeB, nodeC",
   s <- paste("CALL algo.triangle.stream('Customer', null) YIELD nodeA, nodeB, nodeC",
              "RETURN algo.getNodeById(nodeA).id AS nodeA,",
              "algo.getNodeById(nodeB).id AS nodeB, algo.getNodeById(nodeC).id AS nodeC")

   x <- query(ns, s)
   x

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
