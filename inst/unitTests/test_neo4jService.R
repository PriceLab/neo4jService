library(neo4jService)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
# preconditions:  that the a fully-function scratch database is up and running on public port 7900
# do this:
#   1) cd ~/github/neo4jService/instances/scratch
#   2) docker stop scratch_neo4j; docker rm scratch_neo4j
#   3) make start
#   4) make nodeCount
#   5) make   # with default (no) target reports the available targets
#      stop
#      start
#      bash
#      cypher
#      nodeCount
#      fill.local
#      delete.db
#      ui
#------------------------------------------------------------------------------------------------------------------------
if(!exists("ns"))
   ns <- neo4jService("localhost", 7900, user="neo4j", password="hoopa")

query(ns, "match (n) detach delete n")
stopifnot(getNodeCount(ns) == 0)

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_simpleQuery()
   test_runCypherFile()
   test_getNodeAndEdgeTables()
   #test_shortestPath()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   message(sprintf("--- test_constructor"))

   checkEquals(is(ns), "neo4jService")

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_simpleQuery <- function()
{
   message(sprintf("--- test_simpleQuery"))

   query(ns, "match (n) return count (n)")

} # test_simpleQuery
#------------------------------------------------------------------------------------------------------------------------
test_query.raw <- function()
{
   message(sprintf("--- test_query.raw"))

   query.raw(ns, "match (n) return count (n)")

} # test_simpleQuery
#------------------------------------------------------------------------------------------------------------------------
test_runCypherFile <- function()
{
   message(sprintf("--- test_runCypherFile"))

   deleteAll(ns, confirm=FALSE)
   checkEquals(getNodeCount(ns), 0)
   checkEquals(getEdgeCount(ns), 0)

   cypher.file <- "~/github/neo4jService/inst/extdata/import/createTwoActors.cypher"
   runCypherFile(ns, cypher.file)

   checkEquals(getNodeCount(ns), 4)
   checkEquals(getEdgeCount(ns), 3)

} # test_runCypherFile
#------------------------------------------------------------------------------------------------------------------------
test_getNodeAndEdgeTables <- function()
{
   message(sprintf("--- test_getNodeAndEdgeTables"))

   deleteAll(ns, confirm=FALSE)
   checkEquals(getNodeCount(ns), 0)
   checkEquals(getEdgeCount(ns), 0)

   cypher.file <- "~/github/neo4jService/instances/scratch/import/createTwoActors.cypher"
   runCypherFile(ns, cypher.file)

     #------------------------------------------------------------
     #  first test one edge after extracting the whole graph
     #------------------------------------------------------------

   query <- "match (n)-[r]->(m) return n, r, m"  # the whole graph
   x <- getNodeAndEdgeTables(ns, query)
   checkEquals(sort(names(x)), c("edges", "nodes"))

   tbl.nodes <- x$nodes
   tbl.edges <- x$edges

   checkEquals(dim(tbl.nodes), c(7, 7))
   checkEquals(dim(tbl.edges), c(3, 6))

      # check Hanks in Forrest Gump: source->target in correct order?
   tomHanks <- unique(subset(tbl.nodes, name=="Tom Hanks")$id)
   forrestGump <- unique(subset(tbl.nodes, title=="Forrest Gump" & value=="Movie")$id)
   checkEquals(subset(tbl.edges, startNode==tomHanks)$type, "ACTED_IN")
   checkEquals(subset(tbl.edges, startNode==tomHanks)$endNode, forrestGump)

     #------------------------------------------------------------
     # now extract one edge only
     #------------------------------------------------------------

   query <- "match (n{name: 'Tom Hanks'})-[r]->(m) return n, r, m"  # the whole graph
   x <- getNodeAndEdgeTables(ns, query)
   checkEquals(sort(names(x)), c("edges", "nodes"))

   tbl.nodes <- x$nodes
   tbl.edges <- x$edges

   source.id <- tbl.edges$startNode[1]
   target.id <- tbl.edges$endNode[1]

   source.name <- subset(tbl.nodes, id==source.id)$name[1]
   target.name <- subset(tbl.nodes, id==target.id)$title[1]

   checkEquals(source.name, "Tom Hanks")
   checkEquals(target.name, "Forrest Gump")

} # test_nodeAndEdgeTables
#------------------------------------------------------------------------------------------------------------------------
test_shortestPath <- function()
{
   message(sprintf("--- test_shortestPath"))

   sourceNode <- "Person{name:'Tom Hanks'}"
   targetNode <- "Movie{title:'Forrest Gump'}"
   tbl.sp <- shortestPath(ns, sourceNode, targetNode)

} # test_shortestPath
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
   runTests()
