library(neo4jService)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
# make sure that a scratch neo4j database is up and running on the port we want:
# ~/github/neo4jService/inst/extdata/runDocker.sh
# #!/bin/bash
#
# the data directory must contain two neo4j-specific subdirectories: databases and dbms
# when used here, docker apparently adds attributes to that data driectory
# i had thought this was a problem, but it is not
#
# csv files to import must be in <neo4j-home>/import
# where the docker image has NEO4J_HOME
#   NEO4J_HOME=/var/lib/neo4j
#
# NAME=neo4jservicetests
#
# docker run --name=$NAME \
#     --detach \
#     --publish=7499:7474 --publish=7699:7687 \
#     --volume=/Users/paul/github/neo4jService/inst/extdata/data:/data \
#     --volume=/Users/paul/github/neo4jService/inst/extdata/logs:/logs \
#     --user=neo4j \
#     --env NEO4J_AUTH=neo4j/hoopa \
#     --env 'NEO4JLABS_PLUGINS=["apoc", "graph-algorithms"]' \
#     --env NEO4J_dbms_security_procedures_unrestricted=apoc.\\\*,algo.\\\* \
#     neo4j:3.5.12
#------------------------------------------------------------------------------------------------------------------------
if(!exists("ns"))
   ns <- neo4jService("localhost", 7499, user="neo4j", password="hoopa")

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_simpleQuery()
   test_runCypherFile()
   test_nodeAndEdgeTables()

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

   deleteAll(ns)
   checkEquals(nodeCount(ns), 0)
   checkEquals(edgeCount(ns), 0)

   cypher.file <- "~/github/neo4jService/inst/extdata/import/createTwoActors.cypher"
   runCypherFile(ns, cypher.file)

   checkEquals(nodeCount(ns), 4)
   checkEquals(edgeCount(ns), 3)

} # test_runCypherFile
#------------------------------------------------------------------------------------------------------------------------
test_nodeAndEdgeTables <- function()
{
   message(sprintf("--- test_nodeAndEdgeTables"))
   deleteAll(ns, confirm=FALSE)
   checkEquals(nodeCount(ns), 0)
   checkEquals(edgeCount(ns), 0)

   cypher.file <- "~/github/neo4jService/inst/extdata/import/createTwoActors.cypher"
   runCypherFile(ns, cypher.file)

   tbl.nodes <- getNodeTable(ns)
   tbl.edges <- getEdgeTable(ns)
   checkEquals(dim(tbl.nodes), c(4, 6))
   checkEquals(dim(tbl.edges), c(3, 4))

} # test_nodeAndEdgeTables
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
   runTests()
