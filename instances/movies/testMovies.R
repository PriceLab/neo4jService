library(neo4jService)
library(RUnit)
library(RCyjs)
#------------------------------------------------------------------------------------------------------------------------
if(!exists("ns"))
   ns <- neo4jService("localhost", 7998, user="neo4j", password="hoopa")

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_nodeAndEdgeCounts()
   #test_nodeAndEdgeTables()
   test_getGraphDataFrames()
   #test_triangles()

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

   checkEquals(neo4jService::getNodeCount(ns), 171)
   checkEquals(neo4jService::getEdgeCount(ns), 253)

   expectedLabels <- c(":Movie", ":Person")
   expectedTypes <- c("ACTED_IN", "DIRECTED", "FOLLOWS", "PRODUCED", "REVIEWED", "WROTE")

   checkEquals(getNodeLabels(ns), expectedLabels)
   checkEquals(getEdgeTypes(ns), expectedTypes)

   tbl.nodeDist <- getNodeLabelDistribution(ns)
   checkEquals(dim(tbl.nodeDist), c(length(expectedLabels), 2))
   checkEquals(tbl.nodeDist$count, c(38, 133))

   tbl.edgeDist <- getEdgeTypeDistribution(ns)
   checkEquals(dim(tbl.edgeDist), c(length(expectedTypes), 2))
   checkEquals(tbl.edgeDist$count, c(344, 88, 6, 30, 18, 20))

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_nodeAndEdgeTables <- function()
{
   message(sprintf("--- test_nodeAndEdgeTables"))


   qs <- paste0("CALL apoc.export.csv.all(null, {stream:true}) ",
                "YIELD file, nodes, relationships, properties, data ",
                "RETURN file, nodes, relationships, properties, data")

   #s <- "CALL apoc.export.csv.all(null, {stream:true}) YIELD file, nodes, relationships, properties, data RETURN file, nodes, relationships, properties, data"
   x <- query(ns, qs)
   # x[2:4]
   # nodes: 8
   # relationhips: 8
   # properties 23

   s <- x$data[1,1] # 738
   lines <- strsplit(s, "\n")[[1]]
   length(lines)

    #  [1] "\"_id\",\"_labels\",\"id\",\"name\",\"title\",\"year\",\"_start\",\"_end\",\"_type\",\"role\""
    #  [2] "\"0\",\":Person\",\"1\",\"Charlie Sheen\",\"\",\"\",,,,"
    #  [3] "\"1\",\":Person\",\"2\",\"Michael Douglas\",\"\",\"\",,,,"
    #  [4] "\"2\",\":Person\",\"3\",\"Martin Sheen\",\"\",\"\",,,,"
    #  [5] "\"3\",\":Person\",\"4\",\"Morgan Freeman\",\"\",\"\",,,,"
    #  [6] "\"20\",\":Country\",\"\",\"USA\",\"\",\"\",,,,"
    #  [7] "\"21\",\":Movie\",\"1\",\"\",\"Wall Street\",\"1987\",,,,"
    #  [8] "\"22\",\":Movie\",\"2\",\"\",\"The American President\",\"1995\",,,,"
    #  [9] "\"23\",\":Movie\",\"3\",\"\",\"The Shawshank Redemption\",\"1994\",,,,"
    # [10] ",,,,,,\"21\",\"20\",\"MADE_IN\",\"\""
    # [11] ",,,,,,\"22\",\"20\",\"MADE_IN\",\"\""
    # [12] ",,,,,,\"23\",\"20\",\"MADE_IN\",\"\""
    # [13] ",,,,,,\"0\",\"21\",\"PLAYED\",\"Bud Fox\""
    # [14] ",,,,,,\"3\",\"21\",\"PLAYED\",\"Carl Fox\""
    # [15] ",,,,,,\"2\",\"21\",\"PLAYED\",\"Gordon Gekko\""
    # [16] ",,,,,,\"3\",\"22\",\"PLAYED\",\"A.J. MacInerney\""
    # [17] ",,,,,,\"2\",\"22\",\"PLAYED\",\"President Andrew Shepherd\""


   qs2 <- paste0("MATCH (person:Person)-[role:PLAYED]->(movie:Movie) ",
                 "WITH collect(DISTINCT person) AS people, collect(DISTINCT movie) AS movies, ",
                 "collect(role) AS roleRels ",
                 "CALL apoc.export.csv.data(people + movies, roleRels, null, {stream: true}) ",
                 "YIELD file, nodes, relationships, properties, data ",
                 "RETURN file, nodes, relationships, properties, data")

   x2 <- query(ns, qs2)
   x2[2:4]
   # nodes: 5
   # relationhips: 5
   # properties 17
   lines <- strsplit(x2$data[1,1], "\n")[[1]]

   s <- x$data[1,1] # 738
   lines <- strsplit(s, "\n")[[1]]
   lines.tokens <- strsplit(lines, ",")
   lapply(lines.tokens, length)
   length(lines)

} # test_nodeAndEdgeTables
#------------------------------------------------------------------------------------------------------------------------
test_getGraphDataFrames <- function()
{
    message(sprintf("--- test_getGraphDataFrames"))

     #-----------------------------
     # first, the whole database
     #-----------------------------

    query <- "match (n)-[r]->(m) return n, r, m"
    tbls <- getNodeAndEdgeTables(ns, query)
    checkEquals(dim(tbls$nodes), c(171, 9))
    checkEquals(dim(tbls$edges), c(273, 8))

     #-----------------------------
     # now, just Hoffman films
     #-----------------------------

    query <- "match (n{name:'Philip Seymour Hoffman'})-[r]->(m) return n, r, m"
    tbls <- getNodeAndEdgeTables(ns, query)
    checkEquals(names(tbls), c("nodes", "edges"))

    tbl.edges <- tbls$edges
    tbl.nodes <- tbls$nodes

     #--------------------------------------------
     # manipulate colums names, ready for RCyjs
     #--------------------------------------------

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

      #--------------------------------------------------------------------
      # make sure that 164 is Hoffman, 176 and 163 are movies
      # if the graph database has been used for algorithms (triangles,
      # shortest paths) then (alas, because these calculations change
      # graph state, allow for unexpected columns in these following tests
      #--------------------------------------------------------------------

    checkTrue(all(c("164", "Person", "1967", "Philip Seymour Hoffman") %in% as.character(tbl.n[2,1])))
    checkTrue(all(c("163", "Movie", "Twister") %in% as.character(tbl.n[1,1])))
    checkTrue(all(c("175", "Movie", "Charlie Wilson's War") %in% as.character(tbl.n[3,1])))

} # test_getGraphDataFrames
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

   labels <- getNodeLabels(ns)    # :Movie :Person
   from <- "Tom Hanks"
   to   <- "Geena Davis"
     # make sure the nodes can be found
     # query(ns, "match (n:Person) where n.name in ['Geena Davis', 'Tom Hanks'] return n")
     # query(ns, "match (n:Person{name:'Geena Davis'}) return n")
     # query(ns, "match (a:Person{name:'Geena Davis'}), (b:Person{name:'Tom Hanks'}) return a,b")
     # query(ns, sprintf("match (a:Person{name:'%s'}), (b:Person{name:'%s'}) return a,b",
     #                "Geena Davis", "Tom Hanks"))

     # learn the details (signature & results) of algo.shortestPath.stream
     #    query(ns, "CALL dbms.procedures() YIELD name, signature WHERE name='algo.shortestPath.stream' RETURN signature")

   s2 <- sprintf("match (a:Person{name:'%s'}), (b:Person{name: '%s'}) CALL algo.shortestPath.stream(a, b) YIELD nodeId RETURN nodeId", from, to)

   clearSelection(rcy)
   selectNodes(rcy, query(ns, s2)$value)
   getSelectedNodes <- getSelectedNodes(rcy)$id


} # test_shortestPath
#------------------------------------------------------------------------------------------------------------------------
startRCy <- function()
{
   rcy <- RCyjs()

   query <- "match (n)-[r]->(m) return n, r, m"
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
   loadStyleFile(rcy, "style.json")
   #Cyjs::getNodeCount(rcy), 171)
   #checkEquals(RCyjs::getEdgeCount(rcy), 253)
   layout(rcy, "cola")

   rcy

} # startCyjs
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
   runTests()
