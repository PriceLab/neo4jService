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
