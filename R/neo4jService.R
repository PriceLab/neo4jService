#------------------------------------------------------------------------------------------------------------------------
#' @import methods
#' @import neo4r
#'
#' @title neo4jService-class
#'
#' @name neo4jService-class
#' @rdname neo4jService-class
#' @aliases neo4jService
#' @exportClass neo4jService
#'

.neo4jService <- setClass("neo4jService",
                             representation=representation(
                                host="character",
                                port="numeric",
                                #db="Neo4JAPI",  # an R6 object, does not work.
                                state="environment",
                                quiet="logical"
                                )
                             )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('query',     signature='obj', function(obj, s) standardGeneric('query'))
setGeneric('query.raw', signature='obj', function(obj, s) standardGeneric('query.raw'))
setGeneric('deleteAll', signature='obj', function(obj, confirm=TRUE) standardGeneric('deleteAll'))
setGeneric('getNodeCount', signature='obj', function(obj) standardGeneric('getNodeCount'))
setGeneric('getEdgeCount', signature='obj', function(obj, directed=TRUE) standardGeneric('getEdgeCount'))
setGeneric('fullGraph', signature='obj', function(obj) standardGeneric('fullGraph'))
setGeneric('getNodeLabels', signature='obj', function(obj) standardGeneric('getNodeLabels'))
setGeneric('getNodeLabelDistribution', signature='obj', function(obj) standardGeneric('getNodeLabelDistribution'))
setGeneric('getEdgeTypes', signature='obj', function(obj) standardGeneric('getEdgeTypes'))
setGeneric('getEdgeTypeDistribution', signature='obj', function(obj) standardGeneric('getEdgeTypeDistribution'))
setGeneric('getNodeAndEdgeTables', signature='obj', function(obj, query) standardGeneric('getNodeAndEdgeTables'))

# setGeneric('getNodeTable', signature='obj', function(obj) standardGeneric('getNodeTable'))
# setGeneric('getEdgeTable', signature='obj', function(obj, directed=TRUE) standardGeneric('getEdgeTable'))
setGeneric('runCypherFile', signature='obj', function(obj, filename) standardGeneric('runCypherFile'))
#------------------------------------------------------------------------------------------------------------------------
#' create an object of class neo4jService, connect with user and password
#'
#' @description
#' Expression, variant and covariate data for the genes of interest (perhaps unbounded) for pre-term birth studies
#'
#' @rdname neo4jService-class
#'
#' @param host character string
#' @param port numeric
#' @param user character string
#' @param password character string
#' @param quiet logical, default TRUE
#'
#' @export
#'
#' @return An object of the neo4jService class
#'

neo4jService <- function(host, port, user, password, quiet=TRUE)
{
   url <- sprintf("http://%s:%d", host, port)
   state <- new.env(parent=emptyenv())

   tryCatch({
      db <- neo4j_api$new(url, user=user, password=password)
      },
   error = function(e){
      print(e)
      stop()
      })

   state$db <- db

   .neo4jService(host=host, port=port, state=state, quiet=quiet)

} # neo4jService, the constructor
#------------------------------------------------------------------------------------------------------------------------
#' send a query to the database, return it without interpretation or reformatting
#'
#' @description
#' send a well-formed "cypher" query
#'
#' @rdname query.raw
#'
#' @param obj  new4jService object
#' @param s  character string
#'
#' @export
#'
#' @return one or more data.frames
#'

setMethod('query.raw', 'neo4jService',

     function(obj, s){
         suppressMessages(x <- call_neo4j(s, obj@state$db))
         return(x)
         }) # query.raw

#------------------------------------------------------------------------------------------------------------------------
#' send a query to the database
#'
#' @description
#' send a well-formed "cypher" query
#'
#' @rdname query
#'
#' @param obj  new4jService object
#' @param s  character string
#'
#' @export
#'
#' @return one or more data.frames
#'

setMethod('query', 'neo4jService',

     function(obj, s){
         suppressMessages(x <- call_neo4j(s, obj@state$db))
         if(length(x) == 0)
            return(data.frame())
         tbls <- lapply(x, as.data.frame)
         if(length(tbls) == 1)
            return(tbls[[1]])
         return(tbls)
         }) # query

#------------------------------------------------------------------------------------------------------------------------
#' permanently deletes all nodes (and thus all edges)
#'
#' @description
#' detaches and deletes all nodes, and thus all edges
#'
#' @rdname deleteAll
#'
#' @param obj  new4jService object
#' @param confirm  logical, ask user to confirm
#'
#' @export
#'
#' @return nothing
#'
setMethod('deleteAll', 'neo4jService',

     function(obj, confirm=TRUE){
         if(confirm)
            readline(prompt="Really delete all? Press [enter] to continue")
         ignore <- query(obj, "match (n) detach delete n")
         }) # deleteAll

#------------------------------------------------------------------------------------------------------------------------
#' how many nodes in the current graph?
#'
#' @description
#' node count returned
#'
#' @rdname getNodeCount
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return the node count
#'
setMethod('getNodeCount', 'neo4jService',

      function(obj){
         return(query(obj, "match (n) return count(n)")$value)
         }) # getNodeCount

#------------------------------------------------------------------------------------------------------------------------
#' how many edges in the current graph?
#'
#' @description
#' edge count returned, twice as many when undirected
#'
#' @rdname getEdgeCount
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return the edge count
#'
setMethod('getEdgeCount', 'neo4jService',

       function(obj, directed=TRUE) {
         queryString <- "match ()-[r]->() return count(r)"
         if(!directed)
             queryString <- "match ()-[r]-() return count(r)"
         return(query(obj, queryString)$value)
         }) # getEdgeCount

#------------------------------------------------------------------------------------------------------------------------
#' get a 'raw' query result, all edges
#'
#' @description
#' returns all the relationships (edges) between all nodes
#'
#' @rdname fullGraph
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return one or more data.frames
#'
setMethod('fullGraph', 'neo4jService',

     function(obj) {
         return(query(obj, "match (n)-[r]-(m) return r"))
         }) # fullGraph

#------------------------------------------------------------------------------------------------------------------------
#' execute all the cypher commands in the named file
#'
#' @description
#' execute all the cypher commands in the named file
#'
#' @rdname runCypherFile
#'
#' @param obj  new4jService object
#' @param obj  filename character
#'
#' @export
#'
#' @return not sure...
#'
setMethod('runCypherFile', 'neo4jService',

     function(obj, filename){
        suppressMessages(
          result <- send_cypher(filename, obj@state$db, type=c("row"), output="r",
                                include_stats=TRUE, meta=FALSE))
        query(obj, "match (n) set n.id = id(n)")
        invisible(result)
        }) # runCypherFile

#------------------------------------------------------------------------------------------------------------------------
#' labels seem to be types, categories
#'
#' @description
#' rerutn a sorted uniqued list of label (category) names
#'
#' @rdname getNodeLabels
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return character vector
#'
setMethod('getNodeLabels', 'neo4jService',

      function(obj){
          tbl.raw <- query(obj, "match (n) return distinct labels(n)")
          if(nrow(tbl.raw) == 0)
              return(c())

          labels <- sort(unique(unlist(lapply(seq_len(nrow(tbl.raw)), function(r) paste(tbl.raw[r,], collapse=":")))))
          labels <- gsub(":NA", "", labels, fixed=TRUE)
          labels <- paste(":", labels, sep="")

          return(labels)
          }) # getNodeLabels

#------------------------------------------------------------------------------------------------------------------------
#' getNodeLabelDistribution
#'
#' @description
#' report how many nodes are annotated to each label
#'
#' @rdname getNodeLabelDistribution
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return a data.frame
#'
setMethod('getNodeLabelDistribution', 'neo4jService',

      function(obj){
          nodeLabels <- getNodeLabels(obj)
          counts <- lapply(nodeLabels, function(label)
                             query(obj, sprintf("match (n%s) return count(n)", label))$value)
          names(counts) <- nodeLabels
          data.frame(label=names(counts), count=as.numeric(counts), stringsAsFactors=FALSE)
          }) # getNodeLabelDistribution

#------------------------------------------------------------------------------------------------------------------------
#' getEdgeTypes
#'
#' @description
#' rerutn a sorted uniqued list of edge types
#'
#' @rdname getEdgeTypes
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return character vector
#'
setMethod('getEdgeTypes', 'neo4jService',

      function(obj){
          tbl.raw <- query(obj, "match ()-[r]-() return distinct type(r)")
          if(nrow(tbl.raw) == 0)
              return(c())
          return(sort(tbl.raw$value))
          }) # getEdgeTypes

#------------------------------------------------------------------------------------------------------------------------
#' getEdgeTypeDistribution
#'
#' @description
#' report how many nodes are annotated to each label
#'
#' @rdname getEdgeTypeDistribution
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return a data.frame
#'
setMethod('getEdgeTypeDistribution', 'neo4jService',

      function(obj){
          edgeTypes <- getEdgeTypes(obj)
          counts <- lapply(edgeTypes, function(type)
                             query(ns, sprintf("match ()-[r:%s]-() return count(r)", type))$value)
          names(counts) <- edgeTypes
          data.frame(type=names(counts), count=as.numeric(counts), stringsAsFactors=FALSE)
          }) # getEdgeTypeDistribution

#------------------------------------------------------------------------------------------------------------------------
#' return a list of two data.frames, nodes and edges, suitable for rendering in RCyjs
#'
#' @description
#' transform the neo4j data structures into a two base R data.frames
#'
#' @rdname getNodeAndEdgeTables
#'
#' @param obj  neo4jService object
#' @param query character string, a valid neo4j query
#'
#' @export
#'
#' @return a list of two data.frames
#'
setMethod('getNodeAndEdgeTables', 'neo4jService',

       function(obj, query){
           x <- call_neo4j(query, obj@state$db, type="graph")
           x.graph <- unnest_graph(x)
           tbl.nodes <- as.data.frame(x.graph$nodes)
           tbl.edges <- as.data.frame(x.graph$relationships)

           list(nodes=tbl.nodes, edges=tbl.edges)
           }) # getNodeAndEdgeTables

#------------------------------------------------------------------------------------------------------------------------
#' return a reusable data.frame listing nodes and their properties
#'
#' @description
#' nodes and their properties
#'
#' @rdname getNodeTable
#'
#' @param obj  new4jService object
#'
#' @export
#'
#' @return a data.frame
#'
# setMethod('getNodeTable', 'neo4jService',
#
#      function(obj){
#         # add id property to every node if not already present
#         if(nrow(query(obj, "MATCH (n) return(n.id)")) == 0){
#           query(obj, "match (n) set n.id = id(n)")
#           }
#        labels <- getNodeLabels(obj)
#        build.label.table <- function(label){
#          tbl <- query(obj, sprintf("match (n%s) return n", label))
#          tbl$label <- label
#          tbl
#          }
#
#        x <- lapply(labels, build.label.table)
#
#        column.names <- sort(unique(unlist(lapply(x, colnames))))
#        tbl <- setNames(data.frame(matrix(ncol=length(column.names), nrow=0)), column.names)
#        for(tbl.sub in x)
#           tbl <- merge(tbl, tbl.sub, all.x=TRUE, all.y=TRUE)
#
#        column.names <- colnames(tbl)  # may have changed due to the merge
#           # put the table in the right column order, starting with id and label
#        id.index <- grep("^id$", column.names)
#        label.index <- grep("^label$", column.names)
#        other.indices <- seq_len(length(column.names))[-c(id.index, label.index)]
#        other.colnames <- sort(column.names[other.indices])
#        preferred.colnames <- c("id", "label", other.colnames)
#        tbl <- tbl[, preferred.colnames]
#        preferred.row.order <- order(tbl$id)
#        tbl <- tbl[preferred.row.order,]
#        rownames(tbl) <- NULL
#        tbl
#        }) # getNodeTable
#
# #------------------------------------------------------------------------------------------------------------------------
# #' return a reusable data.frame listing edges and their properties
# #'
# #' @description
# #' edges and their properties
# #'
# #' @rdname getEdgeTable
# #'
# #' @param obj  new4jService object
# #' @param directed  logical, default TRUE
# #'
# #' @export
# #'
# #' @return a data.frame
# #'
# setMethod('getEdgeTable', 'neo4jService',
#
#        function(obj, directed=TRUE){
#            x <- query(obj, "match (m)-[r]-(n) return m, n, r, type(r)")
#            attribute.names <- colnames(x$r)
#
#            tbl <- data.frame(a=x$m$id, b=x$n$id, type=x$type$value, stringsAsFactors=FALSE)
#            browser()
#            for(eda in attribute.names){
#               tbl <- cbind(tbl, x$r[, eda])
#               }
#
#            sigs <- vector("character", nrow(tbl))
#
#            if(directed){
#               browser()
#               for(r in seq_len(nrow(tbl))){
#                  ordered.nodes <- sort(c(tbl[r, "a"], tbl[r, "b"]))
#                  sigs[r] <- sprintf("%s:%s:%s", tbl[r, "type"], ordered.nodes[1], ordered.nodes[2])
#                  } # for r
#               browser()
#               deleters <- which(duplicated(sigs))
#               if(length(deleters) > 0)
#                   tbl <- tbl[-deleters,]
#               } # if directed
#
#            colnames(tbl)[1:3] <- c("source", "target", "interaction") # required by rcyjs
#            return(tbl)
#            }) # getEdgeTable
#
#------------------------------------------------------------------------------------------------------------------------

