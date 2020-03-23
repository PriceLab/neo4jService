#!/bin/bash

# the data directory must contain two neo4j-specific subdirectories: databases and dbms
# when used here, docker apparently adds attributes to that data driectory
# i had thought this was a problem, but it is not
#
# csv files to import must be in <neo4j-home>/import
# where the docker image has NEO4J_HOME
#   NEO4J_HOME=/var/lib/neo4j

NAME=neo4jservicetests

docker run --name=$NAME \
    --detach \
    --publish=7499:7474 --publish=7699:7687 \
    --volume=/Users/paul/github/neo4jService/inst/extdata/data:/data \
    --volume=/Users/paul/github/neo4jService/inst/extdata/logs:/logs \
    --user=neo4j \
    --env NEO4J_AUTH=neo4j/hoopa \
    --env 'NEO4JLABS_PLUGINS=["apoc", "graph-algorithms"]' \
    --env NEO4J_dbms_security_procedures_unrestricted=apoc.\\\*,algo.\\\* \
    neo4j:3.5.12
