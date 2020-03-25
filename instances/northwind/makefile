CONTAINER=northwind_neo4j

default:
	@targets: stop start bash

stop:
	- docker stop $(CONTAINER); docker rm northwind_neo4j

start:
	docker run --name=$(CONTAINER) \
          --detach \
          --publish=7477:7474 --publish=7799:7687 \
          --volume=/Users/paul/github/neo4jService/instances/northwind/data:/data \
          --volume=/Users/paul/github/neo4jService/instances/northwind/logs:/logs \
          --volume=/Users/paul/github/neo4jService/instances/northwind/import:/var/lib/neo4j/import \
          --user=neo4j \
          --env NEO4J_AUTH=neo4j/hoopa \
          --env 'NEO4JLABS_PLUGINS=["apoc", "graph-algorithms"]' \
          --env NEO4J_dbms_security_procedures_unrestricted=apoc.\\\*,algo.\\\* \
          neo4j:3.5.12


bash:
	docker exec --interactive --tty $(CONTAINER) /bin/bash

cypher:
	docker exec --interactive --tty $(CONTAINER) /bin/bash -c 'bin/cypher-shell -u neo4j -p hoopa'

nodeCount:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/getNodeCount.cypher | bin/cypher-shell -u neo4j -p hoopa'

fill.http:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/loadCsvViaHttp.cypher | bin/cypher-shell -u neo4j -p hoopa'

fill.local:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/loadLocalFiles.cypher | bin/cypher-shell -u neo4j -p hoopa'

delete.db:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/clearDataBase.cypher | bin/cypher-shell -u neo4j -p hoopa'

ui:
	open http://localhost:7477
	@echo change bolt port to its mapped (mac-side) value: 7799, neo4j/hoopa

