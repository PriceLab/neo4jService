CONTAINER=scratch_neo4j

default:
	@echo targets: stop start bash cypher nodeCount fill.local delete.db ui

stop:
	- docker stop $(CONTAINER); docker rm scratch_neo4j

start:
	docker run --name=$(CONTAINER) \
          --detach \
          --publish=7900:7474 --publish=7899:7687 \
          --volume=/Users/paul/github/neo4jService/instances/scratch/data:/data \
          --volume=/Users/paul/github/neo4jService/instances/scratch/logs:/logs \
          --volume=/Users/paul/github/neo4jService/instances/scratch/import:/var/lib/neo4j/import \
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

fill.local:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/createMovieDemo.cypher | bin/cypher-shell -u neo4j -p hoopa'

fill.personsKnowingPersons:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c cat /var/lib/neo4j/import/peopleKnowingPeople.cypher | bin/cypher-shell -u neo4j -p hoopa

delete.db:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/clearDataBase.cypher | bin/cypher-shell -u neo4j -p hoopa'

ui:
	open http://localhost:7998
	@echo in the ui, change bolt port to the port number  mapped (mac-side) to 7999, user neo4j, pw hoopa

