CONTAINER=spoke_2019_october

default:
	@echo targets: stop start bash cypher nodeCount ui

stop:
	- docker stop $(CONTAINER); docker rm spoke_neo4j

start:
	docker run --name=$(CONTAINER) \
          --publish=7474:7474 --publish=7687:7687 \
          --volume=/Users/paul/github/spokeExperiments/firstLook/data:/data \
          --volume=/Users/paul/github/spokeExperiments/firstLook/logs:/logs \
          --volume=/Users/paul/github/neo4jService/instances/spoke/import:/var/lib/neo4j/import \
          --user=neo4j \
          --env NEO4J_AUTH=neo4j/spoke@isb \
          --env 'NEO4JLABS_PLUGINS=["apoc", "graph-algorithms"]' \
          --env NEO4J_dbms_security_procedures_unrestricted=apoc.\\\*,algo.\\\* \
          neo4j:3.5.12 &


bash:
	docker exec --interactive --tty $(CONTAINER) /bin/bash

cypher:
	docker exec --interactive --tty $(CONTAINER) /bin/bash -c 'bin/cypher-shell -u neo4j -p spoke@isb'

nodeCount:
	docker exec --interactive --tty $(CONTAINER) \
          /bin/bash -c 'cat /var/lib/neo4j/import/getNodeCount.cypher | bin/cypher-shell -u neo4j -p spoke@isb'

ui:
	open http://localhost:7998
	@echo in the ui, change bolt port to the port number  mapped (mac-side) to 7999, user neo4j, pw spoke@isb



          #--volume=/Users/paul/github/neo4jService/instances/spoke/data:/data \
          #--volume=/Users/paul/github/neo4jService/instances/spoke/logs:/logs \
          #--volume=/Users/paul/github/neo4jService/instances/spoke/import:/var/lib/neo4j/import \
