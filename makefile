all:  docs install

docs:
	R -e "devtools::document()"
build:
	(cd ..; R CMD build --no-build-vignettes neo4jService)

install:
	(cd ..; R CMD INSTALL --no-test-load neo4jService)

check:
	(cd ..; R CMD check `ls -t neo4jService) | head -1`)

test:
	for x in inst/unitTests/test_*.R; do echo $$x; R -f $$x; done

