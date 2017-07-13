
ARCHIVE=zipperposition.zip 

package:
	zip -r $(ARCHIVE) bin/

clean:
	rm $(ARCHIVE)

.PHONY: package clean
