
ARCHIVE=zipperposition.zip 

package:
	zip -r $(ARCHIVE) bin/

clean:
	rm $(ARCHIVE) || true

.PHONY: package clean
