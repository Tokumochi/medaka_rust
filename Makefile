test:
	sh ./test.sh

clean:
	rm -f *.o *~ tmp*

.PHONY: test clean