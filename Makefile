test: chibicc
	sh ./test.sh

clean:
	rm -f chibicc *.o *~ tmp*

.PHONY: test clean