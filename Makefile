ghc=ghc -O -Wall $<
all: scan rescue chunk
chunk: chunk.hs
	$(ghc)
scan: scan.hs
	$(ghc)
rescue: rescue.hs
	$(ghc)
