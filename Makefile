all: merkledag.pb unixfs.pb
	cabal build

%.pb: pb/%.proto
	hprotoc -I src -d src $<
