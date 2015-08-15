all: protobuf
	cabal build

protobuf: merkledag.proto
	hprotoc -I src -d src merkledag.proto
