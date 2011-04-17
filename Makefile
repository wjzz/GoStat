#COMPILER=ghc
#OPTIONS=--make -W -Wall -O2 -isrc -odir bin -hidir bin
APP=GoStat
TEST=$(APP)Tests

all:
# 	$(COMPILER) $(OPTIONS) -o dist/$(APP).exe src/Main.hs
	cabal configure && cabal build

run:
	./dist/build/$(APP)/$(APP)

test:	all
	./dist/build/$(TEST)/$(TEST) --maximum-generated-tests=5000

#docs:
##cd src && haddock -o ../docs/ -h -t $(APP) Main.hs && cd ..
