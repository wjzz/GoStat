COMPILER=ghc
OPTIONS=--make -W -Wall -O2 -isrc -odir bin -hidir bin
APP=GoStat

all:
	$(COMPILER) $(OPTIONS) -o dist/$(APP).exe src/Main.hs

run:
	./dist/$(APP).exe

docs:
	cd src && haddock -o ../docs/ -h -t $(APP) Main.hs && cd ..

test:	Tests.exe
	./dist/Tests.exe --maximum-generated-tests=5000

Tests.exe: #src/Tests.hs
	ghc --make -odir bin -hidir bin -isrc/ -o dist/Tests.exe test/Tests.hs