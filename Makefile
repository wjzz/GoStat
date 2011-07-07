APP=GoStat
TEST=$(APP)Tests

all:
	cabal configure && cabal build

run:	all
	./dist/build/$(APP)/$(APP)

test:	all
	./dist/build/$(TEST)/$(TEST) --maximum-generated-tests=5000

linux-release: all
		 cp dist/build/GoStat/GoStat .
		 tar -a -c GoStat public/* CONFIG README doc/*.pdf -f dist/GoStat-binary-linux.tar.gz
		 rm GoStat

windows-release: 
		 tar -a -c GoStat.exe sqlite3.dll StronaStartowa.url public/* CONFIG README doc/*.pdf -f dist/GoStat-binary-windows.tar.gz

install:
	cabal install
