all:
	stack build
	stack exec ghci app/Main.hs

build:
	stack build

exec:
	stack exec ghci app/Main.hs

clean:
	stack clean
	stack purge
