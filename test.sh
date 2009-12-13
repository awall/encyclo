cd src
ghc -e ":load Database" -e "runTestTT unitTests"
ghc -e ":load Quick" -e "main"
