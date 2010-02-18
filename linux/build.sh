cp -f ../src/*.hs ../bin
cd ../bin
ghc --make Main -o encyclo -XTypeSynonymInstances -XFlexibleContexts
