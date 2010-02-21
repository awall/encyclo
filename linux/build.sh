cp -f ../src/*.hs ../src/linux/OS.hs ../bin
cd ../bin
ghc --make Main -o encyclo -XTypeSynonymInstances -XFlexibleContexts
