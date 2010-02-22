MKDIR ..\bin
CD ..\src
COPY /Y *.hs ..\bin
COPY /Y windows\OS.hs ..\bin
CD ..\bin
ghc --make Main -o encyclo.exe -XTypeSynonymInstances -XFlexibleContexts
CD ..
CD windows
