module OS(
  defaultEditCommand
, tempDbPath
, dbPath
, configPath
) where

defaultEditCommand = "vim %1"
tempDbPath = ".encyclotemp"
dbPath = ".encyclo"
configPath = ".encycloconfig"
