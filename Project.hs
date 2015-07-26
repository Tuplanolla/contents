module Project where

data Project =
  Project
    {projectName :: String,
     projectConfig :: String,
     projectTarget :: String,
     projectSwap :: String,
     projectVersion :: (Int, Int, Int)}
  deriving (Eq, Ord, Read, Show)

defaultProject :: Project
defaultProject =
  Project
    {projectName = "The Program Called Contents",
     projectConfig = ".contents",
     projectTarget = "CONTENTS",
     projectSwap = ".CONTENTS.swap",
     projectVersion = (0, 0, 0)}
