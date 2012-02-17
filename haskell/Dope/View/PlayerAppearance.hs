
data PlayerAppearance = PlayerAppearance {
    _playerAppearanceName :: String
    }
$(mkLabels [''PlayerAppearance])

toPlayerAppearance :: Player -> PlayerAppearance
toPlayerAppearance player = PlayerAppearance (get Player.name player)

-- TODO: This should have its own fields, but I'm too lazy
data PlayerIntrospection = PlayerIntrospection {
    _playerIntrospectionPlayer :: Player
    }
$(mkLabels [''PlayerIntrospection])

toPlayerIntrospection :: Player -> PlayerIntrospection
toPlayerIntrospection player = PlayerIntrospection player

data SiteExterior = SiteExterior {
    _siteExteriorName :: String,
    _siteExteriorType :: SiteType,
    _siteExteriorPosition :: Position
    }
$(mkLabels [''SiteExterior])

toSiteExterior :: Site -> SiteExterior
toSiteExterior site = SiteExterior (get Site.name site) (get Site.type site) (get Site.position site)

data SiteInterior = SiteInterior {
    _siteInteriorExterior :: SiteExterior,
    _siteInteriorVisitors :: [PlayerName]
    }
$(mkLabels [''SiteInterior])

toSiteInterior :: TVar GameState -> Site -> STM SiteInterior
toSiteInterior stateVar site = do
    state <- readTVar stateVar
    let playerVars = get GameState.playerVars state
    let guestNames = get Site.guests site
    let guestVars = map (\name -> playerVars Map.! name) guestNames
    guests <- mapM readTVar guestVars
    return (SiteInterior (toSiteExterior site) (map (get Player.name) guests))

