{-# LANGUAGE TemplateHaskell, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module Dope.Logic.Act where

import Dope.Logic.Option
import Dope.Model.Common

import qualified Dope.Model.Player as Player
import Dope.Model.Player (Player (Player))

import qualified Dope.Model.Site as Site
import Dope.Model.Site (Site (Site))

import qualified Dope.Model.DrugBag as DrugBag
import Dope.Model.DrugBag (DrugBag (DrugBag))

import qualified Dope.State.GameState as GameState
import Dope.State.GameState (GameState (GameState))

import qualified Dope.State.Operation as Operation

import Control.Monad
import Control.Concurrent.STM
import System.Random
import Control.Category
import Data.Label
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (find)
import Prelude hiding ((.), id)

optionsIO :: TVar Player -> TVar GameState -> IO ([Option], Player)
optionsIO playerVar stateVar = atomically $ do
    player <- readTVar playerVar
    state <- readTVar stateVar
    posibilities <- options player state
    return (posibilities, player)

options :: Player -> GameState -> STM [Option]
options player state =
    case (get Player.situation player, get Player.place player) of
        (Idle, place) -> 
            case place of 
                Street position -> do
                    sites <- mapM readTVar (Map.elems (get GameState.siteVars state))
                    let site = case find (\s -> get Site.position s == position) sites of
                            Just site -> [Enter (get Site.name site)]
                            Nothing -> []
                    return (TakeACap Requested : DealDrugs Requested : site)
                Inside siteName -> do
                    case Map.lookup siteName (get GameState.siteVars state) of
                        Just siteVar -> do
                            site <- readTVar siteVar
                            case get Site.type site of
                                Jail -> return []
                                Club -> do
                                    return (Exit : map Trade (get Site.guests site))
                        Nothing -> return []
        (Busted, place) -> do
            let vendorNames = map (get DrugBag.seller) (get Player.drugBags player)
            return (BribePolice Requested : map SnitchFriend vendorNames)
        (Trading otherPlayer, place) -> return [AbortTrade]

actIO :: TVar Player -> Option -> TVar GameState -> IO (Maybe String)
actIO playerVar option stateVar = atomically $ act playerVar option stateVar
    
act :: TVar Player -> Option -> TVar GameState -> STM (Maybe String)
act playerVar option stateVar = do
    player <- readTVar playerVar
    state <- readTVar stateVar
    possibilities <- options player state
    if elem option possibilities
        then do 
            case option of
                TakeACap (Parameter destination) -> do
                    let Street origin = get Player.place player
                    let expense = taxameter origin destination
                    let money = get Player.money player
                    if money >= expense
                        then do
                            let player' = set Player.place (Street destination) $ 
                                    set Player.money (money - expense) player
                            writeTVar playerVar player'
                            return Nothing
                        else return $ Just "You ain't got the money"
                TakeACap Requested -> return $ Just "Take a cap, where to"
                Enter siteName -> do
                    Operation.movePlayer state (get Player.name player) (Inside siteName)
                    return Nothing
                Exit -> do
                    let Inside siteName = get Player.place player
                    case Map.lookup siteName (get GameState.siteVars state) of
                        Just siteVar -> do
                            site <- readTVar siteVar
                            Operation.movePlayer state (get Player.name player) (Street (get Site.position site))
                            return Nothing
                        Nothing -> error "Error in options - site should exist"
                DealDrugs (Parameter index) -> do
                    let drugBags = get Player.drugBags player
                    if index < 0 || index >= length drugBags
                        then return $ Just "Stop hallucinating."
                        else do
                            let drugBag = drugBags !! index
                            let quantity = get DrugBag.quantity drugBag
                            let drugBag' = set DrugBag.quantity (quantity - 1) drugBag
                            when (quantity < 1) $ error "Nothing left in the bag of drugs to sell"
                            let drugBags' = if quantity == 1
                                    then take index drugBags ++ drop (index + 1) drugBags
                                    else take index drugBags ++ [drugBag'] ++ drop (index + 1) drugBags
                            writeTVar playerVar (set Player.drugBags drugBags' player)
                            return Nothing
                DealDrugs Requested -> return $ Just "How much though?"
                Trade partnerName -> return $ Just "Trading is not yet implemented"
                AbortTrade -> return $ Just "Trading is not yet implemented"
                BribePolice (Parameter money) -> do
                    Operation.movePlayer state (get Player.name player) (Inside "Jail")
                    return Nothing
                BribePolice Requested -> return $ Just "I wonder how much dough will turn the tides..."
                SnitchFriend friendName -> do
                    Operation.movePlayer state (get Player.name player) (Inside "Jail")
                    return Nothing
        else return $ Just "Option unavailable"

taxameter :: Position -> Position -> Integer 
taxameter (Position x1 y1) (Position x2 y2) = fromIntegral $ abs (x2 - x1) + abs (y2 - y1)


actAndReportOptions :: TVar GameState -> TVar Player -> Option -> IO (Maybe String, Player, [Option])
actAndReportOptions stateVar playerVar option = atomically $ do
    error <- act playerVar option stateVar
    player <- readTVar playerVar
    state <- readTVar stateVar
    possibilities <- options player state
    return (error, player, possibilities)
