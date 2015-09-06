module Data.AddressBook where

import Prelude

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry =
  {
    firstName :: String,
    lastName :: String,
    address :: Address
  }

type Address =
  {
    street :: String,
    city :: String,
    state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry =  entry.lastName ++ ", " ++
                   entry.firstName ++ ": " ++
                   showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street ++ ", " ++
                   addr.city ++ ", " ++
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = (:)

-- head :: AddressBook -> Maybe Entry
-- filter :: (Entry -> Boolean) -> AddressBook -> AddressBook -> Maybe Entry
findEntry :: String -> String -> AddressBook -> Maybe Entry
-- findEntry firstName lastName book = head $ filter filterEntry book
findEntry firstName lastName = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress addr = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == addr.street && entry.address.city == addr.city && entry.address.state == addr.state
