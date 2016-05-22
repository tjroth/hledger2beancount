-- | A library to do stuff.
module Lib
    (
      journalFromFile
    , journalFromFiles
    , accounts
    , openAccountsBean
    , beanFile
    ) where --}


import           Data.List          (nub, sort)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar
import           Hledger.Data.Types
import           Hledger.Read

headerBean :: Text
headerBean = T.pack ";; -*- mode: org; mode: beancount; -*-\n\n option \"operating_currency\" \"USD\"\n"

openAccountsBean :: Day -> Journal -> Text
openAccountsBean dt jrnl = T.append (T.pack prepend) (T.intercalate inter acctNames)
  where
    acctNames = map T.pack (accounts jrnl)
    dtString = show dt
    prepend = dtString ++ " open "
    inter = T.pack ("\n" ++ prepend)

transactionBean :: Transaction -> Text
transactionBean txn = T.concat [line1, ps, T.pack "\n"]
      where
        dt = show (tdate txn)
        line1 = T.pack (dt ++ " * " ++ "\"" ++ tdescription txn ++ "\"" ++ "\n")
        ps = postingsBean (tpostings txn)

transactionsBean :: Journal -> Text
transactionsBean jrnl = T.concat ts
  where
    ts = map transactionBean (jtxns jrnl)

postingsBean :: [Posting] -> Text
postingsBean ps = T.concat (map postingBean ps)

postingBean :: Posting -> Text
postingBean post = T.concat [acct, amt, cmdty, price, T.pack "\n"]
  where
    acct = T.pack ("  " ++ paccount post ++ "      ")
    amt = T.pack (show (aquantity $ unMixed $ pamount post) ++ " ")
    cmdty = T.pack cm
    cm = dollarToUSD (acommodity $ unMixed $ pamount post)
    price = priceToText $ aprice $ unMixed $ pamount post



parseMarketPrices :: Journal -> Text
parseMarketPrices jrnl = T.concat $ map getPriceString prices
  where
    prices = jmarketprices jrnl
    getPriceString mp = T.concat [T.pack (show $ mpdate mp)
                                , T.pack " price "
                                , T.pack (mpcommodity mp)
                                , T.pack " "
                                , T.pack (show $ aquantity $ mpamount mp)
                                , T.pack " USD"
                                , T.pack "\n"]


beanFile :: Day -> Journal -> Text
beanFile dt jrnl = T.concat [headerBean
                            , openAccountsBean dt jrnl
                            , T.pack "\n\n"
                            , transactionsBean jrnl
                            , parseMarketPrices jrnl ]


unMixed :: MixedAmount -> Amount
unMixed (Mixed amt) = head amt

dollarToUSD :: Commodity -> String
dollarToUSD c = if c == "$" then "USD" else c

priceToText :: Price -> Text
priceToText NoPrice = T.pack ""
priceToText (UnitPrice amt) = T.pack (" { " ++ show pr ++ " USD } ")
  where
    pr = aquantity amt
priceToText _ = T.pack "Error: Not accounted for"


accounts :: Journal -> [AccountName]
accounts j = sort . nub $ map paccount ps
  where
    ts = jtxns j
    ps = concatMap tpostings ts


journalFromFile :: FilePath -> IO (Either String Journal)
journalFromFile = readJournalFile (Just "journal") Nothing False


journalFromFiles :: [FilePath] -> IO (Either String Journal)
journalFromFiles = readJournalFiles (Just "journal") Nothing False
