{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Lucid

import System.Environment

import Control.Monad.IO.Class
import Network.HTTP.Req
    ( responseBody,
      (=:),
      jsonResponse,
      (/:),
      https,
      req,
      defaultHttpConfig,
      runReq,
      NoReqBody(NoReqBody),
      GET(GET),
      JsonResponse )

import qualified Data.Text as T hiding (foldr1, map)
import Data.Text (Text)
import Data.Text.Read

import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..))

-- async
import Control.Concurrent.Async
import Data.List (sortBy)

import Data.ByteString.Lazy as Lazy hiding (map, foldr1, maximum, minimum)

-- sort and sum net income for company

-- * api

type QueryApi =
  "query" :> Servant.QueryParam "company" Text     :> Get '[JSON] IncomeStatement :<|>
  "query" :> ReqBody '[JSON] CompaniesQueryPayload :> Post '[JSON] CompaniesQueryResponse :<|>
  "stats" :> ReqBody '[JSON] CompaniesQueryPayload :> Post '[HTML] RawHtml :<|>
  Raw

itemApi :: Proxy QueryApi
itemApi = Proxy


-- * app

run :: IO ()
run = do
  maybePortStr <- lookupEnv "PORT"
  let port :: Int = read (getOrDefault "3000" maybePortStr) 
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

  where 
    getOrDefault :: p -> Maybe p -> p
    getOrDefault defaultValue (Just x)  = x
    getOrDefault defaultValue _ = defaultValue

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server QueryApi
server =
  getCompanyIncomeStatement
  :<|> getStats
  :<|> statsHTML
  :<|> serveDirectoryFileServer "static/"

-- todo generate html page

-- Server API

statsHTML :: CompaniesQueryPayload -> Handler RawHtml
statsHTML payload = do
  case companiesSymbols payload of
    [] -> throwError err400 {
        errBody = "You need to provide at least one company!"
      }
    _  -> return ()
  results <- liftIO $ fetchIncomeStatements payload
  response <- case results of
    Left errorMsg -> throwError err400 {
      errBody = encode errorMsg
    }
    Right results -> return $ 
      CompaniesQueryResponse {
        highestGrossProfit = highestGrossProfitStats results,
        sortedByNetIncome  = sortedNetIncomeStats results,
        aggregatedRevenue  = totalRevenueStats results
      }

  return (RawHtml $ renderBS (renderQueryResults response) )

getStats :: CompaniesQueryPayload -> Handler CompaniesQueryResponse
getStats payload = do
  case companiesSymbols payload of
    [] -> throwError err400 {
        errBody = "You need to provide at least one company!"
      }
    _  -> return ()
  results <- liftIO $ fetchIncomeStatements payload
  case results of
    Left errorMsg -> throwError err400 {
      errBody = encode errorMsg
    }
    Right results -> return $
      CompaniesQueryResponse {
        highestGrossProfit = highestGrossProfitStats results,
        sortedByNetIncome  = sortedNetIncomeStats results,
        aggregatedRevenue  = totalRevenueStats results
      }


getCompanyIncomeStatement :: Maybe Text -> Handler IncomeStatement
getCompanyIncomeStatement (Just symbol) = do
  let query = IncomeStatementQuery symbol
  result <- fetchIncomeStatement query
  case result of
    Left errorMsg -> throwError err400 {
      errBody = encode errorMsg
    }
    Right result ->
      return result

getCompanyIncomeStatement Nothing = throwError err404 {
  errBody = "You need to supply company name as a parameter!"
}

-- Domain Logic

normalize :: Integral a => a -> a
normalize x = x `div` 1000000000

num :: Text -> Int
num = either (error . show) fst  -- Throw an exception if it wasn't a signed decimal
    . signed decimal             -- Read a signed decimal

highestGrossProfitStats :: [IncomeStatement] -> Stats
highestGrossProfitStats statements =
  let
    score = num . grossProfit
    (report, companyName) = bestAnnualReport score statements
  in Stats {
    who = companyName,
    when = fiscalDateEnding report,
    value = normalize . num . grossProfit $ report
  }


sortedNetIncomeStats :: [IncomeStatement] -> [Stats]
sortedNetIncomeStats =
  map toNetIncomeStats .
  sortReportsByNetIncome .
  mergeReports

totalRevenueStats :: [IncomeStatement] -> [Stats]
totalRevenueStats = map sumRevenue

sumRevenue :: IncomeStatement -> Stats
sumRevenue stmt =
  Stats {
    who = companyName,
    value = aggregatedRevenue,
    when = minYear <> "-" <> maxYear
  }
  where
    companyName = symbol stmt
    reports = annualReports stmt
    extractRevenue = normalize . num . totalRevenue
    extractYear = num . T.take 4 . fiscalDateEnding
    aggregatedRevenue = sum . map extractRevenue $ reports
    minYear = intToText . minimum . map extractYear $ reports
    maxYear = intToText . maximum . map extractYear $ reports
    intToText = T.pack . show

bestAnnualReport :: (AnnualReport -> Int) -> [IncomeStatement] -> (AnnualReport, CompanySymbol)
bestAnnualReport scoreF statements =
  foldReports scoreF $ mergeReports statements
  where
    foldReports :: (AnnualReport -> Int) -> [(AnnualReport, CompanySymbol)] -> (AnnualReport, CompanySymbol)
    foldReports scoreF = foldr1 maximum'
      where
        maximum' (r1, sym1) (r2, sym2) =
          let s1 = scoreF r1
              s2 = scoreF r2
          in
            if s1 > s2 then
              (r1, sym1)
            else
              (r2, sym2)

mergeReports :: [IncomeStatement] -> [(AnnualReport, CompanySymbol)]
mergeReports statements =
    do
      stmt <- statements
      let name = symbol stmt
      let reports = annualReports stmt
      map (,name) reports

toNetIncomeStats :: (AnnualReport, CompanySymbol) -> Stats
toNetIncomeStats (report, company) = Stats {
  who = company,
  when = fiscalDateEnding report,
  value = normalize . num . netIncome $ report
}

sortReportsByNetIncome :: [(AnnualReport, CompanySymbol)] -> [(AnnualReport, CompanySymbol)]
sortReportsByNetIncome = sortBy comp
  where
    scoreF = num . netIncome
    comp (r1, _) (r2, _) | s1 < s2   = LT
                         | s1 == s2  = EQ
                         | s1 > s2   = GT
      where s1 = scoreF r1
            s2 = scoreF r2



-- Remote API Calls

-- fetchIncomeStatements :: CompaniesQueryPayload -> IO (Either Text [IncomeStatement])
fetchIncomeStatements (CompaniesQueryPayload companiesSymbols) = do
    asyncResults <- mapM asyncQuery queries
    results <- mapM wait asyncResults
    return $ sequence results

    where
      asyncQuery query = async $ do
        liftIO $ fetchIncomeStatement query

      queries = map IncomeStatementQuery companiesSymbols


fetchIncomeStatement (IncomeStatementQuery queriedSymbol) = runReq defaultHttpConfig $ do
  response <-
    req
      Network.HTTP.Req.GET
      (https "www.alphavantage.co" Network.HTTP.Req./: "query")
      NoReqBody
      jsonResponse $
        "function" =: incomeStatementFunction <>
        "apikey" =: apiKey <>
        "symbol" =: queriedSymbol
  return $ parse response
  where
    apiKey :: Text                  = "P29GLD76NJYI7ELX"
    incomeStatementFunction :: Text = "INCOME_STATEMENT"

    parseJSON :: JsonResponse Value -> Result IncomeStatement
    parseJSON = fromJSON . responseBody

    parseResult (Success x) = Right x
    parseResult _           = Left $ "Invalid company symbol (NASDAQ): " <> queriedSymbol <> "!"

    parse = parseResult . parseJSON



-- Types
-- Our API

-- Query
-- Query details about a couple of companies.
type CompanySymbol = Text

newtype CompaniesQueryPayload = CompaniesQueryPayload {
  companiesSymbols :: [CompanySymbol]
} deriving (Eq, Show, Generic)

instance ToJSON CompaniesQueryPayload
instance FromJSON CompaniesQueryPayload

-- Response
-- Respond with various statistics about queried companies.
data CompaniesQueryResponse = CompaniesQueryResponse {
  highestGrossProfit :: Stats,
  sortedByNetIncome :: [Stats],
  aggregatedRevenue :: [Stats]
} deriving (Eq, Show, Generic)

data Stats = Stats {
  who :: CompanySymbol,
  when :: Text,
  value :: Int
} deriving (Eq, Show, Generic)

instance ToJSON Stats
instance FromJSON Stats

instance ToJSON CompaniesQueryResponse
instance FromJSON CompaniesQueryResponse


-- Foreign API Response
data AnnualReport = AnnualReport {
  fiscalDateEnding :: Text,
  grossProfit :: Text,
  totalRevenue :: Text,
  netIncome :: Text
} deriving (Eq, Show, Generic)

instance ToJSON AnnualReport
instance FromJSON AnnualReport

data IncomeStatement = IncomeStatement {
  symbol :: Text,
  annualReports :: [AnnualReport]
} deriving (Eq, Show, Generic)

instance ToJSON IncomeStatement
instance FromJSON IncomeStatement

-- Foreign API Request
newtype IncomeStatementQuery = IncomeStatementQuery {
  companySymbol :: Text
}



-- serve html

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" Network.HTTP.Media./: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw


    -- create 3 tables
renderQueryResults :: CompaniesQueryResponse -> Html ()
renderQueryResults (CompaniesQueryResponse profit income revenue) = html_ $ do
  head_ $ do
    title_ "REST "
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles.css"]
  body_ $ do
    h1_ "Gathered Statistics"
    table_ $ do 
      tr_  (
        th_ [colspan_ "3"] "Highest Gross Profit"  )

      tr_  (do 
        th_ "who"
        th_ "when"
        th_ "value")
      
      tr_ $ mapM_ (td_ . toHtml) (statsToTr profit)

    br_ []
    bigTable "Aggregated Revenue" mapped1
    br_ []

    br_ []
    bigTable "Sorted Net Income" mapped2

  where 
    mapped2 = map statsToTr income
    mapped1 = map statsToTr revenue

    statsToTr stats = 
      [(show . who) stats,
      (show . when) stats,
      (show . value) stats]

    -- bigTable :: [[String]] -> Html ()
    bigTable title ts = table_ $ do 
      tr_  (
        th_ [colspan_ "3"] title )

      tr_  (do 
        th_ "who"
        th_ "when"
        th_ "value")

      mapM_ row ts
      where row :: [String] -> Html ()
            row r = tr_ (mapM_ (td_ . toHtml) r)
