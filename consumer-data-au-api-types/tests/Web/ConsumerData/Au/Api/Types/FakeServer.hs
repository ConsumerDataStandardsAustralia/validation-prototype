{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
module Web.ConsumerData.Au.Api.Types.FakeServer where

import Control.Lens

import Control.Concurrent       (forkIO, killThread)
import Control.Exception        (bracket, throwIO)
import Country.Identifier       (australia)
import Data.Profunctor          (lmap)
import Data.List.NonEmpty       (NonEmpty((:|)))
import Data.Maybe               (fromMaybe)
import Data.Time (fromGregorian, UTCTime(..))
import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant.API.Generic      (ToServant)
import Servant.Client
    (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientM, Scheme (Http),
    runClientM)
import Servant.Links            (Link)
import Servant.Server           (serve)
import Servant.Server.Generic   (AsServer, genericServer)
import Text.URI                 (Authority (..))
import Text.URI.QQ              (host, scheme)

import Web.ConsumerData.Au.Api.Types

a12345 :: AccountId
a12345 = AccountId (AsciiString "12345")

testBalances :: AccountBalances
testBalances = AccountBalances
  [ AccountBalance a12345
      (Deposits (CurrencyAmount (AmountString "400") Nothing) (CurrencyAmount (AmountString "350.75") Nothing))
  ]

testPerson :: Person
testPerson = Person
  (UTCTime (fromGregorian 2018 11 13) 0)
  "Ben"
  "Kolera"
  ["Leigh"]
  "Mr"
  Nothing
  (Just $ OccupationCode "261313 Software Engineer")

testPhoneNumber :: PhoneNumber
testPhoneNumber = PhoneNumber True PhoneNumberPurposeMobile (Just "+61") (Just "04") "88145427" Nothing "+61488145427"

testEmailAddress :: EmailAddress
testEmailAddress = EmailAddress True EmailAddressPurposeWork "ben.kolera@data61.csiro.au"

testAddress :: PhysicalAddress
testAddress = PhysicalAddress
  AddressPurposeRegistered
  (AddressSimple $ SimpleAddress
    (Just "Ben Kolera")
    "Level 3, T.C Beirne Centre"
    (Just "315 Brunswick St")
    Nothing
    (Just "4006")
    "Fortitude Valley"
    (AustralianState AustraliaStateQLD)
    (Just australia))

testPersonDetail :: PersonDetail
testPersonDetail = PersonDetail
  testPerson
  -- TODO: Fix waargonaut bug where nonempty fails on a single element NEL. :)
  (testPhoneNumber :| [])
  [testEmailAddress]
  [testAddress]

testOrganisation :: Organisation
testOrganisation = Organisation
  (UTCTime (fromGregorian 2018 11 13) 0)
  (Just "Ben")
  "Kolera"
  "Programmer"
  "Data 61"
  (Just "Data 61 Legal Name")
  (Just "D61")
  (Just "abn123")
  (Just "acn123")
  (Just True)
  (Just "icode")
  (Just OrgTypeCompany)
  (Just australia)
  (Just $ UTCTime (fromGregorian 2015 8 1) 0)

testOrganisationDetail :: OrganisationDetail
testOrganisationDetail = OrganisationDetail
  testOrganisation
  [testAddress]

fakePaginator :: Maybe PageNumber -> (Maybe PageNumber -> Link) -> Paginator
fakePaginator pMay = Paginator p p 0 . (lmap Just)
  where
    p = fromMaybe (PageNumber 1) pMay

testAccount :: Account
testAccount =
  Account a12345 "acc12345" (Just "my savings")
    (MaskedAccountNumber "abcde") (Just PCTermDeposits) "saving"
    (Deposits (CurrencyAmount (AmountString "201.30") Nothing) (CurrencyAmount (AmountString "198.80") Nothing))

testAccountDetail :: AccountDetail
testAccountDetail = AccountDetail (Just testAccount) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

identified :: a -> Identified a
identified = Identified a12345 "acc12345" (Just "my savings")

testAccounts :: Accounts
testAccounts = Accounts
  [ testAccount
  ]

testTransactionBasic :: TransactionBasic
testTransactionBasic = TransactionBasic
  (Just (TransactionId (AsciiString "reference"))) False TransactionStatusPosted "" Nothing Nothing Nothing Nothing "ref"

testTransactionDetail :: TransactionDetail
testTransactionDetail = TransactionDetail Nothing TransactionStatusPosted "" Nothing Nothing Nothing Nothing "" Nothing

testAccountTransaction :: AccountTransaction
testAccountTransaction = AccountTransaction a12345 (Just testTransactionBasic)

testAccountTransactions :: AccountTransactions
testAccountTransactions = AccountTransactions $ identified
  [ testTransactionBasic
  ]

testAccountsTransactions :: AccountsTransactions
testAccountsTransactions = AccountsTransactions [testAccountTransaction]

testAccountTransactionDetail :: AccountTransactionDetail
testAccountTransactionDetail = AccountTransactionDetail $ identified testTransactionDetail

testDirectDebitAuthorisations :: DirectDebitAuthorisations
testDirectDebitAuthorisations = DirectDebitAuthorisations
  [ AccountDirectDebit
      a12345
      (Just (AuthorisedEntity "me" "my bank" Nothing Nothing))
      Nothing
      (Just (AmountString "50.00"))
  ]

testPayee :: Payee
testPayee = Payee (PayeeId "5") "payee-nickname" Nothing Domestic

testPayees :: Payees
testPayees = Payees [testPayee]

testPayeeDetail :: PayeeDetail
testPayeeDetail = PayeeDetail testPayee $ PTDDomestic $ DPPayeeId $ DomesticPayeePayId
  "payee" "hello" OrgNumber

testProduct :: Product
testProduct = Product (AsciiString "product-id-5") Nothing Nothing (DateTimeString (UTCTime (fromGregorian 2018 1 1) 0))
  PCTermDeposits "product name" "description" "fancy" Nothing Nothing True Nothing

testProductDetail :: ProductDetail
testProductDetail = ProductDetail (Just testProduct) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

server :: LinkQualifier -> ToServant Api AsServer
server lq = genericServer Api
  { _common = genericServer CommonApi
    { _customer = genericServer CustomerApi
      { _customerBriefGet = pure $ mkStandardResponse
        --(CustomerPerson testPerson)
        (CustomerOrganisation testOrganisation)
        lq
        (links ^.commonLinks.customerLinks.customerBriefGet)
      , _customerDetailsGet = pure $ mkStandardResponse
        (CustomerDetailPerson testPersonDetail)
        --(CustomerDetailOrganisation testOrganisationDetail)
        lq
        (links ^.commonLinks.customerLinks.customerDetailsGet)
      }
    }
  , _banking = genericServer BankingApi
    { _bankingAccounts = genericServer AccountsApi
      { _accountsGet = \pMay -> pure $ mkPaginatedResponse
        testAccounts
        lq
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsGet))
      , _accountsBalancesGet = \pMay -> pure $ mkPaginatedResponse
        testBalances
        lq
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
      , _accountsBalancesPost = pure $ mkPaginatedResponse
        testBalances
        lq
        (fakePaginator Nothing (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
      , _accountsTransactionsGet = \pMay -> pure $ mkPaginatedResponse testAccountsTransactions lq (fakePaginator pMay (const $ (links^.bankingLinks.bankingAccountsLinks.accountsTransactionsGet) pMay))
      , _accountsTransactionsPost = pure $ mkPaginatedResponse testAccountsTransactions lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsTransactionsPost))
      , _accountsDirectDebitsGet = \pMay -> pure $ mkPaginatedResponse testDirectDebitAuthorisations lq (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsGet))
      , _accountsDirectDebitsPost = pure $ mkPaginatedResponse testDirectDebitAuthorisations lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsPost))
      , _accountsById = \accountId -> genericServer AccountApi
        { _accountGet                = pure $ mkStandardResponse testAccountDetail lq (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountGet)
        , _accountTransactionsGet    = pure $ mkPaginatedResponse testAccountTransactions lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionsGet))
        , _accountTransactionByIdGet = \transactionId -> pure $ mkStandardResponse testAccountTransactionDetail lq ((links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionByIdGet) transactionId)
        , _accountDirectDebitsGet    = pure $ mkPaginatedResponse
        testDirectDebitAuthorisations
        lq
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountDirectDebitsGet))
        }
      }
    , _bankingPayees = genericServer PayeesApi
      { _payeesGet = pure $ mkPaginatedResponse testPayees lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingPayeesLinks.payeesGet))
      , _payeesByIdGet = \_payeeId -> pure $ mkStandardResponse testPayeeDetail lq (links^.bankingLinks.bankingPayeesLinks.payeesByIdGet $ _payeeId)
      }
    , _bankingProducts = genericServer ProductsApi
      { _productsGet = pure $ mkPaginatedResponse [testProduct] lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingProductsLinks.productsGet))
      , _productsByIdGet = \_productId -> pure $ mkStandardResponse testProductDetail lq (links^.bankingLinks.bankingProductsLinks.productsByIdGet $ _productId)
      }
    }
  }

app :: LinkQualifier -> Application
app = serve api . server

runServer :: Int -> IO ()
runServer port = run port (app $ fakeQualifier)
  where
    fakeQualifier = LinkQualifier
      [scheme|http|]
      (Authority
       { authUserInfo = Nothing
       , authHost     = [host|localhost|]
       , authPort     = Just $ fromIntegral port
       })
      []

withServer :: Int -> (ClientM ()) -> IO ()
withServer p f = bracket (forkIO $ runServer p) killThread . const $ do
  m <- newManager defaultManagerSettings
  let env = ClientEnv m (BaseUrl Http "localhost" p "") Nothing
  runClientM f env >>= either throwIO pure
