{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.ConsumerData.Au.Api.Types.Data.Currency where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text

-- | ISO 4217
--
-- <https://www.currency-iso.org/en/home/tables/table-a1.html>
data Currency
  = AED
  | AFN
  | ALL
  | AMD
  | ANG
  | AOA
  | ARS
  | AUD
  | AWG
  | AZN
  | BAM
  | BBD
  | BDT
  | BGN
  | BHD
  | BIF
  | BMD
  | BND
  | BOB
  | BOV
  | BRL
  | BSD
  | BTN
  | BWP
  | BYN
  | BZD
  | CAD
  | CDF
  | CHE
  | CHF
  | CHW
  | CLF
  | CLP
  | CNY
  | COP
  | COU
  | CRC
  | CUC
  | CUP
  | CVE
  | CZK
  | DJF
  | DKK
  | DOP
  | DZD
  | EGP
  | ERN
  | ETB
  | EUR
  | FJD
  | FKP
  | GBP
  | GEL
  | GHS
  | GIP
  | GMD
  | GNF
  | GTQ
  | GYD
  | HKD
  | HNL
  | HRK
  | HTG
  | HUF
  | IDR
  | ILS
  | INR
  | IQD
  | IRR
  | ISK
  | JMD
  | JOD
  | JPY
  | KES
  | KGS
  | KHR
  | KMF
  | KPW
  | KRW
  | KWD
  | KYD
  | KZT
  | LAK
  | LBP
  | LKR
  | LRD
  | LSL
  | LYD
  | MAD
  | MDL
  | MGA
  | MKD
  | MMK
  | MNT
  | MOP
  | MRU
  | MUR
  | MVR
  | MWK
  | MXN
  | MXV
  | MYR
  | MZN
  | NAD
  | NGN
  | NIO
  | NOK
  | NPR
  | NZD
  | OMR
  | PAB
  | PEN
  | PGK
  | PHP
  | PKR
  | PLN
  | PYG
  | QAR
  | RON
  | RSD
  | RUB
  | RWF
  | SAR
  | SBD
  | SCR
  | SDG
  | SEK
  | SGD
  | SHP
  | SLL
  | SOS
  | SRD
  | SSP
  | STN
  | SVC
  | SYP
  | SZL
  | THB
  | TJS
  | TMT
  | TND
  | TOP
  | TRY
  | TTD
  | TWD
  | TZS
  | UAH
  | UGX
  | USD
  | USN
  | UYI
  | UYU
  | UYW
  | UZS
  | VES
  | VND
  | VUV
  | WST
  | XAF
  | XAG
  | XAU
  | XBA
  | XBB
  | XBC
  | XBD
  | XCD
  | XDR
  | XOF
  | XPD
  | XPF
  | XPT
  | XSU
  | XTS
  | XUA
  | XXX
  | YER
  | ZAR
  | ZMW
  | ZWL
  deriving (Eq, Ord, Show, Enum, Bounded)

currencyText :: Prism' Text Currency
currencyText = prism (Text.pack . show) $ \case
  "AED" -> Right AED
  "AFN" -> Right AFN
  "ALL" -> Right ALL
  "AMD" -> Right AMD
  "ANG" -> Right ANG
  "AOA" -> Right AOA
  "ARS" -> Right ARS
  "AUD" -> Right AUD
  "AWG" -> Right AWG
  "AZN" -> Right AZN
  "BAM" -> Right BAM
  "BBD" -> Right BBD
  "BDT" -> Right BDT
  "BGN" -> Right BGN
  "BHD" -> Right BHD
  "BIF" -> Right BIF
  "BMD" -> Right BMD
  "BND" -> Right BND
  "BOB" -> Right BOB
  "BOV" -> Right BOV
  "BRL" -> Right BRL
  "BSD" -> Right BSD
  "BTN" -> Right BTN
  "BWP" -> Right BWP
  "BYN" -> Right BYN
  "BZD" -> Right BZD
  "CAD" -> Right CAD
  "CDF" -> Right CDF
  "CHE" -> Right CHE
  "CHF" -> Right CHF
  "CHW" -> Right CHW
  "CLF" -> Right CLF
  "CLP" -> Right CLP
  "CNY" -> Right CNY
  "COP" -> Right COP
  "COU" -> Right COU
  "CRC" -> Right CRC
  "CUC" -> Right CUC
  "CUP" -> Right CUP
  "CVE" -> Right CVE
  "CZK" -> Right CZK
  "DJF" -> Right DJF
  "DKK" -> Right DKK
  "DOP" -> Right DOP
  "DZD" -> Right DZD
  "EGP" -> Right EGP
  "ERN" -> Right ERN
  "ETB" -> Right ETB
  "EUR" -> Right EUR
  "FJD" -> Right FJD
  "FKP" -> Right FKP
  "GBP" -> Right GBP
  "GEL" -> Right GEL
  "GHS" -> Right GHS
  "GIP" -> Right GIP
  "GMD" -> Right GMD
  "GNF" -> Right GNF
  "GTQ" -> Right GTQ
  "GYD" -> Right GYD
  "HKD" -> Right HKD
  "HNL" -> Right HNL
  "HRK" -> Right HRK
  "HTG" -> Right HTG
  "HUF" -> Right HUF
  "IDR" -> Right IDR
  "ILS" -> Right ILS
  "INR" -> Right INR
  "IQD" -> Right IQD
  "IRR" -> Right IRR
  "ISK" -> Right ISK
  "JMD" -> Right JMD
  "JOD" -> Right JOD
  "JPY" -> Right JPY
  "KES" -> Right KES
  "KGS" -> Right KGS
  "KHR" -> Right KHR
  "KMF" -> Right KMF
  "KPW" -> Right KPW
  "KRW" -> Right KRW
  "KWD" -> Right KWD
  "KYD" -> Right KYD
  "KZT" -> Right KZT
  "LAK" -> Right LAK
  "LBP" -> Right LBP
  "LKR" -> Right LKR
  "LRD" -> Right LRD
  "LSL" -> Right LSL
  "LYD" -> Right LYD
  "MAD" -> Right MAD
  "MDL" -> Right MDL
  "MGA" -> Right MGA
  "MKD" -> Right MKD
  "MMK" -> Right MMK
  "MNT" -> Right MNT
  "MOP" -> Right MOP
  "MRU" -> Right MRU
  "MUR" -> Right MUR
  "MVR" -> Right MVR
  "MWK" -> Right MWK
  "MXN" -> Right MXN
  "MXV" -> Right MXV
  "MYR" -> Right MYR
  "MZN" -> Right MZN
  "NAD" -> Right NAD
  "NGN" -> Right NGN
  "NIO" -> Right NIO
  "NOK" -> Right NOK
  "NPR" -> Right NPR
  "NZD" -> Right NZD
  "OMR" -> Right OMR
  "PAB" -> Right PAB
  "PEN" -> Right PEN
  "PGK" -> Right PGK
  "PHP" -> Right PHP
  "PKR" -> Right PKR
  "PLN" -> Right PLN
  "PYG" -> Right PYG
  "QAR" -> Right QAR
  "RON" -> Right RON
  "RSD" -> Right RSD
  "RUB" -> Right RUB
  "RWF" -> Right RWF
  "SAR" -> Right SAR
  "SBD" -> Right SBD
  "SCR" -> Right SCR
  "SDG" -> Right SDG
  "SEK" -> Right SEK
  "SGD" -> Right SGD
  "SHP" -> Right SHP
  "SLL" -> Right SLL
  "SOS" -> Right SOS
  "SRD" -> Right SRD
  "SSP" -> Right SSP
  "STN" -> Right STN
  "SVC" -> Right SVC
  "SYP" -> Right SYP
  "SZL" -> Right SZL
  "THB" -> Right THB
  "TJS" -> Right TJS
  "TMT" -> Right TMT
  "TND" -> Right TND
  "TOP" -> Right TOP
  "TRY" -> Right TRY
  "TTD" -> Right TTD
  "TWD" -> Right TWD
  "TZS" -> Right TZS
  "UAH" -> Right UAH
  "UGX" -> Right UGX
  "USD" -> Right USD
  "USN" -> Right USN
  "UYI" -> Right UYI
  "UYU" -> Right UYU
  "UYW" -> Right UYW
  "UZS" -> Right UZS
  "VES" -> Right VES
  "VND" -> Right VND
  "VUV" -> Right VUV
  "WST" -> Right WST
  "XAF" -> Right XAF
  "XAG" -> Right XAG
  "XAU" -> Right XAU
  "XBA" -> Right XBA
  "XBB" -> Right XBB
  "XBC" -> Right XBC
  "XBD" -> Right XBD
  "XCD" -> Right XCD
  "XDR" -> Right XDR
  "XOF" -> Right XOF
  "XPD" -> Right XPD
  "XPF" -> Right XPF
  "XPT" -> Right XPT
  "XSU" -> Right XSU
  "XTS" -> Right XTS
  "XUA" -> Right XUA
  "XXX" -> Right XXX
  "YER" -> Right YER
  "ZAR" -> Right ZAR
  "ZMW" -> Right ZMW
  "ZWL" -> Right ZWL
  x     -> Left x
