{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AuPost.PAF.StreetType
  ( module AuPost.PAF.StreetType
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E


data StreetType =
    StreetTypeACCS -- ^ Access ACCS
  | StreetTypeALLY -- ^ Alley ALLY
  | StreetTypeALWY -- ^ Alleyway ALWY
  | StreetTypeAMBL -- ^ Amble AMBL
  | StreetTypeANCG -- ^ Anchorage ANCG
  | StreetTypeAPP -- ^ Approach APP
  | StreetTypeARC -- ^ Arcade ARC
  | StreetTypeART -- ^ Artery ART
  | StreetTypeAVE -- ^ Avenue AVE
  | StreetTypeBASN -- ^ Basin BASN
  | StreetTypeBCH -- ^ Beach BCH
  | StreetTypeBEND -- ^ Bend BEND
  | StreetTypeBLK -- ^ Block BLK
  | StreetTypeBVD -- ^ Boulevard BVD
  | StreetTypeBRCE -- ^ Brace BRCE
  | StreetTypeBRAE -- ^ Brae BRAE
  | StreetTypeBRK -- ^ Break BRK
  | StreetTypeBDGE -- ^ Bridge BDGE
  | StreetTypeBDWY -- ^ Broadway BDWY
  | StreetTypeBROW -- ^ Brow BROW
  | StreetTypeBYPA -- ^ Bypass BYPA
  | StreetTypeBYWY -- ^ Byway BYWY
  | StreetTypeCAUS -- ^ Causeway CAUS
  | StreetTypeCTR -- ^ Centre CTR
  | StreetTypeCNWY -- ^ Centreway CNWY
  | StreetTypeCH -- ^ Chase CH
  | StreetTypeCIR -- ^ Circle CIR
  | StreetTypeCLT -- ^ Circlet CLT
  | StreetTypeCCT -- ^ Circuit CCT
  | StreetTypeCRCS -- ^ Circus CRCS
  | StreetTypeCL -- ^ Close CL
  | StreetTypeCLDE -- ^ Colonnade CLDE
  | StreetTypeCMMN -- ^ Common CMMN
  | StreetTypeCON -- ^ Concourse CON
  | StreetTypeCPS -- ^ Copse CPS
  | StreetTypeCNR -- ^ Corner CNR
  | StreetTypeCSO -- ^ Corso CSO
  | StreetTypeCT -- ^ Court CT
  | StreetTypeCTYD -- ^ Courtyard CTYD
  | StreetTypeCOVE -- ^ Cove COVE
  | StreetTypeCRES -- ^ Crescent CRES
  | StreetTypeCRST -- ^ Crest CRST
  | StreetTypeCRSS -- ^ Cross CRSS
  | StreetTypeCRSG -- ^ Crossing CRSG
  | StreetTypeCRD -- ^ Crossroad CRD
  | StreetTypeCOWY -- ^ Crossway COWY
  | StreetTypeCUWY -- ^ Cruiseway CUWY
  | StreetTypeCDS -- ^ Cul-De-Sac CDS
  | StreetTypeCTTG -- ^ Cutting CTTG
  | StreetTypeDALE -- ^ Dale DALE
  | StreetTypeDELL -- ^ Dell DELL
  | StreetTypeDEVN -- ^ Deviation DEVN
  | StreetTypeDIP -- ^ Dip DIP
  | StreetTypeDSTR -- ^ Distributor DSTR
  | StreetTypeDR -- ^ Drive DR
  | StreetTypeDRWY -- ^ Driveway DRWY
  | StreetTypeEDGE -- ^ Edge EDGE
  | StreetTypeELB -- ^ Elbow ELB
  | StreetTypeEND -- ^ End END
  | StreetTypeENT -- ^ Entrance ENT
  | StreetTypeESP -- ^ Esplanade ESP
  | StreetTypeEST -- ^ Estate EST
  | StreetTypeEXP -- ^ Expressway EXP
  | StreetTypeEXTN -- ^ Extension EXTN
  | StreetTypeFAWY -- ^ Fairway FAWY
  | StreetTypeFTRK -- ^ Fire Track FTRK
  | StreetTypeFITR -- ^ Firetrail FITR
  | StreetTypeFLAT -- ^ Flat FLAT
  | StreetTypeFOLW -- ^ Follow FOLW
  | StreetTypeFTWY -- ^ Footway FTWY
  | StreetTypeFSHR -- ^ Foreshore FSHR
  | StreetTypeFORM -- ^ Formation FORM
  | StreetTypeFWY -- ^ Freeway FWY
  | StreetTypeFRNT -- ^ Front FRNT
  | StreetTypeFRTG -- ^ Frontage FRTG
  | StreetTypeGAP -- ^ Gap GAP
  | StreetTypeGDN -- ^ Garden GDN
  | StreetTypeGTE -- ^ Gate GTE
  | StreetTypeGDNS -- ^ Gardens GDNS
  | StreetTypeGTES -- ^ Gates GTES
  | StreetTypeGLD -- ^ Glade GLD
  | StreetTypeGLEN -- ^ Glen GLEN
  | StreetTypeGRA -- ^ Grange GRA
  | StreetTypeGRN -- ^ Green GRN
  | StreetTypeGRND -- ^ Ground GRND
  | StreetTypeGR -- ^ Grove GR
  | StreetTypeGLY -- ^ Gully GLY
  | StreetTypeHTS -- ^ Heights HTS
  | StreetTypeHRD -- ^ Highroad HRD
  | StreetTypeHWY -- ^ Highway HWY
  | StreetTypeHILL -- ^ Hill HILL
  | StreetTypeINTG -- ^ Interchange INTG
  | StreetTypeINTN -- ^ Intersection INTN
  | StreetTypeJNC -- ^ Junction JNC
  | StreetTypeKEY -- ^ Key KEY
  | StreetTypeLDG -- ^ Landing LDG
  | StreetTypeLANE -- ^ Lane LANE
  | StreetTypeLNWY -- ^ Laneway LNWY
  | StreetTypeLEES -- ^ Lees LEES
  | StreetTypeLINE -- ^ Line LINE
  | StreetTypeLINK -- ^ Link LINK
  | StreetTypeLT -- ^ Little LT
  | StreetTypeLKT -- ^ Lookout LKT
  | StreetTypeLOOP -- ^ Loop LOOP
  | StreetTypeLWR -- ^ Lower LWR
  | StreetTypeMALL -- ^ Mall MALL
  | StreetTypeMNDR -- ^ Meander MNDR
  | StreetTypeMEW -- ^ Mew MEW
  | StreetTypeMEWS -- ^ Mews MEWS
  | StreetTypeMWY -- ^ Motorway MWY
  | StreetTypeMT -- ^ Mount MT
  | StreetTypeNOOK -- ^ Nook NOOK
  | StreetTypeOTLK -- ^ Outlook OTLK
  | StreetTypePDE -- ^ Parade PDE
  | StreetTypePARK -- ^ Park PARK
  | StreetTypePKLD -- ^ Parklands PKLD
  | StreetTypePKWY -- ^ Parkway PKWY
  | StreetTypePART -- ^ Part PART
  | StreetTypePASS -- ^ Pass PASS
  | StreetTypePSGE -- ^ Passage PSGE
  | StreetTypePATH -- ^ Path PATH
  | StreetTypePHWY -- ^ Pathway PHWY
  | StreetTypePIAZ -- ^ Piazza PIAZ
  | StreetTypePL -- ^ Place PL
  | StreetTypePLAT -- ^ Plateau PLAT
  | StreetTypePLZA -- ^ Plaza PLZA
  | StreetTypePKT -- ^ Pocket PKT
  | StreetTypePNT -- ^ Point PNT
  | StreetTypePORT -- ^ Port PORT
  | StreetTypePROM -- ^ Promenade PROM
  | StreetTypeQUAD -- ^ Quad QUAD
  | StreetTypeQDGL -- ^ Quadrangle QDGL
  | StreetTypeQDRT -- ^ Quadrant QDRT
  | StreetTypeQY -- ^ Quay QY
  | StreetTypeQYS -- ^ Quays QYS
  | StreetTypeRMBL -- ^ Ramble RMBL
  | StreetTypeRAMP -- ^ Ramp RAMP
  | StreetTypeRNGE -- ^ Range RNGE
  | StreetTypeRCH -- ^ Reach RCH
  | StreetTypeRES -- ^ Reserve RES
  | StreetTypeREST -- ^ Rest REST
  | StreetTypeRTT -- ^ Retreat RTT
  | StreetTypeRIDE -- ^ Ride RIDE
  | StreetTypeRDGE -- ^ Ridge RDGE
  | StreetTypeRGWY -- ^ Ridgeway RGWY
  | StreetTypeROWY -- ^ Right of way ROWY
  | StreetTypeRING -- ^ Ring RING
  | StreetTypeRISE -- ^ Rise RISE
  | StreetTypeRVR -- ^ River RVR
  | StreetTypeRVWY -- ^ Riverway RVWY
  | StreetTypeRVRA -- ^ Riviera RVRA
  | StreetTypeRD -- ^ Road RD
  | StreetTypeRDS -- ^ Roads RDS
  | StreetTypeRDSD -- ^ Roadside RDSD
  | StreetTypeRDWY -- ^ Roadway RDWY
  | StreetTypeRNDE -- ^ Ronde RNDE
  | StreetTypeRSBL -- ^ Rosebowl RSBL
  | StreetTypeRTY -- ^ Rotary RTY
  | StreetTypeRND -- ^ Round RND
  | StreetTypeRTE -- ^ Route RTE
  | StreetTypeROW -- ^ Row ROW
  | StreetTypeRUE -- ^ Rue RUE
  | StreetTypeRUN -- ^ Run RUN
  | StreetTypeSWY -- ^ Service way SWY
  | StreetTypeSDNG -- ^ Siding SDNG
  | StreetTypeSLPE -- ^ Slope SLPE
  | StreetTypeSND -- ^ Sound SND
  | StreetTypeSPUR -- ^ Spur SPUR
  | StreetTypeSQ -- ^ Square SQ
  | StreetTypeSTRS -- ^ Stairs STRS
  | StreetTypeSHWY -- ^ State highway SHWY
  | StreetTypeSTPS -- ^ Steps STPS
  | StreetTypeSTRA -- ^ Strand STRA
  | StreetTypeST -- ^ Street ST
  | StreetTypeSTRP -- ^ Strip STRP
  | StreetTypeSBWY -- ^ Subway SBWY
  | StreetTypeTARN -- ^ Tarn TARN
  | StreetTypeTCE -- ^ Terrace TCE
  | StreetTypeTHOR -- ^ Thoroughfare THOR
  | StreetTypeTLWY -- ^ Tollway TLWY
  | StreetTypeTOP -- ^ Top TOP
  | StreetTypeTOR -- ^ Tor TOR
  | StreetTypeTWRS -- ^ Towers TWRS
  | StreetTypeTRK -- ^ Track TRK
  | StreetTypeTRL -- ^ Trail TRL
  | StreetTypeTRLR -- ^ Trailer TRLR
  | StreetTypeTRI -- ^ Triangle TRI
  | StreetTypeTKWY -- ^ Trunkway TKWY
  | StreetTypeTURN -- ^ Turn TURN
  | StreetTypeUPAS -- ^ Underpass UPAS
  | StreetTypeUPR -- ^ Upper UPR
  | StreetTypeVALE -- ^ Vale VALE
  | StreetTypeVDCT -- ^ Viaduct VDCT
  | StreetTypeVIEW -- ^ View VIEW
  | StreetTypeVLLS -- ^ Villas VLLS
  | StreetTypeVSTA -- ^ Vista VSTA
  | StreetTypeWADE -- ^ Wade WADE
  | StreetTypeWALK -- ^ Walk WALK
  | StreetTypeWKWY -- ^ Walkway WKWY
  | StreetTypeWAY -- ^ Way WAY
  | StreetTypeWHRF -- ^ Wharf WHRF
  | StreetTypeWYND -- ^ Wynd WYND
  | StreetTypeYARD -- ^ Yard YARD
  deriving (Bounded, Enum, Eq, Ord, Show)

streetTypeText ::
  Prism' Text StreetType
streetTypeText =
  prism (\case
            StreetTypeACCS -> "ACCS"
            StreetTypeALLY -> "ALLY"
            StreetTypeALWY -> "ALWY"
            StreetTypeAMBL -> "AMBL"
            StreetTypeANCG -> "ANCG"
            StreetTypeAPP -> "APP"
            StreetTypeARC -> "ARC"
            StreetTypeART -> "ART"
            StreetTypeAVE -> "AVE"
            StreetTypeBASN -> "BASN"
            StreetTypeBCH -> "BCH"
            StreetTypeBEND -> "BEND"
            StreetTypeBLK -> "BLK"
            StreetTypeBVD -> "BVD"
            StreetTypeBRCE -> "BRCE"
            StreetTypeBRAE -> "BRAE"
            StreetTypeBRK -> "BRK"
            StreetTypeBDGE -> "BDGE"
            StreetTypeBDWY -> "BDWY"
            StreetTypeBROW -> "BROW"
            StreetTypeBYPA -> "BYPA"
            StreetTypeBYWY -> "BYWY"
            StreetTypeCAUS -> "CAUS"
            StreetTypeCTR -> "CTR"
            StreetTypeCNWY -> "CNWY"
            StreetTypeCH -> "CH"
            StreetTypeCIR -> "CIR"
            StreetTypeCLT -> "CLT"
            StreetTypeCCT -> "CCT"
            StreetTypeCRCS -> "CRCS"
            StreetTypeCL -> "CL"
            StreetTypeCLDE -> "CLDE"
            StreetTypeCMMN -> "CMMN"
            StreetTypeCON -> "CON"
            StreetTypeCPS -> "CPS"
            StreetTypeCNR -> "CNR"
            StreetTypeCSO -> "CSO"
            StreetTypeCT -> "CT"
            StreetTypeCTYD -> "CTYD"
            StreetTypeCOVE -> "COVE"
            StreetTypeCRES -> "CRES"
            StreetTypeCRST -> "CRST"
            StreetTypeCRSS -> "CRSS"
            StreetTypeCRSG -> "CRSG"
            StreetTypeCRD -> "CRD"
            StreetTypeCOWY -> "COWY"
            StreetTypeCUWY -> "CUWY"
            StreetTypeCDS -> "CDS"
            StreetTypeCTTG -> "CTTG"
            StreetTypeDALE -> "DALE"
            StreetTypeDELL -> "DELL"
            StreetTypeDEVN -> "DEVN"
            StreetTypeDIP -> "DIP"
            StreetTypeDSTR -> "DSTR"
            StreetTypeDR -> "DR"
            StreetTypeDRWY -> "DRWY"
            StreetTypeEDGE -> "EDGE"
            StreetTypeELB -> "ELB"
            StreetTypeEND -> "END"
            StreetTypeENT -> "ENT"
            StreetTypeESP -> "ESP"
            StreetTypeEST -> "EST"
            StreetTypeEXP -> "EXP"
            StreetTypeEXTN -> "EXTN"
            StreetTypeFAWY -> "FAWY"
            StreetTypeFTRK -> "FTRK"
            StreetTypeFITR -> "FITR"
            StreetTypeFLAT -> "FLAT"
            StreetTypeFOLW -> "FOLW"
            StreetTypeFTWY -> "FTWY"
            StreetTypeFSHR -> "FSHR"
            StreetTypeFORM -> "FORM"
            StreetTypeFWY -> "FWY"
            StreetTypeFRNT -> "FRNT"
            StreetTypeFRTG -> "FRTG"
            StreetTypeGAP -> "GAP"
            StreetTypeGDN -> "GDN"
            StreetTypeGTE -> "GTE"
            StreetTypeGDNS -> "GDNS"
            StreetTypeGTES -> "GTES"
            StreetTypeGLD -> "GLD"
            StreetTypeGLEN -> "GLEN"
            StreetTypeGRA -> "GRA"
            StreetTypeGRN -> "GRN"
            StreetTypeGRND -> "GRND"
            StreetTypeGR -> "GR"
            StreetTypeGLY -> "GLY"
            StreetTypeHTS -> "HTS"
            StreetTypeHRD -> "HRD"
            StreetTypeHWY -> "HWY"
            StreetTypeHILL -> "HILL"
            StreetTypeINTG -> "INTG"
            StreetTypeINTN -> "INTN"
            StreetTypeJNC -> "JNC"
            StreetTypeKEY -> "KEY"
            StreetTypeLDG -> "LDG"
            StreetTypeLANE -> "LANE"
            StreetTypeLNWY -> "LNWY"
            StreetTypeLEES -> "LEES"
            StreetTypeLINE -> "LINE"
            StreetTypeLINK -> "LINK"
            StreetTypeLT -> "LT"
            StreetTypeLKT -> "LKT"
            StreetTypeLOOP -> "LOOP"
            StreetTypeLWR -> "LWR"
            StreetTypeMALL -> "MALL"
            StreetTypeMNDR -> "MNDR"
            StreetTypeMEW -> "MEW"
            StreetTypeMEWS -> "MEWS"
            StreetTypeMWY -> "MWY"
            StreetTypeMT -> "MT"
            StreetTypeNOOK -> "NOOK"
            StreetTypeOTLK -> "OTLK"
            StreetTypePDE -> "PDE"
            StreetTypePARK -> "PARK"
            StreetTypePKLD -> "PKLD"
            StreetTypePKWY -> "PKWY"
            StreetTypePART -> "PART"
            StreetTypePASS -> "PASS"
            StreetTypePSGE -> "PSGE"
            StreetTypePATH -> "PATH"
            StreetTypePHWY -> "PHWY"
            StreetTypePIAZ -> "PIAZ"
            StreetTypePL -> "PL"
            StreetTypePLAT -> "PLAT"
            StreetTypePLZA -> "PLZA"
            StreetTypePKT -> "PKT"
            StreetTypePNT -> "PNT"
            StreetTypePORT -> "PORT"
            StreetTypePROM -> "PROM"
            StreetTypeQUAD -> "QUAD"
            StreetTypeQDGL -> "QDGL"
            StreetTypeQDRT -> "QDRT"
            StreetTypeQY -> "QY"
            StreetTypeQYS -> "QYS"
            StreetTypeRMBL -> "RMBL"
            StreetTypeRAMP -> "RAMP"
            StreetTypeRNGE -> "RNGE"
            StreetTypeRCH -> "RCH"
            StreetTypeRES -> "RES"
            StreetTypeREST -> "REST"
            StreetTypeRTT -> "RTT"
            StreetTypeRIDE -> "RIDE"
            StreetTypeRDGE -> "RDGE"
            StreetTypeRGWY -> "RGWY"
            StreetTypeROWY -> "ROWY"
            StreetTypeRING -> "RING"
            StreetTypeRISE -> "RISE"
            StreetTypeRVR -> "RVR"
            StreetTypeRVWY -> "RVWY"
            StreetTypeRVRA -> "RVRA"
            StreetTypeRD -> "RD"
            StreetTypeRDS -> "RDS"
            StreetTypeRDSD -> "RDSD"
            StreetTypeRDWY -> "RDWY"
            StreetTypeRNDE -> "RNDE"
            StreetTypeRSBL -> "RSBL"
            StreetTypeRTY -> "RTY"
            StreetTypeRND -> "RND"
            StreetTypeRTE -> "RTE"
            StreetTypeROW -> "ROW"
            StreetTypeRUE -> "RUE"
            StreetTypeRUN -> "RUN"
            StreetTypeSWY -> "SWY"
            StreetTypeSDNG -> "SDNG"
            StreetTypeSLPE -> "SLPE"
            StreetTypeSND -> "SND"
            StreetTypeSPUR -> "SPUR"
            StreetTypeSQ -> "SQ"
            StreetTypeSTRS -> "STRS"
            StreetTypeSHWY -> "SHWY"
            StreetTypeSTPS -> "STPS"
            StreetTypeSTRA -> "STRA"
            StreetTypeST -> "ST"
            StreetTypeSTRP -> "STRP"
            StreetTypeSBWY -> "SBWY"
            StreetTypeTARN -> "TARN"
            StreetTypeTCE -> "TCE"
            StreetTypeTHOR -> "THOR"
            StreetTypeTLWY -> "TLWY"
            StreetTypeTOP -> "TOP"
            StreetTypeTOR -> "TOR"
            StreetTypeTWRS -> "TWRS"
            StreetTypeTRK -> "TRK"
            StreetTypeTRL -> "TRL"
            StreetTypeTRLR -> "TRLR"
            StreetTypeTRI -> "TRI"
            StreetTypeTKWY -> "TKWY"
            StreetTypeTURN -> "TURN"
            StreetTypeUPAS -> "UPAS"
            StreetTypeUPR -> "UPR"
            StreetTypeVALE -> "VALE"
            StreetTypeVDCT -> "VDCT"
            StreetTypeVIEW -> "VIEW"
            StreetTypeVLLS -> "VLLS"
            StreetTypeVSTA -> "VSTA"
            StreetTypeWADE -> "WADE"
            StreetTypeWALK -> "WALK"
            StreetTypeWKWY -> "WKWY"
            StreetTypeWAY -> "WAY"
            StreetTypeWHRF -> "WHRF"
            StreetTypeWYND -> "WYND"
            StreetTypeYARD -> "YARD"
        )
        (\case
            "ACCS" -> Right StreetTypeACCS
            "ALLY" -> Right StreetTypeALLY
            "ALWY" -> Right StreetTypeALWY
            "AMBL" -> Right StreetTypeAMBL
            "ANCG" -> Right StreetTypeANCG
            "APP" -> Right StreetTypeAPP
            "ARC" -> Right StreetTypeARC
            "ART" -> Right StreetTypeART
            "AVE" -> Right StreetTypeAVE
            "BASN" -> Right StreetTypeBASN
            "BCH" -> Right StreetTypeBCH
            "BEND" -> Right StreetTypeBEND
            "BLK" -> Right StreetTypeBLK
            "BVD" -> Right StreetTypeBVD
            "BRCE" -> Right StreetTypeBRCE
            "BRAE" -> Right StreetTypeBRAE
            "BRK" -> Right StreetTypeBRK
            "BDGE" -> Right StreetTypeBDGE
            "BDWY" -> Right StreetTypeBDWY
            "BROW" -> Right StreetTypeBROW
            "BYPA" -> Right StreetTypeBYPA
            "BYWY" -> Right StreetTypeBYWY
            "CAUS" -> Right StreetTypeCAUS
            "CTR" -> Right StreetTypeCTR
            "CNWY" -> Right StreetTypeCNWY
            "CH" -> Right StreetTypeCH
            "CIR" -> Right StreetTypeCIR
            "CLT" -> Right StreetTypeCLT
            "CCT" -> Right StreetTypeCCT
            "CRCS" -> Right StreetTypeCRCS
            "CL" -> Right StreetTypeCL
            "CLDE" -> Right StreetTypeCLDE
            "CMMN" -> Right StreetTypeCMMN
            "CON" -> Right StreetTypeCON
            "CPS" -> Right StreetTypeCPS
            "CNR" -> Right StreetTypeCNR
            "CSO" -> Right StreetTypeCSO
            "CT" -> Right StreetTypeCT
            "CTYD" -> Right StreetTypeCTYD
            "COVE" -> Right StreetTypeCOVE
            "CRES" -> Right StreetTypeCRES
            "CRST" -> Right StreetTypeCRST
            "CRSS" -> Right StreetTypeCRSS
            "CRSG" -> Right StreetTypeCRSG
            "CRD" -> Right StreetTypeCRD
            "COWY" -> Right StreetTypeCOWY
            "CUWY" -> Right StreetTypeCUWY
            "CDS" -> Right StreetTypeCDS
            "CTTG" -> Right StreetTypeCTTG
            "DALE" -> Right StreetTypeDALE
            "DELL" -> Right StreetTypeDELL
            "DEVN" -> Right StreetTypeDEVN
            "DIP" -> Right StreetTypeDIP
            "DSTR" -> Right StreetTypeDSTR
            "DR" -> Right StreetTypeDR
            "DRWY" -> Right StreetTypeDRWY
            "EDGE" -> Right StreetTypeEDGE
            "ELB" -> Right StreetTypeELB
            "END" -> Right StreetTypeEND
            "ENT" -> Right StreetTypeENT
            "ESP" -> Right StreetTypeESP
            "EST" -> Right StreetTypeEST
            "EXP" -> Right StreetTypeEXP
            "EXTN" -> Right StreetTypeEXTN
            "FAWY" -> Right StreetTypeFAWY
            "FTRK" -> Right StreetTypeFTRK
            "FITR" -> Right StreetTypeFITR
            "FLAT" -> Right StreetTypeFLAT
            "FOLW" -> Right StreetTypeFOLW
            "FTWY" -> Right StreetTypeFTWY
            "FSHR" -> Right StreetTypeFSHR
            "FORM" -> Right StreetTypeFORM
            "FWY" -> Right StreetTypeFWY
            "FRNT" -> Right StreetTypeFRNT
            "FRTG" -> Right StreetTypeFRTG
            "GAP" -> Right StreetTypeGAP
            "GDN" -> Right StreetTypeGDN
            "GTE" -> Right StreetTypeGTE
            "GDNS" -> Right StreetTypeGDNS
            "GTES" -> Right StreetTypeGTES
            "GLD" -> Right StreetTypeGLD
            "GLEN" -> Right StreetTypeGLEN
            "GRA" -> Right StreetTypeGRA
            "GRN" -> Right StreetTypeGRN
            "GRND" -> Right StreetTypeGRND
            "GR" -> Right StreetTypeGR
            "GLY" -> Right StreetTypeGLY
            "HTS" -> Right StreetTypeHTS
            "HRD" -> Right StreetTypeHRD
            "HWY" -> Right StreetTypeHWY
            "HILL" -> Right StreetTypeHILL
            "INTG" -> Right StreetTypeINTG
            "INTN" -> Right StreetTypeINTN
            "JNC" -> Right StreetTypeJNC
            "KEY" -> Right StreetTypeKEY
            "LDG" -> Right StreetTypeLDG
            "LANE" -> Right StreetTypeLANE
            "LNWY" -> Right StreetTypeLNWY
            "LEES" -> Right StreetTypeLEES
            "LINE" -> Right StreetTypeLINE
            "LINK" -> Right StreetTypeLINK
            "LT" -> Right StreetTypeLT
            "LKT" -> Right StreetTypeLKT
            "LOOP" -> Right StreetTypeLOOP
            "LWR" -> Right StreetTypeLWR
            "MALL" -> Right StreetTypeMALL
            "MNDR" -> Right StreetTypeMNDR
            "MEW" -> Right StreetTypeMEW
            "MEWS" -> Right StreetTypeMEWS
            "MWY" -> Right StreetTypeMWY
            "MT" -> Right StreetTypeMT
            "NOOK" -> Right StreetTypeNOOK
            "OTLK" -> Right StreetTypeOTLK
            "PDE" -> Right StreetTypePDE
            "PARK" -> Right StreetTypePARK
            "PKLD" -> Right StreetTypePKLD
            "PKWY" -> Right StreetTypePKWY
            "PART" -> Right StreetTypePART
            "PASS" -> Right StreetTypePASS
            "PSGE" -> Right StreetTypePSGE
            "PATH" -> Right StreetTypePATH
            "PHWY" -> Right StreetTypePHWY
            "PIAZ" -> Right StreetTypePIAZ
            "PL" -> Right StreetTypePL
            "PLAT" -> Right StreetTypePLAT
            "PLZA" -> Right StreetTypePLZA
            "PKT" -> Right StreetTypePKT
            "PNT" -> Right StreetTypePNT
            "PORT" -> Right StreetTypePORT
            "PROM" -> Right StreetTypePROM
            "QUAD" -> Right StreetTypeQUAD
            "QDGL" -> Right StreetTypeQDGL
            "QDRT" -> Right StreetTypeQDRT
            "QY" -> Right StreetTypeQY
            "QYS" -> Right StreetTypeQYS
            "RMBL" -> Right StreetTypeRMBL
            "RAMP" -> Right StreetTypeRAMP
            "RNGE" -> Right StreetTypeRNGE
            "RCH" -> Right StreetTypeRCH
            "RES" -> Right StreetTypeRES
            "REST" -> Right StreetTypeREST
            "RTT" -> Right StreetTypeRTT
            "RIDE" -> Right StreetTypeRIDE
            "RDGE" -> Right StreetTypeRDGE
            "RGWY" -> Right StreetTypeRGWY
            "ROWY" -> Right StreetTypeROWY
            "RING" -> Right StreetTypeRING
            "RISE" -> Right StreetTypeRISE
            "RVR" -> Right StreetTypeRVR
            "RVWY" -> Right StreetTypeRVWY
            "RVRA" -> Right StreetTypeRVRA
            "RD" -> Right StreetTypeRD
            "RDS" -> Right StreetTypeRDS
            "RDSD" -> Right StreetTypeRDSD
            "RDWY" -> Right StreetTypeRDWY
            "RNDE" -> Right StreetTypeRNDE
            "RSBL" -> Right StreetTypeRSBL
            "RTY" -> Right StreetTypeRTY
            "RND" -> Right StreetTypeRND
            "RTE" -> Right StreetTypeRTE
            "ROW" -> Right StreetTypeROW
            "RUE" -> Right StreetTypeRUE
            "RUN" -> Right StreetTypeRUN
            "SWY" -> Right StreetTypeSWY
            "SDNG" -> Right StreetTypeSDNG
            "SLPE" -> Right StreetTypeSLPE
            "SND" -> Right StreetTypeSND
            "SPUR" -> Right StreetTypeSPUR
            "SQ" -> Right StreetTypeSQ
            "STRS" -> Right StreetTypeSTRS
            "SHWY" -> Right StreetTypeSHWY
            "STPS" -> Right StreetTypeSTPS
            "STRA" -> Right StreetTypeSTRA
            "ST" -> Right StreetTypeST
            "STRP" -> Right StreetTypeSTRP
            "SBWY" -> Right StreetTypeSBWY
            "TARN" -> Right StreetTypeTARN
            "TCE" -> Right StreetTypeTCE
            "THOR" -> Right StreetTypeTHOR
            "TLWY" -> Right StreetTypeTLWY
            "TOP" -> Right StreetTypeTOP
            "TOR" -> Right StreetTypeTOR
            "TWRS" -> Right StreetTypeTWRS
            "TRK" -> Right StreetTypeTRK
            "TRL" -> Right StreetTypeTRL
            "TRLR" -> Right StreetTypeTRLR
            "TRI" -> Right StreetTypeTRI
            "TKWY" -> Right StreetTypeTKWY
            "TURN" -> Right StreetTypeTURN
            "UPAS" -> Right StreetTypeUPAS
            "UPR" -> Right StreetTypeUPR
            "VALE" -> Right StreetTypeVALE
            "VDCT" -> Right StreetTypeVDCT
            "VIEW" -> Right StreetTypeVIEW
            "VLLS" -> Right StreetTypeVLLS
            "VSTA" -> Right StreetTypeVSTA
            "WADE" -> Right StreetTypeWADE
            "WALK" -> Right StreetTypeWALK
            "WKWY" -> Right StreetTypeWKWY
            "WAY" -> Right StreetTypeWAY
            "WHRF" -> Right StreetTypeWHRF
            "WYND" -> Right StreetTypeWYND
            "YARD" -> Right StreetTypeYARD
            t -> Left t
        )

streetTypeEncoder :: Applicative f => Encoder f StreetType
streetTypeEncoder = E.prismE streetTypeText E.text

streetTypeDecoder :: Monad f => Decoder f StreetType
streetTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid PAF StreetType")
  streetTypeText
  D.text
