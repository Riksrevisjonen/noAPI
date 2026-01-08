# Get countries

Get all country codes (alpha-3) based on the ISO 3166-1 standard, but
adjusted for data from Statistics Norway.

## Usage

``` r
get_countries(
  year = format(Sys.Date(), "%Y"),
  include_notes = FALSE,
  simplify = TRUE,
  raw_response = FALSE
)
```

## Arguments

- year:

  The year for which the codes should be valid for.

- include_notes:

  If notes should be included or not.

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

data.frame or list

## Details

Fetches all countries and country codes (alpha-3) for a given year based
on the ISO 3166-1 standard. The list includes special codes XUK and XXX
for unknown citizenship and stateless persons, respectively. These codes
are used in official statistics from Statistics Norway.

All years from 1974 until present are supported. The function defaults
to the current year.

If notes are enabled with `include_notes`, a column `note` will be added
to the data.frame. Notes are stated with a code reference that has the
following order (alpha-2, alpha-3, num-3, SSB-3), for example Norway
(NO, NOR, 578, 000). See [Statistics Norway's
webpage](https://www.ssb.no/klass/klassifikasjoner/552) for more
information on the code specification.

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

## Examples

``` r
# Get country for current year
get_countries()
#>     year code                                        name
#> 1   2026  ABW                                       Aruba
#> 2   2026  AFG                                 Afghanistan
#> 3   2026  AGO                                      Angola
#> 4   2026  AIA                                    Anguilla
#> 5   2026  ALA                                       Åland
#> 6   2026  ALB                                     Albania
#> 7   2026  AND                                     Andorra
#> 8   2026  ARE                De forente arabiske emirater
#> 9   2026  ARG                                   Argentina
#> 10  2026  ARM                                     Armenia
#> 11  2026  ASM                            Amerikansk Samoa
#> 12  2026  ATA                                   Antarktis
#> 13  2026  ATF                   De franske sørterritorier
#> 14  2026  ATG                          Antigua og Barbuda
#> 15  2026  AUS                                   Australia
#> 16  2026  AUT                                   Østerrike
#> 17  2026  AZE                                Aserbajdsjan
#> 18  2026  BDI                                     Burundi
#> 19  2026  BEL                                      Belgia
#> 20  2026  BEN                                       Benin
#> 21  2026  BES             Bonaire, Sint Eustatius og Saba
#> 22  2026  BFA                                Burkina Faso
#> 23  2026  BGD                                  Bangladesh
#> 24  2026  BGR                                    Bulgaria
#> 25  2026  BHR                                     Bahrain
#> 26  2026  BHS                                     Bahamas
#> 27  2026  BIH                          Bosnia-Hercegovina
#> 28  2026  BLM                            Saint-Barthélemy
#> 29  2026  BLR                                     Belarus
#> 30  2026  BLZ                                      Belize
#> 31  2026  BMU                                     Bermuda
#> 32  2026  BOL                                     Bolivia
#> 33  2026  BRA                                      Brasil
#> 34  2026  BRB                                    Barbados
#> 35  2026  BRN                                      Brunei
#> 36  2026  BTN                                      Bhutan
#> 37  2026  BVT                                   Bouvetøya
#> 38  2026  BWA                                    Botswana
#> 39  2026  CAF             Den sentralafrikanske republikk
#> 40  2026  CAN                                      Canada
#> 41  2026  CCK                                  Kokosøyene
#> 42  2026  CHE                                      Sveits
#> 43  2026  CHL                                       Chile
#> 44  2026  CHN                                        Kina
#> 45  2026  CIV                            Elfenbeinskysten
#> 46  2026  CMR                                     Kamerun
#> 47  2026  COD                                       Kongo
#> 48  2026  COG                         Kongo - Brazzaville
#> 49  2026  COK                                   Cookøyene
#> 50  2026  COL                                    Colombia
#> 51  2026  COM                                    Komorene
#> 52  2026  CPV                                  Kapp Verde
#> 53  2026  CRI                                  Costa Rica
#> 54  2026  CUB                                        Cuba
#> 55  2026  CUW                                     Curaçao
#> 56  2026  CXR                                Christmasøya
#> 57  2026  CYM                                 Caymanøyene
#> 58  2026  CYP                                      Kypros
#> 59  2026  CZE                                    Tsjekkia
#> 60  2026  DEU                                    Tyskland
#> 61  2026  DJI                                    Djibouti
#> 62  2026  DMA                                    Dominica
#> 63  2026  DNK                                     Danmark
#> 64  2026  DOM                  Den dominikanske republikk
#> 65  2026  DZA                                     Algerie
#> 66  2026  ECU                                     Ecuador
#> 67  2026  EGY                                       Egypt
#> 68  2026  ERI                                     Eritrea
#> 69  2026  ESH                                 Vest-Sahara
#> 70  2026  ESP                                      Spania
#> 71  2026  EST                                     Estland
#> 72  2026  ETH                                     Etiopia
#> 73  2026  FIN                                     Finland
#> 74  2026  FJI                                        Fiji
#> 75  2026  FLK                              Falklandsøyene
#> 76  2026  FRA                                   Frankrike
#> 77  2026  FRO                                    Færøyene
#> 78  2026  FSM                      Mikronesiaføderasjonen
#> 79  2026  GAB                                       Gabon
#> 80  2026  GBR                               Storbritannia
#> 81  2026  GEO                                     Georgia
#> 82  2026  GGY                                    Guernsey
#> 83  2026  GHA                                       Ghana
#> 84  2026  GIB                                   Gibraltar
#> 85  2026  GIN                                      Guinea
#> 86  2026  GLP                                  Guadeloupe
#> 87  2026  GMB                                      Gambia
#> 88  2026  GNB                               Guinea-Bissau
#> 89  2026  GNQ                           Ekvatorial-Guinea
#> 90  2026  GRC                                      Hellas
#> 91  2026  GRD                                     Grenada
#> 92  2026  GRL                                    Grønland
#> 93  2026  GTM                                   Guatemala
#> 94  2026  GUF                               Fransk Guyana
#> 95  2026  GUM                                        Guam
#> 96  2026  GUY                                      Guyana
#> 97  2026  HKG                                    Hongkong
#> 98  2026  HMD                     Heard- og McDonaldøyene
#> 99  2026  HND                                    Honduras
#> 100 2026  HRV                                     Kroatia
#> 101 2026  HTI                                       Haiti
#> 102 2026  HUN                                      Ungarn
#> 103 2026  IDN                                   Indonesia
#> 104 2026  IMN                                         Man
#> 105 2026  IND                                       India
#> 106 2026  IOT       Det britiske territorium i Indiahavet
#> 107 2026  IRL                                      Irland
#> 108 2026  IRN                                        Iran
#> 109 2026  IRQ                                        Irak
#> 110 2026  ISL                                      Island
#> 111 2026  ISR                                      Israel
#> 112 2026  ITA                                      Italia
#> 113 2026  JAM                                     Jamaica
#> 114 2026  JEY                                      Jersey
#> 115 2026  JOR                                      Jordan
#> 116 2026  JPN                                       Japan
#> 117 2026  KAZ                                  Kasakhstan
#> 118 2026  KEN                                       Kenya
#> 119 2026  KGZ                                 Kirgisistan
#> 120 2026  KHM                                   Kambodsja
#> 121 2026  KIR                                    Kiribati
#> 122 2026  KNA                          St. Kitts og Nevis
#> 123 2026  KOR                                   Sør-Korea
#> 124 2026  KWT                                      Kuwait
#> 125 2026  LAO                                        Laos
#> 126 2026  LBN                                     Libanon
#> 127 2026  LBR                                     Liberia
#> 128 2026  LBY                                       Libya
#> 129 2026  LCA                                   St. Lucia
#> 130 2026  LIE                               Liechtenstein
#> 131 2026  LKA                                   Sri Lanka
#> 132 2026  LSO                                     Lesotho
#> 133 2026  LTU                                     Litauen
#> 134 2026  LUX                                  Luxembourg
#> 135 2026  LVA                                      Latvia
#> 136 2026  MAC                                       Macao
#> 137 2026  MAF                           Saint-Martin (FR)
#> 138 2026  MAR                                     Marokko
#> 139 2026  MCO                                      Monaco
#> 140 2026  MDA                                     Moldova
#> 141 2026  MDG                                  Madagaskar
#> 142 2026  MDV                                   Maldivene
#> 143 2026  MEX                                      Mexico
#> 144 2026  MHL                               Marshalløyene
#> 145 2026  MKD                              Nord-Makedonia
#> 146 2026  MLI                                        Mali
#> 147 2026  MLT                                       Malta
#> 148 2026  MMR                                     Myanmar
#> 149 2026  MNE                                  Montenegro
#> 150 2026  MNG                                    Mongolia
#> 151 2026  MNP                              Nord-Marianene
#> 152 2026  MOZ                                    Mosambik
#> 153 2026  MRT                                  Mauritania
#> 154 2026  MSR                                  Montserrat
#> 155 2026  MTQ                                  Martinique
#> 156 2026  MUS                                   Mauritius
#> 157 2026  MWI                                      Malawi
#> 158 2026  MYS                                    Malaysia
#> 159 2026  MYT                                     Mayotte
#> 160 2026  NAM                                     Namibia
#> 161 2026  NCL                                Ny-Caledonia
#> 162 2026  NER                                       Niger
#> 163 2026  NFK                                  Norfolkøya
#> 164 2026  NGA                                     Nigeria
#> 165 2026  NIC                                   Nicaragua
#> 166 2026  NIU                                        Niue
#> 167 2026  NLD                                   Nederland
#> 168 2026  NOR                                       Norge
#> 169 2026  NPL                                       Nepal
#> 170 2026  NRU                                       Nauru
#> 171 2026  NZL                                 New Zealand
#> 172 2026  OMN                                        Oman
#> 173 2026  PAK                                    Pakistan
#> 174 2026  PAN                                      Panama
#> 175 2026  PCN                                    Pitcairn
#> 176 2026  PER                                        Peru
#> 177 2026  PHL                                 Filippinene
#> 178 2026  PLW                                       Palau
#> 179 2026  PNG                             Papua Ny-Guinea
#> 180 2026  POL                                       Polen
#> 181 2026  PRI                                 Puerto Rico
#> 182 2026  PRK                                  Nord-Korea
#> 183 2026  PRT                                    Portugal
#> 184 2026  PRY                                    Paraguay
#> 185 2026  PSE                                   Palestina
#> 186 2026  PYF                            Fransk Polynesia
#> 187 2026  QAT                                       Qatar
#> 188 2026  REU                                     Réunion
#> 189 2026  ROU                                     Romania
#> 190 2026  RUS                                    Russland
#> 191 2026  RWA                                      Rwanda
#> 192 2026  SAU                                Saudi-Arabia
#> 193 2026  SDN                                       Sudan
#> 194 2026  SEN                                     Senegal
#> 195 2026  SGP                                   Singapore
#> 196 2026  SGS            Sør-Georgia og Sør-Sandwichøyene
#> 197 2026  SHN Sankt Helena, Ascension og Tristan da Cunha
#> 198 2026  SJM                       Svalbard og Jan Mayen
#> 199 2026  SLB                                Salomonøyene
#> 200 2026  SLE                                Sierra Leone
#> 201 2026  SLV                                 El Salvador
#> 202 2026  SMR                                  San Marino
#> 203 2026  SOM                                     Somalia
#> 204 2026  SPM                    Saint-Pierre-et-Miquelon
#> 205 2026  SRB                                      Serbia
#> 206 2026  SSD                                   Sør-Sudan
#> 207 2026  STP                        São Tomé og Príncipe
#> 208 2026  SUR                                     Surinam
#> 209 2026  SVK                                    Slovakia
#> 210 2026  SVN                                    Slovenia
#> 211 2026  SWE                                     Sverige
#> 212 2026  SWZ                                    Eswatini
#> 213 2026  SXM                           Sint Maarten (NL)
#> 214 2026  SYC                                 Seychellene
#> 215 2026  SYR                                       Syria
#> 216 2026  TCA                       Turks- og Caicosøyene
#> 217 2026  TCD                                       Tsjad
#> 218 2026  TGO                                        Togo
#> 219 2026  THA                                    Thailand
#> 220 2026  TJK                                Tadsjikistan
#> 221 2026  TKL                                     Tokelau
#> 222 2026  TKM                                Turkmenistan
#> 223 2026  TLS                                   Øst-Timor
#> 224 2026  TON                                       Tonga
#> 225 2026  TTO                          Trinidad og Tobago
#> 226 2026  TUN                                     Tunisia
#> 227 2026  TUR                                      Tyrkia
#> 228 2026  TUV                                      Tuvalu
#> 229 2026  TWN                                      Taiwan
#> 230 2026  TZA                                    Tanzania
#> 231 2026  UGA                                      Uganda
#> 232 2026  UKR                                     Ukraina
#> 233 2026  UMI                           USAs ytre småøyer
#> 234 2026  URY                                     Uruguay
#> 235 2026  USA                                         USA
#> 236 2026  UZB                                  Usbekistan
#> 237 2026  VAT                               Vatikanstaten
#> 238 2026  VCT                  St. Vincent og Grenadinene
#> 239 2026  VEN                                   Venezuela
#> 240 2026  VGB                      De britiske Jomfruøyer
#> 241 2026  VIR                   De amerikanske Jomfruøyer
#> 242 2026  VNM                                     Vietnam
#> 243 2026  VUT                                     Vanuatu
#> 244 2026  WLF                      Wallis- og Futunaøyene
#> 245 2026  WSM                                       Samoa
#> 246 2026  XUK                                    Uoppgitt
#> 247 2026  XXK                                      Kosovo
#> 248 2026  XXX                                    Statsløs
#> 249 2026  YEM                                       Jemen
#> 250 2026  ZAF                                  Sør-Afrika
#> 251 2026  ZMB                                      Zambia
#> 252 2026  ZWE                                    Zimbabwe
```
