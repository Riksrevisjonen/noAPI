# Get municipalities and counties

Get all Norwegian municipalities or counties, and their respective
codes, for a given year.

## Usage

``` r
get_municipalities(
  year = format(Sys.Date(), "%Y"),
  include_notes = FALSE,
  simplify = TRUE,
  raw_response = FALSE
)

get_counties(
  year = format(Sys.Date(), "%Y"),
  include_notes = FALSE,
  simplify = TRUE,
  raw_response = FALSE
)

get_adm_units(year = format(Sys.Date(), "%Y"), simplify = TRUE)
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

`get_municipalities()` supports all years from 1977, while
`get_counties()` supports all years from 1972. `get_adm_units()` is a
wrapper function to get both municipality and county codes in the same
function call. All three functions default to the current year.

The functions returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

## Examples

``` r
# Get municipality codes
get_municipalities()
#>     year code                               name
#> 1   2026 0301                               Oslo
#> 2   2026 1101                          Eigersund
#> 3   2026 1103                          Stavanger
#> 4   2026 1106                          Haugesund
#> 5   2026 1108                            Sandnes
#> 6   2026 1111                            Sokndal
#> 7   2026 1112                               Lund
#> 8   2026 1114                          Bjerkreim
#> 9   2026 1119                                 Hå
#> 10  2026 1120                              Klepp
#> 11  2026 1121                               Time
#> 12  2026 1122                            Gjesdal
#> 13  2026 1124                               Sola
#> 14  2026 1127                          Randaberg
#> 15  2026 1130                             Strand
#> 16  2026 1133                         Hjelmeland
#> 17  2026 1134                             Suldal
#> 18  2026 1135                              Sauda
#> 19  2026 1144                            Kvitsøy
#> 20  2026 1145                               Bokn
#> 21  2026 1146                             Tysvær
#> 22  2026 1149                             Karmøy
#> 23  2026 1151                             Utsira
#> 24  2026 1160                         Vindafjord
#> 25  2026 1505                       Kristiansund
#> 26  2026 1506                              Molde
#> 27  2026 1508                            Ålesund
#> 28  2026 1511                           Vanylven
#> 29  2026 1514                              Sande
#> 30  2026 1515            Herøy (Møre og Romsdal)
#> 31  2026 1516                            Ulstein
#> 32  2026 1517                             Hareid
#> 33  2026 1520                              Ørsta
#> 34  2026 1525                            Stranda
#> 35  2026 1528                          Sykkylven
#> 36  2026 1531                               Sula
#> 37  2026 1532                              Giske
#> 38  2026 1535                            Vestnes
#> 39  2026 1539                              Rauma
#> 40  2026 1547                              Aukra
#> 41  2026 1554                             Averøy
#> 42  2026 1557                            Gjemnes
#> 43  2026 1560                           Tingvoll
#> 44  2026 1563                            Sunndal
#> 45  2026 1566                           Surnadal
#> 46  2026 1573                              Smøla
#> 47  2026 1576                               Aure
#> 48  2026 1577                              Volda
#> 49  2026 1578                              Fjord
#> 50  2026 1579                         Hustadvika
#> 51  2026 1580                              Haram
#> 52  2026 1804                               Bodø
#> 53  2026 1806                             Narvik
#> 54  2026 1811                             Bindal
#> 55  2026 1812                              Sømna
#> 56  2026 1813                            Brønnøy
#> 57  2026 1815                               Vega
#> 58  2026 1816                          Vevelstad
#> 59  2026 1818                   Herøy (Nordland)
#> 60  2026 1820                          Alstahaug
#> 61  2026 1822                          Leirfjord
#> 62  2026 1824                              Vefsn
#> 63  2026 1825                              Grane
#> 64  2026 1826            Aarborte - Hattfjelldal
#> 65  2026 1827                              Dønna
#> 66  2026 1828                              Nesna
#> 67  2026 1832                             Hemnes
#> 68  2026 1833                       Rana - Raane
#> 69  2026 1834                              Lurøy
#> 70  2026 1835                              Træna
#> 71  2026 1836                              Rødøy
#> 72  2026 1837                              Meløy
#> 73  2026 1838                          Gildeskål
#> 74  2026 1839                             Beiarn
#> 75  2026 1840                            Saltdal
#> 76  2026 1841                   Fauske - Fuossko
#> 77  2026 1845                  Sørfold - Fuolldá
#> 78  2026 1848                            Steigen
#> 79  2026 1851                           Lødingen
#> 80  2026 1853                  Evenes - Evená??i
#> 81  2026 1856                               Røst
#> 82  2026 1857                              Værøy
#> 83  2026 1859                           Flakstad
#> 84  2026 1860                          Vestvågøy
#> 85  2026 1865                              Vågan
#> 86  2026 1866                             Hadsel
#> 87  2026 1867                                 Bø
#> 88  2026 1868                             Øksnes
#> 89  2026 1870                  Sortland - Suortá
#> 90  2026 1871                              Andøy
#> 91  2026 1874                           Moskenes
#> 92  2026 1875                   Hábmer - Hamarøy
#> 93  2026 3101                             Halden
#> 94  2026 3103                               Moss
#> 95  2026 3105                          Sarpsborg
#> 96  2026 3107                        Fredrikstad
#> 97  2026 3110                             Hvaler
#> 98  2026 3112                               Råde
#> 99  2026 3114                    Våler (Østfold)
#> 100 2026 3116                           Skiptvet
#> 101 2026 3118                      Indre Østfold
#> 102 2026 3120                          Rakkestad
#> 103 2026 3122                             Marker
#> 104 2026 3124                            Aremark
#> 105 2026 3201                              Bærum
#> 106 2026 3203                              Asker
#> 107 2026 3205                         Lillestrøm
#> 108 2026 3207                       Nordre Follo
#> 109 2026 3209                         Ullensaker
#> 110 2026 3212                           Nesodden
#> 111 2026 3214                              Frogn
#> 112 2026 3216                             Vestby
#> 113 2026 3218                                 Ås
#> 114 2026 3220                            Enebakk
#> 115 2026 3222                          Lørenskog
#> 116 2026 3224                           Rælingen
#> 117 2026 3226                     Aurskog-Høland
#> 118 2026 3228                                Nes
#> 119 2026 3230                           Gjerdrum
#> 120 2026 3232                           Nittedal
#> 121 2026 3234                             Lunner
#> 122 2026 3236                           Jevnaker
#> 123 2026 3238                          Nannestad
#> 124 2026 3240                           Eidsvoll
#> 125 2026 3242                             Hurdal
#> 126 2026 3301                            Drammen
#> 127 2026 3303                          Kongsberg
#> 128 2026 3305                          Ringerike
#> 129 2026 3310                               Hole
#> 130 2026 3312                               Lier
#> 131 2026 3314                         Øvre Eiker
#> 132 2026 3316                              Modum
#> 133 2026 3318                         Krødsherad
#> 134 2026 3320                                Flå
#> 135 2026 3322                            Nesbyen
#> 136 2026 3324                                Gol
#> 137 2026 3326                           Hemsedal
#> 138 2026 3328                                 Ål
#> 139 2026 3330                                Hol
#> 140 2026 3332                             Sigdal
#> 141 2026 3334                           Flesberg
#> 142 2026 3336                             Rollag
#> 143 2026 3338                      Nore og Uvdal
#> 144 2026 3401                        Kongsvinger
#> 145 2026 3403                              Hamar
#> 146 2026 3405                        Lillehammer
#> 147 2026 3407                             Gjøvik
#> 148 2026 3411                          Ringsaker
#> 149 2026 3412                              Løten
#> 150 2026 3413                             Stange
#> 151 2026 3414                          Nord-Odal
#> 152 2026 3415                           Sør-Odal
#> 153 2026 3416                            Eidskog
#> 154 2026 3417                               Grue
#> 155 2026 3418                              Åsnes
#> 156 2026 3419                  Våler (Innlandet)
#> 157 2026 3420                            Elverum
#> 158 2026 3421                             Trysil
#> 159 2026 3422                               Åmot
#> 160 2026 3423                        Stor-Elvdal
#> 161 2026 3424                           Rendalen
#> 162 2026 3425                           Engerdal
#> 163 2026 3426                              Tolga
#> 164 2026 3427                             Tynset
#> 165 2026 3428                             Alvdal
#> 166 2026 3429                            Folldal
#> 167 2026 3430                                 Os
#> 168 2026 3431                              Dovre
#> 169 2026 3432                              Lesja
#> 170 2026 3433                              Skjåk
#> 171 2026 3434                                Lom
#> 172 2026 3435                               Vågå
#> 173 2026 3436                          Nord-Fron
#> 174 2026 3437                                Sel
#> 175 2026 3438                           Sør-Fron
#> 176 2026 3439                            Ringebu
#> 177 2026 3440                               Øyer
#> 178 2026 3441                            Gausdal
#> 179 2026 3442                        Østre Toten
#> 180 2026 3443                       Vestre Toten
#> 181 2026 3446                               Gran
#> 182 2026 3447                        Søndre Land
#> 183 2026 3448                        Nordre Land
#> 184 2026 3449                         Sør-Aurdal
#> 185 2026 3450                            Etnedal
#> 186 2026 3451                        Nord-Aurdal
#> 187 2026 3452                      Vestre Slidre
#> 188 2026 3453                      Øystre Slidre
#> 189 2026 3454                               Vang
#> 190 2026 3901                             Horten
#> 191 2026 3903                        Holmestrand
#> 192 2026 3905                           Tønsberg
#> 193 2026 3907                         Sandefjord
#> 194 2026 3909                             Larvik
#> 195 2026 3911                             Færder
#> 196 2026 4001                          Porsgrunn
#> 197 2026 4003                              Skien
#> 198 2026 4005                           Notodden
#> 199 2026 4010                             Siljan
#> 200 2026 4012                             Bamble
#> 201 2026 4014                            Kragerø
#> 202 2026 4016                          Drangedal
#> 203 2026 4018                               Nome
#> 204 2026 4020                      Midt-Telemark
#> 205 2026 4022                            Seljord
#> 206 2026 4024                           Hjartdal
#> 207 2026 4026                               Tinn
#> 208 2026 4028                          Kviteseid
#> 209 2026 4030                           Nissedal
#> 210 2026 4032                           Fyresdal
#> 211 2026 4034                              Tokke
#> 212 2026 4036                              Vinje
#> 213 2026 4201                              Risør
#> 214 2026 4202                           Grimstad
#> 215 2026 4203                            Arendal
#> 216 2026 4204                       Kristiansand
#> 217 2026 4205                          Lindesnes
#> 218 2026 4206                            Farsund
#> 219 2026 4207                        Flekkefjord
#> 220 2026 4211                           Gjerstad
#> 221 2026 4212                          Vegårshei
#> 222 2026 4213                        Tvedestrand
#> 223 2026 4214                            Froland
#> 224 2026 4215                          Lillesand
#> 225 2026 4216                           Birkenes
#> 226 2026 4217                               Åmli
#> 227 2026 4218                            Iveland
#> 228 2026 4219                    Evje og Hornnes
#> 229 2026 4220                            Bygland
#> 230 2026 4221                              Valle
#> 231 2026 4222                              Bykle
#> 232 2026 4223                           Vennesla
#> 233 2026 4224                             Åseral
#> 234 2026 4225                            Lyngdal
#> 235 2026 4226                         Hægebostad
#> 236 2026 4227                          Kvinesdal
#> 237 2026 4228                             Sirdal
#> 238 2026 4601                             Bergen
#> 239 2026 4602                               Kinn
#> 240 2026 4611                               Etne
#> 241 2026 4612                              Sveio
#> 242 2026 4613                              Bømlo
#> 243 2026 4614                              Stord
#> 244 2026 4615                             Fitjar
#> 245 2026 4616                             Tysnes
#> 246 2026 4617                         Kvinnherad
#> 247 2026 4618                         Ullensvang
#> 248 2026 4619                           Eidfjord
#> 249 2026 4620                              Ulvik
#> 250 2026 4621                               Voss
#> 251 2026 4622                               Kvam
#> 252 2026 4623                          Samnanger
#> 253 2026 4624                      Bjørnafjorden
#> 254 2026 4625                          Austevoll
#> 255 2026 4626                           Øygarden
#> 256 2026 4627                              Askøy
#> 257 2026 4628                            Vaksdal
#> 258 2026 4629                            Modalen
#> 259 2026 4630                            Osterøy
#> 260 2026 4631                              Alver
#> 261 2026 4632                          Austrheim
#> 262 2026 4633                              Fedje
#> 263 2026 4634                         Masfjorden
#> 264 2026 4635                              Gulen
#> 265 2026 4636                             Solund
#> 266 2026 4637                          Hyllestad
#> 267 2026 4638                           Høyanger
#> 268 2026 4639                                Vik
#> 269 2026 4640                            Sogndal
#> 270 2026 4641                            Aurland
#> 271 2026 4642                             Lærdal
#> 272 2026 4643                              Årdal
#> 273 2026 4644                             Luster
#> 274 2026 4645                            Askvoll
#> 275 2026 4646                             Fjaler
#> 276 2026 4647                          Sunnfjord
#> 277 2026 4648                          Bremanger
#> 278 2026 4649                               Stad
#> 279 2026 4650                            Gloppen
#> 280 2026 4651                              Stryn
#> 281 2026 5001                Trondheim - Tråante
#> 282 2026 5006                          Steinkjer
#> 283 2026 5007         Namsos - Nåavmesjenjaelmie
#> 284 2026 5014                              Frøya
#> 285 2026 5020                               Osen
#> 286 2026 5021                             Oppdal
#> 287 2026 5022                            Rennebu
#> 288 2026 5025                      Røros - Rosse
#> 289 2026 5026                           Holtålen
#> 290 2026 5027                     Midtre Gauldal
#> 291 2026 5028                             Melhus
#> 292 2026 5029                              Skaun
#> 293 2026 5031                             Malvik
#> 294 2026 5032                              Selbu
#> 295 2026 5033                              Tydal
#> 296 2026 5034                            Meråker
#> 297 2026 5035                           Stjørdal
#> 298 2026 5036                             Frosta
#> 299 2026 5037                Levanger - Levangke
#> 300 2026 5038                             Verdal
#> 301 2026 5041                     Snåase - Snåsa
#> 302 2026 5042                             Lierne
#> 303 2026 5043                Raarvihke - Røyrvik
#> 304 2026 5044                         Namsskogan
#> 305 2026 5045                              Grong
#> 306 2026 5046                          Høylandet
#> 307 2026 5047                          Overhalla
#> 308 2026 5049                          Flatanger
#> 309 2026 5052                               Leka
#> 310 2026 5053                            Inderøy
#> 311 2026 5054                        Indre Fosen
#> 312 2026 5055                               Heim
#> 313 2026 5056                              Hitra
#> 314 2026 5057                             Ørland
#> 315 2026 5058                             Åfjord
#> 316 2026 5059                            Orkland
#> 317 2026 5060                          Nærøysund
#> 318 2026 5061                             Rindal
#> 319 2026 5501                             Tromsø
#> 320 2026 5503                 Harstad - Hárstták
#> 321 2026 5510                           Kvæfjord
#> 322 2026 5512          Dielddanuorri - Tjeldsund
#> 323 2026 5514                            Ibestad
#> 324 2026 5516                Gratangen - Rivtták
#> 325 2026 5518                  Loabák - Lavangen
#> 326 2026 5520                              Bardu
#> 327 2026 5522                           Salangen
#> 328 2026 5524                            Målselv
#> 329 2026 5526                           Sørreisa
#> 330 2026 5528                              Dyrøy
#> 331 2026 5530                              Senja
#> 332 2026 5532                          Balsfjord
#> 333 2026 5534                            Karlsøy
#> 334 2026 5536                             Lyngen
#> 335 2026 5538 Storfjord - Omasvuotna - Omasvuono
#> 336 2026 5540     Gáivuotna - Kåfjord - Kaivuono
#> 337 2026 5542                           Skjervøy
#> 338 2026 5544          Nordreisa - Ráisa - Raisi
#> 339 2026 5546                          Kvænangen
#> 340 2026 5601                               Alta
#> 341 2026 5603          Hammerfest - Hámmerfeasta
#> 342 2026 5605                       Sør-Varanger
#> 343 2026 5607                              Vadsø
#> 344 2026 5610              Kárá?johka - Karasjok
#> 345 2026 5612         Guovdageaidnu - Kautokeino
#> 346 2026 5614                              Loppa
#> 347 2026 5616                             Hasvik
#> 348 2026 5618                              Måsøy
#> 349 2026 5620                           Nordkapp
#> 350 2026 5622    Porsanger - Porsángu - Porsanki
#> 351 2026 5624                            Lebesby
#> 352 2026 5626                             Gamvik
#> 353 2026 5628                      Deatnu - Tana
#> 354 2026 5630                           Berlevåg
#> 355 2026 5632                          Båtsfjord
#> 356 2026 5634                              Vardø
#> 357 2026 5636                  Unjárga - Nesseby
#> 358 2026 9999                           Uoppgitt

# Get county codes
get_municipalities()
#>     year code                               name
#> 1   2026 0301                               Oslo
#> 2   2026 1101                          Eigersund
#> 3   2026 1103                          Stavanger
#> 4   2026 1106                          Haugesund
#> 5   2026 1108                            Sandnes
#> 6   2026 1111                            Sokndal
#> 7   2026 1112                               Lund
#> 8   2026 1114                          Bjerkreim
#> 9   2026 1119                                 Hå
#> 10  2026 1120                              Klepp
#> 11  2026 1121                               Time
#> 12  2026 1122                            Gjesdal
#> 13  2026 1124                               Sola
#> 14  2026 1127                          Randaberg
#> 15  2026 1130                             Strand
#> 16  2026 1133                         Hjelmeland
#> 17  2026 1134                             Suldal
#> 18  2026 1135                              Sauda
#> 19  2026 1144                            Kvitsøy
#> 20  2026 1145                               Bokn
#> 21  2026 1146                             Tysvær
#> 22  2026 1149                             Karmøy
#> 23  2026 1151                             Utsira
#> 24  2026 1160                         Vindafjord
#> 25  2026 1505                       Kristiansund
#> 26  2026 1506                              Molde
#> 27  2026 1508                            Ålesund
#> 28  2026 1511                           Vanylven
#> 29  2026 1514                              Sande
#> 30  2026 1515            Herøy (Møre og Romsdal)
#> 31  2026 1516                            Ulstein
#> 32  2026 1517                             Hareid
#> 33  2026 1520                              Ørsta
#> 34  2026 1525                            Stranda
#> 35  2026 1528                          Sykkylven
#> 36  2026 1531                               Sula
#> 37  2026 1532                              Giske
#> 38  2026 1535                            Vestnes
#> 39  2026 1539                              Rauma
#> 40  2026 1547                              Aukra
#> 41  2026 1554                             Averøy
#> 42  2026 1557                            Gjemnes
#> 43  2026 1560                           Tingvoll
#> 44  2026 1563                            Sunndal
#> 45  2026 1566                           Surnadal
#> 46  2026 1573                              Smøla
#> 47  2026 1576                               Aure
#> 48  2026 1577                              Volda
#> 49  2026 1578                              Fjord
#> 50  2026 1579                         Hustadvika
#> 51  2026 1580                              Haram
#> 52  2026 1804                               Bodø
#> 53  2026 1806                             Narvik
#> 54  2026 1811                             Bindal
#> 55  2026 1812                              Sømna
#> 56  2026 1813                            Brønnøy
#> 57  2026 1815                               Vega
#> 58  2026 1816                          Vevelstad
#> 59  2026 1818                   Herøy (Nordland)
#> 60  2026 1820                          Alstahaug
#> 61  2026 1822                          Leirfjord
#> 62  2026 1824                              Vefsn
#> 63  2026 1825                              Grane
#> 64  2026 1826            Aarborte - Hattfjelldal
#> 65  2026 1827                              Dønna
#> 66  2026 1828                              Nesna
#> 67  2026 1832                             Hemnes
#> 68  2026 1833                       Rana - Raane
#> 69  2026 1834                              Lurøy
#> 70  2026 1835                              Træna
#> 71  2026 1836                              Rødøy
#> 72  2026 1837                              Meløy
#> 73  2026 1838                          Gildeskål
#> 74  2026 1839                             Beiarn
#> 75  2026 1840                            Saltdal
#> 76  2026 1841                   Fauske - Fuossko
#> 77  2026 1845                  Sørfold - Fuolldá
#> 78  2026 1848                            Steigen
#> 79  2026 1851                           Lødingen
#> 80  2026 1853                  Evenes - Evená??i
#> 81  2026 1856                               Røst
#> 82  2026 1857                              Værøy
#> 83  2026 1859                           Flakstad
#> 84  2026 1860                          Vestvågøy
#> 85  2026 1865                              Vågan
#> 86  2026 1866                             Hadsel
#> 87  2026 1867                                 Bø
#> 88  2026 1868                             Øksnes
#> 89  2026 1870                  Sortland - Suortá
#> 90  2026 1871                              Andøy
#> 91  2026 1874                           Moskenes
#> 92  2026 1875                   Hábmer - Hamarøy
#> 93  2026 3101                             Halden
#> 94  2026 3103                               Moss
#> 95  2026 3105                          Sarpsborg
#> 96  2026 3107                        Fredrikstad
#> 97  2026 3110                             Hvaler
#> 98  2026 3112                               Råde
#> 99  2026 3114                    Våler (Østfold)
#> 100 2026 3116                           Skiptvet
#> 101 2026 3118                      Indre Østfold
#> 102 2026 3120                          Rakkestad
#> 103 2026 3122                             Marker
#> 104 2026 3124                            Aremark
#> 105 2026 3201                              Bærum
#> 106 2026 3203                              Asker
#> 107 2026 3205                         Lillestrøm
#> 108 2026 3207                       Nordre Follo
#> 109 2026 3209                         Ullensaker
#> 110 2026 3212                           Nesodden
#> 111 2026 3214                              Frogn
#> 112 2026 3216                             Vestby
#> 113 2026 3218                                 Ås
#> 114 2026 3220                            Enebakk
#> 115 2026 3222                          Lørenskog
#> 116 2026 3224                           Rælingen
#> 117 2026 3226                     Aurskog-Høland
#> 118 2026 3228                                Nes
#> 119 2026 3230                           Gjerdrum
#> 120 2026 3232                           Nittedal
#> 121 2026 3234                             Lunner
#> 122 2026 3236                           Jevnaker
#> 123 2026 3238                          Nannestad
#> 124 2026 3240                           Eidsvoll
#> 125 2026 3242                             Hurdal
#> 126 2026 3301                            Drammen
#> 127 2026 3303                          Kongsberg
#> 128 2026 3305                          Ringerike
#> 129 2026 3310                               Hole
#> 130 2026 3312                               Lier
#> 131 2026 3314                         Øvre Eiker
#> 132 2026 3316                              Modum
#> 133 2026 3318                         Krødsherad
#> 134 2026 3320                                Flå
#> 135 2026 3322                            Nesbyen
#> 136 2026 3324                                Gol
#> 137 2026 3326                           Hemsedal
#> 138 2026 3328                                 Ål
#> 139 2026 3330                                Hol
#> 140 2026 3332                             Sigdal
#> 141 2026 3334                           Flesberg
#> 142 2026 3336                             Rollag
#> 143 2026 3338                      Nore og Uvdal
#> 144 2026 3401                        Kongsvinger
#> 145 2026 3403                              Hamar
#> 146 2026 3405                        Lillehammer
#> 147 2026 3407                             Gjøvik
#> 148 2026 3411                          Ringsaker
#> 149 2026 3412                              Løten
#> 150 2026 3413                             Stange
#> 151 2026 3414                          Nord-Odal
#> 152 2026 3415                           Sør-Odal
#> 153 2026 3416                            Eidskog
#> 154 2026 3417                               Grue
#> 155 2026 3418                              Åsnes
#> 156 2026 3419                  Våler (Innlandet)
#> 157 2026 3420                            Elverum
#> 158 2026 3421                             Trysil
#> 159 2026 3422                               Åmot
#> 160 2026 3423                        Stor-Elvdal
#> 161 2026 3424                           Rendalen
#> 162 2026 3425                           Engerdal
#> 163 2026 3426                              Tolga
#> 164 2026 3427                             Tynset
#> 165 2026 3428                             Alvdal
#> 166 2026 3429                            Folldal
#> 167 2026 3430                                 Os
#> 168 2026 3431                              Dovre
#> 169 2026 3432                              Lesja
#> 170 2026 3433                              Skjåk
#> 171 2026 3434                                Lom
#> 172 2026 3435                               Vågå
#> 173 2026 3436                          Nord-Fron
#> 174 2026 3437                                Sel
#> 175 2026 3438                           Sør-Fron
#> 176 2026 3439                            Ringebu
#> 177 2026 3440                               Øyer
#> 178 2026 3441                            Gausdal
#> 179 2026 3442                        Østre Toten
#> 180 2026 3443                       Vestre Toten
#> 181 2026 3446                               Gran
#> 182 2026 3447                        Søndre Land
#> 183 2026 3448                        Nordre Land
#> 184 2026 3449                         Sør-Aurdal
#> 185 2026 3450                            Etnedal
#> 186 2026 3451                        Nord-Aurdal
#> 187 2026 3452                      Vestre Slidre
#> 188 2026 3453                      Øystre Slidre
#> 189 2026 3454                               Vang
#> 190 2026 3901                             Horten
#> 191 2026 3903                        Holmestrand
#> 192 2026 3905                           Tønsberg
#> 193 2026 3907                         Sandefjord
#> 194 2026 3909                             Larvik
#> 195 2026 3911                             Færder
#> 196 2026 4001                          Porsgrunn
#> 197 2026 4003                              Skien
#> 198 2026 4005                           Notodden
#> 199 2026 4010                             Siljan
#> 200 2026 4012                             Bamble
#> 201 2026 4014                            Kragerø
#> 202 2026 4016                          Drangedal
#> 203 2026 4018                               Nome
#> 204 2026 4020                      Midt-Telemark
#> 205 2026 4022                            Seljord
#> 206 2026 4024                           Hjartdal
#> 207 2026 4026                               Tinn
#> 208 2026 4028                          Kviteseid
#> 209 2026 4030                           Nissedal
#> 210 2026 4032                           Fyresdal
#> 211 2026 4034                              Tokke
#> 212 2026 4036                              Vinje
#> 213 2026 4201                              Risør
#> 214 2026 4202                           Grimstad
#> 215 2026 4203                            Arendal
#> 216 2026 4204                       Kristiansand
#> 217 2026 4205                          Lindesnes
#> 218 2026 4206                            Farsund
#> 219 2026 4207                        Flekkefjord
#> 220 2026 4211                           Gjerstad
#> 221 2026 4212                          Vegårshei
#> 222 2026 4213                        Tvedestrand
#> 223 2026 4214                            Froland
#> 224 2026 4215                          Lillesand
#> 225 2026 4216                           Birkenes
#> 226 2026 4217                               Åmli
#> 227 2026 4218                            Iveland
#> 228 2026 4219                    Evje og Hornnes
#> 229 2026 4220                            Bygland
#> 230 2026 4221                              Valle
#> 231 2026 4222                              Bykle
#> 232 2026 4223                           Vennesla
#> 233 2026 4224                             Åseral
#> 234 2026 4225                            Lyngdal
#> 235 2026 4226                         Hægebostad
#> 236 2026 4227                          Kvinesdal
#> 237 2026 4228                             Sirdal
#> 238 2026 4601                             Bergen
#> 239 2026 4602                               Kinn
#> 240 2026 4611                               Etne
#> 241 2026 4612                              Sveio
#> 242 2026 4613                              Bømlo
#> 243 2026 4614                              Stord
#> 244 2026 4615                             Fitjar
#> 245 2026 4616                             Tysnes
#> 246 2026 4617                         Kvinnherad
#> 247 2026 4618                         Ullensvang
#> 248 2026 4619                           Eidfjord
#> 249 2026 4620                              Ulvik
#> 250 2026 4621                               Voss
#> 251 2026 4622                               Kvam
#> 252 2026 4623                          Samnanger
#> 253 2026 4624                      Bjørnafjorden
#> 254 2026 4625                          Austevoll
#> 255 2026 4626                           Øygarden
#> 256 2026 4627                              Askøy
#> 257 2026 4628                            Vaksdal
#> 258 2026 4629                            Modalen
#> 259 2026 4630                            Osterøy
#> 260 2026 4631                              Alver
#> 261 2026 4632                          Austrheim
#> 262 2026 4633                              Fedje
#> 263 2026 4634                         Masfjorden
#> 264 2026 4635                              Gulen
#> 265 2026 4636                             Solund
#> 266 2026 4637                          Hyllestad
#> 267 2026 4638                           Høyanger
#> 268 2026 4639                                Vik
#> 269 2026 4640                            Sogndal
#> 270 2026 4641                            Aurland
#> 271 2026 4642                             Lærdal
#> 272 2026 4643                              Årdal
#> 273 2026 4644                             Luster
#> 274 2026 4645                            Askvoll
#> 275 2026 4646                             Fjaler
#> 276 2026 4647                          Sunnfjord
#> 277 2026 4648                          Bremanger
#> 278 2026 4649                               Stad
#> 279 2026 4650                            Gloppen
#> 280 2026 4651                              Stryn
#> 281 2026 5001                Trondheim - Tråante
#> 282 2026 5006                          Steinkjer
#> 283 2026 5007         Namsos - Nåavmesjenjaelmie
#> 284 2026 5014                              Frøya
#> 285 2026 5020                               Osen
#> 286 2026 5021                             Oppdal
#> 287 2026 5022                            Rennebu
#> 288 2026 5025                      Røros - Rosse
#> 289 2026 5026                           Holtålen
#> 290 2026 5027                     Midtre Gauldal
#> 291 2026 5028                             Melhus
#> 292 2026 5029                              Skaun
#> 293 2026 5031                             Malvik
#> 294 2026 5032                              Selbu
#> 295 2026 5033                              Tydal
#> 296 2026 5034                            Meråker
#> 297 2026 5035                           Stjørdal
#> 298 2026 5036                             Frosta
#> 299 2026 5037                Levanger - Levangke
#> 300 2026 5038                             Verdal
#> 301 2026 5041                     Snåase - Snåsa
#> 302 2026 5042                             Lierne
#> 303 2026 5043                Raarvihke - Røyrvik
#> 304 2026 5044                         Namsskogan
#> 305 2026 5045                              Grong
#> 306 2026 5046                          Høylandet
#> 307 2026 5047                          Overhalla
#> 308 2026 5049                          Flatanger
#> 309 2026 5052                               Leka
#> 310 2026 5053                            Inderøy
#> 311 2026 5054                        Indre Fosen
#> 312 2026 5055                               Heim
#> 313 2026 5056                              Hitra
#> 314 2026 5057                             Ørland
#> 315 2026 5058                             Åfjord
#> 316 2026 5059                            Orkland
#> 317 2026 5060                          Nærøysund
#> 318 2026 5061                             Rindal
#> 319 2026 5501                             Tromsø
#> 320 2026 5503                 Harstad - Hárstták
#> 321 2026 5510                           Kvæfjord
#> 322 2026 5512          Dielddanuorri - Tjeldsund
#> 323 2026 5514                            Ibestad
#> 324 2026 5516                Gratangen - Rivtták
#> 325 2026 5518                  Loabák - Lavangen
#> 326 2026 5520                              Bardu
#> 327 2026 5522                           Salangen
#> 328 2026 5524                            Målselv
#> 329 2026 5526                           Sørreisa
#> 330 2026 5528                              Dyrøy
#> 331 2026 5530                              Senja
#> 332 2026 5532                          Balsfjord
#> 333 2026 5534                            Karlsøy
#> 334 2026 5536                             Lyngen
#> 335 2026 5538 Storfjord - Omasvuotna - Omasvuono
#> 336 2026 5540     Gáivuotna - Kåfjord - Kaivuono
#> 337 2026 5542                           Skjervøy
#> 338 2026 5544          Nordreisa - Ráisa - Raisi
#> 339 2026 5546                          Kvænangen
#> 340 2026 5601                               Alta
#> 341 2026 5603          Hammerfest - Hámmerfeasta
#> 342 2026 5605                       Sør-Varanger
#> 343 2026 5607                              Vadsø
#> 344 2026 5610              Kárá?johka - Karasjok
#> 345 2026 5612         Guovdageaidnu - Kautokeino
#> 346 2026 5614                              Loppa
#> 347 2026 5616                             Hasvik
#> 348 2026 5618                              Måsøy
#> 349 2026 5620                           Nordkapp
#> 350 2026 5622    Porsanger - Porsángu - Porsanki
#> 351 2026 5624                            Lebesby
#> 352 2026 5626                             Gamvik
#> 353 2026 5628                      Deatnu - Tana
#> 354 2026 5630                           Berlevåg
#> 355 2026 5632                          Båtsfjord
#> 356 2026 5634                              Vardø
#> 357 2026 5636                  Unjárga - Nesseby
#> 358 2026 9999                           Uoppgitt

# Get both
get_adm_units()
#>     year code                               name county
#> 1   2026 0301                               Oslo     03
#> 2   2026 1101                          Eigersund     11
#> 3   2026 1103                          Stavanger     11
#> 4   2026 1106                          Haugesund     11
#> 5   2026 1108                            Sandnes     11
#> 6   2026 1111                            Sokndal     11
#> 7   2026 1112                               Lund     11
#> 8   2026 1114                          Bjerkreim     11
#> 9   2026 1119                                 Hå     11
#> 10  2026 1120                              Klepp     11
#> 11  2026 1121                               Time     11
#> 12  2026 1122                            Gjesdal     11
#> 13  2026 1124                               Sola     11
#> 14  2026 1127                          Randaberg     11
#> 15  2026 1130                             Strand     11
#> 16  2026 1133                         Hjelmeland     11
#> 17  2026 1134                             Suldal     11
#> 18  2026 1135                              Sauda     11
#> 19  2026 1144                            Kvitsøy     11
#> 20  2026 1145                               Bokn     11
#> 21  2026 1146                             Tysvær     11
#> 22  2026 1149                             Karmøy     11
#> 23  2026 1151                             Utsira     11
#> 24  2026 1160                         Vindafjord     11
#> 25  2026 1505                       Kristiansund     15
#> 26  2026 1506                              Molde     15
#> 27  2026 1508                            Ålesund     15
#> 28  2026 1511                           Vanylven     15
#> 29  2026 1514                              Sande     15
#> 30  2026 1515            Herøy (Møre og Romsdal)     15
#> 31  2026 1516                            Ulstein     15
#> 32  2026 1517                             Hareid     15
#> 33  2026 1520                              Ørsta     15
#> 34  2026 1525                            Stranda     15
#> 35  2026 1528                          Sykkylven     15
#> 36  2026 1531                               Sula     15
#> 37  2026 1532                              Giske     15
#> 38  2026 1535                            Vestnes     15
#> 39  2026 1539                              Rauma     15
#> 40  2026 1547                              Aukra     15
#> 41  2026 1554                             Averøy     15
#> 42  2026 1557                            Gjemnes     15
#> 43  2026 1560                           Tingvoll     15
#> 44  2026 1563                            Sunndal     15
#> 45  2026 1566                           Surnadal     15
#> 46  2026 1573                              Smøla     15
#> 47  2026 1576                               Aure     15
#> 48  2026 1577                              Volda     15
#> 49  2026 1578                              Fjord     15
#> 50  2026 1579                         Hustadvika     15
#> 51  2026 1580                              Haram     15
#> 52  2026 1804                               Bodø     18
#> 53  2026 1806                             Narvik     18
#> 54  2026 1811                             Bindal     18
#> 55  2026 1812                              Sømna     18
#> 56  2026 1813                            Brønnøy     18
#> 57  2026 1815                               Vega     18
#> 58  2026 1816                          Vevelstad     18
#> 59  2026 1818                   Herøy (Nordland)     18
#> 60  2026 1820                          Alstahaug     18
#> 61  2026 1822                          Leirfjord     18
#> 62  2026 1824                              Vefsn     18
#> 63  2026 1825                              Grane     18
#> 64  2026 1826            Aarborte - Hattfjelldal     18
#> 65  2026 1827                              Dønna     18
#> 66  2026 1828                              Nesna     18
#> 67  2026 1832                             Hemnes     18
#> 68  2026 1833                       Rana - Raane     18
#> 69  2026 1834                              Lurøy     18
#> 70  2026 1835                              Træna     18
#> 71  2026 1836                              Rødøy     18
#> 72  2026 1837                              Meløy     18
#> 73  2026 1838                          Gildeskål     18
#> 74  2026 1839                             Beiarn     18
#> 75  2026 1840                            Saltdal     18
#> 76  2026 1841                   Fauske - Fuossko     18
#> 77  2026 1845                  Sørfold - Fuolldá     18
#> 78  2026 1848                            Steigen     18
#> 79  2026 1851                           Lødingen     18
#> 80  2026 1853                  Evenes - Evená??i     18
#> 81  2026 1856                               Røst     18
#> 82  2026 1857                              Værøy     18
#> 83  2026 1859                           Flakstad     18
#> 84  2026 1860                          Vestvågøy     18
#> 85  2026 1865                              Vågan     18
#> 86  2026 1866                             Hadsel     18
#> 87  2026 1867                                 Bø     18
#> 88  2026 1868                             Øksnes     18
#> 89  2026 1870                  Sortland - Suortá     18
#> 90  2026 1871                              Andøy     18
#> 91  2026 1874                           Moskenes     18
#> 92  2026 1875                   Hábmer - Hamarøy     18
#> 93  2026 3101                             Halden     31
#> 94  2026 3103                               Moss     31
#> 95  2026 3105                          Sarpsborg     31
#> 96  2026 3107                        Fredrikstad     31
#> 97  2026 3110                             Hvaler     31
#> 98  2026 3112                               Råde     31
#> 99  2026 3114                    Våler (Østfold)     31
#> 100 2026 3116                           Skiptvet     31
#> 101 2026 3118                      Indre Østfold     31
#> 102 2026 3120                          Rakkestad     31
#> 103 2026 3122                             Marker     31
#> 104 2026 3124                            Aremark     31
#> 105 2026 3201                              Bærum     32
#> 106 2026 3203                              Asker     32
#> 107 2026 3205                         Lillestrøm     32
#> 108 2026 3207                       Nordre Follo     32
#> 109 2026 3209                         Ullensaker     32
#> 110 2026 3212                           Nesodden     32
#> 111 2026 3214                              Frogn     32
#> 112 2026 3216                             Vestby     32
#> 113 2026 3218                                 Ås     32
#> 114 2026 3220                            Enebakk     32
#> 115 2026 3222                          Lørenskog     32
#> 116 2026 3224                           Rælingen     32
#> 117 2026 3226                     Aurskog-Høland     32
#> 118 2026 3228                                Nes     32
#> 119 2026 3230                           Gjerdrum     32
#> 120 2026 3232                           Nittedal     32
#> 121 2026 3234                             Lunner     32
#> 122 2026 3236                           Jevnaker     32
#> 123 2026 3238                          Nannestad     32
#> 124 2026 3240                           Eidsvoll     32
#> 125 2026 3242                             Hurdal     32
#> 126 2026 3301                            Drammen     33
#> 127 2026 3303                          Kongsberg     33
#> 128 2026 3305                          Ringerike     33
#> 129 2026 3310                               Hole     33
#> 130 2026 3312                               Lier     33
#> 131 2026 3314                         Øvre Eiker     33
#> 132 2026 3316                              Modum     33
#> 133 2026 3318                         Krødsherad     33
#> 134 2026 3320                                Flå     33
#> 135 2026 3322                            Nesbyen     33
#> 136 2026 3324                                Gol     33
#> 137 2026 3326                           Hemsedal     33
#> 138 2026 3328                                 Ål     33
#> 139 2026 3330                                Hol     33
#> 140 2026 3332                             Sigdal     33
#> 141 2026 3334                           Flesberg     33
#> 142 2026 3336                             Rollag     33
#> 143 2026 3338                      Nore og Uvdal     33
#> 144 2026 3401                        Kongsvinger     34
#> 145 2026 3403                              Hamar     34
#> 146 2026 3405                        Lillehammer     34
#> 147 2026 3407                             Gjøvik     34
#> 148 2026 3411                          Ringsaker     34
#> 149 2026 3412                              Løten     34
#> 150 2026 3413                             Stange     34
#> 151 2026 3414                          Nord-Odal     34
#> 152 2026 3415                           Sør-Odal     34
#> 153 2026 3416                            Eidskog     34
#> 154 2026 3417                               Grue     34
#> 155 2026 3418                              Åsnes     34
#> 156 2026 3419                  Våler (Innlandet)     34
#> 157 2026 3420                            Elverum     34
#> 158 2026 3421                             Trysil     34
#> 159 2026 3422                               Åmot     34
#> 160 2026 3423                        Stor-Elvdal     34
#> 161 2026 3424                           Rendalen     34
#> 162 2026 3425                           Engerdal     34
#> 163 2026 3426                              Tolga     34
#> 164 2026 3427                             Tynset     34
#> 165 2026 3428                             Alvdal     34
#> 166 2026 3429                            Folldal     34
#> 167 2026 3430                                 Os     34
#> 168 2026 3431                              Dovre     34
#> 169 2026 3432                              Lesja     34
#> 170 2026 3433                              Skjåk     34
#> 171 2026 3434                                Lom     34
#> 172 2026 3435                               Vågå     34
#> 173 2026 3436                          Nord-Fron     34
#> 174 2026 3437                                Sel     34
#> 175 2026 3438                           Sør-Fron     34
#> 176 2026 3439                            Ringebu     34
#> 177 2026 3440                               Øyer     34
#> 178 2026 3441                            Gausdal     34
#> 179 2026 3442                        Østre Toten     34
#> 180 2026 3443                       Vestre Toten     34
#> 181 2026 3446                               Gran     34
#> 182 2026 3447                        Søndre Land     34
#> 183 2026 3448                        Nordre Land     34
#> 184 2026 3449                         Sør-Aurdal     34
#> 185 2026 3450                            Etnedal     34
#> 186 2026 3451                        Nord-Aurdal     34
#> 187 2026 3452                      Vestre Slidre     34
#> 188 2026 3453                      Øystre Slidre     34
#> 189 2026 3454                               Vang     34
#> 190 2026 3901                             Horten     39
#> 191 2026 3903                        Holmestrand     39
#> 192 2026 3905                           Tønsberg     39
#> 193 2026 3907                         Sandefjord     39
#> 194 2026 3909                             Larvik     39
#> 195 2026 3911                             Færder     39
#> 196 2026 4001                          Porsgrunn     40
#> 197 2026 4003                              Skien     40
#> 198 2026 4005                           Notodden     40
#> 199 2026 4010                             Siljan     40
#> 200 2026 4012                             Bamble     40
#> 201 2026 4014                            Kragerø     40
#> 202 2026 4016                          Drangedal     40
#> 203 2026 4018                               Nome     40
#> 204 2026 4020                      Midt-Telemark     40
#> 205 2026 4022                            Seljord     40
#> 206 2026 4024                           Hjartdal     40
#> 207 2026 4026                               Tinn     40
#> 208 2026 4028                          Kviteseid     40
#> 209 2026 4030                           Nissedal     40
#> 210 2026 4032                           Fyresdal     40
#> 211 2026 4034                              Tokke     40
#> 212 2026 4036                              Vinje     40
#> 213 2026 4201                              Risør     42
#> 214 2026 4202                           Grimstad     42
#> 215 2026 4203                            Arendal     42
#> 216 2026 4204                       Kristiansand     42
#> 217 2026 4205                          Lindesnes     42
#> 218 2026 4206                            Farsund     42
#> 219 2026 4207                        Flekkefjord     42
#> 220 2026 4211                           Gjerstad     42
#> 221 2026 4212                          Vegårshei     42
#> 222 2026 4213                        Tvedestrand     42
#> 223 2026 4214                            Froland     42
#> 224 2026 4215                          Lillesand     42
#> 225 2026 4216                           Birkenes     42
#> 226 2026 4217                               Åmli     42
#> 227 2026 4218                            Iveland     42
#> 228 2026 4219                    Evje og Hornnes     42
#> 229 2026 4220                            Bygland     42
#> 230 2026 4221                              Valle     42
#> 231 2026 4222                              Bykle     42
#> 232 2026 4223                           Vennesla     42
#> 233 2026 4224                             Åseral     42
#> 234 2026 4225                            Lyngdal     42
#> 235 2026 4226                         Hægebostad     42
#> 236 2026 4227                          Kvinesdal     42
#> 237 2026 4228                             Sirdal     42
#> 238 2026 4601                             Bergen     46
#> 239 2026 4602                               Kinn     46
#> 240 2026 4611                               Etne     46
#> 241 2026 4612                              Sveio     46
#> 242 2026 4613                              Bømlo     46
#> 243 2026 4614                              Stord     46
#> 244 2026 4615                             Fitjar     46
#> 245 2026 4616                             Tysnes     46
#> 246 2026 4617                         Kvinnherad     46
#> 247 2026 4618                         Ullensvang     46
#> 248 2026 4619                           Eidfjord     46
#> 249 2026 4620                              Ulvik     46
#> 250 2026 4621                               Voss     46
#> 251 2026 4622                               Kvam     46
#> 252 2026 4623                          Samnanger     46
#> 253 2026 4624                      Bjørnafjorden     46
#> 254 2026 4625                          Austevoll     46
#> 255 2026 4626                           Øygarden     46
#> 256 2026 4627                              Askøy     46
#> 257 2026 4628                            Vaksdal     46
#> 258 2026 4629                            Modalen     46
#> 259 2026 4630                            Osterøy     46
#> 260 2026 4631                              Alver     46
#> 261 2026 4632                          Austrheim     46
#> 262 2026 4633                              Fedje     46
#> 263 2026 4634                         Masfjorden     46
#> 264 2026 4635                              Gulen     46
#> 265 2026 4636                             Solund     46
#> 266 2026 4637                          Hyllestad     46
#> 267 2026 4638                           Høyanger     46
#> 268 2026 4639                                Vik     46
#> 269 2026 4640                            Sogndal     46
#> 270 2026 4641                            Aurland     46
#> 271 2026 4642                             Lærdal     46
#> 272 2026 4643                              Årdal     46
#> 273 2026 4644                             Luster     46
#> 274 2026 4645                            Askvoll     46
#> 275 2026 4646                             Fjaler     46
#> 276 2026 4647                          Sunnfjord     46
#> 277 2026 4648                          Bremanger     46
#> 278 2026 4649                               Stad     46
#> 279 2026 4650                            Gloppen     46
#> 280 2026 4651                              Stryn     46
#> 281 2026 5001                Trondheim - Tråante     50
#> 282 2026 5006                          Steinkjer     50
#> 283 2026 5007         Namsos - Nåavmesjenjaelmie     50
#> 284 2026 5014                              Frøya     50
#> 285 2026 5020                               Osen     50
#> 286 2026 5021                             Oppdal     50
#> 287 2026 5022                            Rennebu     50
#> 288 2026 5025                      Røros - Rosse     50
#> 289 2026 5026                           Holtålen     50
#> 290 2026 5027                     Midtre Gauldal     50
#> 291 2026 5028                             Melhus     50
#> 292 2026 5029                              Skaun     50
#> 293 2026 5031                             Malvik     50
#> 294 2026 5032                              Selbu     50
#> 295 2026 5033                              Tydal     50
#> 296 2026 5034                            Meråker     50
#> 297 2026 5035                           Stjørdal     50
#> 298 2026 5036                             Frosta     50
#> 299 2026 5037                Levanger - Levangke     50
#> 300 2026 5038                             Verdal     50
#> 301 2026 5041                     Snåase - Snåsa     50
#> 302 2026 5042                             Lierne     50
#> 303 2026 5043                Raarvihke - Røyrvik     50
#> 304 2026 5044                         Namsskogan     50
#> 305 2026 5045                              Grong     50
#> 306 2026 5046                          Høylandet     50
#> 307 2026 5047                          Overhalla     50
#> 308 2026 5049                          Flatanger     50
#> 309 2026 5052                               Leka     50
#> 310 2026 5053                            Inderøy     50
#> 311 2026 5054                        Indre Fosen     50
#> 312 2026 5055                               Heim     50
#> 313 2026 5056                              Hitra     50
#> 314 2026 5057                             Ørland     50
#> 315 2026 5058                             Åfjord     50
#> 316 2026 5059                            Orkland     50
#> 317 2026 5060                          Nærøysund     50
#> 318 2026 5061                             Rindal     50
#> 319 2026 5501                             Tromsø     55
#> 320 2026 5503                 Harstad - Hárstták     55
#> 321 2026 5510                           Kvæfjord     55
#> 322 2026 5512          Dielddanuorri - Tjeldsund     55
#> 323 2026 5514                            Ibestad     55
#> 324 2026 5516                Gratangen - Rivtták     55
#> 325 2026 5518                  Loabák - Lavangen     55
#> 326 2026 5520                              Bardu     55
#> 327 2026 5522                           Salangen     55
#> 328 2026 5524                            Målselv     55
#> 329 2026 5526                           Sørreisa     55
#> 330 2026 5528                              Dyrøy     55
#> 331 2026 5530                              Senja     55
#> 332 2026 5532                          Balsfjord     55
#> 333 2026 5534                            Karlsøy     55
#> 334 2026 5536                             Lyngen     55
#> 335 2026 5538 Storfjord - Omasvuotna - Omasvuono     55
#> 336 2026 5540     Gáivuotna - Kåfjord - Kaivuono     55
#> 337 2026 5542                           Skjervøy     55
#> 338 2026 5544          Nordreisa - Ráisa - Raisi     55
#> 339 2026 5546                          Kvænangen     55
#> 340 2026 5601                               Alta     56
#> 341 2026 5603          Hammerfest - Hámmerfeasta     56
#> 342 2026 5605                       Sør-Varanger     56
#> 343 2026 5607                              Vadsø     56
#> 344 2026 5610              Kárá?johka - Karasjok     56
#> 345 2026 5612         Guovdageaidnu - Kautokeino     56
#> 346 2026 5614                              Loppa     56
#> 347 2026 5616                             Hasvik     56
#> 348 2026 5618                              Måsøy     56
#> 349 2026 5620                           Nordkapp     56
#> 350 2026 5622    Porsanger - Porsángu - Porsanki     56
#> 351 2026 5624                            Lebesby     56
#> 352 2026 5626                             Gamvik     56
#> 353 2026 5628                      Deatnu - Tana     56
#> 354 2026 5630                           Berlevåg     56
#> 355 2026 5632                          Båtsfjord     56
#> 356 2026 5634                              Vardø     56
#> 357 2026 5636                  Unjárga - Nesseby     56
#> 358 2026 9999                           Uoppgitt     99
#>                          county_name
#> 1                               Oslo
#> 2                           Rogaland
#> 3                           Rogaland
#> 4                           Rogaland
#> 5                           Rogaland
#> 6                           Rogaland
#> 7                           Rogaland
#> 8                           Rogaland
#> 9                           Rogaland
#> 10                          Rogaland
#> 11                          Rogaland
#> 12                          Rogaland
#> 13                          Rogaland
#> 14                          Rogaland
#> 15                          Rogaland
#> 16                          Rogaland
#> 17                          Rogaland
#> 18                          Rogaland
#> 19                          Rogaland
#> 20                          Rogaland
#> 21                          Rogaland
#> 22                          Rogaland
#> 23                          Rogaland
#> 24                          Rogaland
#> 25                   Møre og Romsdal
#> 26                   Møre og Romsdal
#> 27                   Møre og Romsdal
#> 28                   Møre og Romsdal
#> 29                   Møre og Romsdal
#> 30                   Møre og Romsdal
#> 31                   Møre og Romsdal
#> 32                   Møre og Romsdal
#> 33                   Møre og Romsdal
#> 34                   Møre og Romsdal
#> 35                   Møre og Romsdal
#> 36                   Møre og Romsdal
#> 37                   Møre og Romsdal
#> 38                   Møre og Romsdal
#> 39                   Møre og Romsdal
#> 40                   Møre og Romsdal
#> 41                   Møre og Romsdal
#> 42                   Møre og Romsdal
#> 43                   Møre og Romsdal
#> 44                   Møre og Romsdal
#> 45                   Møre og Romsdal
#> 46                   Møre og Romsdal
#> 47                   Møre og Romsdal
#> 48                   Møre og Romsdal
#> 49                   Møre og Romsdal
#> 50                   Møre og Romsdal
#> 51                   Møre og Romsdal
#> 52             Nordland - Nordlánnda
#> 53             Nordland - Nordlánnda
#> 54             Nordland - Nordlánnda
#> 55             Nordland - Nordlánnda
#> 56             Nordland - Nordlánnda
#> 57             Nordland - Nordlánnda
#> 58             Nordland - Nordlánnda
#> 59             Nordland - Nordlánnda
#> 60             Nordland - Nordlánnda
#> 61             Nordland - Nordlánnda
#> 62             Nordland - Nordlánnda
#> 63             Nordland - Nordlánnda
#> 64             Nordland - Nordlánnda
#> 65             Nordland - Nordlánnda
#> 66             Nordland - Nordlánnda
#> 67             Nordland - Nordlánnda
#> 68             Nordland - Nordlánnda
#> 69             Nordland - Nordlánnda
#> 70             Nordland - Nordlánnda
#> 71             Nordland - Nordlánnda
#> 72             Nordland - Nordlánnda
#> 73             Nordland - Nordlánnda
#> 74             Nordland - Nordlánnda
#> 75             Nordland - Nordlánnda
#> 76             Nordland - Nordlánnda
#> 77             Nordland - Nordlánnda
#> 78             Nordland - Nordlánnda
#> 79             Nordland - Nordlánnda
#> 80             Nordland - Nordlánnda
#> 81             Nordland - Nordlánnda
#> 82             Nordland - Nordlánnda
#> 83             Nordland - Nordlánnda
#> 84             Nordland - Nordlánnda
#> 85             Nordland - Nordlánnda
#> 86             Nordland - Nordlánnda
#> 87             Nordland - Nordlánnda
#> 88             Nordland - Nordlánnda
#> 89             Nordland - Nordlánnda
#> 90             Nordland - Nordlánnda
#> 91             Nordland - Nordlánnda
#> 92             Nordland - Nordlánnda
#> 93                           Østfold
#> 94                           Østfold
#> 95                           Østfold
#> 96                           Østfold
#> 97                           Østfold
#> 98                           Østfold
#> 99                           Østfold
#> 100                          Østfold
#> 101                          Østfold
#> 102                          Østfold
#> 103                          Østfold
#> 104                          Østfold
#> 105                         Akershus
#> 106                         Akershus
#> 107                         Akershus
#> 108                         Akershus
#> 109                         Akershus
#> 110                         Akershus
#> 111                         Akershus
#> 112                         Akershus
#> 113                         Akershus
#> 114                         Akershus
#> 115                         Akershus
#> 116                         Akershus
#> 117                         Akershus
#> 118                         Akershus
#> 119                         Akershus
#> 120                         Akershus
#> 121                         Akershus
#> 122                         Akershus
#> 123                         Akershus
#> 124                         Akershus
#> 125                         Akershus
#> 126                         Buskerud
#> 127                         Buskerud
#> 128                         Buskerud
#> 129                         Buskerud
#> 130                         Buskerud
#> 131                         Buskerud
#> 132                         Buskerud
#> 133                         Buskerud
#> 134                         Buskerud
#> 135                         Buskerud
#> 136                         Buskerud
#> 137                         Buskerud
#> 138                         Buskerud
#> 139                         Buskerud
#> 140                         Buskerud
#> 141                         Buskerud
#> 142                         Buskerud
#> 143                         Buskerud
#> 144                        Innlandet
#> 145                        Innlandet
#> 146                        Innlandet
#> 147                        Innlandet
#> 148                        Innlandet
#> 149                        Innlandet
#> 150                        Innlandet
#> 151                        Innlandet
#> 152                        Innlandet
#> 153                        Innlandet
#> 154                        Innlandet
#> 155                        Innlandet
#> 156                        Innlandet
#> 157                        Innlandet
#> 158                        Innlandet
#> 159                        Innlandet
#> 160                        Innlandet
#> 161                        Innlandet
#> 162                        Innlandet
#> 163                        Innlandet
#> 164                        Innlandet
#> 165                        Innlandet
#> 166                        Innlandet
#> 167                        Innlandet
#> 168                        Innlandet
#> 169                        Innlandet
#> 170                        Innlandet
#> 171                        Innlandet
#> 172                        Innlandet
#> 173                        Innlandet
#> 174                        Innlandet
#> 175                        Innlandet
#> 176                        Innlandet
#> 177                        Innlandet
#> 178                        Innlandet
#> 179                        Innlandet
#> 180                        Innlandet
#> 181                        Innlandet
#> 182                        Innlandet
#> 183                        Innlandet
#> 184                        Innlandet
#> 185                        Innlandet
#> 186                        Innlandet
#> 187                        Innlandet
#> 188                        Innlandet
#> 189                        Innlandet
#> 190                         Vestfold
#> 191                         Vestfold
#> 192                         Vestfold
#> 193                         Vestfold
#> 194                         Vestfold
#> 195                         Vestfold
#> 196                         Telemark
#> 197                         Telemark
#> 198                         Telemark
#> 199                         Telemark
#> 200                         Telemark
#> 201                         Telemark
#> 202                         Telemark
#> 203                         Telemark
#> 204                         Telemark
#> 205                         Telemark
#> 206                         Telemark
#> 207                         Telemark
#> 208                         Telemark
#> 209                         Telemark
#> 210                         Telemark
#> 211                         Telemark
#> 212                         Telemark
#> 213                            Agder
#> 214                            Agder
#> 215                            Agder
#> 216                            Agder
#> 217                            Agder
#> 218                            Agder
#> 219                            Agder
#> 220                            Agder
#> 221                            Agder
#> 222                            Agder
#> 223                            Agder
#> 224                            Agder
#> 225                            Agder
#> 226                            Agder
#> 227                            Agder
#> 228                            Agder
#> 229                            Agder
#> 230                            Agder
#> 231                            Agder
#> 232                            Agder
#> 233                            Agder
#> 234                            Agder
#> 235                            Agder
#> 236                            Agder
#> 237                            Agder
#> 238                         Vestland
#> 239                         Vestland
#> 240                         Vestland
#> 241                         Vestland
#> 242                         Vestland
#> 243                         Vestland
#> 244                         Vestland
#> 245                         Vestland
#> 246                         Vestland
#> 247                         Vestland
#> 248                         Vestland
#> 249                         Vestland
#> 250                         Vestland
#> 251                         Vestland
#> 252                         Vestland
#> 253                         Vestland
#> 254                         Vestland
#> 255                         Vestland
#> 256                         Vestland
#> 257                         Vestland
#> 258                         Vestland
#> 259                         Vestland
#> 260                         Vestland
#> 261                         Vestland
#> 262                         Vestland
#> 263                         Vestland
#> 264                         Vestland
#> 265                         Vestland
#> 266                         Vestland
#> 267                         Vestland
#> 268                         Vestland
#> 269                         Vestland
#> 270                         Vestland
#> 271                         Vestland
#> 272                         Vestland
#> 273                         Vestland
#> 274                         Vestland
#> 275                         Vestland
#> 276                         Vestland
#> 277                         Vestland
#> 278                         Vestland
#> 279                         Vestland
#> 280                         Vestland
#> 281          Trøndelag - Trööndelage
#> 282          Trøndelag - Trööndelage
#> 283          Trøndelag - Trööndelage
#> 284          Trøndelag - Trööndelage
#> 285          Trøndelag - Trööndelage
#> 286          Trøndelag - Trööndelage
#> 287          Trøndelag - Trööndelage
#> 288          Trøndelag - Trööndelage
#> 289          Trøndelag - Trööndelage
#> 290          Trøndelag - Trööndelage
#> 291          Trøndelag - Trööndelage
#> 292          Trøndelag - Trööndelage
#> 293          Trøndelag - Trööndelage
#> 294          Trøndelag - Trööndelage
#> 295          Trøndelag - Trööndelage
#> 296          Trøndelag - Trööndelage
#> 297          Trøndelag - Trööndelage
#> 298          Trøndelag - Trööndelage
#> 299          Trøndelag - Trööndelage
#> 300          Trøndelag - Trööndelage
#> 301          Trøndelag - Trööndelage
#> 302          Trøndelag - Trööndelage
#> 303          Trøndelag - Trööndelage
#> 304          Trøndelag - Trööndelage
#> 305          Trøndelag - Trööndelage
#> 306          Trøndelag - Trööndelage
#> 307          Trøndelag - Trööndelage
#> 308          Trøndelag - Trööndelage
#> 309          Trøndelag - Trööndelage
#> 310          Trøndelag - Trööndelage
#> 311          Trøndelag - Trööndelage
#> 312          Trøndelag - Trööndelage
#> 313          Trøndelag - Trööndelage
#> 314          Trøndelag - Trööndelage
#> 315          Trøndelag - Trööndelage
#> 316          Trøndelag - Trööndelage
#> 317          Trøndelag - Trööndelage
#> 318          Trøndelag - Trööndelage
#> 319          Troms - Romsa - Tromssa
#> 320          Troms - Romsa - Tromssa
#> 321          Troms - Romsa - Tromssa
#> 322          Troms - Romsa - Tromssa
#> 323          Troms - Romsa - Tromssa
#> 324          Troms - Romsa - Tromssa
#> 325          Troms - Romsa - Tromssa
#> 326          Troms - Romsa - Tromssa
#> 327          Troms - Romsa - Tromssa
#> 328          Troms - Romsa - Tromssa
#> 329          Troms - Romsa - Tromssa
#> 330          Troms - Romsa - Tromssa
#> 331          Troms - Romsa - Tromssa
#> 332          Troms - Romsa - Tromssa
#> 333          Troms - Romsa - Tromssa
#> 334          Troms - Romsa - Tromssa
#> 335          Troms - Romsa - Tromssa
#> 336          Troms - Romsa - Tromssa
#> 337          Troms - Romsa - Tromssa
#> 338          Troms - Romsa - Tromssa
#> 339          Troms - Romsa - Tromssa
#> 340 Finnmark - Finnmárku - Finmarkku
#> 341 Finnmark - Finnmárku - Finmarkku
#> 342 Finnmark - Finnmárku - Finmarkku
#> 343 Finnmark - Finnmárku - Finmarkku
#> 344 Finnmark - Finnmárku - Finmarkku
#> 345 Finnmark - Finnmárku - Finmarkku
#> 346 Finnmark - Finnmárku - Finmarkku
#> 347 Finnmark - Finnmárku - Finmarkku
#> 348 Finnmark - Finnmárku - Finmarkku
#> 349 Finnmark - Finnmárku - Finmarkku
#> 350 Finnmark - Finnmárku - Finmarkku
#> 351 Finnmark - Finnmárku - Finmarkku
#> 352 Finnmark - Finnmárku - Finmarkku
#> 353 Finnmark - Finnmárku - Finmarkku
#> 354 Finnmark - Finnmárku - Finmarkku
#> 355 Finnmark - Finnmárku - Finmarkku
#> 356 Finnmark - Finnmárku - Finmarkku
#> 357 Finnmark - Finnmárku - Finmarkku
#> 358                         Uoppgitt
```
