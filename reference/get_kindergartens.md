# Get kindergartens

Fetch information from the National Kindergarten Registry (NBR).

## Usage

``` r
get_kindergartens(x, simplify = TRUE, raw_response = FALSE)
```

## Arguments

- x:

  A Norwegian county or municipality code. Use 'all' to retrieve all
  kindergartens

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

data.frame or list

## Details

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

See the [API documentation](https://www.udir.no/om-udir/data/nxr/) for
further details (in Norwegian only).

## See also

[`get_counties()`](https://riksrevisjonen.github.io/noAPI/reference/get_municipalities.md)
for retrieving county codes, and
[`get_municipalities()`](https://riksrevisjonen.github.io/noAPI/reference/get_municipalities.md)
for retrieving municipality codes.

## Examples

``` r
# Get schools by county
get_kindergartens(11)
#>         Orgnr                                                   Navn
#> 1   998724295                                          A2 Holding AS
#> 2   873158352                                       Aksdal barnehage
#> 3   973465724                                        Aktiv barnehage
#> 4   984318766                                    Amanda barnehage SA
#> 5   987058285                          Anna Nilssens Minne barnehage
#> 6   975212289                                   Askeladden barnehage
#> 7   873512032                                         Aski barnehage
#> 8   928462404                                      Aski barnehage AS
#> 9   989305026                               Asparmarka Fus barnehage
#> 10  974611112                                   Asperholen barnehage
#> 11  994964593                            Aud Torsen Familiebarnehage
#> 12  873864052      Auglend barnehage Tilrettelagt for Hørselshemmede
#> 13  992593865 Auglendsdalen, Frelsesarmeens barnehage i Stavanger AS
#> 14  975295966                                       Austbø barnehage
#> 15  973864025                                  Austre Åmøy barnehage
#> 16  974610981                                     Austrått barnehage
#> 17  975314650                                    Avaldsnes barnehage
#> 18  934004299                                Balder Fus barnehage AS
#> 19  999592147                            Bambi og Trampe Familie Bhg
#> 20  979150237                                      Bamsebo barnehage
#> 21  972397733                                   Bamsebu barnehage SA
#> 22  974612402                                   Bamsefaret barnehage
#> 23  975324524        Bamsefaret barnehage Avd. Jåtten Åpen Barnehage
#> 24  921752660                              Bamsefaret åpen barnehage
#> 25  974798697                                   Barnas Hus barnehage
#> 26  974798735                                   Barnas Hus barnehage
#> 27  988231401                                   Barnas Kulturhage AS
#> 28  991081534                                  Barnebygg Utemiljø AS
#> 29  982836999                                 Barnebygg Utvikling AS
#> 30  977010047                                   Barnehagen Himmelblå
#> 31  914562856                                  Barnehagen Holmegaten
#> 32  873893362                              Barnehagen i Bryne Kyrkje
#> 33  973337211                                       Barnehagen Tasta
#> 34  975323471                                   Bekketunet barnehage
#> 35  915376770                                   Bekkevegen barnehage
#> 36  984184743                                    Bikubå barnehage SA
#> 37  930699071                                      Biss Preschool AS
#> 38  985774811                   Biå steinerbarnehage AS avd Storhaug
#> 39  921792409             Biå steinerbarnehage AS Avdeling Mosvangen
#> 40  974798719                                    Bjergsted barnehage
#> 41  974798700           Bjergsted barnehage Avdeling Klingenberggata
#> 42  975296938                                         Bjoa barnehage
#> 43  994454420                                     Bjorhaug barnehage
#> 44  987711507                                  Bjørgene barnehage AS
#> 45  975308162                                    Bleikemyr barnehage
#> 46  994875698                               Bogafjell Åpne barnehage
#> 47  984811977                      Bogafjellbakken naturbarnehage SA
#> 48  973513826                                      Boganes barnehage
#> 49  974122081                                  Bokkaskogen barnehage
#> 50  974617684                               Bokn kommunale barnehage
#> 51  973459341                                         Bore barnehage
#> 52  875308262                                     Borsheim barnehage
#> 53  883620372                             Brakahaug Familiebarnehage
#> 54  872285482                                 Brakamoen barnehage SA
#> 55  975323528                            Breidablikk kulturbarnehage
#> 56  975308480                                          Bru barnehage
#> 57  974611201                                     Brueland barnehage
#> 58  990721726                              Bruhammaren Fus barnehage
#> 59  876671832                                   Bryne Opne barnehage
#> 60  988081132                            Brådalsfjellet barnehage AS
#> 61  993439495                                        Bråde barnehage
#> 62  875315072                                Bråsteintunet barnehage
#> 63  887536902                                 Buggeland barnehage SA
#> 64  974612348                                         Buøy barnehage
#> 65  920062407                             Byfjordparken barnehage AS
#> 66  975846318                            Byggeklossen Åpne barnehage
#> 67  979332556                                  Bygnes vitenbarnehage
#> 68  973485830                                  Byhaugen barnehage SA
#> 69  975308316                                      Bærland barnehage
#> 70  973448730                                        Bø barnehage AS
#> 71  985140340                                  Børesvingen barnehage
#> 72  911963531            Båtstadstien Familiebarnehage Monika Larsen
#> 73  987899468                                         Dale barnehage
#> 74  995109735                           Dansehagen åpen barnehage AS
#> 75  988584150                                  Delfinen barnehage AS
#> 76  975308324                                       Dirdal barnehage
#> 77  974612194                                    Djupamyrå barnehage
#> 78  880506552                               Dragaberget barnehage SA
#> 79  989969781                              Dragerskogen barnehage AS
#> 80  980498158                                 Dronningåsen barnehage
#> 81  973854860                             Duehuset Korttidsbarnehage
#> 82  982943620                                   Dusavik barnehage Da
#> 83  972217832                                   Dusavik barnehage SA
#> 84  989562975                           Dvergsnestangen barnehage AS
#> 85  990955131                             Egenes idrettsbarnehage AS
#> 86  987913592                                   Eigerøy barnehage SA
#> 87  991511105                                   Eikeberget barnehage
#> 88  975321746                                   Eikehaugen barnehage
#> 89  990821933                          Eivindsholen Fus barnehage AS
#> 90  975182606                               Ekofisk Junior barnehage
#> 91  973383418                                       Emmaus barnehage
#> 92  973810944                          Endrestø friluftsbarnehage SA
#> 93  991001387                                 Engel Familiebarnehage
#> 94  985334641                                   Engelsvoll barnehage
#> 95  930530174                               Engleongane barnehage AS
#> 96  974612291                                     Eskeland barnehage
#> 97  988482633                              Espira Bjørgene barnehage
#> 98  993464325                         Espira Bråsteintunet barnehage
#> 99  920568106                         Espira Bråsteintunet barnehage
#> 100 988801593                     Espira Eikenøtta naturbarnehage AS
#> 101 916419260                                Espira Fjeldebakkane AS
#> 102 973513834                                  Espira Fjellsenden AS
#> 103 986933565                               Espira Garhaug barnehage
#> 104 992840196                              Espira Karmsund barnehage
#> 105 992482273                         Espira Krystallveien barnehage
#> 106 992203897                             Espira Litlasund barnehage
#> 107 973730215                                  Espira Lura barnehage
#> 108 995492776                                      Espira Nykirke AS
#> 109 992461489                             Espira Ormadalen barnehage
#> 110 989586696                                   Espira Ree barnehage
#> 111 934684818                                  Espira Sandnesheia AS
#> 112 996908739                    Espira Sandtoppen naturbarnehage AS
#> 113 988230693                         Espira Scala Hundvåg barnehage
#> 114 988230707                           Espira Scala Tasta barnehage
#> 115 884876532                          Espira Skjeraberget barnehage
#> 116 892206082                            Espira Skåredalen barnehage
#> 117 992875194                               Espira Sletten barnehage
#> 118 974701480                            Espira Taremareby barnehage
#> 119 915691757                                   Espira Tastarustå AS
#> 120 992833033                                   Espira Tau barnehage
#> 121 992205113                              Espira Tjøsvoll barnehage
#> 122 989875922                            Espira Vagletjørn barnehage
#> 123 985488703                              Espira Veldetun barnehage
#> 124 886933592                                Espira Østrem barnehage
#> 125 988116459                                Espira Århaug barnehage
#> 126 989018655                                Esso-Ansattes barnehage
#> 127 999398596                                Esso-Ansattes barnehage
#> 128 995469332                            Eventus Solahagen barnehage
#> 129 983023673                             Eventyrberget barnehage SA
#> 130 973489321                                 Eventyrhagen barnehage
#> 131 873459662                          Ferkingstad Korttidsbarnehage
#> 132 973341200                                       Figgjo barnehage
#> 133 890906362                                   Figgjoelva barnehage
#> 134 815177142                                    Finnås barnehage AS
#> 135 984033915                                 Fiskebekk barnehage SA
#> 136 973383558                                       Fister barnehage
#> 137 986926577                             Fjeldebakkane barnehage AS
#> 138 975322009                            Fjellhaug friluftsbarnehage
#> 139 974551497                                    Fjellheim barnehage
#> 140 972214558                                 Fjellstemmen barnehage
#> 141 991560653                             Fladaberg Fus barnehage AS
#> 142 998710030                                 Flassabekken barnehage
#> 143 976672321                                         Fogn barnehage
#> 144 975308367                                      Forsand barnehage
#> 145 985487340                                   Forus åpen barnehage
#> 146 973459511                                        Fosen barnehage
#> 147 973590405                                      Fotland barnehage
#> 148 975314642                                  Frakkagjerd barnehage
#> 149 976036468                                  Frøystad barnehage SA
#> 150 915321240                                               Fun4kids
#> 151 987288892                                Furuholmen barnehage AS
#> 152 992044470                                                 Fus AS
#> 153 975321991                                      Førland barnehage
#> 154 975315509                                      Førland barnehage
#> 155 984705972                                  Førresdalen barnehage
#> 156 975308510                                Førresfjorden barnehage
#> 157 874610992                                      Ganddal barnehage
#> 158 972397865                                      Gard barnehage SA
#> 159 986207120                                   Garhaug barnehage AS
#> 160 986461078                              Gauselbakken barnehage SA
#> 161 993918474                                    Gautesete barnehage
#> 162 975816826                                   Geitastova barnehage
#> 163 993336122                                        Gilja barnehage
#> 164 973895281                Gjesdal Bondekvinnelag V/jorunn Nevland
#> 165 975325865                         Gjesdal kommune Åpen barnehage
#> 166 980465551                             Gode Venner Åpen barnehage
#> 167 974612399                                        Gosen barnehage
#> 168 974584158                                      Grannes barnehage
#> 169 874611182                                   Gravarslia barnehage
#> 170 935296773                                  Grid Fus barnehage AS
#> 171 974551500                                       Grødem barnehage
#> 172 983817025                                 Grøne Bråden barnehage
#> 173 994315692                              Grønnestølen barnehage AS
#> 174 973863576                                     Gullfaks barnehage
#> 175 973806602                           Gullongane Private barnehage
#> 176 977200946                                         Gunn Marit Owe
#> 177 985138885                                   Gåserud barnehage AS
#> 178 988988294                              Hafrsfjord åpen barnehage
#> 179 991276939                                   Hagatoppen barnehage
#> 180 972397741                 Hakkebakkeskogen barnehage Vormedal SA
#> 181 988675717        Hakkebakkeskogen Private barnehage AS avd Madla
#> 182 989324977              Hakkebakkeskogen private barnehage Stokka
#> 183 974798778        Hakkebakkeskogen private barnehage Tjensvoll AS
#> 184 923344217                                      Halsnøy barnehage
#> 185 974742977              Halsnøy Oppvekstsenter Avdeling barnehage
#> 186 982273854                                     Hammeren barnehage
#> 187 998400058                                         Hana barnehage
#> 188 972286478                             Hans og Grete barnehage SA
#> 189 986209077                          Haugaland idrettsbarnehage AS
#> 190 815225732               Haugesund International Preschool Fus AS
#> 191 982162971                                Haugland gardsbarnehage
#> 192 974612364                                    Haugtussa barnehage
#> 193 973863266                                     Havglimt barnehage
#> 194 975325814                                    Havhesten barnehage
#> 195 974584131                                  Havnealleen barnehage
#> 196 989427946                            Heddeveien Fus barnehage AS
#> 197 990428697                                  Helganes barnehage AS
#> 198 985086087                              Helldalsåsen barnehage AS
#> 199 973459007                                    Helleland barnehage
#> 200 994614371                                Helleland Fus barnehage
#> 201 973459015                                      Hellvik barnehage
#> 202 975268365                                  Hemmingstad barnehage
#> 203 916742789                               Hertervigtunet barnehage
#> 204 923344349                                        Hesby barnehage
#> 205 975320944                                    Heskestad barnehage
#> 206 991875441                                    Hestnesveien 255 AS
#> 207 975315045                                     Hillevåg barnehage
#> 208 973383396               Hillevåg barnehage Avdeling Timoteiveien
#> 209 973459600                         Hinderaker og Visnes barnehage
#> 210 973297058                                        Hinna barnehage
#> 211 874187542                                Hinna Korttidsbarnehage
#> 212 975070336                                   Hjelmeland barnehage
#> 213 928158284                                Holbekk Barnehagetun AS
#> 214 983959466                        Holmegaten familiebarnehage Ans
#> 215 915177093                                     Holum barnehage AS
#> 216 974610728                                    Hommersåk barnehage
#> 217 981331389                                    Horpestad barnehage
#> 218 975308227                                    Hovsherad barnehage
#> 219 976670094                                       Husabø barnehage
#> 220 975324915                                    Husafjell barnehage
#> 221 974612445                                Husebøstykket barnehage
#> 222 973484494                                    Huskestua barnehage
#> 223 994519204                                    Høgeholen barnehage
#> 224 974611155                                         Høle barnehage
#> 225 975325830                                      Høyland barnehage
#> 226 980525856                             Håbafjell idrettsbarnehage
#> 227 972397873                                    Håbet Fus barnehage
#> 228 987925396                              Hålandsmarkå barnehage SA
#> 229 873459522                                        Håvik barnehage
#> 230 989305174                                  Iglemyr Fus barnehage
#> 231 973878921                                          Imi barnehage
#> 232 987040858                                     Imi åpen barnehage
#> 233 989439944                                  Imi åpen barnehage AS
#> 234 983344267                                      Imsland barnehage
#> 235 880613502            Innføringsbarnehagen Johannes læringssenter
#> 236 975028798        International School Of Stavanger avd barnehage
#> 237 975300765                     Jelsa oppvekstsenter avd barnehage
#> 238 973502018                                    Jernhagen barnehage
#> 239 985956839                                 Jubelgjengen barnehage
#> 240 973459120                             Julebygda Heltidsbarnehage
#> 241 991037314                                      Jærbarnehagen Fus
#> 242 974611007                                  Jønningheia barnehage
#> 243 975296016                                    Jørpeland barnehage
#> 244 975308383                                 Jøsenfjorden barnehage
#> 245 975308391                                     Jøsneset barnehage
#> 246 996675262                                        Jåttå barnehage
#> 247 974735512                                       Kampen barnehage
#> 248 986358501                                     Kanutten barnehage
#> 249 973513818                                    Karihagen barnehage
#> 250 990636419                                  Karmsund barnehage AS
#> 251 990472386                           Kiellandskogen Fus barnehage
#> 252 973448706                                Kirketunet barnehage AS
#> 253 975308456                                Kjølvikstølen barnehage
#> 254 919361719                                     Kleivane barnehage
#> 255 973889982                               Kleppe friluftsbarnehage
#> 256 989427849                            Kleppestemmen Fus barnehage
#> 257 975027198                                       Kløver barnehage
#> 258 893447172                                    Kløvereng barnehage
#> 259 889476842                       Kløvereng barnehage Stavanger AS
#> 260 987477644                                Kløverenga barnehage AS
#> 261 989605127                                 Kniveåsen barnehage AS
#> 262 995075482                              Knøttene familiebarnehage
#> 263 982451361                                Knøttene åpen barnehage
#> 264 988190527                       Knåtten Private Familiebarnehage
#> 265 990233977                                   Kodlidalen barnehage
#> 266 979525699                             Kollberge Familiebarnehage
#> 267 995342758                     Kollberge Familiebarnehage Limited
#> 268 976828593                                       Kolnes barnehage
#> 269 974001292                                   Kornberget barnehage
#> 270 974187558                                       Krabat barnehage
#> 271 929838920                                   Kreativ barnehage AS
#> 272 885472672                            Kreativ barnehage Bogafjell
#> 273 929838939                         Kreativ barnehage Bogafjell AS
#> 274 916115881                                 Kreativ barnehage Hove
#> 275 929838963                              Kreativ barnehage Hove AS
#> 276 992081910                             Krystallveien barnehage AS
#> 277 981294149                                     Kvala barnehage AS
#> 278 973459716                                     Kvalavåg barnehage
#> 279 974612429                                    Kvaleberg barnehage
#> 280 993885339                                   Kvernaland barnehage
#> 281 974612437                                    Kvernevik barnehage
#> 282 974166186                                    Kvitodden barnehage
#> 283 873910542                                      Kvitsøy barnehage
#> 284 873489332                                 Kyrkjebakken barnehage
#> 285 887731152                                Kyvikdalen barnehage SA
#> 286 989003720                                   Kyvikmarka barnehage
#> 287 898597512                                       Laetus Invest AS
#> 288 917084319                                     Langgata barnehage
#> 289 989476610                                        Lassa barnehage
#> 290 916658109                                     Lassa barnehage AS
#> 291 994160214                                    Lassamyra barnehage
#> 292 975308472                                      Leabøen barnehage
#> 293 988726206                                     Lekeland barnehage
#> 294 975323536                              Lensmannsgarden barnehage
#> 295 987249471  Lerkå Musikk-Familiebarnehage Lena Ekholdt Sveinsvoll
#> 296 986769641  Lilleputt private familiebarnehage Marianne Pettersen
#> 297 987219963                                               Li-Te AS
#> 298 890641482                                 Litlasund barnehage AS
#> 299 885938442                                         Loen barnehage
#> 300 975829820      Losgaten Familiebarnehage Randi Kalland Bjørnevåg
#> 301 873263792                                 Lundegård barnehage SA
#> 302 889305142                               Lundehagen Fus barnehage
#> 303 991431950                                           Lura Bedehus
#> 304 975308286                                     Lyefjell barnehage
#> 305 983491251                                   Lykkeliten barnehage
#> 306 997828143             Lykkeliten Familiebarnehage Hennika Lødner
#> 307 995276097                Lykkeliten Familiebarnehage Kine Sinnes
#> 308 975315347                                    Lyngmarka barnehage
#> 309 984273894                                      Lysskar barnehage
#> 310 994234331                 Læringsverkstedet Bratteborg barnehage
#> 311 987445394              Læringsverkstedet Fjeldebakkane barnehage
#> 312 974509679                      Læringsverkstedet Forus barnehage
#> 313 981305469              Læringsverkstedet Frøyland gårdsbarnehage
#> 314 919643927      Læringsverkstedet Hannes Lekestue Madla barnehage
#> 315 973463500  Læringsverkstedet Hannes Lekestue Tjensvoll barnehage
#> 316 997813553                  Læringsverkstedet Julebygda barnehage
#> 317 917151644                     Læringsverkstedet Jåsund barnehage
#> 318 974797615                   Læringsverkstedet Kopervik barnehage
#> 319 998547180              Læringsverkstedet Lervig Brygge barnehage
#> 320 973152556                   Læringsverkstedet Novarmen barnehage
#> 321 990906955                   Læringsverkstedet Osa gårdsbarnehage
#> 322 986119698              Læringsverkstedet Strutsen naturbarnehage
#> 323 880541072                Læringsverkstedet Trollberget barnehage
#> 324 889484772               Læringsverkstedet Vatnekrossen barnehage
#> 325 987013060                 Læringsverkstedet Åkrasanden barnehage
#> 326 989331558                               Lønneberget barnehage AS
#> 327 991196404                                  Løvestad barnehage AS
#> 328 973154710                                    Madlavoll barnehage
#> 329 974612224                                     Malthaug barnehage
#> 330 973863878                                   Mariamarka barnehage
#> 331 995111527                      Marihøna Private Familiebarnehage
#> 332 931491660                                        Markå barnehage
#> 333 985131678                                     Markå barnehage SA
#> 334 989635433                              Marthahaugen barnehage AS
#> 335 976719883                              Maudland Fus barnehage AS
#> 336 894505192                              Maudland Fus barnehage AS
#> 337 975315053                                      Maurtua barnehage
#> 338 972399434                                      Maurtuå barnehage
#> 339 979272200                          Miniklubben Naturbarnehage AS
#> 340 973912186                               Mjughaugskogen barnehage
#> 341 987108002                            Mjåsund Familiebarnehage Da
#> 342 974612259                                   Molkeholen barnehage
#> 343 975323579                                     Monsanut barnehage
#> 344 994192159                                            Monsunen AS
#> 345 991299351                                   Morgedalen barnehage
#> 346 915177131                                    Moster barnehage AS
#> 347 916336101                                Motlandsmarka barnehage
#> 348 992196440                         Motlandsmarka Familiebarnehage
#> 349 994323768                      Motlandsmarka Familiebarnehage Da
#> 350 875325892                                      Mottaksbarnehagen
#> 351 873504102                                Muldvarpen barnehage SA
#> 352 933964817                                        Mykje barnehage
#> 353 973857606                                  Myklaberget barnehage
#> 354 874860212                                     Myrsnibå barnehage
#> 355 974288028                                     Myrsnipa barnehage
#> 356 973900749                                         Myrå barnehage
#> 357 976827910                                        Måken barnehage
#> 358 994591061                                    Naturbarnehagene AS
#> 359 973158902                               Nausthaugen barnehage AS
#> 360 973908464                              Nbu's bondegårdsbarnehage
#> 361 974798999                                    Nedstrand barnehage
#> 362 875308432                 Nesflaten oppvekstsenter avd barnehage
#> 363 988414557                                      Nesjane barnehage
#> 364 995902052                                           Nina Mjølhus
#> 365 982926610                                Noas Ark Open barnehage
#> 366 992239875                                Nordjordet barnehage AS
#> 367 990038120                                  Novvegen barnehage AS
#> 368 994944908                             Nubben Familiebarnehage Da
#> 369 975308294                                       Nyland barnehage
#> 370 973863339                                  Nymansveien barnehage
#> 371 984093446                                    Nyvoll barnehage SA
#> 372 893423222                                Nærlandparken barnehage
#> 373 930948519                   Nøtteliten Fam barnehage Dvergene AS
#> 374 930948349                 Nøtteliten Fam barnehage Skogstroll AS
#> 375 930846880                    Nøtteliten Fam barnehage Smårips AS
#> 376 930847054                Nøtteliten Fam barnehage Trollungene AS
#> 377 975308332                                      Oltedal barnehage
#> 378 975300374                                         Ombo barnehage
#> 379 934017862                                   Ona Fus barnehage AS
#> 380 992123974                                 Ormadalen barnehage AS
#> 381 985669864                                   Ormaskogen barnehage
#> 382 975308278                                         Orre barnehage
#> 383 975300315                                  Orstad naturbarnehage
#> 384 926369199                                    Pilevegen barnehage
#> 385 974611104                                    Porsholen barnehage
#> 386 974156423              Preg barnehager AS avd Frøyland barnehage
#> 387 974156342               Preg barnehager AS avd Solheim barnehage
#> 388 974183730                              Preg barnehager Finnøy AS
#> 389 919816988                            Preg barnehager Frøyland AS
#> 390 974134985                               Preg barnehager Hinna AS
#> 391 974146460                               Preg barnehager Lassa AS
#> 392 991581189                                Preg barnehager Lura AS
#> 393 974156431                                 Preg barnehager Tau AS
#> 394 873157232                      Preg barnehager Tryggheimvegen AS
#> 395 989857746                                Presthaug Fus barnehage
#> 396 988615889                                Qmarkå Fus barnehage AS
#> 397 973484656                                     Rabalder barnehage
#> 398 924579811                              Rabalder Fus barnehage AS
#> 399 976671805                             Ragnestølsberget barnehage
#> 400 973589032                                  Raketten barnehage SA
#> 401 974612208                                Ramsvigskogen barnehage
#> 402 975308405                                       Randøy barnehage
#> 403 988642401                                       Ree barnehage AS
#> 404 974187566                              Regnbuen Kanvas-barnehage
#> 405 974617528                                     Rennesøy barnehage
#> 406 988221708                             Rise & Sæbbe Barnehager AS
#> 407 991956484                              Rishagen Fus barnehage AS
#> 408 974611015                                        Riska barnehage
#> 409 977009898                                             Riska Sokn
#> 410 916824327                              Riskatun Fus barnehage AS
#> 411 988734209           Riskjell Familiebarnehage Ved C. Killingland
#> 412 974584174                                       Risnes barnehage
#> 413 979450133                               Rissebærstraen barnehage
#> 414 993964867                                 Risøy Familiebarnehage
#> 415 974612275                                     Roaldsøy barnehage
#> 416 989304879                               Robåten Fus barnehage AS
#> 417 873512172                                   Rodamyr barnehage SA
#> 418 994807137                        Rognan Trollskogen barnehage AS
#> 419 972179728                                Rognebærhagen barnehage
#> 420 993423246                                   Rosktoppen barnehage
#> 421 992820349                     Rossabø Fus aktivitetsbarnehage AS
#> 422 973482599                             Rufus friluftsbarnehage SA
#> 423 975308073                                    Rundevoll barnehage
#> 424 975308464                                        Rustå barnehage
#> 425 972214361                                  Rusvik naturbarnehage
#> 426 973477986                                 Ryggstranden barnehage
#> 427 983235875                                     Ryvingen barnehage
#> 428 994765825                                   Rødknappen barnehage
#> 429 987055421                              Rødlandsmyrå barnehage SA
#> 430 975308200                                        Røvær barnehage
#> 431 974584204                                    Røyneberg barnehage
#> 432 891521332                              Røyrvik gårdsbarnehage AS
#> 433 974623544                                  Røysekatten barnehage
#> 434 993003352                                  Rådhusmarka barnehage
#> 435 975299058                                      Sagatun barnehage
#> 436 989560603                             Salamonskogen barnehage AS
#> 437 973862340                            Sammen Vardafjell barnehage
#> 438 875315722                                         Sand barnehage
#> 439 975308448                           Sand barnehage avd Joagarden
#> 440 935512042                            Sand barnehage avd Randåsen
#> 441 974612216                                       Sandal barnehage
#> 442 973900722                                        Sande barnehage
#> 443 974584069                                        Sande barnehage
#> 444 987624159                           Sandehålå Sfo Birger Høydahl
#> 445 974618176                                      Sandeid barnehage
#> 446 991496416                    Sandhåland musikk-gårdsbarnehage AS
#> 447 890517692                           Sandtoppen Naturbarnehage AS
#> 448 974611023                                      Sandved barnehage
#> 449 993003301                                Sandvedhaugen barnehage
#> 450 993439584                                 Sandvikveien barnehage
#> 451 974612305                                   Satelitten barnehage
#> 452 975204154                                      Sevland barnehage
#> 453 973383574                                    Sjernarøy barnehage
#> 454 983492673                                  Skaarlia barnehage SA
#> 455 988010898                                  Skadberg barnehage SA
#> 456 992589817                        Skattekisten kulturbarnehage AS
#> 457 973863959                                   Skeiehagen barnehage
#> 458 993072079                                  Skjalgstova barnehage
#> 459 980684598                                 Skjeraberget barnehage
#> 460 974618192                                       Skjold barnehage
#> 461 975308529                              Skjoldastraumen barnehage
#> 462 914094755              Skjoldastraumen Maritime Fus barnehage AS
#> 463 992435976                                    Skogen barnehage AS
#> 464 912013804                                  Skorpefjell barnehage
#> 465 974612186                                   Skredbakka barnehage
#> 466 975290964                                     Skudenes barnehage
#> 467 975297543                              Skudenes Kortidsbarnehage
#> 468 991174451                                 Skulegata barnehage AS
#> 469 988542296                                 Skåre Fus barnehage AS
#> 470 990807701                                Skåredalen barnehage AS
#> 471 984470789                                  Slagstemmen barnehage
#> 472 979150172                                     Slettebø barnehage
#> 473 992755245                                   Sletten barnehage AS
#> 474 874612162                                    Slåtthaug barnehage
#> 475 983639534                                     Smeaheia barnehage
#> 476 974612356                                       Smiene barnehage
#> 477 973863347                                    Smietunet barnehage
#> 478 973864483                                Smiodden åpen barnehage
#> 479 994349880                        Små barnehager AS avd Torvastad
#> 480 994888498                            Små barnehager AS avd Viken
#> 481 915367240                           Små barnehager avd Brakahaug
#> 482 933225607                            Små barnehager Brakahaug AS
#> 483 972174068                               Små barnehager Grutle AS
#> 484 934007018                               Små barnehager Grutle AS
#> 485 998006287                               Små Barnehager Haugesund
#> 486 911655861                     Små barnehager Haugesund avd Risøy
#> 487 816281172                              Små barnehager Sentrum AS
#> 488 919858702                                 Småbarnehager Røvær AS
#> 489 893816232                         Småtrollingan familiebarnehage
#> 490 993762938           Snehvit Familie barnehage John Tomren Høgset
#> 491 985961972                             Snurrefjellet barnehage AS
#> 492 874584142                                        Snøde barnehage
#> 493 973484087                                         Sola barnehage
#> 494 975308219                                Solandsbakken barnehage
#> 495 974551527                                    Solbakken barnehage
#> 496 974142740                                      Solborg barnehage
#> 497 990154031                                Solknatten barnehage AS
#> 498 979986505                Solsikken barnehage Kitte M Kristiansen
#> 499 974584212                                   Solstrålen barnehage
#> 500 979322399                          Solstrålen barnehage Øygarden
#> 501 993439592                                      Solvang barnehage
#> 502 975308359                                        Solås barnehage
#> 503 983093167                               Soma Gård naturbarnehage
#> 504 972217840                                  Soria Moria barnehage
#> 505 932163713                                   Sporafjell barnehage
#> 506 988370592                                  Sporvongane barnehage
#> 507 888854282                            Sprell Levende barnehage AS
#> 508 987046635                              Spødarbakken barnehage SA
#> 509 975070719                                     St. Olav barnehage
#> 510 992414693                    St. Petri barnehage Avdeling Egenes
#> 511 993447137                                  St. Svithun barnehage
#> 512 974611031                                   Stangeland barnehage
#> 513 980760618                             Stangelandsforen barnehage
#> 514 995248247                           Stavanger British Pre-School
#> 515 991194800                         Steinarskogen Fus barnehage AS
#> 516 984868898                                 Steinerskolen på Jæren
#> 517 976828216                                  Steingarden barnehage
#> 518 975325849                                  Steingarden barnehage
#> 519 974612178                                   Steinhagen barnehage
#> 520 987730951                             Steinhaugane Fus barnehage
#> 521 974093634                                    Steinholt barnehage
#> 522 986707700                             Steinsfjellet barnehage SA
#> 523 986934308                               Steinsviken barnehage AS
#> 524 990739617                                Stemvegen Fus barnehage
#> 525 973470086                                    Stiftelsen Hammaren
#> 526 974798670                           Stiftelsen Håholen barnehage
#> 527 985439605                           Stiftelsen Norheim barnehage
#> 528 973464841                Stiftelsen Soria Moria steinerbarnehage
#> 529 971554878             Stiftelsen Steinerbarnehagen Bukkene Bruse
#> 530 974612372                                       Stokka barnehage
#> 531 974122103                       Stokka barnehage Astmabarnehagen
#> 532 992695250                               Stokka åpen barnehage AS
#> 533 921752849                                  Stokkadalen barnehage
#> 534 994454439                             Stokkalandsmarka barnehage
#> 535 974612321                                  Stokkatunet barnehage
#> 536 988645990                             Stongafjellet barnehage AS
#> 537 891634242                          Storafjellet Fus barnehage AS
#> 538 975198146                                    Storesund barnehage
#> 539 974796880                                     Storhaug barnehage
#> 540 985917981                                Storhaug åpen barnehage
#> 541 930044997                                     Strømvig barnehage
#> 542 988645214                  Styrka Førskuletilbod Innan barnehage
#> 543 987541466                              Styrkingstiltak barnehage
#> 544 975299228                                      Stølane barnehage
#> 545 987422424                                       Suldal barnehage
#> 546 974617013           Suldal barnehage avd Barnehageadministrasjon
#> 547 996047717               Suldal kommune Barnehagefagleg Rådgjevar
#> 548 992239883                                Sundbyfoss barnehage AS
#> 549 974325543                                        Sunde barnehage
#> 550 991299319                                     Sundelia barnehage
#> 551 973861905                            Svanholmen Kanvas-barnehage
#> 552 992098899                               Sviland Fus barnehage AS
#> 553 993595136                               Sylling Fus barnehage AS
#> 554 974584182                                        Sømme barnehage
#> 555 913825721                                        Sørbø barnehage
#> 556 920019242                                   Sørbø barnehage Nord
#> 557 991343628                                    Sørhellet barnehage
#> 558 996675270                               Sørmarka Arena barnehage
#> 559 988916102                       Sørmarka Private Familibarnehage
#> 560 974584077                                       Sørnes barnehage
#> 561 987751894                                  Sørnes åpen barnehage
#> 562 973485849                                       Talgje barnehage
#> 563 974584050                                     Tananger barnehage
#> 564 973535765                            Tananger menighetsbarnehage
#> 565 992755288                                       Tau barnehage AS
#> 566 930010014                                    Teknikken barnehage
#> 567 973459414                          The Childrens House barnehage
#> 568 934330951                                Tinnfabrikken barnehage
#> 569 974584190                                       Tjelta barnehage
#> 570 985093474                           Tjemslandshagen barnehage SA
#> 571 973863975                                    Tjensvoll barnehage
#> 572 874612332                                      Tjensås barnehage
#> 573 999324657                                    Tjodmarka barnehage
#> 574 891808062                                  Tjøsvoll barnehage AS
#> 575 996258661                          Tjøttaparken Fus barnehage AS
#> 576 975943232                                Tommeliten barnehage SA
#> 577 975323498                                       Toppen barnehage
#> 578 995078422                    Tordenskjold Private Familiebarneha
#> 579 973864033                                   Torsahagen barnehage
#> 580 973279998                                    Torvastad barnehage
#> 581 974499738        Trekløver Familiebarnehage Henriette Brynjelsen
#> 582 975294579                                  Trekløveren barnehage
#> 583 974612410                                       Triade barnehage
#> 584 975266842                             Tripp Trapp åpen barnehage
#> 585 986434283                   Trollhaugen foreldrelagsbarnehage SA
#> 586 973459384                                  Trollkroken barnehage
#> 587 972154172                                  Trollongane barnehage
#> 588 975315061                                  Trollskogen barnehage
#> 589 982353858      Trollungen Familiebarnehage Gro Kristin Tollefsen
#> 590 973857576                                       Trones barnehage
#> 591 994530607                                  Trygstad barnehage AS
#> 592 990141223                                          Trymiviuna AS
#> 593 879199972                                           Tu barnehage
#> 594 975323420                             Tusenbeinet åpen barnehage
#> 595 972726184                                 Tusenfryd barnehage AS
#> 596 979198094                                     Tyrihans barnehage
#> 597 975300773                                    Tysværvåg barnehage
#> 598 987879734                                Udland Fus barnehage AS
#> 599 974612267                                    Ulsberget barnehage
#> 600 974325489                                    Ulsnes barnehage SA
#> 601 974185563                                      Undheim barnehage
#> 602 975299015                                        Urhei barnehage
#> 603 987850027                         Utsikten Vest Fus barnehage AS
#> 604 974617811                                       Utsira barnehage
#> 605 988676543                                Vagletjørn barnehage AS
#> 606 973863967                                    Vannassen barnehage
#> 607 989495488                           Vannverksdammen barnehage AS
#> 608 987509066                                     Vanse barnehage AS
#> 609 979352581                    Vanvik oppvekstsenter avd barnehage
#> 610 874611042                                      Varatun barnehage
#> 611 920019293                         Varatun barnehage Varatunhagen
#> 612 973231782                                       Varden barnehage
#> 613 973465716                                    Varden barnehage SA
#> 614 974612380                                   Vardeneset barnehage
#> 615 998374103                               Varhaug idrettsbarnehage
#> 616 991241043                      Vassbrekke Fus kulturbarnehage AS
#> 617 887908052                                      Vasshus barnehage
#> 618 988956848                              Vasshus gardsbarnehage AS
#> 619 974612240                                       Vassøy barnehage
#> 620 974611058                                        Vatne barnehage
#> 621 974618184                                         Vats barnehage
#> 622 973459724                                          Vea barnehage
#> 623 929590627                                   Veden åpen barnehage
#> 624 985508283                                  Veldetun barnehage AS
#> 625 975290980                                   Veslefrikk barnehage
#> 626 888115692                                       Vestly barnehage
#> 627 998713528                                Vestly idrettsbarnehage
#> 628 975317943                                  Vestre Åmøy barnehage
#> 629 975268330                                   Vibereiret barnehage
#> 630 986091947               Vibå musikkfamiliebarnehage Hilde Nysted
#> 631 875323512                                        Vigre barnehage
#> 632 975268322                                    Vigrestad barnehage
#> 633 975296946                                     Vikebygd barnehage
#> 634 974618168                                      Vikedal barnehage
#> 635 992821167                               Vikedal Fus barnehage AS
#> 636 993964573                                 Viken Familiebarnehage
#> 637 992552603                                      Vikevåg barnehage
#> 638 912702952                            Villbassen Familiebarnehage
#> 639 973908588                                  Villduen barnehage SA
#> 640 973475517                                    Villrosen barnehage
#> 641 975314634                                       Vinjar barnehage
#> 642 974551519                                        Viste barnehage
#> 643 991599355                                  Vistestølen barnehage
#> 644 975323595                                     Vormedal barnehage
#> 645 973511335                    Vormedal barnehage avd Myklandvegen
#> 646 891006462                             Vormedal Korttidsbarnehage
#> 647 973449656                             Vormedal Korttidsbarnehage
#> 648 975308413                                   Vormedalen barnehage
#> 649 974612550                                    Vågedalen barnehage
#> 650 974612283                                        Vågen barnehage
#> 651 975308618                                        Vågen barnehage
#> 652 985345996                                     Vågsmyra barnehage
#> 653 988988103                                       Våland barnehage
#> 654 973265059                                Vålandshaugen barnehage
#> 655 893589872                               Ze-Bra gårdsbarnehage AS
#> 656 986346708                         Ze-Bra Steindal Gårdsbarnehage
#> 657 975323544                                     Øksnevad barnehage
#> 658 987017422                             Ølensjøen Fus barnehage AS
#> 659 973494783                            Ørnestein gårdsbarnehage AS
#> 660 989826956                                    Østhusvik barnehage
#> 661 986529950                                    Østrem barnehage AS
#> 662 984937253                                       Øygard barnehage
#> 663 920273955                                Øygardsvatnet barnehage
#> 664 975323501                                     Øyno Fus barnehage
#> 665 976671821                                       Ådalen barnehage
#> 666 912275310                               Åkra Sokneråds barnehage
#> 667 973502158                                     Åna Sira barnehage
#> 668 980239055                                         Åpen barnehage
#> 669 973877070                               Åpen barnehage Haugesund
#> 670 875299042                                        Årdal barnehage
#> 671 991297626                               Åsebøen Fus barnehage AS
#>                  Karakteristikk Fylkesnr Kommunenr Epost ErAktiv ErBarnehage
#> 1                                     11      1103    NA   FALSE        TRUE
#> 2                                     11      1146    NA    TRUE        TRUE
#> 3                                     11      1108    NA    TRUE        TRUE
#> 4                                     11      1106    NA    TRUE        TRUE
#> 5                                     11      1106    NA    TRUE        TRUE
#> 6                                     11      1119    NA    TRUE        TRUE
#> 7                                     11      1149    NA    TRUE        TRUE
#> 8                                     11      1149    NA   FALSE        TRUE
#> 9                                     11      1146    NA    TRUE        TRUE
#> 10                                    11      1108    NA    TRUE        TRUE
#> 11                                    11      1160    NA   FALSE        TRUE
#> 12                                    11      1103    NA    TRUE        TRUE
#> 13                                    11      1103    NA    TRUE        TRUE
#> 14                                    11      1121    NA    TRUE        TRUE
#> 15                                    11      1103    NA   FALSE        TRUE
#> 16                                    11      1108    NA    TRUE        TRUE
#> 17                                    11      1149    NA    TRUE        TRUE
#> 18                                    11      1106    NA    TRUE        TRUE
#> 19                                    11      1124    NA   FALSE        TRUE
#> 20                                    11      1122    NA    TRUE        TRUE
#> 21                                    11      1103    NA    TRUE        TRUE
#> 22                                    11      1103    NA    TRUE        TRUE
#> 23   Avd. Jåtten Åpen barnehage       11      1103    NA   FALSE        TRUE
#> 24                                    11      1103    NA    TRUE        TRUE
#> 25    Avdeling Bekkefaret Kirke       11      1103    NA   FALSE        TRUE
#> 26                                    11      1103    NA    TRUE        TRUE
#> 27                                    11      1130    NA    TRUE        TRUE
#> 28                                    11      1149    NA   FALSE        TRUE
#> 29                                    11      1149    NA   FALSE        TRUE
#> 30                                    11      1103    NA    TRUE        TRUE
#> 31                                    11      1106    NA   FALSE        TRUE
#> 32                                    11      1121    NA    TRUE        TRUE
#> 33                                    11      1103    NA    TRUE        TRUE
#> 34                                    11      1103    NA    TRUE        TRUE
#> 35                                    11      1130    NA    TRUE        TRUE
#> 36                                    11      1130    NA    TRUE        TRUE
#> 37                                    11      1103    NA    TRUE        TRUE
#> 38                                    11      1103    NA    TRUE        TRUE
#> 39                                    11      1103    NA    TRUE        TRUE
#> 40                                    11      1103    NA    TRUE        TRUE
#> 41     Avdeling Klingenberggata       11      1103    NA   FALSE        TRUE
#> 42                                    11      1160    NA    TRUE        TRUE
#> 43                                    11      1119    NA    TRUE        TRUE
#> 44                                    11      1106    NA   FALSE        TRUE
#> 45                                    11      1106    NA    TRUE        TRUE
#> 46                                    11      1108    NA    TRUE        TRUE
#> 47                                    11      1108    NA    TRUE        TRUE
#> 48                                    11      1103    NA    TRUE        TRUE
#> 49                                    11      1103    NA    TRUE        TRUE
#> 50                                    11      1145    NA    TRUE        TRUE
#> 51                                    11      1120    NA   FALSE        TRUE
#> 52                                    11      1120    NA    TRUE        TRUE
#> 53                                    11      1106    NA   FALSE        TRUE
#> 54                                    11      1135    NA    TRUE        TRUE
#> 55                                    11      1119    NA    TRUE        TRUE
#> 56                                    11      1103    NA    TRUE        TRUE
#> 57                                    11      1108    NA    TRUE        TRUE
#> 58                                    11      1122    NA    TRUE        TRUE
#> 59                                    11      1121    NA   FALSE        TRUE
#> 60                                    11      1149    NA   FALSE        TRUE
#> 61                                    11      1103    NA    TRUE        TRUE
#> 62                                    11      1106    NA   FALSE        TRUE
#> 63                                    11      1108    NA    TRUE        TRUE
#> 64                                    11      1103    NA    TRUE        TRUE
#> 65                                    11      1103    NA    TRUE        TRUE
#> 66                                    11      1102    NA   FALSE        TRUE
#> 67                                    11      1149    NA    TRUE        TRUE
#> 68                                    11      1103    NA    TRUE        TRUE
#> 69                                    11      1122    NA   FALSE        TRUE
#> 70                                    11      1111    NA    TRUE        TRUE
#> 71                                    11      1103    NA    TRUE        TRUE
#> 72                                    11      1124    NA   FALSE        TRUE
#> 73                                    11      1102    NA   FALSE        TRUE
#> 74                                    11      1103    NA    TRUE        TRUE
#> 75                                    11      1103    NA   FALSE        TRUE
#> 76                                    11      1122    NA   FALSE        TRUE
#> 77                                    11      1103    NA    TRUE        TRUE
#> 78                                    11      1103    NA    TRUE        TRUE
#> 79                                    11      1149    NA   FALSE        TRUE
#> 80                                    11      1103    NA    TRUE        TRUE
#> 81                                    11      1101    NA   FALSE        TRUE
#> 82                                    11      1103    NA   FALSE        TRUE
#> 83                                    11      1103    NA    TRUE        TRUE
#> 84                                    11      1149    NA   FALSE        TRUE
#> 85                                    11      1103    NA    TRUE        TRUE
#> 86                                    11      1101    NA    TRUE        TRUE
#> 87                                    11      1124    NA    TRUE        TRUE
#> 88                                    11      1130    NA    TRUE        TRUE
#> 89                                    11      1121    NA    TRUE        TRUE
#> 90                                    11      1124    NA    TRUE        TRUE
#> 91                                    11      1103    NA    TRUE        TRUE
#> 92                                    11      1127    NA    TRUE        TRUE
#> 93                                    11      1120    NA   FALSE        TRUE
#> 94                                    11      1120    NA    TRUE        TRUE
#> 95                                    11      1108    NA   FALSE        TRUE
#> 96                                    11      1103    NA    TRUE        TRUE
#> 97                                    11      1106    NA    TRUE        TRUE
#> 98                                    11      1106    NA   FALSE        TRUE
#> 99                                    11      1106    NA    TRUE        TRUE
#> 100                                   11      1108    NA    TRUE        TRUE
#> 101                                   11      1149    NA   FALSE        TRUE
#> 102                                   11      1103    NA    TRUE        TRUE
#> 103                                   11      1146    NA    TRUE        TRUE
#> 104                                   11      1149    NA    TRUE        TRUE
#> 105                                   11      1108    NA    TRUE        TRUE
#> 106                                   11      1149    NA    TRUE        TRUE
#> 107                                   11      1108    NA    TRUE        TRUE
#> 108                                   11      1119    NA   FALSE        TRUE
#> 109                                   11      1120    NA    TRUE        TRUE
#> 110                                   11      1121    NA    TRUE        TRUE
#> 111                                   11      1149    NA    TRUE        TRUE
#> 112                                   11      1108    NA    TRUE        TRUE
#> 113                                   11      1103    NA    TRUE        TRUE
#> 114                                   11      1103    NA    TRUE        TRUE
#> 115                                   11      1119    NA    TRUE        TRUE
#> 116                                   11      1106    NA    TRUE        TRUE
#> 117                                   11      1149    NA    TRUE        TRUE
#> 118                                   11      1108    NA    TRUE        TRUE
#> 119                                   11      1103    NA    TRUE        TRUE
#> 120                                   11      1130    NA    TRUE        TRUE
#> 121                                   11      1149    NA    TRUE        TRUE
#> 122                                   11      1108    NA    TRUE        TRUE
#> 123                                   11      1149    NA    TRUE        TRUE
#> 124                                   11      1149    NA    TRUE        TRUE
#> 125                                   11      1149    NA    TRUE        TRUE
#> 126                                   11      1102    NA   FALSE        TRUE
#> 127                                   11      1108    NA   FALSE        TRUE
#> 128                                   11      1124    NA    TRUE        TRUE
#> 129                                   11      1124    NA    TRUE        TRUE
#> 130                                   11      1149    NA    TRUE        TRUE
#> 131                                   11      1149    NA   FALSE        TRUE
#> 132                                   11      1108    NA    TRUE        TRUE
#> 133                                   11      1108    NA    TRUE        TRUE
#> 134                                   11      1149    NA   FALSE        TRUE
#> 135                                   11      1122    NA    TRUE        TRUE
#> 136                                   11      1133    NA    TRUE        TRUE
#> 137                                   11      1130    NA   FALSE        TRUE
#> 138                                   11      1146    NA    TRUE        TRUE
#> 139                                   11      1127    NA    TRUE        TRUE
#> 140                                   11      1106    NA    TRUE        TRUE
#> 141                                   11      1149    NA    TRUE        TRUE
#> 142                                   11      1122    NA    TRUE        TRUE
#> 143                                   11      1103    NA    TRUE        TRUE
#> 144                                   11      1108    NA    TRUE        TRUE
#> 145                                   11      1103    NA    TRUE        TRUE
#> 146                                   11      1149    NA   FALSE        TRUE
#> 147                                   11      1121    NA   FALSE        TRUE
#> 148                                   11      1146    NA   FALSE        TRUE
#> 149                                   11      1103    NA    TRUE        TRUE
#> 150                                   11      1120    NA   FALSE        TRUE
#> 151                                   11      1149    NA   FALSE        TRUE
#> 152                                   11      1106    NA   FALSE        TRUE
#> 153                                   11      1146    NA    TRUE        TRUE
#> 154                                   11      1130    NA    TRUE        TRUE
#> 155                                   11      1146    NA    TRUE        TRUE
#> 156                                   11      1146    NA    TRUE        TRUE
#> 157                                   11      1108    NA    TRUE        TRUE
#> 158                                   11      1106    NA    TRUE        TRUE
#> 159                                   11      1146    NA   FALSE        TRUE
#> 160                                   11      1103    NA    TRUE        TRUE
#> 161                                   11      1103    NA    TRUE        TRUE
#> 162                                   11      1103    NA    TRUE        TRUE
#> 163                                   11      1122    NA    TRUE        TRUE
#> 164                                   11      1122    NA   FALSE        TRUE
#> 165                                   11      1122    NA   FALSE        TRUE
#> 166                                   11      1102    NA   FALSE        TRUE
#> 167                                   11      1103    NA    TRUE        TRUE
#> 168                                   11      1124    NA   FALSE        TRUE
#> 169                                   11      1108    NA    TRUE        TRUE
#> 170                                   11      1106    NA    TRUE        TRUE
#> 171                                   11      1127    NA   FALSE        TRUE
#> 172                                   11      1101    NA    TRUE        TRUE
#> 173                                   11      1149    NA   FALSE        TRUE
#> 174                                   11      1103    NA    TRUE        TRUE
#> 175                                   11      1103    NA   FALSE        TRUE
#> 176                                   11      1160    NA   FALSE        TRUE
#> 177                                   11      1149    NA   FALSE        TRUE
#> 178                                   11      1103    NA    TRUE        TRUE
#> 179                                   11      1103    NA    TRUE        TRUE
#> 180                                   11      1149    NA    TRUE        TRUE
#> 181                   Avd Madla       11      1103    NA   FALSE        TRUE
#> 182                                   11      1103    NA    TRUE        TRUE
#> 183                                   11      1103    NA    TRUE        TRUE
#> 184                                   11      1103    NA    TRUE        TRUE
#> 185          Avdeling barnehage       11      1141    NA   FALSE        TRUE
#> 186                                   11      1112    NA    TRUE        TRUE
#> 187                                   11      1108    NA    TRUE        TRUE
#> 188                                   11      1108    NA    TRUE        TRUE
#> 189                                   11      1106    NA    TRUE        TRUE
#> 190                                   11      1106    NA    TRUE        TRUE
#> 191                                   11      1121    NA    TRUE        TRUE
#> 192                                   11      1103    NA    TRUE        TRUE
#> 193                                   11      1103    NA    TRUE        TRUE
#> 194                                   11      1103    NA    TRUE        TRUE
#> 195                                   11      1124    NA    TRUE        TRUE
#> 196                                   11      1103    NA    TRUE        TRUE
#> 197                                   11      1149    NA   FALSE        TRUE
#> 198                                   11      1149    NA   FALSE        TRUE
#> 199                                   11      1101    NA   FALSE        TRUE
#> 200                                   11      1101    NA    TRUE        TRUE
#> 201                                   11      1101    NA    TRUE        TRUE
#> 202                                   11      1106    NA    TRUE        TRUE
#> 203                                   11      1103    NA    TRUE        TRUE
#> 204                                   11      1103    NA   FALSE        TRUE
#> 205                                   11      1112    NA    TRUE        TRUE
#> 206                                   11      1101    NA   FALSE        TRUE
#> 207                                   11      1103    NA    TRUE        TRUE
#> 208       Avdeling Timoteiveien       11      1103    NA   FALSE        TRUE
#> 209                                   11      1149    NA   FALSE        TRUE
#> 210                                   11      1103    NA   FALSE        TRUE
#> 211                                   11      1103    NA   FALSE        TRUE
#> 212                                   11      1133    NA    TRUE        TRUE
#> 213                                   11      1149    NA    TRUE        TRUE
#> 214                                   11      1106    NA    TRUE        TRUE
#> 215                                   11      1149    NA   FALSE        TRUE
#> 216                                   11      1108    NA    TRUE        TRUE
#> 217                                   11      1120    NA    TRUE        TRUE
#> 218                                   11      1112    NA    TRUE        TRUE
#> 219                                   11      1101    NA   FALSE        TRUE
#> 220                                   11      1122    NA   FALSE        TRUE
#> 221                                   11      1103    NA    TRUE        TRUE
#> 222                                   11      1103    NA    TRUE        TRUE
#> 223                                   11      1124    NA    TRUE        TRUE
#> 224                                   11      1108    NA    TRUE        TRUE
#> 225                                   11      1119    NA   FALSE        TRUE
#> 226                                   11      1108    NA    TRUE        TRUE
#> 227                                   11      1108    NA    TRUE        TRUE
#> 228                                   11      1124    NA    TRUE        TRUE
#> 229                                   11      1149    NA   FALSE        TRUE
#> 230                                   11      1108    NA    TRUE        TRUE
#> 231                                   11      1106    NA   FALSE        TRUE
#> 232                                   11      1103    NA    TRUE        TRUE
#> 233                                   11      1103    NA    TRUE        TRUE
#> 234                                   11      1160    NA   FALSE        TRUE
#> 235                                   11      1103    NA    TRUE        TRUE
#> 236                                   11      1103    NA    TRUE        TRUE
#> 237                                   11      1134    NA    TRUE        TRUE
#> 238                                   11      1101    NA    TRUE        TRUE
#> 239                                   11      1124    NA   FALSE        TRUE
#> 240                                   11      1102    NA   FALSE        TRUE
#> 241                                   11      1121    NA    TRUE        TRUE
#> 242                                   11      1108    NA    TRUE        TRUE
#> 243                                   11      1130    NA    TRUE        TRUE
#> 244                                   11      1133    NA    TRUE        TRUE
#> 245                                   11      1133    NA    TRUE        TRUE
#> 246                                   11      1103    NA    TRUE        TRUE
#> 247                                   11      1103    NA    TRUE        TRUE
#> 248                                   11      1103    NA   FALSE        TRUE
#> 249                                   11      1102    NA   FALSE        TRUE
#> 250                                   11      1149    NA   FALSE        TRUE
#> 251                                   11      1101    NA    TRUE        TRUE
#> 252                                   11      1149    NA    TRUE        TRUE
#> 253                                   11      1134    NA    TRUE        TRUE
#> 254                                   11      1108    NA    TRUE        TRUE
#> 255                                   11      1120    NA    TRUE        TRUE
#> 256                                   11      1120    NA    TRUE        TRUE
#> 257                                   11      1106    NA   FALSE        TRUE
#> 258                                   11      1103    NA    TRUE        TRUE
#> 259                                   11      1103    NA   FALSE        TRUE
#> 260                                   11      1149    NA   FALSE        TRUE
#> 261                                   11      1149    NA   FALSE        TRUE
#> 262                                   11      1103    NA    TRUE        TRUE
#> 263                                   11      1108    NA    TRUE        TRUE
#> 264                                   11      1149    NA   FALSE        TRUE
#> 265                                   11      1122    NA    TRUE        TRUE
#> 266                                   11      1122    NA   FALSE        TRUE
#> 267                                   11      1122    NA   FALSE        TRUE
#> 268                                   11      1149    NA    TRUE        TRUE
#> 269                                   11      1124    NA    TRUE        TRUE
#> 270                                   11      1103    NA    TRUE        TRUE
#> 271                                   11      1108    NA    TRUE        TRUE
#> 272                                   11      1108    NA    TRUE        TRUE
#> 273                                   11      1108    NA   FALSE        TRUE
#> 274                                   11      1108    NA    TRUE        TRUE
#> 275                                   11      1108    NA   FALSE        TRUE
#> 276                                   11      1102    NA   FALSE        TRUE
#> 277                                   11      1106    NA    TRUE        TRUE
#> 278                                   11      1149    NA   FALSE        TRUE
#> 279                                   11      1103    NA    TRUE        TRUE
#> 280                                   11      1121    NA    TRUE        TRUE
#> 281                                   11      1103    NA    TRUE        TRUE
#> 282                                   11      1103    NA   FALSE        TRUE
#> 283                                   11      1144    NA    TRUE        TRUE
#> 284                                   11      1149    NA   FALSE        TRUE
#> 285                                   11      1106    NA    TRUE        TRUE
#> 286                                   11      1103    NA   FALSE        TRUE
#> 287                                   11      1119    NA    TRUE        TRUE
#> 288                                   11      1108    NA    TRUE        TRUE
#> 289                                   11      1103    NA   FALSE        TRUE
#> 290                                   11      1103    NA   FALSE        TRUE
#> 291                                   11      1103    NA    TRUE        TRUE
#> 292                                   11      1135    NA    TRUE        TRUE
#> 293                                   11      1108    NA    TRUE        TRUE
#> 294                                   11      1119    NA    TRUE        TRUE
#> 295                                   11      1120    NA   FALSE        TRUE
#> 296                                   11      1103    NA    TRUE        TRUE
#> 297                                   11      1106    NA   FALSE        TRUE
#> 298                                   11      1149    NA   FALSE        TRUE
#> 299                                   11      1119    NA   FALSE        TRUE
#> 300                                   11      1106    NA   FALSE        TRUE
#> 301                                   11      1108    NA    TRUE        TRUE
#> 302                                   11      1108    NA    TRUE        TRUE
#> 303                                   11      1102    NA   FALSE        TRUE
#> 304                                   11      1121    NA   FALSE        TRUE
#> 305                                   11      1101    NA    TRUE        TRUE
#> 306                                   11      1124    NA   FALSE        TRUE
#> 307                                   11      1120    NA   FALSE        TRUE
#> 308                                   11      1120    NA    TRUE        TRUE
#> 309                                   11      1106    NA    TRUE        TRUE
#> 310                                   11      1108    NA    TRUE        TRUE
#> 311                                   11      1130    NA    TRUE        TRUE
#> 312                                   11      1103    NA    TRUE        TRUE
#> 313                                   11      1111    NA    TRUE        TRUE
#> 314                                   11      1103    NA    TRUE        TRUE
#> 315                                   11      1103    NA    TRUE        TRUE
#> 316                                   11      1108    NA    TRUE        TRUE
#> 317                                   11      1124    NA    TRUE        TRUE
#> 318                                   11      1149    NA    TRUE        TRUE
#> 319                                   11      1103    NA    TRUE        TRUE
#> 320                                   11      1124    NA    TRUE        TRUE
#> 321                                   11      1108    NA    TRUE        TRUE
#> 322                                   11      1108    NA    TRUE        TRUE
#> 323                                   11      1103    NA    TRUE        TRUE
#> 324                                   11      1108    NA    TRUE        TRUE
#> 325                                   11      1149    NA    TRUE        TRUE
#> 326                                   11      1106    NA    TRUE        TRUE
#> 327                                   11      1149    NA   FALSE        TRUE
#> 328                                   11      1103    NA    TRUE        TRUE
#> 329                                   11      1103    NA    TRUE        TRUE
#> 330                                   11      1103    NA    TRUE        TRUE
#> 331                                   11      1103    NA   FALSE        TRUE
#> 332                                   11      1120    NA    TRUE        TRUE
#> 333                                   11      1120    NA   FALSE        TRUE
#> 334                                   11      1149    NA   FALSE        TRUE
#> 335                                   11      1108    NA    TRUE        TRUE
#> 336                                   11      1102    NA   FALSE        TRUE
#> 337                                   11      1103    NA    TRUE        TRUE
#> 338                                   11      1121    NA    TRUE        TRUE
#> 339                                   11      1130    NA   FALSE        TRUE
#> 340                                   11      1103    NA    TRUE        TRUE
#> 341                                   11      1146    NA   FALSE        TRUE
#> 342                                   11      1103    NA    TRUE        TRUE
#> 343                                   11      1121    NA    TRUE        TRUE
#> 344                                   11      1149    NA   FALSE        TRUE
#> 345                                   11      1103    NA   FALSE        TRUE
#> 346                                   11      1149    NA   FALSE        TRUE
#> 347                                   11      1119    NA    TRUE        TRUE
#> 348                                   11      1119    NA   FALSE        TRUE
#> 349                                   11      1119    NA   FALSE        TRUE
#> 350                                   11      1149    NA   FALSE        TRUE
#> 351                                   11      1103    NA    TRUE        TRUE
#> 352                                   11      1149    NA    TRUE        TRUE
#> 353                                   11      1108    NA    TRUE        TRUE
#> 354                                   11      1120    NA    TRUE        TRUE
#> 355                                   11      1108    NA    TRUE        TRUE
#> 356                                   11      1127    NA    TRUE        TRUE
#> 357                                   11      1119    NA    TRUE        TRUE
#> 358                                   11      1102    NA   FALSE        TRUE
#> 359                                   11      1160    NA    TRUE        TRUE
#> 360                                   11      1106    NA    TRUE        TRUE
#> 361                                   11      1146    NA    TRUE        TRUE
#> 362                                   11      1134    NA    TRUE        TRUE
#> 363                                   11      1122    NA    TRUE        TRUE
#> 364                                   11      1149    NA   FALSE        TRUE
#> 365                                   11      1119    NA    TRUE        TRUE
#> 366                                   11      1149    NA   FALSE        TRUE
#> 367                                   11      1124    NA   FALSE        TRUE
#> 368                                   11      1121    NA   FALSE        TRUE
#> 369                                   11      1121    NA   FALSE        TRUE
#> 370                                   11      1103    NA    TRUE        TRUE
#> 371                                   11      1127    NA    TRUE        TRUE
#> 372                                   11      1119    NA   FALSE        TRUE
#> 373                                   11      1106    NA    TRUE        TRUE
#> 374                                   11      1106    NA    TRUE        TRUE
#> 375                                   11      1106    NA    TRUE        TRUE
#> 376                                   11      1106    NA    TRUE        TRUE
#> 377                                   11      1122    NA    TRUE        TRUE
#> 378                                   11      1103    NA    TRUE        TRUE
#> 379                                   11      1106    NA    TRUE        TRUE
#> 380                                   11      1120    NA   FALSE        TRUE
#> 381                                   11      1108    NA    TRUE        TRUE
#> 382                                   11      1120    NA    TRUE        TRUE
#> 383                                   11      1120    NA    TRUE        TRUE
#> 384                                   11      1119    NA    TRUE        TRUE
#> 385                                   11      1108    NA    TRUE        TRUE
#> 386                                   11      1121    NA    TRUE        TRUE
#> 387                                   11      1103    NA    TRUE        TRUE
#> 388                                   11      1103    NA    TRUE        TRUE
#> 389                                   11      1121    NA    TRUE        TRUE
#> 390                                   11      1103    NA    TRUE        TRUE
#> 391                                   11      1103    NA    TRUE        TRUE
#> 392                                   11      1108    NA    TRUE        TRUE
#> 393                                   11      1130    NA    TRUE        TRUE
#> 394                                   11      1119    NA    TRUE        TRUE
#> 395                                   11      1106    NA    TRUE        TRUE
#> 396                                   11      1103    NA    TRUE        TRUE
#> 397                                   11      1108    NA    TRUE        TRUE
#> 398                                   11      1108    NA   FALSE        TRUE
#> 399                                   11      1121    NA   FALSE        TRUE
#> 400                                   11      1101    NA    TRUE        TRUE
#> 401                                   11      1103    NA    TRUE        TRUE
#> 402                                   11      1133    NA   FALSE        TRUE
#> 403                                   11      1149    NA   FALSE        TRUE
#> 404                                   11      1103    NA   FALSE        TRUE
#> 405                                   11      1103    NA    TRUE        TRUE
#> 406                                   11      1102    NA   FALSE        TRUE
#> 407                                   11      1122    NA    TRUE        TRUE
#> 408                                   11      1108    NA    TRUE        TRUE
#> 409                                   11      1102    NA   FALSE        TRUE
#> 410                                   11      1106    NA   FALSE        TRUE
#> 411                                   11      1120    NA   FALSE        TRUE
#> 412                                   11      1124    NA    TRUE        TRUE
#> 413                                   11      1108    NA    TRUE        TRUE
#> 414                                   11      1106    NA   FALSE        TRUE
#> 415                                   11      1103    NA    TRUE        TRUE
#> 416                                   11      1101    NA    TRUE        TRUE
#> 417                                   11      1124    NA    TRUE        TRUE
#> 418                                   11      1103    NA   FALSE        TRUE
#> 419                                   11      1103    NA    TRUE        TRUE
#> 420                                   11      1119    NA    TRUE        TRUE
#> 421                                   11      1106    NA    TRUE        TRUE
#> 422                                   11      1106    NA    TRUE        TRUE
#> 423                                   11      1101    NA    TRUE        TRUE
#> 424                                   11      1135    NA    TRUE        TRUE
#> 425                                   11      1149    NA    TRUE        TRUE
#> 426                                   11      1127    NA    TRUE        TRUE
#> 427                                   11      1149    NA    TRUE        TRUE
#> 428                                   11      1103    NA   FALSE        TRUE
#> 429                                   11      1130    NA    TRUE        TRUE
#> 430                                   11      1106    NA    TRUE        TRUE
#> 431                                   11      1124    NA    TRUE        TRUE
#> 432                                   11      1108    NA    TRUE        TRUE
#> 433                                   11      1114    NA    TRUE        TRUE
#> 434                                   11      1108    NA    TRUE        TRUE
#> 435                                   11      1106    NA    TRUE        TRUE
#> 436                                   11      1149    NA   FALSE        TRUE
#> 437                                   11      1106    NA    TRUE        TRUE
#> 438                Avd Randåsen       11      1134    NA   FALSE        TRUE
#> 439                                   11      1134    NA    TRUE        TRUE
#> 440                                   11      1134    NA    TRUE        TRUE
#> 441                                   11      1103    NA    TRUE        TRUE
#> 442                                   11      1127    NA    TRUE        TRUE
#> 443                                   11      1124    NA    TRUE        TRUE
#> 444                                   11      1124    NA   FALSE        TRUE
#> 445                                   11      1160    NA    TRUE        TRUE
#> 446                                   11      1149    NA    TRUE        TRUE
#> 447                                   11      1102    NA   FALSE        TRUE
#> 448                                   11      1108    NA    TRUE        TRUE
#> 449                                   11      1108    NA    TRUE        TRUE
#> 450                                   11      1103    NA    TRUE        TRUE
#> 451                                   11      1103    NA    TRUE        TRUE
#> 452                                   11      1149    NA    TRUE        TRUE
#> 453                                   11      1103    NA    TRUE        TRUE
#> 454                                   11      1108    NA    TRUE        TRUE
#> 455                                   11      1124    NA    TRUE        TRUE
#> 456                                   11      1101    NA    TRUE        TRUE
#> 457                                   11      1103    NA    TRUE        TRUE
#> 458                                   11      1124    NA   FALSE        TRUE
#> 459                                   11      1114    NA   FALSE        TRUE
#> 460                                   11      1160    NA    TRUE        TRUE
#> 461                                   11      1146    NA   FALSE        TRUE
#> 462                                   11      1146    NA    TRUE        TRUE
#> 463                                   11      1149    NA   FALSE        TRUE
#> 464                                   11      1103    NA    TRUE        TRUE
#> 465                                   11      1103    NA    TRUE        TRUE
#> 466                                   11      1149    NA    TRUE        TRUE
#> 467                                   11      1149    NA   FALSE        TRUE
#> 468                                   11      1121    NA   FALSE        TRUE
#> 469                                   11      1106    NA    TRUE        TRUE
#> 470                                   11      1106    NA   FALSE        TRUE
#> 471                                   11      1149    NA    TRUE        TRUE
#> 472                                   11      1101    NA    TRUE        TRUE
#> 473                                   11      1149    NA   FALSE        TRUE
#> 474                                   11      1103    NA    TRUE        TRUE
#> 475                                   11      1108    NA    TRUE        TRUE
#> 476                                   11      1103    NA    TRUE        TRUE
#> 477                                   11      1103    NA    TRUE        TRUE
#> 478                                   11      1103    NA    TRUE        TRUE
#> 479                                   11      1149    NA    TRUE        TRUE
#> 480                                   11      1149    NA    TRUE        TRUE
#> 481                                   11      1106    NA    TRUE        TRUE
#> 482                                   11      1106    NA    TRUE        TRUE
#> 483                                   11      1106    NA    TRUE        TRUE
#> 484                                   11      1106    NA   FALSE        TRUE
#> 485               Avd Brakahaug       11      1106    NA   FALSE        TRUE
#> 486                                   11      1106    NA    TRUE        TRUE
#> 487                                   11      1106    NA    TRUE        TRUE
#> 488                                   11      1106    NA    TRUE        TRUE
#> 489                                   11      1135    NA   FALSE        TRUE
#> 490                                   11      1103    NA   FALSE        TRUE
#> 491                                   11      1149    NA   FALSE        TRUE
#> 492                                   11      1124    NA    TRUE        TRUE
#> 493                                   11      1124    NA    TRUE        TRUE
#> 494                                   11      1106    NA    TRUE        TRUE
#> 495                                   11      1127    NA    TRUE        TRUE
#> 496                                   11      1103    NA    TRUE        TRUE
#> 497                                   11      1149    NA   FALSE        TRUE
#> 498                                   11      1103    NA   FALSE        TRUE
#> 499                                   11      1124    NA    TRUE        TRUE
#> 500                                   11      1124    NA    TRUE        TRUE
#> 501                                   11      1103    NA    TRUE        TRUE
#> 502                                   11      1122    NA    TRUE        TRUE
#> 503                                   11      1108    NA    TRUE        TRUE
#> 504                                   11      1103    NA   FALSE        TRUE
#> 505                                   11      1120    NA    TRUE        TRUE
#> 506                                   11      1121    NA    TRUE        TRUE
#> 507                                   11      1102    NA   FALSE        TRUE
#> 508                                   11      1121    NA    TRUE        TRUE
#> 509                                   11      1106    NA    TRUE        TRUE
#> 510             Avdeling Egenes       11      1103    NA   FALSE        TRUE
#> 511                                   11      1103    NA    TRUE        TRUE
#> 512                                   11      1108    NA    TRUE        TRUE
#> 513                                   11      1108    NA    TRUE        TRUE
#> 514                                   11      1103    NA   FALSE        TRUE
#> 515                                   11      1149    NA    TRUE        TRUE
#> 516                                   11      1119    NA   FALSE        TRUE
#> 517                                   11      1120    NA    TRUE        TRUE
#> 518             Avd Tusenbeinet       11      1120    NA   FALSE        TRUE
#> 519                                   11      1103    NA    TRUE        TRUE
#> 520                                   11      1149    NA    TRUE        TRUE
#> 521                                   11      1101    NA   FALSE        TRUE
#> 522                                   11      1106    NA    TRUE        TRUE
#> 523                                   11      1149    NA   FALSE        TRUE
#> 524                                   11      1124    NA    TRUE        TRUE
#> 525                                   11      1124    NA    TRUE        TRUE
#> 526                                   11      1108    NA    TRUE        TRUE
#> 527                                   11      1149    NA    TRUE        TRUE
#> 528                                   11      1106    NA    TRUE        TRUE
#> 529                                   11      1103    NA    TRUE        TRUE
#> 530                                   11      1103    NA    TRUE        TRUE
#> 531             Astmabarnehagen       11      1103    NA   FALSE        TRUE
#> 532                                   11      1103    NA    TRUE        TRUE
#> 533                                   11      1103    NA    TRUE        TRUE
#> 534                                   11      1119    NA    TRUE        TRUE
#> 535                                   11      1103    NA    TRUE        TRUE
#> 536                                   11      1149    NA   FALSE        TRUE
#> 537                                   11      1114    NA    TRUE        TRUE
#> 538                                   11      1149    NA    TRUE        TRUE
#> 539                                   11      1120    NA    TRUE        TRUE
#> 540                                   11      1103    NA    TRUE        TRUE
#> 541                                   11      1103    NA    TRUE        TRUE
#> 542                                   11      1146    NA   FALSE        TRUE
#> 543                                   11      1141    NA   FALSE        TRUE
#> 544                                   11      1134    NA    TRUE        TRUE
#> 545               Styrka Tilbod       11      1134    NA   FALSE        TRUE
#> 546 Avd Barnehageadministrasjon       11      1134    NA   FALSE        TRUE
#> 547                                   11      1134    NA    TRUE        TRUE
#> 548                                   11      1149    NA   FALSE        TRUE
#> 549                                   11      1103    NA    TRUE        TRUE
#> 550                                   11      1103    NA    TRUE        TRUE
#> 551                                   11      1108    NA    TRUE        TRUE
#> 552                                   11      1108    NA    TRUE        TRUE
#> 553                                   11      1106    NA   FALSE        TRUE
#> 554                                   11      1124    NA    TRUE        TRUE
#> 555                                   11      1108    NA    TRUE        TRUE
#> 556                                   11      1108    NA    TRUE        TRUE
#> 557                                   11      1120    NA    TRUE        TRUE
#> 558                                   11      1103    NA    TRUE        TRUE
#> 559                                   11      1103    NA   FALSE        TRUE
#> 560                                   11      1124    NA    TRUE        TRUE
#> 561                                   11      1124    NA    TRUE        TRUE
#> 562                                   11      1103    NA    TRUE        TRUE
#> 563                                   11      1124    NA   FALSE        TRUE
#> 564                                   11      1124    NA    TRUE        TRUE
#> 565                                   11      1149    NA   FALSE        TRUE
#> 566                                   11      1103    NA    TRUE        TRUE
#> 567                                   11      1124    NA    TRUE        TRUE
#> 568                                   11      1103    NA    TRUE        TRUE
#> 569                                   11      1124    NA   FALSE        TRUE
#> 570                                   11      1119    NA    TRUE        TRUE
#> 571                                   11      1103    NA    TRUE        TRUE
#> 572                                   11      1103    NA    TRUE        TRUE
#> 573                                   11      1124    NA    TRUE        TRUE
#> 574                                   11      1149    NA   FALSE        TRUE
#> 575                                   11      1120    NA    TRUE        TRUE
#> 576                                   11      1103    NA    TRUE        TRUE
#> 577                                   11      1106    NA   FALSE        TRUE
#> 578                                   11      1103    NA   FALSE        TRUE
#> 579                                   11      1103    NA    TRUE        TRUE
#> 580                                   11      1149    NA   FALSE        TRUE
#> 581                                   11      1106    NA   FALSE        TRUE
#> 582                                   11      1119    NA    TRUE        TRUE
#> 583              Avdeling Veden       11      1103    NA   FALSE        TRUE
#> 584                                   11      1108    NA    TRUE        TRUE
#> 585                                   11      1108    NA    TRUE        TRUE
#> 586                                   11      1124    NA   FALSE        TRUE
#> 587                                   11      1121    NA    TRUE        TRUE
#> 588                                   11      1103    NA   FALSE        TRUE
#> 589                                   11      1122    NA   FALSE        TRUE
#> 590                                   11      1108    NA    TRUE        TRUE
#> 591                                   11      1149    NA   FALSE        TRUE
#> 592                                   11      1103    NA   FALSE        TRUE
#> 593                                   11      1120    NA   FALSE        TRUE
#> 594                                   11      1101    NA   FALSE        TRUE
#> 595                                   11      1103    NA   FALSE        TRUE
#> 596                                   11      1103    NA    TRUE        TRUE
#> 597                                   11      1146    NA    TRUE        TRUE
#> 598                                   11      1106    NA    TRUE        TRUE
#> 599                                   11      1103    NA   FALSE        TRUE
#> 600                                   11      1103    NA    TRUE        TRUE
#> 601                                   11      1121    NA    TRUE        TRUE
#> 602                                   11      1119    NA    TRUE        TRUE
#> 603                                   11      1108    NA    TRUE        TRUE
#> 604                                   11      1151    NA    TRUE        TRUE
#> 605                                   11      1149    NA   FALSE        TRUE
#> 606                                   11      1103    NA    TRUE        TRUE
#> 607                                   11      1149    NA   FALSE        TRUE
#> 608                                   11      1149    NA   FALSE        TRUE
#> 609                                   11      1134    NA    TRUE        TRUE
#> 610                                   11      1108    NA    TRUE        TRUE
#> 611                                   11      1108    NA    TRUE        TRUE
#> 612                                   11      1103    NA    TRUE        TRUE
#> 613                                   11      1101    NA    TRUE        TRUE
#> 614                                   11      1103    NA    TRUE        TRUE
#> 615                                   11      1119    NA    TRUE        TRUE
#> 616                                   11      1106    NA    TRUE        TRUE
#> 617                                   11      1120    NA   FALSE        TRUE
#> 618                                   11      1120    NA    TRUE        TRUE
#> 619                                   11      1103    NA    TRUE        TRUE
#> 620                                   11      1108    NA    TRUE        TRUE
#> 621                                   11      1160    NA   FALSE        TRUE
#> 622                                   11      1149    NA    TRUE        TRUE
#> 623                                   11      1103    NA    TRUE        TRUE
#> 624                                   11      1149    NA   FALSE        TRUE
#> 625                                   11      1135    NA    TRUE        TRUE
#> 626                                   11      1121    NA   FALSE        TRUE
#> 627                                   11      1121    NA    TRUE        TRUE
#> 628                                   11      1103    NA    TRUE        TRUE
#> 629                                   11      1119    NA    TRUE        TRUE
#> 630                                   11      1120    NA    TRUE        TRUE
#> 631                                   11      1119    NA    TRUE        TRUE
#> 632                                   11      1119    NA    TRUE        TRUE
#> 633                                   11      1160    NA    TRUE        TRUE
#> 634                                   11      1160    NA   FALSE        TRUE
#> 635                                   11      1160    NA    TRUE        TRUE
#> 636                                   11      1149    NA   FALSE        TRUE
#> 637                                   11      1103    NA    TRUE        TRUE
#> 638                                   11      1101    NA   FALSE        TRUE
#> 639                                   11      1106    NA    TRUE        TRUE
#> 640                                   11      1103    NA   FALSE        TRUE
#> 641                                   11      1134    NA    TRUE        TRUE
#> 642                                   11      1127    NA    TRUE        TRUE
#> 643                                   11      1127    NA    TRUE        TRUE
#> 644                                   11      1149    NA    TRUE        TRUE
#> 645            Avd Myklandvegen       11      1149    NA   FALSE        TRUE
#> 646                                   11      1149    NA   FALSE        TRUE
#> 647                                   11      1149    NA   FALSE        TRUE
#> 648                                   11      1133    NA   FALSE        TRUE
#> 649                                   11      1103    NA    TRUE        TRUE
#> 650                                   11      1103    NA    TRUE        TRUE
#> 651                                   11      1160    NA    TRUE        TRUE
#> 652                                   11      1103    NA   FALSE        TRUE
#> 653                                   11      1103    NA    TRUE        TRUE
#> 654                                   11      1103    NA    TRUE        TRUE
#> 655                                   11      1127    NA    TRUE        TRUE
#> 656                                   11      1127    NA   FALSE        TRUE
#> 657                                   11      1120    NA   FALSE        TRUE
#> 658                                   11      1160    NA    TRUE        TRUE
#> 659                                   11      1103    NA    TRUE        TRUE
#> 660                                   11      1103    NA    TRUE        TRUE
#> 661                                   11      1149    NA   FALSE        TRUE
#> 662                                   11      1108    NA    TRUE        TRUE
#> 663                                   11      1122    NA    TRUE        TRUE
#> 664                                   11      1111    NA    TRUE        TRUE
#> 665                                   11      1121    NA    TRUE        TRUE
#> 666                                   11      1149    NA   FALSE        TRUE
#> 667                                   11      1111    NA   FALSE        TRUE
#> 668                                   11      1130    NA   FALSE        TRUE
#> 669                                   11      1106    NA    TRUE        TRUE
#> 670                                   11      1133    NA    TRUE        TRUE
#> 671                                   11      1149    NA    TRUE        TRUE
#>     ErBarnehageeier ErOffentligBarnehage ErPrivatBarnehage ErInaktivIBasil
#> 1             FALSE                FALSE              TRUE            TRUE
#> 2             FALSE                 TRUE             FALSE           FALSE
#> 3             FALSE                FALSE              TRUE           FALSE
#> 4             FALSE                FALSE              TRUE           FALSE
#> 5             FALSE                FALSE              TRUE           FALSE
#> 6             FALSE                 TRUE             FALSE           FALSE
#> 7             FALSE                FALSE              TRUE           FALSE
#> 8             FALSE                FALSE              TRUE            TRUE
#> 9             FALSE                FALSE              TRUE           FALSE
#> 10            FALSE                 TRUE             FALSE            TRUE
#> 11            FALSE                FALSE              TRUE            TRUE
#> 12            FALSE                 TRUE             FALSE           FALSE
#> 13            FALSE                FALSE              TRUE           FALSE
#> 14            FALSE                 TRUE             FALSE           FALSE
#> 15            FALSE                 TRUE             FALSE            TRUE
#> 16            FALSE                 TRUE             FALSE           FALSE
#> 17            FALSE                 TRUE             FALSE           FALSE
#> 18            FALSE                FALSE              TRUE            TRUE
#> 19            FALSE                FALSE              TRUE            TRUE
#> 20            FALSE                 TRUE             FALSE           FALSE
#> 21            FALSE                FALSE              TRUE           FALSE
#> 22            FALSE                 TRUE             FALSE           FALSE
#> 23            FALSE                 TRUE             FALSE            TRUE
#> 24            FALSE                 TRUE             FALSE           FALSE
#> 25            FALSE                 TRUE             FALSE            TRUE
#> 26            FALSE                 TRUE             FALSE            TRUE
#> 27            FALSE                FALSE              TRUE           FALSE
#> 28            FALSE                FALSE              TRUE            TRUE
#> 29            FALSE                FALSE              TRUE            TRUE
#> 30            FALSE                FALSE              TRUE           FALSE
#> 31            FALSE                FALSE              TRUE            TRUE
#> 32            FALSE                FALSE              TRUE           FALSE
#> 33            FALSE                 TRUE             FALSE           FALSE
#> 34            FALSE                 TRUE             FALSE            TRUE
#> 35            FALSE                 TRUE             FALSE           FALSE
#> 36            FALSE                FALSE              TRUE           FALSE
#> 37            FALSE                FALSE              TRUE           FALSE
#> 38            FALSE                FALSE              TRUE           FALSE
#> 39            FALSE                FALSE              TRUE            TRUE
#> 40            FALSE                 TRUE             FALSE           FALSE
#> 41            FALSE                 TRUE             FALSE            TRUE
#> 42            FALSE                 TRUE             FALSE           FALSE
#> 43            FALSE                 TRUE             FALSE           FALSE
#> 44            FALSE                FALSE              TRUE            TRUE
#> 45            FALSE                 TRUE             FALSE           FALSE
#> 46            FALSE                FALSE              TRUE           FALSE
#> 47            FALSE                FALSE              TRUE           FALSE
#> 48            FALSE                FALSE              TRUE           FALSE
#> 49            FALSE                 TRUE             FALSE           FALSE
#> 50            FALSE                 TRUE             FALSE           FALSE
#> 51            FALSE                 TRUE             FALSE            TRUE
#> 52            FALSE                 TRUE             FALSE           FALSE
#> 53            FALSE                FALSE              TRUE            TRUE
#> 54            FALSE                FALSE              TRUE           FALSE
#> 55            FALSE                 TRUE             FALSE           FALSE
#> 56            FALSE                 TRUE             FALSE           FALSE
#> 57            FALSE                 TRUE             FALSE           FALSE
#> 58            FALSE                FALSE              TRUE           FALSE
#> 59            FALSE                 TRUE             FALSE            TRUE
#> 60            FALSE                FALSE              TRUE            TRUE
#> 61            FALSE                 TRUE             FALSE           FALSE
#> 62            FALSE                 TRUE             FALSE            TRUE
#> 63            FALSE                FALSE              TRUE           FALSE
#> 64            FALSE                 TRUE             FALSE           FALSE
#> 65            FALSE                FALSE              TRUE           FALSE
#> 66            FALSE                FALSE              TRUE            TRUE
#> 67            FALSE                 TRUE             FALSE           FALSE
#> 68            FALSE                FALSE              TRUE           FALSE
#> 69            FALSE                 TRUE             FALSE            TRUE
#> 70            FALSE                FALSE              TRUE           FALSE
#> 71            FALSE                FALSE              TRUE           FALSE
#> 72            FALSE                FALSE              TRUE            TRUE
#> 73            FALSE                FALSE              TRUE            TRUE
#> 74            FALSE                FALSE              TRUE           FALSE
#> 75            FALSE                FALSE              TRUE            TRUE
#> 76            FALSE                 TRUE             FALSE            TRUE
#> 77            FALSE                 TRUE             FALSE            TRUE
#> 78            FALSE                FALSE              TRUE           FALSE
#> 79            FALSE                FALSE              TRUE            TRUE
#> 80            FALSE                FALSE              TRUE           FALSE
#> 81            FALSE                 TRUE             FALSE            TRUE
#> 82            FALSE                FALSE              TRUE            TRUE
#> 83            FALSE                FALSE              TRUE           FALSE
#> 84            FALSE                FALSE              TRUE            TRUE
#> 85            FALSE                FALSE              TRUE           FALSE
#> 86            FALSE                FALSE              TRUE           FALSE
#> 87            FALSE                 TRUE             FALSE           FALSE
#> 88            FALSE                 TRUE             FALSE           FALSE
#> 89            FALSE                FALSE              TRUE           FALSE
#> 90            FALSE                FALSE              TRUE           FALSE
#> 91            FALSE                 TRUE             FALSE           FALSE
#> 92            FALSE                FALSE              TRUE           FALSE
#> 93            FALSE                FALSE              TRUE            TRUE
#> 94            FALSE                 TRUE             FALSE           FALSE
#> 95            FALSE                FALSE              TRUE            TRUE
#> 96            FALSE                 TRUE             FALSE           FALSE
#> 97            FALSE                FALSE              TRUE           FALSE
#> 98            FALSE                FALSE              TRUE            TRUE
#> 99            FALSE                FALSE              TRUE           FALSE
#> 100           FALSE                FALSE              TRUE           FALSE
#> 101           FALSE                FALSE              TRUE            TRUE
#> 102           FALSE                FALSE              TRUE           FALSE
#> 103           FALSE                FALSE              TRUE           FALSE
#> 104           FALSE                FALSE              TRUE           FALSE
#> 105           FALSE                FALSE              TRUE           FALSE
#> 106           FALSE                FALSE              TRUE           FALSE
#> 107           FALSE                FALSE              TRUE           FALSE
#> 108           FALSE                FALSE              TRUE            TRUE
#> 109           FALSE                FALSE              TRUE           FALSE
#> 110           FALSE                FALSE              TRUE           FALSE
#> 111           FALSE                FALSE              TRUE           FALSE
#> 112           FALSE                FALSE              TRUE           FALSE
#> 113           FALSE                FALSE              TRUE           FALSE
#> 114           FALSE                FALSE              TRUE           FALSE
#> 115           FALSE                FALSE              TRUE           FALSE
#> 116           FALSE                FALSE              TRUE           FALSE
#> 117           FALSE                FALSE              TRUE           FALSE
#> 118           FALSE                FALSE              TRUE           FALSE
#> 119           FALSE                FALSE              TRUE           FALSE
#> 120           FALSE                FALSE              TRUE           FALSE
#> 121           FALSE                FALSE              TRUE           FALSE
#> 122           FALSE                FALSE              TRUE           FALSE
#> 123           FALSE                FALSE              TRUE           FALSE
#> 124           FALSE                FALSE              TRUE           FALSE
#> 125           FALSE                FALSE              TRUE           FALSE
#> 126           FALSE                FALSE              TRUE            TRUE
#> 127           FALSE                FALSE              TRUE            TRUE
#> 128           FALSE                FALSE              TRUE           FALSE
#> 129           FALSE                FALSE              TRUE           FALSE
#> 130           FALSE                FALSE              TRUE           FALSE
#> 131           FALSE                FALSE              TRUE            TRUE
#> 132           FALSE                 TRUE             FALSE           FALSE
#> 133           FALSE                 TRUE             FALSE           FALSE
#> 134           FALSE                FALSE              TRUE            TRUE
#> 135           FALSE                FALSE              TRUE           FALSE
#> 136           FALSE                 TRUE             FALSE           FALSE
#> 137           FALSE                FALSE              TRUE            TRUE
#> 138           FALSE                 TRUE             FALSE           FALSE
#> 139           FALSE                 TRUE             FALSE           FALSE
#> 140           FALSE                FALSE              TRUE           FALSE
#> 141           FALSE                FALSE              TRUE           FALSE
#> 142           FALSE                 TRUE             FALSE           FALSE
#> 143           FALSE                 TRUE             FALSE           FALSE
#> 144           FALSE                 TRUE             FALSE           FALSE
#> 145           FALSE                FALSE              TRUE           FALSE
#> 146           FALSE                 TRUE             FALSE            TRUE
#> 147           FALSE                FALSE              TRUE            TRUE
#> 148           FALSE                 TRUE             FALSE            TRUE
#> 149           FALSE                FALSE              TRUE           FALSE
#> 150           FALSE                FALSE              TRUE            TRUE
#> 151           FALSE                FALSE              TRUE            TRUE
#> 152           FALSE                FALSE              TRUE            TRUE
#> 153           FALSE                 TRUE             FALSE           FALSE
#> 154           FALSE                 TRUE             FALSE           FALSE
#> 155           FALSE                 TRUE             FALSE           FALSE
#> 156           FALSE                 TRUE             FALSE           FALSE
#> 157           FALSE                 TRUE             FALSE           FALSE
#> 158           FALSE                FALSE              TRUE           FALSE
#> 159           FALSE                FALSE              TRUE            TRUE
#> 160           FALSE                FALSE              TRUE           FALSE
#> 161           FALSE                 TRUE             FALSE           FALSE
#> 162           FALSE                FALSE              TRUE           FALSE
#> 163           FALSE                 TRUE             FALSE           FALSE
#> 164           FALSE                FALSE              TRUE            TRUE
#> 165           FALSE                 TRUE             FALSE            TRUE
#> 166           FALSE                FALSE              TRUE            TRUE
#> 167           FALSE                 TRUE             FALSE           FALSE
#> 168           FALSE                 TRUE             FALSE           FALSE
#> 169           FALSE                 TRUE             FALSE           FALSE
#> 170           FALSE                FALSE              TRUE           FALSE
#> 171           FALSE                 TRUE             FALSE            TRUE
#> 172           FALSE                 TRUE             FALSE           FALSE
#> 173           FALSE                FALSE              TRUE            TRUE
#> 174           FALSE                 TRUE             FALSE           FALSE
#> 175           FALSE                FALSE              TRUE            TRUE
#> 176           FALSE                FALSE              TRUE            TRUE
#> 177           FALSE                FALSE              TRUE            TRUE
#> 178           FALSE                 TRUE             FALSE           FALSE
#> 179           FALSE                 TRUE             FALSE           FALSE
#> 180           FALSE                FALSE              TRUE           FALSE
#> 181           FALSE                FALSE              TRUE            TRUE
#> 182           FALSE                FALSE              TRUE           FALSE
#> 183           FALSE                FALSE              TRUE           FALSE
#> 184           FALSE                 TRUE             FALSE           FALSE
#> 185           FALSE                 TRUE             FALSE            TRUE
#> 186           FALSE                FALSE              TRUE           FALSE
#> 187           FALSE                 TRUE             FALSE            TRUE
#> 188           FALSE                FALSE              TRUE           FALSE
#> 189           FALSE                FALSE              TRUE           FALSE
#> 190           FALSE                FALSE              TRUE           FALSE
#> 191           FALSE                FALSE              TRUE           FALSE
#> 192           FALSE                 TRUE             FALSE           FALSE
#> 193           FALSE                 TRUE             FALSE           FALSE
#> 194           FALSE                 TRUE             FALSE           FALSE
#> 195           FALSE                 TRUE             FALSE           FALSE
#> 196           FALSE                FALSE              TRUE           FALSE
#> 197           FALSE                FALSE              TRUE            TRUE
#> 198           FALSE                FALSE              TRUE            TRUE
#> 199           FALSE                 TRUE             FALSE            TRUE
#> 200           FALSE                FALSE              TRUE           FALSE
#> 201           FALSE                 TRUE             FALSE           FALSE
#> 202           FALSE                 TRUE             FALSE           FALSE
#> 203           FALSE                 TRUE             FALSE           FALSE
#> 204           FALSE                 TRUE             FALSE            TRUE
#> 205           FALSE                 TRUE             FALSE           FALSE
#> 206           FALSE                FALSE              TRUE            TRUE
#> 207           FALSE                 TRUE             FALSE           FALSE
#> 208           FALSE                 TRUE             FALSE            TRUE
#> 209           FALSE                 TRUE             FALSE            TRUE
#> 210           FALSE                 TRUE             FALSE            TRUE
#> 211           FALSE                 TRUE             FALSE            TRUE
#> 212           FALSE                 TRUE             FALSE           FALSE
#> 213           FALSE                FALSE              TRUE            TRUE
#> 214           FALSE                FALSE              TRUE           FALSE
#> 215           FALSE                FALSE              TRUE            TRUE
#> 216           FALSE                 TRUE             FALSE           FALSE
#> 217           FALSE                 TRUE             FALSE            TRUE
#> 218           FALSE                 TRUE             FALSE           FALSE
#> 219           FALSE                 TRUE             FALSE            TRUE
#> 220           FALSE                 TRUE             FALSE            TRUE
#> 221           FALSE                 TRUE             FALSE           FALSE
#> 222           FALSE                FALSE              TRUE           FALSE
#> 223           FALSE                 TRUE             FALSE           FALSE
#> 224           FALSE                 TRUE             FALSE           FALSE
#> 225           FALSE                 TRUE             FALSE            TRUE
#> 226           FALSE                FALSE              TRUE           FALSE
#> 227           FALSE                FALSE              TRUE           FALSE
#> 228           FALSE                FALSE              TRUE           FALSE
#> 229           FALSE                 TRUE             FALSE            TRUE
#> 230           FALSE                FALSE              TRUE           FALSE
#> 231           FALSE                FALSE              TRUE            TRUE
#> 232           FALSE                FALSE              TRUE            TRUE
#> 233           FALSE                FALSE              TRUE           FALSE
#> 234           FALSE                 TRUE             FALSE           FALSE
#> 235           FALSE                 TRUE             FALSE           FALSE
#> 236           FALSE                FALSE              TRUE           FALSE
#> 237           FALSE                 TRUE             FALSE           FALSE
#> 238           FALSE                 TRUE             FALSE           FALSE
#> 239           FALSE                FALSE              TRUE            TRUE
#> 240           FALSE                FALSE              TRUE            TRUE
#> 241           FALSE                FALSE              TRUE           FALSE
#> 242           FALSE                 TRUE             FALSE           FALSE
#> 243           FALSE                 TRUE             FALSE           FALSE
#> 244           FALSE                 TRUE             FALSE           FALSE
#> 245           FALSE                 TRUE             FALSE            TRUE
#> 246           FALSE                 TRUE             FALSE           FALSE
#> 247           FALSE                 TRUE             FALSE           FALSE
#> 248           FALSE                FALSE              TRUE            TRUE
#> 249           FALSE                FALSE              TRUE            TRUE
#> 250           FALSE                FALSE              TRUE            TRUE
#> 251           FALSE                FALSE              TRUE           FALSE
#> 252           FALSE                FALSE              TRUE           FALSE
#> 253           FALSE                 TRUE             FALSE           FALSE
#> 254           FALSE                 TRUE             FALSE           FALSE
#> 255           FALSE                 TRUE             FALSE           FALSE
#> 256           FALSE                FALSE              TRUE           FALSE
#> 257           FALSE                FALSE              TRUE            TRUE
#> 258           FALSE                 TRUE             FALSE           FALSE
#> 259           FALSE                FALSE              TRUE            TRUE
#> 260           FALSE                FALSE              TRUE            TRUE
#> 261           FALSE                FALSE              TRUE            TRUE
#> 262           FALSE                FALSE              TRUE            TRUE
#> 263           FALSE                FALSE              TRUE           FALSE
#> 264           FALSE                FALSE              TRUE            TRUE
#> 265           FALSE                 TRUE             FALSE           FALSE
#> 266           FALSE                FALSE              TRUE            TRUE
#> 267           FALSE                FALSE              TRUE            TRUE
#> 268           FALSE                 TRUE             FALSE           FALSE
#> 269           FALSE                FALSE              TRUE           FALSE
#> 270           FALSE                 TRUE             FALSE           FALSE
#> 271           FALSE                FALSE              TRUE            TRUE
#> 272           FALSE                FALSE              TRUE           FALSE
#> 273           FALSE                FALSE              TRUE            TRUE
#> 274           FALSE                FALSE              TRUE           FALSE
#> 275           FALSE                FALSE              TRUE            TRUE
#> 276           FALSE                FALSE              TRUE            TRUE
#> 277           FALSE                FALSE              TRUE           FALSE
#> 278           FALSE                FALSE              TRUE            TRUE
#> 279           FALSE                 TRUE             FALSE           FALSE
#> 280           FALSE                 TRUE             FALSE           FALSE
#> 281           FALSE                 TRUE             FALSE           FALSE
#> 282           FALSE                 TRUE             FALSE            TRUE
#> 283           FALSE                 TRUE             FALSE           FALSE
#> 284           FALSE                FALSE              TRUE            TRUE
#> 285           FALSE                FALSE              TRUE           FALSE
#> 286           FALSE                 TRUE             FALSE            TRUE
#> 287           FALSE                FALSE              TRUE            TRUE
#> 288           FALSE                 TRUE             FALSE           FALSE
#> 289           FALSE                FALSE              TRUE            TRUE
#> 290           FALSE                FALSE              TRUE            TRUE
#> 291           FALSE                 TRUE             FALSE           FALSE
#> 292           FALSE                 TRUE             FALSE           FALSE
#> 293           FALSE                FALSE              TRUE           FALSE
#> 294           FALSE                 TRUE             FALSE           FALSE
#> 295           FALSE                FALSE              TRUE            TRUE
#> 296           FALSE                FALSE              TRUE           FALSE
#> 297           FALSE                FALSE              TRUE           FALSE
#> 298           FALSE                FALSE              TRUE            TRUE
#> 299           FALSE                 TRUE             FALSE            TRUE
#> 300           FALSE                FALSE              TRUE            TRUE
#> 301           FALSE                FALSE              TRUE           FALSE
#> 302           FALSE                FALSE              TRUE           FALSE
#> 303           FALSE                FALSE              TRUE            TRUE
#> 304           FALSE                 TRUE             FALSE           FALSE
#> 305           FALSE                 TRUE             FALSE           FALSE
#> 306           FALSE                FALSE              TRUE            TRUE
#> 307           FALSE                FALSE              TRUE            TRUE
#> 308           FALSE                 TRUE             FALSE           FALSE
#> 309           FALSE                 TRUE             FALSE           FALSE
#> 310           FALSE                FALSE              TRUE           FALSE
#> 311           FALSE                FALSE              TRUE           FALSE
#> 312           FALSE                FALSE              TRUE           FALSE
#> 313           FALSE                FALSE              TRUE           FALSE
#> 314           FALSE                FALSE              TRUE           FALSE
#> 315           FALSE                FALSE              TRUE           FALSE
#> 316           FALSE                FALSE              TRUE           FALSE
#> 317           FALSE                FALSE              TRUE           FALSE
#> 318           FALSE                FALSE              TRUE           FALSE
#> 319           FALSE                FALSE              TRUE           FALSE
#> 320           FALSE                FALSE              TRUE           FALSE
#> 321           FALSE                FALSE              TRUE           FALSE
#> 322           FALSE                FALSE              TRUE           FALSE
#> 323           FALSE                FALSE              TRUE           FALSE
#> 324           FALSE                FALSE              TRUE           FALSE
#> 325           FALSE                FALSE              TRUE           FALSE
#> 326           FALSE                FALSE              TRUE           FALSE
#> 327           FALSE                FALSE              TRUE            TRUE
#> 328           FALSE                 TRUE             FALSE           FALSE
#> 329           FALSE                 TRUE             FALSE           FALSE
#> 330           FALSE                 TRUE             FALSE           FALSE
#> 331           FALSE                FALSE              TRUE            TRUE
#> 332           FALSE                 TRUE             FALSE           FALSE
#> 333           FALSE                FALSE              TRUE            TRUE
#> 334           FALSE                FALSE              TRUE            TRUE
#> 335           FALSE                FALSE              TRUE           FALSE
#> 336           FALSE                FALSE              TRUE            TRUE
#> 337           FALSE                 TRUE             FALSE            TRUE
#> 338           FALSE                 TRUE             FALSE           FALSE
#> 339           FALSE                FALSE              TRUE            TRUE
#> 340           FALSE                FALSE              TRUE           FALSE
#> 341           FALSE                FALSE              TRUE            TRUE
#> 342           FALSE                 TRUE             FALSE           FALSE
#> 343           FALSE                 TRUE             FALSE           FALSE
#> 344           FALSE                FALSE              TRUE            TRUE
#> 345           FALSE                 TRUE             FALSE            TRUE
#> 346           FALSE                FALSE              TRUE            TRUE
#> 347           FALSE                 TRUE             FALSE           FALSE
#> 348           FALSE                FALSE              TRUE            TRUE
#> 349           FALSE                FALSE              TRUE            TRUE
#> 350           FALSE                 TRUE             FALSE            TRUE
#> 351           FALSE                FALSE              TRUE           FALSE
#> 352           FALSE                 TRUE             FALSE           FALSE
#> 353           FALSE                 TRUE             FALSE           FALSE
#> 354           FALSE                 TRUE             FALSE            TRUE
#> 355           FALSE                FALSE              TRUE           FALSE
#> 356           FALSE                 TRUE             FALSE           FALSE
#> 357           FALSE                 TRUE             FALSE           FALSE
#> 358           FALSE                FALSE              TRUE            TRUE
#> 359           FALSE                FALSE              TRUE           FALSE
#> 360           FALSE                FALSE              TRUE           FALSE
#> 361           FALSE                 TRUE             FALSE           FALSE
#> 362           FALSE                 TRUE             FALSE           FALSE
#> 363           FALSE                 TRUE             FALSE           FALSE
#> 364           FALSE                FALSE              TRUE            TRUE
#> 365           FALSE                FALSE              TRUE           FALSE
#> 366           FALSE                FALSE              TRUE            TRUE
#> 367           FALSE                FALSE              TRUE            TRUE
#> 368           FALSE                FALSE              TRUE            TRUE
#> 369           FALSE                 TRUE             FALSE            TRUE
#> 370           FALSE                 TRUE             FALSE           FALSE
#> 371           FALSE                FALSE              TRUE           FALSE
#> 372           FALSE                 TRUE             FALSE            TRUE
#> 373           FALSE                FALSE              TRUE           FALSE
#> 374           FALSE                FALSE              TRUE           FALSE
#> 375           FALSE                FALSE              TRUE           FALSE
#> 376           FALSE                FALSE              TRUE           FALSE
#> 377           FALSE                 TRUE             FALSE           FALSE
#> 378           FALSE                 TRUE             FALSE           FALSE
#> 379           FALSE                FALSE              TRUE            TRUE
#> 380           FALSE                FALSE              TRUE            TRUE
#> 381           FALSE                FALSE              TRUE           FALSE
#> 382           FALSE                 TRUE             FALSE           FALSE
#> 383           FALSE                 TRUE             FALSE           FALSE
#> 384           FALSE                 TRUE             FALSE           FALSE
#> 385           FALSE                 TRUE             FALSE           FALSE
#> 386           FALSE                FALSE              TRUE            TRUE
#> 387           FALSE                FALSE              TRUE            TRUE
#> 388           FALSE                FALSE              TRUE           FALSE
#> 389           FALSE                FALSE              TRUE           FALSE
#> 390           FALSE                FALSE              TRUE           FALSE
#> 391           FALSE                FALSE              TRUE           FALSE
#> 392           FALSE                FALSE              TRUE           FALSE
#> 393           FALSE                FALSE              TRUE           FALSE
#> 394           FALSE                FALSE              TRUE           FALSE
#> 395           FALSE                FALSE              TRUE           FALSE
#> 396           FALSE                FALSE              TRUE           FALSE
#> 397           FALSE                 TRUE             FALSE           FALSE
#> 398           FALSE                FALSE              TRUE            TRUE
#> 399           FALSE                 TRUE             FALSE            TRUE
#> 400           FALSE                FALSE              TRUE           FALSE
#> 401           FALSE                 TRUE             FALSE           FALSE
#> 402           FALSE                 TRUE             FALSE            TRUE
#> 403           FALSE                FALSE              TRUE            TRUE
#> 404           FALSE                FALSE              TRUE            TRUE
#> 405           FALSE                 TRUE             FALSE           FALSE
#> 406           FALSE                FALSE              TRUE            TRUE
#> 407           FALSE                FALSE              TRUE           FALSE
#> 408           FALSE                 TRUE             FALSE           FALSE
#> 409           FALSE                FALSE              TRUE            TRUE
#> 410           FALSE                FALSE              TRUE            TRUE
#> 411           FALSE                FALSE              TRUE            TRUE
#> 412           FALSE                 TRUE             FALSE           FALSE
#> 413           FALSE                 TRUE             FALSE           FALSE
#> 414           FALSE                FALSE              TRUE            TRUE
#> 415           FALSE                 TRUE             FALSE            TRUE
#> 416           FALSE                FALSE              TRUE           FALSE
#> 417           FALSE                FALSE              TRUE           FALSE
#> 418           FALSE                FALSE              TRUE            TRUE
#> 419           FALSE                FALSE              TRUE           FALSE
#> 420           FALSE                 TRUE             FALSE           FALSE
#> 421           FALSE                FALSE              TRUE           FALSE
#> 422           FALSE                FALSE              TRUE           FALSE
#> 423           FALSE                 TRUE             FALSE           FALSE
#> 424           FALSE                 TRUE             FALSE           FALSE
#> 425           FALSE                FALSE              TRUE           FALSE
#> 426           FALSE                FALSE              TRUE           FALSE
#> 427           FALSE                FALSE              TRUE           FALSE
#> 428           FALSE                FALSE              TRUE            TRUE
#> 429           FALSE                FALSE              TRUE           FALSE
#> 430           FALSE                 TRUE             FALSE            TRUE
#> 431           FALSE                 TRUE             FALSE           FALSE
#> 432           FALSE                FALSE              TRUE           FALSE
#> 433           FALSE                 TRUE             FALSE           FALSE
#> 434           FALSE                 TRUE             FALSE            TRUE
#> 435           FALSE                 TRUE             FALSE           FALSE
#> 436           FALSE                FALSE              TRUE            TRUE
#> 437           FALSE                FALSE              TRUE           FALSE
#> 438           FALSE                 TRUE             FALSE            TRUE
#> 439           FALSE                 TRUE             FALSE           FALSE
#> 440           FALSE                 TRUE             FALSE           FALSE
#> 441           FALSE                 TRUE             FALSE           FALSE
#> 442           FALSE                 TRUE             FALSE           FALSE
#> 443           FALSE                 TRUE             FALSE           FALSE
#> 444           FALSE                FALSE              TRUE            TRUE
#> 445           FALSE                 TRUE             FALSE           FALSE
#> 446           FALSE                FALSE              TRUE           FALSE
#> 447           FALSE                FALSE              TRUE            TRUE
#> 448           FALSE                 TRUE             FALSE           FALSE
#> 449           FALSE                 TRUE             FALSE           FALSE
#> 450           FALSE                 TRUE             FALSE           FALSE
#> 451           FALSE                 TRUE             FALSE           FALSE
#> 452           FALSE                 TRUE             FALSE           FALSE
#> 453           FALSE                 TRUE             FALSE           FALSE
#> 454           FALSE                FALSE              TRUE           FALSE
#> 455           FALSE                FALSE              TRUE           FALSE
#> 456           FALSE                FALSE              TRUE           FALSE
#> 457           FALSE                 TRUE             FALSE           FALSE
#> 458           FALSE                 TRUE             FALSE            TRUE
#> 459           FALSE                 TRUE             FALSE            TRUE
#> 460           FALSE                 TRUE             FALSE           FALSE
#> 461           FALSE                 TRUE             FALSE            TRUE
#> 462           FALSE                FALSE              TRUE           FALSE
#> 463           FALSE                FALSE              TRUE            TRUE
#> 464           FALSE                 TRUE             FALSE           FALSE
#> 465           FALSE                 TRUE             FALSE           FALSE
#> 466           FALSE                 TRUE             FALSE           FALSE
#> 467           FALSE                 TRUE             FALSE            TRUE
#> 468           FALSE                FALSE              TRUE            TRUE
#> 469           FALSE                FALSE              TRUE           FALSE
#> 470           FALSE                FALSE              TRUE            TRUE
#> 471           FALSE                FALSE              TRUE           FALSE
#> 472           FALSE                 TRUE             FALSE           FALSE
#> 473           FALSE                FALSE              TRUE            TRUE
#> 474           FALSE                 TRUE             FALSE           FALSE
#> 475           FALSE                 TRUE             FALSE           FALSE
#> 476           FALSE                 TRUE             FALSE           FALSE
#> 477           FALSE                 TRUE             FALSE           FALSE
#> 478           FALSE                 TRUE             FALSE           FALSE
#> 479           FALSE                FALSE              TRUE           FALSE
#> 480           FALSE                FALSE              TRUE           FALSE
#> 481           FALSE                FALSE              TRUE            TRUE
#> 482           FALSE                FALSE              TRUE           FALSE
#> 483           FALSE                FALSE              TRUE           FALSE
#> 484           FALSE                FALSE              TRUE            TRUE
#> 485           FALSE                FALSE              TRUE            TRUE
#> 486           FALSE                FALSE              TRUE           FALSE
#> 487           FALSE                FALSE              TRUE           FALSE
#> 488           FALSE                FALSE              TRUE           FALSE
#> 489           FALSE                FALSE              TRUE            TRUE
#> 490           FALSE                FALSE              TRUE            TRUE
#> 491           FALSE                FALSE              TRUE            TRUE
#> 492           FALSE                 TRUE             FALSE           FALSE
#> 493           FALSE                FALSE              TRUE           FALSE
#> 494           FALSE                 TRUE             FALSE           FALSE
#> 495           FALSE                 TRUE             FALSE           FALSE
#> 496           FALSE                FALSE              TRUE           FALSE
#> 497           FALSE                FALSE              TRUE            TRUE
#> 498           FALSE                FALSE              TRUE            TRUE
#> 499           FALSE                 TRUE             FALSE           FALSE
#> 500           FALSE                 TRUE             FALSE           FALSE
#> 501           FALSE                 TRUE             FALSE           FALSE
#> 502           FALSE                 TRUE             FALSE           FALSE
#> 503           FALSE                FALSE              TRUE           FALSE
#> 504           FALSE                 TRUE             FALSE            TRUE
#> 505           FALSE                 TRUE             FALSE           FALSE
#> 506           FALSE                FALSE              TRUE           FALSE
#> 507           FALSE                FALSE              TRUE            TRUE
#> 508           FALSE                FALSE              TRUE           FALSE
#> 509           FALSE                 TRUE             FALSE            TRUE
#> 510           FALSE                 TRUE             FALSE            TRUE
#> 511           FALSE                 TRUE             FALSE           FALSE
#> 512           FALSE                 TRUE             FALSE           FALSE
#> 513           FALSE                 TRUE             FALSE           FALSE
#> 514           FALSE                FALSE              TRUE            TRUE
#> 515           FALSE                FALSE              TRUE           FALSE
#> 516           FALSE                FALSE              TRUE            TRUE
#> 517           FALSE                 TRUE             FALSE            TRUE
#> 518           FALSE                 TRUE             FALSE            TRUE
#> 519           FALSE                 TRUE             FALSE           FALSE
#> 520           FALSE                FALSE              TRUE           FALSE
#> 521           FALSE                FALSE              TRUE            TRUE
#> 522           FALSE                FALSE              TRUE           FALSE
#> 523           FALSE                FALSE              TRUE            TRUE
#> 524           FALSE                FALSE              TRUE           FALSE
#> 525           FALSE                FALSE              TRUE           FALSE
#> 526           FALSE                FALSE              TRUE           FALSE
#> 527           FALSE                FALSE              TRUE           FALSE
#> 528           FALSE                FALSE              TRUE           FALSE
#> 529           FALSE                FALSE              TRUE           FALSE
#> 530           FALSE                 TRUE             FALSE           FALSE
#> 531           FALSE                 TRUE             FALSE            TRUE
#> 532           FALSE                FALSE              TRUE           FALSE
#> 533           FALSE                 TRUE             FALSE           FALSE
#> 534           FALSE                 TRUE             FALSE           FALSE
#> 535           FALSE                 TRUE             FALSE           FALSE
#> 536           FALSE                FALSE              TRUE            TRUE
#> 537           FALSE                FALSE              TRUE           FALSE
#> 538           FALSE                 TRUE             FALSE           FALSE
#> 539           FALSE                 TRUE             FALSE           FALSE
#> 540           FALSE                FALSE              TRUE           FALSE
#> 541           FALSE                 TRUE             FALSE           FALSE
#> 542           FALSE                 TRUE             FALSE            TRUE
#> 543           FALSE                 TRUE             FALSE            TRUE
#> 544           FALSE                 TRUE             FALSE           FALSE
#> 545           FALSE                 TRUE             FALSE            TRUE
#> 546           FALSE                 TRUE             FALSE            TRUE
#> 547           FALSE                 TRUE             FALSE            TRUE
#> 548           FALSE                FALSE              TRUE            TRUE
#> 549           FALSE                 TRUE             FALSE           FALSE
#> 550           FALSE                 TRUE             FALSE           FALSE
#> 551           FALSE                FALSE              TRUE           FALSE
#> 552           FALSE                FALSE              TRUE           FALSE
#> 553           FALSE                FALSE              TRUE            TRUE
#> 554           FALSE                 TRUE             FALSE           FALSE
#> 555           FALSE                 TRUE             FALSE           FALSE
#> 556           FALSE                 TRUE             FALSE           FALSE
#> 557           FALSE                 TRUE             FALSE           FALSE
#> 558           FALSE                 TRUE             FALSE           FALSE
#> 559           FALSE                FALSE              TRUE            TRUE
#> 560           FALSE                 TRUE             FALSE           FALSE
#> 561           FALSE                FALSE              TRUE           FALSE
#> 562           FALSE                 TRUE             FALSE           FALSE
#> 563           FALSE                 TRUE             FALSE            TRUE
#> 564           FALSE                FALSE              TRUE           FALSE
#> 565           FALSE                FALSE              TRUE            TRUE
#> 566           FALSE                 TRUE             FALSE           FALSE
#> 567           FALSE                FALSE              TRUE           FALSE
#> 568           FALSE                 TRUE             FALSE           FALSE
#> 569           FALSE                 TRUE             FALSE            TRUE
#> 570           FALSE                FALSE              TRUE           FALSE
#> 571           FALSE                 TRUE             FALSE           FALSE
#> 572           FALSE                 TRUE             FALSE           FALSE
#> 573           FALSE                FALSE              TRUE           FALSE
#> 574           FALSE                FALSE              TRUE            TRUE
#> 575           FALSE                FALSE              TRUE           FALSE
#> 576           FALSE                FALSE              TRUE           FALSE
#> 577           FALSE                 TRUE             FALSE            TRUE
#> 578           FALSE                FALSE              TRUE            TRUE
#> 579           FALSE                 TRUE             FALSE           FALSE
#> 580           FALSE                 TRUE             FALSE            TRUE
#> 581           FALSE                FALSE              TRUE            TRUE
#> 582           FALSE                 TRUE             FALSE           FALSE
#> 583           FALSE                 TRUE             FALSE            TRUE
#> 584           FALSE                FALSE              TRUE           FALSE
#> 585           FALSE                FALSE              TRUE           FALSE
#> 586           FALSE                FALSE              TRUE            TRUE
#> 587           FALSE                 TRUE             FALSE           FALSE
#> 588           FALSE                 TRUE             FALSE            TRUE
#> 589           FALSE                FALSE              TRUE            TRUE
#> 590           FALSE                 TRUE             FALSE           FALSE
#> 591           FALSE                FALSE              TRUE            TRUE
#> 592           FALSE                FALSE              TRUE            TRUE
#> 593           FALSE                 TRUE             FALSE            TRUE
#> 594           FALSE                 TRUE             FALSE            TRUE
#> 595           FALSE                FALSE              TRUE            TRUE
#> 596           FALSE                 TRUE             FALSE           FALSE
#> 597           FALSE                 TRUE             FALSE           FALSE
#> 598           FALSE                FALSE              TRUE           FALSE
#> 599           FALSE                 TRUE             FALSE            TRUE
#> 600           FALSE                FALSE              TRUE           FALSE
#> 601           FALSE                 TRUE             FALSE           FALSE
#> 602           FALSE                 TRUE             FALSE            TRUE
#> 603           FALSE                FALSE              TRUE           FALSE
#> 604           FALSE                 TRUE             FALSE           FALSE
#> 605           FALSE                FALSE              TRUE            TRUE
#> 606           FALSE                 TRUE             FALSE           FALSE
#> 607           FALSE                FALSE              TRUE            TRUE
#> 608           FALSE                FALSE              TRUE            TRUE
#> 609           FALSE                 TRUE             FALSE           FALSE
#> 610           FALSE                 TRUE             FALSE           FALSE
#> 611           FALSE                 TRUE             FALSE           FALSE
#> 612           FALSE                 TRUE             FALSE           FALSE
#> 613           FALSE                FALSE              TRUE           FALSE
#> 614           FALSE                 TRUE             FALSE           FALSE
#> 615           FALSE                 TRUE             FALSE           FALSE
#> 616           FALSE                FALSE              TRUE           FALSE
#> 617           FALSE                 TRUE             FALSE            TRUE
#> 618           FALSE                FALSE              TRUE           FALSE
#> 619           FALSE                 TRUE             FALSE           FALSE
#> 620           FALSE                 TRUE             FALSE           FALSE
#> 621           FALSE                 TRUE             FALSE            TRUE
#> 622           FALSE                 TRUE             FALSE           FALSE
#> 623           FALSE                 TRUE             FALSE           FALSE
#> 624           FALSE                FALSE              TRUE            TRUE
#> 625           FALSE                 TRUE             FALSE           FALSE
#> 626           FALSE                 TRUE             FALSE            TRUE
#> 627           FALSE                 TRUE             FALSE           FALSE
#> 628           FALSE                 TRUE             FALSE           FALSE
#> 629           FALSE                 TRUE             FALSE           FALSE
#> 630           FALSE                FALSE              TRUE            TRUE
#> 631           FALSE                 TRUE             FALSE           FALSE
#> 632           FALSE                 TRUE             FALSE           FALSE
#> 633           FALSE                 TRUE             FALSE           FALSE
#> 634           FALSE                 TRUE             FALSE            TRUE
#> 635           FALSE                FALSE              TRUE           FALSE
#> 636           FALSE                FALSE              TRUE            TRUE
#> 637           FALSE                 TRUE             FALSE           FALSE
#> 638           FALSE                FALSE              TRUE            TRUE
#> 639           FALSE                FALSE              TRUE           FALSE
#> 640           FALSE                FALSE              TRUE            TRUE
#> 641           FALSE                 TRUE             FALSE           FALSE
#> 642           FALSE                 TRUE             FALSE           FALSE
#> 643           FALSE                 TRUE             FALSE           FALSE
#> 644           FALSE                 TRUE             FALSE           FALSE
#> 645           FALSE                 TRUE             FALSE            TRUE
#> 646           FALSE                FALSE              TRUE            TRUE
#> 647           FALSE                FALSE              TRUE            TRUE
#> 648           FALSE                 TRUE             FALSE            TRUE
#> 649           FALSE                 TRUE             FALSE           FALSE
#> 650           FALSE                 TRUE             FALSE           FALSE
#> 651           FALSE                 TRUE             FALSE           FALSE
#> 652           FALSE                FALSE              TRUE            TRUE
#> 653           FALSE                 TRUE             FALSE           FALSE
#> 654           FALSE                 TRUE             FALSE           FALSE
#> 655           FALSE                FALSE              TRUE           FALSE
#> 656           FALSE                FALSE              TRUE            TRUE
#> 657           FALSE                 TRUE             FALSE            TRUE
#> 658           FALSE                FALSE              TRUE           FALSE
#> 659           FALSE                FALSE              TRUE           FALSE
#> 660           FALSE                 TRUE             FALSE           FALSE
#> 661           FALSE                FALSE              TRUE            TRUE
#> 662           FALSE                 TRUE             FALSE           FALSE
#> 663           FALSE                 TRUE             FALSE           FALSE
#> 664           FALSE                FALSE              TRUE           FALSE
#> 665           FALSE                 TRUE             FALSE           FALSE
#> 666           FALSE                FALSE              TRUE            TRUE
#> 667           FALSE                FALSE              TRUE            TRUE
#> 668           FALSE                 TRUE             FALSE            TRUE
#> 669           FALSE                FALSE              TRUE           FALSE
#> 670           FALSE                 TRUE             FALSE           FALSE
#> 671           FALSE                FALSE              TRUE           FALSE
#>                        DatoEndret
#> 1   2025-09-02T08:58:29.683+00:00
#> 2   2025-11-13T00:24:47.967+00:00
#> 3     2025-12-12T00:24:01.9+00:00
#> 4   2025-12-12T00:36:54.343+00:00
#> 5    2025-10-15T01:39:05.68+00:00
#> 6   2025-09-13T01:20:23.753+00:00
#> 7   2025-12-12T00:46:22.977+00:00
#> 8   2025-09-02T08:03:23.517+00:00
#> 9    2025-12-12T00:27:37.12+00:00
#> 10  2025-09-02T08:03:32.617+00:00
#> 11  2025-04-03T19:40:57.863+00:00
#> 12   2025-11-13T00:22:34.92+00:00
#> 13  2025-10-15T01:26:45.677+00:00
#> 14  2025-12-24T00:15:25.513+00:00
#> 15   2025-04-03T19:43:13.97+00:00
#> 16      2025-11-13T00:42:50+00:00
#> 17   2025-12-12T00:23:53.76+00:00
#> 18  2025-11-02T04:00:57.553+00:00
#> 19  2025-04-03T19:42:19.293+00:00
#> 20   2025-10-15T01:26:16.82+00:00
#> 21  2025-12-12T00:30:06.597+00:00
#> 22   2025-12-12T00:16:25.21+00:00
#> 23  2025-04-03T19:42:58.737+00:00
#> 24  2025-09-02T08:04:37.713+00:00
#> 25  2025-04-03T19:43:09.197+00:00
#> 26  2025-09-02T08:04:29.803+00:00
#> 27  2026-01-02T04:12:20.463+00:00
#> 28  2025-04-03T19:42:33.693+00:00
#> 29  2025-04-03T19:42:51.557+00:00
#> 30  2025-12-12T00:29:02.873+00:00
#> 31  2025-04-03T19:42:11.087+00:00
#> 32  2025-12-23T00:15:27.667+00:00
#> 33  2025-12-12T00:16:23.957+00:00
#> 34  2025-09-02T08:04:34.957+00:00
#> 35  2025-11-13T00:42:53.137+00:00
#> 36   2026-01-02T04:05:39.95+00:00
#> 37  2025-12-12T00:33:08.257+00:00
#> 38  2025-10-15T01:39:53.337+00:00
#> 39  2025-09-02T08:10:44.363+00:00
#> 40  2025-12-12T00:16:23.367+00:00
#> 41   2025-04-03T19:43:09.12+00:00
#> 42   2025-12-12T00:44:24.94+00:00
#> 43  2025-12-12T00:21:48.423+00:00
#> 44  2025-04-03T19:41:31.457+00:00
#> 45   2025-12-12T00:25:15.47+00:00
#> 46    2025-09-02T08:12:36.6+00:00
#> 47  2025-12-12T00:36:56.313+00:00
#> 48   2025-10-15T01:26:01.56+00:00
#> 49   2025-11-13T00:22:30.74+00:00
#> 50   2025-10-15T01:36:32.76+00:00
#> 51  2025-04-03T19:43:18.403+00:00
#> 52   2025-12-12T00:25:26.63+00:00
#> 53  2025-04-03T19:43:29.243+00:00
#> 54  2025-10-15T01:44:28.563+00:00
#> 55  2025-12-12T00:21:48.147+00:00
#> 56  2025-12-12T00:16:29.297+00:00
#> 57   2025-12-12T00:27:08.53+00:00
#> 58  2025-11-13T00:40:41.573+00:00
#> 59  2025-12-24T00:15:10.177+00:00
#> 60   2025-04-03T19:42:42.62+00:00
#> 61   2025-12-12T00:16:27.96+00:00
#> 62  2025-04-03T19:42:17.627+00:00
#> 63   2025-12-12T00:41:33.05+00:00
#> 64  2025-12-12T00:16:23.463+00:00
#> 65  2025-12-12T00:24:29.447+00:00
#> 66   2025-04-03T19:42:58.29+00:00
#> 67  2025-12-12T00:23:54.157+00:00
#> 68  2025-10-15T01:47:01.797+00:00
#> 69   2025-04-03T19:43:03.59+00:00
#> 70   2025-12-12T00:33:04.42+00:00
#> 71  2025-10-15T01:29:16.713+00:00
#> 72  2025-04-03T19:43:25.863+00:00
#> 73  2025-04-03T19:41:31.167+00:00
#> 74   2025-11-13T00:26:37.74+00:00
#> 75   2025-04-03T19:42:40.72+00:00
#> 76  2025-04-03T19:43:03.547+00:00
#> 77  2025-09-02T08:04:31.893+00:00
#> 78  2025-12-12T00:18:19.983+00:00
#> 79  2025-04-03T19:42:37.157+00:00
#> 80  2025-12-12T00:35:07.073+00:00
#> 81  2025-04-03T19:43:14.063+00:00
#> 82  2025-04-03T19:41:39.813+00:00
#> 83  2025-10-15T01:45:18.407+00:00
#> 84  2025-04-03T19:41:27.033+00:00
#> 85  2025-11-13T00:38:39.257+00:00
#> 86  2025-09-13T01:25:46.533+00:00
#> 87   2025-10-15T01:17:07.77+00:00
#> 88  2025-12-12T00:33:33.733+00:00
#> 89  2025-12-12T00:22:08.457+00:00
#> 90  2025-09-02T08:18:33.047+00:00
#> 91  2025-12-12T00:16:25.097+00:00
#> 92  2025-11-13T00:40:29.527+00:00
#> 93  2025-04-03T19:42:34.057+00:00
#> 94  2025-12-12T00:25:26.977+00:00
#> 95  2025-04-03T16:44:50.313+00:00
#> 96  2025-12-12T00:16:25.367+00:00
#> 97   2025-09-02T08:19:24.24+00:00
#> 98  2025-04-03T19:42:24.617+00:00
#> 99  2025-12-12T00:45:42.717+00:00
#> 100 2025-10-15T01:32:42.433+00:00
#> 101 2025-04-03T16:44:43.843+00:00
#> 102 2025-12-12T00:45:22.243+00:00
#> 103 2025-10-15T01:42:10.643+00:00
#> 104 2025-12-12T00:33:55.847+00:00
#> 105 2025-10-15T01:37:33.793+00:00
#> 106 2025-12-12T00:37:23.103+00:00
#> 107  2025-12-12T00:40:48.12+00:00
#> 108 2025-04-03T19:42:21.973+00:00
#> 109  2025-11-13T00:26:04.72+00:00
#> 110 2025-12-12T00:42:25.037+00:00
#> 111  2025-09-02T08:20:20.58+00:00
#> 112  2025-10-15T01:35:46.96+00:00
#> 113  2025-10-15T01:47:55.37+00:00
#> 114 2025-12-12T00:32:51.093+00:00
#> 115  2025-12-12T00:36:33.93+00:00
#> 116 2025-12-12T00:26:21.883+00:00
#> 117 2025-10-15T01:35:21.327+00:00
#> 118 2025-12-12T00:35:12.017+00:00
#> 119   2025-12-12T00:46:30.6+00:00
#> 120  2025-12-12T00:22:03.23+00:00
#> 121 2025-12-12T00:27:19.197+00:00
#> 122 2025-12-12T00:27:38.443+00:00
#> 123 2025-12-12T00:33:49.137+00:00
#> 124  2025-12-12T00:25:02.95+00:00
#> 125  2025-11-13T00:39:12.37+00:00
#> 126  2025-04-03T19:42:39.59+00:00
#> 127  2025-04-03T19:42:19.92+00:00
#> 128 2025-12-12T00:20:40.953+00:00
#> 129  2025-12-12T00:45:54.29+00:00
#> 130 2025-11-13T00:42:58.293+00:00
#> 131 2025-04-03T19:43:32.483+00:00
#> 132 2025-12-12T00:24:57.273+00:00
#> 133 2025-09-02T08:03:36.577+00:00
#> 134  2025-04-03T16:44:23.73+00:00
#> 135  2025-11-13T00:26:56.53+00:00
#> 136 2025-12-12T00:33:03.823+00:00
#> 137  2025-04-03T19:42:45.69+00:00
#> 138 2025-12-12T00:23:37.817+00:00
#> 139  2025-12-12T00:42:52.16+00:00
#> 140 2025-12-12T00:29:24.417+00:00
#> 141  2025-11-13T00:45:16.62+00:00
#> 142  2025-11-13T00:27:34.78+00:00
#> 143  2025-12-12T00:16:29.19+00:00
#> 144 2025-12-12T00:30:47.813+00:00
#> 145 2025-10-15T01:41:25.293+00:00
#> 146 2025-04-03T19:43:18.317+00:00
#> 147 2025-09-02T08:07:54.837+00:00
#> 148 2025-04-03T19:43:01.557+00:00
#> 149  2025-11-13T00:27:07.33+00:00
#> 150 2025-04-03T16:44:42.717+00:00
#> 151 2025-04-03T19:41:32.497+00:00
#> 152  2025-04-03T19:41:16.97+00:00
#> 153 2025-12-12T00:23:37.727+00:00
#> 154 2025-12-12T00:32:39.803+00:00
#> 155 2025-11-13T00:24:48.513+00:00
#> 156   2025-12-12T00:23:37.6+00:00
#> 157  2025-11-13T00:47:01.97+00:00
#> 158 2026-01-02T04:05:36.197+00:00
#> 159 2025-04-03T19:42:47.247+00:00
#> 160 2025-12-12T00:40:56.057+00:00
#> 161 2025-10-15T01:23:08.707+00:00
#> 162 2025-10-15T01:34:13.737+00:00
#> 163 2025-11-13T00:27:34.683+00:00
#> 164 2025-04-03T19:43:13.837+00:00
#> 165 2025-04-03T19:41:46.783+00:00
#> 166 2025-04-03T19:42:53.763+00:00
#> 167 2025-12-12T00:16:24.143+00:00
#> 168 2025-09-13T01:26:05.797+00:00
#> 169  2025-10-15T01:17:08.49+00:00
#> 170 2025-11-02T04:01:17.293+00:00
#> 171  2025-04-03T19:43:11.62+00:00
#> 172 2025-09-02T08:26:33.513+00:00
#> 173  2025-04-03T19:42:22.02+00:00
#> 174  2025-12-12T00:16:27.33+00:00
#> 175 2025-04-03T19:42:00.917+00:00
#> 176 2025-04-03T19:41:44.693+00:00
#> 177 2025-04-03T19:42:49.453+00:00
#> 178 2025-09-02T08:04:35.717+00:00
#> 179  2025-12-12T00:16:29.76+00:00
#> 180 2025-12-12T00:18:24.543+00:00
#> 181  2025-09-02T08:27:14.53+00:00
#> 182 2025-12-12T00:38:20.953+00:00
#> 183 2025-10-15T01:45:26.867+00:00
#> 184  2025-12-12T00:16:29.95+00:00
#> 185   2025-04-03T19:43:09.7+00:00
#> 186  2025-12-12T00:45:46.94+00:00
#> 187 2025-09-02T08:03:35.473+00:00
#> 188  2025-11-13T00:40:57.86+00:00
#> 189  2025-12-12T00:47:14.66+00:00
#> 190 2025-09-13T01:32:06.637+00:00
#> 191 2025-11-13T00:31:38.067+00:00
#> 192 2025-12-12T00:16:24.243+00:00
#> 193 2025-11-13T00:22:32.337+00:00
#> 194  2025-12-12T00:16:27.21+00:00
#> 195 2025-12-12T00:23:06.147+00:00
#> 196 2025-09-13T01:26:46.677+00:00
#> 197 2025-04-03T19:41:23.583+00:00
#> 198 2025-04-03T19:42:49.663+00:00
#> 199 2025-04-03T19:43:18.567+00:00
#> 200 2025-11-13T00:38:05.707+00:00
#> 201  2025-12-12T00:42:00.61+00:00
#> 202 2025-11-13T00:18:41.553+00:00
#> 203  2025-12-12T00:16:28.72+00:00
#> 204 2025-04-03T19:42:09.363+00:00
#> 205 2025-11-13T00:35:46.643+00:00
#> 206   2025-04-03T19:42:29.8+00:00
#> 207  2025-11-13T00:22:34.52+00:00
#> 208 2025-04-03T19:43:19.563+00:00
#> 209 2025-04-03T19:43:18.273+00:00
#> 210 2025-04-03T19:43:20.177+00:00
#> 211 2025-04-03T19:43:31.367+00:00
#> 212  2025-10-15T01:21:25.54+00:00
#> 213 2025-09-02T08:29:18.553+00:00
#> 214  2025-09-02T08:29:19.73+00:00
#> 215 2025-04-03T16:44:42.447+00:00
#> 216 2025-11-13T00:47:54.923+00:00
#> 217  2025-09-02T08:13:09.66+00:00
#> 218 2025-10-15T01:30:13.247+00:00
#> 219  2025-04-03T19:42:57.54+00:00
#> 220 2025-04-03T19:42:58.647+00:00
#> 221 2025-11-13T00:22:33.087+00:00
#> 222 2025-12-12T00:38:12.253+00:00
#> 223 2025-09-13T01:26:05.733+00:00
#> 224  2025-12-12T00:39:31.33+00:00
#> 225  2025-04-03T19:41:46.87+00:00
#> 226  2025-11-13T00:34:55.64+00:00
#> 227  2025-10-15T01:38:06.02+00:00
#> 228 2025-10-15T01:27:26.837+00:00
#> 229 2025-04-03T19:43:32.527+00:00
#> 230 2025-10-15T01:44:12.803+00:00
#> 231  2025-04-03T19:43:13.88+00:00
#> 232   2025-09-02T08:30:52.8+00:00
#> 233 2025-09-02T08:30:53.783+00:00
#> 234  2025-09-10T01:16:13.72+00:00
#> 235 2025-11-13T00:22:35.997+00:00
#> 236  2025-12-12T00:35:35.87+00:00
#> 237 2025-09-13T01:30:10.847+00:00
#> 238  2025-12-12T00:42:00.92+00:00
#> 239  2025-04-03T19:41:35.92+00:00
#> 240 2025-04-03T19:43:18.447+00:00
#> 241   2025-11-13T00:44:28.6+00:00
#> 242 2025-12-12T00:33:50.273+00:00
#> 243 2026-01-02T04:08:19.517+00:00
#> 244 2025-09-02T08:22:23.183+00:00
#> 245 2025-09-02T08:22:23.253+00:00
#> 246  2025-10-15T01:23:08.85+00:00
#> 247  2025-12-12T00:16:24.94+00:00
#> 248 2025-04-03T19:41:35.003+00:00
#> 249 2025-04-03T19:42:01.953+00:00
#> 250 2025-09-02T08:31:55.993+00:00
#> 251 2025-12-12T00:36:39.473+00:00
#> 252  2026-01-02T04:05:13.12+00:00
#> 253 2025-09-02T08:31:26.443+00:00
#> 254  2025-12-12T00:36:20.92+00:00
#> 255 2025-12-12T00:25:26.527+00:00
#> 256  2025-11-13T00:39:17.34+00:00
#> 257 2025-04-03T19:43:08.373+00:00
#> 258 2025-12-12T00:16:28.297+00:00
#> 259   2025-04-03T19:42:15.3+00:00
#> 260 2025-04-03T19:42:44.417+00:00
#> 261 2025-04-03T19:41:26.813+00:00
#> 262 2025-11-23T00:15:16.883+00:00
#> 263 2025-09-02T08:33:05.443+00:00
#> 264  2025-04-03T19:41:30.52+00:00
#> 265 2025-12-12T00:20:46.517+00:00
#> 266  2025-04-03T19:41:43.31+00:00
#> 267  2025-04-03T19:41:04.57+00:00
#> 268  2025-12-12T00:23:53.54+00:00
#> 269 2025-11-13T00:37:30.853+00:00
#> 270  2025-10-15T01:23:07.27+00:00
#> 271     2025-09-02T08:33:25+00:00
#> 272 2025-10-15T01:39:02.223+00:00
#> 273  2025-09-02T08:33:25.67+00:00
#> 274 2025-11-13T00:42:54.593+00:00
#> 275 2025-09-02T08:33:27.013+00:00
#> 276 2025-04-03T19:41:16.757+00:00
#> 277 2025-11-13T00:43:39.677+00:00
#> 278 2025-04-03T19:43:18.223+00:00
#> 279 2025-12-12T00:16:23.687+00:00
#> 280 2025-12-24T00:15:25.957+00:00
#> 281   2025-12-12T00:16:23.6+00:00
#> 282 2025-04-03T19:41:58.943+00:00
#> 283 2025-12-12T00:23:25.023+00:00
#> 284 2025-04-03T19:43:32.083+00:00
#> 285 2025-12-12T00:38:32.917+00:00
#> 286 2025-04-03T19:42:39.743+00:00
#> 287  2025-09-02T08:34:10.34+00:00
#> 288 2025-12-12T00:44:08.683+00:00
#> 289  2025-04-03T19:42:38.48+00:00
#> 290 2025-04-03T19:43:24.057+00:00
#> 291  2025-12-12T00:16:28.53+00:00
#> 292  2025-12-12T00:29:42.98+00:00
#> 293 2025-12-12T00:34:26.357+00:00
#> 294 2025-11-13T00:25:36.533+00:00
#> 295 2025-04-03T19:41:32.723+00:00
#> 296  2025-10-15T01:25:09.83+00:00
#> 297  2025-10-29T00:15:09.02+00:00
#> 298  2025-04-03T19:42:14.78+00:00
#> 299 2025-04-03T19:42:16.477+00:00
#> 300 2025-04-03T19:42:58.333+00:00
#> 301 2025-10-15T01:32:22.583+00:00
#> 302 2025-10-15T01:17:15.147+00:00
#> 303 2025-04-03T19:42:32.557+00:00
#> 304  2025-12-24T00:15:18.04+00:00
#> 305 2025-11-13T00:24:10.863+00:00
#> 306 2025-04-03T19:41:01.083+00:00
#> 307   2025-04-03T19:40:55.5+00:00
#> 308 2025-12-12T00:25:26.833+00:00
#> 309 2025-12-12T00:25:15.683+00:00
#> 310 2025-11-13T00:25:16.617+00:00
#> 311 2026-01-02T04:13:41.867+00:00
#> 312 2025-12-12T00:44:12.547+00:00
#> 313 2025-11-13T00:31:18.673+00:00
#> 314 2025-11-13T00:22:57.467+00:00
#> 315 2025-12-12T00:31:45.247+00:00
#> 316  2025-10-15T01:45:58.72+00:00
#> 317 2025-11-13T00:28:30.743+00:00
#> 318 2025-12-12T00:39:18.167+00:00
#> 319 2025-09-13T01:32:09.663+00:00
#> 320  2025-12-12T00:28:43.84+00:00
#> 321 2025-11-13T00:38:07.147+00:00
#> 322 2025-09-13T01:49:02.693+00:00
#> 323 2025-12-12T00:30:25.987+00:00
#> 324 2025-11-13T00:43:01.963+00:00
#> 325 2025-12-12T00:26:25.007+00:00
#> 326  2025-10-15T01:35:54.92+00:00
#> 327  2025-04-03T19:41:21.41+00:00
#> 328 2025-10-15T01:23:07.337+00:00
#> 329  2025-11-13T00:22:33.19+00:00
#> 330 2025-09-13T01:18:39.017+00:00
#> 331 2025-04-03T19:41:05.303+00:00
#> 332 2025-12-12T00:25:27.337+00:00
#> 333 2025-12-02T00:15:11.963+00:00
#> 334  2025-04-03T19:41:26.68+00:00
#> 335 2025-12-12T00:43:58.013+00:00
#> 336 2025-04-03T19:42:12.807+00:00
#> 337   2025-09-02T08:04:34.3+00:00
#> 338  2025-12-24T00:15:26.26+00:00
#> 339 2025-04-03T19:42:55.877+00:00
#> 340 2025-12-12T00:45:27.613+00:00
#> 341  2025-04-03T19:42:45.19+00:00
#> 342 2025-12-12T00:16:25.543+00:00
#> 343  2025-12-24T00:15:25.64+00:00
#> 344 2025-04-03T19:41:09.017+00:00
#> 345 2025-04-03T19:41:20.803+00:00
#> 346 2025-04-03T16:44:42.537+00:00
#> 347 2025-12-12T00:21:48.697+00:00
#> 348 2025-04-03T19:42:29.457+00:00
#> 349 2025-04-03T19:40:59.677+00:00
#> 350 2025-04-03T19:42:17.127+00:00
#> 351 2025-12-12T00:30:28.763+00:00
#> 352   2025-10-15T01:23:15.8+00:00
#> 353  2025-12-12T00:36:31.61+00:00
#> 354  2025-09-02T08:13:09.55+00:00
#> 355  2025-11-13T00:46:35.23+00:00
#> 356  2025-10-15T01:25:29.51+00:00
#> 357 2025-12-12T00:21:47.967+00:00
#> 358  2025-04-03T19:40:59.19+00:00
#> 359 2025-12-12T00:19:48.973+00:00
#> 360  2025-10-15T01:28:09.48+00:00
#> 361  2025-12-12T00:23:37.41+00:00
#> 362 2025-09-02T08:31:26.297+00:00
#> 363 2025-12-12T00:20:46.417+00:00
#> 364 2025-04-03T19:42:21.427+00:00
#> 365 2025-09-02T08:44:07.597+00:00
#> 366 2025-04-03T19:42:29.283+00:00
#> 367  2025-04-03T19:42:36.76+00:00
#> 368 2025-04-03T19:41:06.887+00:00
#> 369 2025-04-03T19:43:03.633+00:00
#> 370 2025-11-13T00:22:32.527+00:00
#> 371 2025-10-15T01:30:37.117+00:00
#> 372 2025-04-03T19:42:13.487+00:00
#> 373 2025-09-02T08:46:55.493+00:00
#> 374 2025-09-02T08:46:56.763+00:00
#> 375 2025-09-02T08:46:57.347+00:00
#> 376 2025-09-02T08:46:57.957+00:00
#> 377 2025-12-12T00:20:46.187+00:00
#> 378 2025-12-12T00:16:29.053+00:00
#> 379  2025-11-02T04:01:43.13+00:00
#> 380 2025-04-03T19:41:16.357+00:00
#> 381 2025-09-02T08:47:32.487+00:00
#> 382  2025-12-12T00:25:26.72+00:00
#> 383 2025-12-12T00:25:26.413+00:00
#> 384   2025-12-12T00:21:48.8+00:00
#> 385 2025-10-15T01:28:43.883+00:00
#> 386 2025-09-02T08:48:06.833+00:00
#> 387   2025-09-02T08:48:06.7+00:00
#> 388  2025-10-15T01:39:24.54+00:00
#> 389 2025-12-12T00:46:32.623+00:00
#> 390 2025-10-15T01:24:06.283+00:00
#> 391 2025-10-15T01:32:03.693+00:00
#> 392 2025-09-13T01:44:26.763+00:00
#> 393 2025-12-02T04:05:21.247+00:00
#> 394 2025-12-12T00:22:24.783+00:00
#> 395 2025-12-12T00:44:56.903+00:00
#> 396 2025-12-12T00:44:40.287+00:00
#> 397  2025-09-14T01:26:14.11+00:00
#> 398 2025-04-03T16:44:48.233+00:00
#> 399   2025-04-03T19:42:57.5+00:00
#> 400 2025-11-13T00:28:31.467+00:00
#> 401 2025-12-12T00:16:25.633+00:00
#> 402  2025-09-02T08:22:23.32+00:00
#> 403 2025-04-03T19:41:28.987+00:00
#> 404 2025-09-02T07:56:43.457+00:00
#> 405 2025-12-12T00:16:29.403+00:00
#> 406 2025-04-03T19:41:30.313+00:00
#> 407 2025-11-13T00:21:56.367+00:00
#> 408 2025-12-12T00:20:59.987+00:00
#> 409 2025-04-03T19:41:45.003+00:00
#> 410 2025-04-03T16:44:44.193+00:00
#> 411 2025-04-03T19:42:40.547+00:00
#> 412  2025-10-15T01:17:07.48+00:00
#> 413  2025-11-13T00:38:33.47+00:00
#> 414 2025-04-03T19:42:23.027+00:00
#> 415  2025-09-02T08:04:32.69+00:00
#> 416  2025-12-12T00:47:18.51+00:00
#> 417 2025-12-12T00:41:46.277+00:00
#> 418  2025-04-03T19:40:58.19+00:00
#> 419 2025-12-12T00:22:23.217+00:00
#> 420 2025-10-15T01:21:29.123+00:00
#> 421  2025-09-02T08:50:09.88+00:00
#> 422  2025-12-12T00:46:46.05+00:00
#> 423 2025-12-12T00:42:00.703+00:00
#> 424  2025-12-12T00:29:42.89+00:00
#> 425 2025-10-15T01:26:42.687+00:00
#> 426 2025-12-12T00:40:09.027+00:00
#> 427 2025-12-12T00:31:59.487+00:00
#> 428 2025-09-02T08:19:35.693+00:00
#> 429  2025-12-12T00:45:15.61+00:00
#> 430 2025-09-02T08:12:10.427+00:00
#> 431 2025-12-12T00:23:05.963+00:00
#> 432  2025-12-12T00:21:59.86+00:00
#> 433  2025-12-12T00:35:34.95+00:00
#> 434 2025-09-02T08:03:35.357+00:00
#> 435 2025-12-12T00:25:15.363+00:00
#> 436 2025-04-03T19:41:27.077+00:00
#> 437 2025-10-15T01:47:33.803+00:00
#> 438 2025-04-03T19:42:17.497+00:00
#> 439 2025-12-12T00:43:54.517+00:00
#> 440  2025-09-02T08:31:26.75+00:00
#> 441  2025-12-12T00:16:25.82+00:00
#> 442  2025-11-13T00:21:55.56+00:00
#> 443 2025-12-12T00:23:05.767+00:00
#> 444 2025-04-03T19:41:32.013+00:00
#> 445 2025-12-12T00:35:33.633+00:00
#> 446 2025-09-02T08:51:24.593+00:00
#> 447 2025-09-02T08:20:21.177+00:00
#> 448 2025-12-12T00:35:11.387+00:00
#> 449  2025-12-12T00:38:04.88+00:00
#> 450 2025-12-12T00:16:28.083+00:00
#> 451   2025-12-12T00:16:25.9+00:00
#> 452  2025-12-12T00:23:53.95+00:00
#> 453 2025-09-02T08:04:38.077+00:00
#> 454 2025-11-13T00:45:21.723+00:00
#> 455  2025-12-12T00:35:05.12+00:00
#> 456 2025-12-12T00:35:00.283+00:00
#> 457  2025-12-12T00:16:24.35+00:00
#> 458  2025-04-03T19:42:26.94+00:00
#> 459 2025-06-15T01:16:06.197+00:00
#> 460  2025-12-12T00:46:56.55+00:00
#> 461  2025-04-03T19:43:03.37+00:00
#> 462  2025-10-15T01:38:21.15+00:00
#> 463 2025-12-24T00:15:16.417+00:00
#> 464 2025-11-13T00:22:38.053+00:00
#> 465 2025-12-12T00:16:25.983+00:00
#> 466 2025-10-15T01:23:15.293+00:00
#> 467 2025-04-03T19:43:05.797+00:00
#> 468 2025-04-03T19:41:21.533+00:00
#> 469 2025-11-13T00:37:17.907+00:00
#> 470 2025-04-03T19:42:34.477+00:00
#> 471 2025-10-15T01:16:53.933+00:00
#> 472  2025-11-13T00:24:10.52+00:00
#> 473  2025-04-03T19:41:13.49+00:00
#> 474 2025-12-12T00:16:26.077+00:00
#> 475 2025-11-13T00:41:58.923+00:00
#> 476 2025-12-12T00:16:24.453+00:00
#> 477  2025-12-12T00:16:27.11+00:00
#> 478 2025-09-02T08:04:37.947+00:00
#> 479 2025-09-02T08:53:14.777+00:00
#> 480 2025-09-02T08:53:14.927+00:00
#> 481 2025-09-02T08:53:17.163+00:00
#> 482  2025-11-13T00:45:28.19+00:00
#> 483  2026-01-02T04:09:16.05+00:00
#> 484 2025-09-02T08:53:18.367+00:00
#> 485 2025-09-02T08:53:16.827+00:00
#> 486 2025-11-13T00:23:53.447+00:00
#> 487  2025-12-12T00:29:34.61+00:00
#> 488 2025-12-12T00:42:44.557+00:00
#> 489  2025-04-03T19:42:13.25+00:00
#> 490 2025-04-03T19:41:10.023+00:00
#> 491   2025-04-03T19:41:35.9+00:00
#> 492  2025-12-12T00:23:06.06+00:00
#> 493  2025-09-13T01:41:20.71+00:00
#> 494  2025-12-12T00:25:15.59+00:00
#> 495 2025-10-15T01:25:29.397+00:00
#> 496 2025-09-13T01:27:46.283+00:00
#> 497 2025-04-03T19:41:24.453+00:00
#> 498  2025-04-03T19:41:42.83+00:00
#> 499  2025-10-15T01:17:07.42+00:00
#> 500 2025-09-02T08:18:05.817+00:00
#> 501  2025-12-12T00:16:28.19+00:00
#> 502 2025-09-02T08:06:37.687+00:00
#> 503  2025-09-02T08:54:38.79+00:00
#> 504 2025-04-03T19:42:08.293+00:00
#> 505 2025-12-12T00:25:27.433+00:00
#> 506 2025-11-13T00:25:31.857+00:00
#> 507  2025-04-03T19:42:15.55+00:00
#> 508 2025-12-12T00:37:54.977+00:00
#> 509   2025-09-02T08:12:10.1+00:00
#> 510 2025-04-03T19:41:14.837+00:00
#> 511   2025-12-12T00:16:28.4+00:00
#> 512  2025-12-12T00:30:15.24+00:00
#> 513  2025-12-12T00:45:17.42+00:00
#> 514  2025-04-03T19:40:55.64+00:00
#> 515 2025-11-13T00:49:57.653+00:00
#> 516 2025-04-03T19:41:37.767+00:00
#> 517 2025-09-02T08:13:09.077+00:00
#> 518 2025-04-03T19:41:46.827+00:00
#> 519 2025-12-12T00:16:26.167+00:00
#> 520 2025-09-13T01:43:46.463+00:00
#> 521  2025-09-02T08:48:06.35+00:00
#> 522 2025-11-13T00:27:39.553+00:00
#> 523 2025-04-03T19:42:45.647+00:00
#> 524  2025-12-12T00:27:16.07+00:00
#> 525 2025-12-12T00:35:39.607+00:00
#> 526  2025-12-12T00:43:42.03+00:00
#> 527  2025-12-12T00:34:05.27+00:00
#> 528 2025-09-13T01:37:31.457+00:00
#> 529  2025-12-12T00:40:48.95+00:00
#> 530 2025-12-12T00:16:24.537+00:00
#> 531   2025-04-03T19:43:12.5+00:00
#> 532 2025-09-02T08:56:24.033+00:00
#> 533  2025-12-12T00:16:28.86+00:00
#> 534 2025-12-12T00:21:48.513+00:00
#> 535  2025-11-13T00:22:33.54+00:00
#> 536 2025-04-03T19:41:28.913+00:00
#> 537 2025-12-12T00:39:41.183+00:00
#> 538 2025-12-12T00:23:53.853+00:00
#> 539 2025-11-13T00:34:28.883+00:00
#> 540  2025-09-02T08:56:38.87+00:00
#> 541 2025-12-12T00:16:30.173+00:00
#> 542  2025-04-03T19:41:28.94+00:00
#> 543  2025-04-03T19:42:44.02+00:00
#> 544 2025-11-13T00:34:11.377+00:00
#> 545 2025-04-03T19:42:44.557+00:00
#> 546 2025-04-03T19:41:56.617+00:00
#> 547 2025-09-02T08:31:26.663+00:00
#> 548  2025-04-03T19:42:29.24+00:00
#> 549 2025-11-13T00:22:35.893+00:00
#> 550 2025-10-15T01:23:08.087+00:00
#> 551  2025-12-12T00:26:03.21+00:00
#> 552   2025-10-15T01:40:12.1+00:00
#> 553 2025-09-02T08:57:22.467+00:00
#> 554 2025-12-12T00:23:06.233+00:00
#> 555  2025-12-12T00:27:33.65+00:00
#> 556  2025-09-02T08:03:35.99+00:00
#> 557 2025-10-15T01:23:23.123+00:00
#> 558  2025-11-13T00:22:36.73+00:00
#> 559 2025-04-03T19:41:28.267+00:00
#> 560 2025-09-13T01:26:05.223+00:00
#> 561 2025-09-02T08:57:47.077+00:00
#> 562 2025-12-12T00:16:29.593+00:00
#> 563 2025-04-03T19:43:10.803+00:00
#> 564 2025-10-15T01:43:44.433+00:00
#> 565  2025-04-03T19:41:13.45+00:00
#> 566  2025-11-13T00:22:38.32+00:00
#> 567 2025-12-12T00:38:09.307+00:00
#> 568   2025-12-12T00:16:30.3+00:00
#> 569  2025-04-03T19:43:10.76+00:00
#> 570   2025-11-13T00:39:51.7+00:00
#> 571  2025-12-12T00:16:24.65+00:00
#> 572 2025-12-12T00:16:24.763+00:00
#> 573  2025-12-12T00:42:16.83+00:00
#> 574 2025-04-03T19:43:28.047+00:00
#> 575 2025-12-12T00:37:34.537+00:00
#> 576 2025-09-13T01:45:35.703+00:00
#> 577 2025-04-03T19:42:59.477+00:00
#> 578 2025-04-03T19:41:06.317+00:00
#> 579 2025-12-12T00:16:26.267+00:00
#> 580 2025-04-03T19:43:20.273+00:00
#> 581  2025-04-03T19:41:58.19+00:00
#> 582 2025-12-12T00:21:47.833+00:00
#> 583  2025-04-03T19:43:10.33+00:00
#> 584 2025-09-02T08:59:06.457+00:00
#> 585  2025-11-13T00:38:28.43+00:00
#> 586  2025-04-03T19:43:18.36+00:00
#> 587  2025-12-24T00:15:26.13+00:00
#> 588 2025-04-03T19:43:01.157+00:00
#> 589 2025-04-03T19:41:40.557+00:00
#> 590  2025-12-12T00:39:04.71+00:00
#> 591 2025-04-03T19:41:08.223+00:00
#> 592  2025-09-02T08:58:29.58+00:00
#> 593 2025-04-03T19:43:29.563+00:00
#> 594 2025-04-03T19:42:59.563+00:00
#> 595 2025-04-03T19:42:07.847+00:00
#> 596 2025-12-12T00:16:27.427+00:00
#> 597   2025-12-12T00:23:37.5+00:00
#> 598 2025-11-13T00:17:06.713+00:00
#> 599  2025-04-03T19:43:10.43+00:00
#> 600 2025-12-12T00:32:15.007+00:00
#> 601 2025-12-24T00:15:26.547+00:00
#> 602  2025-09-02T08:02:45.88+00:00
#> 603 2025-09-02T09:01:01.237+00:00
#> 604 2025-09-13T01:29:36.477+00:00
#> 605 2025-04-03T19:41:28.693+00:00
#> 606 2025-11-13T00:22:31.513+00:00
#> 607 2025-04-03T19:42:38.283+00:00
#> 608 2025-04-03T19:42:44.253+00:00
#> 609 2025-09-13T01:55:48.747+00:00
#> 610 2025-12-12T00:41:30.927+00:00
#> 611 2025-09-02T08:03:36.107+00:00
#> 612  2025-11-13T00:22:30.62+00:00
#> 613 2025-11-13T00:49:13.877+00:00
#> 614 2025-09-13T01:18:37.607+00:00
#> 615 2025-12-12T00:21:48.607+00:00
#> 616  2025-09-02T09:01:19.08+00:00
#> 617 2025-09-02T08:13:09.237+00:00
#> 618  2025-11-13T00:40:19.23+00:00
#> 619 2025-12-12T00:16:26.383+00:00
#> 620 2025-10-15T01:46:56.973+00:00
#> 621 2025-09-10T01:16:12.173+00:00
#> 622 2025-12-12T00:23:53.637+00:00
#> 623  2025-09-02T08:04:39.59+00:00
#> 624 2025-04-03T19:42:48.913+00:00
#> 625  2025-10-15T01:26:23.37+00:00
#> 626 2025-04-03T19:43:28.537+00:00
#> 627 2025-12-24T00:15:26.377+00:00
#> 628   2025-12-12T00:16:29.5+00:00
#> 629 2025-12-12T00:21:47.737+00:00
#> 630 2025-09-02T09:01:52.387+00:00
#> 631 2025-11-13T00:25:36.337+00:00
#> 632 2025-10-15T01:21:28.633+00:00
#> 633 2025-12-12T00:30:40.117+00:00
#> 634  2025-09-02T08:11:14.54+00:00
#> 635  2025-12-12T00:38:04.26+00:00
#> 636  2025-04-03T19:42:23.07+00:00
#> 637 2025-10-15T01:23:09.543+00:00
#> 638  2025-04-03T19:42:11.69+00:00
#> 639 2025-12-12T00:44:14.187+00:00
#> 640 2025-04-03T19:42:03.727+00:00
#> 641  2025-12-12T00:39:27.84+00:00
#> 642 2025-09-13T01:25:44.003+00:00
#> 643  2025-11-13T00:21:55.65+00:00
#> 644 2025-12-12T00:23:54.047+00:00
#> 645 2025-04-03T19:43:15.497+00:00
#> 646   2025-04-03T19:42:14.4+00:00
#> 647 2025-04-03T19:42:06.127+00:00
#> 648 2025-09-02T08:22:23.367+00:00
#> 649 2025-12-12T00:16:26.913+00:00
#> 650 2025-12-12T00:16:26.733+00:00
#> 651 2025-11-13T00:34:24.147+00:00
#> 652 2025-04-03T19:41:36.913+00:00
#> 653  2025-10-15T01:23:07.96+00:00
#> 654 2025-12-12T00:16:23.807+00:00
#> 655  2025-10-15T01:23:24.83+00:00
#> 656 2025-04-03T19:41:35.087+00:00
#> 657  2025-04-03T19:42:59.43+00:00
#> 658  2025-11-13T00:38:29.66+00:00
#> 659  2025-12-12T00:41:27.87+00:00
#> 660  2025-11-13T00:22:37.78+00:00
#> 661 2025-04-03T19:42:46.467+00:00
#> 662  2025-10-15T01:42:03.86+00:00
#> 663 2025-11-13T00:27:34.873+00:00
#> 664 2025-11-13T00:27:06.677+00:00
#> 665  2025-12-24T00:15:25.77+00:00
#> 666 2025-04-03T19:43:25.213+00:00
#> 667  2025-04-03T19:43:16.13+00:00
#> 668  2025-04-03T19:42:54.53+00:00
#> 669 2025-09-02T09:03:21.373+00:00
#> 670 2025-10-15T01:39:01.433+00:00
#> 671 2025-12-12T00:42:20.027+00:00

# Get schools by municipality
get_kindergartens(1101)
#>        Orgnr                            Navn Karakteristikk Fylkesnr Kommunenr
#> 1  973854860      Duehuset Korttidsbarnehage                      11      1101
#> 2  987913592            Eigerøy barnehage SA                      11      1101
#> 3  983817025          Grøne Bråden barnehage                      11      1101
#> 4  973459007             Helleland barnehage                      11      1101
#> 5  994614371         Helleland Fus barnehage                      11      1101
#> 6  973459015               Hellvik barnehage                      11      1101
#> 7  991875441             Hestnesveien 255 AS                      11      1101
#> 8  976670094                Husabø barnehage                      11      1101
#> 9  973502018             Jernhagen barnehage                      11      1101
#> 10 990472386    Kiellandskogen Fus barnehage                      11      1101
#> 11 983491251            Lykkeliten barnehage                      11      1101
#> 12 973589032           Raketten barnehage SA                      11      1101
#> 13 989304879        Robåten Fus barnehage AS                      11      1101
#> 14 975308073             Rundevoll barnehage                      11      1101
#> 15 992589817 Skattekisten kulturbarnehage AS                      11      1101
#> 16 979150172              Slettebø barnehage                      11      1101
#> 17 974093634             Steinholt barnehage                      11      1101
#> 18 975323420      Tusenbeinet åpen barnehage                      11      1101
#> 19 973465716             Varden barnehage SA                      11      1101
#> 20 912702952     Villbassen Familiebarnehage                      11      1101
#>    Epost ErAktiv ErBarnehage ErBarnehageeier ErOffentligBarnehage
#> 1     NA   FALSE        TRUE           FALSE                 TRUE
#> 2     NA    TRUE        TRUE           FALSE                FALSE
#> 3     NA    TRUE        TRUE           FALSE                 TRUE
#> 4     NA   FALSE        TRUE           FALSE                 TRUE
#> 5     NA    TRUE        TRUE           FALSE                FALSE
#> 6     NA    TRUE        TRUE           FALSE                 TRUE
#> 7     NA   FALSE        TRUE           FALSE                FALSE
#> 8     NA   FALSE        TRUE           FALSE                 TRUE
#> 9     NA    TRUE        TRUE           FALSE                 TRUE
#> 10    NA    TRUE        TRUE           FALSE                FALSE
#> 11    NA    TRUE        TRUE           FALSE                 TRUE
#> 12    NA    TRUE        TRUE           FALSE                FALSE
#> 13    NA    TRUE        TRUE           FALSE                FALSE
#> 14    NA    TRUE        TRUE           FALSE                 TRUE
#> 15    NA    TRUE        TRUE           FALSE                FALSE
#> 16    NA    TRUE        TRUE           FALSE                 TRUE
#> 17    NA   FALSE        TRUE           FALSE                FALSE
#> 18    NA   FALSE        TRUE           FALSE                 TRUE
#> 19    NA    TRUE        TRUE           FALSE                FALSE
#> 20    NA   FALSE        TRUE           FALSE                FALSE
#>    ErPrivatBarnehage ErInaktivIBasil                    DatoEndret
#> 1              FALSE            TRUE 2025-04-03T19:43:14.063+00:00
#> 2               TRUE           FALSE 2025-09-13T01:25:46.533+00:00
#> 3              FALSE           FALSE 2025-09-02T08:26:33.513+00:00
#> 4              FALSE            TRUE 2025-04-03T19:43:18.567+00:00
#> 5               TRUE           FALSE 2025-11-13T00:38:05.707+00:00
#> 6              FALSE           FALSE  2025-12-12T00:42:00.61+00:00
#> 7               TRUE            TRUE   2025-04-03T19:42:29.8+00:00
#> 8              FALSE            TRUE  2025-04-03T19:42:57.54+00:00
#> 9              FALSE           FALSE  2025-12-12T00:42:00.92+00:00
#> 10              TRUE           FALSE 2025-12-12T00:36:39.473+00:00
#> 11             FALSE           FALSE 2025-11-13T00:24:10.863+00:00
#> 12              TRUE           FALSE 2025-11-13T00:28:31.467+00:00
#> 13              TRUE           FALSE  2025-12-12T00:47:18.51+00:00
#> 14             FALSE           FALSE 2025-12-12T00:42:00.703+00:00
#> 15              TRUE           FALSE 2025-12-12T00:35:00.283+00:00
#> 16             FALSE           FALSE  2025-11-13T00:24:10.52+00:00
#> 17              TRUE            TRUE  2025-09-02T08:48:06.35+00:00
#> 18             FALSE            TRUE 2025-04-03T19:42:59.563+00:00
#> 19              TRUE           FALSE 2025-11-13T00:49:13.877+00:00
#> 20              TRUE            TRUE  2025-04-03T19:42:11.69+00:00

if (FALSE) { # \dontrun{
# Get all schools
df <- get_kindergartens('all')
} # }
```
