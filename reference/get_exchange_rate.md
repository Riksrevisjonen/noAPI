# Get exchange rate

Fetch exchange rates from *Norges Bank*.

## Usage

``` r
get_exchange_rate(
  currency,
  frequency = c("daily", "monthly", "annual"),
  n_obs = 10,
  start = NULL,
  end = NULL,
  simplify = TRUE,
  raw_response = FALSE
)
```

## Arguments

- currency:

  The currency to fetch data for.

- frequency:

  Frequency of data. Can be daily, monthly or annual. Defaults to daily.

- n_obs:

  The number of observations to fetch. See details.

- start:

  The first date to fetch data for.

- end:

  The last date to fetch data for.

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

data.frame or list

## Details

Currency must be formatted as a three letter [ISO
3166](https://www.iso.org/iso-3166-country-codes.html) code. A list of
available country codes can be found at [Norges
Bank](https://www.norges-bank.no/en/topics/Statistics/exchange_rates/).
You can fetch observations for several currencies by passing a character
vector. If you want to include all currencies, you can set the
`currency` argument to `ALL`. Note that the number of observations is
limited to 20 when retrieving all currencies.

By default, the 10 latest observations are returned. You can adjust the
number of observations by setting `n_obs` to a positive integer. You can
also fetch exchange rates for a specific time period by setting the
arguments `start` and `end`. Both arguments accept a date in the format
`%Y-%m-%d` (*YYYY-MM-DD*).

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

## Examples

``` r
# Get latest daily exchange rates
get_exchange_rate('EUR')
#>    currency frequency exch_rate decimals calculated unit_multiplier  date_from
#> 1       EUR  Business   11.9150        4      false           Units 2025-12-19
#> 2       EUR  Business   11.8875        4      false           Units 2025-12-22
#> 3       EUR  Business   11.8490        4      false           Units 2025-12-23
#> 4       EUR  Business   11.8270        4      false           Units 2025-12-29
#> 5       EUR  Business   11.8260        4      false           Units 2025-12-30
#> 6       EUR  Business   11.8430        4      false           Units 2025-12-31
#> 7       EUR  Business   11.7985        4      false           Units 2026-01-02
#> 8       EUR  Business   11.7795        4      false           Units 2026-01-05
#> 9       EUR  Business   11.7285        4      false           Units 2026-01-06
#> 10      EUR  Business   11.7585        4      false           Units 2026-01-07
#>       date_to       date
#> 1  2025-12-19 2025-12-19
#> 2  2025-12-22 2025-12-22
#> 3  2025-12-23 2025-12-23
#> 4  2025-12-29 2025-12-29
#> 5  2025-12-30 2025-12-30
#> 6  2025-12-31 2025-12-31
#> 7  2026-01-02 2026-01-02
#> 8  2026-01-05 2026-01-05
#> 9  2026-01-06 2026-01-06
#> 10 2026-01-07 2026-01-07
# Get monthly exchange rates for multiple currencies
get_exchange_rate(c('EUR', 'USD'), frequency = 'monthly')
#>    currency frequency exch_rate decimals calculated unit_multiplier  date_from
#> 1       EUR   Monthly   11.5472        4      false           Units 2025-03-01
#> 2       EUR   Monthly   11.8313        4      false           Units 2025-04-01
#> 3       EUR   Monthly   11.6022        4      false           Units 2025-05-01
#> 4       EUR   Monthly   11.5879        4      false           Units 2025-06-01
#> 5       EUR   Monthly   11.8537        4      false           Units 2025-07-01
#> 6       EUR   Monthly   11.8653        4      false           Units 2025-08-01
#> 7       EUR   Monthly   11.6702        4      false           Units 2025-09-01
#> 8       EUR   Monthly   11.6633        4      false           Units 2025-10-01
#> 9       EUR   Monthly   11.7402        4      false           Units 2025-11-01
#> 10      EUR   Monthly   11.8448        4      false           Units 2025-12-01
#> 11      USD   Monthly   10.6867        4      false           Units 2025-03-01
#> 12      USD   Monthly   10.5593        4      false           Units 2025-04-01
#> 13      USD   Monthly   10.2881        4      false           Units 2025-05-01
#> 14      USD   Monthly   10.0577        4      false           Units 2025-06-01
#> 15      USD   Monthly   10.1519        4      false           Units 2025-07-01
#> 16      USD   Monthly   10.2013        4      false           Units 2025-08-01
#> 17      USD   Monthly    9.9474        4      false           Units 2025-09-01
#> 18      USD   Monthly   10.0285        4      false           Units 2025-10-01
#> 19      USD   Monthly   10.1558        4      false           Units 2025-11-01
#> 20      USD   Monthly   10.1196        4      false           Units 2025-12-01
#>       date_to    date
#> 1  2025-03-31 2025-03
#> 2  2025-04-30 2025-04
#> 3  2025-05-31 2025-05
#> 4  2025-06-30 2025-06
#> 5  2025-07-31 2025-07
#> 6  2025-08-31 2025-08
#> 7  2025-09-30 2025-09
#> 8  2025-10-31 2025-10
#> 9  2025-11-30 2025-11
#> 10 2025-12-31 2025-12
#> 11 2025-03-31 2025-03
#> 12 2025-04-30 2025-04
#> 13 2025-05-31 2025-05
#> 14 2025-06-30 2025-06
#> 15 2025-07-31 2025-07
#> 16 2025-08-31 2025-08
#> 17 2025-09-30 2025-09
#> 18 2025-10-31 2025-10
#> 19 2025-11-30 2025-11
#> 20 2025-12-31 2025-12
# Get last 5 observations for all currencies
get_exchange_rate('ALL', n_obs = 5)
#>     currency frequency   exch_rate decimals calculated unit_multiplier
#> 1        HUF  Business    3.074900        4      false        Hundreds
#> 2        HUF  Business    3.075900        4      false        Hundreds
#> 3        HUF  Business    3.063200        4      false        Hundreds
#> 4        HUF  Business    3.046100        4      false        Hundreds
#> 5        HUF  Business    3.054300        4      false        Hundreds
#> 6        DKK  Business  158.560000        2      false        Hundreds
#> 7        DKK  Business  157.960000        2      false        Hundreds
#> 8        DKK  Business  157.700000        2      false        Hundreds
#> 9        DKK  Business  156.950000        2      false        Hundreds
#> 10       DKK  Business  157.360000        2      false        Hundreds
#> 11       RUB  Business   11.040000        3      false        Hundreds
#> 12       RUB  Business   10.539000        3      false        Hundreds
#> 13       RUB  Business   10.777000        3      false        Hundreds
#> 14       RUB  Business    8.613000        3      false        Hundreds
#> 15       RUB  Business    8.413000        3      false        Hundreds
#> 16       MMK  Business    0.480000        4      false        Hundreds
#> 17       MMK  Business    0.479300        4      false        Hundreds
#> 18       MMK  Business    0.480900        4      false        Hundreds
#> 19       MMK  Business    0.477100        4      false        Hundreds
#> 20       MMK  Business    0.479200        4      false        Hundreds
#> 21       XDR  Business   13.799460        5       true           Units
#> 22       XDR  Business   13.769110        5       true           Units
#> 23       XDR  Business   13.795100        5       true           Units
#> 24       XDR  Business   13.709100        5       true           Units
#> 25       XDR  Business   13.758120        5       true           Units
#> 26       TRY  Business   23.460000        2      false        Hundreds
#> 27       TRY  Business   23.390000        2      false        Hundreds
#> 28       TRY  Business   23.460000        2      false        Hundreds
#> 29       TRY  Business   23.280000        2      false        Hundreds
#> 30       TRY  Business   23.380000        2      false        Hundreds
#> 31       RON  Business  232.360000        2      false        Hundreds
#> 32       RON  Business  231.820000        2      false        Hundreds
#> 33       RON  Business  231.540000        2      false        Hundreds
#> 34       RON  Business  230.460000        2      false        Hundreds
#> 35       RON  Business  231.080000        2      false        Hundreds
#> 36       NZD  Business    5.811100        4      false           Units
#> 37       NZD  Business    5.807200        4      false           Units
#> 38       NZD  Business    5.803900        4      false           Units
#> 39       NZD  Business    5.797000        4      false           Units
#> 40       NZD  Business    5.821000        4      false           Units
#> 41       JPY  Business    6.433300        4      false        Hundreds
#> 42       JPY  Business    6.414300        4      false        Hundreds
#> 43       JPY  Business    6.439300        4      false        Hundreds
#> 44       JPY  Business    6.404100        4      false        Hundreds
#> 45       JPY  Business    6.428600        4      false        Hundreds
#> 46       HKD  Business    1.294800        4      false           Units
#> 47       HKD  Business    1.291900        4      false           Units
#> 48       HKD  Business    1.297000        4      false           Units
#> 49       HKD  Business    1.286400        4      false           Units
#> 50       HKD  Business    1.292500        4      false           Units
#> 51       CZK  Business   48.863000        3      false        Hundreds
#> 52       CZK  Business   48.801000        3      false        Hundreds
#> 53       CZK  Business   48.686000        3      false        Hundreds
#> 54       CZK  Business   48.475000        3      false        Hundreds
#> 55       CZK  Business   48.409000        3      false        Hundreds
#> 56       CAD  Business    7.361400        4      false           Units
#> 57       CAD  Business    7.329600        4      false           Units
#> 58       CAD  Business    7.322400        4      false           Units
#> 59       CAD  Business    7.271700        4      false           Units
#> 60       CAD  Business    7.287600        4      false           Units
#> 61       BDT  Business    8.240000        2      false        Hundreds
#> 62       BDT  Business    8.230000        2      false        Hundreds
#> 63       BDT  Business    8.260000        2      false        Hundreds
#> 64       BDT  Business    8.200000        2      false        Hundreds
#> 65       BDT  Business    8.240000        2      false        Hundreds
#> 66       USD  Business   10.079100        4      false           Units
#> 67       USD  Business   10.066100        4      false           Units
#> 68       USD  Business   10.099000        4      false           Units
#> 69       USD  Business   10.018400        4      false           Units
#> 70       USD  Business   10.063800        4      false           Units
#> 71       THB  Business   31.821000        3      false        Hundreds
#> 72       THB  Business   32.068000        3      false        Hundreds
#> 73       THB  Business   32.188000        3      false        Hundreds
#> 74       THB  Business   32.038000        3      false        Hundreds
#> 75       THB  Business   32.122000        3      false        Hundreds
#> 76       PLN  Business    2.805700        4      false           Units
#> 77       PLN  Business    2.801000        4      false           Units
#> 78       PLN  Business    2.792800        4      false           Units
#> 79       PLN  Business    2.785500        4      false           Units
#> 80       PLN  Business    2.789800        4      false           Units
#> 81       AUD  Business    6.736200        4      false           Units
#> 82       AUD  Business    6.738900        4      false           Units
#> 83       AUD  Business    6.734200        4      false           Units
#> 84       AUD  Business    6.732000        4      false           Units
#> 85       AUD  Business    6.770600        4      false           Units
#> 86       MYR  Business    2.483700        4      false           Units
#> 87       MYR  Business    2.483000        4      false           Units
#> 88       MYR  Business    2.479800        4      false           Units
#> 89       MYR  Business    2.475500        4      false           Units
#> 90       MYR  Business    2.480000        4      false           Units
#> 91       INR  Business   11.215000        3      false        Hundreds
#> 92       INR  Business   11.160000        3      false        Hundreds
#> 93       INR  Business   11.185000        3      false        Hundreds
#> 94       INR  Business   11.113000        3      false        Hundreds
#> 95       INR  Business   11.200000        3      false        Hundreds
#> 96       IDR  Business    0.060298        6      false        Hundreds
#> 97       IDR  Business    0.060218        6      false        Hundreds
#> 98       IDR  Business    0.060331        6      false        Hundreds
#> 99       IDR  Business    0.059740        6      false        Hundreds
#> 100      IDR  Business    0.060017        6      false        Hundreds
#> 101      GBP  Business   13.572100        4      false           Units
#> 102      GBP  Business   13.531900        4      false           Units
#> 103      GBP  Business   13.577100        4      false           Units
#> 104      GBP  Business   13.538600        4      false           Units
#> 105      GBP  Business   13.571700        4      false           Units
#> 106      CNY  Business  143.970000        2      false        Hundreds
#> 107      CNY  Business  143.930000        2      false        Hundreds
#> 108      CNY  Business  144.570000        2      false        Hundreds
#> 109      CNY  Business  143.440000        2      false        Hundreds
#> 110      CNY  Business  143.950000        2      false        Hundreds
#> 111      BRL  Business    1.840000        4      false           Units
#> 112      BRL  Business    1.850900        4      false           Units
#> 113      BRL  Business    1.855900        4      false           Units
#> 114      BRL  Business    1.855700        4      false           Units
#> 115      BRL  Business    1.866600        4      false           Units
#> 116      VND  Business    0.038300        4      false        Hundreds
#> 117      VND  Business    0.038300        4      false        Hundreds
#> 118      VND  Business    0.038400        4      false        Hundreds
#> 119      VND  Business    0.038100        4      false        Hundreds
#> 120      VND  Business    0.038300        4      false        Hundreds
#> 121      BYN  Business    3.431100        4      false           Units
#> 122      BYN  Business    3.426700        4      false           Units
#> 123      BYN  Business    3.397300        4      false           Units
#> 124      BYN  Business    3.407400        4      false           Units
#> 125      BYN  Business    3.422900        4      false           Units
#> 126      TWI  Business  134.460000        2       true           Units
#> 127      TWI  Business  134.130000        2       true           Units
#> 128      TWI  Business  134.100000        2       true           Units
#> 129      TWI  Business  133.530000        2       true           Units
#> 130      TWI  Business  133.980000        2       true           Units
#> 131      SGD  Business    7.840500        4      false           Units
#> 132      SGD  Business    7.825500        4      false           Units
#> 133      SGD  Business    7.843600        4      false           Units
#> 134      SGD  Business    7.827300        4      false           Units
#> 135      SGD  Business    7.853700        4      false           Units
#> 136      PKR  Business    3.599000        3      false        Hundreds
#> 137      PKR  Business    3.595000        3      false        Hundreds
#> 138      PKR  Business    3.606000        3      false        Hundreds
#> 139      PKR  Business    3.546000        3      false        Hundreds
#> 140      PKR  Business    3.561000        3      false        Hundreds
#> 141      MXN  Business   56.080000        2      false        Hundreds
#> 142      MXN  Business   56.110000        2      false        Hundreds
#> 143      MXN  Business   56.070000        2      false        Hundreds
#> 144      MXN  Business   55.790000        2      false        Hundreds
#> 145      MXN  Business   55.970000        2      false        Hundreds
#> 146      ILS  Business    3.160600        4      false           Units
#> 147      ILS  Business    3.170400        4      false           Units
#> 148      ILS  Business    3.196600        4      false           Units
#> 149      ILS  Business    3.167700        4      false           Units
#> 150      ILS  Business    3.168100        4      false           Units
#> 151      I44  Business  119.860000        2       true           Units
#> 152      I44  Business  119.590000        2       true           Units
#> 153      I44  Business  119.650000        2       true           Units
#> 154      I44  Business  119.080000        2       true           Units
#> 155      I44  Business  119.470000        2       true           Units
#> 156      EUR  Business   11.843000        4      false           Units
#> 157      EUR  Business   11.798500        4      false           Units
#> 158      EUR  Business   11.779500        4      false           Units
#> 159      EUR  Business   11.728500        4      false           Units
#> 160      EUR  Business   11.758500        4      false           Units
#> 161      CHF  Business 1271.530000        2      false        Hundreds
#> 162      CHF  Business 1269.200000        2      false        Hundreds
#> 163      CHF  Business 1268.110000        2      false        Hundreds
#> 164      CHF  Business 1262.890000        2      false        Hundreds
#> 165      CHF  Business 1263.810000        2      false        Hundreds
#> 166      HRK  Business  138.580000        2      false        Hundreds
#> 167      HRK  Business  139.160000        2      false        Hundreds
#> 168      HRK  Business  138.650000        2      false        Hundreds
#> 169      HRK  Business  139.990000        2      false        Hundreds
#> 170      HRK  Business  139.510000        2      false        Hundreds
#> 171      BGN  Business  607.810000        2      false        Hundreds
#> 172      BGN  Business  605.840000        2      false        Hundreds
#> 173      BGN  Business  604.710000        2      false        Hundreds
#> 174      BGN  Business  604.660000        2      false        Hundreds
#> 175      BGN  Business  605.530000        2      false        Hundreds
#> 176      ISK  Business    8.050000        2      false        Hundreds
#> 177      ISK  Business    8.000000        2      false        Hundreds
#> 178      ISK  Business    7.990000        2      false        Hundreds
#> 179      ISK  Business    7.970000        2      false        Hundreds
#> 180      ISK  Business    7.990000        2      false        Hundreds
#> 181      ZAR  Business    0.609100        4      false           Units
#> 182      ZAR  Business    0.609500        4      false           Units
#> 183      ZAR  Business    0.612700        4      false           Units
#> 184      ZAR  Business    0.611000        4      false           Units
#> 185      ZAR  Business    0.611600        4      false           Units
#> 186      TWD  Business   32.181000        3      false        Hundreds
#> 187      TWD  Business   32.042000        3      false        Hundreds
#> 188      TWD  Business   32.041000        3      false        Hundreds
#> 189      TWD  Business   31.823000        3      false        Hundreds
#> 190      TWD  Business   32.004000        3      false        Hundreds
#> 191      SEK  Business  109.440000        2      false        Hundreds
#> 192      SEK  Business  109.160000        2      false        Hundreds
#> 193      SEK  Business  109.200000        2      false        Hundreds
#> 194      SEK  Business  108.950000        2      false        Hundreds
#> 195      SEK  Business  109.520000        2      false        Hundreds
#> 196      PHP  Business   17.098000        3      false        Hundreds
#> 197      PHP  Business   17.105000        3      false        Hundreds
#> 198      PHP  Business   17.094000        3      false        Hundreds
#> 199      PHP  Business   16.920000        3      false        Hundreds
#> 200      PHP  Business   16.966000        3      false        Hundreds
#> 201      KRW  Business    0.697900        4      false        Hundreds
#> 202      KRW  Business    0.696700        4      false        Hundreds
#> 203      KRW  Business    0.696800        4      false        Hundreds
#> 204      KRW  Business    0.691600        4      false        Hundreds
#> 205      KRW  Business    0.695200        4      false        Hundreds
#>      date_from    date_to       date
#> 1   2025-12-31 2025-12-31 2025-12-31
#> 2   2026-01-02 2026-01-02 2026-01-02
#> 3   2026-01-05 2026-01-05 2026-01-05
#> 4   2026-01-06 2026-01-06 2026-01-06
#> 5   2026-01-07 2026-01-07 2026-01-07
#> 6   2025-12-31 2025-12-31 2025-12-31
#> 7   2026-01-02 2026-01-02 2026-01-02
#> 8   2026-01-05 2026-01-05 2026-01-05
#> 9   2026-01-06 2026-01-06 2026-01-06
#> 10  2026-01-07 2026-01-07 2026-01-07
#> 11  2022-02-23 2022-02-23 2022-02-23
#> 12  2022-02-24 2022-02-24 2022-02-24
#> 13  2022-02-25 2022-02-25 2022-02-25
#> 14  2022-02-28 2022-02-28 2022-02-28
#> 15  2022-03-01 2022-03-01 2022-03-01
#> 16  2025-12-31 2025-12-31 2025-12-31
#> 17  2026-01-02 2026-01-02 2026-01-02
#> 18  2026-01-05 2026-01-05 2026-01-05
#> 19  2026-01-06 2026-01-06 2026-01-06
#> 20  2026-01-07 2026-01-07 2026-01-07
#> 21  2025-12-31 2025-12-31 2025-12-31
#> 22  2026-01-02 2026-01-02 2026-01-02
#> 23  2026-01-05 2026-01-05 2026-01-05
#> 24  2026-01-06 2026-01-06 2026-01-06
#> 25  2026-01-07 2026-01-07 2026-01-07
#> 26  2025-12-31 2025-12-31 2025-12-31
#> 27  2026-01-02 2026-01-02 2026-01-02
#> 28  2026-01-05 2026-01-05 2026-01-05
#> 29  2026-01-06 2026-01-06 2026-01-06
#> 30  2026-01-07 2026-01-07 2026-01-07
#> 31  2025-12-31 2025-12-31 2025-12-31
#> 32  2026-01-02 2026-01-02 2026-01-02
#> 33  2026-01-05 2026-01-05 2026-01-05
#> 34  2026-01-06 2026-01-06 2026-01-06
#> 35  2026-01-07 2026-01-07 2026-01-07
#> 36  2025-12-31 2025-12-31 2025-12-31
#> 37  2026-01-02 2026-01-02 2026-01-02
#> 38  2026-01-05 2026-01-05 2026-01-05
#> 39  2026-01-06 2026-01-06 2026-01-06
#> 40  2026-01-07 2026-01-07 2026-01-07
#> 41  2025-12-31 2025-12-31 2025-12-31
#> 42  2026-01-02 2026-01-02 2026-01-02
#> 43  2026-01-05 2026-01-05 2026-01-05
#> 44  2026-01-06 2026-01-06 2026-01-06
#> 45  2026-01-07 2026-01-07 2026-01-07
#> 46  2025-12-31 2025-12-31 2025-12-31
#> 47  2026-01-02 2026-01-02 2026-01-02
#> 48  2026-01-05 2026-01-05 2026-01-05
#> 49  2026-01-06 2026-01-06 2026-01-06
#> 50  2026-01-07 2026-01-07 2026-01-07
#> 51  2025-12-31 2025-12-31 2025-12-31
#> 52  2026-01-02 2026-01-02 2026-01-02
#> 53  2026-01-05 2026-01-05 2026-01-05
#> 54  2026-01-06 2026-01-06 2026-01-06
#> 55  2026-01-07 2026-01-07 2026-01-07
#> 56  2025-12-31 2025-12-31 2025-12-31
#> 57  2026-01-02 2026-01-02 2026-01-02
#> 58  2026-01-05 2026-01-05 2026-01-05
#> 59  2026-01-06 2026-01-06 2026-01-06
#> 60  2026-01-07 2026-01-07 2026-01-07
#> 61  2025-12-31 2025-12-31 2025-12-31
#> 62  2026-01-02 2026-01-02 2026-01-02
#> 63  2026-01-05 2026-01-05 2026-01-05
#> 64  2026-01-06 2026-01-06 2026-01-06
#> 65  2026-01-07 2026-01-07 2026-01-07
#> 66  2025-12-31 2025-12-31 2025-12-31
#> 67  2026-01-02 2026-01-02 2026-01-02
#> 68  2026-01-05 2026-01-05 2026-01-05
#> 69  2026-01-06 2026-01-06 2026-01-06
#> 70  2026-01-07 2026-01-07 2026-01-07
#> 71  2025-12-31 2025-12-31 2025-12-31
#> 72  2026-01-02 2026-01-02 2026-01-02
#> 73  2026-01-05 2026-01-05 2026-01-05
#> 74  2026-01-06 2026-01-06 2026-01-06
#> 75  2026-01-07 2026-01-07 2026-01-07
#> 76  2025-12-31 2025-12-31 2025-12-31
#> 77  2026-01-02 2026-01-02 2026-01-02
#> 78  2026-01-05 2026-01-05 2026-01-05
#> 79  2026-01-06 2026-01-06 2026-01-06
#> 80  2026-01-07 2026-01-07 2026-01-07
#> 81  2025-12-31 2025-12-31 2025-12-31
#> 82  2026-01-02 2026-01-02 2026-01-02
#> 83  2026-01-05 2026-01-05 2026-01-05
#> 84  2026-01-06 2026-01-06 2026-01-06
#> 85  2026-01-07 2026-01-07 2026-01-07
#> 86  2025-12-31 2025-12-31 2025-12-31
#> 87  2026-01-02 2026-01-02 2026-01-02
#> 88  2026-01-05 2026-01-05 2026-01-05
#> 89  2026-01-06 2026-01-06 2026-01-06
#> 90  2026-01-07 2026-01-07 2026-01-07
#> 91  2025-12-31 2025-12-31 2025-12-31
#> 92  2026-01-02 2026-01-02 2026-01-02
#> 93  2026-01-05 2026-01-05 2026-01-05
#> 94  2026-01-06 2026-01-06 2026-01-06
#> 95  2026-01-07 2026-01-07 2026-01-07
#> 96  2025-12-31 2025-12-31 2025-12-31
#> 97  2026-01-02 2026-01-02 2026-01-02
#> 98  2026-01-05 2026-01-05 2026-01-05
#> 99  2026-01-06 2026-01-06 2026-01-06
#> 100 2026-01-07 2026-01-07 2026-01-07
#> 101 2025-12-31 2025-12-31 2025-12-31
#> 102 2026-01-02 2026-01-02 2026-01-02
#> 103 2026-01-05 2026-01-05 2026-01-05
#> 104 2026-01-06 2026-01-06 2026-01-06
#> 105 2026-01-07 2026-01-07 2026-01-07
#> 106 2025-12-31 2025-12-31 2025-12-31
#> 107 2026-01-02 2026-01-02 2026-01-02
#> 108 2026-01-05 2026-01-05 2026-01-05
#> 109 2026-01-06 2026-01-06 2026-01-06
#> 110 2026-01-07 2026-01-07 2026-01-07
#> 111 2025-12-31 2025-12-31 2025-12-31
#> 112 2026-01-02 2026-01-02 2026-01-02
#> 113 2026-01-05 2026-01-05 2026-01-05
#> 114 2026-01-06 2026-01-06 2026-01-06
#> 115 2026-01-07 2026-01-07 2026-01-07
#> 116 2025-12-31 2025-12-31 2025-12-31
#> 117 2026-01-02 2026-01-02 2026-01-02
#> 118 2026-01-05 2026-01-05 2026-01-05
#> 119 2026-01-06 2026-01-06 2026-01-06
#> 120 2026-01-07 2026-01-07 2026-01-07
#> 121 2025-12-31 2025-12-31 2025-12-31
#> 122 2026-01-02 2026-01-02 2026-01-02
#> 123 2026-01-05 2026-01-05 2026-01-05
#> 124 2026-01-06 2026-01-06 2026-01-06
#> 125 2026-01-07 2026-01-07 2026-01-07
#> 126 2025-12-31 2025-12-31 2025-12-31
#> 127 2026-01-02 2026-01-02 2026-01-02
#> 128 2026-01-05 2026-01-05 2026-01-05
#> 129 2026-01-06 2026-01-06 2026-01-06
#> 130 2026-01-07 2026-01-07 2026-01-07
#> 131 2025-12-31 2025-12-31 2025-12-31
#> 132 2026-01-02 2026-01-02 2026-01-02
#> 133 2026-01-05 2026-01-05 2026-01-05
#> 134 2026-01-06 2026-01-06 2026-01-06
#> 135 2026-01-07 2026-01-07 2026-01-07
#> 136 2025-12-31 2025-12-31 2025-12-31
#> 137 2026-01-02 2026-01-02 2026-01-02
#> 138 2026-01-05 2026-01-05 2026-01-05
#> 139 2026-01-06 2026-01-06 2026-01-06
#> 140 2026-01-07 2026-01-07 2026-01-07
#> 141 2025-12-31 2025-12-31 2025-12-31
#> 142 2026-01-02 2026-01-02 2026-01-02
#> 143 2026-01-05 2026-01-05 2026-01-05
#> 144 2026-01-06 2026-01-06 2026-01-06
#> 145 2026-01-07 2026-01-07 2026-01-07
#> 146 2025-12-31 2025-12-31 2025-12-31
#> 147 2026-01-02 2026-01-02 2026-01-02
#> 148 2026-01-05 2026-01-05 2026-01-05
#> 149 2026-01-06 2026-01-06 2026-01-06
#> 150 2026-01-07 2026-01-07 2026-01-07
#> 151 2025-12-31 2025-12-31 2025-12-31
#> 152 2026-01-02 2026-01-02 2026-01-02
#> 153 2026-01-05 2026-01-05 2026-01-05
#> 154 2026-01-06 2026-01-06 2026-01-06
#> 155 2026-01-07 2026-01-07 2026-01-07
#> 156 2025-12-31 2025-12-31 2025-12-31
#> 157 2026-01-02 2026-01-02 2026-01-02
#> 158 2026-01-05 2026-01-05 2026-01-05
#> 159 2026-01-06 2026-01-06 2026-01-06
#> 160 2026-01-07 2026-01-07 2026-01-07
#> 161 2025-12-31 2025-12-31 2025-12-31
#> 162 2026-01-02 2026-01-02 2026-01-02
#> 163 2026-01-05 2026-01-05 2026-01-05
#> 164 2026-01-06 2026-01-06 2026-01-06
#> 165 2026-01-07 2026-01-07 2026-01-07
#> 166 2022-12-23 2022-12-23 2022-12-23
#> 167 2022-12-27 2022-12-27 2022-12-27
#> 168 2022-12-28 2022-12-28 2022-12-28
#> 169 2022-12-29 2022-12-29 2022-12-29
#> 170 2022-12-30 2022-12-30 2022-12-30
#> 171 2025-12-22 2025-12-22 2025-12-22
#> 172 2025-12-23 2025-12-23 2025-12-23
#> 173 2025-12-29 2025-12-29 2025-12-29
#> 174 2025-12-30 2025-12-30 2025-12-30
#> 175 2025-12-31 2025-12-31 2025-12-31
#> 176 2025-12-31 2025-12-31 2025-12-31
#> 177 2026-01-02 2026-01-02 2026-01-02
#> 178 2026-01-05 2026-01-05 2026-01-05
#> 179 2026-01-06 2026-01-06 2026-01-06
#> 180 2026-01-07 2026-01-07 2026-01-07
#> 181 2025-12-31 2025-12-31 2025-12-31
#> 182 2026-01-02 2026-01-02 2026-01-02
#> 183 2026-01-05 2026-01-05 2026-01-05
#> 184 2026-01-06 2026-01-06 2026-01-06
#> 185 2026-01-07 2026-01-07 2026-01-07
#> 186 2025-12-31 2025-12-31 2025-12-31
#> 187 2026-01-02 2026-01-02 2026-01-02
#> 188 2026-01-05 2026-01-05 2026-01-05
#> 189 2026-01-06 2026-01-06 2026-01-06
#> 190 2026-01-07 2026-01-07 2026-01-07
#> 191 2025-12-31 2025-12-31 2025-12-31
#> 192 2026-01-02 2026-01-02 2026-01-02
#> 193 2026-01-05 2026-01-05 2026-01-05
#> 194 2026-01-06 2026-01-06 2026-01-06
#> 195 2026-01-07 2026-01-07 2026-01-07
#> 196 2025-12-31 2025-12-31 2025-12-31
#> 197 2026-01-02 2026-01-02 2026-01-02
#> 198 2026-01-05 2026-01-05 2026-01-05
#> 199 2026-01-06 2026-01-06 2026-01-06
#> 200 2026-01-07 2026-01-07 2026-01-07
#> 201 2025-12-31 2025-12-31 2025-12-31
#> 202 2026-01-02 2026-01-02 2026-01-02
#> 203 2026-01-05 2026-01-05 2026-01-05
#> 204 2026-01-06 2026-01-06 2026-01-06
#> 205 2026-01-07 2026-01-07 2026-01-07
```
