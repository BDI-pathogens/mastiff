# Return a length-n vector of unique short strings e.g. for labelling n objects

If n \<= 26, we return `letters[1:n]`. If 26 \< n \<= 26^2, we return
`c("aa", "ab", "ac", ... "az", "ba", "bb", ...)`. If 26^2 \< n \<= 26^3,
we return `c("aaa", "aab", ...)`

## Usage

``` r
get_n_character_ids(n)
```

## Arguments

- n:

  a non-negative integer

## Value

a character vector

## Examples

``` r
get_n_character_ids(1)
#> [1] "a"
get_n_character_ids(5)
#> [1] "a" "b" "c" "d" "e"
get_n_character_ids(28)
#>  [1] "aa" "ab" "ac" "ad" "ae" "af" "ag" "ah" "ai" "aj" "ak" "al" "am" "an" "ao"
#> [16] "ap" "aq" "ar" "as" "at" "au" "av" "aw" "ax" "ay" "az" "ba" "bb"
```
