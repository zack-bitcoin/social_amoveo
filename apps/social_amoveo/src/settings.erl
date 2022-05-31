-module(settings).
-export([bytes/0, %hard drive bytes we can use for this social media website.
         coins/0, %approximate number of veo currently.
         half_life/0,%how long do coin-hours last
         cpu/0 %how many computations we can do per block
]).

bytes() ->
    100000000.%100 megabytes
coins() -> %in satoshis
    5000000000000.
half_life() -> %in blocks
    1008.
cpu() ->
    1000000.
%(coin-hours produced per hour are the same as the market cap. So if you have (Market cap) many coin-hours, then you should be able to use nearly 100% of the server's CPU per hour)
    
