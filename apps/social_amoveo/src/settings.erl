-module(settings).
-export([bytes/0, %hard drive bytes we can use for this social media website.
         coins/0, %approximate number of veo currently.
         half_life/0,%how long do coin-hours last
         cpu/0, %how many computations we can do per block
         dm_cost/0,
         post_cost/0,
         follow_cost/0,
         coins_per_byte/0
]).

bytes() ->
   %_123123123
    1000000000.%1 gigabytes
coins() -> %in satoshis
    %84.6k veo
   %_____12345678
    8460000000000.

coins_per_byte() ->
    %actually satoshis per byte.
    coins() div bytes().
    %8460 satoshis per byte -> 0.000084 veo-hours per byte -> 1 veo gives you 11k bytes.

half_life() -> %in blocks
    1008.
cpu() ->
    1000000.
dm_cost() ->
    2.
post_cost() ->
    2.
follow_cost() ->
    2.
%(coin-hours produced per hour are the same as the market cap. So if you have (Market cap) many coin-hours, then you should be able to use nearly 100% of the server's CPU per hour)
    
