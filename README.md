social_amoveo
=====

[Here is a live version to try it out](http://159.223.201.207:8095/home.html). It is free to use, but you need to own VEO, the currency from [Amoveo](https://github.com/zack-bitcoin/amoveo) in order to participate. Or, someone who has VEO can give you access.

Uses coin-hours from Amoveo to meter access to a forum.

In order to run this program, you need to be running an amoveo full node first. https://github.com/zack-bitcoin/amoveo

You need to edit the /config/sys.config.tmpl file from the Amoveo full node.
Here is what the values should be in that file
```
    {block_meta, true},
    {block_meta_block, false},
    {block_meta_txs, false},
    {block_meta_governance, false},
    {block_meta_before, false},
    {block_meta_following, true},
```
This is so the full node will write each account's new balance on the block, after processing that block. Which makes it possible for social-amoveo to scan the blocks and know the history of account balances.


turn it on
``` sh start.sh ```

attach to the REPL of a running program
``` sh attach.sh ```

after attaching, you can shut it off
``` utils:off() ```
It is important to turn it off this way, because then your changes will be saved to the hard drive. So you don't need to scan the blocks again, and posts wont get deleted.

to delete the database and start a fresh server with zero accounts and zero posts.
``` sh clean.sh ```



example commands you can use after attaching.
===========

to give/take coins to an account:
```accounts:update_veo_balance(AccountID, NewBalance).```

to give/take coin-hours to an account:
```account:change_coin_hours(AccountID, Delta).```

To learn about other commands, take a look at [the signed api](/apps/social_amoveo/src/signed_handler.erl). Seeing the definitions of the api should give context to understand what you can do.
