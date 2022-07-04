Social Amoveo
=====

Clicking "im not a robot" isnt going to work forever. Robots will learn to click like us.
Doing PoW on phones isnt a feasible alternative, because of battery life.
We should use PoS as our spam prevention mechanism. You need to own coins to use a free website.

The goal of this software is to use coin-hours as the spam prevention mechanism for a simplified clone of twitter.

[Here is a live version to try it out](http://159.223.201.207:8095/home.html). It is free to use, but you need to own VEO, the currency from [Amoveo](https://github.com/zack-bitcoin/amoveo) in order to participate. Or, someone who has VEO can give you access.

You can read the public posts without needing to own veo.

Why use Coin-Hours?
=========

coin-hours are a currency. you can use them as collateral. You can spend them. So, using coin-hours gives all the benefits of using a cryptocurrency.

Having coin-hours means we can use crypto-economics to secure our website. So we can have crypto-economic guarantees about how much hard drive space or cpu or bandwidth could be consumed by operating the website.

This makes it cheaper to run the website, because you know exactly how much memory and cpu you could need. You don't need to buy extra as a buffer.
It makes it secure against many kinds of attacks, because it is impossible for the attacker to use more than their resource limit.

Coin-hours are more scalable than cryptocurrency
=========

Coin-hours are 100% off-chain. Using them as a spam prevention mechanism doesn't require anything to happen on-chain.

A cryptocurrency on a blockchain can only have like 20 transactions per second. With coin-hours, balances can change as many times per second as your server's database can handle.

With coin-hours, we never pay any transaction fees, and we never need to wait for any confirmations.

With coin-hours, it is technically free to use your website.

The website is as fast as any other centralized website.

Coin-hours give more control to the website owner.
===========

The owner of the website can print themselves new coin-hours. They can sell coin-hours.


Installing on your server, to run your own social network.
=========

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
