social_amoveo
=====

Uses coin-hours from Amoveo to meter access to a forum.

It is a work in progress.
Notes on what more needs to be built is in the [todo](todo) file.

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
It is important to turn it off this way, because then you don't need to re-scan the blocks when you turn it on again.

to delete the database and start a fresh server with zero accounts and zero posts.
``` sh clean.sh ```
